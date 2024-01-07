library(tidyverse)
library(tidymodels)
library(lubridate)
library(janitor)
library(mgcv)
library(fitdistrplus)
library(tweedie)
library(statmod)
library(itsadug)
library(keras)
library(tensorflow)
library(gt)
library(gtExtras)
library(nflfastR)
library(ggimage)

#install_tensorflow(envname = "r-tensorflow")
#install_keras()
# use_condaenv("keras-tf", required = TRUE)

library(keras)
library(tensorflow)



###### Loading in the data

games <- read.csv("games.csv")
players <- read.csv("players.csv") %>% clean_names() %>%
  dplyr::mutate(ft = str_split_i(height,"-",1) %>% as.numeric(),
                inch = str_split_i(height,"-",2) %>% as.numeric(),
                height_in = ft*12+inch
  )
plays <- read.csv("plays.csv") 
tackles <- read.csv("tackles.csv") 

teams <- nflfastR::teams_colors_logos
# Loading in the first week of tracking data to play around with
track1 <- read.csv("tracking_week_1.csv") %>% clean_names()

#####

##### Defining start and end events

track1 %>% pull(event) %>% unique()
# first_contact , tackle, qb_slide , touchdown, qb_sack , fumble - end 
# run, handoff, lateral, snap_direct, pass_arrived, pass_outcome_caught - start

start_triggers <- c("run", "handoff", "lateral", "snap_direct", "pass_arrived", "pass_outcome_caught")
end_triggers <- c("first_contact" , "tackle", "qb_slide" , "touchdown", "qb_sack" , "fumble","out_of_bounds")

#####


##### Understanding missed tackles EDA

num_plays_w_tackle <- tackles %>% dplyr::group_by(gameId,playId) %>%
  summarise(missed_tackle_binomial = ifelse(sum(pff_missedTackle, na.rm = TRUE)> 0,1,0)) %>%
  ungroup() %>% nrow()

missed_tackles <- tackles %>% filter(pff_missedTackle == 1)

num_plays_w_missed_tackle <- missed_tackles %>% distinct(gameId,playId) %>% nrow()

num_plays_w_missed_tackle / num_plays_w_tackle


#Missed tackles only occurred on 15% of the plays with a tackle involved. 

# Function to calculate total missed tackle yardage
missed_tackle_yards <- function(week) {
  # read in
  trackfile <- paste0("tracking_week_",week,".csv")
  track1 <- read_csv(trackfile) %>% clean_names()
  
  # only plays with missed tackles
  track_mt <- track1 %>% inner_join(missed_tackles, by = c("game_id" = "gameId","play_id" = "playId"))
  # only the ball carrier for those plays
  allcar <- track_mt %>%
        inner_join(plays %>% dplyr::select(gameId,playId,ballCarrierId), 
                   by = c("game_id" = "gameId","play_id" = "playId","nfl_id" = "ballCarrierId")) %>%
        setNames(paste0("car_",names(.))) 
  
  # start indicated at first contact
  # end indicated with the end tags: tackle, touchdown, oob
  
  mtsum <- allcar %>%
    dplyr::group_by(car_game_id,car_play_id) %>%
    dplyr::summarise(start_frame = pick(car_event,car_frame_id) %>% filter(car_event == "first_contact") %>% 
                      pull(car_frame_id) %>% min(),
                    end_frame = pick(car_event,car_frame_id) %>% 
                      filter(car_event %in% c("tackle","qb_slide","touchdown",
                                          "qb_sack","fumble","out_of_bounds")) %>%
                      pull(car_frame_id) %>% min(),
                    start_x = pick(car_event,car_x) %>% filter(car_event == "first_contact") %>% 
                      pull(car_x) %>% min(),
                    end_x = pick(car_event,car_x) %>% 
                      filter(car_event %in% c("tackle","qb_slide","touchdown",
                                          "qb_sack","fumble","out_of_bounds")) %>%
                      pull(car_x) %>% min(),
                    diff = abs(end_x - start_x)
                  )
  
  
  return(mtsum)

}

mt_summary <- 1:9 %>% map_dfr(missed_tackle_yards)






# those plays don't have a first contact variable, can't assign value to missed yards filter less than inf

mt_summary <- mt_summary %>% filter(diff < Inf)

mt_summary %>%
  ggplot(aes(x = diff)) + geom_density()

mt_summary %>% pull(diff) %>% summary() # Distribution of the yards gained after missed tackles

total_missed_tackle_yds <- mt_summary %>% pull(diff) %>% sum() # Total yards from missed tackles 

total_yards <- plays %>% inner_join(tackles) %>% distinct(gameId,playId, .keep_all = TRUE) %>%
  pull(playResult) %>% sum()

# Percentage of all yards coming from missed tackles
total_missed_tackle_yds / total_yards


#On plays with a missed tackle, average of 7.7 yards and median of 5.5 yards were gained after the missed tackle. Total of 13,104 yards occurred after missed tackles. Also, for plays where a tackle was made, 15.5%  of the total yards came after a missed tackle. This is a significant amount of yards that resulted from missed tackles. Definitely something that should be accounted for. 

# Team level missed tackles

mt_plot <- mt_summary %>% 
  left_join(plays, by = c("car_game_id" = "gameId","car_play_id" = "playId")) %>%
  dplyr::group_by(defensiveTeam) %>%
  dplyr::summarise(yds_from_missed_tackles = sum(diff, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(teams %>% dplyr::select(team_abbr,team_logo_wikipedia,team_color),
             by = c("defensiveTeam" = "team_abbr")) %>%
  ggplot() +
  geom_bar(aes(x = reorder(defensiveTeam,desc(yds_from_missed_tackles)), y = yds_from_missed_tackles,
               fill = team_color),
           stat = "identity") +
  geom_image(aes(x = reorder(defensiveTeam,desc(yds_from_missed_tackles)), y = -25, 
                 image = team_logo_wikipedia),size = 0.04) +
  scale_fill_identity() +
  xlab("") + ylab("Yards Allowed From Missed Tackles") +
  ggtitle("2022 Week 1-9 Defensive Yards Allowed From Missed Tackles") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# ggsave(filename = "mt_plot.png",plot = mt_plot,dpi = 320,
#        height = 6, width = 8,
#        bg = "white")


##### Getting the average duration between catch/handoff and tackle

# Create a loop that takes a week of tracking data
# groups all the plays
# returns a data frame with the play id and the time between start and end triggers
# use the ball for only one set of tracking data per play
tr_files <- list.files()[grepl("tracking",list.files())]

carry_duration <- function(row_n) {
  
  
    bbbb <- read.csv(tr_files[row_n]) %>%
      clean_names() %>%
      filter(display_name == "football") %>%
      dplyr::mutate(starts  = ifelse(event %in% start_triggers, "start",NA),
                ends = ifelse(event %in% end_triggers, "end",NA),
                cap_frames = ifelse(starts == "start" | ends == "end", frame_id,NA)) %>%
      dplyr::group_by(game_id,play_id) %>%
      dplyr::summarise(duration = max(cap_frames, na.rm = TRUE) - min(cap_frames, na.rm = TRUE)) %>% 
      ungroup()
  return(bbbb)
}

play_carry_duration <- 1:length(tr_files) %>% map_dfr(carry_duration)

# Play carry duration EDA

ggplot(play_carry_duration, aes(x = duration / 10)) + geom_density()

play_carry_duration %>% pull(duration) %>% summary()

#### Looking at the pairs of carrier and tackler

clean_tackles <- tackles %>%
  filter(tackle == 1, assist == 0, pff_missedTackle == 0)

car_tack_pairs_each_week <- function(week) {
  trackfile <- paste0("tracking_week_",week,".csv")
  track1 <- read_csv(trackfile) %>% clean_names()
  
  alltack <- track1 %>% 
    inner_join(clean_tackles,
               by = c("game_id" = "gameId","play_id" = "playId","nfl_id" = "nflId")) %>%
    inner_join(players %>% dplyr::select(nfl_id, height_in,weight))
  
  allcar <- track1 %>%
    inner_join(plays %>% dplyr::select(gameId,playId,ballCarrierId), 
               by = c("game_id" = "gameId","play_id" = "playId","nfl_id" = "ballCarrierId")) %>%
    setNames(paste0("car_",names(.))) 
  
  alloff <- track1 %>%
    inner_join(plays %>% dplyr::select(gameId,playId,possessionTeam,ballCarrierId), 
               by = c("game_id" = "gameId","play_id" = "playId","club" = "possessionTeam")) %>%
    filter(nfl_id != ballCarrierId) %>%
    setNames(paste0("off_",names(.))) 

  
  pair <- inner_join(allcar,alltack, 
                     by = c("car_game_id" = "game_id",
                            "car_play_id" = "play_id",
                            "car_frame_id" = "frame_id")) %>%
    inner_join(play_testing_set %>% dplyr::select(gameId,playId),
               by = c("car_game_id" = "gameId", "car_play_id" = "playId")) %>%
    dplyr::group_by(car_game_id,car_play_id) %>%
    dplyr::mutate(start = pick(event,car_frame_id) %>% filter(event %in% start_triggers) %>% 
                    pull(car_frame_id) %>% min(),
                  end = pick(event,car_frame_id) %>% filter(event %in% end_triggers) %>% 
                    pull(car_frame_id) %>% min(),
                  dist_to_car = sqrt(((x-car_x)^2) +  ((y-car_y)^2)),
                  vel_to_car = dist_to_car - lag(dist_to_car), # in meters per hz
                  acc_to_car = vel_to_car - lag(vel_to_car), # in meters per hz^2
                  jerk_to_car = acc_to_car - lag(acc_to_car), # in meters per hz^3
                  rel_velo = s - car_s,
                  start_dist = pick(start,dist_to_car) %>% filter(car_frame_id == start) %>% pull(dist_to_car),
                  timer = car_frame_id - start,
                  time_to_col = end - car_frame_id,
                  start_frame = ifelse(timer == 0,TRUE,FALSE),
                  unq_play_id = paste0(car_game_id,"-",car_play_id)
    ) %>%
    ungroup() %>%
    filter(car_frame_id >= start, car_frame_id <= end)
  
  return(pair)
  
}

carrier_tackler <- 1:9 %>% map_dfr(car_tack_pairs_each_week)

#####

##### Understanding duration of carriers for future frame predictions

carrier_tackler %>% 
  dplyr::group_by(unq_play_id) %>%
  summarise(dur = max(timer)) %>%
  pull(dur) %>% summary()

carrier_tackler %>% 
  dplyr::group_by(unq_play_id) %>%
  summarise(dur = max(timer)) %>%
  pull(dur) %>% quantile(c(0.8,0.9,0.95,0.99))

carrier_tackler %>% 
  dplyr::group_by(unq_play_id) %>%
  summarise(dur = max(timer)) %>%
  ggplot(aes(x = dur)) + geom_density()


#####



##### Understanding distance between carrier and tackler to predict tackles


simple_dist <- bind_rows(carrier_tackler %>% 
                           dplyr::group_by(unq_play_id) %>%
                           dplyr::mutate(tackle_bin = ifelse(timer == max(timer),1,0)) %>%
                           ungroup() %>%
                           dplyr::select(unq_play_id,dist_to_car,vel_to_car,acc_to_car,jerk_to_car,
                                         height_in,weight,car_height_in,car_weight,
                                         tackle_bin),
                         carrier_nontackler %>%
                           dplyr::mutate(tackle_bin = 0) %>%
                           dplyr::select(unq_play_id,dist_to_car,vel_to_car,acc_to_car,jerk_to_car,
                                         height_in,weight,car_height_in,car_weight,
                                         tackle_bin)) %>%
  na.omit() %>%
  dplyr::group_by(tackle_bin) %>%
  slice_sample(n = 4000) %>%
  ungroup()


# Simple random forest model to predict tackles based on relative distance
# Used a balanced sample to prevent model bias

dist_tack_mod <- randomForest::randomForest(as.factor(tackle_bin) ~ dist_to_car,
                                            data = simple_dist, 
                                            type = "classification")

MLmetrics::Accuracy((predict(dist_tack_mod,simple_dist)),simple_dist$tackle_bin)

tibble(dist_to_car = seq(0.1,10,0.1)) %>%
  dplyr::mutate(preds = predict(dist_tack_mod,.)) %>% 
  ggplot(aes(x = dist_to_car, y = preds)) +
  geom_point() + geom_smooth()

# 1.6 is the cut point
# Any values less than 1.6 yards we will consider a tackle to be made in that frame going forward

#####


##### Setting up the data for each carrier - defender pair for modeling

# Set up LSTM with multiple batches based on timer duration. Max of 104 hz (timestamps) need to change each pair into length 104. 
# Pad with zeros for all plays less than 104 frames 
# Then set the batch length to be 104 to feed into the model each pair. 
# "Masking feature" in keras to ignore 0 value, could use this.



# Setting up the function to pair all carrier and defender pairs

car_def_pairs_each_week <- function(week) {
  stt <- Sys.time()
  trackfile <- paste0("tracking_week_",week,".csv")
  track1 <- read_csv(trackfile) %>% clean_names()
  
  tackle_involved_id <- tackles %>% pull(nflId)
  
  alldef <- track1 %>% 
    inner_join(plays %>% dplyr::select(gameId,playId,defensiveTeam), 
                   by = c("game_id" = "gameId","play_id" = "playId")) %>%
    filter(display_name != "football",
           club == defensiveTeam
           ) 

  allcar <- track1 %>%
        inner_join(plays %>% dplyr::select(gameId,playId,ballCarrierId), 
                   by = c("game_id" = "gameId","play_id" = "playId","nfl_id" = "ballCarrierId")) %>%
        setNames(paste0("car_",names(.))) 
  

  pair <- inner_join(allcar,alldef, 
                                by = c("car_game_id" = "game_id",
                                       "car_play_id" = "play_id",
                                       "car_frame_id" = "frame_id")) %>%
    dplyr::group_by(car_game_id,car_play_id,nfl_id) %>%
    dplyr::mutate(start = pick(event,car_frame_id) %>% filter(event %in% start_triggers) %>% 
                    pull(car_frame_id) %>% min(),
                  alt_start = pick(event,car_frame_id) %>% filter(event  == "ball_snap") %>% 
                    pull(car_frame_id) %>% min(),
                  start = ifelse(start == Inf,alt_start,start),
                  end = pick(event,car_frame_id) %>% filter(event == "tackle") %>% # changing this from end trigger to tackle in hopes for improvement
                    pull(car_frame_id) %>% min(),
                  dist_to_car = sqrt(((x-car_x)^2) +  ((y-car_y)^2)),
                  timer = car_frame_id - start,
                  unq_play_id = paste0(car_game_id,"-",car_play_id,"-",car_nfl_id,"-",nfl_id)
                  ) %>%
    ungroup() %>%
    filter(car_frame_id >= start,car_frame_id <= end) %>%
    dplyr::select(unq_play_id,timer,dist_to_car)

    ste <- Sys.time()
  print(ste-stt)
  return(pair)

}

carrier_def <- 1:9 %>% map_dfr(car_def_pairs_each_week) 

#####

##### Initial setup with one LSTM layer


set.seed(222)

maxlen <- carrier_def$timer %>% max()

unq <- carrier_def$pair_id %>% unique()

# Getting mean and standard deviation for value scaling

dist_mn <- mean(carrier_def$dist_to_car)
dist_sd <-sd(carrier_def$dist_to_car)

# Function to create a lagged dataframe
calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% map(~partial(lag, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_lag{lags}")))
}


# how many lags should be used for lstm 
lagz <- 20

# Padding each sequence with 0's and creating the lagged dataframe to feed into LSTM
padded <- expand_grid(
  unq_play_id = carrier_def$unq_play_id %>% unique(),
  timer = 0:maxlen) %>%
  left_join(carrier_def %>% distinct() %>% dplyr::mutate(dist_to_car = (dist_to_car - dist_mn) / dist_sd)) %>%
  dplyr::select(unq_play_id,timer,dist_to_car) %>%
  replace_na(list(dist_to_car = -dist_mn/dist_sd)) %>% # -dist_mn/dist_sd is 0 scaled
  dplyr::group_by(unq_play_id) %>%
  calculate_lags(dist_to_car,1:lagz) %>%
  replace(is.na(.),0)

# Training testing split
split <- group_initial_split(padded,
                             group = unq_play_id,
                             p = 0.75)

tr_lstm <- training(split) 
te_lstm <- testing(split)


tr_paddedx <- as.matrix(tr_lstm[,-c(1:3)])
tr_paddedy <- tr_lstm$dist_to_car

tr_paddedx_arr <- array(
    data = as.numeric(unlist(tr_paddedx)),
    dim = c(
        nrow(tr_paddedx),
        lagz,
        1
    )
)

tr_paddedy_arr <- array(
    data = as.numeric(unlist(tr_paddedy)),
    dim = c(
        length(tr_paddedy),
        1,
        1
    )
)

MLmetrics::RMSE(te_paddedy, lag(te_paddedy,1,default = 0)) # 0.129 is the baseline RMSE

rm(padded,tr_lstm,te_lstm,split)
gc()

# Initial sequential keras model with one layer lstm
model <- keras_model_sequential() %>%
  layer_masking(mask_value = -dist_mn/dist_sd, # All 0 sequences will be masked/ignored
                batch_input_shape = c(maxlen+1, lagz, 1) # batch size, timesteps, features
                             ) %>%
  layer_lstm(units = 32,
             stateful = TRUE
             ) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = "mse", optimizer = "adam")

summary(model)

with(tf$device("CPU"),
    rez <- model %>%
      fit(x = tr_paddedx_arr, y = tr_paddedy_arr,
          batch_size = maxlen+1,
          epochs = 10,
          validation_split = 0.2,
          shuffle = FALSE)
)

one_layer_tr_plot <- plot(rez)

ggsave(one_layer_tr_plot,filename = "one_layer_lstm_tr_plot.png",
       dpi = 320)

# Training seems to bottleneck


# Evaluating the testing data
te_paddedx <- as.matrix(te_lstm[,-c(1:3)])
te_paddedy <- te_lstm$dist_to_car

te_paddedx_arr <- array(
    data = as.numeric(unlist(te_paddedx)),
    dim = c(
        nrow(te_paddedx),
        lagz,
        1
    )
)

te_paddedy_arr <- array(
    data = as.numeric(unlist(te_paddedy)),
    dim = c(
        length(te_paddedy),
        1,
        1
    )
)
with(tf$device("CPU"),
model_forecast <- model %>%
  predict(te_paddedx_arr, batch_size = maxlen+1)
)

MLmetrics::RMSE(model_forecast,te_paddedy)



with(tf$device("CPU"),
     model %>% evaluate(te_paddedx_arr,te_paddedy_arr, batch_size = maxlen+1) # 1.37 with one layer
)




##### Adding complexity with second layer 
model <- keras_model_sequential() %>%
  layer_masking(mask_value = -dist_mn/dist_sd,
                batch_input_shape = c(maxlen+1, lagz, 1) # batch size, timesteps, features
                             ) %>%
  layer_lstm(units = 32,
             return_sequences = TRUE,
             dropout = 0.1,
             recurrent_dropout = 0.5,
             stateful = TRUE
             ) %>%
  layer_lstm(units = 32,
             stateful = TRUE
             ) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = "mse", optimizer = "adam")

summary(model)

checkpoint_path <- "lstm_2l_training/cp.ckpt"
checkpoint_dir <- fs::path_dir(checkpoint_path)

# Create a callback that saves the model's weights
cp_callback <- callback_model_checkpoint(
  filepath = checkpoint_path,
  save_weights_only = TRUE,
  verbose = 1
)


with(tf$device("CPU"),
     rez2 <- model %>%
       fit(x = tr_paddedx_arr, y = tr_paddedy_arr,
           batch_size = maxlen+1,
           epochs = 10,
           validation_split = 0.2,
           shuffle = FALSE,
           callbacks = list(cp_callback)
       )
)

two_layer_tr_plot <- plot(rez2)

ggsave(two_layer_tr_plot,filename = "two_layer_lstm_tr_plot.png",
       dpi = 320)


load_model_weights_tf(model,checkpoint_path) # with proper trained weights

with(tf$device("CPU"),
     model %>% evaluate(te_paddedx_arr,te_paddedy_arr, batch_size = maxlen + 1) ### 0.0624 loss
)

# Very good model results with 0.06 MSE on a held out test set.


#### Looking at carrier defender pairs at the last timestamp
last_ts_lagged <- carrier_def %>% distinct() %>% 
  dplyr::mutate(dist_to_car = (dist_to_car - dist_mn) / dist_sd) %>%
  dplyr::select(unq_play_id,timer,dist_to_car) %>%
  replace_na(list(dist_to_car = -dist_mn/dist_sd)) %>%
  dplyr::group_by(unq_play_id) %>%
  calculate_lags(dist_to_car,1:lagz) %>%
  replace(is.na(.),0) %>%
  dplyr::group_by(unq_play_id) %>%
  filter(timer == max(timer)) %>%
  ungroup()

last_ts_lagged %>%
  ggplot(aes(x = dist_to_car)) +
  geom_density()

#####



#144 frames is too long, need to predict ahead the next 42 frames.


##### Setting up a function to sequentially build out predictions

set_random_seed(444)
load_model_weights_tf(model,checkpoint_path) # with proper trained weights

# This funciton predicts the relative distance on the next frame, 
# then recurrently adds each prediction to the previous data to 
# keep predicting with rolling data
build_100_frame_predictions <- function(rowx) {
  
  lf_num <- demo_set$timer[rowx]
  
  id <- demo_set$unq_play_id[rowx]
  
  matt <- as.matrix(demo_set[rowx,-c(1:3)])
  
  arr <- array(
    data = as.numeric(unlist(matt)),
    dim = c(
      1,
      lagz,
      1
    )
  )
  
  
  
  with(tf$device("CPU"),
       next_pred <- (model %>%
                       predict(arr, verbose = 0))[1,1]
  )
  
  preds <- numeric()
  preds[1] <- next_pred
  
  cur_frame <- c(next_pred,as.numeric(unlist(matt))[1:19])
  for (i in 2:42) {
    
    
    newarr <- array(
      data = cur_frame,
      dim = c(
        1,
        lagz,
        1
      )
    )
    
    with(tf$device("CPU"),
         new_pred <- (model %>% predict(newarr, verbose = 0))[1,1]
    )
    
    preds[i] <- new_pred
    
    old_vals <- cur_frame[1:19]
    cur_frame <- c(new_pred,old_vals)
    
    
  }
  
  full_preds <- data.frame(unq_play_id = id,
                           timer = seq(lf_num+1,lf_num+42),
                           dist_to_car = as.numeric(preds)
  )
  
  
  return(full_preds)
  
  
  
}

# subsetting the last_ts dataframe for only the last week

# Due to long prediction time only week 9 is used for demonstration
wk9gms <- read.csv("tracking_week_9.csv") %>% 
  clean_names() %>%
  pull(game_id) %>%
  unique()

demo_set <- last_ts_lagged %>%
  dplyr::mutate(game_id = unq_play_id %>% str_split_i("-",1)) %>%
  filter(game_id %in% wk9gms) %>%
  dplyr::select(-game_id)

# Calculating predictions here
demo_predictions <- 1:nrow(demo_set) %>% map(build_100_frame_predictions, .progress = TRUE)

demo_predictions_fin <- bind_rows(demo_predictions)

# Plotting results against the input data
lstm_pred_viz <- ggplot() +
  geom_density(data = last_ts_lagged,aes(x = dist_to_car, color = "Tackle frame (n)")) +
  geom_density(data = demo_predictions_fin %>% dplyr::group_by(unq_play_id) %>%
                 filter(timer == min(timer)), 
               aes(x = dist_to_car,color = "LSTM prediction (n+1)")) +
  scale_color_manual(values = c("Tackle frame (n)" = "blue",
                                "LSTM prediction (n+1)" = "red")) +
  theme_light() +
  ggtitle("Evaluation of LSTM Prediction") +
  xlab("Relative Distance") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# ggsave(plot = lstm_pred_viz,filename = "lstm_pred_viz.png",dpi = 320,
#        height = 6, width = 8)

#####

##### Summarizing predictions at the play level

tack_gr_each_play <- tackles %>%
  filter(tackle == 1 | assist == 1 | forcedFumble == 1) %>%
  dplyr::group_by(gameId,playId) %>%
  dplyr::summarise(tack_all_ids = list(nflId)) %>%
  ungroup()


scaled_cut <- (1.6 - dist_mn) / dist_sd

# Data frame to summarize each frame with data on the closest defender
frame_sum <- demo_predictions_fin %>%
  dplyr::mutate(game_id = str_split_i(unq_play_id,"-",1),
                play_id = str_split_i(unq_play_id,"-",2),
                car_nfl_id = str_split_i(unq_play_id,"-",3),
                def_nfl_id = str_split_i(unq_play_id,"-",4)) %>%
  inner_join(tack_gr_each_play %>% dplyr::mutate(across(everything(),as.character)),
             by = c("game_id" = "gameId","play_id" = "playId")) %>%
  dplyr::group_by(game_id,play_id) %>%
  filter(!(def_nfl_id %in% tack_all_ids)) %>% # Only looking at defenders not involved in the original tackle
  ungroup() %>%
  dplyr::group_by(game_id,play_id,timer) %>%
  summarise(pred_tackle = ifelse(min(dist_to_car) < scaled_cut,1,0), # Do we predict a tackle will occur (min(dist) < 1.6)
            pred_closest_dist = min(dist_to_car), # what was the closest distance
            pred_tackler_id = pick(dist_to_car,def_nfl_id) %>% filter(dist_to_car == pred_closest_dist) %>%
              pull(def_nfl_id) # who was the closest defender
            ) %>% ungroup()
View(frame_sum)

# Summarizing each frame into information on each play
play_sum <- frame_sum %>%
  dplyr::group_by(game_id,play_id) %>%
  summarise(new_tackle_occurs = ifelse(sum(pred_tackle) > 0,1,0), # would a new tackle occur this play
            new_tackle_frame = case_when(
              new_tackle_occurs == 1 ~ pick(timer,pred_tackle) %>% filter(pred_tackle == 1) %>% 
                pull(timer) %>% min(),
              TRUE ~ max(timer)), # in what frame would the new tackle occur
            orig_tackle_frame = min(timer)-1, # when was the original tackle
            diff_in_frames = new_tackle_frame - orig_tackle_frame, # difference in original and new tackle
            new_tackler_id = pick(new_tackle_frame,pred_tackler_id) %>% filter(timer == new_tackle_frame) %>%
              pull(pred_tackler_id) # new tackler
  )

# summary info on the new tackles frames
play_sum %>%
  ggplot(aes(x = diff_in_frames)) +
  geom_density()

play_sum %>% pull(diff_in_frames) %>% summary()

#####


##### Projecting carrier trajectory
clean_tackles <- tackles %>%
  filter(tackle == 1, assist == 0, pff_missedTackle == 0)

# Function to cleanly pull out only carrier data from tracking
carrier_only_data <- function(week) {
  
  trackfile <- paste0("tracking_week_",week,".csv")
  track1 <- read_csv(trackfile) %>% clean_names()
  
  
  
  allcar <- track1 %>%
    inner_join(plays %>% dplyr::select(gameId,playId,ballCarrierId), 
               by = c("game_id" = "gameId","play_id" = "playId","nfl_id" = "ballCarrierId")) %>%
    inner_join(players %>% dplyr::select(nfl_id, height_in,weight)) %>%
    setNames(paste0("car_",names(.))) %>%
    dplyr::group_by(car_game_id,car_play_id) %>%
    dplyr::mutate(start = pick(car_event,car_frame_id) %>% filter(car_event %in% start_triggers) %>% 
                    pull(car_frame_id) %>% min(),
                  end = pick(car_event,car_frame_id) %>% filter(car_event == "tackle") %>% # changing this from end trigger to tackle in hopes for improvement
                    pull(car_frame_id) %>% min(),
                  timer = car_frame_id - start
    ) %>%
    ungroup() %>%
    filter(car_frame_id >= start, car_frame_id <= end) %>%
    dplyr::select(car_game_id,car_play_id,timer,car_x)
  
  return(allcar)
  
}

carrier <- 9 %>% map_dfr(carrier_only_data)




## Projecting hypothetical yards

# building a gam model and returning prediction based on data
# cubic spline with 3 knots to emulate underlying physics displacement formula
# accounting for distance, speed, acceleration, and jerk properties
# Only interested in the carriers distance in the x values, since these are used to calculate yardage in football
carr_traj <- function(subset_df) {
  # subset df should consist of car_x, timer and predicted frames
  mod <- gam(car_x ~ s(timer, bs = "cr", k = 3), data = subset_df)
  summary(mod)
  pred_x <- predict.gam(mod, newdata = data.frame(timer = subset_df$new_tackle_frame[1]))
  return(pred_x)
}


#####


# Combining the play level LSTM results for next hypothetical tackle frame with GAM trajectory function
rb_other_traj <- carrier %>%
  dplyr::mutate(across(car_game_id:car_play_id,as.character)) %>%
  inner_join(play_sum, by = c("car_game_id" = "game_id","car_play_id" = "play_id")) %>%
  dplyr::group_by(car_game_id,car_play_id) %>%
  dplyr::mutate(x_from_traj = carr_traj(pick(car_x,timer,new_tackle_frame)) ) %>%
  ungroup()

# Example play for visual

car_ex <- rb_other_traj %>%
  filter(car_game_id == 2022110600, car_play_id == 1082)
# new tackle frame is 22

gam_ex <- gam(car_x ~ s(timer, bs = "cr",k = 3), data = car_ex)

pred_ex <- predict.gam(gam_ex,newdata = data.frame(timer = 0:22))


sample_play_viz <- ggplot() +
  geom_point(data = car_ex,aes(x = timer, y = car_x, color = "Actual Values")) +
  geom_line(data = data.frame(timer = 0:22, car_x = pred_ex), 
            aes(x = timer, y = car_x,color = "GAM Function")) +
  geom_vline(xintercept = 12,linetype = "dotted", color = "blue",
             size = 1.25) +
  geom_vline(xintercept = 22,linetype = "dotted", color = "blue",
             size = 1.25) +
  geom_text(aes(x = 10, y = 51),label = "Original \nTackle\nFrame",color = "blue") +
  geom_text(aes(x = 20, y = 50.9),label = "Proj.\nNext \nTackle\nFrame",color = "blue") +
  geom_segment(aes(x = 12, xend = 22, y = car_ex$car_x[nrow(car_ex)], yend = car_ex$car_x[nrow(car_ex)])) +
  geom_segment(aes(x = 22, xend = 22, y = car_ex$car_x[nrow(car_ex)], yend = pred_ex[length(pred_ex)]),
               color = "green", size = 1.3) +
  geom_label(aes(x = 24.5, y = 48.35),label = "0.31 yds\nsaved",color = "green", fontface = "bold") +
  theme_gray() +
  scale_color_manual(values = c("Actual Values" = "black",
                                "GAM Function" = "red")) +
  ggtitle("Yards Saved Calculation Visualized",
          subtitle = "Game ID: 2022110600 Play ID: 1082") +
  ylab("Yards (X coordinate)") +
  xlab("Frames") + 
  scale_x_continuous(breaks = seq(0,26,2), limits = c(0,26)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank())

ggsave(sample_play_viz, filename = "sample_play_viz.png",dpi = 320)

# Combining with original play tackle information to calculate yards saved on each play
yds_saved_db <- rb_other_traj %>%
  dplyr::group_by(car_game_id,car_play_id) %>%
  filter(timer == max(timer)) %>% # last row for each play
  ungroup() %>%
  dplyr::mutate(yds_saved = abs(x_from_traj - car_x)) %>%
  inner_join(tackles %>%
               filter(tackle == 1) %>%
               dplyr::group_by(gameId,playId) %>%
               slice(1) %>%
               ungroup() %>%
               mutate(across(gameId:playId,as.character)) %>% 
               setNames(paste0("orig_",names(.))) , 
             by = c("car_game_id" = "orig_gameId", "car_play_id" = "orig_playId")) %>%
  dplyr::select(car_game_id,car_play_id,orig_nflId,orig_tackle_frame,
                new_tackler_id,new_tackle_frame,diff_in_frames,yds_saved)

yds_saved_db %>%
  ggplot(aes(x = yds_saved)) +
  geom_density()

yds_saved_db %>%
  ggplot(aes(x = diff_in_frames,y = yds_saved)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#We now have yards saved for all the demo plays and can see the clear relationship between the predicted difference in time between the next hypothetical collision and yards saved. 

#####

##### Evaluating yards saved by team and by player for the demo set

#player
pl_tab_viz <- yds_saved_db %>%
  inner_join(plays %>%
               mutate(across(gameId:playId,as.character)) %>% 
               dplyr::select(gameId,playId,quarter:defensiveTeam), 
             by = c("car_game_id" = "gameId","car_play_id" = "playId")) %>%
  inner_join(players %>% 
               dplyr::select(nfl_id,position,display_name) %>%
               setNames(paste0("orig_tackler_",names(.))), by = c("orig_nflId" = "orig_tackler_nfl_id")) %>%
  inner_join(teams %>% dplyr::select(team_abbr,team_logo_wikipedia), 
             by = c("defensiveTeam" = "team_abbr")) %>% 
  dplyr::group_by(car_game_id,team_logo_wikipedia,defensiveTeam,possessionTeam,orig_nflId,
                  orig_tackler_display_name,orig_tackler_position) %>%
  summarise(tackles = n(),
            total_yds_saved = round(sum(yds_saved),2),
            yds_saved_per_tackle = round(total_yds_saved / tackles,2)) %>%
  ungroup() %>%
  arrange(car_game_id,defensiveTeam,desc(total_yds_saved)) %>%
  gt() %>%
  tab_header(title = "2022 Week 9 Yards Saved Individual Leaders") %>%
  cols_label(
    car_game_id ~ "Game ID",
    team_logo_wikipedia ~ "Logo",
    defensiveTeam ~ "Team",
    possessionTeam ~ "Opponent",
    orig_nflId ~ "ID",
    orig_tackler_display_name ~ "Tackler",
    orig_tackler_position ~ "POS",
    tackles ~ "Tackles",
    total_yds_saved ~ "Total Yards Saved",
    yds_saved_per_tackle ~ "Yards Saved Per Tackle"
  ) %>%
  gt_img_rows(columns = team_logo_wikipedia) %>%
  data_color(
    columns = c(tackles,total_yds_saved,yds_saved_per_tackle),
    method = "numeric",
    palette = "PiYG",
    na_color = "white"
  ) %>%
  # opt_interactive(
  #   use_search = TRUE,
  #   use_filters = TRUE,
  #   use_resizers = TRUE,
  #   use_highlight = TRUE,
  #   use_compact_mode = TRUE,
  #   use_text_wrapping = FALSE,
  #   use_page_size_select = TRUE
  # ) %>%
  opt_align_table_header(align = "center") %>%
  cols_width(
    car_game_id ~ px(150),
    defensiveTeam ~ px(80),
    possessionTeam ~ px(80),
    orig_nflId ~ px(80),
    orig_tackler_display_name ~ px(150),
    orig_tackler_position ~ px(80),
    team_logo_wikipedia ~ px(80),
  ) %>%
  cols_align(align = "center")

#gtsave(pl_tab_viz, filename = "pl_tab_viz_img.png")

#team

tm_tab_viz <- yds_saved_db %>%
  inner_join(plays %>%
               mutate(across(gameId:playId,as.character)) %>% 
               dplyr::select(gameId,playId,quarter:defensiveTeam), 
             by = c("car_game_id" = "gameId","car_play_id" = "playId")) %>%
  inner_join(teams %>% dplyr::select(team_abbr,team_logo_wikipedia), 
             by = c("defensiveTeam" = "team_abbr")) %>% 
  dplyr::group_by(car_game_id,team_logo_wikipedia,defensiveTeam) %>%
  summarise(tackles = n(),
            total_yds_saved = round(sum(yds_saved),2),
            yds_saved_per_tackle = round(total_yds_saved / tackles,2)) %>%
  ungroup() %>%
  arrange(car_game_id,defensiveTeam,desc(total_yds_saved)) %>%
  gt() %>%
  tab_header(title = "2022 Week 9 Yards Saved By Team") %>%
  cols_label(
    car_game_id ~ "Game ID",
    defensiveTeam ~ "Team",
    team_logo_wikipedia ~ "Logo",
    tackles ~ "Tackles",
    total_yds_saved ~ "Total Yards Saved",
    yds_saved_per_tackle ~ "Yards Saved Per Tackle"
  ) %>%
  gt_img_rows(columns = team_logo_wikipedia) %>%
  data_color(
    columns = c(tackles,total_yds_saved,yds_saved_per_tackle),
    method = "numeric",
    palette = "PiYG",
    na_color = "white"
  ) %>%
  # opt_interactive(
  #   use_search = TRUE,
  #   use_filters = TRUE,
  #   use_resizers = TRUE,
  #   use_highlight = TRUE,
  #   use_compact_mode = FALSE,
  #   use_text_wrapping = FALSE,
  #   use_page_size_select = TRUE,
  #   page_size_default = 32
  # ) %>%
  opt_align_table_header(align = "center") %>%
  cols_width(
    car_game_id ~ px(150),
    defensiveTeam ~ px(80),
    team_logo_wikipedia ~ px(80)
  ) %>% 
  cols_align(align = "center")

tm_tab_viz

#gtsave(tm_tab_viz, filename = "tm_tab_viz_img.png")


# Yards saved by defensive position group

general_pos_table <- tibble(
  orig_tackler_position = c("CB","DB","DE","DT","FS","ILB","MLB","NT","OLB","SS"),
  pos_group = c("DB","DB","DL","DL","DB","LB","LB","DL","LB","DB")
)

general_pos_viz <- yds_saved_db %>%
  inner_join(plays %>%
               mutate(across(gameId:playId,as.character)) %>% 
               dplyr::select(gameId,playId,quarter:defensiveTeam), 
             by = c("car_game_id" = "gameId","car_play_id" = "playId")) %>%
  inner_join(players %>% 
               dplyr::select(nfl_id,position,display_name) %>%
               setNames(paste0("orig_tackler_",names(.))), by = c("orig_nflId" = "orig_tackler_nfl_id")) %>%
  inner_join(general_pos_table) %>%
  dplyr::group_by(pos_group) %>%
  summarise(tackles = n(),
            total_yds_saved = round(sum(yds_saved),2),
            yds_saved_per_tackle = round(total_yds_saved / tackles,2)) %>%
  ungroup() %>%
  pivot_longer(cols = tackles:yds_saved_per_tackle) %>%
  ggplot(aes(x = factor(pos_group, levels = c("DB","LB","DL")), y = value, fill = name)) +
  geom_bar(position = "dodge",stat = "identity") +
  facet_wrap(~name, scales = "free") +
  theme_bw() + 
  ggtitle("Tackle Metrics Position Group Breakdown",
          subtitle = "2022 Week 9") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_blank(),
        legend.title = element_blank(),
        strip.background = element_rect(alpha("lightgrey",0.5)))

# ggsave(filename = "general_pos_viz.png",
#        plot = general_pos_viz,
#        dpi = "retina",
#        scale = 2)

#####

##### Looking into team level results compared to missed tackle yards

tm_lvl_yds_saved_demo <- yds_saved_db %>%
  inner_join(plays %>%
               mutate(across(gameId:playId,as.character)) %>% 
               dplyr::select(gameId,playId,quarter:defensiveTeam), 
             by = c("car_game_id" = "gameId","car_play_id" = "playId")) %>%
  inner_join(teams %>% dplyr::select(team_abbr,team_logo_wikipedia), 
             by = c("defensiveTeam" = "team_abbr")) %>% 
  dplyr::group_by(car_game_id,team_logo_wikipedia,defensiveTeam) %>%
  summarise(tackles = n(),
            total_yds_saved = round(sum(yds_saved),2),
            yds_saved_per_tackle = round(total_yds_saved / tackles,2)) %>%
  ungroup() %>%
  arrange(car_game_id,defensiveTeam,desc(total_yds_saved))

tm_lvl_yds_saved_demo %>%
  inner_join( mt_summary %>% 
                left_join(plays, by = c("car_game_id" = "gameId","car_play_id" = "playId")) %>%
                dplyr::group_by(defensiveTeam) %>%
                dplyr::summarise(yds_from_missed_tackles = sum(diff, na.rm = TRUE)) %>%
                ungroup() %>%
                inner_join(teams %>% dplyr::select(team_abbr,team_logo_wikipedia,team_color),
                           by = c("defensiveTeam" = "team_abbr"))) %>%
  dplyr::select(where(is.numeric)) %>%
  cor()
  

#####
