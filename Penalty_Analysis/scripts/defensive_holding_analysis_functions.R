
library(tidyverse)

filter_frame_func <- function(df){
  # function to filter play from time ball was snapped to qb throw/sack etc
  
  ball_snap_frame <- df %>%
    select(frame_id, event) %>%
    filter(event %in% c("ball_snap")) %>%
    distinct() %>%
    pull(frame_id)
  
  # find frame where pass thrown/sack....
  end_frame <- df %>%
    select(frame_id, event) %>%
    filter(
      event %in% c(
        "pass_forward",
        "run",
        "safety",
        "qb_strip_sack",
        "pass_tipped",
        "qb_sack"
      )
    ) %>%
    distinct() %>%
    pull(frame_id)
  
  # find offensive player(s) penalty defender was closest to from ball snap to pass thrown
  play_filtered <- 
    df %>%
    filter(frame_id %in% c(ball_snap_frame:end_frame))
  
  return(play_filtered)
}

data_prep_function <- function(eval_data) {
  
  prepped_data <- eval_data %>%
    filter(!off_pos %in% c(NA, "QB")) %>% 
    group_by(game_id, play_id, frame_id, nfl_id_off) %>% 
    filter(distance_from_opponent == min(distance_from_opponent)) %>%
    ungroup() %>% 
    nest(-c(game_id, play_id, nfl_id_off)) %>% 
    mutate(filtered_data = map(data, purrr::possibly(filter_frame_func, otherwise = NA))) %>% 
    drop_na() %>% 
    select(-data) %>% 
    unnest(filtered_data) %>% 
    mutate(dh = 0)
  
  return(prepped_data)
}



# function to create features
create_features_function <- function(data) {
  
  # summarise for modeling features: play length, largest decrease in speed, acceleration, distance, closest distance to opp  
  features_data_1 <-
    data %>%
    arrange(frame_id) %>%
    mutate(
      seconds_from_snap = row_number() / 10, # length of play from snap to throw/sack
      change_in_speed_1 = s_off - lag(s_off, default = 0), # change in speed from last frame
      change_in_speed_2 = s_off - lag(s_off, n = 2, default = 0),
      change_in_speed_3 = s_off - lag(s_off, n = 3, default = 0),
      change_in_speed_4 = s_off - lag(s_off, n = 4, default = 0),
      change_in_speed_5 = s_off - lag(s_off, n = 5, default = 0),
      change_in_speed_6 = s_off - lag(s_off, n = 6, default = 0),
      change_in_speed_7 = s_off - lag(s_off, n = 7, default = 0),
      change_in_speed_8 = s_off - lag(s_off, n = 8, default = 0),
      change_in_speed_9 = s_off - lag(s_off, n = 9, default = 0),
      change_in_speed_10 = s_off - lag(s_off, n = 10, default = 0),
      change_in_acceleration = a_off - lag(a_off, default = 0), # change in acceleration from last frame
      change_in_distance = dis_off - lag(dis_off, default = 0),# change in distance from last frame
      distance_times_speed = s_off*distance_from_opponent # distance from opp multiplied by speed of offensive player
    ) %>%
    mutate(
      change_in_distance_times_speed_1 = distance_times_speed - lag(distance_times_speed, default = 0),
      change_in_distance_times_speed_2 = distance_times_speed - lag(distance_times_speed, n = 2, default = 0),
      change_in_distance_times_speed_3 = distance_times_speed - lag(distance_times_speed, n = 3, default = 0),
      change_in_distance_times_speed_4 = distance_times_speed - lag(distance_times_speed, n = 4, default = 0),
      change_in_distance_times_speed_5 = distance_times_speed - lag(distance_times_speed, n = 5, default = 0),
      change_in_distance_times_speed_6 = distance_times_speed - lag(distance_times_speed, n = 6, default = 0),
      change_in_distance_times_speed_7 = distance_times_speed - lag(distance_times_speed, n = 7, default = 0),
      change_in_distance_times_speed_8 = distance_times_speed - lag(distance_times_speed, n = 8, default = 0),
      change_in_distance_times_speed_9 = distance_times_speed - lag(distance_times_speed, n = 9, default = 0),
      change_in_distance_times_speed_10 = distance_times_speed - lag(distance_times_speed, n = 10, default = 0)
    ) %>% 
    summarise(
      play_length = max(seconds_from_snap),
      across(starts_with("change_in"),
             .fns = min,
             .names = "min_{col}"),
      min_distance_from_opp = min(distance_from_opponent),
      avg_distance_from_opp = mean(distance_from_opponent, na.rm = TRUE),
      dh = max(dh)
    )
  
  # filter for single observation to get down, yards to go, defender in box, player locations at snap of ball
  features_data_2 <- data %>% 
    filter(event %in% "ball_snap") %>% 
    select(down, yards_to_go, defenders_in_the_box, personnel_o, personnel_d,
           x_off_at_snap = x_off, y_off_at_snap = y_off, x_def_at_snap = x_def, y_def_at_snap = y_def)
  
  # speed difference between receiver and defender
  features_data_3 <- data %>% 
    mutate(speed_difference = s_off - s_def) %>% 
    select(distance_from_opponent, speed_difference) %>% 
    summarise(max_speed_differece = max(speed_difference),
              min_speed_difference = min(speed_difference))
  
  # determine if player was defended by man coverage
  features_data_4 <- 
    data %>%
    select(nfl_id_def, event, frame_id) %>%
    group_by(nfl_id_def) %>%
    summarise(total = n()) %>%
    mutate(percentage_as_closest_defender = total / sum(total)) %>%
    mutate(man_coverage = ifelse(percentage_as_closest_defender >= .95, 1, 0)) %>% 
    ungroup() %>% 
    filter(percentage_as_closest_defender == max(percentage_as_closest_defender)) %>% 
    select(nfl_id_def, percentage_as_closest_defender, man_coverage)
  
  
  features_data <- bind_cols(features_data_1, features_data_2, features_data_3, features_data_4)
  
  return(features_data)
  
}
