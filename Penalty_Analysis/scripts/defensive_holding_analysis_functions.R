library(tidyverse)

closest_offensive_player_func <- function(df) {
  
  # this function find the offensive player that was closest to the defender for duration of play
  
  # find frame where ball is snapped
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
        "qb_sack"
      )
    ) %>%
    distinct() %>%
    pull(frame_id)
  
  
  # find offensive player(s) penalty defender was closest to from ball snap to pass thrown
  offensive_players <- 
    df %>%
    filter(frame_id %in% c(ball_snap_frame:end_frame[1])) %>%
    # filter out football
    filter(!is.na(off_jersey_num)) %>% 
    select(frame_id,
           event,
           def_jersey_num,
           off_jersey_num,
           distance_from_opponent) %>%
    group_by(frame_id) %>%
    mutate(min_distance = min(distance_from_opponent)) %>%
    ungroup() %>%
    filter(distance_from_opponent == min_distance) %>%
    count(closest_off_jersey_num = off_jersey_num)
  
  # offensive players could result in multiple players, calculate minimum distance of each
  offensive_players_distance_df <- df %>%
    group_by(off_jersey_num, nfl_id_off) %>%
    summarise(
      min_distance_from_opp = distance_from_opponent %>% min(),
    ) %>% 
    ungroup()
  
  # join data 
  # if multiple, find player that had minimum distance of closest offensive players 
  # possible that we may be grabbing wrong offensive player, may revisit
  offensive_players_df <- offensive_players %>% 
    left_join(offensive_players_distance_df, by = c("closest_off_jersey_num" = "off_jersey_num")) %>% 
    filter(min_distance_from_opp == min(min_distance_from_opp))
  
  return(offensive_players_df)
}


# -------------------------------------------------------------------------
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
        "qb_sack"
      )
    ) %>%
    distinct() %>%
    pull(frame_id)
  
  # find offensive player(s) penalty defender was closest to from ball snap to pass thrown
  play_filtered <- 
    df %>%
    filter(frame_id %in% c(ball_snap_frame:end_frame[1]))
  
  return(play_filtered)
}


# -------------------------------------------------------------------------
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


# -------------------------------------------------------------------------
create_features_function <- function(data) {
  
  # function to create features
  
  total_frames <- data %>%
    count(off_jersey_num) %>%
    pull(n)
  
  # summarise for modeling features: play length, largest decrease in speed, acceleration, distance, closest distance to opp
  features_data_1 <-
    data %>%
    arrange(frame_id) %>%
    mutate(
      seconds_from_snap = row_number() / 10, # length of play from snap to throw/sack
      change_in_speed_1 = s_off - lag(s_off, default = 0), # change in speed from last frame
      change_in_speed_10 = s_off - lag(s_off, n = 10, default = 0), # change in speed from 1 sec ago
      distance_times_speed = s_off * distance_from_opponent # distance from opp multiplied by speed of offensive player
    ) %>%
    mutate(
      change_in_distance_times_speed_1  = distance_times_speed - lag(distance_times_speed, default = 0),
      change_in_distance_times_speed_10 = distance_times_speed - lag(distance_times_speed, n = 10, default = 0)
    ) %>%
    summarise(
      play_length = max(seconds_from_snap),
      across(
        starts_with("change_in"),
        .fns = min,
        .names = "min_{col}"
      ),
      min_distance_from_opp = min(distance_from_opponent),
      max_distance_from_opp = max(distance_from_opponent),
      max_min_distance_diff = max_distance_from_opp - min_distance_from_opp,
      avg_distance_from_opp = mean(distance_from_opponent, na.rm = TRUE),
      dh = max(dh)
    )
  
  # filter for single observation to get down, yards to go, defender in box, player locations at snap of ball
  features_data_2 <- data %>%
    filter(event %in% "ball_snap") %>%
    select(
      down,
      yards_to_go,
      defenders_in_the_box,
      x_off_at_snap = x_off,
      y_off_at_snap = y_off,
      x_def_at_snap = x_def,
      y_def_at_snap = y_def,
      distance_at_snap = distance_from_opponent
    ) %>%
    mutate(press_coverage = ifelse(distance_at_snap <= 2.0, 1, 0) %>% as.factor())
  
  # speed difference between receiver and defender
  features_data_3 <- data %>%
    mutate(speed_difference = s_off - s_def) %>%
    select(distance_from_opponent, speed_difference) %>%
    summarise(
      max_speed_difference = max(speed_difference),
      min_speed_difference = min(speed_difference)
    )
  
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
    select(nfl_id_def, percentage_as_closest_defender, man_coverage) %>%
    head(1)
  
  # percentage of frames where offensive player is within 1.25 yards of a defender, where offensive players speed decreases
  features_data_5 <-
    data %>%
    arrange(frame_id) %>%
    mutate(change_in_speed_1 = s_off - lag(s_off, default = 0)) %>%
    mutate(
      close_defender = ifelse(distance_from_opponent < 1.25, 1, 0),
      speed_decrease = ifelse(change_in_speed_1 < 0, 1, 0)
    ) %>%
    mutate(s_decr_and_close_defender = ifelse(close_defender   == 1 &
                                                speed_decrease == 1, 1, 0)) %>%
    summarise(
      total_close_defender = sum(close_defender),
      total_speed_decrease = sum(speed_decrease),
      total_s_decr_and_close_defender = sum(s_decr_and_close_defender)
    ) %>%
    mutate(
      percentage_frames_within_1.25_yards = total_close_defender / total_frames,
      percentage_frames_speed_decrease = total_speed_decrease / total_frames,
      percentage_s_decr_and_close_defender = total_s_decr_and_close_defender / total_frames
    ) %>%
    select(
      percentage_frames_within_1.25_yards,
      percentage_frames_speed_decrease,
      percentage_s_decr_and_close_defender
    )
  
  # find max consecutive frames where closest defender was within 1.25 yards and offensive players speed decreased
  features_data_6 <-
    data %>%
    arrange(frame_id) %>%
    mutate(change_in_speed_1 = s_off - lag(s_off, default = 0)) %>%
    mutate(
      close_defender = ifelse(distance_from_opponent < 1.25, 1, 0),
      speed_decrease = ifelse(change_in_speed_1 < 0, 1, 0)
    ) %>%
    mutate(s_decr_and_close_defender = ifelse(close_defender   == 1 &
                                                speed_decrease == 1, 1, 0)) %>%
    select(frame_id, s_decr_and_close_defender) %>%
    group_by(grp = with(rle(s_decr_and_close_defender), rep(seq_along(lengths), lengths))) %>%
    mutate(consecutive_frames = seq_along(grp)) %>%
    ungroup() %>%
    filter(s_decr_and_close_defender == 1) %>%
    summarise(max_consecutive_frames = max(consecutive_frames)) %>%
    mutate(
      max_consecutive_frames = ifelse(max_consecutive_frames == -Inf, 0, max_consecutive_frames)
    )
  
  features_data <-
    bind_cols(
      features_data_1,
      features_data_2,
      features_data_3,
      features_data_4,
      features_data_5,
      features_data_6
    )
  
  return(features_data)
  
}
