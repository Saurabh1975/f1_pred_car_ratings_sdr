library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(extrafont)
library(purrr)
library(broom)
library(zoo)

#Sports Packages
library(nflreadr)
library(nflfastR)
library(sportyR)


#pbp_df_hist <- readRDS('Data/pbp_full_99_24.rds') %>%
#  filter(season >= 2006)



pbp_df_current_season <- load_pbp(2016:2025) 


pbp_df <- pbp_df_current_season


ftn_data <- load_ftn_charting(seasons = TRUE)


pbp_ftn_df <- pbp_df %>%
  left_join(ftn_data, by = c('season', 'week', 'game_id' = 'nflverse_game_id', 'play_id' = 'nflverse_play_id')) %>%
  # Do Team Game Numbers
  group_by(season, posteam) %>%
  arrange(game_id, .by_group = TRUE) %>%
  mutate(
    posteam_game_number = dense_rank(game_id)
  ) %>%
  ungroup() %>%
  group_by(season, defteam) %>%
  arrange(game_id) %>%
  mutate(
    defteam_game_number = dense_rank(game_id)
  ) %>%
  ungroup() 




ints <- pbp_ftn_df %>%
  filter(interception == 1, season_type == "REG") %>%
  filter(play_type == "pass") %>%
  mutate(
    yardline = yardline_100,   # or pass-specific variables if useful
    clock_secs = half_seconds_remaining,
    score_diff = posteam_score_post - defteam_score_post
  )

library(mgcv)
int_epa_model <- gam(
  epa ~ 
    s(yardline, k = 20) +
    s(air_yards, k = 10) +
    s(down, k = 4) +
    s(ydstogo, k = 10) +
    s(clock_secs, k = 10) +
    s(score_diff, k = 10) +
    pass_location,
  data = ints
)
ints$expected_int_epa <- predict(int_epa_model, newdata = ints)

# Theoretical incomplete on INT worthy throws
potential_incomplete_df <- pbp_ftn_df %>%
  filter(season >= 2022) %>%
  filter(season_type == "REG") %>%
  filter(play_type == "pass") %>%
  mutate(
    down = pmin(down + 1, 4)
  )

potential_incomplete_df_epa = calculate_expected_points(potential_incomplete_df) %>%
  mutate(epa_inc =  ep...8 - ep...81)





#EPA Adjustments
pbp_ftn_df_adj <- pbp_ftn_df %>%
  filter(season >= 2022) %>%
  filter(season_type == 'REG') %>%
  filter(play_type %in% c('run', 'pass')) %>%
  # Bring in Expected INT EPA
  left_join(
    ints %>% select(game_id, play_id, expected_int_epa), 
    by = c("game_id", "play_id")
  ) %>%
  mutate(
    expected_int_epa = replace_na(expected_int_epa, 0)
  ) %>%
  # Bring in Counterfactual Incomplete EPA
  left_join(
    potential_incomplete_df_epa %>% select(game_id, play_id, epa_inc), 
    by = c("game_id", "play_id")
  ) %>%
  mutate(
    epa_inc = replace_na(epa_inc, 0)
  ) %>%
  # EPA Adjustments
  mutate(
    # Fumble Adjustments
    fumble_rec_adjustment = ifelse(fumble == 1 & fumble_lost == 0, avg_fumble_epa, 0),
    fumble_lost_adjustment = ifelse(fumble == 1 & fumble_lost == 1, avg_fumble_epa- epa, 0),
    # FTN Adjustments
    drop_ball_adjustment = ifelse(is_drop, air_epa + xyac_epa - epa, 0),
    ftn_int_adjustment = case_when(
      is_interception_worthy == 1 ~ expected_int_epa - epa,
      is_interception_worthy == 0 & interception == 1 & !is_drop ~ epa_inc - epa,
      is_interception_worthy == 0 & interception == 1 & is_drop ~ air_epa + xyac_epa - epa,
      TRUE ~ 0
    )
  ) %>%
  select(
    season, game_id, posteam, defteam, 
    posteam_game_number, defteam_game_number,
    play_id, play_type, desc, 
    down, ydstogo, yards_gained,
    # Other Binary Flags
    is_interception_worthy,
    epa, air_epa, yac_epa, xyac_epa, 
    # Other Metrics
    complete_pass, fumble, fumble_lost,is_drop,  interception, 
    # ADjustment Helper Columns
    epa_inc,
    ## Adjustments
    fumble_rec_adjustment, fumble_lost_adjustment,
    # FTN Adjustments
    drop_ball_adjustment, ftn_int_adjustment
  ) %>%
  # Build adjusted EPA flavors
  mutate(
    epa_fumble  = epa + fumble_rec_adjustment + fumble_lost_adjustment,
    epa_drop    = epa + drop_ball_adjustment,
    epa_ftn_int = epa + ftn_int_adjustment
  ) 



team_game_epa <- pbp_ftn_df_adj %>%
  filter(season < 2025) %>%
  group_by(season, game_id, posteam) %>%
  summarise(
    # Offensive EPA (NO drop adjustment)
    off_epa_raw    = mean(epa, na.rm = TRUE),
    off_epa_fumble = mean(epa_fumble, na.rm = TRUE),
    off_epa_ftn_int = mean(epa_ftn_int, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    pbp_ftn_df_adj %>%
      group_by(season, game_id, defteam) %>%
      summarise(
        # Defensive EPA (YES drop adjustment)
        def_epa_raw    = mean(epa, na.rm = TRUE),
        def_epa_fumble = mean(epa_fumble, na.rm = TRUE),
        def_epa_ftn_int = mean(epa_ftn_int, na.rm = TRUE),
        def_epa_drop   = mean(epa_drop, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("season", "game_id", "posteam" = "defteam")
  )



team_game_epa <- team_game_epa %>%
  mutate(
    net_epa_raw     = off_epa_raw     - def_epa_raw,
    net_epa_fumble  = off_epa_fumble  - def_epa_fumble,
    net_epa_ftn_int = off_epa_ftn_int - def_epa_ftn_int,
    net_epa_adj     = off_epa_ftn_int - (def_epa_ftn_int + def_epa_drop)
  ) %>%
  arrange(season, posteam, game_id) %>%
  group_by(season, posteam) %>%
  mutate(game_n = row_number()) %>%
  ungroup()



k_min <- 4   # minimum games before we start forecasting
h     <- 4   # predict next 3 games



rolling_df <- team_game_epa %>%
  group_by(season, posteam) %>%
  arrange(game_n) %>%
  mutate(
    # Rolling means up to game k
    roll_raw = zoo::rollmean(net_epa_raw, k_min, align = "right", fill = NA),
    roll_adj = zoo::rollmean(net_epa_adj, k_min, align = "right", fill = NA),
    
    # Forward-looking target: next h games
    future_raw = lead(zoo::rollmean(net_epa_raw, h, align = "left"), 1),
    future_adj = lead(zoo::rollmean(net_epa_raw, h, align = "left"), 1)
  ) %>%
  filter(!is.na(roll_raw), !is.na(future_raw)) %>%
  ungroup()