

### Static: Spotup, 
### Movement: OffScreen, Handoff, PRRollman
### Creation: Isolation, PRBallHandler
### Other: Misc, Transition


synergy_df<-  nba_synergyplaytypes(league_id = '00',
                                    season = year_to_season(most_recent_nba_season() - 1),
                                    play_type = 'OffScreen',
                                    per_mode = 'Totals')


off_screen_df <- synergy_df$SynergyPlayType %>%
  mutate(
    FGM = as.numeric(FGM),
    FGA = as.numeric(FGA),
    EFG_PCT = as.numeric(EFG_PCT)
  ) %>%
  mutate(
    fgm_2 = round((FGA*EFG_PCT - 1.5*FGM)/-0.5),
    fgm_3 = round(FGM - fgm_2)
  )


colnames( synergy_df$SynergyPlayType)


synergy_df<-  nba_synergyplaytypes(league_id = '00',
                                   season = year_to_season(most_recent_nba_season() - 1),
                                   play_type = 'Spotup',
                                   per_mode = 'Totals')


spot_up_df <- synergy_df$SynergyPlayType %>%
  mutate(
    FGM = as.numeric(FGM),
    FGA = as.numeric(FGA),
    EFG_PCT = as.numeric(EFG_PCT)
  ) %>%
  mutate(
    fgm_2 = (FGA*EFG_PCT - 1.5*FGM)/-0.5,
    fgm_3 = FGM - fgm_2
  )







  synergy_df<-  nba_synergyplaytypes(league_id = '00',
                                   season = year_to_season(most_recent_nba_season() - 1),
                                   play_type = 'Postup',
                                   per_mode = 'Totals')


cut_df <- synergy_df$SynergyPlayType %>%
  mutate(
    FGM = as.numeric(FGM),
    FGA = as.numeric(FGA),
    EFG_PCT = as.numeric(EFG_PCT)
  ) %>%
  mutate(
    fgm_2 = (FGA*EFG_PCT - 1.5*FGM)/-0.5,
    fgm_3 = FGM - fgm_2
  )























library(hoopR)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# --------------------------------------------------
# 1) Define Synergy play type buckets
# --------------------------------------------------

play_type_buckets <- tibble::tribble(
  ~play_type,        ~bucket,
  "Spotup",          "static",
  "OffScreen",       "movement",
  "Handoff",         "movement",
  "PRRollman",       "movement",
  "Isolation",       "creation",
  "PRBallHandler",   "creation",
  "Misc",            "other",
  "Transition",      "other"
)

play_types_to_pull <- play_type_buckets$play_type

# --------------------------------------------------
# 2) Function to pull one play type
# --------------------------------------------------

pull_synergy_play_type <- function(play_type_val) {
  
  message("Pulling Synergy play type: ", play_type_val)
  
  synergy_df <- nba_synergyplaytypes(
    league_id = "00",
    season = year_to_season(most_recent_nba_season() - 1),
    play_type = play_type_val,
    per_mode = "Totals"
  )
  
  synergy_df$SynergyPlayType %>%
    mutate(
      play_type_pulled = play_type_val,
      FGM = as.numeric(FGM),
      FGA = as.numeric(FGA),
      EFG_PCT = as.numeric(EFG_PCT)
    ) %>%
    mutate(
      fgm_2 = round((FGA * EFG_PCT - 1.5 * FGM) / -0.5),
      fgm_3 = round(FGM - fgm_2)
    )
}

# --------------------------------------------------
# 3) Pull all play types and combine
# --------------------------------------------------

synergy_all <- map_dfr(
  play_types_to_pull,
  pull_synergy_play_type
)

# --------------------------------------------------
# 4) Add your upstream buckets
# --------------------------------------------------

synergy_all_bucketed <- synergy_all %>%
  left_join(
    play_type_buckets,
    by = c("play_type_pulled" = "play_type")
  )

# --------------------------------------------------
# 5) Summarise 3PM by player and bucket
# --------------------------------------------------

player_3pm_by_bucket <- synergy_all_bucketed %>%
  group_by(
    PLAYER_ID,
    PLAYER_NAME,
    # TEAM_ID,
    # TEAM_ABBREVIATION,
    # TEAM_NAME,
    bucket
  ) %>%
  summarise(
    fgm_3 = sum(fgm_3, na.rm = TRUE),
    FGM = sum(FGM, na.rm = TRUE),
    FGA = sum(FGA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bucket_col = paste0("fgm_3_", bucket)
  ) %>%
  select(
    PLAYER_ID,
    PLAYER_NAME,
    # TEAM_ID,
    # TEAM_ABBREVIATION,
    # TEAM_NAME,
    bucket_col,
    fgm_3
  ) %>%
  pivot_wider(
    names_from = bucket_col,
    values_from = fgm_3,
    values_fill = 0
  )

# --------------------------------------------------
# 6) Add total 3PM across these Synergy buckets
# --------------------------------------------------

player_3pm_by_bucket <- player_3pm_by_bucket %>%
  mutate(
    fgm_3_synergy_total =
      fgm_3_static +
      fgm_3_movement +
      fgm_3_creation +
      fgm_3_other
  ) %>%
  arrange(desc(fgm_3_synergy_total))

player_3pm_by_bucket


## bring in tracking data



adv_df <- readRDS(
  '/Users/saurabhr/Documents/GitHub/nba_rapm/Data/Input Data/advanced_stats_2025.rds')

threes_df <-  readRDS(
  '/Users/saurabhr/Documents/GitHub/nba_rapm/Data/Input Data/Player/Tracking/player_ptshot_threes_2025_26.rds')

ptshot_threes_2025_wide_sum <- threes_df %>%
  group_by(PLAYER_ID, PLAYER_NAME) %>%
  summarise(
    FG3A_tightly_contested = sum(FG3A_tightly_contested, na.rm = TRUE),
    FG3A_contested = sum(FG3A_contested, na.rm = TRUE),
    FG3A_open = sum(FG3A_open, na.rm = TRUE),
    FG3A_wide_open = sum(FG3A_wide_open, na.rm = TRUE),
    FG3M = sum(FG3M_wide_open, na.rm = TRUE) + sum(FG3M_contested, na.rm = TRUE),
    FG3A = sum(FG3A_wide_open, na.rm = TRUE) + sum(FG3A_contested, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(
    time_of_poss_df %>% select(TEAM_ID, TEAM_ABBREVIATION, PLAYER_ID, POSS),
    by = 'PLAYER_ID'
  ) %>%
  mutate(
    POSS = as.numeric(POSS),
    FG3A_tightly_contested_75 = FG3A_tightly_contested/POSS*75,
    FG3A_contested_75 = FG3A_contested/POSS*75,
    FG3A_open_75 = FG3A_open/POSS*75,
    FG3A_wide_open_75 = FG3A_wide_open/POSS*75,
    FG3A_75 = FG3A/POSS*75,
    pct_wide_open = FG3A_wide_open_75/FG3A_75,
    pct_contested = FG3A_tightly_contested_75/FG3A_75
  ) 


player_3pm_by_bucket_plus_tracking <- player_3pm_by_bucket %>%
  left_join(
    ptshot_threes_2025_wide_sum %>% 
      select(PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION,
             FG3M, FG3A, pct_wide_open, POSS)
  ) %>%
  mutate(
    fgm_3_unknown = FG3M - fgm_3_synergy_total
  )

