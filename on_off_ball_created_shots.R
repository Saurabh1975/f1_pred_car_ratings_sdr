get_shooting_metrics_onball_offball_by_season <- function(
    season_year,
    league_id = "00",
    per_mode = "Totals"
) {
  season_str <- year_to_season(season_year)
  
  log_step("==================================================")
  log_step("Season start: ", season_year, " (", season_str, ")")
  
  touch_bins <- c(
    "Touch < 2 Seconds",
    "Touch 2-6 Seconds",
    "Touch 6+ Seconds"
  )
  
  log_step("[PTSHOT] Pulling LeagueDashPTShots (touch time bins)")
  
  ptshot_long <- purrr::map_dfr(touch_bins, function(bin) {
    log_step("[PTSHOT]   → touch_time_range='", bin, "'")
    
    hoopR::nba_leaguedashplayerptshot(
      league_id = league_id,
      season    = season_str,
      per_mode  = per_mode,
      touch_time_range = bin
    )$LeagueDashPTShots %>%
      dplyr::as_tibble() %>%
      dplyr::select(
        PLAYER_ID, PLAYER_NAME,
        PLAYER_LAST_TEAM_ID, PLAYER_LAST_TEAM_ABBREVIATION,
        FG2M, FG2A, FG3M, FG3A
      ) %>%
      dplyr::mutate(
        season_year = season_year,
        touch_time_range = bin,
        on_ball = bin %in% c("Touch 2-6 Seconds", "Touch 6+ Seconds"),
        
        PLAYER_ID = as.character(PLAYER_ID),
        PLAYER_LAST_TEAM_ID = as.character(PLAYER_LAST_TEAM_ID),
        
        dplyr::across(c(FG2M, FG2A, FG3M, FG3A), to_int)
      )
  })
  
  log_step("[PTSHOT] Done. Rows: ", nrow(ptshot_long))
  log_step("[AGG] Aggregating on-ball vs off-ball FG2/FG3 + All FG + eFG + shares")
  
  ptshot_player <- ptshot_long %>%
    dplyr::group_by(
      season_year,
      PLAYER_LAST_TEAM_ID, PLAYER_LAST_TEAM_ABBREVIATION,
      PLAYER_ID, PLAYER_NAME
    ) %>%
    dplyr::summarise(
      # ----------------------------
      # Base totals (2s + 3s)
      # ----------------------------
      FG2M_total = sum(FG2M, na.rm = TRUE),
      FG2A_total = sum(FG2A, na.rm = TRUE),
      FG3M_total = sum(FG3M, na.rm = TRUE),
      FG3A_total = sum(FG3A, na.rm = TRUE),
      
      FG2M_off_ball = sum(FG2M[!on_ball], na.rm = TRUE),
      FG2A_off_ball = sum(FG2A[!on_ball], na.rm = TRUE),
      FG3M_off_ball = sum(FG3M[!on_ball], na.rm = TRUE),
      FG3A_off_ball = sum(FG3A[!on_ball], na.rm = TRUE),
      
      FG2M_on_ball = sum(FG2M[on_ball], na.rm = TRUE),
      FG2A_on_ball = sum(FG2A[on_ball], na.rm = TRUE),
      FG3M_on_ball = sum(FG3M[on_ball], na.rm = TRUE),
      FG3A_on_ball = sum(FG3A[on_ball], na.rm = TRUE),
      
      # ----------------------------
      # All FG (2s + 3s)
      # ----------------------------
      FGM_total = FG2M_total + FG3M_total,
      FGA_total = FG2A_total + FG3A_total,
      
      FGM_off_ball = FG2M_off_ball + FG3M_off_ball,
      FGA_off_ball = FG2A_off_ball + FG3A_off_ball,
      
      FGM_on_ball = FG2M_on_ball + FG3M_on_ball,
      FGA_on_ball = FG2A_on_ball + FG3A_on_ball,
      
      # ----------------------------
      # eFG% (total / on / off)
      # eFG% = (FGM + 0.5*FG3M) / FGA
      # ----------------------------
      efg_total = dplyr::if_else(
        FGA_total > 0,
        (FGM_total + 0.5 * FG3M_total) / FGA_total,
        NA_real_
      ),
      efg_off_ball = dplyr::if_else(
        FGA_off_ball > 0,
        (FGM_off_ball + 0.5 * FG3M_off_ball) / FGA_off_ball,
        NA_real_
      ),
      efg_on_ball = dplyr::if_else(
        FGA_on_ball > 0,
        (FGM_on_ball + 0.5 * FG3M_on_ball) / FGA_on_ball,
        NA_real_
      ),
      
      # ----------------------------
      # Shares: attempts + makes
      # ----------------------------
      pct_fga_on_ball  = dplyr::if_else(FGA_total > 0, FGA_on_ball / FGA_total, NA_real_),
      pct_fga_off_ball = dplyr::if_else(FGA_total > 0, FGA_off_ball / FGA_total, NA_real_),
      
      pct_fgm_on_ball  = dplyr::if_else(FGM_total > 0, FGM_on_ball / FGM_total, NA_real_),
      pct_fgm_off_ball = dplyr::if_else(FGM_total > 0, FGM_off_ball / FGM_total, NA_real_),
      
      # ----------------------------
      # Shares: shot mix (2s vs 3s) within total and within each bucket
      # ----------------------------
      pct_fg3a_total = dplyr::if_else(FGA_total > 0, FG3A_total / FGA_total, NA_real_),
      pct_fg2a_total = dplyr::if_else(FGA_total > 0, FG2A_total / FGA_total, NA_real_),
      
      pct_fg3a_on_ball  = dplyr::if_else(FGA_on_ball > 0, FG3A_on_ball / FGA_on_ball, NA_real_),
      pct_fg2a_on_ball  = dplyr::if_else(FGA_on_ball > 0, FG2A_on_ball / FGA_on_ball, NA_real_),
      
      pct_fg3a_off_ball = dplyr::if_else(FGA_off_ball > 0, FG3A_off_ball / FGA_off_ball, NA_real_),
      pct_fg2a_off_ball = dplyr::if_else(FGA_off_ball > 0, FG2A_off_ball / FGA_off_ball, NA_real_),
      
      .groups = "drop"
    )
  
  log_step("[AGG] Done. Players: ", nrow(ptshot_player))
  
  list(
    ptshot_long = ptshot_long,
    ptshot_player = ptshot_player
  )
}
