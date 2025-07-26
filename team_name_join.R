

odds_history_teams <- opening_odds_df %>%
  select(team) %>%
  unique() %>%
  rename(odds_history_team = team) 
  




epl_standings <- fb_season_team_stats("ENG", "M", 2010:2025, "1st", "league_table")

fbref_teams <- epl_standings %>%
  group_by(Squad) %>%
  summarize(latest_pl_season = max(Season_End_Year)) %>%
  ungroup() %>%
  rename(fbref_team = Squad) %>% # Rename column Squad to fbref_team
  mutate(
    fbref_team = str_replace(fbref_team, "Utd", "United"),               # Replace "Utd" with "United"
    fbref_team = str_replace(fbref_team, "QPR", "Queens Park Rangers")  # Replace "QPR" with "Queens Park Rangers"
  ) %>%
  left_join(odds_history_teams, by = c('fbref_team' = 'odds_history_team'), keep =TRUE)