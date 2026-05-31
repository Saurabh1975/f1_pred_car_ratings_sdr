library(hoopR)
library(dplyr)
library(tidyr)

library(data.table)
library(stringr)
library(purrr)
library(jsonlite)

library(dplyr)
library(purrr)
library(stringr)


season = '2025-26'
pbp_dir <- paste0("Data/PBP/", season, '/Annotated PBP')

pbp_files <- list.files(
  path = pbp_dir,
  pattern = "^pbp_annotated_.*\\.rds$",
  full.names = TRUE
)

length(pbp_files)

read_pbp_safe <- function(path) {
  tryCatch(
    readRDS(path),
    error = function(e) {
      message("✗ Failed to read: ", basename(path), " | ", e$message)
      NULL
    }
  )
}



pbp_all <- pbp_files %>%
  set_names() %>%
  map(read_pbp_safe) %>%
  discard(is.null) %>%
  map_dfr(~ {
    df <- as_tibble(.x)
    
    df
  })  %>%
  filter(season_type_description == 'Regular Season')







df_adv <- nba_leaguedashplayerstats(
  league_id = '00',
  season = season,
  measure_type = "Advanced",
  player_or_team = "Player",
  per_mode = 'Totals'
)$LeagueDashPlayerStats %>%
  as_tibble() %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION, GP, MIN, POSS) %>%
  mutate(
    season = season,
    PLAYER_ID = as.character(PLAYER_ID),
    GP = to_int(GP),
    MIN = to_dbl(MIN),
    POSS = to_dbl(POSS),
  )




# 2) Pull player master ONCE
player_master <- get_player_master_from_playerindex(
  season = NULL,                 # or year_to_season(2025)
  historical = 0
)





pr_rollman_df <- nba_synergyplaytypes(league_id = '00',
                                   per_mode = 'Totals',
                                   play_type = 'PRRollman',
                                   season = year_to_season(most_recent_nba_season() - 1)
)$SynergyPlayType 

pr_rollman_df <- pr_rollman_df %>%
  mutate(
    POSS = as.numeric(POSS),
    PPP = as.numeric(PPP),
    )
  

colnames(pr_rollman_df)

prr_df_annotated <-  df_adv %>%
  left_join(
    player_master, by = c('PLAYER_ID' = 'player_id')
  ) %>%
  left_join(
    pr_rollman_df %>% 
        select(TEAM_ID, PLAYER_ID, PLAY_TYPE, PTS, POSS, PPP) %>%
      rename(PR_ROLL_POSS = POSS), 
    by = c('PLAYER_ID' = 'PLAYER_ID', 
                       'TEAM_ID' = 'TEAM_ID')
  ) %>%
  mutate(
    PR_ROLL_POSS_75 = PR_ROLL_POSS/POSS*75
  ) %>%
  filter(POSS >= 1000) %>%
  filter(MIN > 25)




on_off_ball_df <- get_shooting_metrics_onball_offball_by_season(season_year = 2025)

on_off_ball_df_player <- on_off_ball_df$ptshot_player


colnames(on_off_ball_df_player_annotated)


on_off_ball_df_player_annotated <-  df_adv %>%
  left_join(
    player_master, by = c('PLAYER_ID' = 'player_id')
  ) %>%
  left_join(
    on_off_ball_df_player %>% 
      select(-season_year, -PLAYER_LAST_TEAM_ID, 
             -PLAYER_LAST_TEAM_ABBREVIATION, -PLAYER_NAME),
    by = c('PLAYER_ID' = 'PLAYER_ID')
  ) %>%
  left_join(
    pr_rollman_df %>% 
      select(TEAM_ID, PLAYER_ID, PLAY_TYPE, PTS, POSS, PPP) %>%
      rename(PR_ROLL_POSS = POSS), 
    by = c('PLAYER_ID' = 'PLAYER_ID', 
           'TEAM_ID' = 'TEAM_ID')
  ) %>%
  mutate(
    FG2A_off_ball_75 = FG2A_off_ball/POSS*75,
    FG3A_off_ball_75 = FG3A_off_ball/POSS*75,
    FGA_off_ball_75 = FGA_off_ball/POSS*75,
    PR_ROLL_POSS_75 = PR_ROLL_POSS/POSS*75
  ) %>%
  select(
    1:15,
    FG2A_off_ball_75,
    FG3A_off_ball_75,
    FGA_off_ball_75,
    PR_ROLL_POSS_75,
    PPP
  ) %>%
  filter(POSS >= 1000) %>%
  filter(MIN >= 25)
  



## Video Section

df <- pbp_all %>%
  filter(shot_result == 'Made') %>%
  filter(player_name_i == 'K. Knueppel') %>%
  filter(assist_player_name_initial == 'L. Ball') %>%
  filter(defensive_team_name == 'Bucks') %>%
  filter(!is.na(assist_person_id)) %>%
  filter(shot_distance > 0) %>%
  filter(!str_detect(qualifiers, "fastbreak")) %>%
  filter(!str_detect(qualifiers, "2ndchance"))


library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)

get_video_asset <- function(game_id, event_id) {
  
  url <- paste0(
    "https://stats.nba.com/stats/videoeventsasset?",
    "GameEventID=", event_id,
    "&GameID=", game_id
  )
  
  res <- GET(
    url,
    add_headers(
      "Host" = "stats.nba.com",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:72.0)",
      "Accept" = "application/json, text/plain, */*",
      "Accept-Language" = "en-US,en;q=0.5",
      "Accept-Encoding" = "gzip, deflate, br",
      "x-nba-stats-origin" = "stats",
      "x-nba-stats-token" = "true",
      "Connection" = "keep-alive",
      "Referer" = "https://stats.nba.com/",
      "Pragma" = "no-cache",
      "Cache-Control" = "no-cache"
    )
  )
  
  stop_for_status(res)
  
  out <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  video_urls <- out$resultSets$Meta$videoUrls
  playlist   <- out$resultSets$playlist
  
  if (is.null(video_urls) || length(video_urls) == 0) {
    return(tibble(video = NA_character_, desc = NA_character_))
  }
  
  tibble(
    video = video_urls$lurl[1],
    desc  = playlist$dsc[1]
  )
}


df_with_video <- df %>%
  mutate(
    game_id_chr = as.character(game_id_chr),
    event_num   = as.character(event_num)
  ) %>%
  distinct(game_id_chr, event_num, .keep_all = TRUE) %>%
  mutate(
    video_data = map2(
      game_id_chr,
      event_num,
      ~ tryCatch(
        get_video_asset(.x, .y),
        error = function(e) tibble(video = NA_character_, desc = NA_character_)
      )
    )
  ) %>%
  unnest(video_data)





videos <- df_with_video %>%
  filter(!is.na(video)) %>%
  pull(video)

for (v in videos) {
  browseURL(v)
  Sys.sleep(1)  # 1 second between opens
}








## Viz




alpha_big_point   <- 0.9
alpha_small_point <- 0.25

label_big_size    <- 6
label_small_size  <- 3



library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)

highlight_big   <- c("Kon Knueppel")
highlight_small <- c('Jay Huff', 'Brook Lopez',
                     'Sam Merrill', 'Lauri Markkanen',
                     'Jaylon Tyson', 'Myles Turner',
                     'Karl-Anthony Towns',
                     'Joel Embiid', 'Alex Sarr'
)


team_colors <- read.csv('Data/teamColors.csv') %>%
  mutate(
    TEAM_ID = as.character(TEAM_ID)
  )

viz_df <- on_off_ball_df_player_annotated %>%
  left_join(team_colors %>% select(TEAM_ID, Primary.Color), by = "TEAM_ID") %>%
  mutate(
    # remove Jr. before building short name
    player_name_clean = PLAYER_NAME %>%
      str_remove("\\s+(Jr\\.?|Sr\\.?|II|III|IV|V|VI|VII|VIII|IX|X)$") %>%
      str_squish(),
    first_initial = str_sub(str_extract(player_name_clean, "^[A-Za-z]+"), 1, 1),
    last_name     = str_extract(player_name_clean, "[A-Za-z]+$"),
    player_short_name = paste0(first_initial, ". ", last_name),
    
    point_size = case_when(
      PLAYER_NAME %in% highlight_big ~ 3.0,
      TRUE ~ 1.6
    ),
    
    point_alpha = case_when(
      PLAYER_NAME %in% highlight_big ~ alpha_big_point,
      TRUE ~ alpha_small_point
    ),
    
    label_big   = if_else(PLAYER_NAME %in% highlight_big, player_short_name, NA_character_),
    label_small = if_else(PLAYER_NAME %in% highlight_small, player_short_name, NA_character_)
  )

colnames(viz_df)

# player_height_inches, PPP

p <- ggplot(viz_df, aes(x = FG3A_off_ball_75, y = PR_ROLL_POSS_75)) +
  geom_point(
    aes(
      color = Primary.Color,
      size  = point_size,
      alpha = point_alpha
    )
  ) +
  scale_color_identity() +
  scale_size_identity() +
  scale_alpha_identity() +
  geom_text_repel(
    aes(label = label_big, color = Primary.Color),
    na.rm = TRUE,
    size = label_big_size,
    fontface = "bold",
    box.padding = 0.25,
    point.padding = 0.2,
    min.segment.length = 0
  ) +
  
  geom_text_repel(
    aes(label = label_small, color = Primary.Color),
    na.rm = TRUE,
    size = label_small_size,
    box.padding = 0.2,
    point.padding = 0.15,
    min.segment.length = 0
  ) +
  
  labs(
    x = "Off Ball Three Point Attempts/75",
    y = "Roll Man Possessions / 75",
    title = "<span style='color:#002B5C;'>Kon Knueppel</span> | Stretch Big?",
    subtitle = "Off Ball Three Point Attempts / 75 vs Roll Man Possessions / 75",
    caption = "Off Ball defined as touch time <2 seconds\n2025–26 |  Min. 1000 Poss / 25+ MPG | Data: NBA API via hoopR | Viz: @SaurabhOnTap"
  ) +
  
  theme_saurabh() +
  
  # --- very light grid overlay ---
  theme(
    plot.title = element_markdown(face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
  ) + theme(plot.margin = margin(10, 15, 10, 10))


p

ggsave(
  "kon.png",
  plot = p,
  width = 6,
  height = 6,
  dpi = 300,
  device = ragg::agg_png
)





### Top Combos



assist_combos <- pbp_all %>%
  filter(shot_result == 'Made') %>%
  filter(!is.na(assist_person_id)) %>%
  group_by(possession_team_id, possession_team_name, 
           assist_person_id, assist_player_name_initial,
           player1_id, player_name_i)  %>%
  summarise(
    made_shots = n()
  ) %>%
  ungroup() %>%
  mutate(
    possession_team_id = as.character(possession_team_id),
    assist_person_id = as.character(assist_person_id)
  )



# 2) Count shared possessions for each alley-oop combo
assist_combos_with_shared <- assist_combos %>%
  mutate(
    assist_person_id = as.character(assist_person_id),
    player1_id       = as.character(player1_id),
    possession_team_id = as.character(possession_team_id)
  ) %>%
  mutate(
    shared_possessions = pmap_int(
      list(possession_team_id, assist_person_id, player1_id),
      function(team_id, passer_id, dunker_id) {
        poss_lineups %>%
          filter(possession_team_id == team_id) %>%
          filter(has_id(posteam_player_ids, passer_id),
                 has_id(posteam_player_ids, dunker_id)) %>%
          summarise(n = n_distinct(paste(game_id_chr, possession_id, sep = "_"))) %>%
          pull(n)
      }
    )
  ) %>%
  mutate(
    made_shots_75 = made_shots/shared_possessions*75
  )


df <- assist_combos_with_shared %>%
  filter(shared_possessions != 0)
