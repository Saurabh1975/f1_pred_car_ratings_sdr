library(dplyr)



pbp_df <- load_pbp(seasons = c(2010:2024))


unique(pbp_df$posteam)




pbp_df <- load_pbp(seasons = c(2025))
pbp_metrics_df_export_df_current_season <- pbp_df %>%
  filter(play_type %in% c('pass', 'run')) %>%
  select(
    posteam, season, game_id, play_id, play_type,
    #Metrics
    qb_dropback,  epa
  ) 





# 1) Build team-season means from history (e.g., 2022-2024)
hist_team <- pbp_ftn_df_export_df %>%
  filter(play_type %in% c("pass","run")) %>%
  group_by(season, posteam) %>%
  summarise(epa_season = mean(epa, na.rm = TRUE), .groups = "drop")

# 2) Lag by one season to get EPA_{t-1}
hist_pairs <- hist_team %>%
  mutate(prev_season = season - 1L) %>%
  inner_join(hist_team %>% rename(prev_season = season, epa_prev = epa_season),
             by = c("posteam", "prev_season"))

# 3) Fit EPA_t ~ α + ρ * EPA_{t-1}
fit <- lm(epa_season ~ epa_prev, data = hist_pairs)
alpha_hat <- coef(fit)[1]
rho_hat   <- coef(fit)[2]



# 4) Get last-season EPA for each current team
current_last <- hist_team %>%
  group_by(posteam) %>%
  filter(season == max(season)) %>%  # last completed season in your history
  ungroup() %>%
  select(posteam, epa_prev = epa_season)

# 5) Build team-specific prior mean μ0_team
mu0_league <- mean(hist_team$epa_season, na.rm = TRUE)  # fallback if missing
mu0_team <- current_last %>%
  mutate(mu0_team = alpha_hat + rho_hat * epa_prev)

# 6) Choose prior strength n0 (e.g., variance-ratio)
# sigma_play: per-play SD; sigma_between: SD of team-season means
sigma_between <- sd(hist_team$epa_season, na.rm = TRUE)
sigma_play <- pbp_ftn_df_export_df %>%
  filter(play_type %in% c("pass","run")) %>%
  summarise(sd_play = sd(epa, na.rm = TRUE)) %>% pull(sd_play)
n0 <- as.numeric((sigma_play^2) / (sigma_between^2))
n0 <- max(n0, 1)  # guardrail

# 7) Live posterior for current season (update per team as plays accumulate)
current_team_epa <- pbp_metrics_df_export_df_current_season %>%
  filter(play_type %in% c("pass","run")) %>%
  group_by(season, posteam) %>%
  summarise(n = n(), xbar = mean(epa, na.rm = TRUE), .groups = "drop") %>%
  left_join(mu0_team, by = "posteam") %>%
  mutate(mu0_team = ifelse(is.na(mu0_team), mu0_league, mu0_team),
         post_mean = (n0*mu0_team + n*xbar) / (n0 + n),
         post_se   = sigma_play / sqrt(n0 + n),
         lo80_post = post_mean - 1.282 * post_se,
         hi80_post = post_mean + 1.282 * post_se)




viz_df <- current_team_epa %>%
  left_join(
    teams_colors_logos, by = c('posteam' = 'team_abbr')
  ) %>%
  # reorder posteam factor by xbar so highest is at top
  mutate(posteam = forcats::fct_reorder(posteam, post_mean, .desc = FALSE))


colnames(viz_df)
viz_df$team_wordmark[1]

viz_df %>%
  ggplot(aes(y = posteam)) +
  geom_rect(aes(xmin = post_mean - 0.674*post_se, xmax = post_mean + 0.674*post_se,
                ymin = as.numeric(posteam) - 0.45,ymax = as.numeric(posteam) + 0.45,
                fill = team_color),
            alpha = 0.15) +
  geom_rect(aes(xmin = post_mean - 1.282*post_se, xmax = post_mean + 1.282*post_se,
                ymin = as.numeric(posteam) - 0.45,ymax = as.numeric(posteam) + 0.45,
                fill = team_color),
            alpha = 0.15) +
  # vertical line for posterior mean
  geom_segment(aes(x = post_mean, xend = post_mean,
                   y = as.numeric(posteam) - 0.45,
                   yend = as.numeric(posteam) + 0.45,
                   color = team_color),
               linewidth = 0.5) +
  geom_point(aes(x = xbar, color = team_color), size = 3) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_saurabh()





mu0_team <- current_last %>%
  mutate(mu0_team = alpha_hat + rho_hat * epa_prev)  # team-specific prior

checkpoints <- pbp_metrics_df_export_df_current_season %>%
  filter(play_type %in% c("pass","run")) %>%
  arrange(season, posteam, game_id, play_id) %>%
  group_by(season, posteam) %>%
  mutate(n = row_number(),
         xbar = cummean(epa)) %>%
  ungroup() %>%
  left_join(mu0_team, by = "posteam") %>%   # <-- attach prior mean
  mutate(
    mu0_team = ifelse(is.na(mu0_team), mu0_league, mu0_team),  # fallback if missing
    post_mean = (n0*mu0_team + n*xbar) / (n0 + n),
    post_se   = sigma_play / sqrt(n0 + n),
    lo80_post = post_mean - 1.282*post_se,
    hi80_post = post_mean + 1.282*post_se
  )



checkpoints %>%
  filter(posteam == "BAL") %>%
  ggplot(aes(x = n, y = post_mean)) +
  geom_ribbon(aes(ymin = lo80_post, ymax = hi80_post), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "BAL Posterior EPA/play (2025 season)",
    x = "Plays observed",
    y = "Posterior mean EPA/play with 80% CI"
  ) +
  theme_minimal(base_size = 12)





library(ggimage)

viz_df %>%
  mutate(y_num = as.numeric(posteam)) %>%
  ggplot(aes(y = y_num)) +
  # 50% band
  geom_rect(aes(xmin = post_mean - 0.674*post_se, xmax = post_mean + 0.674*post_se,
                ymin = y_num - 0.45, ymax = y_num + 0.45,
                fill = team_color),
            alpha = 0.15) +
  # 80% band
  geom_rect(aes(xmin = post_mean - 1.282*post_se, xmax = post_mean + 1.282*post_se,
                ymin = y_num - 0.45, ymax = y_num + 0.45,
                fill = team_color),
            alpha = 0.15) +
  # vertical line for posterior mean
  geom_segment(aes(x = post_mean, xend = post_mean,
                   y = y_num - 0.45, yend = y_num + 0.45,
                   color = team_color),
               linewidth = 0.5) +
  # observed mean as dot
  geom_point(aes(x = xbar, color = team_color), size = 3) +
  # add images at left margin
  geom_image(aes(x = min(post_mean) - 0.15,   # push logos a bit left of the bands
                 image = team_wordmark),
             size = 0.08, by = "height", inherit.aes = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_y_continuous(breaks = NULL) +    # remove numeric y axis ticks
  theme_saurabh() +
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())



colnames(viz_df)

viz_df2 <- viz_df %>%
  mutate(y_num = as.numeric(posteam))

# Put logos a bit left of the left band edge
x_left <- min(viz_df2$post_mean - 1.282*viz_df2$post_se) - .05   # tweak this offset for your scale
x_left <- min(viz_df2$post_mean - 1.282*viz_df2$post_se, viz_df2$xbar) - .05   # tweak this offset for your scale

ggplot(viz_df2, aes(y = y_num)) +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype = 'dashed') +
  # 50% band
  geom_rect(aes(xmin = post_mean - 0.674*post_se, xmax = post_mean + 0.674*post_se,
                ymin = y_num - 0.45, ymax = y_num + 0.45, fill = team_color),
            alpha = 0.15) +
  # 80% band
  geom_rect(aes(xmin = post_mean - 1.282*post_se, xmax = post_mean + 1.282*post_se,
                ymin = y_num - 0.45, ymax = y_num + 0.45, fill = team_color),
            alpha = 0.15) +
  # posterior mean line
  geom_segment(aes(x = post_mean, xend = post_mean,
                   y = y_num - 0.45, yend = y_num + 0.45, color = team_color),
               linewidth = 0.5) +
  # observed mean point
  geom_point(aes(x = xbar, color = team_color), size = 2) +
  # team wordmarks as y labels
  geom_image(data = viz_df2,
             aes(x = x_left, y = y_num, image = team_logo_wikipedia),
             size = 0.03, by = "height", inherit.aes = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(-0.4, 0.3, by = 0.1)) +   # <–– custom x-axis ticks
  scale_y_continuous(breaks = NULL) +
  labs(title = '2025 Stablized Offnsive EPA/Play (W1)',
       subtitle = "Observed EPA/play (•) vs. Bayesian posterior distrubtion (|, band)",
       caption = "Posterior estimates combine 2024 team EPA/play priors with 2010–2024 play-level data. 
             Intervals represent uncertainty in each team’s season-long EPA/play tendency.",
       x = 'EPA/Play', y = NULL) +
  theme_saurabh() 



  ggsave('stable_epa_2025_w1.png')