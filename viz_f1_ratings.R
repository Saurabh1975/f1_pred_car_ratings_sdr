library(ggplot2)
library(ggtext)
library(extrafont)
library(gtable)
library(gtExtras)

# Function: theme_saurabh
# Purpose: Personal, minimalist theme
theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=14, family = 'Roboto Black', color = '#101010'),
    plot.subtitle=element_text(size=10, family = 'Roboto Slab'),
    plot.caption=element_text(size=7, family = 'Roboto Mono'),
    panel.background = element_blank(),
    
  )
}



### Constructor Viz


plot_constructor_rapm_history <- function(selected_constructor){
  
  print(selected_constructor)
  
  selected_constructor_name = constructors  %>% 
    filter(constructor_id == selected_constructor) %>% pull(name)
  
  
  start_date = as.Date('2014-01-01')
  end_date = max(constructor_rapm_history$model_date)
  
  
  
  rapm_lower_limit = min((constructor_rapm_history %>% filter(model_date > start_date) %>% pull(rapm_blended)))
  rapm_upper_limit = max((constructor_rapm_history %>% filter(model_date > start_date) %>% pull(rapm_blended)))
  
  df_constructor <- constructor_rapm_history %>%
    filter(parent_constructor_id == selected_constructor)  %>%
    filter(model_date >= start_date) %>%
    mutate(rapm_blended_lower_limit =  rapm_blended - (rapm_error*1.96),
           rapm_blended_upper_limit =  rapm_blended + (rapm_error*1.96))
  
  
  season_start_end <- results_unfiltered %>% 
    group_by(season) %>%
    summarize(season_start = as.Date(min(date)),
              season_end = as.Date(max(date)),
              season_start_round = min(round_overall) - 1,
              season_end_round = max(round_overall)) %>%
    filter(season >= min(df_constructor$season), season <= max(df_constructor$season))
  
  
  
  df_constructor_last_point <- df_constructor %>%
    group_by(parent_constructor_id) %>%
    slice(n()) 
  
  
  
  
  # Create Line Plot (Season/Round)
  g <- ggplot(df_constructor, aes(x = overall_round, y = rapm_blended, color = primary_color, 
                                  group = parent_constructor_id)) +
    #Actual Line Chart
    geom_ribbon(aes(ymin = rapm_blended_lower_limit, ymax = rapm_blended_upper_limit, fill = primary_color), 
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(aes(y = rapm), alpha = 0.25) +
    #Season Breaks
    geom_vline(data = season_start_end, aes(xintercept = season_start_round), 
               color = '#404040', linetype = 'dashed') +
    geom_text(data = season_start_end %>% filter(season > 2013), 
              aes(x = season_start_round + (season_end_round - season_start_round)/2, label = season), 
              y= Inf,  color = '#404040', 
              inherit.aes = FALSE, vjust = 1, size = 3) +
    scale_color_identity() +
    scale_fill_identity() +
    xlim(c(min(season_start_end$season_start_round) - 1, 
           max(season_start_end$season_start_round) + 15)) +
    ylim(c(rapm_lower_limit, rapm_upper_limit + (rapm_upper_limit - rapm_lower_limit)*.1)) +
    theme_saurabh() +
    labs(
      title = paste0("<span style='color:", df_constructor$primary_color[1], ";'>", selected_constructor_name, "</span> - Constructor Rating"),
      subtitle = paste0("V6 Hybrid Era 2014 to ", end_date),
      x = "",
      y = "Rating"
    ) +
    theme(
      axis.text.x = element_blank(), # Remove x-axis tick labels
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      plot.title = element_markdown() # Enable markdown for title
    )
  
  
  ggsave(plot = g, filename = paste0("f1dataR - Exports/Visuals/Ratings/Trend/Constructor/", selected_constructor, ".png"),
         width = 8, height = 4)
  
  
  
}



current_constructor_list <- constructor_rapm_history %>%
  ungroup() %>%
  filter(model_date == max(model_date)) %>%
  pull(parent_constructor_id)


for(constructor in current_constructor_list){
  
  plot_constructor_rapm_history(constructor)
  
}




selected_driver = 'bottas'

### Driver Viz

plot_driver_rapm_history <- function(selected_driver){
  
  
  print(selected_driver)
  
  
  selected_driver_name = drivers  %>% filter(driver_id == selected_driver) %>% pull(driver_name)
  
  
  start_date = as.Date('2014-01-01')
  end_date = max(driver_rapm_history$model_date)
  
  
  
  
  rapm_lower_limit = min((constructor_rapm_history %>% filter(model_date > start_date) %>% pull(rapm_blended)))
  rapm_upper_limit = max((constructor_rapm_history %>% filter(model_date > start_date) %>% pull(rapm_blended)))
  
  df_driver <- driver_rapm_history %>%
    filter(driver_id == selected_driver)  %>%
    filter(model_date >= start_date) %>%
    mutate(rapm_blended_lower_limit =  rapm_blended - (rapm_error*1.96),
           rapm_blended_upper_limit =  rapm_blended + (rapm_error*1.96))
  
  
  season_start_end <- results_unfiltered %>% 
    group_by(season) %>%
    summarize(season_start = as.Date(min(date)),
              season_end = as.Date(max(date)),
              season_start_round = min(round_overall) - 1,
              season_end_round = max(round_overall)) %>%
    filter(season >= min(df_driver$season), season <= max(df_driver$season))
  
  
  
  df_driver_last_point <- df_driver %>%
    group_by(driver_id) %>%
    slice(n())  # Select the last row for each group
  
  driver_stints_viz <- driver_stints %>% 
    filter(driver_id == selected_driver, stint_end > start_date) %>%
    group_by(parent_constructor_id, parent_constructor_name) %>%
    arrange(stint_start) %>%
    summarize(stint_start = min(stint_start),
              stint_end = max(stint_end),
              stint_start_overall_round = min(stint_start_overall_round),
              stint_end_overall_round = max(stint_end_overall_round),
              primary_color = last(primary_color)) %>%
    mutate(stint_start = max(min(df_driver$model_date), stint_start),
           stint_start_overall_round = 
             max(min(df_driver$overall_round), stint_start_overall_round)
    ) %>%
    ungroup() %>%
    arrange(stint_start)
  
  
  other_drivers <- driver_stints_viz %>%
    rename(stint_start_overall_round_main = stint_start_overall_round,
           stint_end_overall_round_main = stint_end_overall_round) %>%
    dplyr::select(parent_constructor_id, stint_start_overall_round_main, 
                  stint_end_overall_round_main) %>%
    left_join(driver_rapm_history, 
              by = c('parent_constructor_id' = 'parent_constructor_id')) %>%
    filter(stint_start_overall_round_main <= overall_round,
           stint_end_overall_round_main >= overall_round) %>%
    filter(driver_id != selected_driver)
  
  
  
  
  # Create Line Plot (Season/Round)
  g <- ggplot(df_driver, aes(x = overall_round, y = rapm_blended, color = primary_color, group = driver_id)) +
    geom_ribbon(aes(ymin = rapm_blended_lower_limit, ymax = rapm_blended_upper_limit, fill = primary_color), 
                alpha = 0.15, color = NA) +
    #Actual Line Chart
    geom_line(linewidth = 1) +
    #geom_line(data = other_drivers, linewidth = 0.25, color = '#999999') +
    geom_point(aes(y = rapm), alpha = 0.25) +
    #Season Breaks
    geom_vline(data = season_start_end, aes(xintercept = season_start_round), 
               color = '#404040', linetype = 'dashed') +
    geom_text(data = season_start_end %>% filter(season > 2013), 
              aes(x = season_start_round + (season_end_round - season_start_round)/2, label = season), 
              y= Inf,  color = '#404040', 
              inherit.aes = FALSE, vjust = 1, size = 3) +
    #Constructor Headings
    geom_segment(data = driver_stints_viz, 
                 aes(x = stint_start_overall_round, 
                     xend = stint_end_overall_round, color = primary_color), 
                 y = rapm_upper_limit, yend = rapm_upper_limit, linewidth = 3,
                 inherit.aes =  FALSE) +
    geom_text(data = driver_stints_viz, 
              aes(x = (stint_end_overall_round - stint_start_overall_round)/2 + stint_start_overall_round, 
                  label = parent_constructor_name, color = primary_color), 
              y = rapm_upper_limit ,size = 3, vjust = -1,
              inherit.aes =  FALSE) +
    #Label end of Line
    #geom_text(data = df_driver_last_point, aes(label = driver_id),
    #          hjust = -0.1, vjust = -0.5, size = 3) +  # Add text annotations for last points
    scale_color_identity() +
    scale_fill_identity() +
    xlim(c(min(season_start_end$season_start_round) - 1, 
           max(season_start_end$season_start_round) + 15)) +
    ylim(c(rapm_lower_limit, rapm_upper_limit + (rapm_upper_limit - rapm_lower_limit)*.1)) +
    theme_saurabh() +
    labs(title = paste0(selected_driver_name, " - Driver Rating"),
         subtitle = paste0("V6 Hybrid Era 2014 to ", end_date),
         x = "",
         y = "Rating") +
    theme(
      axis.text.x = element_blank(), # Remove x-axis tick labels
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      plot.subtitle = element_markdown() # Enable markdown for title
    )
  
  
  ggsave(plot = g, filename = paste0("f1dataR - Exports/Visuals/Ratings/Trend/Driver/", selected_driver, ".png"),
         width = 8, height = 4)
  
  
  
}






current_driver_list <- driver_rapm_history %>%
  ungroup() %>%
  filter(model_date == max(model_date)) %>%
  pull(driver_id)

for(driver in current_driver_list){
  
  plot_driver_rapm_history(driver)
  
}






### Point in Time Rankings



get_overall_rankings_tbl <- function(current_overall_rankings){
  
  
  tbl <- current_overall_rankings %>%
    gt() %>%
    tab_header(
      title = md("**Driver/Constructor Composite Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      driver_headshot_url  = "Driver",
      driver_name = "",
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended_driver = "Driver Rating",
      rapm_blended_constructor = "Constructor Rating",
      rapm_blended_overall = "Overall Rating"
    )  %>%
    gt_img_rows(columns = driver_headshot_url, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    fmt_number(
      columns = c(rapm_blended_driver, rapm_blended_constructor, 
                  rapm_blended_overall),
      decimals = 1,
      use_seps = FALSE,
      force_sign = TRUE
    ) %>%
    tab_style(
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono")),
          weight = "bold"
        )
      ),
      locations = cells_body(columns = vars(rapm_blended_driver, rapm_blended_constructor, rapm_blended_overall))
    ) %>%
    tab_style(
      style = list(
        cell_text(font = c(
          google_font(name = "Roboto")))),
      locations = cells_body(columns = c(parent_constructor_name, driver_name))) %>%
    tab_options(
      table.font.names = "Roboto", 
      table.font.size = 14,
      heading.title.font.size = 20,
      heading.subtitle.font.size = 13,
      column_labels.font.size = 13,
      column_labels.font.weight = 'bold',
      row_group.background.color = '#fAfAfA',
      row_group.font.size = 11,
      row_group.font.weight = 'bold',
      data_row.padding = px(5)
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) %>%
    gt_add_divider(columns = c(constructor_img_url, rapm_blended_constructor), color = '#808080', weight = 0.75) %>%
    cols_align(
      align = "center",
      columns = c(constructor_img_url, rapm_blended_driver, rapm_blended_constructor,
                  rapm_blended_overall)
    ) %>%
    data_color(
      columns = vars(c(rapm_blended_driver, rapm_blended_constructor)),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-4.5, 4.5)
      ),
      apply_to = "text"
    ) %>%
    data_color(
      columns = vars(rapm_blended_overall),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-8, 8)
      )
    ) 
  
  return(tbl)
  
  
}



get_driver_rankings_tbl <- function(current_driver_rankings){
  
  tbl_driver <- current_driver_rankings %>%
    gt() %>%
    tab_header(
      title = md("**Driver Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      driver_headshot_url  = "Driver",
      driver_name = "",
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended = "Driver Rating"
    )  %>%
    gt_img_rows(columns = driver_headshot_url, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    fmt_number(
      columns = c(rapm_blended),
      decimals = 1,
      use_seps = FALSE,
      force_sign = TRUE
    ) %>%
    tab_style(
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono")),
          weight = "bold"
        )
      ),
      locations = cells_body(columns = vars(rapm_blended))
    ) %>%
    tab_style(
      style = list(
        cell_text(font = c(
          google_font(name = "Roboto")))),
      locations = cells_body(columns = c(parent_constructor_name, driver_name))) %>%
    tab_options(
      table.font.names = "Roboto", 
      table.font.size = 14,
      heading.title.font.size = 20,
      heading.subtitle.font.size = 13,
      column_labels.font.size = 13,
      column_labels.font.weight = 'bold',
      row_group.background.color = '#fAfAfA',
      row_group.font.size = 11,
      row_group.font.weight = 'bold',
      data_row.padding = px(5)
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) %>%
    cols_align(
      align = "center",
      columns = c(constructor_img_url,rapm_blended)
    ) %>%
    data_color(
      columns = vars(rapm_blended),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-3, 3)
      ),
      apply_to = "text"
    ) 
  
  return(tbl_driver)
}


get_constructor_rankings_tbl <- function(current_constructor_rankings){
  
  tbl_constructor <- current_constructor_rankings %>%
    gt() %>%
    tab_header(
      title = md("**Constructor Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended = "Constructor Rating"
    )  %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    fmt_number(
      columns = c(rapm_blended),
      decimals = 1,
      use_seps = FALSE,
      force_sign = TRUE
    ) %>%
    tab_style(
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono")),
          weight = "bold"
        )
      ),
      locations = cells_body(columns = vars(rapm_blended))
    ) %>%
    tab_style(
      style = list(
        cell_text(font = c(
          google_font(name = "Roboto")))),
      locations = cells_body(columns = c(parent_constructor_name))) %>%
    tab_options(
      table.font.names = "Roboto", 
      table.font.size = 14,
      heading.title.font.size = 20,
      heading.subtitle.font.size = 13,
      column_labels.font.size = 13,
      column_labels.font.weight = 'bold',
      row_group.background.color = '#fAfAfA',
      row_group.font.size = 11,
      row_group.font.weight = 'bold',
      data_row.padding = px(5)
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) %>%
    cols_align(
      align = "center",
      columns = c(constructor_img_url,rapm_blended)
    ) %>%
    data_color(
      columns = vars(rapm_blended),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-4, 4)
      ),
      apply_to = "text"
    ) 
  
  return(tbl_constructor)
  
}



#Set date from which to pull rankings
latest_date = max(constructor_rapm_history  %>% 
                    filter(season < 2024) %>% pull(model_date))

latest_race = schedule  %>% filter(date == latest_date) 
latest_race = paste0(latest_race$race_name, ", ", latest_race$season)

# Get rankings for date above

current_constructor_rankings <- constructor_rapm_history %>%
  ungroup() %>%
  filter(model_date == latest_date)  %>%
  mutate(constructor_img_url = paste0('Images/Constructor/png/', parent_constructor_id, '.png')) %>%
  dplyr::select(constructor_img_url, parent_constructor_name, 
                rapm_blended) %>%
  arrange(-rapm_blended) 


current_driver_rankings <- driver_rapm_history %>%
  ungroup() %>%
  filter(model_date == latest_date) %>%
  mutate(driver_headshot_url = paste0('Images/Drivers/png/', driver_id, '.png'),
         constructor_img_url = paste0('Images/Constructor/png/', parent_constructor_id, '.png')) %>%
  dplyr::select(driver_headshot_url, driver_name,
                constructor_img_url, parent_constructor_name, 
                rapm_blended) %>%
  arrange(-rapm_blended) 


current_overall_rankings <- driver_rapm_history %>%
  ungroup() %>%
  filter(model_date == latest_date) %>%
  mutate(driver_headshot_url = paste0('Images/Drivers/png/', driver_id, '.png'),
         constructor_img_url = paste0('Images/Constructor/png/', parent_constructor_id, '.png')) %>%
  dplyr::select(driver_headshot_url, driver_name,
                constructor_img_url, parent_constructor_name, 
                rapm_blended) %>%
  inner_join(current_constructor_rankings %>% dplyr::select(parent_constructor_name, rapm_blended),
             by = 'parent_constructor_name') %>%
  rename(rapm_blended_driver = rapm_blended.x,
         rapm_blended_constructor = rapm_blended.y) %>%
  mutate(rapm_blended_overall = rapm_blended_driver + rapm_blended_constructor) %>%
  arrange(-rapm_blended_overall) 


overall_rankings_tbl <- get_overall_rankings_tbl(current_overall_rankings)

constructor_rankings_tbl <- get_constructor_rankings_tbl(current_constructor_rankings)

driver_rankings_tbl <- get_driver_rankings_tbl(current_driver_rankings)



gtsave(data = overall_rankings_tbl, 
       filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/overall_rankings - ", latest_date, ".png"))


gtsave(data = constructor_rankings_tbl, 
       filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/constructor_rankings - ", latest_date, ".png"))


gtsave(data = driver_rankings_tbl, 
       filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/driver_rankings - ", latest_date, ".png"))




