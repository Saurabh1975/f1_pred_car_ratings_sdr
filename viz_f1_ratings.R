library(ggplot2)
library(ggtext)
library(extrafont)
library(gtable)
library(gtExtras)
library(webshot2)

# Function: theme_saurabh
# Purpose: Personal, minimalist theme
theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=18, family = 'Roboto Black', color = '#101010'),
    plot.subtitle=element_text(size=12, family = 'Roboto Slab'),
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
  
  
  ggsave(plot = g, filename = paste0("f1dataR - Exports/Visuals/Ratings/Trend/Constructor/", selected_constructor, ".jpg"),
         width = 8, height = 4, dpi = 1200)
  
  
  
}



current_constructor_list <- constructor_rapm_history %>%
  ungroup() %>%
  filter(model_date == max(model_date)) %>%
  pull(parent_constructor_id)


for(constructor in current_constructor_list){
  
  plot_constructor_rapm_history(constructor)
  
}




### Driver Viz

selected_driver = 'ricciardo'

plot_driver_rapm_history <- function(selected_driver){
  
  
  
  selected_driver_name = drivers  %>% filter(driver_id == selected_driver) %>% pull(driver_name)
  
  
  start_date = as.Date('2014-01-01')
  end_date = max(driver_rapm_history$model_date)
  
  
  
  
  rapm_lower_limit = min((constructor_rapm_history %>% filter(model_date > start_date) %>% pull(rapm_blended)))
  rapm_upper_limit = max((constructor_rapm_history %>% filter(model_date > start_date) %>% pull(rapm_blended)))
  
  df_driver <- driver_rapm_history %>%
    filter(driver_id == selected_driver)  %>%
    filter(model_date >= start_date) %>%
    mutate(rapm_blended_lower_limit =  rapm_blended - (rapm_error*1.96),
           rapm_blended_upper_limit =  rapm_blended + (rapm_error*1.96)) %>%
    mutate(id = consecutive_id(primary_color))
  
  
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
  g <- ggplot(df_driver, aes(x = overall_round, y = rapm, color = primary_color, group = driver_id)) +
    geom_ribbon(aes(ymin = rapm_blended_lower_limit, ymax = rapm_blended_upper_limit, 
                    fill = primary_color, group = id), 
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
    labs(title = paste0(selected_driver_name, " - Driver Rating (Raw)"),
         subtitle = paste0("V6 Hybrid Era 2014 to ", end_date),
         x = "",
         y = "Rating") +
    theme(
      axis.text.x = element_blank(), # Remove x-axis tick labels
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      plot.subtitle = element_markdown() # Enable markdown for title
    )
  
  
  ggsave(plot = g, filename = paste0("f1dataR - Exports/Visuals/Ratings/Trend/Driver/", selected_driver, " - raw.jpg"),
         width = 8, height = 4, dpi = 1200)
  
  
  
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
    mutate(rapm_blended_driver_error = paste0("± ", abs(round(rapm_blended_driver_error, 2))),
           rapm_blended_constructor_error = paste0("± ", abs(round(rapm_blended_constructor_error, 2))),
           rapm_blended_overall_error = paste0("± ", abs(round(rapm_blended_overall_error, 2)))) %>%
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
      rapm_blended_driver_error = "",
      rapm_blended_constructor = "Constructor Rating",
      rapm_blended_constructor_error = "",
      rapm_blended_overall = "Overall Rating",
      rapm_blended_overall_error = ""
    )  %>%
    gt_img_rows(columns = driver_headshot_url, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended_driver, col2 = rapm_blended_driver_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    gt_merge_stack(col1 = rapm_blended_constructor, col2 = rapm_blended_constructor_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    fmt_number(
      columns = c(rapm_blended_driver, rapm_blended_constructor, 
                  rapm_blended_overall),
      decimals = 2,
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
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono"))
        )
      ),
      locations = cells_body(columns = vars(rapm_blended_overall_error))
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) %>%
    gt_add_divider(columns = c(constructor_img_url, rapm_blended_constructor), 
                   color = '#808080', weight = 0.75) %>%
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
    mutate(rapm_error = paste0("± ", abs(round(rapm_error, 2)))) %>%
    gt() %>%
    tab_header(
      title = md("**Driver Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      driver_headshot_url  = "Driver",
      driver_name = "",
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended = "Driver Rating",
      rapm_error = ""
    )  %>%
    gt_img_rows(columns = driver_headshot_url, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended, col2 = rapm_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    fmt_number(
      columns = c(rapm_blended),
      decimals = 2,
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
        cell_text(
          font = c(google_font(name = "Roboto Mono")),
        )
      ),
      locations = cells_body(columns = vars(rapm_error))
    ) %>%
    tab_style(
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono")),
        )
      ),
      locations = cells_body(columns = vars(rapm_error))
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
        domain = c(-3.5, 3.5)
      ),
      apply_to = "text"
    ) 
  
  return(tbl_driver)
}


get_constructor_rankings_tbl <- function(current_constructor_rankings){
  
  tbl_constructor <- current_constructor_rankings %>%
    mutate(rapm_error = paste0("± ", abs(round(rapm_error, 2)))) %>%
    gt() %>%
    tab_header(
      title = md("**Constructor Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended = "Constructor Rating",
      rapm_error = ""
    )  %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended, col2 = rapm_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
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
                     pull(model_date))

#filter(season < 2024) %>%

latest_race = schedule  %>% filter(date == latest_date) 
latest_race = paste0(latest_race$race_name, ", ", latest_race$season)

# Get rankings for date above

direct_path <- 'C:/Users/saurabh.rane/OneDrive - Slalom/NBA/f1_pred_car_ratings_sdr/'


current_constructor_rankings <- constructor_rapm_history %>%
  ungroup() %>%
  filter(model_date == latest_date)  %>%
  mutate(constructor_img_url = paste0(direct_path,'supplementary_data/Images/Constructor/png/', parent_constructor_id, '.png')) %>%
  dplyr::select(constructor_img_url, parent_constructor_name, 
                rapm_blended, rapm_error) %>%
  arrange(-rapm_blended) 


current_driver_rankings <- driver_rapm_history %>%
  ungroup() %>%
  filter(model_date == latest_date) %>%
  mutate(driver_headshot_url = paste0(direct_path,'supplementary_data/Images/Drivers/png/', driver_id, '.png'),
         constructor_img_url = paste0(direct_path,'supplementary_data/Images/Constructor/png/', parent_constructor_id, '.png')) %>%
  dplyr::select(driver_headshot_url, driver_name,
                constructor_img_url, parent_constructor_name, 
                rapm_blended, rapm_error) %>%
  arrange(-rapm_blended) 


current_overall_rankings <- driver_rapm_history %>%
  ungroup() %>%
  filter(model_date == latest_date) %>%
  mutate(driver_headshot_url = paste0(direct_path,'supplementary_data/Images/Drivers/png/', driver_id, '.png'),
         constructor_img_url = paste0(direct_path,'supplementary_data/Images/Constructor/png/', parent_constructor_id, '.png')) %>%
  dplyr::select(driver_headshot_url, driver_name,
                constructor_img_url, parent_constructor_name, 
                rapm_blended, rapm_error) %>%
  inner_join(current_constructor_rankings %>% dplyr::select(parent_constructor_name, rapm_blended, rapm_error),
             by = 'parent_constructor_name') %>%
  rename(rapm_blended_driver = rapm_blended.x,
         rapm_blended_driver_error = rapm_error.x,
         rapm_blended_constructor = rapm_blended.y,
         rapm_blended_constructor_error = rapm_error.y) %>%
  mutate(rapm_blended_overall = rapm_blended_driver + rapm_blended_constructor,
         rapm_blended_overall_error = sqrt(rapm_blended_driver_error^2 + rapm_blended_constructor_error^2)) %>%
  arrange(-rapm_blended_overall) 


overall_rankings_tbl <- get_overall_rankings_tbl(current_overall_rankings )

constructor_rankings_tbl <- get_constructor_rankings_tbl(current_constructor_rankings)

driver_rankings_tbl <- get_driver_rankings_tbl(current_driver_rankings)



gtsave_extra(data = overall_rankings_tbl, 
       filename = paste0("f1dataR - Exports/overall_rankings - ", latest_date, ".png"), 
       zoom = 10)

gtsave(data = overall_rankings_tbl, 
       filename = paste0("f1dataR - Exports/overall_rankings - ", latest_date, ".html"))


gtsave(data = constructor_rankings_tbl, 
       filename = paste0("f1dataR - Exports/constructor_rankings - ", latest_date, ".png"))


gtsave(data = driver_rankings_tbl, 
       filename = paste0("f1dataR - Exports/driver_rankings - ", latest_date, ".png"))














#Two Column Tables

#Overall Rankings

get_overall_rankings_split_tbl <- function(current_overall_rankings){
  
  df1 <- current_overall_rankings[1:10, ]
  df2 <- current_overall_rankings[11:20, ]
  
  # Reset the index for both dataframes
  df1 <- df1 %>% mutate(index = row_number())
  df2 <- df2 %>% mutate(index = row_number())
  
  # Rename columns in df2 with _2 suffix
  colnames(df2) <- paste0(colnames(df2), "_2")
  
  # Join the two dataframes on the new index column
  current_overall_rankings_split <- df1 %>% 
    inner_join(df2, by = c("index" = "index_2")) %>%
    dplyr::select(-index)  # Remove the index column if not needed
  
  
  overall_rankings_tbl_split <- current_overall_rankings_split %>%
    mutate(rapm_blended_driver_error = paste0("± ", abs(round(rapm_blended_driver_error, 2))),
           rapm_blended_constructor_error = paste0("± ", abs(round(rapm_blended_constructor_error, 2))),
           rapm_blended_overall_error = paste0("± ", abs(round(rapm_blended_overall_error, 2))),
           rapm_blended_driver_error_2 = paste0("± ", abs(round(rapm_blended_driver_error_2, 2))),
           rapm_blended_constructor_error_2 = paste0("± ", abs(round(rapm_blended_constructor_error_2, 2))),
           rapm_blended_overall_error_2 = paste0("± ", abs(round(rapm_blended_overall_error_2, 2)))) %>%
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
      rapm_blended_driver_error = "",
      rapm_blended_constructor = "Constructor Rating",
      rapm_blended_constructor_error = "",
      rapm_blended_overall = "Overall Rating",
      rapm_blended_overall_error = "",
      #Second Column
      driver_headshot_url_2  = "Driver",
      driver_name_2 = "",
      constructor_img_url_2 = "Constructor",
      parent_constructor_name_2 = "",
      rapm_blended_driver_2 = "Driver Rating",
      rapm_blended_driver_error_2 = "",
      rapm_blended_constructor_2 = "Constructor Rating",
      rapm_blended_constructor_error_2 = "",
      rapm_blended_overall_2 = "Overall Rating",
      rapm_blended_overall_error_2 = ""
    )  %>%
    #Cols Row 1
    gt_img_rows(columns = driver_headshot_url, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    #Cols Row 2
    gt_img_rows(columns = driver_headshot_url_2, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url_2, height = 25, img_source = 'local') %>%
    #Merge Stacks Col 1
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended_driver, col2 = rapm_blended_driver_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    gt_merge_stack(col1 = rapm_blended_constructor, col2 = rapm_blended_constructor_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    #Merge Stacks Col 2
    gt_merge_stack(col1 = constructor_img_url_2, col2 = parent_constructor_name_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended_driver_2, col2 = rapm_blended_driver_error_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    gt_merge_stack(col1 = rapm_blended_constructor_2, col2 = rapm_blended_constructor_error_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    fmt_number(
      columns = c(rapm_blended_driver, rapm_blended_constructor, rapm_blended_overall,
                  rapm_blended_driver_2, rapm_blended_constructor_2, rapm_blended_overall_2),
      decimals = 2,
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
      locations = cells_body(columns = vars(rapm_blended_driver, rapm_blended_constructor, rapm_blended_overall,
                                            rapm_blended_driver_2, rapm_blended_constructor_2, rapm_blended_overall_2))
    ) %>%
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
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono"))
        )
      ),
      locations = cells_body(columns = vars(rapm_blended_overall_error,
                                            rapm_blended_overall_error_2))
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) %>%
    gt_add_divider(columns = c(constructor_img_url, rapm_blended_constructor,
                               constructor_img_url_2, rapm_blended_constructor_2), 
                   color = '#808080', weight = 0.75) %>%
    gt_add_divider(columns = c(rapm_blended_overall_error), 
                   color = '#FFFFFF', weight = px(30)) %>%
    cols_align(
      align = "center",
      columns = c(constructor_img_url, rapm_blended_driver, rapm_blended_constructor, rapm_blended_overall,
                  constructor_img_url_2, rapm_blended_driver_2, rapm_blended_constructor_2, rapm_blended_overall_2)
    ) %>%
    data_color(
      columns = vars(c(rapm_blended_driver, rapm_blended_constructor,
                       rapm_blended_driver_2, rapm_blended_constructor_2)),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-4.5, 4.5)
      ),
      apply_to = "text"
    ) %>%
    data_color(
      columns = vars(rapm_blended_overall,
                     rapm_blended_overall_2),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-8, 8)
      )
    ) %>%
    tab_options(
      table.width = px(540)  # 7.5 inches * 72 pixels per inch
    )
  
  return(overall_rankings_tbl_split)
  
  
}



overall_rankings_tbl_split <- get_overall_rankings_split_tbl(current_overall_rankings)

gtsave(data = overall_rankings_tbl_split, 
             filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/overall_rankings_split - ", latest_date, ".html")) 


gtsave_extra(data = overall_rankings_tbl_split, 
       filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/overall_rankings_split - ", latest_date, ".png"), 
       vwidth = 1350,
       zoom = 10) 





#Driver Rankings

get_driver_rankings_split_tbl <- function(current_driver_rankings){
  
  df1 <- current_driver_rankings[1:10, ]
  df2 <- current_driver_rankings[11:20, ]
  
  # Reset the index for both dataframes
  df1 <- df1 %>% mutate(index = row_number())
  df2 <- df2 %>% mutate(index = row_number())
  
  # Rename columns in df2 with _2 suffix
  colnames(df2) <- paste0(colnames(df2), "_2")
  
  # Join the two dataframes on the new index column
  current_driver_rankings_split <- df1 %>% 
    inner_join(df2, by = c("index" = "index_2")) %>%
    dplyr::select(-index)  # Remove the index column if not needed
  
  
  
  driver_rankings_split_tbl <- current_driver_rankings_split %>%
    mutate(rapm_error = paste0("± ", abs(round(rapm_error, 2))),
           rapm_error_2 = paste0("± ", abs(round(rapm_error_2, 2)))) %>%
    gt() %>%
    tab_header(
      title = md("**Driver Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      #Col 1
      driver_headshot_url  = "Driver",
      driver_name = "",
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended = "Driver Rating",
      rapm_error = "",
      #Col 2
      driver_headshot_url_2  = "Driver",
      driver_name_2 = "",
      constructor_img_url_2 = "Constructor",
      parent_constructor_name_2 = "",
      rapm_blended_2 = "Driver Rating",
      rapm_error_2 = ""
    )  %>%
    #Col Split 1
    gt_img_rows(columns = driver_headshot_url, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    #Col Split 2
    gt_img_rows(columns = driver_headshot_url_2, height = 30, img_source = 'local') %>%
    gt_img_rows(columns = constructor_img_url_2, height = 25, img_source = 'local') %>%
    #Col Split 1
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended, col2 = rapm_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    #Col Split 2
    gt_merge_stack(col1 = constructor_img_url_2, col2 = parent_constructor_name_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended_2, col2 = rapm_error_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    fmt_number(
      columns = c(rapm_blended,
                  rapm_blended_2),
      decimals = 2,
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
      locations = cells_body(columns = vars(rapm_blended,
                                            rapm_blended_2))
    ) %>%
    tab_style(
      style = list(
        cell_text(
          font = c(google_font(name = "Roboto Mono")),
        )
      ),
      locations = cells_body(columns = vars(rapm_error,
                                            rapm_error_2))
    ) %>%
    tab_style(
      style = list(
        cell_text(font = c(
          google_font(name = "Roboto")))),
      locations = cells_body(columns = c(parent_constructor_name, driver_name,
                                         parent_constructor_name_2, driver_name_2))) %>%
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
    gt_add_divider(columns = c(rapm_blended), 
                   color = '#FFFFFF', weight = px(30)) %>%
    data_color(
      columns = vars(rapm_blended),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-3.5, 3.5)
      ),
      apply_to = "text"
    ) 

  return(driver_rankings_split_tbl)
  
}


driver_rankings_tbl_split <- get_driver_rankings_split_tbl(current_driver_rankings)


gtsave(data = driver_rankings_tbl_split, 
             filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/driver_rankings_split - ", latest_date, ".html")) 




gtsave_extra(data = driver_rankings_tbl_split, 
             filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/driver_rankings_split - ", latest_date, ".png"), 
             zoom = 10) 






#Constructor Rankings


get_constructor_rankings_split_tbl <- function(current_constructor_rankings){
  
  
  df1 <- current_constructor_rankings[1:5, ]
  df2 <- current_constructor_rankings[6:10, ]
  
  # Reset the index for both dataframes
  df1 <- df1 %>% mutate(index = row_number())
  df2 <- df2 %>% mutate(index = row_number())
  
  # Rename columns in df2 with _2 suffix
  colnames(df2) <- paste0(colnames(df2), "_2")
  
  # Join the two dataframes on the new index column
  current_constructor_rankings_split <- df1 %>% 
    inner_join(df2, by = c("index" = "index_2")) %>%
    dplyr::select(-index)  # Remove the index column if not needed
  
  
  
  tbl_constructor_split <- current_constructor_rankings_split %>%
    mutate(rapm_error = paste0("± ", abs(round(rapm_error, 2))),
           rapm_error_2 = paste0("± ", abs(round(rapm_error_2, 2)))) %>%
    gt() %>%
    tab_header(
      title = md("**Constructor Ratings**"),
      subtitle = paste0("As of the ", latest_race)) %>% 
    cols_label(
      #Col Split 1
      constructor_img_url = "Constructor",
      parent_constructor_name = "",
      rapm_blended = "Constructor Rating",
      rapm_error = "",
      #Col Split 2
      constructor_img_url_2 = "Constructor",
      parent_constructor_name_2 = "",
      rapm_blended_2 = "Constructor Rating",
      rapm_error_2 = ""
    )  %>%
    #Col Split 1
    gt_img_rows(columns = constructor_img_url, height = 25, img_source = 'local') %>%
    #Col Split 2
    gt_img_rows(columns = constructor_img_url_2, height = 25, img_source = 'local') %>%
    #Col Split 1
    gt_merge_stack(col1 = constructor_img_url, col2 = parent_constructor_name, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended, col2 = rapm_error, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    #Col Split 2
    gt_merge_stack(col1 = constructor_img_url_2, col2 = parent_constructor_name_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'), palette = c('black', '#404040')) %>%
    gt_merge_stack(col1 = rapm_blended_2, col2 = rapm_error_2, small_cap=TRUE, 
                   font_weight = c('normal','bold'),) %>%
    fmt_number(
      columns = c(rapm_blended, rapm_blended_2),
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
      locations = cells_body(columns = vars(rapm_blended, rapm_blended_2))
    ) %>%
    tab_style(
      style = list(
        cell_text(font = c(
          google_font(name = "Roboto")))),
      locations = cells_body(columns = c(parent_constructor_name,
                                         parent_constructor_name_2))) %>%
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
      columns = c(constructor_img_url,rapm_blended,
                  constructor_img_url_2,rapm_blended_2)
    ) %>%
    gt_add_divider(columns = c(rapm_blended), 
                   color = '#FFFFFF', weight = px(30)) %>%
    data_color(
      columns = vars(rapm_blended, rapm_blended_2),
      colors = scales::col_numeric(
        palette = c("#de425b", "#eb7a52", "#f8b267", "#c6c96a", "#8aac49", "#488f31"),
        domain = c(-4, 4)
      ),
      apply_to = "text"
    ) 
  
  return(tbl_constructor_split)

}



constructor_rankings_tbl_split <- get_constructor_rankings_split_tbl(current_constructor_rankings)

gtsave(data = constructor_rankings_tbl_split, 
             filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/constructor_rankings_split - ", latest_date, ".html")) 

gtsave_extra(data = constructor_rankings_tbl_split, 
             filename = paste0("f1dataR - Exports/Visuals/Ratings/Tables/constructor_rankings_split - ", latest_date, ".png"), 
             zoom = 10) 





