
library(gt)
library(gtExtras)
library(scales)
library(extrafont)
library(dplyr)


# Function: theme_saurabh
# Purpose: Personal, minimalist theme
theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=16, family = 'Roboto Black', color = '#101010'),
    plot.subtitle=element_text(size=11, family = 'Roboto Slab'),
    plot.caption=element_text(size=7, family = 'Roboto Mono'),
    panel.background = element_blank(),
    
  )
}


###Reliability Plot

text_annotations <- data.frame(
  x = c(1986, 1997.5, 2010, 2019),
  label = c('1.5L Turbo', 'V10 Era', 'V8 Era', 'Hybrid Era'),
  seasons = c(1983, 1989, 2006, 2014)
)


reliabiltiy_const_df <- results %>%
  group_by(season) %>%
  summarise(races = n(),
            finishes = sum(finished),
            classification_pct =  finishes/races)


reliabiltiy_const_df %>%
  mutate(season = as.integer(season)) %>%
  filter(season > 1982) %>%
  ungroup() %>%
  ggplot(aes(y = classification_pct, x = season, group = 1)) +
  geom_point(color = '#E10600') +
  geom_path(color = '#E10600') + 
  geom_vline(data = text_annotations, aes(xintercept = seasons), 
             color = '#404040', linetype = 'dashed') +
  geom_text(data = text_annotations, aes(x = x, label = label), 
            y= Inf,  color = '#404040', 
            inherit.aes = FALSE, vjust = 1.5, size = 3,fontface = "bold") +
  theme_saurabh() +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Race Classifcation %",
       subtitle = "1983 - Present",
       x = "Season",
       y = "Classifcation %")



ggsave(paste0("f1dataR - Exports/Visuals/Misc/classifiction_pct.jpg"), width = 8, height = 4, dpi = 600)




















### Parent/Child Relationships

constructor_mapping_tbl <- constructor_mapping %>%
  mutate(constructor_img_url = paste0('supplementary_data/Images/Constructor/png/', parent_constructor_id, '.png'))




const_images <- constructor_mapping_tbl %>% 
  dplyr::select(parent_constructor_name, constructor_img_url) %>% 
  filter(!is.na(parent_constructor_name)) %>%
  tibble::deframe()


constructor_mapping_tbl <- constructor_mapping_tbl %>%
  dplyr::select(parent_constructor_name, 
                constructor_name) 


df1 <- constructor_mapping_tbl[1:13, ]
df2 <- constructor_mapping_tbl[14:25, ]

# Reset the index for both dataframes
df1 <- df1 %>% mutate(index = row_number())
df2 <- df2 %>% mutate(index = row_number())

# Rename columns in df2 with _2 suffix
colnames(df2) <- paste0(colnames(df2), "_2")

# Join the two dataframes on the new index column
constructor_mapping_tbl_split <- df1 %>% 
  inner_join(df2, by = c("index" = "index_2")) %>%
  dplyr::select(-index)  # Remove the index column if not needed




output_constructor_mapping_tbl <- constructor_mapping_tbl %>%
  dplyr::select(parent_constructor_name, 
                constructor_name) %>%
  mutate(constructor_name = paste0(constructor_name, " "))  %>%
  slice(13:25) %>%
  filter(!is.na(parent_constructor_name)) %>%
  gt(
    groupname_col  = "parent_constructor_name",
    rowname_col   = "constructor_name",
    row_group_as_column = TRUE
  ) %>% 
  cols_label(
    parent_constructor_name = "Parent Constructor",
    constructor_name = "Constructor"
  ) %>%
  gt::text_transform(
    locations = cells_row_groups(),
    fn = function(x) {
      lapply(x, function(x) {
        gt::html(paste(
          local_image(
            filename = const_images[[x]],
            height = 25
          ),
          "<span>", x, "</span>"
        ))
      })
    }
  )  %>%
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
  )



gtsave_extra(data = output_constructor_mapping_tbl, 
       filename = paste0("f1dataR - Exports/Visuals/Misc/constructor_mapping_tbl_split2.png"),
       zoom = 10)

gtsave(data = output_constructor_mapping_tbl, 
       filename = paste0("f1dataR - Exports/Visuals/Misc/constructor_mapping_tbl.tex"))

gtsave(data = output_constructor_mapping_tbl, 
       filename = paste0("f1dataR - Exports/Visuals/Misc/constructor_mapping_tbl.html"))

webshot2::webshot(paste0("f1dataR - Exports/Visuals/Misc/constructor_mapping_tbl.html"), 
                  paste0("f1dataR - Exports/Visuals/Misc/constructor_mapping_tbl.jpg"),
                  zoom = 4
)




### Performance for new/same/change drivers


prev_team_mapping <- results_pred %>%
  arrange(driver_id, season) %>%  # Ensure data is sorted by driver and season
  group_by(driver_id, season) %>%
  summarize(
    parent_constructor_id = first(parent_constructor_id)) %>%
  mutate(prev_team = lag(parent_constructor_id),
         category = case_when(
           is.na(prev_team) ~ "New Driver",  # If no previous team, the driver is new
           parent_constructor_id == prev_team ~ "Same Team",  # If same as last season, same team
           parent_constructor_id != prev_team ~ "New Team"  # If different from last season, new team
         ))

model_perf_driver_change <- results_pred %>%
  filter(season > 2013, season < 2024) %>%
  left_join(prev_team_mapping %>% dplyr::select(driver_id, season, category), by = c("driver_id", "season")) %>%
  group_by(category) %>%
  summarise(driver_season = n(),
            mae = mean(position_pred_diff),
            median_abs_error = median(position_pred_diff),
            avg_position = mean(position),
            avg_position_sd = sd(position)
  )



tbl_model_perf_driver_change <- model_perf_driver_change %>% 
  dplyr::select(category, driver_season, avg_position, mae) %>%
  arrange(avg_position) %>%
  gt() %>%
  tab_header(
    title = md("**Mean Absolute Error by Driver-Season**"),
    subtitle = paste0("Tested on 2014 - 2023 Races")) %>% 
  cols_label(
    category  = "Driver",
    driver_season = "Driver-Seasons",
    avg_position = "Avg Finishing Position",
    mae = "MAE"
  )  %>%
  fmt_number(
    columns = c(driver_season),
    decimals = 0
  )  %>%
  fmt_number(
    columns = c(avg_position),
    decimals = 1
  )  %>%
  fmt_number(
    columns = c(mae),
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c(driver_season, avg_position, mae)
  ) %>%
  tab_style(
    style = list(
      cell_text(
        font = c(google_font(name = "Roboto Mono")),      )
    ),
    locations = cells_body(columns = c(driver_season, avg_position, mae))
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
  ) 


gtsave_extra(data = tbl_model_perf_driver_change, 
       filename = paste0("f1dataR - Exports/Visuals/Misc/model_perf_driver_change.png"),
       zoom = 10)


gtsave(data = tbl_model_perf_driver_change, 
       filename = paste0("f1dataR - Exports/Visuals/Misc/model_perf_driver_change.tex"))


gtsave(data = tbl_model_perf_driver_change, 
       filename = paste0("f1dataR - Exports/Visuals/Misc/model_perf_driver_change.html"))

webshot2::webshot(paste0("f1dataR - Exports/Visuals/Misc/model_perf_driver_change.html"), 
                  paste0("f1dataR - Exports/Visuals/Misc/model_perf_driver_change.jpg"),
                  zoom = 4
)
