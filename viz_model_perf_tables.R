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




model_metrics <- readRDS("f1dataR - Exports/Data/model_metrics_core.rds") %>%
  dplyr::select(dnf_model, mae_unweighted, kt_overall, kt_overall_blended,
         kendall_constructor_partial, kendall_driver_partial, kendall_const_driver_ratio)  %>%
  mutate(dnf_model = factor(dnf_model, levels = c("all_dnf", "partial_credit_dnf", "no_dnf", "quali"))) %>%
  dplyr::arrange(dnf_model)  %>%
  mutate(dnf_model = recode(dnf_model,
                            "all_dnf" = "All DNFs Included",
                            "partial_credit_dnf" = "DNF Assigned to Constructor/Driver",
                            "no_dnf" = "No DNFs Included",
                            "quali" = "Qualification")) %>%
  mutate_at(vars(-dnf_model), as.numeric) %>%
  mutate(implied_constructor_influence = kendall_const_driver_ratio/(kendall_const_driver_ratio+1))





tbl_model_core <- model_metrics %>%
  gt() %>%
  tab_header(
    title = md("**Time Weighted Linear Regression Model Performance**"),
    subtitle = paste0("Tested on 2014 - 2024 Races")) %>% 
  cols_label(
    dnf_model  = "Model Data",
    mae_unweighted = "MAE",
    kt_overall = "Kendall Tau (𝜏)",
    kt_overall_blended = "𝜏-LOESS Blended",
    kendall_constructor_partial = html("Partial 𝜏<sub>Constructor</sub>"),
    kendall_driver_partial = html("Partial 𝜏<sub>Driver</sub>"),
    kendall_const_driver_ratio = html("Partial Constructor/Driver 𝜏<sup>2</sup> Ratio"),
    implied_constructor_influence = "Implied Constructor Influence"
  )  %>%
  fmt_number(
    columns = c(mae_unweighted),
    decimals = 1
  )  %>%
  fmt_number(
    columns = c(kt_overall, kt_overall_blended, kendall_constructor_partial, 
                kendall_driver_partial, kendall_const_driver_ratio),
    decimals = 3
  ) %>%
  fmt_percent(
    columns = implied_constructor_influence,
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_text(
        font = c(google_font(name = "Roboto Mono")),      )
    ),
    locations = cells_body(columns = c(mae_unweighted, kt_overall, kt_overall_blended, kendall_constructor_partial, 
                                       kendall_driver_partial, kendall_const_driver_ratio, 
                                       implied_constructor_influence))
  ) %>%
  tab_spanner(
    label = "Model Metrics",
    columns = c(mae_unweighted, kt_overall, kt_overall_blended)
  ) %>%
  tab_spanner(
    label = "Variance Explained",
    columns = c(kendall_constructor_partial, kendall_driver_partial, kendall_const_driver_ratio, 
                implied_constructor_influence)
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




gtsave(data = tbl_model_core, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/model_metrics.tex"))

gtsave(data = tbl_model_core, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/model_metrics.html"))


gtsave(data = tbl_model_core, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/model_metrics.png"),
       zoom = 4)




webshot2::webshot(paste0("f1dataR - Exports/Visuals/Model Metrics/model_metrics.png"), 
                  paste0("f1dataR - Exports/Visuals/Model Metrics/model_metrics_test.jpg"),
                  zoom = 4
)



#### Log Model Output

log_model_metrics <- readRDS('f1dataR - Exports/Data/log_model_t20_all_dnf_performance.rds') %>%
  #filter(dnf_filter == 'no_dnf') %>%
  mutate(model_type = case_when(
    top_n_boolean == 4 ~ "Podium (T3)",
    top_n_boolean == 7 ~ "Top 6",
    top_n_boolean == 11 ~ "Points (T10)"
  )) %>%
  dplyr::select(model_type, pseudo_r2_full, partial_pseudo_r2_constructor, partial_pseudo_r2_driver) %>%
  mutate(partial_psuedo_const_driver_ratio = partial_pseudo_r2_constructor/partial_pseudo_r2_driver,
         implied_constructor_influence = partial_psuedo_const_driver_ratio/(partial_psuedo_const_driver_ratio+1)) %>%
  dplyr::select(-partial_psuedo_const_driver_ratio)




tbl_log_metrics <- log_model_metrics %>%
  filter(!is.na(model_type)) %>%
  gt() %>%
  tab_header(
    title = md("**Time Weighted Logistic Regression Model Performance (DNF-Inclusive)**"),
    subtitle = paste0("Against Top N Cut Offs | Tested on 2014 - 2024 Races")) %>% 
  cols_label(
    model_type  = "Top N",
    pseudo_r2_full = html("Psuedo R<sup>2</sup>"),
    partial_pseudo_r2_constructor =  html("Partial Psuedo R<sup>2</sup><sub>Constructor</sub>"),
    partial_pseudo_r2_driver = html("Partial Psuedo R<sup>2</sup><sub>Driver</sub>"),
    implied_constructor_influence = "Implied Constructor Influence"
  )  %>%
  fmt_number(
    columns = c(pseudo_r2_full, partial_pseudo_r2_constructor, partial_pseudo_r2_driver),
    decimals = 3
  ) %>%
  fmt_percent(
    columns = implied_constructor_influence,
    decimals = 1
  ) %>%
  tab_style(
    style = list(
      cell_text(
        font = c(google_font(name = "Roboto Mono")),      )
    ),
    locations = cells_body(columns = c(pseudo_r2_full, partial_pseudo_r2_constructor, partial_pseudo_r2_driver, 
                                       implied_constructor_influence))
  ) %>%
  tab_footnote(
    footnote =  html("McFaddens Psuedo R<sup>2</sup>"),
    locations = cells_column_labels(columns = pseudo_r2_full)
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


gtsave(data = tbl_log_metrics, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/log_model_metrics.tex"))


gtsave(data = tbl_log_metrics, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/log_model_metrics.html"))




webshot2::webshot(paste0("f1dataR - Exports/Visuals/Model Metrics/log_model_metrics.html"), 
                  paste0("f1dataR - Exports/Visuals/Model Metrics/log_model_metrics.jpg"),
                  zoom = 4
)




####Log Model Vizzes



log_model_performance <- readRDS('f1dataR - Exports/Data/log_model_t20_all_dnf_performance.rds')  %>%
  mutate(const_driver_ratio = partial_pseudo_r2_constructor/partial_pseudo_r2_driver,
         implied_constructor_influence = const_driver_ratio/(const_driver_ratio+1))


text_annotations <- data.frame(
  x = c(3, 6),
  label = c('Podium (T3)', 'Top 6')
)



g <- log_model_performance %>%
  filter(top_n_boolean < 11) %>%
  ggplot(aes(x = top_n_boolean), linewidth = 1.5) +
  geom_vline(data = text_annotations, aes(xintercept = x), 
             color = '#404040', linetype = 'dashed') +
  geom_point(aes(y = pseudo_r2_full), color = '#E10600') +
  geom_path(aes(y = pseudo_r2_full), color = '#E10600') + 
  geom_point(aes(y = partial_pseudo_r2_driver), color = '#61E294') +
  geom_path(aes(y = partial_pseudo_r2_driver), color = '#61E294') + 
  geom_point(aes(y = partial_pseudo_r2_constructor), color = '#087E8B') +
  geom_path(aes(y = partial_pseudo_r2_constructor), color = '#087E8B') + 
  geom_hline(aes(yintercept = 0), 
             color = '#404040') +
  geom_text(data = text_annotations, aes(x = x, label = label), 
            y= Inf,  color = '#404040', 
            inherit.aes = FALSE, vjust = 1.5, hjust = -0.1, size = 3,fontface = "bold") +
  theme_saurabh() +
  labs(title = "McFadden R2 for Predicting Top N Placement | 2014 - 2024",
       subtitle = "Logistic Ridge Regression, DNF-Inclusive - 
       <b><span style='color:#E10600;'> Overall</span></b> | 
       <b><span style='color:#087E8B;'> Constructors</span></b> | 
       <b><span style='color:#61E294;'>Drivers</span></b>",
       x = "N",
       y = "")  +
  theme(
    plot.subtitle = element_markdown() # Enable markdown for title
  ) 

chart_printer(
  g = g, 
  chart_filepath = paste0("f1dataR - Exports/Visuals/Model Metrics/log_mcfaddens_trend_t20.jpg"),
  img_width = 8, img_height = 2
)









g <- log_model_performance %>%
  filter(top_n_boolean < 11) %>%
  ggplot(aes(x = top_n_boolean)) +
  geom_vline(data = text_annotations, aes(xintercept = x), 
             color = '#404040', linetype = 'dashed') +
  geom_point(aes(y = implied_constructor_influence), color = '#E10600') +
  geom_path(aes(y = implied_constructor_influence), color = '#E10600') + 
  geom_vline(data = text_annotations, aes(xintercept = x), 
             color = '#404040', linetype = 'dashed') +
  geom_text(data = text_annotations, aes(x = x, label = label), 
            y= Inf,  color = '#404040', 
            inherit.aes = FALSE, vjust = 1.5, hjust = -0.1, size = 3,fontface = "bold") +
  theme_saurabh() +
  labs(title = "Percent Variance Explained by Constructors",
       subtitle = "Logistic Regression Predicting Top N Placement | 2014 - 2024",
       x = "N",
       y = "") + 
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1))


chart_printer(
  g = g, 
  chart_filepath = paste0("f1dataR - Exports/Visuals/Model Metrics/log_const_imp_trend_t20.jpg"),
  img_width = 8, img_height = 2
)







### Decay Perf



time_decay_parameter_search <- readRDS('f1dataR - Exports/Data/time_decay_grid_search.rds')  %>%
  select(season_decay, round_decay, mae_unweighted, kt_overall, 
         nDCG_top3, nDCG_top6, nDCG_top10)



tbl_time_decay_serach <- time_decay_parameter_search %>%
  gt() %>%
  tab_header(
    title = md("**Time Weighted Linear Regression Model Performance**"),
    subtitle = paste0("On 2014 - 2024 Races")) %>% 
  cols_label(
    season_decay  = "Season",
    round_decay = "Round",
    mae_unweighted = "Mean Absolute Error",
    kt_overall = "Kendall Tau (𝜏)",
    nDCG_top3 = "3",
    nDCG_top6 = "6",
    nDCG_top10 = "10"
  )  %>%
  fmt_number(
    columns = c(season_decay, round_decay),
    decimals = 4
  )  %>%
  fmt_number(
    columns = c(mae_unweighted, kt_overall, nDCG_top3, nDCG_top6, nDCG_top10),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_text(
        font = c(google_font(name = "Roboto Mono")),      )
    ),
    locations = cells_body()
  ) %>%
  tab_spanner(
    label = "Exponential Decay Factor",
    columns = c(season_decay, round_decay)
  ) %>%
  tab_spanner(
    label = "Normalized Discounted Cumulative Gain",
    columns = c(nDCG_top3, nDCG_top6, nDCG_top10)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFD9D9") # Highlight second row in light yellow
    ),
    locations = cells_body(
      rows = season_decay == 0.75 & round_decay == 0.075
    )
  )  %>%
  tab_footnote(
    footnote = html("Overall decay weighting is:<br>𝑒<sup>(Season Decay)*(Current Season - Race Season)</sup> 
                    * 𝑒<sup>(Round Decay)*(Current Round - Race Round)</sup>"),
    locations = cells_column_spanners("Exponential Decay Factor")
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





gtsave(data = tbl_time_decay_serach, 
       filename = paste0("f1dataR - Exports/Visuals/Model Metrics/time_decay_search_metrics.png"),
       zoom = 4)


