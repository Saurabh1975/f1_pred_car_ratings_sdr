
library(gt)
library(gtExtras)
library(ggplot2)
library(ggtext)
library(scales)
library(extrafont)
library(dplyr)
library(glue)
library(stringr)

chart_printer <- function(g, chart_filepath, img_width = 8, img_height = 4) {
  
  # Manually open a Cairo-based PNG graphics device
  png(
    filename = chart_filepath,
    width = img_width,
    height = img_height,
    units = "in",
    res = 1200,
    type = "cairo"  # <--- Forces use of Cairo on macOS
  )
  
  # Print the plot to the device
  print(g)
  
  # Close the device
  dev.off()
  
  
}


# Function: theme_saurabh
# Purpose: Personal, minimalist theme
theme_saurabh <- function () { 
  theme(
    text=element_text(family='Roboto Mono'),
    plot.title=element_text(size=18, family = 'Roboto Black', color = '#101010'),
    plot.subtitle=element_text(size=11, family = 'Roboto Slab'),
    plot.caption=element_text(size=7, family = 'Roboto Mono'),
    panel.background = element_blank()
    
  )
}


###Reliability Plot

text_annotations <- data.frame(
  x = c(1986, 1997.5, 2010, 2019),
  label = c('1.5L Turbo', 'V10 Era', 'V8 Era', 'Hybrid Era'),
  seasons = c(1983, 1989, 2006, 2014)
)




reliabiltiy_const_df <- results %>%
  filter(!(status %in% c('Did not qualify', 'Did not prequalify'))) %>%
  group_by(season) %>%
  summarise(races = n(),
            finishes = sum(finished),
            classification_pct =  finishes/races)


last_point <- reliabiltiy_const_df %>%
  mutate(season = as.integer(season)) %>%
  filter(season > 1982) %>%
  arrange(season) %>%
  slice_tail(n = 1)


g <- reliabiltiy_const_df %>%
  mutate(season = as.integer(season)) %>%
  filter(season > 1982) %>%
  ungroup() %>%
  ggplot(aes(y = classification_pct, x = season, group = 1)) +
  geom_vline(data = text_annotations, aes(xintercept = seasons), 
             color = '#404040', linetype = 'dashed') +
  geom_point(color = '#E10600') +
  geom_path(color = '#E10600') +
  
  # annotation labels
  geom_text(
    data = text_annotations,
    aes(x = x, label = label),
    y = Inf,
    color = '#404040',
    inherit.aes = FALSE,
    vjust = 1.5,
    size = 3,
    fontface = "bold"
  ) +
  
  geom_label(
    data = last_point,
    aes(
      x = season,
      y = classification_pct,
      label = scales::percent(classification_pct, accuracy = 1)
    ),
    fill = "#E10600",
    color = "white",
    fontface = "bold",
    size = 3,
    vjust = 1.25,                 # move above the point
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.25, "lines") # rounded corners
  ) +
  
  theme_saurabh() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Race Classifcation %",
    subtitle = "1983 - 2025 | Qualified Drivers Only",
    x = "Season",
    y = "Classifcation %"
  )

g

chart_printer(
  g = g, 
  chart_filepath = paste0("f1dataR - Exports/Visuals/Misc/classifiction_pct.png")
)



###Reliability Reasons

car_failure_reason <- c("ERS", "Oil pressure", "Engine", "Technical", "Gearbox",
                        "Electrical", "Power Unit",  "Brakes", "Clutch", 
                        "Mechanical", "Turbo", "Rear wing", "Drivetrain", 
                        "Suspension", "Oil leak",  "Water leak", "Wheel",
                        "Power loss",  "Spun off", "Fuel system", "Transmission",
                        "Water pressure", "Electronics", "Wheel", "Power loss",
                        "Fuel system", "Transmission", "Front wing", "Tyre",
                        "Throttle", "Brake duct", "Hydraulics", "Battery", 
                        "Overheating", "Wheel nut", "Vibrations",   "Driveshaft",
                        "Fuel pressure", "Seat", "Spark plugs", "Steering", 
                        "Radiator",  "Cooling system", "Water pump" , 
                        "Fuel leak", "Fuel pump", "Undertray","Differential", "Exhaust")

driver_failure_reason <- c("Collision", "Accident",  "Collision damage", 
                           "Spun off", "Illness")


unknown_failure_reason <- c( "Disqualified", "Withdrew", "Retired", "Excluded",
                             "Puncture", "Damage",  "Out of fuel", "Debris" )


dnf_detail <- results %>%
  filter(!finished, season > 2013) %>%
  mutate(status_consolidated = case_when(
    status %in% car_failure_reason ~ "Car Related",
    status %in% driver_failure_reason ~ "Driver Related",
    status %in% unknown_failure_reason ~ "Not Attributable",
  ))


#Examples of weirdly coded accidents
##Albon/Sainz 2024 Canada Grand Prix
##ALbon/Tsunoda CDMX
#Situations that are unattributable
##e.g. Russell

# Prepare data
dnf_detail_status_count <- dnf_detail %>%
  group_by(status_consolidated, status) %>%
  summarise(status_count = n()) %>%
  ungroup() %>%
  arrange(desc(status_count)) %>%
  head(15) %>%
  mutate(
    status = forcats::fct_reorder(status, status_count),  # Reorder factor
    alpha_value = scales::rescale(status_count, to = c(0.5, 1))  # Scale alpha between 0.25 and 1
  )


custom_fill_colors <- c(
  "Driver Related" = "#FF1E00", 
  "Car Related" = "#39AA72", 
  "Not Attributable" = "#404040"
)

subtitle_text <- paste(
  glue("<span style='color:{custom_fill_colors}; white-space:nowrap'>{names(custom_fill_colors)}</span>"),
  collapse = " | "
)

# Plot
g <- dnf_detail_status_count %>%
  ggplot(aes(x = status_count, y = status)) +
  geom_bar(aes(alpha = alpha_value, fill = status_consolidated),  # Vary alpha based on scaled values
           width = 0.75, stat = 'identity') +
  geom_text(aes(label = status, color =status_consolidated ), hjust = -0.1, vjust = 0.5,  
            family = "Roboto Mono", size = 4) +  
  geom_text(aes(label = ifelse(status_count > 40, status_count, "")), hjust = 1.1, vjust = 0.5,  
            family = "Roboto Mono", size = 4, color = 'white') + 
  theme_saurabh() +
  theme(
    axis.title.y = element_blank(),  
    plot.subtitle = element_markdown(),  # Enable markdown rendering
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),
    legend.position = "none"  # Hide the legend

  ) +
  xlim(0, max(dnf_detail_status_count$status_count) * 1.1) +  # Extend x-axis for text placement
  scale_alpha_identity()   + 
  scale_fill_manual(values = custom_fill_colors) +  # Custom fill colors
  scale_color_manual(values = custom_fill_colors) +  # Custom fill colors
  labs(title = "Top DNF Reasons - 2014 to 2024",
       subtitle = subtitle_text,
       x = "Count of DNFs",
       y = "")


chart_printer(
  g = g, 
  chart_filepath = paste0("f1dataR - Exports/Visuals/Misc/dnf_reason_count.png")
)



dnf_detail_cons_count <- dnf_detail %>%
  group_by(status_consolidated) %>%
  summarise(status_count = n())  %>%
  arrange(desc(status_count)) %>%
  mutate(
    status_consolidated = forcats::fct_reorder(status_consolidated, status_count),  # Reorder factor
    alpha_value = scales::rescale(status_count, to = c(0.5, 1))  # Scale alpha between 0.25 and 1
  )



# Plot
dnf_detail_cons_count %>%
  ggplot(aes(x = status_count, y = status_consolidated)) +
  geom_bar(aes(alpha = alpha_value),  # Vary alpha based on scaled values
           width = 0.75, stat = 'identity', fill = '#E10600') +
  geom_text(aes(label = status_consolidated), hjust = -0.1, vjust = 0.5,  
            family = "Roboto Mono", size = 4, color = '#404040') +  
  geom_text(aes(label = ifelse(status_count > 40, status_count, "")), hjust = 1.1, vjust = 0.5,  
            family = "Roboto Mono", size = 4, color = 'white') + 
  theme_saurabh() +
  theme(
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank() 
  ) +
  xlim(0, max(dnf_detail_cons_count$status_count) * 1.125) +  # Extend x-axis for text placement
  scale_alpha_identity()   + 
  labs(title = "Top DNF Reasons Summarized",
       subtitle = "2014 - 2024",
       x = "Count of DNFs",
       y = "")





# Find consecutive DNFs
consecutive_dnf <- dnf_detail %>%
  arrange(driver_id, constructor_id, season, round) %>%  # Ensure proper ordering
  group_by(driver_id, constructor_id) %>%
  mutate(consecutive = round - lag(round) == 1, 
         consecutive = round - lag(round, n = 2) == 2) %>%  # Check if rounds are consecutive
  filter(consecutive) %>%  # Keep only consecutive instances
  ungroup()

# Find consecutive DNFs
consecutive_dnf <- dnf_detail %>%
  group_by(constructor_id, season, round) %>%
  ungroup() %>%
  arrange(constructor_id, season, round) %>%  # Ensure proper ordering
  group_by(constructor_id) %>%
  mutate(consecutive = (round - lag(round) == 1) & round - lag(round, n = 2) == 2) %>%  # Check if rounds are consecutive
  filter(consecutive) %>%  # Keep only consecutive instances
  ungroup()



file_path = "f1dataR - Exports/Data/rapm_history_posWeighted_DNF_bootstrapped.rds"


#Read in latest RAPM History
rapm_history_dnf_am <- readRDS(file_path) %>% 
  filter(entity_id %in% c('alonso-d', 'stroll-d', 'aston_martin-c')
         , season == 2023) 
  



#Get No DNF Data

file_path = "f1dataR - Exports/Data/rapm_history_posWeighted_noDNF_bootstrapped.rds"

rapm_history_no_dnf_am <- read.csv(file_path)  %>% 
  filter(entity_id %in% c('alonso-d', 'stroll-d', 'aston_martin-c')
         , season == 2023) 


#alonso-d
#stroll-d
#aston_martin-c

selected_entity = 'aston_martin-c'


chart_2023_races <- schedule %>%
  filter(season == 2023) %>%
  mutate(race_name_abv = str_remove(race_name, " Grand Prix$")) %>%
  pull(race_name_abv)

g <- ggplot(rapm_history_dnf_am %>% filter(entity_id == selected_entity), aes(x = round, y = -rapm)) +
  geom_line(linewidth = 1, color = '#666769') +
  geom_point(color = '#666769') +
  geom_line(data = rapm_history_no_dnf_am %>% filter(entity_id == selected_entity),
            linewidth = 1, color = '#037A68') +
  geom_point(data = rapm_history_no_dnf_am %>% filter(entity_id == selected_entity),
             color = '#037A68') +
  geom_vline(aes(xintercept = 15), 
             color = '#404040', linetype = 'dashed') +
  geom_vline(aes(xintercept = 19), 
             color = '#404040', linetype = 'dashed') +
  geom_text(data = data.frame(1), aes(x = 17), label = 'AM 3 DNFs\nin 4 races',
            y= Inf,  color = '#404040', 
            inherit.aes = FALSE, vjust = 1.5, size = 3,fontface = "bold") +
  theme_saurabh() +
  labs(
    title = paste0("Aston Martin (AM) 2023 Rating"),
    subtitle = paste0("<b><span style='color:#037A68;'>DNF Exclusive</span>", " | "
                      , "<span style='color:#666769;'>DNF Inclusive</span></b>"),
    x = "",
    y = "Rating"
  ) +
  theme(
    plot.subtitle = element_markdown(), # Enable markdown for title
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  scale_x_continuous(
    breaks = seq_along(chart_2023_races),
    labels = chart_2023_races
  ) 




chart_printer(
  g = g, 
  chart_filepath = paste0("f1dataR - Exports/Visuals/Misc/am_dnf_outlier.png")
)
