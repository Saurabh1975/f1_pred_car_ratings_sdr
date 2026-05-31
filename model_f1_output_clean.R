
library(dplyr)

position_weighted = TRUE
dnf_inclusive = FALSE

file_path =  sprintf("f1dataR - Exports/Data/RAPM Outputs/rapm_history_pos%s_%s_bootstrapped.rds",
                     ifelse(position_weighted, "Weighted", "Unweighted"),
                     ifelse(dnf_inclusive, "DNF", "noDNF"))



print(file_path)

#Read in latest RAPM History
rapm_history <- readRDS(file_path) 
  #  %>% filter(overall_round <= 1113)

write.csv(rapm_history %>% dplyr::select(-temp), file_path) 



# Get Driver History
driver_rapm_history <- rapm_history %>% 
  filter(grepl("-d", entity_id))  %>% 
  mutate(driver_id = as.character(sub("-d", "", entity_id))) %>% 
  left_join(driver_start_end, by = 'driver_id') %>%
  filter(model_date >= driver_start, model_date <= driver_end) %>%
  rename(current_constructor_id = constructor_id, current_primary_color = primary_color,
         current_parent_constructor_id = parent_constructor_id,
         current_parent_constructor_name = parent_constructor_name,
         current_constructor_name = constructor_name)   %>%
  left_join(driver_stints, by = 'driver_id') %>%
  filter(model_date >= stint_start, model_date <= stint_end) %>%
  mutate(rapm = -rapm,
         rapm_loess = -rapm_loess,
         rapm_blended = -rapm_blended,
         rapm_error = -rapm_error)

driver_file_path =  sprintf("f1dataR - Exports/Data/driver_rapm_history_pos%s_%s_bootstrapped.rds",
                     ifelse(position_weighted, "Weighted", "Unweighted"),
                     ifelse(dnf_inclusive, "DNF", "noDNF"))


saveRDS(object = driver_rapm_history, 
        file = driver_file_path)





# Get Constructor history at parent level
parent_construtor_start_end <- constructor_start_end %>%
  group_by(parent_constructor_id) %>%
  summarize(parent_constructor_start  = min(start_date),
            parent_constructor_end = max(end_date))


constructor_rapm_history <- rapm_history %>% 
  filter(grepl("-c", entity_id))  %>% 
  mutate(parent_constructor_id = as.character(sub("-c", "", entity_id))) %>% 
  left_join(parent_construtor_start_end, by = 'parent_constructor_id') %>%
  filter(model_date >= parent_constructor_start, model_date <= parent_constructor_end) %>%
  left_join(constructors %>% dplyr::select(-nationality),
            by = c('parent_constructor_id' = 'constructor_id')) %>%
  left_join(constructor_start_end %>% group_by(parent_constructor_id) %>% summarise(primary_color = max(primary_color)),
            by = c('parent_constructor_id')) %>%
  rename(parent_constructor_name = name) %>%
  mutate(rapm = -rapm,
         rapm_loess = -rapm_loess,
         rapm_blended = -rapm_blended,
         rapm_error = -rapm_error)


const_file_path =  sprintf("f1dataR - Exports/Data/constructor_rapm_history_pos%s_%s_bootstrapped.rds",
                            ifelse(position_weighted, "Weighted", "Unweighted"),
                            ifelse(dnf_inclusive, "DNF", "noDNF"))


saveRDS(object = constructor_rapm_history, 
        file = const_file_path)



diff_constructor <- setdiff(names(constructor_rapm_history), names(driver_rapm_history))

# Columns in driver_rapm_history but not in constructor_rapm_history
diff_driver <- setdiff(names(driver_rapm_history), names(constructor_rapm_history))

# Output the differences
print("Columns in constructor_rapm_history but not in driver_rapm_history:")
print(diff_constructor)

print("Columns in driver_rapm_history but not in constructor_rapm_history:")
print(diff_driver)


rapm_history_combined_cleaned <- bind_rows(constructor_rapm_history, driver_rapm_history) %>%
  left_join(schedule %>% 
              #dplyr::rename(overall_round = round_overall) %>%
              mutate(round = as.numeric(round)) %>%
              dplyr::select(season, round, race_name),  by = c('season', 'round')) %>% 
  mutate(race_full_name = paste0(season, " - Race ", round, ": ", race_name),
         display_name = ifelse(grepl("-d", entity_id), driver_name, parent_constructor_name))



history_file_path =  sprintf("f1dataR - Exports/Data/rapm_history_combined_cleaned_pos%s_%s.rds",
                           ifelse(position_weighted, "Weighted", "Unweighted"),
                           ifelse(dnf_inclusive, "DNF", "noDNF"))



write.csv(rapm_history_combined_cleaned %>% select(-temp), 
          '/Users/saurabhr/Documents/GitHub/f1_rapm_app/public/rapm_history_combined_cleaned.csv')


saveRDS(rapm_history_combined_cleaned %>%
            filter(season > 2013), history_file_path) 



rapm_history_combined_cleaned$temp


library(dplyr)
library(purrr)

# Find list-columns
list_cols <- names(rapm_history_combined_cleaned)[
  map_lgl(rapm_history_combined_cleaned, is.list)
]

list_cols