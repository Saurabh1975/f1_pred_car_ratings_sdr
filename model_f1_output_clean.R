
library(dplyr)

#Read in latest RAPM History
rapm_history <- readRDS("f1dataR - Exports/Models/rapm_history_posWeighted_noDNF_bootstrapped.rds") 
  #  %>% filter(overall_round <= 1113)

write.csv(rapm_history %>% dplyr::select(-temp), "f1dataR - Exports/Models/rapm_history_posWeighted_noDNF_bootstrapped.csv") 



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

saveRDS(object = driver_rapm_history, 
        file = paste0("f1dataR - Exports/Data/driver_rapm_history_posWeighted_noDNF_bootstrapped.csv"))





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

saveRDS(object = constructor_rapm_history, 
        file = paste0("f1dataR - Exports/Data/constructor_rapm_history_posWeighted_noDNF_bootstrapped.csv"))



diff_constructor <- setdiff(names(constructor_rapm_history), names(driver_rapm_history))

# Columns in driver_rapm_history but not in constructor_rapm_history
diff_driver <- setdiff(names(driver_rapm_history), names(constructor_rapm_history))

# Output the differences
print("Columns in constructor_rapm_history but not in driver_rapm_history:")
print(diff_constructor)

print("Columns in driver_rapm_history but not in constructor_rapm_history:")
print(diff_driver)


rapm_history_combined_cleaned <- rbind(constructor_rapm_history, driver_rapm_history) %>%
  left_join(schedule %>% 
              dplyr::rename(overall_round = round_overall) %>%
              dplyr::select(overall_round, race_name),  by = c('overall_round'))


write.csv(rapm_history_combined_cleaned %>% dplyr::select(-temp), "f1dataR - Exports/Models/rapm_history_combined_cleaned.csv") 

