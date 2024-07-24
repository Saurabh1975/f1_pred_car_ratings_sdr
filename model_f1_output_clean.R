

#Read in latest RAPM History
rapm_history <- readRDS("f1dataR - Exports/Models/rapm_history_posWeighted_noDNF_bootstrapped.rds") 



# Get Driver History
driver_rapm_history <- rapm_history %>% 
  filter(grepl("-d", entity_id))  %>% 
  mutate(driver_id = as.character(sub("-d", "", entity_id))) %>% 
  left_join(driver_start_end, by = 'driver_id') %>%
  filter(model_date >= driver_start, model_date <= driver_end) %>%
  rename(current_constructor_id = constructor_id, current_primary_color = primary_color,
         current_parent_constructor_id = parent_constructor_id)   %>%
  left_join(driver_stints %>% dplyr::select(-parent_constructor_name, -constructor_name) , by = 'driver_id') %>%
  filter(model_date >= stint_start, model_date <= stint_end) %>%
  mutate(rapm = -rapm,
         rapm_loess = -rapm_loess,
         rapm_blended = -rapm_blended,
         rapm_error = -rapm_error)



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





