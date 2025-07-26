
library(dplyr)


#Get DNF Data

file_path = "f1dataR - Exports/Data/rapm_history_posWeighted_DNF_bootstrapped.rds"


#Read in latest RAPM History
rapm_history_dnf <- readRDS(file_path) 



#Get No DNF Data

file_path = "f1dataR - Exports/Data/rapm_history_posWeighted_noDNF_bootstrapped.rds"

rapm_history_no_dnf <- readRDS(file_path) 





# Get Constructor history at parent level
parent_construtor_start_end <- constructor_start_end %>%
  group_by(parent_constructor_id) %>%
  summarize(parent_constructor_start  = min(start_date),
            parent_constructor_end = max(end_date))


constructor_rapm_history_no_dnf <- rapm_history_no_dnf %>% 
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


