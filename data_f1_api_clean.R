



#Packages to clean/process data
library(dplyr)
library(purrr)


## Read in All Historical Data

#Schedule
schedule <- readRDS("api_data/schedule.rds") %>%
  mutate(round_overall = row_number())
#Constructors
constructors <- readRDS("api_data/constructors.rds")
#Drivers
drivers <- readRDS("api_data/drivers.rds") %>% 
  mutate(driver_name = paste0(given_name, " ", family_name))
#Results
results <- readRDS("api_data/results.rds")
#sprint
sprint_results <- readRDS("api_data/sprint_results.rds")



## Read/Prep anicillary data

# Get start/end of constructors by round
constructor_start_end <- results %>% 
  left_join(schedule %>% dplyr::select(season, round, round_overall, circuit_id, date), 
            by = c('season', 'round', 'circuit_id'))  %>%
  group_by(constructor_id) %>%
  summarize(start_season = min(season), end_season = max(season),
            start_date = min(date), end_date = max(date), 
            start_round_overall = min(round_overall), 
            end_round_overall = max(round_overall))

write.csv(constructor_start_end, 'supplementary_data/constructor_info.csv')


# Read in constructor colors
constructor_color <- read.csv('supplementary_data/constructors_colors.csv') %>%
  rename(constructor_id = constructorRef) %>%
  dplyr::select(constructor_id, primary_color, secondary_color)



## Create Constructor Data 


constructor_mapping <- read.csv("supplementary_data/constructor_mapping.csv") %>%
  dplyr::select(parent_constructor_id, constructor_id, primary_color) %>%
  left_join(constructors %>% dplyr::select(-nationality), 
            by = c('parent_constructor_id' = 'constructor_id')) %>%
  left_join(constructors %>% dplyr::select(-nationality), 
            by = c('constructor_id' = 'constructor_id')) %>%
  rename(parent_constructor_name = name.x, constructor_name = name.y)

constructor_start_end <- results %>% 
  mutate(season = as.integer(season)) %>%
  inner_join(schedule %>% dplyr::select(season, round, round_overall, circuit_id, date), 
             by = c('season', 'round', 'circuit_id'))  %>%
  group_by(constructor_id) %>%
  summarize(start_season = min(season), end_season = max(season),
            start_date = min(date), end_date = max(date), 
            start_round_overall = min(round_overall), 
            end_round_overall = max(round_overall)) %>%
  left_join(constructor_mapping, by = c('constructor_id'))




## Create Driver Data

driver_start_end <-  results %>%
  mutate(season = as.integer(season)) %>%
  inner_join(schedule, by = c('season', 'round', 'circuit_id')) %>%
  arrange(driver_id, desc(date)) %>%
  group_by(driver_id) %>%
  summarise(driver_start = as.Date(min(date)),     # Earliest date for the driver
            driver_end = as.Date(max(date)),       # Latest date for the driver
            driver_start_round_overall =  min(round_overall),     
            driver_end_round_overall = max(round_overall),       
            constructor_id = first(constructor_id)) %>%
  left_join(constructor_mapping, by = 'constructor_id') 


driver_stints <- results %>%
  mutate(season = as.integer(season)) %>%
  inner_join(schedule, by = c('season', 'round', 'circuit_id')) %>%
  arrange(driver_id, date) %>%
  group_by(driver_id) %>%
  mutate(change = constructor_id != lag(constructor_id, default = constructor_id[1])) %>%
  group_by(driver_id, stint = cumsum(change)) %>%
  summarise(constructor_id = first(constructor_id),
            stint_start = as.Date(min(date)),
            stint_end = as.Date(max(date)),
            stint_start_overall_round = min(round_overall),
            stint_end_overall_round = max(round_overall),
            .groups = 'drop') %>%
  dplyr::select(-stint) %>%
  group_by(driver_id, constructor_id) %>%
  arrange(stint_start) %>%
  mutate(stint = row_number()) %>%
  left_join(constructor_mapping, by = c('constructor_id')) %>%
  left_join(drivers %>% dplyr::select(driver_id, driver_name), by = 'driver_id')



## Clean results data for modeling

results_unfiltered <- results %>%
  mutate(season = as.integer(season)) %>%
  left_join(schedule, 
            by = c('season', 'round', 'circuit_id'))  %>%
  left_join(constructor_mapping, 
            by = c('constructor_id'))  %>%
  filter(season >= 2012) %>%
  dplyr::select(season, round, date, round_overall, circuit_id, parent_constructor_id, 
                constructor_id, driver_id, quali_position,  position, finished, status) %>%
  mutate(date = as.Date(date),
         season = as.numeric(season),
         driver_id = paste0(driver_id,"-d"),
         constructor_id = paste0(constructor_id,"-c"),
         parent_constructor_id = paste0(parent_constructor_id,"-c"),
         quali_position = as.numeric(quali_position),
         position = as.numeric(position),
         dnf_driver = case_when(
           grepl("Collision|Accident|Spun Off", status) ~ TRUE,
           TRUE ~ FALSE
         ),
         dnf_constructor = !finished & case_when(
           !grepl("Collision|Accident|Spun Off", status) ~ TRUE,
           TRUE ~ FALSE
         )
  ) %>% 
  group_by(date)


