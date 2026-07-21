# File: f1_api_pull.R
# Purpose: Grab data via the Ergast API and save down RDS files


#F1 data package for hitting Ergast API
library(f1dataR)
#Packages to clean/process data
library(dplyr)
library(purrr)

#Current Date
current_year <- as.numeric(format(Sys.Date(),'%Y'))

##Drivers

#Update Drivers
drivers <- readRDS(file = paste0("api_data/drivers.rds"))

#Get Current Year Drivers
current_drivers <- load_drivers(current_year)  %>%
  select(-code, -permanent_number) %>%
  distinct()

#Distinct Drivers
drivers <- drivers %>%
  bind_rows(current_drivers) %>%
  distinct()

#Save Drivers
saveRDS(object = drivers, file = paste0("api_data/drivers.rds"))



##Constructors


# Constructors Pull & Save
constructors <- load_constructors()
saveRDS(object = constructors, file = paste0("api_data/constructors.rds"))




## Schedule

#Get saved schedule
schedule <- readRDS(file = paste0("api_data/schedule.rds")) %>%
  select(-round_overall)


#Get current schedule
current_schedule <- load_schedule(current_year)  %>%
  select(-lat, -long, -time) %>%
  filter(date < Sys.Date()) %>%
  mutate(season = as.integer(season))

#Identify and add missing races
missing_races <- anti_join(current_schedule, schedule, by = c("circuit_id", "season")) 
print(paste0("Missing ", nrow(missing_races), " Races: ", missing_races %>% pull(circuit_id)))


schedule <- schedule %>%
  rbind(missing_races) %>%
  arrange(season,  round) %>%
  mutate(
    round_overall = row_number()
  )

#Save down new races
saveRDS(object = schedule, file = paste0("api_data/schedule.rds"))

## Results

if(nrow(missing_races > 0)){
  # Read in old results
  results <- readRDS(file = paste0("api_data/results.rds"))
  
  
  
  # Apply the function to each row of the dataframe
  new_result_dfs <- apply(missing_races, 1, process_results)
  
  new_results <- bind_rows(map(new_result_dfs, ~mutate(.x, fastest_rank = as.character(fastest_rank)))) %>%
    select(season, round, circuit_id, constructor_id, driver_id, quali_position, position, grid, points, status, gap, time_sec) %>%
    mutate(finished = grepl("Finished|Lap", status),
           finished = as.logical(finished))
  
  # Bind in new results, save down
  results <- results %>%
    rbind(new_results) %>%
    distinct()
  
  
  saveRDS(object = results, file = paste0("api_data/results.rds"))
  
}


# 
# ## Sprints
# 
# 
# process_sprints <- function(row) {
#   print(paste0(row[["season"]], " - ", row[["round"]], " - ", row[["circuit_id"]]))
#   
#   season <- row[["season"]]
#   round <- row[["round"]]
#   result <- load_sprint(season = season, round = round)
#   circuit_id <- row[["circuit_id"]]  # Assuming circuit_id is a column in schedule_new
#   
#   
#   result <- cbind(result, season = season, round = round, circuit_id = circuit_id,
#                   sprint = TRUE) 
#   
#   
#   
#   return(result)
# }
# 
# #Grab new sprints if they exist
# if(nrow(missing_races %>% filter(!is.na(sprint_date))) > 0){
#   sprints <- readRDS(file = paste0("api_data/sprint_results.rds"))
#   
#   
#   new_sprints_df <- apply(missing_races %>% filter(!is.na(sprint_date)), 1, process_sprints)
#   
#   new_sprints <- bind_rows(new_sprints_df) %>%
#     select(season, round, circuit_id, constructor_id, driver_id, 
#            position,points, status, gap, time_sec, sprint) %>%
#     mutate(finished = grepl("Finished|Lap", status),
#            finished = as.logical(finished))
#   
#   sprints <- results %>%
#     rbind(new_sprints) %>%
#     distinct()
#     
#   saveRDS(object = sprints, file = paste0("api_data/sprint_results.rds"))
#   
#   
#   
#   
# }
# 
# 
