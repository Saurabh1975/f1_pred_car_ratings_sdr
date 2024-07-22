# File: f1_api_pull.R
# Purpose: Grab data via the Ergast API and save down RDS files


#F1 data package for hitting Ergast API
library(f1dataR)
#Packages to clean/process data
library(dplyr)
library(purrr)



## Drivers Pull & Save
driver_dfs <- map(1950:2024, ~load_drivers(season = .x))

drivers <- bind_rows(map(driver_dfs, ~mutate(.x, permanent_number = as.character(permanent_number)))) %>%
  select(-code, -permanent_number) %>%
  distinct()

saveRDS(object = drivers, file = paste0("api_data/drivers.rds"))



## Constructors Pull & Save
constructors <- load_constructors()
saveRDS(object = constructors, file = paste0("api_data/constructors.rds"))




## Schedule Pull & Save
schedule_dfs <- map(1950:2024, ~load_schedule(season = .x))

schedule <- bind_rows(schedule_dfs) %>%
  select(-lat, -long, -time) %>%
  filter(date < Sys.Date())

saveRDS(object = schedule, file = paste0("api_data/schedule.rds"))



## Results Load
process_results <- function(row) {
  print(paste0(row[["season"]], " - ", row[["round"]], " - ", row[["circuit_id"]]))
  
  #Assign meta data
  season <- row[["season"]]
  round <- row[["round"]]
  result <- load_results(season = season, round = round)
  circuit_id <- row[["circuit_id"]]  # Assuming circuit_id is a column in schedule_new
  
  result <- cbind(result, season = season, round = round, circuit_id = circuit_id, sprint = FALSE) 
  
  #Add qualifcation data if available
  if (season >= 2003){
    quali <- load_quali(season = season, round = round) %>%
      rename(quali_position = position) %>%
      select(driver_id, quali_position)
    
    result <- result   %>%
      left_join(quali, by = 'driver_id')
  }
  
  
  return(result)
}



# Apply the function to each row of the dataframe
result_dfs <- apply(schedule, 1, process_results)

results <- bind_rows(map(result_dfs, ~mutate(.x, fastest_rank = as.character(fastest_rank)))) %>%
  select(season, round, circuit_id, constructor_id, driver_id, quali_position, position, grid, points, status, gap, time_sec) %>%
  mutate(finished = grepl("Finished|Lap", status),
         finished = as.logical(finished))


saveRDS(object = results, file = paste0("api_data/results.rds"))


## Sprints Load
# Currently not used for anything
process_sprints <- function(row) {
  print(paste0(row[["season"]], " - ", row[["round"]], " - ", row[["circuit_id"]]))
  
  season <- row[["season"]]
  round <- row[["round"]]
  result <- load_sprint(season = season, round = round)
  circuit_id <- row[["circuit_id"]]  # Assuming circuit_id is a column in schedule_new
  
  
  result <- cbind(result, season = season, round = round, circuit_id = circuit_id,
                  sprint = TRUE) 
  
  
  
  return(result)
}

sprints_df <- apply(schedule %>% filter(!is.na(sprint_date)), 1, process_sprints)


sprints <- bind_rows(sprints_df) %>%
  select(season, round, circuit_id, constructor_id, driver_id, 
         position,points, status, gap, time_sec, sprint) %>%
  mutate(finished = grepl("Finished|Lap", status),
         finished = as.logical(finished))

saveRDS(object = sprints, file = paste0("api_data/sprint_results"))
