
results <- readRDS("api_data/results.rds")




## Schedule Pull & Save
schedule_dfs <- map(2023:2024, ~load_schedule(season = .x))

schedule <- bind_rows(schedule_dfs) %>%
  select(-lat, -long, -time) %>%
  filter(date < Sys.Date())


# Apply the function to each row of the dataframe
result_dfs <- apply(schedule, 1, process_results)