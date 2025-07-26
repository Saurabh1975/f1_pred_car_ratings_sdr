library(dplyr)

df_test <- read.csv('f1dataR - Exports/Models/rapm_history_combined_cleaned.csv') %>%
  mutate(race_full_name = paste0(season, " - Race ", round, ": ", race_name),
         display_name = ifelse(grepl("-d", entity_id), driver_name, parent_constructor_name))


write.csv(df_test, 'app_data/rapm_history_combined_cleaned.csv')



missing_constructor_mapping <- df_test %>%
  group_by(parent_constructor_id, constructor_id, primary_color) %>%
  summarize(dummy = 1)


driver_df_list <- driver_rapm_history %>%
  group_by(entity_id) %>%
  summarize(
    latest_season = max(season),
    races = n())













# Load the required library
if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}

library(magick)

# Define the directories
input_dir <- "supplementary_data/Images/Constructor/avif/"
output_dir <- "supplementary_data/Images/Constructor/png/"


# List all .avif files in the input directory
avif_files <- list.files(input_dir, pattern = "\\.avif$", full.names = TRUE)

# Loop through each .avif file and convert to .png
for (file in avif_files) {
  
  print(file)
  # Read the .avif file
  image <- image_read(file)
  
  # Generate the output filename
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), ".png"))
  
  # Write the image as .png
  image_write(image, path = output_file, format = "png")
  
  # Print a success message for each conversion
  cat("Converted:", file, "to", output_file, "\n")
}

cat("All files converted successfully!\n")
