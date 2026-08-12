########################################################
# GCFR_Rainfall_Climate_Join.R
#
# Purpose: Join CHELSA environmental data to GCFR
# trait collection sites
#
# Date: March 2025
# Author(s): Henry Frye, ChatGPT
########################################################

# Load required libraries
library(sf)
library(raster)
library(stringr)
library(dplyr)

# Read the CSV file containing point data of field trait collections
point_data <- st_read('GCFR_Traits/Spatial_Data/Field_Trait_Locations.geojson')

# Read the folder containing GeoTIFF files (available via CHELSA or upon request of co-author)
tiff_folder <- "/Volumes/Extreme_SSD/1981-2010/pr"
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)

# Initialize an empty data frame to store the extracted values
extracted_data <- NULL
variable_names <- c()

# Loop through the GeoTIFF files
for (i in 1:length(tiff_files)) {
  # Read the GeoTIFF file
  raster_data <- raster(tiff_files[i])
  
  # Extract the variable name from the file path
  file_name <- basename(tiff_files[i])
  variable_name <- gsub("\\.[^.]*$", "", file_name)
  variable_name <- gsub("[^a-zA-Z0-9_]", "", variable_name)
  variable_name <- gsub("CHELSA_", "", variable_name)
  variable_name <- gsub("_19812010_V21", "", variable_name)
  
  # Set the CRS of the GPS point data to match the GeoTIFF
  point_data_sf <- st_as_sf(point_data, coords = c("GP_Long_E_adj", "GP_Lat_S_adj"), crs = st_crs(raster_data))
  
  # Project the GPS points to the same CRS as the GeoTIFF
  point_data_sf <- st_transform(point_data_sf, crs = st_crs(raster_data))
  
  # Extract values for each point from the GeoTIFF file
  values <- extract(raster_data, point_data_sf)
  
  # Check if any points fall outside the extent of the GeoTIFF
  #valid_points <- !is.na(values)
  
  # Determine the number of rows for the extracted values
  #num_rows <- ifelse(i == 1, sum(valid_points), nrow(extracted_data))
  
  # Pad the extracted values with NAs to match the number of rows
  #if (sum(valid_points) < num_rows) {
  #  values <- c(values, rep(NA, num_rows - sum(valid_points)))
  #}
  
  # Add the extracted values to the data frame
  extracted_data <- cbind(extracted_data, values)
  
  # Store the variable name
  variable_names <- c(variable_names, variable_name)
}

# Set column names of extracted_data
colnames(extracted_data) <- variable_names

extracted_data <- as.data.frame(extracted_data)

# The following steps calculate the percent of rainfall in the austral winter
# Calculate the sum of each row across all columns
extracted_data <- extracted_data %>%
  mutate(AnnualTotPr = rowSums(across(everything())))

# Calculate the denominator as the sum of col1, col2, and col3
extracted_data <- extracted_data %>%
  mutate(WinterRainfallTot = pr_06 + pr_07 + pr_08)


# Create the new column by dividing row_sum by the denominator
extracted_data  <- extracted_data  %>%
  mutate(WinterPrProp = WinterRainfallTot / AnnualTotPr)
extracted_data

# Define 0.48 threshold of WinterPrProp, categorize regions to winter rainfal, neither, or summer/allyear
extracted_data <- extracted_data %>%
  mutate(RainfallCat = case_when(
    WinterPrProp >= 0.48 ~ "Winter",
    WinterPrProp > 0.33 & WinterPrProp< 0.48 ~ "Neither",
    WinterPrProp <= 0.33 ~ "Summer/All_Year"
  ))

extracted_data <- extracted_data %>% dplyr::select(!AnnualTotPr)

# Write the merged CSV with extracted values
merged_data <- cbind(point_data, extracted_data)

data_output_path = 'GCFR_Traits/Data_Workflow_1_Environmental_Join/Climate_Summary_Data_Outputs'

write.csv(merged_data, paste0(data_output_path,'/Rainfall_field_traits.csv'))

# Write out regional summary
regional_summary <- merged_data %>% group_by(region) %>%
  summarise(MeanWinterPrProp = mean(WinterPrProp, na.rm = TRUE),
  SdWinterPrProp = sd(WinterPrProp, na.rm = TRUE))
regional_summary_csv <- as.data.frame(regional_summary)
write.csv(regional_summary_csv[,1:3], paste0(data_output_path,'/Rainfall_regional_summary_field_traits.csv'))
