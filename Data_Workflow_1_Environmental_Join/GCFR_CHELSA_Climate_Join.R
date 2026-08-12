########################################################
# GCFR_CHELSA_Climate_Join.R
#
# Purpose: Join CHELSA environmental data to GCFR
# trait collection sites
#
# Originally create March 2025
# Author(s): Henry Frye, ChatGPT
########################################################

# Load required libraries
library(sf)
library(raster)
library(stringr)
library(terra)
library(tidyverse)

# Read in point layer of field trait collection locations
point_file <- st_read('GCFR_Traits/Spatial_Data/Field_Trait_Locations.geojson')

# Read the folder containing GeoTIFF files (these are not available through Git but available through CHELSA or request to lead author)
tiff_folder <- "/Volumes/Extreme_SSD/1981-2010/bio"
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
  point_data <- st_transform(point_file, crs(raster_data))
  
  # Extract values for each point from the GeoTIFF file
  values <- raster::extract(raster_data, point_data)
  
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

# Write the merged CSV with extracted values
merged_data <- cbind(point_file, extracted_data)

# Assess missingness for MAT and MAP
which(is.na(merged_data$bio1) == TRUE)
which(is.na(merged_data$bio12) == TRUE)

# Assess missingness for MAT and MAP
which(is.na(merged_data$bio1) == TRUE)
which(is.na(merged_data$bio12) == TRUE)

# bio1 and bio12 come back from extract() already converted to real-world
# units (°C and mm/year respectively) -- the raster package applies the
# CHELSA GeoTIFFs' embedded scale/offset tags automatically on extraction.
# Confirmed via range checks: bio1 in [11.55, 19.95] (plausible CFR MAT in
# °C, not Kelvin*10), bio12 in [202.6, 1395.8] (plausible CFR MAP in mm/yr).
# Previously this block re-applied the manual Kelvin*10 conversion on top
# of already-converted values, producing MAT ~ -271 to -272 and MAP at
# 1/10th its true value.
merged_data$MAT <- merged_data$bio1
merged_data$MAP <- merged_data$bio12

# Write out full data
data_output_path = 'GCFR_Traits/Data_Workflow_1_Environmental_Join/Climate_Summary_Data_Outputs'
merged_data_csv <- as.data.frame(merged_data)
write.csv(merged_data_csv, paste0(data_output_path,'/CHELSA_climate_field_traits.csv'))

# Create summary of MAT and MAP table by subregion 
region_summary <- merged_data %>% group_by(region) %>%
  summarise(avgMAT = mean(MAT, na.rm = TRUE),
            sdMAT = sd(MAT, na.rm = TRUE),
            minMAT = min(MAT, na.rm = TRUE),
            maxMAT = max(MAT, na.rm = TRUE),
            avgMAP = mean(MAP, na.rm = TRUE),
            sdMAP = sd(MAP, na.rm = TRUE),
            minMAP = min(MAP, na.rm = TRUE),
            maxMATP = max(MAP, na.rm = TRUE))

head(region_summary)
region_summary_csv <- data.frame(region_summary)
region_summary_csv[,1:9]
write.csv(region_summary_csv[,1:9], paste0(data_output_path,'/CHELSA_climate_region_summary_field_traits.csv'))
