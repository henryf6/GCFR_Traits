########################################################
# GCFR_CHELSA_Climate_Join.R
#
# Purpose: Join CHELSA environmental data to Dimensions
# trait collection sites
#
# Date: March 2025
# Author(s): Henry Frye, ChatGPT
########################################################

# Load required libraries
library(sf)
library(raster)
library(stringr)
library(terra)
library(tidyverse)


# this is the most recent bioscape plot center main file
point_file <- st_read('/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/Data/SpatialData/Field_Trait_Locations.geojson')

# Older joined version that needs to be updated (Dec. 2024)
#point_file <- st_read('TownsendBioSCapePlotsGPSCleanV1.geojson')

# Step 2: Read the folder containing GeoTIFF files
tiff_folder <- "/Volumes/Extreme_SSD/1981-2010/bio"
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)

# Step 3: Initialize an empty data frame to store the extracted values
extracted_data <- NULL
variable_names <- c()

# Step 4: Loop through the GeoTIFF files
for (i in 1:length(tiff_files)) {
  # Step 4a: Read the GeoTIFF file
  raster_data <- raster(tiff_files[i])
  
  # Step 4b: Extract the variable name from the file path
  file_name <- basename(tiff_files[i])
  variable_name <- gsub("\\.[^.]*$", "", file_name)
  variable_name <- gsub("[^a-zA-Z0-9_]", "", variable_name)
  variable_name <- gsub("CHELSA_", "", variable_name)
  variable_name <- gsub("_19812010_V21", "", variable_name)
  
  # Step 4c Set the CRS of the GPS point data to match the GeoTIFF
  point_data <- st_transform(point_file, crs(raster_data))
  
  # Step 4d Extract values for each point from the GeoTIFF file
  values <- extract(raster_data, point_data)
  
  # Check if any points fall outside the extent of the GeoTIFF
  valid_points <- !is.na(values)
  
  # Determine the number of rows for the extracted values
  num_rows <- ifelse(i == 1, sum(valid_points), nrow(extracted_data))
  
  # Pad the extracted values with NAs to match the number of rows
  if (sum(valid_points) < num_rows) {
    values <- c(values, rep(NA, num_rows - sum(valid_points)))
  }
  
  # Add the extracted values to the data frame
  extracted_data <- cbind(extracted_data, values)
  
  # Store the variable name
  variable_names <- c(variable_names, variable_name)
}

# Step 5: Set column names of extracted_data
colnames(extracted_data) <- variable_names

# Step 6: Write the merged CSV with extracted values
merged_data <- cbind(point_file, extracted_data)

# Step 7: Assess missingness for MAT and MAP
which(is.na(merged_data$bio1) == TRUE)
which(is.na(merged_data$bio12) == TRUE)

# Step 8: Convert MAT and MAP to original units

# MAT has a scale factor of 0.1 and offset of -237.15
merged_data$MAT <- (merged_data$bio1 *.1) - 237.15
 
# MAP has a scale factor 0.1 and is measured in kg m^-2 per year and convert it to
# mm per year
merged_data$MAP <- (merged_data$bio12 *.1) 

# Step 9: Write out full data
data_output_path = '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/Data/ClimateSummaryData'
merged_data_csv <- as.data.frame(merged_data)
write.csv(merged_data_csv, paste0(data_output_path,'/CHELSA_climate_field_traits.csv'))

# Step 10 create summary of MAT and MAP table by subregion 
region_summary <- merged_data %>% group_by(region) %>%
  summarise(avgMAT = mean(MAT),
            sdMAT = sd(MAT),
            minMAT = min(MAT),
            maxMAT = max(MAT),
            avgMAP = mean(MAP),
            sdMAP = sd(MAP),
            minMAP = min(MAP),
            maxMATP = max(MAP))

head(region_summary)
region_summary_csv <- data.frame(region_summary)
region_summary_csv[,1:9]
write.csv(region_summary_csv[,1:9], paste0(data_output_path,'/CHELSA_climate_region_summary_field_traits.csv'))
