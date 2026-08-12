########################################################
# GCFR_Elevation_Join.R
#
# Purpose: Join Earth Engine derived data to GCFR
# trait collection sites
#
# Date: March 2025
# Author(s): Henry Frye, ChatGPT
########################################################

# Load required packages
library(sf)
library(terra)
library(stars)
library(dplyr)
library(stringr)
library(tidyverse)

# Set the path to the directory containing the TIFF files (these files are accessible through EarthExplorer or from request of lead author)
tiff_directory <- '/Volumes/Extreme_SSD/EarthEnginePellieData'

# Read the CSV file containing point data
point_data <- st_read('GCFR_Traits/Spatial_Data/Field_Trait_Locations.geojson')

# Get the unique raster folders from the CSV file
tiff_folders <- c("Chili", "Dem", "TopoDiv") 

# Create an empty list to store the extracted values
extracted_values <- list()
extracted_folder <- list()

for (folder in tiff_folders) {
  # Get the list of TIFF files in the current folder
  tiff_files <- list.files(file.path(tiff_directory, folder), pattern = ".tif$", full.names = TRUE)
  
  # Create an empty list to store the extracted values
  extracted_values <- list()
  
  # Loop through each raster file
  for (file in tiff_files) {
    # Read the raster file
    raster_data <- rast(file)
    
    # Set the coordinate reference system (CRS) of the point data to match the raster
    projected_points <- st_transform(st_as_sf(point_data, coords = c("longitude", "latitude"), crs = 4326), crs(raster_data))
    
    # Extract raster values for each point
    values <- terra::extract(raster_data, projected_points)
    values <- values[,-1]
    # Join extracted values back to data
    joined_points <- cbind(projected_points,values)
    
    # remove points that didn't overlap with that particular raster
    joined_points <- joined_points %>% filter(!is.na(values))
    
    
    # Store the extracted values of the 
    extracted_values[[file]] <- data.frame(joined_points$longitude, joined_points$latitude, joined_points$values)
  }
  
  combined_values <- do.call(rbind, extracted_values)
  rownames(combined_values) <- NULL
  extracted_folder[[folder]] <- combined_values
  
}


# Now merge data together
colnames(extracted_folder$Chili) <- c("longitude", "latitude",'Chili')
extracted_folder$Chili <- unique(extracted_folder$Chili)
point_data <- dplyr::left_join(point_data, extracted_folder$Chili, by = c("longitude", "latitude"))

colnames(extracted_folder$Dem) <- c("longitude", "latitude",'Elevation')
extracted_folder$Dem <- unique(extracted_folder$Dem)
point_data <- dplyr::left_join(point_data, extracted_folder$Dem, by = c("longitude", "latitude"))

colnames(extracted_folder$TopoDiv) <- c("longitude", "latitude",'TopoDiv')
extracted_folder$TopoDiv <- unique(extracted_folder$TopoDiv)
point_data <- dplyr::left_join(point_data, extracted_folder$TopoDiv, by = c("longitude", "latitude"))

# Check missingingness
missing_elev <- which(is.na(point_data$Elevation) == TRUE)

# Note a couple missing values in HTR, but unlikely to affect range reporting
point_data$region[missing_elev]

# DEM has a scale factor of 30.922080775909325
point_data$Elevation <- point_data$Elevation * (1/30.922080775909325)

# Write out full join
data_output_path = 'GCFR_Traits/Data_Workflow_1_Environmental_Join/Climate_Summary_Data_Outputs'

write.csv(point_data, paste0(data_output_path,'/Elevation_field_traits.csv'))

# Write out region summary
region_summary <- point_data %>% group_by(region) %>%
  summarise(maxElev = max(Elevation, na.rm = TRUE),
            minElev = min(Elevation, na.rm = TRUE))
region_summary
region_summary_csv<- as.data.frame(region_summary)
write.csv(region_summary_csv[,1:3], paste0(data_output_path,'/Elevation_region_summary_field_traits.csv'))
