

library(tidyverse)
library(openxlsx)
library(sf)

# --- 1. Read and reshape from wide-transposed to tidy ---
# Assumes structure like your printout: col 1 = variable name,
# remaining columns = one per releve
raw <- read.xlsx("/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/hangklip_datum_rubber_sheet/Hangklip.CBoucherMSc.xlsx")

long <- raw %>%
  rename(variable = 1) %>%
  pivot_longer(-variable, names_to = "releve_col", values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value)

# --- 2. Parse fixed-width DDMMSS (degrees-minutes-seconds, 2-2-2 digits) ---
dms6_to_dd <- function(x) {
  x <- sprintf("%06d", as.integer(round(as.numeric(x)))) # pad to 6 digits, handles dropped leading zeros
  deg <- as.numeric(substr(x, 1, 2))
  min <- as.numeric(substr(x, 3, 4))
  sec <- as.numeric(substr(x, 5, 6))
  deg + min / 60 + sec / 3600
}

long <- long %>%
  mutate(
    lat_dd = -dms6_to_dd(`Latitude (degr./min/sec)`), # force negative — southern hemisphere
    lon_dd =dms6_to_dd(`Longitude (degr./min/sec)`)  # positive — eastern hemisphere
  )

# Sanity check — Kogelberg/Hangklip should land roughly lat -34.1 to -34.4, lon 18.7 to 19.1
long %>% summarise(across(c(lat_dd, lon_dd), range, na.rm = TRUE)) %>% print()

# --- 3. Build spatial layer (Cape Datum geographic) ---
pts_cape <- long %>%
  filter(!is.na(lat_dd), !is.na(lon_dd)) %>%
  rename(releve_number = `Relevé number`) %>%
  select(releve_number, lat_dd, lon_dd) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = 4222) # Cape geographic

# --- 4. Reproject as needed ---
pts_hbek94 <- st_transform(pts_cape, 4148) # Hartebeesthoek94 (~WGS84), for overlay w/ modern imagery
pts_lo19 <- st_transform(pts_cape, 22279) # Cape / Lo19 projected grid, if you need meters

# --- 5. Write to GeoPackage ---
st_write(pts_cape, "/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/hangklip_datum_rubber_sheet/boucher_releves.gpkg", layer = "releves_cape_datum", delete_dsn = TRUE)
st_write(pts_hbek94, "/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/hangklip_datum_rubber_sheet/boucher_releves.gpkg", layer = "releves_hartebeesthoek94", append = TRUE)
st_write(pts_lo19, "/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/hangklip_datum_rubber_sheet/boucher_releves.gpkg", layer = "releves_lo19", append = TRUE)



# Look for suspicious rounding patterns — lots of "00" or "30" seconds
# suggests visual/manual estimation rather than instrument reading
long %>%
  mutate(lat_sec = as.integer(substr(sprintf("%06d", as.integer(`Latitude (degr./min/sec)`)), 5, 6))) %>%
  count(lat_sec) %>%
  arrange(desc(n))

long %>%
  mutate(lat_sec = as.integer(substr(sprintf("%06d", as.integer(`Latitude (degr./min/sec)`)), 5, 6))) %>%
  filter(lat_sec == "12" |lat_sec == "48")

# make hangklip only layer for modification from releve data
releve <-  read.csv('/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/releve.csv')

hangklip_sf <- releve %>%
  filter(subregion == 'hangklip') %>%
  select(plot, latitude, longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

st_write(hangklip_sf, "/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/hangklip_datum_rubber_sheet/hangklip_releve_old.gpkg", layer = "plots", delete_dsn = TRUE)
st_write(hangklip_sf, "/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/hangklip_datum_rubber_sheet/hangklip_releve_new.gpkg", layer = "plots", delete_dsn = FALSE)
  
