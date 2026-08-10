########################################################
# Workflow3_Data_Polish.R
#
# Purpose: Make uniform column names and order,
# create data dictionaries
#
# Date Created: July 2025
# Most recent modification: 
# Author(s): Henry Frye, Copilot, Claude
########################################################

# Load in libraries
library(tidyverse)
library(sf)

# Load in cleaned data from Workflow 2
canopy_chem <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Intermediate_Outputs/canopy_chem_intermediate.csv')
leaf_struc <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Intermediate_Outputs/leaf_struc_intermediate.csv')
spectrait <- read_csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/species_trait_taxa_clean.csv')
vnirspec <- read_csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/vnir_taxa_clean.csv')
releve <- read_csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/releve_taxa_clean.csv')

# Load in quality check data (leaf structure)
lma_outliers        <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_lma_values.csv',
                                col_types = cols(replicate = col_character()))
thickness_outliers  <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_thickness_values.csv',
                                col_types = cols(replicate = col_character()))
fwc_outliers        <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_fwc_values.csv',
                                col_types = cols(replicate = col_character()))
ldmc_outliers       <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_ldmc_values.csv',
                                col_types = cols(replicate = col_character()))
succulence_outliers <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_succulence_values.csv',
                                col_types = cols(replicate = col_character()))
twig_fwc_outliers   <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_twig_fwc_values.csv',
                                col_types = cols(replicate = col_character()))
struc_outliers             <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/flagged_struc_values.csv')

# Load in quality check data (foliar chemistry and canopies)
chem_outliers             <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_chemistry_values.csv')
canopy_geom_outliers      <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_canopy_geometry_values.csv')
height_magnitude_outliers <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_height_magnitude_values.csv')
canopy_magnitude_outliers <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_canopy_magnitude_values.csv')
branch_order_outliers     <- read_csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/bad_branch_order_values.csv')
n_lma_covariation_outliers <- read_csv(
  'GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/outlying_n_lma_covariation_values.csv',
  col_types = cols(replicate = col_character())
)

# read in Manning and Goldblatt classification for a data fix on releve data
taxa <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/GMTaxonomy.csv')
comm_loc <- read.csv('GCFR_Traits/Spatial_Data/Comm_Plot_Lat_Lon_Coords.csv')

# Clean up field traits (foliar chemistry and canopy) #####
canopy_chem_polished <- canopy_chem %>% rename('scientific_name_original' = 'Species', # original designation
                                             'genus_MG' = 'Genus_GM', # match the order of the flora citation
                                             'family_MG' = 'Family_GM', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO',
                                             'subregion' = 'region',
                                             'canopy_cover_cm2' = 'canopy_area_cm2') %>% # match manuscript descriptions
                                             select(!c(genus_MG, NewUID)) %>%
                                            unite(sample_ID, date, sample, subregion, collector, sep = "_", remove = FALSE) %>%
                                            relocate(sample_ID, .before = 1)
# remove this column due to mismatch issues with backbone

# Remove [] from authority column
canopy_chem_polished$scientific_name_authorship_WFO <- gsub("\\[|\\]", "", canopy_chem_polished$scientific_name_authorship_WFO)

# Create year of measurement column (based on this local file: /Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/ZA_Dimensions_Data/data_base/Spec_Trait_All.csv)
canopy_chem_polished <- canopy_chem_polished %>% mutate(year = case_when(
  subregion == 'baviaanskloof' ~ 2011,
  subregion == 'htr' ~ 2014,
  subregion == 'hangklip' ~ 2012,
  subregion == 'langeberg' ~ 2012,
  subregion == 'cederberg' ~ 2012,
  subregion == 'cape_point' ~ 2010
))


# Format and convert dates
field_dates_str <- sprintf("%04d", canopy_chem_polished$date)
month <- substr(field_dates_str, 1, 2)
day <- substr(field_dates_str, 3, 4)
full_dates_field <- paste(canopy_chem_polished$year,month, day, sep = "-")
canopy_chem_polished$date <- as.Date(full_dates_field)

# year column no longer necessary
canopy_chem_polished <- canopy_chem_polished %>% dplyr::select(!year)

# Fix pubescence column: change blank pubescence category to NA, and fix trailing spaces 
# Clean the column
canopy_chem_polished <- canopy_chem_polished %>%
  mutate(
    pubescence = na_if(pubescence, ""),             # Convert "" to NA
    pubescence = str_trim(pubescence),              # Remove leading/trailing whitespace
    pubescence = str_to_upper(pubescence),          # Convert to uppercase
    pubescence = case_when(
    pubescence == "Y" ~ "B", # Convert Y to B, this happened in two cases, both for Phylica rigidifolia. Looking at iNat suggest that both leaves and stem are pubsecent
    pubescence == "N?" ~ "N", # This occured only for two observations of galenia africana, it should be glabrous except when young. The uncertainty probably occured since this observation was likely a youger individual
    TRUE ~ pubescence                           # Keep others as is
    )
  )


# Remove obvious carbon outliers above 100% and treat as NA
canopy_chem_polished <- canopy_chem_polished %>% mutate(percent_C = na_if(percent_C,max(canopy_chem_polished$percent_C, na.rm = TRUE)))

# ---- NA out canopy axis/area values for confirmed geometry mismatches ----
# Axis1, axis2, and area are NA'd together -- unlike the lma "suspected_bad"
# check belwow, geometry mismatches don't tell us whether an axis was mis-entered
# or the area was mis-calculated, so we can't isolate the bad column and
# stay conservative by clearing all three.

canopy_geom_keys <- canopy_geom_outliers %>%
  distinct(sample_id) %>%
  rename(sample_ID = sample_id)

canopy_cols <- c("canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_cover_cm2")

canopy_chem_polished_flagged <- canopy_chem_polished %>%
  mutate(across(all_of(canopy_cols),
                ~ if_else(sample_ID %in% canopy_geom_keys$sample_ID, NA_real_, .)))

# confirm the expected number of individuals lost these three columns
canopy_chem_polished_flagged %>%
  filter(sample_ID %in% canopy_geom_keys$sample_ID) %>%
  summarise(across(all_of(canopy_cols), ~ sum(is.na(.))))

# ---- Build a single quality_flag column (multiple reasons possible) -----
# No replicate-based confidence tiering here (no within-individual
# replication to cross-check against), so these stay as flags for reviewer
# visibility/downstream filtering rather than triggering removal.

flag_sources <- list(
  height_magnitude  = height_magnitude_outliers  %>% distinct(sample_id),
  canopy_magnitude  = canopy_magnitude_outliers  %>% distinct(sample_id),
  n_lma_covariation = n_lma_covariation_outliers %>% distinct(sample_id)
)

flag_table <- bind_rows(
  lapply(names(flag_sources), function(nm) {
    flag_sources[[nm]] %>% mutate(flag_source = nm)
  })
) %>%
  rename(sample_ID = sample_id) %>%
  group_by(sample_ID) %>%
  summarise(quality_flag = paste(sort(unique(flag_source)), collapse = "; "),
            .groups = "drop")

canopy_chem_polished_flagged <- canopy_chem_polished_flagged %>%
  left_join(flag_table, by = "sample_ID") %>%
  mutate(quality_flag = replace_na(quality_flag, ""))

canopy_chem_polished_flagged %>% count(quality_flag, sort = TRUE)

# Write out polished file
write.csv(canopy_chem_polished_flagged,
          '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/canopy_leaf_chemistry.csv',
          row.names = FALSE)

# Clean up foliar structure and water traits #####

leaf_struc_polished <- leaf_struc %>% rename('sample_ID' = 'sample_id', 
                                              'scientific_name_original' = 'Species', # original designation
                                             'genus_MG' = 'Genus_GM', # match the order of the flora citation
                                             'family_MG' = 'Family_GM', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO',
                                             'subregion' = 'region',
                                              'lwc' = 'fwc') %>% # match manuscript descriptions
                                               select(!c(genus_MG, NewUID))
                                              


# Remove [] from authority column
leaf_struc_polished$scientific_name_authorship_WFO <- gsub("\\[|\\]", "", leaf_struc_polished$scientific_name_authorship_WFO)

# Create year of measurement column (based on this file: /Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/ZA_Dimensions_Data/data_base/Spec_Trait_All.csv)
leaf_struc_polished <- leaf_struc_polished %>% mutate(year = case_when(
  subregion == 'baviaanskloof' ~ 2011,
  subregion == 'htr' ~ 2014,
  subregion == 'hangklip' ~ 2012,
  subregion == 'langeberg' ~ 2012,
  subregion == 'cederberg' ~ 2012,
  subregion == 'cape_point' ~ 2010
))


# Format and convert dates
lab_dates_str <- sprintf("%04d", leaf_struc_polished$date)
month <- substr(lab_dates_str, 1, 2)
day <- substr(lab_dates_str, 3, 4)
full_dates_lab <- paste(leaf_struc_polished$year,month, day, sep = "-")
leaf_struc_polished$date <- as.Date(full_dates_lab)

# Year column no longer necessary
leaf_struc_polished <- leaf_struc_polished %>% dplyr::select(!year)

# Clean the column
leaf_struc_polished <- leaf_struc_polished %>%
  mutate(
    num_leaves = na_if(num_leaves, ""), # Convert "" to NA
    num_leaves = na_if(num_leaves, "missing"), # Convert "missing" to NA
    num_leaves = str_trim(num_leaves),  
    num_leaves = case_when( 
      num_leaves == "1*" ~ "stem", # based on notes in original sheet
      TRUE ~ num_leaves))




# ---- NA out measurements corresponding to confirmed bad LMA replicates ----
# Key: sample_id + replicate. bad_lma uses lowercase "sample_id"/"replicate";
# leaf_struc_polished uses "sample_ID"/"replicate" -- joining across the naming
# mismatch below, so double check these are the same key structure/type
# (e.g. replicate stored as character in both) before trusting the join.
expand_ratio_flags <- function(df, comp1_col, comp2_col, source_name) {
  df <- df %>% distinct(sample_id, replicate, suspected_bad)
  
  unambiguous <- df %>%
    filter(suspected_bad != "ambiguous") %>%
    transmute(sample_id, replicate, raw_col = suspected_bad, source = source_name)
  
  ambiguous <- df %>%
    filter(suspected_bad == "ambiguous") %>%
    tidyr::crossing(raw_col = c(comp1_col, comp2_col)) %>%
    transmute(sample_id, replicate, raw_col, source = source_name)
  
  bind_rows(unambiguous, ambiguous)
}

removal_map <- bind_rows(
  # LMA: categorical labels need translating to actual raw column names
  {
    lma_df <- lma_outliers %>% distinct(sample_id, replicate, suspected_bad)
    unambiguous <- lma_df %>%
      filter(suspected_bad != "ambiguous") %>%
      transmute(sample_id, replicate,
                raw_col = if_else(suspected_bad == "weight", "leaf_dry_wgt_g", "leaf_area_cm2"),
                source = "lma")
    ambiguous <- lma_df %>%
      filter(suspected_bad == "ambiguous") %>%
      tidyr::crossing(raw_col = c("leaf_dry_wgt_g", "leaf_area_cm2")) %>%
      transmute(sample_id, replicate, raw_col, source = "lma")
    bind_rows(unambiguous, ambiguous)
  },
  
  expand_ratio_flags(fwc_outliers,        "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "fwc"),
  expand_ratio_flags(ldmc_outliers,       "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "ldmc"),
  expand_ratio_flags(succulence_outliers, "leaf_fresh_wgt_g", "leaf_area_cm2",  "succulence"),
  expand_ratio_flags(twig_fwc_outliers,   "twig_fresh_g",     "twig_dry_g",     "twig_fwc"),
  
  # Thickness: single-measure check, the flagged trait IS the raw column
  thickness_outliers %>%
    distinct(sample_id, replicate) %>%
    mutate(raw_col = "leaf_thickness_mm", source = "thickness")
) %>%
  rename(sample_ID = sample_id) %>%              # match leaf_struc_polished naming
  distinct(sample_ID, replicate, raw_col)         # collapse duplicate instructions from overlapping checks

scanner_cols <- c("leaf_length_cm", "avg_leaf_width_cm", "max_leaf_width_cm")

# Any row where leaf_area_cm2 was implicated also implicates the other
# scanner-derived dimensions, since all four come off the same instrument pass
area_flagged_keys <- removal_map %>%
  filter(raw_col == "leaf_area_cm2") %>%
  distinct(sample_ID, replicate)

scanner_removal_map <- area_flagged_keys %>%
  tidyr::crossing(raw_col = scanner_cols)

removal_map <- bind_rows(removal_map, scanner_removal_map) %>%
  distinct(sample_ID, replicate, raw_col)

removal_map %>% count(raw_col, sort = TRUE)

leaf_struc_polished_flagged <- leaf_struc_polished %>%
  mutate(key = paste(sample_ID, replicate))

for (col in unique(removal_map$raw_col)) {
  affected_keys <- removal_map %>%
    filter(raw_col == col) %>%
    transmute(key = paste(sample_ID, replicate)) %>%
    pull(key)
  
  leaf_struc_polished_flagged <- leaf_struc_polished_flagged %>%
    mutate(!!col := if_else(key %in% affected_keys, NA_real_, .data[[col]]))
}

leaf_struc_polished_flagged <- leaf_struc_polished_flagged %>% select(-key)

# ---- A out weight pairs where water content == 0 ----
# Checking raw weight equality directly rather than a derived ratio column,
# since the ratio column may be stale relative to the current state of the
# raw weights at this point in the pipeline
sum(leaf_struc_polished_flagged$leaf_fresh_wgt_g == leaf_struc_polished_flagged$leaf_dry_wgt_g, na.rm = TRUE)  
sum(leaf_struc_polished_flagged$twig_fresh_g == leaf_struc_polished_flagged$twig_dry_g, na.rm = TRUE)  


leaf_struc_polished_flagged <- leaf_struc_polished_flagged %>%
  mutate(
    leaf_water_dup = coalesce(leaf_fresh_wgt_g == leaf_dry_wgt_g, FALSE),
    twig_water_dup = coalesce(twig_fresh_g == twig_dry_g, FALSE),
    
    leaf_fresh_wgt_g = if_else(leaf_water_dup, NA_real_, leaf_fresh_wgt_g),
    leaf_dry_wgt_g   = if_else(leaf_water_dup, NA_real_, leaf_dry_wgt_g),
    
    twig_fresh_g = if_else(twig_water_dup, NA_real_, twig_fresh_g),
    twig_dry_g   = if_else(twig_water_dup, NA_real_, twig_dry_g)
  ) %>%
  select(-leaf_water_dup, -twig_water_dup)

sum(leaf_struc_polished_flagged$leaf_fresh_wgt_g == leaf_struc_polished_flagged$leaf_dry_wgt_g, na.rm = TRUE)  # should be 0 now
sum(leaf_struc_polished_flagged$twig_fresh_g == leaf_struc_polished_flagged$twig_dry_g, na.rm = TRUE)  # should be 0 now


# ---- Flag (not remove) leaf_struc replicates outside global plausibility
# ranges or implicated in N-LMA covariation ----
# Neither of these goes through the removal_map/NA-out treatment above:
# global_range flags come from a literature reference range rather than a
# same-dataset replicate-confidence check, and n_lma_covariation is a
# cross-dataset signal (LMA vs. field-measured %N) -- both stay visible
# for reviewer scrutiny rather than triggering removal, consistent with
# how canopy_chem_polished_flagged handles its flags.

struc_flag_sources <- list(
  global_range = struc_outliers %>%
    distinct(sample_id, replicate, trait_flagged) %>%
    transmute(sample_id, replicate,
              flag_source = paste0("global_range_", trait_flagged)),
  
  n_lma_covariation = n_lma_covariation_outliers %>%
    distinct(sample_id, replicate) %>%
    mutate(flag_source = "n_lma_covariation")
)

struc_flag_table <- bind_rows(struc_flag_sources) %>%
  rename(sample_ID = sample_id) %>%
  mutate(key = paste(sample_ID, replicate)) %>%
  group_by(key) %>%
  summarise(quality_flag = paste(sort(unique(flag_source)), collapse = "; "),
            .groups = "drop")

leaf_struc_polished_flagged <- leaf_struc_polished_flagged %>%
  mutate(key = paste(sample_ID, replicate)) %>%
  left_join(struc_flag_table, by = "key") %>%
  mutate(quality_flag = replace_na(quality_flag, "")) %>%
  select(-key)

leaf_struc_polished_flagged %>% count(quality_flag, sort = TRUE)

# Recompute derived traits from the now-NA'd raw components. Since this
# recalculation matched stored values exactly on the unflagged data (0
# mismatches), this safely propagates every raw-column removal into its
# dependent derived traits without hand-tracking which trait needs which column.
# recompute all derived traits from the now-NA'd raw columns ----
leaf_struc_polished_flagged <- leaf_struc_polished_flagged %>%
  mutate(
    lma        = leaf_dry_wgt_g / leaf_area_cm2 * 10000,
    lwc        = (leaf_fresh_wgt_g - leaf_dry_wgt_g) / leaf_dry_wgt_g,
    ldmc       = leaf_dry_wgt_g / leaf_fresh_wgt_g,
    succulence = leaf_fresh_wgt_g / leaf_area_cm2 * 10000, 
    twig_fwc   = (twig_fresh_g - twig_dry_g) / twig_dry_g,
    lwr        = leaf_length_cm / avg_leaf_width_cm   
  )

# check the duplicated weight keys
dup_twig_keys <- leaf_struc_polished %>%
  filter(twig_fresh_g == twig_dry_g) %>%
  select(sample_ID, replicate)

leaf_struc_polished_flagged %>%
  semi_join(dup_twig_keys, by = c("sample_ID", "replicate")) %>%
  select(sample_ID, replicate, twig_fresh_g, twig_dry_g, twig_fwc)

# Write out polished file
write.csv(leaf_struc_polished_flagged, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/leaf_struct_water_traits.csv',
          row.names= FALSE)


# Clean up columns for species traits table #####

spectrait_polished <- spectrait %>% rename('scientific_name_original' = 'species', # original designation
                                           'scientific_name_MG' = 'genus_species_GM',
                                             'family_MG' = 'family_GM', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO') %>%
                                            select(!lifecycle_POSA)

# Remove extra column (family designation based on Plants of South Africa)
spectrait_polished <- spectrait_polished %>% dplyr::select(!family_POSA)

# Correct authority for Brunsvigia nervosa
spectrait_polished <- spectrait_polished %>% mutate(scientific_name_authorship_WFO = case_when(
  scientific_name_WFO == 'Brunsvigia nervosa' ~ '(Poir.) Masw.',
  TRUE ~ scientific_name_WFO
))

# Remove [] from authority column
spectrait_polished$scientific_name_authorship <- gsub("\\[|\\]", "", spectrait_polished$scientific_name_authorship_WFO)

# Investigate blank scientific_name_original rows
spectrait_polished[which(spectrait_polished$scientific_name_original == ''),]

# Remove the wonky Albuca cf. namaquensis entry... it doesn't appear in the other datasets
spectrait_polished <- spectrait_polished %>% dplyr::filter(scientific_name_MG != 'Albuca cf. namaquensis')

# Convert blank entries for lifecycle_POSA to NA; column removed, now deprecated.
# unique(spectrait_polished$lifecycle_POSA)
# spectrait_polished <- spectrait_polished %>% mutate(lifecycle_POSA =  na_if(lifecycle_POSA, "")) # Convert "" to NA

# Check cases where both annual and perennial selected
subset_df <- spectrait_polished %>% filter(perennial == 1, annual == 1)

# Check evergreen and deciduous status
subset_df <- spectrait_polished %>% filter(evergreen == 0, deciduous == 0)

# Clean seasonally apparent/identifiable (if you are apparent you should be indentifiable)
spectrait_polished %>% filter(seasonally_apparent == 1, seasonally_identifiable== 0)

spectrait_polished <- spectrait_polished %>%
  mutate(seasonally_identifiable = if_else(seasonally_apparent == 1 & seasonally_identifiable == 0, 1, seasonally_identifiable))

# Clean succulent status
spectrait_polished <- spectrait_polished %>%
  mutate(succulent = if_else(stem_succulent == 1 & succulent == 0, 1, succulent),
         succulent = if_else(leaf_succulent == 1 & succulent == 0, 1, succulent),)

# Clean flower begin variable
unique(spectrait_polished$flower_begin)
table(spectrait_polished$flower_begin)
spectrait_polished[which(spectrait_polished$flower_begin == 'A'),] # A stands for all-year
spectrait_polished[which(spectrait_polished$flower_begin == 'E'),] # Almost all-year
spectrait_polished[which(spectrait_polished$flower_begin == 'H'),] # Unknown meaning
spectrait_polished[which(spectrait_polished$flower_begin == 'I'),] # unsure on meaning, MG has this as Nov.–May.
spectrait_polished[which(spectrait_polished$flower_begin == 'R'),] # entry for this is Oct.–Nov, best to change
spectrait_polished[which(spectrait_polished$flower_begin == '?'),] # unknown

spectrait_polished <- spectrait_polished %>% mutate(
                      flower_begin =na_if(flower_begin,'H'),
                      flower_begin = na_if(flower_begin,'?'),
                      flower_begin = na_if(flower_begin,''),
                      flower_begin = case_when( flower_begin == 'A' ~ 'All year',
                                           flower_begin == 'E' ~ 'Almost all year',
                                           flower_begin == 'I' ~ 'Nov',
                                           flower_begin == 'R' ~ 'Oct',
                                           TRUE ~ flower_begin))
table(spectrait_polished$flower_begin)

# Clean flower end column
unique(spectrait_polished$flower_end)
spectrait_polished[which(spectrait_polished$flower_end == '?'),]

spectrait_polished <- spectrait_polished %>% mutate(
  flower_end = na_if(flower_end,'?'),
  flower_end = na_if(flower_end,''))
unique(spectrait_polished$flower_end)

# Clean up alt flower begin column
unique(spectrait_polished$flower_begin_alt)
spectrait_polished <- spectrait_polished %>% mutate(
  flower_begin_alt = na_if(flower_begin_alt,''))
  
unique(spectrait_polished$flower_begin_alt)

# Clean up alt flower end column
unique(spectrait_polished$flower_end_alt)
spectrait_polished <- spectrait_polished %>% mutate(
  flower_end_alt = na_if(flower_end_alt,''))

# Clean up functional twig column
unique(spectrait_polished$functional_twig)

spectrait_polished <- spectrait_polished %>% mutate(
  functional_twig = na_if(functional_twig,''),
  functional_twig = case_when( functional_twig == '0' ~ 'no',
  TRUE ~ functional_twig))

# Clean up leaf type column
unique(spectrait_polished$leaf_type)
spectrait_polished <- spectrait_polished %>% mutate(
  leaf_type = case_when( leaf_type == 'Leaf' ~ 'leaf',
                         leaf_type == 'Cladode' ~ 'cladode',
                         leaf_type == 'Cladodes' ~ 'cladode',
                         leaf_type == 'Frond' ~ 'frond',
                         leaf_type == 'None' ~ 'none',
                         leaf_type == 'Phyllode' ~ 'phyllode',
                         leaf_type == 'microphylls' ~ 'microphyll',
                               TRUE ~ leaf_type))

# check no leaf and leaf type
subset_data1 <- spectrait_polished %>% filter(virtually_no_leaves == 1, leaf_type != 'none' )
write.csv(subset_data1, '/Users/henryfrye/Downloads/leaf_type_and_virtually_no_leaves.csv')

subset_data2 <- spectrait_polished %>% filter(virtually_no_leaves == 1, functional_leaf == 'leaf' )
write.csv(subset_data2, '/Users/henryfrye/Downloads/functional_leaf_and_virtually_no_leaves.csv')
# Cassytha ciliolata, Cassytha filiformis should be culm stem,
# Juncus kraussii, Soroveta ambigua ditto

 
# fix flammability where i is a typo of l
spectrait_polished <- spectrait_polished %>% mutate(flammability = case_when(
  flammability == 'i' ~ 'l',
  TRUE ~ flammability))


# Convert family MG to sentence case
spectrait_polished$family_MG <- str_to_title(spectrait_polished$family_MG)

# Remove functional twig column as this was a project specific designation
#   and not for general use
spectrait_polished <- spectrait_polished %>% dplyr::select(!functional_twig)
which(spectrait_polished  %>% duplicated() == TRUE)

# Write out polished file
write.csv(spectrait_polished, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/species_traits.csv',
          row.names= FALSE)

# Clean up vnir spectroscopy #####

vnirspec_polished <- vnirspec %>% rename('scientific_name_original' = 'finalname', # original designation
                                             'family_MG' = 'FamilyManningGoldblatt', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO',
                                             'subregion' = 'Subregion',
                                              'latitude' = 'Latitude',
                                         'longitude' = 'Longitude',
                                         'sample' = 'Sample',
                                         'date' = 'DateMonthDay') %>% # match manuscript descriptions
                                select(! NewUID) %>%
                                unite(sample_ID, date, sample, subregion, sep = "_", remove = FALSE) %>%
                                relocate(sample_ID, .before = 1)

# Remove [] from authority column
vnirspec_polished$scientific_name_authorship <- gsub("\\[|\\]", "", vnirspec_polished$scientific_name_authorship_WFO)

# No need to include the split the genus and species info or subregion abbreviation,
#   redundant information used for the EcoSis submission
vnirspec_polished <- vnirspec_polished %>% dplyr::select(!Genus) %>% 
  dplyr::select(!Species) %>%
  dplyr::select(!SubregAbbr)


# Convert family from all caps to sentence case (do this for species traits)
vnirspec_polished$family_MG <- str_to_title(vnirspec_polished$family_MG)

# Convert subregion to lower
vnirspec_polished$subregion <- str_to_lower(vnirspec_polished$subregion)

# Fix the baviaanskloof misspelling in subregion
vnirspec_polished <- vnirspec_polished %>% mutate(subregion = case_when(
  subregion == 'baviaanksloof' ~ 'baviaanskloof',
  TRUE ~ subregion
  
))


# Create year of measurement column (based on this file: /Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/ZA_Dimensions_Data/data_base/Spec_Trait_All.csv)
vnirspec_polished <- vnirspec_polished %>% mutate(year = case_when(
  subregion == 'baviaanskloof' ~ 2011,
  subregion == 'htr' ~ 2014,
  subregion == 'hangklip' ~ 2012,
  subregion == 'langeberg' ~ 2012,
  subregion == 'cederberg' ~ 2012,
  subregion == 'cape point' ~ 2010
))


# Format and convert dates
vnirspec_dates_str <- sprintf("%04d", vnirspec_polished$date)
month <- substr(vnirspec_dates_str, 1, 2)
day <- substr(vnirspec_dates_str, 3, 4)
full_dates_vnir <- paste(vnirspec_polished$year,month, day, sep = "-")
vnirspec_polished$date <- as.Date(full_dates_vnir)

# remove low signal measurements, i.e., R800 < .3
vnirspec_polished <- vnirspec_polished %>% filter(X800 > 30)

# Write out polished file
write.csv(vnirspec_polished, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/vnir_spectra.csv',
          row.names= FALSE)


# Clean up releve data #####

releve_polished <- releve %>% rename('scientific_name_original' = 'Species', # original designation
                                         'family_MG' = 'Family', # match the order of the flora citation
                                         'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                         'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
                                         'family_WFO' = 'Family_WFO',
                                         'plot' = 'Plot',
                                         'year' = 'Year',
                                         'subregion' = 'Site',
                                         'percent_cover' = 'PercCover',
                                         'abundance_class' = 'AbundClass',
                                         'plot_percent_cover' = 'PlotPercCov',
                                         'relative_percent_cover' = 'RelPercCover')
                               

# Remove family MG column, does not have a proper join and can be removed since WFO information is largely redunant
releve_polished <- releve_polished %>% select(!family_MG)

# Remove [] from authority column
releve_polished$scientific_name_authorship <- gsub("\\[|\\]", "", releve_polished$scientific_name_authorship_WFO)

# ---- Workflow3 addendum: zero percent_cover recode ----
# Context: percent_cover == 0 values are not true absences (species is 
# present in the releve, per raw field sheets) but reflect the botanist 
# omitting a cover estimate for trace/minute individuals. Reassigning to 
# 0.1, consistent with the minimum non-zero rung of the cover-abundance 
# ordinal scale used elsewhere in this survey convention (e.g., Braun-
# Blanquet-style minor-cover classes).

# 1. Isolate the affected rows for the provenance log, BEFORE any changes
zero_cover_log <- releve_polished %>%
  filter(percent_cover == 0) %>%
  mutate(
    original_percent_cover = percent_cover,
    recode_reason = "No cover estimate assigned by field botanist for trace-abundance individual; reassigned to minimum ordinal-scale value (0.1) per survey convention",
    recode_date = Sys.Date()
  )

n_recoded <- nrow(zero_cover_log)
message(glue::glue("Recoding {n_recoded} percent_cover values of 0 to 0.1"))

# 2. Apply the recode to the working dataframe
releve_polished <- releve_polished %>%
  mutate(
    percent_cover = if_else(percent_cover == 0, 0.1, percent_cover)
  )

# 3. Sanity check: confirm no zeros remain and no unintended rows changed
stopifnot(sum(releve_polished$percent_cover == 0, na.rm = TRUE) == 0)
stopifnot(nrow(zero_cover_log) == n_recoded)

# 4. Write the log out for the repo (mirrors your outlier_review_flags.csv pattern)
write_csv(zero_cover_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/zero_cover_recode_log.csv")

# check relationship between cape point abundance classes and percent cover
releve %>%
  filter(Site == 'cape_point', Year == 2010, !is.na(PercCover), !is.na(AbundClass)) %>%
  ggplot(aes(x = factor(AbundClass), y = PercCover)) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  scale_y_log10()

# now deprecated:
# Convert 1966/1996 Cape to perc cover, 1978 hangklip/ 1993 Langeberg/ Cederberg convert 
#   back to original abundance class for copmletion,
# older_subregions <- c('langeberg', 'cederberg', 'hangklip')
# htr_cover_class_year <- c('2004','2013') # the 2014 htr plots were done on percent cover scale
# old_cape_year <- c('1966', '1996')
# 
# releve_polished <- releve_polished %>% mutate(abundance_class = case_when(
#   subregion %in% older_subregions & percent_cover == .1 ~ 0,
#   subregion %in% older_subregions & percent_cover == 2.5 ~ 1,
#   subregion %in% older_subregions & percent_cover == 15 ~ 2,
#   subregion %in% older_subregions & percent_cover == 37.5 ~ 3,
#   subregion %in% older_subregions & percent_cover == 62.5 ~ 4,
#   subregion %in% older_subregions & percent_cover == 87.5 ~ 5,
#   subregion == 'htr' & year %in% htr_cover_class_year & percent_cover == .1 ~ 0,
#   subregion == 'htr' & year %in% htr_cover_class_year & percent_cover == 2.5 ~ 1,
#   subregion == 'htr' & year %in% htr_cover_class_year & percent_cover > 5 & percent_cover < 25 ~ 2,
#   subregion == 'htr' & year %in% htr_cover_class_year & percent_cover == 37.5 ~ 3,
#   subregion == 'htr' & year %in% htr_cover_class_year & percent_cover == 62.5 ~ 4,
#   subregion == 'htr' & year %in% htr_cover_class_year & percent_cover == 87.5 ~ 5,
#   TRUE ~ abundance_class
# )) %>% mutate(percent_cover = case_when(
#   subregion == 'cape_point' & year %in% old_cape_year & abundance_class == 0 ~ .1,
#   subregion == 'cape_point' & year %in% old_cape_year & abundance_class == 1 ~ 2.5,
#   subregion == 'cape_point' & year %in% old_cape_year & abundance_class == 2 ~ 15,
#   subregion == 'cape_point' & year %in% old_cape_year & abundance_class == 3 ~ 37.5,
#   subregion == 'cape_point' & year %in% old_cape_year & abundance_class == 4 ~ 62.5,
#   subregion == 'cape_point' & year %in% old_cape_year & abundance_class == 5 ~ 87.5,
#   TRUE ~ percent_cover
# ))

# ---- Cape Point 2010: remove derived (non-original) abundance_class ----
# Context: percent_cover was the genuine field-collected measure for cape_point
# 2010. abundance_class values present for this year/subregion were derived via
# conversion by a collaborator for a separate paper/analysis, then inadvertently
# incorporated during survey aggregation. These do not represent independent
# field assessment and are removed to avoid misrepresenting derived values as
# original observations.

cape_point_2010_log <- releve_polished %>%
  filter(subregion == 'cape_point', year == 2010, !is.na(abundance_class)) %>%
  mutate(
    original_abundance_class = abundance_class,
    removal_reason = "abundance_class derived via conversion for a separate collaborator analysis; not independently field-assessed. percent_cover retained as the original measure for this survey.",
    removal_date = Sys.Date()
  )

n_naed <- nrow(cape_point_2010_log)
message(glue::glue("NAing {n_naed} derived abundance_class values for cape_point 2010"))

releve_polished <- releve_polished %>%
  mutate(
    abundance_class = if_else(
      subregion == 'cape_point' & year == 2010,
      NA_real_,
      abundance_class
    )
  )

stopifnot(sum(releve_polished$subregion == 'cape_point' & releve_polished$year == 2010 & !is.na(releve_polished$abundance_class)) == 0)

write_csv(cape_point_2010_log, "cape_point_2010_abundance_class_removal_log.csv")

# ---- Add cover_method column ----
# Documents the original field-recording protocol per subregion/year, based on:
#   1. Which of percent_cover / abundance_class was populated in the raw data
#   2. Whether percent_cover values are continuous field estimates or 
#      discretized midpoints of an underlying ordinal class scale
# See reviewer-response diagnostics (site/year cross-tab, midpoint-match checks)
# for the evidence behind each classification.

older_subregions <- c('langeberg', 'cederberg', 'hangklip')

releve_polished <- releve_polished %>%
  mutate(
    cover_method = case_when(
      # Cape Point: abundance-class-only years
      subregion == 'cape_point' & year %in% c(1966, 1996) ~ "abundance_class_only",
      # Cape Point 2010: percent cover
      subregion == 'cape_point' & year == 2010 ~ "percent_cover_continuous",
      # Langeberg/Cederberg/Hangklip: percent_cover column holds 6-tier class midpoints
      subregion %in% older_subregions ~ "percent_cover_class_derived",
      # HTR 2004/2013: percent_cover holds a finer ordinal-class midpoint scale
      subregion == 'htr' & year %in% c(2004, 2013) ~ "percent_cover_class_derived_fine",
      # HTR 2014: recorded on true percent-cover scale (per original data notes)
      subregion == 'htr' & year == 2014 ~ "percent_cover_continuous",
      # Baviaanskloof: continuous field percent-cover estimates
      subregion == 'baviaanskloof' ~ "percent_cover_continuous",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(cover_method, .after = abundance_class)

# Sanity check: confirm every row got classified and counts match your
# earlier group_by(Site, Year) summary
releve_polished %>%
  count(subregion, year, cover_method) %>%
  arrange(subregion, year)

# Confirm no NAs slipped through
stopifnot(sum(is.na(releve_polished$cover_method)) == 0)

# check distribution of values over 100 for percent continuous values:
# continuous_over100 <- plot_summary %>%
#   filter(cover_method == "percent_cover_continuous")
# 
# continuous_over100 %>%
#   summarise(
#     n = n(),
#     n_over100 = sum(plot_percent_cover > 100),
#     n_100_150 = sum(plot_percent_cover > 100 & plot_percent_cover <= 150),
#     n_150_200 = sum(plot_percent_cover > 100 & plot_percent_cover > 150 & plot_percent_cover <= 200),
#     n_over200 = sum(plot_percent_cover > 200)
#   )
# 
# ggplot(continuous_over100, aes(x = plot_percent_cover)) +
#   geom_histogram(binwidth = 10, boundary = 100) +
#   geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
#   geom_vline(xintercept = 200, linetype = "dotted", color = "orange") +
#   labs(title = "plot_percent_cover distribution — percent_cover_continuous plots",
#        x = "plot_percent_cover", y = "n plots")



# Add in missing relative covers
releve_polished <- releve_polished %>%
  group_by(plot,year) %>%
  mutate(plot_percent_cover = if_else(
    is.na(plot_percent_cover),
    sum(percent_cover, na.rm = TRUE),
    plot_percent_cover
  )) %>%
  mutate(relative_percent_cover = if_else(
    is.na(relative_percent_cover),
    percent_cover/plot_percent_cover,
    relative_percent_cover
  )) %>%
  ungroup()
  
# Join in lat/long data
comm_loc_join <- comm_loc %>% unite(plot, Site, PLOT, sep = "_" )

# Remove suffixes like "_a", "_b" from plot values
releve_polished$plot_clean <- gsub("(_[a-z])$", "", releve_polished$plot)

# create a releve_ID column
releve_polished <- releve_polished %>% unite("releve_ID", plot, year, sep = "_", remove = FALSE) 

# re-calculate the total cover column since it appear incorrect
releve_polished <- releve_polished %>%
  group_by(releve_ID) %>%
  mutate(plot_percent_cover = sum(percent_cover, na.rm = TRUE)) %>%
  ungroup() 

# Then join using the cleaned column
releve_polished <- left_join(releve_polished, comm_loc_join, by = c("plot_clean" = "plot")) %>%
  select(!plot_clean) %>%
  rename('latitude' = 'Latitude',
         'longitude' = 'Longitude') %>%
  select(scientific_name_original:subregion, latitude, longitude, percent_cover:plot_percent_cover)

# Insert corrected hangklip releve locations
corrected_hangklip <- read_sf('GCFR_Traits/Spatial_Data/hangklip_releve_new.gpkg')

corrected_hangklip_coords <- corrected_hangklip %>%
  st_drop_geometry() %>%
  bind_cols(st_coordinates(corrected_hangklip) %>% as.data.frame()) %>%
  select(plot, longitude = X, latitude = Y)

releve_polished <- releve_polished %>%
  left_join(corrected_hangklip_coords, by = "plot", suffix = c("", "_new")) %>%
  mutate(
    latitude  = if_else(subregion == "hangklip" & !is.na(latitude_new), latitude_new, latitude),
    longitude = if_else(subregion == "hangklip" & !is.na(longitude_new), longitude_new, longitude)
  ) %>%
  select(-latitude_new, -longitude_new)


# check for any unmatched plots
releve_polished %>% 
  filter(subregion == "hangklip") %>% 
  distinct(plot) %>% 
  anti_join(corrected_hangklip, by = "plot")

# Write out polished file
write_csv(releve_polished, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/releve.csv')

# Create data dictionary function ####

create_data_dictionary <- function(df) {
  summary <- data.frame(
    `Column Name` = character(0),
    `Description` = character(0),
    `Variable class` = character(0),
    `Variable Range/Categories` = character(0),
    `Units` = character(0),
    `Number missing` = integer(0),
    stringsAsFactors = FALSE
  )
  
  for (col in names(df)) {
    col_data <- df[[col]]
    col_class <- class(col_data)[1]
    num_missing <- sum(is.na(col_data))
    
    if (is.numeric(col_data)) {
      if (all(is.na(col_data))) {
        var_range <- "NA - NA"
      } else {
        var_range <- paste0(min(col_data, na.rm = TRUE), " - ", max(col_data, na.rm = TRUE))
      }
    } else {
      unique_vals <- sort(unique(na.omit(as.character(col_data))))
      if (length(unique_vals) > 0) {
        var_range <- paste0(unique_vals[1], " - ", unique_vals[length(unique_vals)])
      } else {
        var_range <- "NA - NA"
      }
    }
    
    summary <- rbind(summary, data.frame(
      `Column Name` = col,
      `Description` = NA_character_,
      `Variable class` = col_class,
      `Variable Range/Categories` = var_range,
      `Units` = NA_character_,
      `Number missing` = num_missing,
      stringsAsFactors = FALSE
    ))
  }
  
  return(summary)
}


# Data dictionary for field traits (foliar chemistry and canopy ) ####
field_dict <- create_data_dictionary(canopy_chem_polished_flagged)

write.csv(field_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/field_dictionary_raw.csv',
          row.names = FALSE)

# Unique number of entries
length(unique(canopy_chem_polished_flagged$sample_ID))

# Unique species number
length(unique(canopy_chem_polished_flagged$scientific_name_original))
length(unique(canopy_chem_polished_flagged$scientific_name_WFO)) # use WFO number since a couple names were synonyms

# Species with highest and low N value
canopy_chem_polished_flagged[which(canopy_chem_polished_flagged$percent_N == min(canopy_chem_polished_flagged$percent_N, na.rm = TRUE)),]
canopy_chem_polished_flagged[which(canopy_chem_polished_flagged$percent_N == max(canopy_chem_polished_flagged$percent_N, na.rm = TRUE)),]

# species with highest and low C value
canopy_chem_polished_flagged[which(canopy_chem_polished_flagged$percent_C == min(canopy_chem_polished_flagged$percent_C, na.rm = TRUE)),]
canopy_chem_polished_flagged[which(canopy_chem_polished_flagged$percent_C == max(canopy_chem_polished_flagged$percent_C, na.rm = TRUE)),]

# Explore missing branch order
missing_order <- canopy_chem_polished_flagged[which(is.na(canopy_chem_polished_flagged$branch_order) == TRUE),]
missing_order

# Explore missing pubescence
missing_pub <- canopy_chem_polished_flagged[which(is.na(canopy_chem_polished_flagged$pubescence) == TRUE),]

table(missing_pub$subregion)
table(canopy_chem_polished_flagged$subregion)

# Explore missing chemistry
missing_N <- canopy_chem_polished_flagged[which(is.na(canopy_chem_polished_flagged$percent_N) == TRUE),]

table(missing_N$subregion)
table(missing_N$family_MG)

# Data dictionary create for lab traits (leaf structure and water) ####

lab_dict <- create_data_dictionary(leaf_struc_polished_flagged)

write.csv(lab_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/lab_dictionary_raw.csv',
          row.names = FALSE)

# Unique number of entries
length(unique(leaf_struc_polished_flagged$sample_ID))

# Highest and lowest lma species
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$lma == min(leaf_struc_polished_flagged$lma, na.rm = TRUE)),]
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$lma == max(leaf_struc_polished_flagged$lma, na.rm = TRUE)),]

# Highest and lowest lwc species
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$fwc == min(leaf_struc_polished_flagged$fwc, na.rm = TRUE)),]
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$fwc == max(leaf_struc_polished_flagged$fwc, na.rm = TRUE)),]

# Highest and lowest thickness species
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$leaf_thickness_mm == min(leaf_struc_polished_flagged$leaf_thickness_mm, na.rm = TRUE)),]
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$leaf_thickness_mm == max(leaf_struc_polished_flagged$leaf_thickness_mm, na.rm = TRUE)),]

# Highest and lowest lwr species
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$lwr == min(leaf_struc_polished_flagged$lwr, na.rm = TRUE)),]
leaf_struc_polished_flagged[which(leaf_struc_polished_flagged$lwr == max(leaf_struc_polished_flagged$lwr, na.rm = TRUE)),]

# Missingness of leaf area
missing_area <- leaf_struc_polished_flagged[which(is.na(leaf_struc_polished_flagged$leaf_area_cm2) == TRUE),]
missing_area

# Missingness of fresh weight
missing_fresh_wt <- leaf_struc_polished_flagged[which(is.na(leaf_struc_polished_flagged$leaf_fresh_wgt_g) == TRUE),]
missing_fresh_wt
table(missing_fresh_wt$subregion)

# Data dictionary for species traits ####
spectrait_dict <- create_data_dictionary(spectrait_polished)

write.csv(spectrait_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/species_trait_dictionary_raw.csv',
          row.names = FALSE)

# Which missing for lifecycle POSA
missinglife <- spectrait_polished[which(is.na(spectrait_polished$lifecycle_POSA) == TRUE),]
sort(table(missinglife$family_WFO))

# Which missing for flowering begin
missing_flower_begin <- spectrait_polished[which(is.na(spectrait_polished$flower_begin) == TRUE),]
sort(table(missing_flower_begin$family_WFO))

# Which missing for flowering end
missing_flower_end <- spectrait_polished[which(is.na(spectrait_polished$flower_end) == TRUE),]
sort(table(missing_flower_end$family_WFO))

# Which missing for leaf length
missing_length <- spectrait_polished[which(is.na(spectrait_polished$leaf_length) == TRUE),]
sort(table(missing_length$family_WFO))


# Data dictionary create for vnir spectroscopy ####
vnirspec_dict <- create_data_dictionary(vnirspec_polished)

write.csv(vnirspec_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/vnir_spec_dictionary_raw.csv',
          row.names = FALSE)


# Minimum reflectance value
min(vnirspec[,15:514])

# Maximum value
max(vnirspec[,15:514])

# Number of measurements
length(unique(vnirspec_polished$sample_ID))

# Number of species
length(unique(vnirspec_polished$scientific_name_WFO))

# What is the most measured species
sort(table(vnirspec_polished$scientific_name_WFO),decreasing = TRUE)[1:10]

# Breakdown of species replicates
table(table(vnirspec_polished$scientific_name_WFO))

# Data dictionary create for releve data ####

releve_dict <- create_data_dictionary(releve_polished)

write.csv(releve_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/releve_dictionary_raw.csv',
          row.names = FALSE)

