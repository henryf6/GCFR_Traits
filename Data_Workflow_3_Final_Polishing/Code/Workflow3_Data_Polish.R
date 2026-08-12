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

spectrait_polished <- spectrait %>%
  rename('scientific_name_original' = 'species', # original designation
         'scientific_name_MG' = 'genus_species_GM',
         'family_MG' = 'family_GM', # match the order of the flora citation
         'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
         'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
         'family_WFO' = 'Family_WFO') %>%
  select(!lifecycle_POSA)

# Convert family MG to title case
spectrait_polished$family_MG <- str_to_title(spectrait_polished$family_MG)

# Remove extra column (family designation based on Plants of South Africa)
spectrait_polished <- spectrait_polished %>% select(!family_POSA)

# Correct authority for Brunsvigia nervosa
spectrait_polished <- spectrait_polished %>%
  mutate(scientific_name_authorship_WFO = case_when(
    scientific_name_WFO == 'Brunsvigia nervosa' ~ '(Poir.) Masw.',
    TRUE ~ scientific_name_authorship_WFO
  ))

# Remove [] from authority column
spectrait_polished$scientific_name_authorship_WFO <-
  gsub("\\[|\\]", "", spectrait_polished$scientific_name_authorship_WFO)

# Investigate blank scientific_name_original rows
spectrait_polished[which(spectrait_polished$scientific_name_original == ''), ]

# Provenance: Albuca cf. namaquensis removal
albuca_removal_log <- spectrait_polished %>%
  filter(scientific_name_MG == 'Albuca cf. namaquensis') %>%
  mutate(removal_date = Sys.Date(),
         removal_reason = "does not appear in other datasets; treated as a spurious/unmatched entry")
write_csv(albuca_removal_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/albuca_namaquensis_removal_log.csv")

spectrait_polished <- spectrait_polished %>%
  filter(scientific_name_MG != 'Albuca cf. namaquensis')

# Convert blank entries for lifecycle_POSA to NA; column removed, now deprecated.
# unique(spectrait_polished$lifecycle_POSA)
# spectrait_polished <- spectrait_polished %>% mutate(lifecycle_POSA = na_if(lifecycle_POSA, ""))

# Check cases where both annual and perennial selected
ann_per_subset_df <- spectrait_polished %>% filter(perennial == 1, annual == 1)
# these are listed as likely polymorphic in the flora references

# Workflow: NA-recode records with 0/0 evergreen-deciduous (reviewer comment:
# "not evergreen and not deciduous" for 101 names should be NA, not 0/0)

evergreen_deciduous_na_log <- spectrait_polished %>%
  filter(evergreen == 0, deciduous == 0) %>%
  select(scientific_name_original, scientific_name_MG, scientific_name_WFO,
         evergreen, deciduous) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = "both evergreen and deciduous = 0; treated as missing data per reviewer comment")

nrow(evergreen_deciduous_na_log)  # sanity check against the 101 names the reviewer cites

write_csv(evergreen_deciduous_na_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/evergreen_deciduous_na_recode_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(
    evergreen  = if_else(evergreen == 0 & deciduous == 0, NA_real_, evergreen),
    deciduous  = if_else(evergreen == 0 & deciduous == 0, NA_real_, deciduous)
  )

# Sanity check: any records where evergreen AND deciduous are both 1?
# (the opposite data-quality issue — can't logically be both)

evergreen_deciduous_conflict_check <- spectrait_polished %>%
  filter(evergreen == 1, deciduous == 1) %>%
  select(scientific_name_original, scientific_name_MG, scientific_name_WFO,
         evergreen, deciduous)

nrow(evergreen_deciduous_conflict_check)
evergreen_deciduous_conflict_check

# Workflow: recode deciduous 1 -> 0 for records with evergreen == 1 & deciduous == 1
# Rationale: re-checked source flora entries for all 21 affected species; no textual
# support for deciduousness found in any case. For 8/21 (Elegia spp. + Euphorbia hamata),
# pattern is consistent with a dioecious/deciduous text-mining mix-up (all dioecious == 1).
# Remaining 13 lack a clear mechanism but show the same absence of flora support.

evergreen_deciduous_conflict_log <- spectrait_polished %>%
  filter(evergreen == 1, deciduous == 1) %>%
  select(scientific_name_original, scientific_name_MG, scientific_name_WFO,
         evergreen, deciduous, dioecious, monoecious) %>%
  mutate(
    recode_date = Sys.Date(),
    recode_reason = "evergreen & deciduous both = 1; re-checked flora, no support for deciduous; recoded deciduous to 0",
    likely_mechanism = if_else(dioecious == 1, "dioecious/deciduous text-mining mix-up (unconfirmed)", "unknown")
  )

nrow(evergreen_deciduous_conflict_log)  # should be 21

write_csv(evergreen_deciduous_conflict_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/evergreen_deciduous_conflict_recode_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(
    deciduous = if_else(evergreen == 1 & deciduous == 1, 0, deciduous)
  )

# Workflow: recode lvs_intermediate = 2 -> 1 for Carpha glomerata
# Single erroneous value (n=1) found via reviewer comment; hand-checked against
# flora entry, confirmed leaf shape is intermediate (should be binary flag = 1)

lvs_intermediate_stray_value_log <- spectrait_polished %>%
  filter(lvs_intermediate == 2) %>%
  select(scientific_name_original, scientific_name_MG, scientific_name_WFO,
         leaf_type, lvs_linear, lvs_intermediate, lvs_oval, lvs_compound,
         lvs_lobed, lvs_dissected) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = "lvs_intermediate erroneously coded as 2; hand-checked against flora, confirmed intermediate shape; recoded to 1")

nrow(lvs_intermediate_stray_value_log)  # should be 1

write_csv(lvs_intermediate_stray_value_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/lvs_intermediate_stray_value_recode_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(lvs_intermediate = if_else(lvs_intermediate == 2, 1, lvs_intermediate))

# Workflow: rename lvs_intermediate -> lvs_shape_other
# Reviewer comment: "intermediate" implies a discrete botanical shape class, but this
# column is a residual/catch-all bin for shapes not classified as linear or oval during
# text-mining (e.g. lanceolate, ovate, elliptic, oblanceolate, oblong, and combinations
# thereof). Renamed for clarity.

spectrait_polished <- spectrait_polished %>%
  rename(lvs_shape_other = lvs_intermediate)

table(spectrait_polished$lvs_shape_other)  # confirm clean binary now

# Provenance: seasonally_apparent / seasonally_identifiable harmonization
seasonal_harmonize_log <- spectrait_polished %>%
  filter(seasonally_apparent == 1, seasonally_identifiable == 0) %>%
  select(scientific_name_original, scientific_name_MG, seasonally_apparent, seasonally_identifiable) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = "seasonally_apparent = 1 implies seasonally_identifiable should also = 1 (species with a dormant season are necessarily unidentifiable during that period); recoded 0 -> 1")
write_csv(seasonal_harmonize_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/seasonal_apparent_identifiable_harmonize_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(seasonally_identifiable = if_else(seasonally_apparent == 1 & seasonally_identifiable == 0, 1, seasonally_identifiable))

# Provenance: succulent harmonization from stem_succulent / leaf_succulent
succulent_harmonize_log <- spectrait_polished %>%
  filter((stem_succulent == 1 & succulent == 0) | (leaf_succulent == 1 & succulent == 0)) %>%
  select(scientific_name_original, scientific_name_MG, succulent, leaf_succulent, stem_succulent) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = "leaf_succulent or stem_succulent = 1 implies succulent should also = 1; recoded 0 -> 1")
write_csv(succulent_harmonize_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/succulent_harmonize_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(succulent = if_else(stem_succulent == 1 & succulent == 0, 1, succulent),
         succulent = if_else(leaf_succulent == 1 & succulent == 0, 1, succulent))

# Clean flower begin variable - inspection
unique(spectrait_polished$flower_begin)
table(spectrait_polished$flower_begin)
spectrait_polished[which(spectrait_polished$flower_begin == 'A'), ] # A stands for all-year
spectrait_polished[which(spectrait_polished$flower_begin == 'E'), ] # Almost all-year
spectrait_polished[which(spectrait_polished$flower_begin == 'H'), ] # Unknown meaning
spectrait_polished[which(spectrait_polished$flower_begin == 'I'), ] # unsure on meaning, MG has this as Nov.-May.
spectrait_polished[which(spectrait_polished$flower_begin == 'R'), ] # entry for this is Oct.-Nov, best to change
spectrait_polished[which(spectrait_polished$flower_begin == '?'), ] # unknown

# Provenance: flower_begin recoding
flower_begin_recode_log <- spectrait_polished %>%
  filter(flower_begin %in% c('A', 'E', 'H', 'I', 'R', '?', '')) %>%
  select(scientific_name_original, scientific_name_MG, flower_begin) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = case_when(
           flower_begin == 'A' ~ "'A' recoded to 'All year'",
           flower_begin == 'E' ~ "'E' recoded to 'Almost all year'",
           flower_begin == 'I' ~ "'I' recoded to 'Nov' per G&M flora entry (Nov.-May range)",
           flower_begin == 'R' ~ "'R' recoded to 'Oct'; flora entry gives Oct.-Nov. range, used the more expansive (earlier) bound",
           flower_begin %in% c('H', '?', '') ~ "code meaning could not be determined; recoded to NA",
           TRUE ~ NA_character_))
write_csv(flower_begin_recode_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/flower_begin_recode_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(
    flower_begin = na_if(flower_begin, 'H'),
    flower_begin = na_if(flower_begin, '?'),
    flower_begin = na_if(flower_begin, ''),
    flower_begin = case_when(
      flower_begin == 'A' ~ 'All year',
      flower_begin == 'E' ~ 'Almost all year',
      flower_begin == 'I' ~ 'Nov',
      flower_begin == 'R' ~ 'Oct',
      TRUE ~ flower_begin))

table(spectrait_polished$flower_begin)

# Clean flower end column
unique(spectrait_polished$flower_end)
spectrait_polished[which(spectrait_polished$flower_end == '?'), ]

spectrait_polished <- spectrait_polished %>%
  mutate(flower_end = na_if(flower_end, '?'),
         flower_end = na_if(flower_end, ''))
unique(spectrait_polished$flower_end)

# Clean up alt flower begin column
unique(spectrait_polished$flower_begin_alt)
spectrait_polished <- spectrait_polished %>%
  mutate(flower_begin_alt = na_if(flower_begin_alt, ''))
unique(spectrait_polished$flower_begin_alt)

# Clean up alt flower end column
unique(spectrait_polished$flower_end_alt)
spectrait_polished <- spectrait_polished %>%
  mutate(flower_end_alt = na_if(flower_end_alt, ''))

# Clean up leaf type column
unique(spectrait_polished$leaf_type)
spectrait_polished <- spectrait_polished %>%
  mutate(leaf_type = case_when(
    leaf_type == 'Leaf' ~ 'leaf',
    leaf_type == 'Cladode' ~ 'cladode',
    leaf_type == 'Cladodes' ~ 'cladode',
    leaf_type == 'Frond' ~ 'frond',
    leaf_type == 'None' ~ 'none',
    leaf_type == 'Phyllode' ~ 'phyllode',
    leaf_type == 'microphylls' ~ 'microphyll',
    TRUE ~ leaf_type))

# Fix 1: virtually_no_leaves 0 -> 1 where functional_leaf indicates leafless/culm-based photosynthesis
vnl_from_functional_leaf_log <- spectrait_polished %>%
  filter(functional_leaf %in% c("culm_stem", "none"), virtually_no_leaves == 0) %>%
  select(scientific_name_original, family_MG, leaf_type, functional_leaf, virtually_no_leaves) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = "functional_leaf indicates culm/stem-based photosynthesis; virtually_no_leaves recoded 0 -> 1 to match")

nrow(vnl_from_functional_leaf_log)  # should be 9
write_csv(vnl_from_functional_leaf_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/virtually_no_leaves_from_functional_leaf_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(virtually_no_leaves = if_else(functional_leaf %in% c("culm_stem", "none") & virtually_no_leaves == 0,
                                       1, virtually_no_leaves))

# Fix 2: functional_leaf recode for hand-checked reverse conflicts, and virtually_no_leaves
# recode for Conophytum minusculum (opposite direction: genus-level check within Aizoaceae
# supports virtually_no_leaves = 0, not a functional_leaf correction)
reverse_conflict_hand_check_log <- spectrait_polished %>%
  filter(scientific_name_original %in% c("Cassytha ciliolata", "Cassytha filiformis",
                                         "Cuscuta africana", "Juncus kraussii",
                                         "Soroveta ambigua", "Indigofera ionii",
                                         "Conophytum minusculum")) %>%
  select(scientific_name_original, family_MG, leaf_type, functional_leaf, virtually_no_leaves) %>%
  mutate(recode_date = Sys.Date(),
         field_recoded = case_when(
           scientific_name_original == "Conophytum minusculum" ~ "virtually_no_leaves",
           TRUE ~ "functional_leaf"),
         recode_reason = case_when(
           scientific_name_original == "Indigofera ionii" ~
             "functional_leaf 'leaf' -> 'mixed'; matches worked example in original 2014 documentation (leaves present only on young growth)",
           scientific_name_original == "Conophytum minusculum" ~
             "virtually_no_leaves 1 -> 0; genus is highly leaf-succulent (fused leaf pairs), and all other Aizoaceae in dataset coded virtually_no_leaves = 0; treated as consistent leaf-bearing morphology rather than leafless",
           TRUE ~ "functional_leaf recoded to 'culm_stem'; hand-checked against flora description / growth form"))

write_csv(reverse_conflict_hand_check_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/functional_leaf_virtually_no_leaves_hand_check_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(
    functional_leaf = case_when(
      scientific_name_original == "Indigofera ionii" ~ "mixed",
      scientific_name_original %in% c("Cassytha ciliolata", "Cassytha filiformis",
                                      "Cuscuta africana", "Juncus kraussii",
                                      "Soroveta ambigua") ~ "culm_stem",
      TRUE ~ functional_leaf),
    virtually_no_leaves = if_else(scientific_name_original == "Conophytum minusculum",
                                  0, virtually_no_leaves)
  )

# sanity check: should now return 0 rows both directions
spectrait_polished %>% filter(functional_leaf %in% c("culm_stem", "none"), virtually_no_leaves == 0) %>% nrow()
spectrait_polished %>% filter(functional_leaf == "leaf", virtually_no_leaves == 1) %>% nrow()

# ---- Retire leaf_type; replace with reliable structural binary flags ----
# Rationale: comparison of the 2014 dated documentation (1,816 species; 1,114 "Leaf",
# 671 "No Data") against the source CSVs feeding this pipeline shows leaf_type's "leaf"
# value grew from 1,114 to 1,776/1,816 records with no corresponding code change -
# consistent with an undocumented manual backfill (likely undergraduate RA work per
# 2014 notes) that defaulted missing classifications to "leaf" rather than leaving them
# NA. This cannot be distinguished from genuine keyword-confirmed "leaf" records in the
# current data, so the "leaf" category is treated as unreliable. Frond/cladode/phyllode/
# microphyll values were never subject to this default (they required a positive keyword
# match) and are retained as explicit binary flags. functional_leaf (independently
# compiled, field-informed) remains the authoritative source for leaf-presence status.

leaf_type_removal_log <- spectrait_polished %>%
  select(scientific_name_original, scientific_name_MG, scientific_name_WFO,
         leaf_type, functional_leaf, virtually_no_leaves) %>%
  mutate(archive_date = Sys.Date(),
         removal_reason = "leaf_type 'leaf' value found to reflect an undocumented default fill, not a verified classification; column retired in favor of structural binary flags (frond/cladode/phyllode/microphyll) plus functional_leaf")

write_csv(leaf_type_removal_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/leaf_type_full_archive_and_removal_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(
    lvs_frond      = if_else(leaf_type == "frond", 1, 0),
    lvs_cladode    = if_else(leaf_type == "cladode", 1, 0),
    lvs_phyllode   = if_else(leaf_type == "phyllode", 1, 0),
    lvs_microphyll = if_else(leaf_type == "microphyll", 1, 0)
  )

# sanity check: flag counts should match original keyword-confirmed leaf_type counts
# (frond=19, cladode=18, phyllode=2, microphyll=1, per earlier table(leaf_type))
colSums(spectrait_polished[c("lvs_frond", "lvs_cladode", "lvs_phyllode", "lvs_microphyll")])

spectrait_polished <- spectrait_polished %>% select(!leaf_type)

# Provenance: flammability 'i' typo fix
flammability_typo_log <- spectrait_polished %>%
  filter(flammability == 'i') %>%
  select(scientific_name_original, scientific_name_MG, flammability) %>%
  mutate(recode_date = Sys.Date(),
         recode_reason = "'i' is a typo for 'l' (low flammability); recoded")
write_csv(flammability_typo_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/flammability_typo_recode_log.csv")

spectrait_polished <- spectrait_polished %>%
  mutate(flammability = case_when(
    flammability == 'i' ~ 'l',
    TRUE ~ flammability))

# Provenance: functional_twig column removal
functional_twig_removal_log <- spectrait_polished %>%
  select(scientific_name_original, scientific_name_MG, functional_twig) %>%
  mutate(removal_date = Sys.Date(),
         removal_reason = "functional_twig was a Dimensions-project-specific designation per original 2014 documentation, not intended for general use; column removed")
write_csv(functional_twig_removal_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Provenance/functional_twig_removal_log.csv")

spectrait_polished <- spectrait_polished %>% select(!functional_twig)

# Duplicate check (inspection only)
which(spectrait_polished %>% duplicated() == TRUE)

# Write out polished file
write_csv(spectrait_polished,
          'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/species_traits.csv')

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

# ==== Clean up releve data ====

releve_polished <- releve %>% rename(
  'scientific_name_original' = 'Species',
  'scientific_name_WFO' = 'ScientificName_WFO',
  'scientific_name_authorship_WFO' = 'scientificNameAuthorship',
  'family_WFO' = 'Family_WFO',
  'subregion' = 'site',
  'abundance_class' = 'abund_class',
  'plot_percent_cover' = 'PlotPercCov',
  'relative_percent_cover' = 'RelPercCover'
) 

releve_polished$scientific_name_authorship_WFO <- gsub("\\[|\\]", "", releve_polished$scientific_name_authorship_WFO)

# ---- Add cover_method column ----
# Documents original field-recording protocol per subregion/year.
older_subregions <- c('langeberg', 'cederberg', 'hangklip')

releve_polished <- releve_polished %>%
  mutate(
    cover_method = case_when(
      subregion == 'cape_point' & year %in% c(1966, 1996) ~ "condensed_Acocks_abundance_class_only",
      subregion == 'cape_point' & year == 2010 ~ "percent_cover_continuous",
      subregion %in% older_subregions ~ "percent_cover_class_derived_from_Braun_Blanquet",
      subregion == 'htr' & year %in% c(2004, 2013) ~ "percent_cover_class_from_Braun_Blanquet_Werger",
      subregion == 'htr' & year == 2014 ~ "percent_cover_continuous",
      subregion == 'baviaanskloof' ~ "percent_cover_continuous",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(cover_method, .after = abundance_class)

stopifnot(sum(is.na(releve_polished$cover_method)) == 0)


# ---- Zero percent_cover recode ----
# percent_cover == 0 is not a true absence -- reflects the botanist omitting 
# a cover estimate for trace/minute individuals. Reassign to 0.1, the minimum 
# non-zero rung of the cover-abundance ordinal scale.

zero_cover_log <- releve_polished %>%
  filter(percent_cover == 0) %>%
  mutate(
    original_percent_cover = percent_cover,
    recode_reason = "No cover estimate assigned by field botanist for trace-abundance individual; reassigned to minimum ordinal-scale value (0.1) per survey convention",
    recode_date = Sys.Date()
  )

message(glue::glue("Recoding {nrow(zero_cover_log)} percent_cover values of 0 to 0.1"))

releve_polished <- releve_polished %>%
  mutate(percent_cover = if_else(percent_cover == 0, 0.1, percent_cover))

stopifnot(sum(releve_polished$percent_cover == 0, na.rm = TRUE) == 0)

write_csv(zero_cover_log, "GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/zero_cover_recode_log.csv")


# ---- Cape Point: abundance_class is genuine, independently field-collected data ----
# Both percent_cover and abundance_class were recorded per-entry on the original 
# 1966/1996 data sheets, using a Acocks-derived individual-count category 
# scale (1=1-4 individuals ... 5=>100 individuals; see Util_Abundance_Class2Count_CP.R 
# / Util_Abundance_Count2Class_CP.R). This is NOT a Braun-Blanquet areal-cover class 
# scale, and is retained as-is rather than converted to/from percent_cover in either 
# direction:
#   - 2010: percent_cover and abundance_class both genuinely field-measured; both kept.
#   - 1966/1996: only abundance_class was recorded (no percent_cover); percent_cover 
#     remains NA, since count-based abundance has no valid conversion to areal cover.

# (no code needed here -- releve_polished already carries both columns through 
# unmodified from the upstream join; this comment documents the decision)

# ---- HTR plot size metadata ----
# 20x20m plots (mostly Tanqua Karoo, per HVDM email 2014-05-01) vs standard 
# 10x10m. Verified: no plot-number collision with Renosterveld or Akkerendam 
# datasets, so safe to match on bare plot number for non-_ak/_nieu HTR plots.
# Other subregions' historical forest-plot sizing is not documented in any 
# surviving source and is left NA.
plots_20x20 <- c(53, 110, 111, 112, 114, 115, 116, 118, 121, 173, 244, 245, 385, 386, 387)

releve_polished <- releve_polished %>%
  mutate(
    plot_size = case_when(
      subregion == "htr" & !str_detect(plot, "_ak$|_nieu$") & 
        str_extract(plot, "\\d+") %in% as.character(plots_20x20) ~ "20x20m",
      subregion == "htr" ~ "10x10m",
      TRUE ~ NA_character_
    )
  )

# ---- Single recalculation of plot totals (percent_cover column now complete) ----
releve_polished <- releve_polished %>%
  group_by(plot, year) %>%
  mutate(
    plot_percent_cover = if (all(is.na(percent_cover))) NA_real_ else sum(percent_cover, na.rm = TRUE),
    relative_percent_cover = percent_cover / plot_percent_cover
  ) %>%
  ungroup()

# ---- Join lat/long, plot ID, hangklip correction ----
comm_loc_join <- comm_loc %>% unite(plot, Site, PLOT, sep = "_")

releve_polished$plot_clean <- gsub("(_[a-z])$", "", releve_polished$plot)

releve_polished <- releve_polished %>% unite("releve_ID", plot, year, sep = "_", remove = FALSE)

releve_polished <- left_join(releve_polished, comm_loc_join, by = c("plot_clean" = "plot")) %>%
  select(!plot_clean) %>%
  rename('latitude' = 'Latitude', 'longitude' = 'Longitude') %>%
  select(scientific_name_original:subregion, latitude, longitude, 
         percent_cover:relative_percent_cover,
         plot_size, releve_ID)
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

releve_polished %>% filter(subregion == "hangklip") %>% distinct(plot) %>% anti_join(corrected_hangklip, by = "plot")

write_csv(releve_polished, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/releve.csv')


# check over 100 distribution
plot_summary <- releve_polished %>%
  distinct(plot, year, subregion, cover_method, plot_percent_cover)

# overall >100 rate, and whether cape_point 1966/1996 drops out entirely as expected
plot_summary %>%
  group_by(cover_method) %>%
  summarise(
    n_plots = n(),
    n_over100 = sum(plot_percent_cover > 100, na.rm = TRUE),
    pct_over100 = round(100 * n_over100 / n_plots, 1),
    n_na = sum(is.na(plot_percent_cover)),
    max_cover = max(plot_percent_cover, na.rm = TRUE)
  ) %>%
  arrange(desc(pct_over100))

# Cape Point 2010 specifically, for the response's ~33%/100-150 claim
plot_summary %>% filter(subregion == "cape_point", year == 2010) %>%
  summarise(n = n(), n_over100 = sum(plot_percent_cover > 100), pct = round(100*n_over100/n, 1))

# confirm the mismatch issue is resolved dataset-wide, not just HTR/Langeberg
releve_polished %>%
  group_by(plot, year, subregion) %>%
  summarise(summed = sum(percent_cover, na.rm = TRUE), reported = first(plot_percent_cover), .groups = "drop") %>%
  mutate(diff = summed - reported) %>%
  filter(abs(diff) > 0.01) %>%
  nrow()

# overall 100 computation
plot_summary %>%
  summarise(
    n_plots = n(),
    n_with_cover = sum(!is.na(plot_percent_cover)),
    n_over100 = sum(plot_percent_cover > 100, na.rm = TRUE),
    pct_over100_of_all = round(100 * n_over100 / n_plots, 1),
    pct_over100_of_assessed = round(100 * n_over100 / n_with_cover, 1)
  )

continuous_over100 <- plot_summary %>%
  filter(cover_method == "percent_cover_continuous")

continuous_over100 %>%
  summarise(
    n = n(),
    n_over100 = sum(plot_percent_cover > 100),
    n_100_150 = sum(plot_percent_cover > 100 & plot_percent_cover <= 150),
    n_150_200 = sum(plot_percent_cover > 100 & plot_percent_cover > 150 & plot_percent_cover <= 200),
    n_over200 = sum(plot_percent_cover > 200)
  )

ggplot(continuous_over100, aes(x = plot_percent_cover)) +
  geom_histogram(binwidth = 10, boundary = 100) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 200, linetype = "dotted", color = "orange") +
  labs(title = "plot_percent_cover distribution — percent_cover_continuous plots",
       x = "plot_percent_cover", y = "n plots")

# note that the 2011 a b plot got merged in the calculation...
continuous_over100 %>%
  group_by(subregion, year) %>%
  summarise(
    n = n(),
    n_over100 = sum(plot_percent_cover > 100),
    pct_over100 = round(100 * n_over100 / n, 1),
    .groups = "drop"
  )

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

