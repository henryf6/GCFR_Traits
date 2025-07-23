########################################################
# Workflow3_Data_Polish.R
#
# Purpose: Make uniform column names and order, create data dictionaries
# as summaries
#
# Date Created: July 2025
# Most recent modification: 
# Author(s): Henry Frye, Copilot
########################################################

# Load in libraries
library(tidyverse)

# Load in cleaned data from Workflow 2
fieldtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/field_trait_taxa_clean.csv')
labtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/lab_trait_taxa_clean.csv')
spectrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/species_trait_taxa_clean.csv')
vnirspec <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/vnir_taxa_clean.csv')
releve <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/releve_taxa_clean.csv')

# Clean up columns for field traits #####

fieldtrait_polished <- fieldtrait %>% rename('unique_ID' = 'NewUID', # ID system is no longer "new"
                                             'scientific_name_original' = 'Species', # original designation
                                             'genus_MG' = 'Genus_GM', # match the order of the flora citation
                                             'family_MG' = 'Family_GM', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO',
                                             'subregion' = 'region') # match manuscript descriptions
                                             
# Create year of measurement column (based on this file: /Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/ZA_Dimensions_Data/data_base/Spec_Trait_All.csv)
fieldtrait_polished <- fieldtrait_polished %>% mutate(year = case_when(
  subregion == 'baviaanskloof' ~ 2011,
  subregion == 'htr' ~ 2014,
  subregion == 'hangklip' ~ 2012,
  subregion == 'langeberg' ~ 2012,
  subregion == 'cederberg' ~ 2012,
  subregion == 'cape_point' ~ 2010
))


# Format and convert dates
field_dates_str <- sprintf("%04d", fieldtrait_polished$date)
month <- substr(field_dates_str, 1, 2)
day <- substr(field_dates_str, 3, 4)
full_dates_field <- paste(fieldtrait_polished$year,month, day, sep = "-")
fieldtrait_polished$date <- as.Date(full_dates_field)

# year column no longer necessary
fieldtrait_polished <- fieldtrait_polished %>% dplyr::select(!year)

# Fix pubescence column: change blank pubescence category to NA, and fix trailing spaces 
# Clean the column
fieldtrait_polished <- fieldtrait_polished %>%
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

# Return only the first occurring row duplicate
fieldtrait_polished_no_dupes <- fieldtrait_polished %>%
  filter(!duplicated(.))

# write out polished file
write.csv(fieldtrait_polished_no_dupes, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Field_Traits_Final.csv',
          row.names= FALSE)


# Clean up columns for lab traits #####

labtrait_polished <- labtrait %>% rename('unique_ID' = 'NewUID', # ID system is no longer "new"
                                             'scientific_name_original' = 'Species', # original designation
                                             'genus_MG' = 'Genus_GM', # match the order of the flora citation
                                             'family_MG' = 'Family_GM', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO',
                                             'subregion' = 'region') # match manuscript descriptions

# Create year of measurement column (based on this file: /Users/henryfrye/Dropbox/Intellectual_Endeavours/UConn/Research/ZA_Dimensions_Data/data_base/Spec_Trait_All.csv)
labtrait_polished <- labtrait_polished %>% mutate(year = case_when(
  subregion == 'baviaanskloof' ~ 2011,
  subregion == 'htr' ~ 2014,
  subregion == 'hangklip' ~ 2012,
  subregion == 'langeberg' ~ 2012,
  subregion == 'cederberg' ~ 2012,
  subregion == 'cape_point' ~ 2010
))


# Format and convert dates
lab_dates_str <- sprintf("%04d", labtrait_polished$date)
month <- substr(lab_dates_str, 1, 2)
day <- substr(lab_dates_str, 3, 4)
full_dates_lab <- paste(labtrait_polished$year,month, day, sep = "-")
labtrait_polished$date <- as.Date(full_dates_lab)

# year column no longer necessary
labtrait_polished <- labtrait_polished %>% dplyr::select(!year)

# Clean the column
labtrait_polished <- labtrait_polished %>%
  mutate(
    num_leaves = na_if(num_leaves, ""), # Convert "" to NA
    num_leaves = na_if(num_leaves, "missing"), # Convert "missing" to NA
    num_leaves = str_trim(num_leaves),  
    num_leaves = case_when( 
      num_leaves == "1*" ~ "stem", # based on notes in original sheet
      TRUE ~ num_leaves))

# remove obvious 400% carbon outlier and make missing
fieldtrait_polished <- fieldtrait_polished %>% mutate(percent_C = na_if(percent_C,max(fieldtrait_polished$percent_C, na.rm = TRUE)))
                                                 
# Return only the first occurring row duplicate
labtrait_polished_no_dupes <- labtrait_polished %>%
  filter(!duplicated(.))

# write out polished file
write.csv(labtrait_polished_no_dupes, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Lab_Traits_Final.csv',
          row.names= FALSE)


# Clean up columns for species traits #####

spectrait_polished <- spectrait %>% rename('scientific_name_original' = 'species', # original designation
                                           'scientific_name_MG' = 'genus_species_GM',
                                             'family_MG' = 'family_GM', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO') 

# remove extraneous column (family designation based on Plants of South Africa)
spectrait_polished <- spectrait_polished %>% dplyr::select(!family_POSA)

# investigate blank scientific_name_original rows
spectrait_polished[which(spectrait_polished$scientific_name_original == ''),]

# remove the wonky Albuca cf. namaquensis entry... it doesn't appear in the other datasets
spectrait_polished <- spectrait_polished %>% dplyr::filter(scientific_name_MG != 'Albuca cf. namaquensis')

# Convert blank entries for lifecycle_POSA to NA
unique(spectrait_polished$lifecycle_POSA)
spectrait_polished <- spectrait_polished %>% mutate(lifecycle_POSA =  na_if(lifecycle_POSA, "")) # Convert "" to NA

# Clean up flower begin
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

# clean up flower end column
unique(spectrait_polished$flower_end)
spectrait_polished[which(spectrait_polished$flower_end == '?'),]

spectrait_polished <- spectrait_polished %>% mutate(
  flower_end = na_if(flower_end,'?'),
  flower_end = na_if(flower_end,''))
unique(spectrait_polished$flower_end)

# clean up alt flower begin column
unique(spectrait_polished$flower_begin_alt)
spectrait_polished <- spectrait_polished %>% mutate(
  flower_begin_alt = na_if(flower_begin_alt,''))
  
unique(spectrait_polished$flower_begin_alt)

# clean up alt flower end column
unique(spectrait_polished$flower_end_alt)
spectrait_polished <- spectrait_polished %>% mutate(
  flower_end_alt = na_if(flower_end_alt,''))

# clean up functional twig column
unique(spectrait_polished$functional_twig)

spectrait_polished <- spectrait_polished %>% mutate(
  functional_twig = na_if(functional_twig,''),
  functional_twig = case_when( functional_twig == '0' ~ 'no',
  TRUE ~ functional_twig))

# clean up leaf type column
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

# convert family MG to sentence case
spectrait_polished$family_MG <- str_to_title(spectrait_polished$family_MG)

# remove functional twig column as this was a project specific designation
# and not for general use
spectrait_polished <- spectrait_polished %>% dplyr::select(!functional_twig)
which(spectrait_polished  %>% duplicated() == TRUE)

# write out polished file
write.csv(spectrait_polished, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Species_Traits_Final.csv',
          row.names= FALSE)

# Clean up columns for vnir spectroscopy #####


vnirspec_polished <- vnirspec %>% rename('unique_ID' = 'NewUID', # ID system is no longer "new"
                                             'scientific_name_original' = 'finalname', # original designation
                                             'family_MG' = 'FamilyManningGoldblatt', # match the order of the flora citation
                                             'scientific_name_WFO' = 'ScientificName_WFO', # match other column name format
                                             'scientific_name_authorship' = 'scientificNameAuthorship',
                                             'family_WFO' = 'Family_WFO',
                                             'subregion' = 'Subregion',
                                              'latitude' = 'Latitude',
                                         'longitude' = 'Longitude',
                                         'sample' = 'Sample',
                                         'date' = 'DateMonthDay') # match manuscript descriptions

# No need to include the split the genus and species info or subregion abbreviation, redundant information used for the EcoSis submission
vnirspec_polished <- vnirspec_polished %>% dplyr::select(!Genus) %>% 
  dplyr::select(!Species) %>%
  dplyr::select(!SubregAbbr)


# convert family from all caps to sentence case (do this for species traits)
vnirspec_polished$family_MG <- str_to_title(vnirspec_polished$family_MG)

# convert subregion to lower
vnirspec_polished$subregion <- str_to_lower(vnirspec_polished$subregion)

# fix the baviaanskloof misspeling in subregion
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

# write out polished file
write.csv(vnirspec_polished, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/VNIR_Spectra_Final.csv',
          row.names= FALSE)


# Clean up columns for releve data #####

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



# Data dictionary create for field traits ####
field_dict <- create_data_dictionary(fieldtrait_polished_no_dupes)

write.csv(field_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/field_dictionary_raw.csv',
          row.names = FALSE)

# Unique number of entries
length(unique(fieldtrait_polished_no_dupes$unique_ID))

# Unique species number
length(unique(fieldtrait_polished_no_dupes$scientific_name_original))
length(unique(fieldtrait_polished_no_dupes$scientific_name_WFO)) # use WFO number since a couple names were synonyms

# species with highest and low N value
fieldtrait_polished_no_dupes[which(fieldtrait_polished_no_dupes$percent_N == min(fieldtrait_polished_no_dupes$percent_N, na.rm = TRUE)),]
fieldtrait_polished_no_dupes[which(fieldtrait_polished_no_dupes$percent_N == max(fieldtrait_polished_no_dupes$percent_N, na.rm = TRUE)),]

# species with highest and low C value
fieldtrait_polished_no_dupes[which(fieldtrait_polished_no_dupes$percent_C == min(fieldtrait_polished_no_dupes$percent_C, na.rm = TRUE)),]
fieldtrait_polished_no_dupes[which(fieldtrait_polished_no_dupes$percent_C == max(fieldtrait_polished_no_dupes$percent_C, na.rm = TRUE)),]

# Explore missing genus_GM taxa
missing_gen <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$genus_MG) == TRUE),]

# Explore missing branch order
missing_order <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$branch_order) == TRUE),]
missing_order

# Explore missing pubescence
missing_pub <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$pubescence) == TRUE),]

table(missing_pub$subregion)
table(fieldtrait_polished_no_dupes$subregion)

# Explore missing chemistry
missing_N <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$percent_N) == TRUE),]

table(missing_N$subregion)
table(missing_N$family_MG)

# Data dictionary create for lab traits ####

# create data dictionary for lab traits
lab_dict <- create_data_dictionary(labtrait_polished_no_dupes)

write.csv(lab_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/lab_dictionary_raw.csv',
          row.names = FALSE)

# Unique number of entries
length(unique(labtrait_polished_no_dupes$unique_ID))

# Highest and lowest lma species
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$lma == min(labtrait_polished_no_dupes$lma, na.rm = TRUE)),]
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$lma == max(labtrait_polished_no_dupes$lma, na.rm = TRUE)),]

# Highest and lowest lwc species
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$fwc == min(labtrait_polished_no_dupes$fwc, na.rm = TRUE)),]
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$fwc == max(labtrait_polished_no_dupes$fwc, na.rm = TRUE)),]

# Highest and lowest thickness species
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$leaf_thickness_mm == min(labtrait_polished_no_dupes$leaf_thickness_mm, na.rm = TRUE)),]
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$leaf_thickness_mm == max(labtrait_polished_no_dupes$leaf_thickness_mm, na.rm = TRUE)),]

# Highest and lowest lwr species
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$lwr == min(labtrait_polished_no_dupes$lwr, na.rm = TRUE)),]
labtrait_polished_no_dupes[which(labtrait_polished_no_dupes$lwr == max(labtrait_polished_no_dupes$lwr, na.rm = TRUE)),]

# missingness of area
missing_area <- labtrait_polished_no_dupes[which(is.na(labtrait_polished_no_dupes$leaf_area_cm2) == TRUE),]
missing_area

missing_fresh_wt <- labtrait_polished_no_dupes[which(is.na(labtrait_polished_no_dupes$leaf_fresh_wgt_g) == TRUE),]
missing_fresh_wt
table(missing_fresh_wt$subregion)

# Data dictionary create for species traits ####
spectrait_dict <- create_data_dictionary(spectrait_polished)

write.csv(spectrait_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/species_trait_dictionary_raw.csv',
          row.names = FALSE)


# Data dictionary create for vnir spectroscopy ####
vnirspec_dict <- create_data_dictionary(vnirspec_polished)

write.csv(vnirspec_dict, 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_dictionaries/vnir_spec_dictionary_raw.csv',
          row.names = FALSE)


# Minimum reflectance value
min(vnirspec[,15:514])

# Maximum value
max(vnirspec[,15:514])

# Data dictionary create for releve data ####

