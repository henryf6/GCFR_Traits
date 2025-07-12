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

# Clean up columns for species traits #####

# Clean up columns for vnir spectroscopy #####

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

# Explore missing genus_GM taxa
missing_gen <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$genus_MG) == TRUE),]

# Explore missing branch order
missing_order <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$branch_order) == TRUE),]
missing_order

# Explore missing pubescence
missing_pub <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$pubescence) == TRUE),]
View(missing_pub)
table(missing_pub$subregion)
table(fieldtrait_polished_no_dupes$subregion)

# Explore missing chemistry
missing_N <- fieldtrait_polished_no_dupes[which(is.na(fieldtrait_polished_no_dupes$percent_N) == TRUE),]
View(missing_N)
table(missing_N$subregion)
table(missing_N$family_MG)

# Data dictionary create for lab traits ####

# Data dictionary create for species traits ####

# Data dictionary create for vnir spectroscopy ####

# Data dictionary create for releve data ####
