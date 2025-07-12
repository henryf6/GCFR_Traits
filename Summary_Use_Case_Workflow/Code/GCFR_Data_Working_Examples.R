########################################################
# GCFR_Data_Working_Examples.R
#
# Purpose: This script demonstrates some examples of
# of using the trait and spectroscopy data sets from the 
# Greater Cape Floristic Region
#
# Date Created: February 2025
# Most recent modification: 
# Author(s): Henry Frye
########################################################

# Load libraries
library(tidyverse)


# Read in data
labtrait <- read.csv('GCFR_Traits/Summary_Use_Case_Workflows/Cleaned_data/lab_trait_taxa_clean.csv')
fieldtrait <- read.csv('GCFR_Traits/Summary_Use_Case_Workflows/Cleaned_data/field_trait_taxa_clean.csv')
vnirspec <- read.csv('GCFR_Traits/Summary_Use_Case_Workflows/Cleaned_data/vnir_taxa_clean.csv')
spectrait <- read.csv('GCFR_Traits/Summary_Use_Case_Workflows/Cleaned_data/species_trait_taxa_clean.csv')

#### Describe the number of structural leaf and twig traits measured ####
colnames(labtrait)

labflat <- purrr::flatten_dbl(labtrait[,16:29])
missinglab <- which(is.na(labflat) == TRUE)

labtraitobs <- labflat[-missinglab]

# include pubescence category from field traits
pub_complete <- fieldtrait$pubescence %>% na.omit()

# number of measured and derived structural
print(length(labtraitobs) + length(pub_complete))

# number of unique species from lab traits
length(unique(labtrait$ScientificName_WFO))

# number of unique families from lab traits
length(unique(labtrait$Family_WFO))

# number of average replicates and s.d./range
lab_count <- labtrait %>% group_by(ScientificName_WFO, region) %>%
  summarise(SampleCounts = n()) %>% ungroup()

# check variation by region
lab_count %>% group_by(region)   %>% summarise(mean = mean(SampleCounts))

# overall summaries
median(lab_count$SampleCounts)
mean(lab_count$SampleCounts)
sd(lab_count$SampleCounts)
range(lab_count$SampleCounts)

#### Describe the number of canopy structural traits measured ####
colnames(fieldtrait)

fieldcanopyflat <- purrr::flatten_dbl(fieldtrait[,11:15])
missingcanopyflat <- which(is.na(fieldcanopyflat) == TRUE)

fieldcanopytraitobs <- fieldcanopyflat[-missingcanopyflat]

print(length(fieldcanopytraitobs))
length(labtraitobs)


# number of average replicates and s.d./rangef
field_count <- fieldtrait %>% group_by(finalname, region) %>%
  summarise(SampleCounts = n()) %>% ungroup()

# check variation by region
field_count %>% group_by(region)   %>% summarise(mean = mean(SampleCounts))

# overall summaries
median(field_count$SampleCounts)
mean(field_count$SampleCounts)
sd(field_count$SampleCounts)
range(field_count$SampleCounts)

#### Describe the number of elemental/isotope traits measured ####
fieldelemflat <- purrr::flatten_dbl(fieldtrait[,17:21])
missingelemflat <- which(is.na(fieldelemflat) == TRUE)

fieldelemtraitobs <- fieldelemflat[-missingelemflat]

print(length(fieldelemtraitobs))

# number of species
length(unique(fieldtrait$finalname))

# number of families
length(unique(fieldtrait$family_GM))


#### Describe the number of spectral measurements measured ####
# number of measurements
dim(vnirspec)[1]

# number of unique species
length(unique(vnirspec$finalname))

# number of unique families 
length(unique(vnirspec$FamilyManningGoldblatt))


# number of average replicates and s.d./rangef
spec_count <- vnirspec %>% group_by(finalname, Subregion) %>%
  summarise(SampleCounts = n()) %>% ungroup()

# check variation by region
spec_count %>% group_by(Subregion)   %>% summarise(mean = mean(SampleCounts))

# overall summaries
median(spec_count$SampleCounts)
mean(spec_count$SampleCounts)
sd(spec_count$SampleCounts)
range(spec_count$SampleCounts)

spec_count_rep <- vnirspec %>% group_by(NewUID, Subregion) %>%
  summarise(SampleCounts = n()) %>% ungroup()

mean(spec_count_rep$SampleCounts)

#### Describe the species-level trait dataset ####

length(unique(spectrait$species))



#### Check the number of taxa with only genus level ID's. ####
fieldtrait %>% dplyr::filter(str_detect(finalname, "spp"))

#### Descriptions for Data Records section ####
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



#### Descriptions of lab traits ####

# how many unique species?
length(unique(labtrait$ScientificName_WFO))

colnames(labtrait)
lab_dict <- create_data_dictionary(labtrait)


write.csv(lab_dict, file = 'GCFR_Traits/Summary_Use_Case_Workflows/Data_Outputs/lab_dictionary_raw.csv', row.names= FALSE)

# check the species differences with the manning and goldblatt text
gm_taxa <- read.csv(file = '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/Data/GMTaxonomy.csv')
sp_id_misalign <- mattlab %>% dplyr::filter(! species %in% gm_taxa$Taxon)

# how many taxa are off
length(unique((sp_id_misalign$species)))

# write out taxa list for Jasper to look through
iffy_spp <- unique(sp_id_misalign$species)

write.csv(iffy_spp, file = 'Data/Dimensions_Iffy_Spp.csv', row.names= FALSE)
       