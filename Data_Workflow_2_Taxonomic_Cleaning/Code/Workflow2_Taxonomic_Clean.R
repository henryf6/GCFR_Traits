########################################################
# Workflow2_Taxonomic_changes.R
#
# Purpose: This script cleans up taxa that do not align with 
# the Goldblatt and Manning CFR 2012 taxonomy
#
# Date Created: February 2025
# Most recent modification: 
# Author(s): Henry Frye, Copilot
########################################################

# Load libraries
library(tidyverse)
library(WorldFlora)
library(taxize)
library(arrow)

# Read in data
labtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/LabTraitDataERT.csv')
fieldtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/FieldTraitDataERT.csv')
vnirspec <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/GCFRSpectralLibraryEcoSisV3.csv')
spectrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/speciesXtraits.csv')
releve <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/Releve_All.csv')

# Check lab traits ####

# check the species differences with the manning and goldblatt text
gm_taxa <- read.csv(file = 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/GMTaxonomy.csv')
sp_id_misalign_lab <- labtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)

# how many taxa are off
length(unique((sp_id_misalign_lab$finalname)))

# Remove sp, sp. or species from final name designations for the sake consistency
labtrait$finalname <- ifelse(
  grepl("\\bsp\\.?\\b|\\bsp\\s*\\d+\\b|\\bspecies\\b", labtrait$finalname, ignore.case = TRUE) &
    !grepl("\\bsubsp\\b|\\bvar\\b|\\bcf\\b", labtrait$finalname, ignore.case = TRUE),
  gsub("\\s+(sp\\.?\\s*\\d*|species).*", "", labtrait$finalname, ignore.case = TRUE),
  labtrait$finalname
)

#re-run and view mismatches
sp_id_misalign_lab <- labtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)

# Remove form C, part of this taxa Lotononis falcata (form C)
labtrait <- labtrait %>% mutate(finalname = case_when(
  finalname == 'Lotononis falcata (form C)' ~ 'Lotononis falcata',
  TRUE ~ finalname
))

# Check field traits first ####
sp_id_misalign_field <- fieldtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname)))

# Remove sp, sp. or species from final name designations for the sake consistency
fieldtrait$finalname <- ifelse(
  grepl("\\bsp\\.?\\b|\\bsp\\s*\\d+\\b|\\bspecies\\b", fieldtrait$finalname, ignore.case = TRUE) &
    !grepl("\\bsubsp\\b|\\bvar\\b|\\bcf\\b", fieldtrait$finalname, ignore.case = TRUE),
  gsub("\\s+(sp\\.?\\s*\\d*|species).*", "", fieldtrait$finalname, ignore.case = TRUE),
  fieldtrait$finalname
)

# Remove form C, part of this taxa Lotononis falcata (form C)
fieldtrait <- fieldtrait %>% mutate(finalname = case_when(
  finalname == 'Lotononis falcata (form C)' ~ 'Lotononis falcata',
  TRUE ~ finalname
))


sp_id_misalign_field <- fieldtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname)))

# Check species-level traits ####
sp_id_misalign_field <- spectrait %>% dplyr::filter(! genus_species_GM %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname))) # 0, which makes sense since the species are based on GM

# Check spectral data
sp_id_misalign_vnir <- vnirspec %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname))) # great 0 as well!

# WFO taxa match for lab traits ####
wfo_taxonomy <- read_tsv_arrow("/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/ArboretumPhyloPheno/LngArbCode/PhylogeneticCode/classification_v.2023.12.csv")

# Check names of arboretum against World Flora
NameCheck <- WFO.match(spec.data = labtrait, WFO.data= wfo_taxonomy, spec.name = "finalname",
                       Fuzzy.min = TRUE)
Name_single <- WFO.one(NameCheck)


NameCheck_sel <- Name_single %>% dplyr::select(NewUID ,finalname.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple <- NameCheck_sel %>% distinct()

labtrait_wfo <- left_join(labtrait, NameCheck_mini_simple, by = c('finalname' = 'finalname.ORIG'))

labtrait_taxa_cleaned <- labtrait_wfo %>% rename('ScientificName_WFO' = scientificName, 'Species' = finalname, 'Family_WFO' = family,
                                         'NewUID' = NewUID.x) %>% 
  dplyr::select(NewUID, Species:Family_GM, ScientificName_WFO, scientificNameAuthorship, Family_WFO, latitude:twig_fwc)


write.csv(labtrait_taxa_cleaned, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/lab_trait_taxa_clean.csv',row.names= FALSE)

# WFO taxa match for field traits

# Check names of arboretum against World Flora
NameCheckField <- WFO.match(spec.data = fieldtrait, WFO.data= wfo_taxonomy, spec.name = "finalname",
                       Fuzzy.min = TRUE)
Name_single_field <- WFO.one(NameCheckField)

NameCheck_sel_field <-Name_single_field %>% dplyr::select(NewUID ,finalname.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_field <- NameCheck_sel_field %>% distinct()

fieldtrait_wfo <- left_join(fieldtrait, NameCheck_mini_simple_field, by = c('finalname' = 'finalname.ORIG'))

fieldtrait_taxa_cleaned <- fieldtrait_wfo %>% rename('ScientificName_WFO' = scientificName, 'Species' = finalname, 'Family_WFO' = family,
                                                 'NewUID' = NewUID.x) %>% 
  dplyr::select(NewUID, Species:Family_GM, ScientificName_WFO, scientificNameAuthorship, Family_WFO, latitude:d_13C_12C)

write.csv(fieldtrait_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/field_trait_taxa_clean.csv',row.names= FALSE)

# WFO taxa match for species traits ####

NameCheckSpec <- WFO.match(spec.data = spectrait, WFO.data= wfo_taxonomy, spec.name = "species",
                            Fuzzy.min = TRUE)
Name_single_spec <- WFO.one(NameCheckSpec)

NameCheck_sel_spec <-Name_single_spec %>% dplyr::select(species.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_spec <- NameCheck_sel_spec %>% distinct()

spectrait_wfo <- left_join(spectrait, NameCheck_mini_simple_spec, by = c('species' = 'species.ORIG'))

spectrait_taxa_cleaned <- spectrait_wfo %>% rename('ScientificName_WFO' = scientificName, 'Family_WFO' = family) %>% 
  dplyr::select(species:family_POSA, ScientificName_WFO, scientificNameAuthorship, Family_WFO, perennial:functional_twig)


spectrait_taxa_cleaned <-  spectrait_taxa_cleaned %>% mutate(family_GM = str_to_title(family_GM),
                                  family_POSA= str_to_title(family_POSA))

write.csv(spectrait_taxa_cleaned, '/Users/henryfrye/Dropbox/Intellectual_Endeavours/DimensionsDataPaper/GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/species_trait_taxa_clean.csv',row.names= FALSE)

# WFO taxa match for vnir spectroscopy data ####

NameCheck_vnir <- WFO.match(spec.data = vnirspec, WFO.data= wfo_taxonomy, spec.name = "finalname",
                           Fuzzy.min = TRUE)
Name_single_vnir <- WFO.one(NameCheck_vnir)

NameCheck_sel_vnir <- Name_single_vnir %>% dplyr::select(finalname.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_vnir <- NameCheck_sel_vnir %>% distinct()

vnir_wfo <- left_join(vnirspec, NameCheck_mini_simple_vnir, by = c('finalname' = 'finalname.ORIG'))

vnir_taxa_cleaned <- vnir_wfo %>% rename('ScientificName_WFO' = scientificName, 'Family_WFO' = family) %>% 
  dplyr::select(NewUID, finalname, Genus, Species, FamilyManningGoldblatt, ScientificName_WFO, scientificNameAuthorship, Family_WFO, Latitude:X949)

write.csv(vnir_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/vnir_taxa_clean.csv',row.names= FALSE)

# WFO taxa match for releve data ####

NameCheck_releve <- WFO.match(spec.data = releve, WFO.data= wfo_taxonomy, spec.name = "Species",
                            Fuzzy.min = TRUE)
Name_single_releve <- WFO.one(NameCheck_releve)

NameCheck_sel_releve <- Name_single_releve %>% dplyr::select(Species.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_releve <- NameCheck_sel_releve %>% distinct()

releve_wfo <- left_join(releve, NameCheck_mini_simple_releve, by = c('Species' = 'Species.ORIG'))

releve_taxa_cleaned <- releve_wfo %>% rename('ScientificName_WFO' = scientificName, 'Family_WFO' = family) %>% 
  dplyr::select(Species, Family, ScientificName_WFO, scientificNameAuthorship, Family_WFO, Plot:RelPercCover)

write.csv(releve_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/releve_taxa_clean.csv',row.names= FALSE)
