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
#library(arrow)

# Read in data
labtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/LabTraitDataERT.csv')
fieldtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/FieldTraitDataERT.csv')
vnirspec <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/GCFRSpectralLibraryEcoSisV3.csv')
spectrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/speciesXtraits.csv')
releve <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/Releve_All.csv')

# The backbone is not available via this git repo since it is nearly 1 gb in size. 
# The latest version can be accessed here:
#   https://www.worldfloraonline.org/downloadData;jsessionid=16FC96696DE5D11981026F44546F3E96
wfo_taxonomy <- read_tsv("/Volumes/Enspec/users/henry/WFO_backbones/classification_v.2026.6.csv")

# Make minor taxonomic edits by hand given input from botanist (RT) ####

# For labtait, fieldtrait, and vnirspec adjust the original species name entry as follows [per Ross Turner checks]
#   1) replace Erica areolata at 710_17_langeberg as just Erica, E. areolata is a narrow endemic in a different region
#   2) The accepted name for Erica demissa is Erica oresbia for 703_8_baviaanskloof, 718_9_baviaanskloof, and 
#   701_59_baviaanskloof ... the WFO match does not perform well here. 
#   3) 711_18_baviaanskloof make sure Erica sparrmanii with two r's and i's and only 1 n

labtrait <- labtrait %>% mutate(finalname = case_when(
  NewUID == '710_17_langeberg' & finalname == 'Erica areolata' ~ 'Erica',
  NewUID == '703_8_baviaanskloof' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '718_9_baviaanskloof' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '701_59_baviaanskloof' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '711_18_baviaanskloof' & finalname == 'Erica sparrmannii' ~ 'Erica sparrmanii',
  TRUE ~ finalname
))

fieldtrait <- fieldtrait %>% mutate(finalname = case_when(
  NewUID == '710_17_langeberg' & finalname == 'Erica areolata' ~ 'Erica',
  NewUID == '703_8_baviaanskloof' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '718_9_baviaanskloof' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '701_59_baviaanskloof' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '711_18_baviaanskloof' & finalname == 'Erica sparrmannii' ~ 'Erica sparrmanii',
  TRUE ~ finalname
))

vnirspec <- vnirspec %>% mutate(finalname = case_when(
  NewUID == '710_17_LB' & finalname == 'Erica areolata' ~ 'Erica',
  NewUID == '703_8_BK' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '718_9_BK' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '701_59_BK' & finalname == 'Erica demissa' ~ 'Erica oresbia',
  NewUID == '711_18_BK' & finalname == 'Erica sparrmannii' ~ 'Erica sparrmanii',
  TRUE ~ finalname
))


# add the sp./species suffix-stripping regex to releve, same as labtrait/fieldtrait
# (this was previously missing for releve, which is why these names failed to 
# resolve against WFO in both the original 2013 pipeline and the first WFO pass)
releve <- releve %>% mutate(finalname_pre = ifelse(
  grepl("\\bsp\\.?\\b|\\bsp\\s*\\d+\\b|\\bspecies\\b", Species, ignore.case = TRUE) &
    !grepl("\\bsubsp\\b|\\bvar\\b|\\bcf\\b", Species, ignore.case = TRUE),
  gsub("\\s+(sp\\.?\\s*\\d*|species).*", "", Species, ignore.case = TRUE),
  Species
))

# manual corrections for names WFO.match wont' resolve, verified by hand
releve <- releve %>% mutate(finalname_pre = case_when(
  finalname_pre %in% c("Crotularia") ~ "Crotalaria",              # from "Crotularia sp.", "Crotularia sp. (rank)"
  finalname_pre == "Anacampceros"    ~ "Anacampseros",
  finalname_pre == "Stratiola"       ~ "Struthiola",               # HR838, likely field dictation error
  finalname_pre == "Euphorbia slap stems" ~ "Euphorbia",
  TRUE ~ finalname_pre
))

# Data checks lab traits (these are the foliar chemistry and canopy data) ####

# Check the species differences with the manning and goldblatt text
gm_taxa <- read.csv(file = 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/GMTaxonomy.csv')
sp_id_misalign_lab <- labtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)

# How many taxa are off?
length(unique((sp_id_misalign_lab$finalname)))

# Remove "sp", "sp." or "species" from final name designations for the sake consistency
labtrait$finalname <- ifelse(
  grepl("\\bsp\\.?\\b|\\bsp\\s*\\d+\\b|\\bspecies\\b", labtrait$finalname, ignore.case = TRUE) &
    !grepl("\\bsubsp\\b|\\bvar\\b|\\bcf\\b", labtrait$finalname, ignore.case = TRUE),
  gsub("\\s+(sp\\.?\\s*\\d*|species).*", "", labtrait$finalname, ignore.case = TRUE),
  labtrait$finalname
)

# Re-run and view mismatches
sp_id_misalign_lab <- labtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)

# Remove "(form C)", part of this taxa Lotononis falcata (form C)
labtrait <- labtrait %>% mutate(finalname = case_when(
  finalname == 'Lotononis falcata (form C)' ~ 'Lotononis falcata',
  TRUE ~ finalname
))

# Data checks for field traits (leaf morphology and water content) ####
sp_id_misalign_field <- fieldtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname)))

# Remove "sp", "sp." or "species" from final name designations for the sake consistency
fieldtrait$finalname <- ifelse(
  grepl("\\bsp\\.?\\b|\\bsp\\s*\\d+\\b|\\bspecies\\b", fieldtrait$finalname, ignore.case = TRUE) &
    !grepl("\\bsubsp\\b|\\bvar\\b|\\bcf\\b", fieldtrait$finalname, ignore.case = TRUE),
  gsub("\\s+(sp\\.?\\s*\\d*|species).*", "", fieldtrait$finalname, ignore.case = TRUE),
  fieldtrait$finalname
)

# Remove "(form C)", part of this taxa Lotononis falcata (form C)
fieldtrait <- fieldtrait %>% mutate(finalname = case_when(
  finalname == 'Lotononis falcata (form C)' ~ 'Lotononis falcata',
  TRUE ~ finalname
))

# standardize taxon names for missing spaces and periods
standardize_taxon_names <- function(x) {
  # Add a period after subsp/var/cf when missing (but not when already present)
  x <- gsub("\\b(subsp|var|cf)\\b(?!\\.)", "\\1.", x, perl = TRUE)
  
  # Normalize spacing after the abbreviation period to exactly one space
  # (catches "cf.crassa" -> "cf. crassa" and any double-spaced cases)
  x <- gsub("\\b(subsp|var|cf)\\.\\s*", "\\1. ", x, perl = TRUE)
  
  # Collapse any incidental double spaces / trailing whitespace introduced above
  x <- trimws(gsub("\\s+", " ", x))
  
  # Flag plot-based placeholder names (e.g. "Plot70sp1") as unknown species
  x <- ifelse(grepl("^Plot\\d+sp\\d*$", x, ignore.case = TRUE), "unknown species", x)
  
  x
}

labtrait$finalname   <- standardize_taxon_names(labtrait$finalname)
fieldtrait$finalname <- standardize_taxon_names(fieldtrait$finalname)
vnirspec$finalname   <- standardize_taxon_names(vnirspec$finalname)

releve$Species   <- standardize_taxon_names(releve$Species)
spectrait$species <- standardize_taxon_names(spectrait$species)
spectrait$genus_species_GM <- standardize_taxon_names(spectrait$genus_species_GM)

sp_id_misalign_field <- fieldtrait %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname)))

# Data check for species-level trait table ####
sp_id_misalign_field <- spectrait %>% dplyr::filter(! genus_species_GM %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname))) # 0, which makes sense since the species are based on GM

# Check spectral data ####
sp_id_misalign_vnir <- vnirspec %>% dplyr::filter(! finalname %in% gm_taxa$Taxon)
length(unique((sp_id_misalign_field$finalname))) # great 0 as well!

# WFO taxon harmonization for foliar structure and water traits ####

# Add a synthetic row index purely to guarantee a 1:1 join with the WFO match
# output below. NewUID alone isn't unique here (needs collector + replicate,
# which aren't reconstructed as a full sample ID until Workflow 3) -- but
# WFO.match/WFO.one return exactly one output row per input row regardless,
# so a row index is a safe and simpler join key than any natural column.
labtrait <- labtrait %>% mutate(wfo_row_id = row_number())

NameCheck <- WFO.match(spec.data = labtrait, WFO.data= wfo_taxonomy, spec.name = "finalname",
                       Fuzzy.min = TRUE)
Name_single <- WFO.one(NameCheck)

# Diagnostic-first: confirm WFO.one preserved exactly one row per input row
# before trusting a positional join on wfo_row_id
stopifnot(nrow(Name_single) == nrow(labtrait))
stopifnot(!any(duplicated(Name_single$wfo_row_id)))

NameCheck_sel <- Name_single %>% dplyr::select(wfo_row_id, scientificName, family, scientificNameAuthorship)

labtrait_wfo <- left_join(labtrait, NameCheck_sel, by = 'wfo_row_id')

# Confirm the join didn't change row count (i.e. stayed 1:1)
stopifnot(nrow(labtrait_wfo) == nrow(labtrait))

labtrait_taxa_cleaned <- labtrait_wfo %>% rename('ScientificName_WFO' = scientificName, 'Species' = finalname, 'Family_WFO' = family) %>% 
  dplyr::select(NewUID, Species:Family_GM, ScientificName_WFO, scientificNameAuthorship, Family_WFO, latitude:twig_fwc)

write.csv(labtrait_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/lab_trait_taxa_clean.csv',row.names= FALSE)

# Provenance note Aug 12, 2026: Joins have been updated for a unique temporary row index
# the original join caused a many-to-many join which duplicated rows (27,337). The rows are now 9547. 
# The duplication issue was addressed in workflow 3, but this catches the issue at the source.

# WFO taxon harmonization for foliar chemistry and canopy traits #####

fieldtrait <- fieldtrait %>% mutate(wfo_row_id = row_number())

NameCheckField <- WFO.match(spec.data = fieldtrait, WFO.data= wfo_taxonomy, spec.name = "finalname",
                            Fuzzy.min = TRUE)
nrow(NameCheck)
Name_single_field <- WFO.one(NameCheckField)
nrow(Name_single_field)
stopifnot(nrow(Name_single_field) == nrow(fieldtrait))
stopifnot(!any(duplicated(Name_single_field$wfo_row_id)))

NameCheck_sel_field <- Name_single_field %>% dplyr::select(wfo_row_id, scientificName, family, scientificNameAuthorship)

fieldtrait_wfo <- left_join(fieldtrait, NameCheck_sel_field, by = 'wfo_row_id')

stopifnot(nrow(fieldtrait_wfo) == nrow(fieldtrait))

fieldtrait_taxa_cleaned <- fieldtrait_wfo %>% rename('ScientificName_WFO' = scientificName, 'Species' = finalname, 'Family_WFO' = family) %>% 
  dplyr::select(NewUID, Species:Family_GM, ScientificName_WFO, scientificNameAuthorship, Family_WFO, latitude:d_13C_12C)

write.csv(fieldtrait_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/field_trait_taxa_clean.csv',row.names= FALSE)

# Provenance note Aug 12, 2026: Joins have been updated for a unique temporary row index
# the original join caused a many-to-many join which duplicated rows (7431). The rows are now 2509. 
# The duplication issue was addressed in workflow 3, but this catches the issue at the source.


# WFO taxon harmonization for species trait table ####

NameCheckSpec <- WFO.match(spec.data = spectrait, WFO.data= wfo_taxonomy, spec.name = "species",
                            Fuzzy.min = TRUE)
Name_single_spec <- WFO.one(NameCheckSpec)

NameCheck_sel_spec <-Name_single_spec %>% dplyr::select(species.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_spec <- NameCheck_sel_spec %>% distinct()

spectrait_wfo <- left_join(spectrait, NameCheck_mini_simple_spec, by = c('species' = 'species.ORIG'))

spectrait_taxa_cleaned <- spectrait_wfo %>% rename('ScientificName_WFO' = scientificName, 'Family_WFO' = family) %>% 
  dplyr::select(species:family_POSA, ScientificName_WFO, scientificNameAuthorship, Family_WFO, perennial:dispersal)


spectrait_taxa_cleaned <-  spectrait_taxa_cleaned %>% mutate(family_GM = str_to_title(family_GM),
                                  family_POSA= str_to_title(family_POSA))

write.csv(spectrait_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/species_trait_taxa_clean.csv',row.names= FALSE)

# WFO taxon harmonization for vnir spectroscopy data ####

NameCheck_vnir <- WFO.match(spec.data = vnirspec, WFO.data= wfo_taxonomy, spec.name = "finalname",
                           Fuzzy.min = TRUE)
Name_single_vnir <- WFO.one(NameCheck_vnir)

NameCheck_sel_vnir <- Name_single_vnir %>% dplyr::select(finalname.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_vnir <- NameCheck_sel_vnir %>% distinct()

vnir_wfo <- left_join(vnirspec, NameCheck_mini_simple_vnir, by = c('finalname' = 'finalname.ORIG'))

vnir_taxa_cleaned <- vnir_wfo %>% rename('ScientificName_WFO' = scientificName, 'Family_WFO' = family) %>% 
  dplyr::select(NewUID, finalname, Genus, Species, FamilyManningGoldblatt, ScientificName_WFO, scientificNameAuthorship, Family_WFO, Latitude:X949)

write.csv(vnir_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/vnir_taxa_clean.csv',row.names= FALSE)

# WFO taxon harmonization for releve data ####

NameCheck_releve <- WFO.match(spec.data = releve, WFO.data= wfo_taxonomy, spec.name = "finalname_pre",
                            Fuzzy.min = TRUE)
Name_single_releve <- WFO.one(NameCheck_releve)

NameCheck_sel_releve <- Name_single_releve %>% 
  dplyr::select(finalname_pre.ORIG, scientificName, family, scientificNameAuthorship)

NameCheck_mini_simple_releve <- NameCheck_sel_releve %>% distinct()

releve_wfo <- left_join(releve, NameCheck_mini_simple_releve, by = c('finalname_pre' = 'finalname_pre.ORIG'))

releve_wfo <- releve_wfo %>%
  mutate(
    family = case_when(
      !is.na(family) ~ family,
      str_detect(finalname_pre, "Mesembryanthemaceae") ~ "Mesembryanthemaceae",
      finalname_pre == "Orchid" ~ "Orchidaceae",  # post-suffix-strip, "Orchid sp" -> "Orchid"
      TRUE ~ family
    ),
    recovery_status = case_when(
      !is.na(scientificName) ~ "wfo_matched",
      Species == "27987" ~ "flagged_non_taxonomic_entry",
      str_detect(finalname_pre, "Mesembryanthemaceae") | finalname_pre == "Orchid" ~ "recovered_family_only",
      TRUE ~ "unresolved_morphotype"
    )
  )

# quick sanity check before finalizing -- confirm counts look like what you expect,
# and eyeball anything new that fell into unresolved_morphotype
releve_wfo %>% count(recovery_status)
releve_wfo %>% filter(recovery_status == "unresolved_morphotype") %>% distinct(Species, finalname_pre)

releve_taxa_cleaned <- releve_wfo %>% 
  rename('ScientificName_WFO' = scientificName, 'Family_WFO' = family) %>% 
  dplyr::select(Species, ScientificName_WFO, scientificNameAuthorship, Family_WFO, 
                recovery_status, plot:RelPercCover)

write.csv(releve_taxa_cleaned, 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/releve_taxa_clean.csv', row.names = FALSE)
