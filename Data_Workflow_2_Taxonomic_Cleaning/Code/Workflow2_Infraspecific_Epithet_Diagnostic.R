########################################################
# Workflow2_Infraspecific_Epithet_Diagnostic.R
#
# Purpose: Reviewer response diagnostic (Reviewer 2) -- trace where in
# the pipeline (raw input -> Workflow 2 taxonomic cleaning -> Workflow 3
# polish) the subsp./var. epithet is being dropped for a subset of
# species-level records. Several reviewer-flagged names (e.g. "Ehrharta
# ramosa subsp ramosa") come through the final species_traits.csv with
# the infraspecific portion missing from scientific_name_original and/or
# scientific_name_MG, inconsistently across records and columns. This
# script isolates which pipeline stage and which column is responsible,
# then sweeps the full dataset to check whether the reviewer's named
# examples are the whole problem or just what they happened to spot-check.
#
# NOTE: diagnostic only -- does not modify any data.
#
# Date Created: August 2026
# Author(s): Henry Frye, Claude
########################################################

library(tidyverse)

data_input_path        <- 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/'
wf2_output_path         <- 'GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/'
wf3_output_path         <- 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/'
reviewer_response_path  <- 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Reviewer_Response_Diagnostics/'

# ---- 0. Anchor list: the binomials the reviewer actually flagged ----
# genus + species epithet only (infraspecific rank stripped out), so we
# can find the same record at every pipeline stage regardless of what
# currently is/isn't attached to it
flagged_binomials <- tibble::tribble(
  ~genus,          ~species_epithet,   ~expected_infra_rank, ~expected_infra_epithet,
  "Babiana",       "sambucina",        "var.",                "longibracteata",
  "Ehrharta",      "ramosa",           "subsp.",              "ramosa",
  "Osteospermum",  "incanum",          "subsp.",              "incanum",
  "Ursinia",       "nana",             "subsp.",              "nana",
  "Atriplex",      "cinerea",          "subsp.",              "bolusii",
  "Oxalis",        "massoniana",       "var.",                "massoniana",
  "Eriocephalus",  "microphyllus",     "var.",                "microphyllus",
  "Eriocephalus",  "ericoides",        "subsp.",              "ericoides"
)

# ---- 1. Helper: parse a name string into genus / species / infra rank / infra epithet ----
parse_name <- function(x) {
  x <- str_squish(as.character(x))
  tibble(
    raw              = x,
    genus            = word(x, 1),
    species_epithet  = word(x, 2),
    infra_rank       = str_extract(x, "\\b(subsp|var|cf)\\.?\\b"),
    infra_epithet    = str_extract(x, "(?<=(subsp|var|cf)\\.?\\s)\\S+")
  )
}

# ---- 2. Load each pipeline stage ----
# adjust these read_csv() paths/filenames if your actual intermediate
# outputs live somewhere else -- I don't have these two files to confirm
# against, only the final species_traits.csv
raw_input  <- read_csv(paste0(data_input_path, 'speciesXtraits.csv'))
wf2_output <- read_csv(paste0(wf2_output_path, 'species_trait_taxa_clean.csv'))
wf3_output <- read_csv(paste0(wf3_output_path, 'species_traits.csv'))

# ---- 3. Parse the relevant name columns at each stage ----
raw_species_parsed <- parse_name(raw_input$species)              %>% mutate(stage = "raw_input",  field = "species")
raw_gm_parsed       <- parse_name(raw_input$genus_species_GM)    %>% mutate(stage = "raw_input",  field = "genus_species_GM")

wf2_species_parsed <- parse_name(wf2_output$species)              %>% mutate(stage = "wf2_output", field = "species")
wf2_gm_parsed       <- parse_name(wf2_output$genus_species_GM)    %>% mutate(stage = "wf2_output", field = "genus_species_GM")

wf3_orig_parsed <- parse_name(wf3_output$scientific_name_original) %>% mutate(stage = "wf3_output", field = "scientific_name_original")
wf3_mg_parsed    <- parse_name(wf3_output$scientific_name_MG)       %>% mutate(stage = "wf3_output", field = "scientific_name_MG")

all_parsed <- bind_rows(
  raw_species_parsed, raw_gm_parsed,
  wf2_species_parsed, wf2_gm_parsed,
  wf3_orig_parsed, wf3_mg_parsed
)

# ---- 4. Filter to just the flagged binomials, across every stage/field ----
trace_table <- all_parsed %>%
  inner_join(flagged_binomials, by = c("genus", "species_epithet")) %>%
  select(genus, species_epithet, stage, field, raw, infra_rank, infra_epithet,
         expected_infra_rank, expected_infra_epithet) %>%
  arrange(genus, species_epithet, field, stage)

trace_table
# read this top to bottom for each genus/species_epithet/field group:
# whichever stage first shows infra_epithet == NA (after a previous stage
# had it populated) is where the bug is introduced

write_csv(trace_table, paste0(reviewer_response_path, 'infraspecific_epithet_trace.csv'))

# ---- 5. Flag exactly where the epithet disappears ----
stage_order <- c("raw_input", "wf2_output", "wf3_output")

epithet_loss_check <- trace_table %>%
  mutate(
    stage = factor(stage, levels = stage_order, ordered = TRUE),
    has_infra = !is.na(infra_epithet)
  ) %>%
  arrange(genus, species_epithet, field, stage) %>%
  group_by(genus, species_epithet, field) %>%
  mutate(
    prev_has_infra         = lag(has_infra),
    dropped_at_this_stage  = !has_infra & prev_has_infra
  ) %>%
  ungroup()

# these rows tell you exactly which stage AND which column (species/
# scientific_name_original vs. genus_species_GM/scientific_name_MG) is
# responsible for stripping the epithet for each named example
epithet_loss_check %>% filter(dropped_at_this_stage)

write_csv(epithet_loss_check, paste0(reviewer_response_path, 'infraspecific_epithet_loss_by_stage.csv'))

# ---- 6. Full-dataset sweep ----
# the named list above is only what the reviewer happened to spot-check --
# this checks every record in the final output for a mismatch between
# scientific_name_original and scientific_name_MG having/lacking an infra
# rank, to see the true scope (including cases like Eriocephalus where
# BOTH columns lost it, which section 5 alone won't catch since there's
# nothing left to compare against by wf3_output)
cross_field_check <- wf3_output %>%
  transmute(
    genus_species_original = str_extract(scientific_name_original, "^\\S+\\s+\\S+"),
    genus_species_mg       = str_extract(scientific_name_MG, "^\\S+\\s+\\S+"),
    has_infra_original     = str_detect(scientific_name_original, "\\b(subsp|var|cf)\\.?\\b"),
    has_infra_mg           = str_detect(scientific_name_MG, "\\b(subsp|var|cf)\\.?\\b")
  ) %>%
  filter(has_infra_original != has_infra_mg)

cross_field_check
write_csv(cross_field_check, paste0(reviewer_response_path, 'infraspecific_mismatch_full_dataset.csv'))

# ---- 7. Both-columns-lost-it check ----
# separately worth a manual skim: any genus+species pair present with an
# infra rank in raw_input but with NEITHER wf3 column showing one --
# these won't appear in section 6's mismatch check at all
both_lost <- epithet_loss_check %>%
  filter(stage == "wf3_output", !has_infra) %>%
  distinct(genus, species_epithet, field) %>%
  count(genus, species_epithet) %>%
  filter(n == 2)  # both scientific_name_original AND scientific_name_MG missing it at wf3

both_lost
