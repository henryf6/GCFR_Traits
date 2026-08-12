########################################################
# Workflow3_CAM_Family_Diagnostic.R
#
# Purpose: Reviewer response diagnostic (Reviewer 1, Comment 5) --
# characterize the proportion of putative CAM taxa (Crassulaceae,
# Aizoaceae) by subregion, and re-assess the aridity-d13C relationship
# with and without those families to test whether they explain the
# absence of the expected bimodal d13C pattern. Also profiles family
# composition (particularly Asteraceae) in the two most arid subregions
# (cederberg, htr) as an alternative explanatory driver.
#
# NOTE: this is a diagnostic script for the reviewer response, not a
# data-cleaning step -- no records in canopy_leaf_chemistry.csv are
# altered here. Outputs are descriptive summary tables cited directly
# in the response to Reviewer 1, Comment 5.
#
# Date Created: August 2026
# Author(s): Henry Frye, Claude
########################################################

library(tidyverse)

data_output_path <- 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/'
reviewer_response_path <- 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Reviewer_Response_Diagnostics/'

canopy_chem <- read_csv(paste0(data_output_path, 'canopy_leaf_chemistry.csv'))

# ---- 1. Putative CAM family flag ----
# No complete photosynthetic-pathway data exists for this flora; Crassulaceae
# and Aizoaceae are treated as putatively CAM here, consistent with the
# reviewer response text (most members of both families in the GCFR are
# CAM or CAM-facultative). Flag, don't filter yet, so both the full and
# CAM-excluded summaries can be built from the same object below.
cam_families <- c("Crassulaceae", "Aizoaceae")

canopy_chem <- canopy_chem %>%
  mutate(putative_cam = family_MG %in% cam_families)

# ---- 2. Family composition by subregion (all families) ----
family_composition <- canopy_chem %>%
  filter(!is.na(family_MG)) %>%
  count(subregion, family_MG) %>%
  group_by(subregion) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(subregion, desc(pct)) %>%
  ungroup()

write_csv(family_composition, paste0(reviewer_response_path, 'family_composition_by_subregion.csv'))

# ---- 3. Asteraceae proportion for the two arid subregions cited in the response ----
asteraceae_summary <- family_composition %>%
  filter(family_MG == "Asteraceae", subregion %in% c("cederberg", "htr")) %>%
  select(subregion, n, pct)

asteraceae_summary
# double check these two numbers against what's written in the response --
# last time I ran this on the console version of the data I had 14% / 32%;
# confirm the polished-data numbers below still match before locking the text

# ---- 4. Putative CAM proportion by subregion ----
cam_summary <- canopy_chem %>%
  filter(!is.na(family_MG)) %>%
  group_by(subregion) %>%
  summarise(
    n_total = n(),
    n_cam = sum(putative_cam),
    pct_cam = round(100 * n_cam / n_total, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_cam))

write_csv(cam_summary, paste0(reviewer_response_path, 'putative_cam_proportion_by_subregion.csv'))

# ---- 5. d13C pattern: full dataset vs. CAM-excluded, by subregion ----
d13c_by_subregion <- function(df, label) {
  df %>%
    filter(!is.na(d_13C_12C)) %>%
    group_by(subregion) %>%
    summarise(
      n = n(),
      mean_d13C = mean(d_13C_12C, na.rm = TRUE),
      sd_d13C = sd(d_13C_12C, na.rm = TRUE),
      median_d13C = median(d_13C_12C, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(dataset = label, .before = 1)
}

d13c_full    <- d13c_by_subregion(canopy_chem, "all_taxa")
d13c_no_cam  <- d13c_by_subregion(canopy_chem %>% filter(!putative_cam), "cam_excluded")

d13c_comparison <- bind_rows(d13c_full, d13c_no_cam) %>%
  arrange(subregion, dataset)

write_csv(d13c_comparison, paste0(reviewer_response_path, 'd13C_cam_exclusion_comparison.csv'))

d13c_comparison


# ---- 6. Visual check: does excluding CAM taxa restore the expected bimodal shape? ----
ggplot(canopy_chem %>% filter(!is.na(d_13C_12C)),
       aes(x = d_13C_12C, fill = putative_cam)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
  facet_wrap(~subregion) +
  labs(title = "d13C distribution by subregion, putative CAM taxa (Crassulaceae/Aizoaceae) highlighted",
       fill = "Putative CAM")
