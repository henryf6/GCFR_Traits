########################################################
# Workflow4_Outlier_Detection.R
#
# Purpose: Flag potentially problematic trait values using
# a tiered approach 
#
#
# Date Created: July 2026
# Author(s): Henry Frye, Claude
########################################################

library(tidyverse)
library(smatr)   

# ---- 0. Load harmonized data from workflow 2 --------------------------------
fieldtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/field_trait_taxa_clean.csv')
labtrait <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Outputs/lab_trait_taxa_clean.csv')

labtrait <- labtrait %>%
  unite(sample_id, date, sample, region, collector, sep = "_", remove = FALSE) %>%
  relocate(sample_id, .before = 1) %>%
  filter(!duplicated(.))


fieldtrait <- fieldtrait %>%
  unite(sample_id, date, sample, region, collector, sep = "_", remove = FALSE) %>%
  relocate(sample_id, .before = 1) %>%
  filter(!duplicated(.))

data_quality_path = 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Quality_Check_Outputs/'
intermediate_data_path = 'GCFR_Traits/Data_Workflow_3_Final_Polishing/Intermediate_Outputs/'

write_csv(fieldtrait, paste0(intermediate_data_path, 'canopy_chem_intermediate.csv'))
write_csv(labtrait, paste0(intermediate_data_path, 'leaf_struc_intermediate.csv'))

# ---- Resequence duplicated replicate labels within sample_id --------------
# Where a sample_id has replicate labels that repeat (e.g., 1,1,2,2,3,3...),
# treat these as genuinely separate measurement events (per your read of the
# num_leaves pattern) rather than duplicate entries, and renumber them
# sequentially in original row order: 1,1,2,2,3,3 -> 1,2,3,4,5,6

# Identify affected sample_ids first, for your own record/documentation
dupe_replicate_samples <- labtrait %>%
  count(sample_id, replicate) %>%
  filter(n > 1) %>%
  distinct(sample_id) %>%
  pull(sample_id)

length(dupe_replicate_samples)  # confirm this is 18

labtrait <- labtrait %>%
  mutate(row_order = row_number()) %>%
  group_by(sample_id) %>%
  mutate(
    replicate = if_else(
      sample_id %in% dupe_replicate_samples,
      as.character(rank(row_order, ties.method = "first")),
      replicate
    )
  ) %>%
  ungroup() %>%
  select(-row_order)

# Verify: affected samples should now have unique, sequential replicate values
labtrait %>%
  filter(sample_id %in% dupe_replicate_samples) %>%
  count(sample_id, replicate) %>%
  filter(n > 1)   # should return 0 rows

# ---- Re-calculate derived values to ensure --------------


# ---- 1. Define all flagging functions for leaf structure data --------------------------------

flag_bad_lma_component <- function(df,
                                   sample_col = "sample_id",
                                   lma_col = "lma",
                                   weight_col = "leaf_dry_wgt_g",
                                   area_col = "leaf_area_cm2",
                                   ratio_threshold = 2) {
  
  loo_median <- function(x) {
    sapply(seq_along(x), function(i) median(x[-i], na.rm = TRUE))
  }
  
  df %>%
    group_by(.data[[sample_col]]) %>%
    filter(n() >= 2, !is.na(.data[[lma_col]])) %>%
    mutate(
      lma_loo_med    = loo_median(.data[[lma_col]]),
      weight_loo_med = loo_median(.data[[weight_col]]),
      area_loo_med   = loo_median(.data[[area_col]]),
      
      lma_ratio   = .data[[lma_col]] / lma_loo_med,
      weight_dev  = abs(log2(.data[[weight_col]] / weight_loo_med)),
      area_dev    = abs(log2(.data[[area_col]]   / area_loo_med)),
      
      flag_bad_lma = lma_ratio > ratio_threshold | lma_ratio < (1 / ratio_threshold),
      
      suspected_bad = case_when(
        !flag_bad_lma            ~ NA_character_,
        area_dev > weight_dev    ~ "area",
        weight_dev > area_dev    ~ "weight",
        TRUE                     ~ "ambiguous"
      )
    ) %>%
    ungroup() %>%
    filter(flag_bad_lma) %>%
    select(
      NewUID, all_of(sample_col), replicate, ScientificName_WFO,
      all_of(lma_col), lma_ratio,
      all_of(weight_col), weight_dev,
      all_of(area_col), area_dev,
      suspected_bad
    ) %>%
    arrange(desc(lma_ratio))
}

# ---- 1. TIER 1a: Check for possible bad measures/entries using LMA for bad weights and areas ---------------
# note that these aren't necessarily bad measures sometimes more leaves were used
# in measurements resulting in higher weights/areas compared to other replicates. This is why this
# get cross-checked with the CV results later.

flag_bad_lma_component <- function(df,
                                   sample_col = "sample_id",
                                   lma_col = "lma",
                                   weight_col = "leaf_dry_wgt_g",
                                   area_col = "leaf_area_cm2",
                                   ratio_threshold = 2) {
  
  loo_median <- function(x) {
    sapply(seq_along(x), function(i) median(x[-i], na.rm = TRUE))
  }
  
  df %>%
    group_by(.data[[sample_col]]) %>%
    filter(n() >= 2, !is.na(.data[[lma_col]])) %>%
    mutate(
      lma_loo_med    = loo_median(.data[[lma_col]]),
      weight_loo_med = loo_median(.data[[weight_col]]),
      area_loo_med   = loo_median(.data[[area_col]]),
      
      lma_ratio   = .data[[lma_col]] / lma_loo_med,
      weight_dev  = abs(log2(.data[[weight_col]] / weight_loo_med)),
      area_dev    = abs(log2(.data[[area_col]]   / area_loo_med)),
      
      flag_bad_lma = lma_ratio > ratio_threshold | lma_ratio < (1 / ratio_threshold),
      
      suspected_bad = case_when(
        !flag_bad_lma            ~ NA_character_,
        area_dev > weight_dev    ~ "area",
        weight_dev > area_dev    ~ "weight",
        TRUE                     ~ "ambiguous"
      )
    ) %>%
    ungroup() %>%
    filter(flag_bad_lma) %>%
    select(
      NewUID, all_of(sample_col), replicate, ScientificName_WFO,
      all_of(lma_col), lma_ratio,
      all_of(weight_col), weight_dev,
      all_of(area_col), area_dev,
      suspected_bad
    ) %>%
    arrange(desc(lma_ratio))
}

flag_high_cv <- function(df, group_var, trait_col, min_n = 2, cv_quantile = 0.95) {
  df %>%
    filter(!is.na(.data[[trait_col]])) %>%
    group_by(.data[[group_var]]) %>%
    filter(n() >= min_n) %>%
    summarise(
      n_obs = n(),
      mean_val = mean(.data[[trait_col]], na.rm = TRUE),
      sd_val = sd(.data[[trait_col]], na.rm = TRUE),
      cv = sd_val / mean_val,
      .groups = "drop"
    ) %>%
    mutate(flag_high_cv = cv > quantile(cv, cv_quantile, na.rm = TRUE))
}


flag_bad_ratio_component <- function(df,
                                     sample_col = "sample_id",
                                     ratio_col,
                                     comp1_col,
                                     comp2_col,
                                     ratio_threshold = 2) {
  
  loo_median <- function(x) {
    sapply(seq_along(x), function(i) median(x[-i], na.rm = TRUE))
  }
  
  df %>%
    group_by(.data[[sample_col]]) %>%
    filter(n() >= 2, !is.na(.data[[ratio_col]])) %>%
    mutate(
      ratio_loo_med  = loo_median(.data[[ratio_col]]),
      comp1_loo_med  = loo_median(.data[[comp1_col]]),
      comp2_loo_med  = loo_median(.data[[comp2_col]]),
      
      trait_ratio = .data[[ratio_col]] / ratio_loo_med,
      comp1_dev   = abs(log2(.data[[comp1_col]] / comp1_loo_med)),
      comp2_dev   = abs(log2(.data[[comp2_col]] / comp2_loo_med)),
      
      flag_bad = trait_ratio > ratio_threshold | trait_ratio < (1 / ratio_threshold),
      
      suspected_bad = case_when(
        !flag_bad               ~ NA_character_,
        comp1_dev > comp2_dev   ~ comp1_col,
        comp2_dev > comp1_dev   ~ comp2_col,
        TRUE                    ~ "ambiguous"
      )
    ) %>%
    ungroup() %>%
    filter(flag_bad) %>%
    select(
      NewUID, all_of(sample_col), replicate, ScientificName_WFO,
      all_of(ratio_col), trait_ratio,
      all_of(comp1_col), comp1_dev,
      all_of(comp2_col), comp2_dev,
      suspected_bad
    ) %>%
    arrange(desc(trait_ratio))
}

flag_bad_single_measure <- function(df,
                                    sample_col = "sample_id",
                                    measure_col,
                                    ratio_threshold = 2) {
  
  loo_median <- function(x) {
    sapply(seq_along(x), function(i) median(x[-i], na.rm = TRUE))
  }
  
  df %>%
    group_by(.data[[sample_col]]) %>%
    filter(n() >= 2, !is.na(.data[[measure_col]])) %>%
    mutate(
      measure_loo_med = loo_median(.data[[measure_col]]),
      measure_ratio    = .data[[measure_col]] / measure_loo_med,
      flag_bad_measure = measure_ratio > ratio_threshold | measure_ratio < (1 / ratio_threshold)
    ) %>%
    ungroup() %>%
    filter(flag_bad_measure) %>%
    select(NewUID, all_of(sample_col), replicate, ScientificName_WFO,
           all_of(measure_col), measure_ratio) %>%
    arrange(desc(measure_ratio))
}




# ---- 2. Tissue-type exclusion (leaf-check vs twig-check frames) ------
no_leaf_y_stem_y_twig <- labtrait %>%
  filter(is.na(leaf_fresh_wgt_g)) %>%
  filter(num_leaves == 'stem' & !is.na(twig_fresh_g))

labtrait_leaf_check <- labtrait %>%
  anti_join(no_leaf_y_stem_y_twig, by = c("sample_id", "replicate"))

labtrait_twig_check <- labtrait

# ---- 3. Leaf structure plausibility check ---------------------------------
# flags for implausble values based on previously reported ranges
struc_bounds <- tibble::tribble(
  ~trait,          ~lower, ~upper,
  "lma",     14/10000,1500/10000, # range from Wright et al 2004
  "leaf_thickness_mm", 0.019500,  30.00000, # from TRY 2020
  "fwc", 0.4211618, 64.05119, # from TRY 2020
  "leaf_length_cm", 0.6300000 / 10,  4250.00000 /10, # from TRY 2020
  "avg_leaf_width_cm", 0.0100000 , 91.65712 # from TRY 2020
)

flag_implausible_struc <- function(df, trait_col, lower, upper) {
  df %>%
    filter(!is.na(.data[[trait_col]])) %>%
    mutate(
      flag_implausible = .data[[trait_col]] < lower | .data[[trait_col]] > upper
    ) %>%
    filter(flag_implausible) %>%
    select(sample_id, NewUID, replicate, ScientificName_WFO, all_of(trait_col))
}

struc_flags <- purrr::pmap_dfr(struc_bounds, function(trait, lower, upper) {
  if (is.na(lower) || is.na(upper)) return(NULL)  # skip until you fill it in
  flag_implausible_struc(labtrait, trait, lower, upper) %>%
    mutate(trait_flagged = trait, .before = 1)
})
# the one chemistry flag is from an unreasonable carbon value (over 100% ) that will get removed later.


# ---- 4. Run flags + CV once each, on the correct frame ----------------
possible_bad_lma_flags        <- flag_bad_lma_component(labtrait_leaf_check)
possible_bad_thickness_flags  <- flag_bad_single_measure(labtrait_leaf_check, measure_col = "leaf_thickness_mm")
possible_bad_fwc_flags        <- flag_bad_ratio_component(labtrait_leaf_check, ratio_col = "fwc", comp1_col = "leaf_fresh_wgt_g", comp2_col = "leaf_dry_wgt_g")
possible_bad_ldmc_flags       <- flag_bad_ratio_component(labtrait_leaf_check, ratio_col = "ldmc", comp1_col = "leaf_fresh_wgt_g", comp2_col = "leaf_dry_wgt_g")
possible_bad_succulence_flags <- flag_bad_ratio_component(labtrait_leaf_check, ratio_col = "succulence", comp1_col = "leaf_fresh_wgt_g", comp2_col = "leaf_area_cm2")
possible_bad_twig_fwc_flags   <- flag_bad_ratio_component(labtrait_twig_check, ratio_col = "twig_fwc", comp1_col = "twig_fresh_g", comp2_col = "twig_dry_g")

lma_cv_sample        <- flag_high_cv(labtrait_leaf_check, "sample_id", "lma")
thickness_cv_sample  <- flag_high_cv(labtrait_leaf_check, "sample_id", "leaf_thickness_mm")
fwc_cv_sample         <- flag_high_cv(labtrait_leaf_check, "sample_id", "fwc")
ldmc_cv_sample        <- flag_high_cv(labtrait_leaf_check, "sample_id", "ldmc")
succulence_cv_sample  <- flag_high_cv(labtrait_leaf_check, "sample_id", "succulence")
twig_fwc_cv_sample    <- flag_high_cv(labtrait_twig_check, "sample_id", "twig_fwc")

# ---- 5. Join ratio + CV, then write each trait out separately --------
build_confidence_flags <- function(flag_df, cv_df, ratio_col_sym) {
  flag_df %>%
    distinct() %>%
    left_join(cv_df %>% select(sample_id, cv, flag_high_cv), by = "sample_id") %>%
    mutate(confidence = if_else(flag_high_cv, "both", "ratio_only", missing = "ratio_only")) %>%
    arrange(desc(confidence == "both"), desc(.data[[ratio_col_sym]]))
}

bad_lma        <- build_confidence_flags(possible_bad_lma_flags, lma_cv_sample, "lma_ratio")
bad_thickness  <- build_confidence_flags(possible_bad_thickness_flags, thickness_cv_sample, "measure_ratio")
bad_fwc        <- build_confidence_flags(possible_bad_fwc_flags, fwc_cv_sample, "trait_ratio")
bad_ldmc       <- build_confidence_flags(possible_bad_ldmc_flags, ldmc_cv_sample, "trait_ratio")
bad_succulence <- build_confidence_flags(possible_bad_succulence_flags, succulence_cv_sample, "trait_ratio")
bad_twig_fwc   <- build_confidence_flags(possible_bad_twig_fwc_flags, twig_fwc_cv_sample, "trait_ratio")

write_csv(struc_flags,     paste0(data_quality_path, 'flagged_struc_values.csv'))
write_csv(bad_lma,        paste0(data_quality_path, 'bad_lma_values.csv'))
write_csv(bad_thickness,  paste0(data_quality_path, 'bad_thickness_values.csv'))
write_csv(bad_fwc,        paste0(data_quality_path, 'bad_fwc_values.csv'))
write_csv(bad_ldmc,       paste0(data_quality_path, 'bad_ldmc_values.csv'))
write_csv(bad_succulence, paste0(data_quality_path, 'bad_succulence_values.csv'))
write_csv(bad_twig_fwc,   paste0(data_quality_path, 'bad_twig_fwc_values.csv'))

# ---- 6. Check overlap between flags --------

flag_list <- list(
  lma        = bad_lma,
  thickness  = bad_thickness,
  fwc        = bad_fwc,
  ldmc       = bad_ldmc,
  succulence = bad_succulence,
  twig_fwc   = bad_twig_fwc
)

flag_overlap <- bind_rows(
  lapply(names(flag_list), function(nm) {
    flag_list[[nm]] %>%
      distinct(sample_id, replicate) %>%
      mutate(flag_source = nm)
  })
)

# Which replicates are flagged by more than one trait check?
flag_overlap_summary <- flag_overlap %>%
  distinct(sample_id, replicate, flag_source) %>%
  group_by(sample_id, replicate) %>%
  summarise(n_flags = n(), flags = paste(sort(flag_source), collapse = ", "), .groups = "drop") %>%
  arrange(desc(n_flags))

flag_overlap_summary %>% count(flags, sort = TRUE)

# ---- 7. Double check calculations --------
labtrait_recalc <- labtrait %>%
  mutate(
    lma_check        = leaf_dry_wgt_g / leaf_area_cm2,
    fwc_check         = (leaf_fresh_wgt_g - leaf_dry_wgt_g) / leaf_dry_wgt_g,
    ldmc_check        = leaf_dry_wgt_g / leaf_fresh_wgt_g,
    succulence_check  = (leaf_fresh_wgt_g - leaf_dry_wgt_g) / leaf_area_cm2,
    twig_fwc_check    = (twig_fresh_g - twig_dry_g) / twig_dry_g,
    
    lma_diff        = abs(lma - lma_check),
    fwc_diff         = abs(fwc - fwc_check),
    ldmc_diff        = abs(ldmc - ldmc_check),
    succulence_diff  = abs(succulence - succulence_check),
    twig_fwc_diff    = abs(twig_fwc - twig_fwc_check)
  )

# Flag any mismatch beyond floating-point tolerance
derived_trait_mismatches <- labtrait_recalc %>%
  filter(
    lma_diff > 1e-6 | fwc_diff > 1e-6 | ldmc_diff > 1e-6 |
      succulence_diff > 1e-6 | twig_fwc_diff > 1e-6
  ) %>%
  select(sample_id, replicate, ScientificName_WFO,
         lma, lma_check, lma_diff,
         fwc, fwc_check, fwc_diff,
         ldmc, ldmc_check, ldmc_diff,
         succulence, succulence_check, succulence_diff,
         twig_fwc, twig_fwc_check, twig_fwc_diff)

nrow(derived_trait_mismatches)

# ---- 8. Chemistry plausibility check ---------------------------------
# Not a statistical outlier check -- 
# This only catches physically implausible values (entry/transcription
# errors: decimal shifts, sign errors, unit mismatches).
#
# Fill in bounds from your reference study (cite it in the reviewer
# response / methods -- e.g. a GLOPNET-type global leaf economics dataset
# for %N, %C, C:N; a C3/C4 isotope compilation for d13C; a foliar d15N
# range for d15N).

chem_bounds <- tibble::tribble(
  ~trait,          ~lower, ~upper,
  "percent_N",     0.2,     46.81,   # TRY Kattge et al 2020; lower from Wright et al 2004
  "percent_C",     0,     65.7, # TRY Kattge
  "C_to_N_ratio",  NA,     NA,
  "d_15N_14N",     -14.4,     21.4, # Craine et al 2015 Plant Soil; corresponds to TRY as well
  "d_13C_12C",     -39,     -9.8 # from TRY Kattge et al 2020
)

flag_implausible_chem <- function(df, trait_col, lower, upper) {
  df %>%
    filter(!is.na(.data[[trait_col]])) %>%
    mutate(
      flag_implausible = .data[[trait_col]] < lower | .data[[trait_col]] > upper
    ) %>%
    filter(flag_implausible) %>%
    select(sample_id, NewUID, ScientificName_WFO, all_of(trait_col))
}

chem_flags <- purrr::pmap_dfr(chem_bounds, function(trait, lower, upper) {
  if (is.na(lower) || is.na(upper)) return(NULL)  # skip until you fill it in
  flag_implausible_chem(fieldtrait, trait, lower, upper) %>%
    mutate(trait_flagged = trait, .before = 1)
})
# the one chemistry flag is from an unreasonable carbon value (over 100% ) that will get removed later.

# ---- 9. Canopy geometry recheck ---------------------------------------
fieldtrait_recalc <- fieldtrait %>%
  mutate(
    canopy_area_check = pi * ((canopy_axis_1_cm  + canopy_axis_2_cm )/ 4)^2,
    canopy_area_ratio = canopy_area_cm2 / canopy_area_check,
    flag_canopy_geom   = canopy_area_ratio > 1.5 | canopy_area_ratio < (1 / 1.5)
  )

canopy_geom_flags <- fieldtrait_recalc %>%
  filter(flag_canopy_geom) %>%
  select(sample_id, NewUID, ScientificName_WFO,
         canopy_axis_1_cm, canopy_axis_2_cm,
         canopy_area_cm2, canopy_area_check, canopy_area_ratio)

# ---- 10. Magnitude / decimal-shift check --------------------------------
flag_magnitude_error <- function(df, group_var, trait_col, min_n = 5, log10_threshold = 1) {
  df %>%
    filter(!is.na(.data[[trait_col]]), .data[[trait_col]] > 0) %>%
    group_by(.data[[group_var]]) %>%
    filter(n() >= min_n) %>%
    mutate(
      group_median   = median(.data[[trait_col]], na.rm = TRUE),
      log10_dev      = abs(log10(.data[[trait_col]] / group_median)),
      flag_magnitude = log10_dev > log10_threshold
    ) %>%
    ungroup() %>%
    filter(flag_magnitude) %>%
    select(sample_id, NewUID, ScientificName_WFO, all_of(group_var),
           all_of(trait_col), group_median, log10_dev)
}

height_magnitude_flags <- flag_magnitude_error(fieldtrait, "Family_WFO", "height_cm", log10_threshold = 2)
canopy_magnitude_flags <- flag_magnitude_error(fieldtrait, "Family_WFO", "canopy_area_cm2", log10_threshold = 2)

# ---- 11. Branch order validity ------------------------------------------
branch_order_flags <- fieldtrait %>%
  filter(!is.na(branch_order)) %>%
  mutate(flag_branch_order = branch_order %% 1 != 0 | branch_order < 0 | branch_order > 4) %>%
  filter(flag_branch_order) %>%
  select(sample_id, NewUID, ScientificName_WFO, branch_order)
# no branch order flags

# ---- 12. LES covariation check (N vs LMA), robust version --------------
# Pre-filter: exclude sample_ids already flagged as bad LMA (high-confidence
# only) by Workflow4, so they don't distort the fit before it even runs.
bad_lma_ids <- bad_lma %>%
  filter(confidence == "both") %>%
  pull(sample_id)

n_lma <- fieldtrait %>%
  select(sample_id, ScientificName_WFO, Family_WFO, percent_N) %>%
  inner_join(
    labtrait %>%
      filter(!sample_id %in% bad_lma_ids) %>%
      select(sample_id, replicate, lma),
    by = "sample_id"
  ) %>%
  filter(!is.na(percent_N), !is.na(lma), percent_N > 0, lma > 0)

# robust = TRUE uses Huber M-estimation to downweight leverage points
# rather than letting them determine the slope
sma_fit_robust <- sma(log10(percent_N) ~ log10(lma), data = n_lma, robust = TRUE)
summary(sma_fit_robust)

n_lma <- n_lma %>%
  mutate(
    log_N        = log10(percent_N),
    log_LMA      = log10(lma),
    sma_resid    = residuals(sma_fit_robust),
    sma_resid_z  = as.numeric(scale(sma_resid)),
    flag_covariation = abs(sma_resid_z) > 3
  )

possible_covar_outlier <- n_lma %>% filter(flag_covariation) %>% arrange(desc(abs(sma_resid_z)))
possible_covar_outlier

ggplot(n_lma, aes(log_LMA, log_N, color = flag_covariation)) +
  geom_point() +
  scale_color_manual(values = c("grey40", "red")) +
  labs(title = "percent_N vs LMA (log-log, robust SMA), flagged points in red")

# ---- 13. Write out flag tables -----------------------------------------
write_csv(chem_flags,             paste0(data_quality_path, 'bad_chemistry_values.csv'))
write_csv(canopy_geom_flags,      paste0(data_quality_path, 'bad_canopy_geometry_values.csv'))
write_csv(height_magnitude_flags, paste0(data_quality_path, 'bad_height_magnitude_values.csv'))
write_csv(canopy_magnitude_flags, paste0(data_quality_path, 'bad_canopy_magnitude_values.csv'))
write_csv(branch_order_flags,     paste0(data_quality_path, 'bad_branch_order_values.csv'))
write_csv(n_lma %>% filter(flag_covariation),
          paste0(data_quality_path, 'outlying_n_lma_covariation_values.csv'))

# ---- 14. Check overlap between flags ------------------------------------

field_flag_list <- list(
  chemistry        = chem_flags %>% rename(any_value = trait_flagged),
  canopy_geom      = canopy_geom_flags,
  height_magnitude = height_magnitude_flags,
  canopy_magnitude = canopy_magnitude_flags,
  branch_order     = branch_order_flags,
  n_lma_covariation = n_lma %>% filter(flag_covariation)
)

field_flag_overlap <- bind_rows(
  lapply(names(field_flag_list), function(nm) {
    field_flag_list[[nm]] %>%
      distinct(sample_id) %>%
      mutate(flag_source = nm)
  })
)

# Which sample_ids are flagged by more than one check?
field_flag_overlap_summary <- field_flag_overlap %>%
  distinct(sample_id, flag_source) %>%
  group_by(sample_id) %>%
  summarise(n_flags = n(), flags = paste(sort(flag_source), collapse = ", "), .groups = "drop") %>%
  arrange(desc(n_flags))

field_flag_overlap_summary %>% count(flags, sort = TRUE)


