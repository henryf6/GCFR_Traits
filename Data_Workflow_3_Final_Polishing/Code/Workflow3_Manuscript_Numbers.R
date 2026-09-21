########################################################
# Workflow3_Manuscript_Numbers.R
#
# Purpose: Compute every number quoted in the manuscript, response letter
# and DAAC documentation directly from the released data files, so each
# value can be traced to a visible R expression.
#
# Output: Manuscript_Numbers/manuscript_numbers.csv
#   columns: item, value, used_in, source_file, expression
#   (expression = the R code, as text, that produced the value)
# Also:   Manuscript_Numbers/manuscript_numbers_check.csv compares a
#   subset of items with the numbers Henry expected (see section P4).
#   The expected values are only used for that comparison; they never
#   feed into manuscript_numbers.csv.
#
# Inputs: the five released files in Data_Outputs/ and the files in
# Quality_Check_Outputs/. Two further input groups are used ONLY where
# the released files cannot answer the question (listed in P1):
#   - Intermediate_Outputs/ (pre-removal values, to count how many
#     values the outlier removal blanked)
#   - Provenance/ (recode logs, to count recoded species)
#
# Run from the DimensionsDataPaper folder (same convention as the other
# workflow scripts, paths start with 'GCFR_Traits/').
#
# Date Created: September 2026
# Author(s): Henry Frye, Claude
########################################################

library(tidyverse)

# ==== P. PARAMETERS (data-dependent values only; no computation) ============

# ---- P1. Paths and files ----------------------------------------------------
base_path    <- 'GCFR_Traits/Data_Workflow_3_Final_Polishing/'
out_path     <- paste0(base_path, 'Data_Outputs/')
qc_path      <- paste0(base_path, 'Quality_Check_Outputs/')
interm_path  <- paste0(base_path, 'Intermediate_Outputs/')
prov_path    <- paste0(base_path, 'Provenance/')
numbers_path <- paste0(base_path, 'Manuscript_Numbers/')

released_files <- c(canopy  = 'canopy_leaf_chemistry.csv',
                    leaf    = 'leaf_struct_water_traits.csv',
                    vnir    = 'vnir_spectra.csv',
                    species = 'species_traits.csv',
                    releve  = 'releve.csv')

qc_files <- c(bad_lma                 = 'bad_lma_values.csv',
              bad_fwc                 = 'bad_fwc_values.csv',
              bad_ldmc                = 'bad_ldmc_values.csv',
              bad_succulence          = 'bad_succulence_values.csv',
              bad_twig_fwc            = 'bad_twig_fwc_values.csv',
              bad_thickness           = 'bad_thickness_values.csv',
              flagged_struc           = 'flagged_struc_values.csv',
              bad_chemistry           = 'bad_chemistry_values.csv',
              bad_canopy_geometry     = 'bad_canopy_geometry_values.csv',
              bad_canopy_magnitude    = 'bad_canopy_magnitude_values.csv',
              bad_height_magnitude    = 'bad_height_magnitude_values.csv',
              bad_branch_order        = 'bad_branch_order_values.csv',
              n_lma_covariation       = 'outlying_n_lma_covariation_values.csv',
              zero_cover_recode_log   = 'zero_cover_recode_log.csv')

# Additional inputs (outside the released files / QC outputs):
interm_files <- c(leaf   = 'leaf_struc_intermediate.csv',    # values before outlier removal
                  canopy = 'canopy_chem_intermediate.csv')
prov_files   <- c(leaf_type_archive    = 'leaf_type_full_archive_and_removal_log.csv',
                  evergreen_deciduous_na       = 'evergreen_deciduous_na_recode_log.csv',
                  evergreen_deciduous_conflict = 'evergreen_deciduous_conflict_recode_log.csv')

# ---- P2. Column groups ------------------------------------------------------
# Trait-observation groups. These mirror the counting in
# Workflow4_Analysis.R (section "summary numbers"): leaf/twig = 15 structural
# columns in the leaf file + pubescence category from the canopy file;
# canopy structure = 3 columns; chemistry = 5 columns.
leaf_struc_cols   <- c('leaf_area_cm2', 'leaf_length_cm', 'avg_leaf_width_cm', 'max_leaf_width_cm',
                       'leaf_thickness_mm', 'leaf_fresh_wgt_g', 'leaf_dry_wgt_g', 'twig_fresh_g',
                       'twig_dry_g', 'lma', 'lwc', 'succulence', 'ldmc', 'lwr', 'twig_fwc')
canopy_struc_cols <- c('height_cm', 'canopy_cover_cm2', 'branch_order')
chem_cols         <- c('percent_N', 'percent_C', 'C_to_N_ratio', 'd_15N_14N', 'd_13C_12C')
pubescence_col    <- 'pubescence'

# Numeric trait columns summarized per column (section C)
canopy_trait_cols <- c('height_cm', 'canopy_axis_1_cm', 'canopy_axis_2_cm', 'canopy_cover_cm2',
                       'branch_order', chem_cols)
leaf_trait_cols   <- leaf_struc_cols

# Column names that differ between the intermediate files and the released files
leaf_interm_rename   <- c(lwc = 'fwc')                          # released name = intermediate name
canopy_interm_rename <- c(canopy_cover_cm2 = 'canopy_area_cm2')

# ---- P3. Other data-dependent settings --------------------------------------
family_of_interest <- 'Asteraceae'
cover_threshold    <- 100            # plot total cover above this counts as ">100%"
cape_point_name    <- 'cape_point'
collector_col      <- 'collector'    # trait sample_ID = <VNIR sample_ID>_<collector>
species_col        <- 'scientific_name_WFO'
family_col         <- 'family_WFO'

# Outlier removal, mirrors Workflow3_Data_Polish.R ("removal_map"):
lma_component_map <- c(weight = 'leaf_dry_wgt_g', area = 'leaf_area_cm2')
ratio_components  <- list(fwc        = c('leaf_fresh_wgt_g', 'leaf_dry_wgt_g'),
                          ldmc       = c('leaf_fresh_wgt_g', 'leaf_dry_wgt_g'),
                          succulence = c('leaf_fresh_wgt_g', 'leaf_area_cm2'),
                          twig_fwc   = c('twig_fresh_g', 'twig_dry_g'))
scanner_cols      <- c('leaf_length_cm', 'avg_leaf_width_cm', 'max_leaf_width_cm')  # follow leaf_area_cm2

# ---- P4. Expected values for the comparison file only -----------------------
# item name (as written in manuscript_numbers.csv), expected value, where it
# came from. tolerance: absolute difference allowed for numeric comparison.
expected <- tribble(
  ~item,                                             ~expected, ~from,
  'rows__canopy_leaf_chemistry',                     2509,      'Next up 11a / DAAC guide',
  'rows__leaf_struct_water_traits',                  9547,      'Next up 11a / DAAC guide',
  'rows__vnir_spectra',                              3076,      'Next up 11a / DAAC guide',
  'cols__vnir_spectra',                              511,       'Next up 11a / DAAC guide',
  'rows__species_traits',                            2225,      'Next up 11a / DAAC guide',
  'rows__releve',                                    44160,     'Next up 11a / DAAC guide',
  'trait_obs__leaf_twig',                            132199,    'Next up 11a (abstract)',
  'trait_obs__canopy_structure',                     7513,      'Next up 11a (abstract)',
  'trait_obs__chemistry',                            11948,     'Next up 11a (abstract)',
  'trait_obs__total',                                151660,    'Next up 11a (abstract)',
  'species__leaf_struct_water_traits',               1327,      'Next up 11b',
  'families__leaf_struct_water_traits',              99,        'Next up 11b',
  'species__canopy_leaf_chemistry',                  1327,      'Next up 11b',
  'families__canopy_leaf_chemistry',                 99,        'Next up 11b',
  'leaf__succulence__min',                           6.536,     'Next up 11c',
  'leaf__succulence__max',                           28299.531, 'Next up 11c',
  'leaf__succulence__n_blank',                       928,       'Next up 11c',
  'canopy__canopy_cover_cm2__min',                   0.442,     'Next up 11c',
  'canopy__canopy_cover_cm2__max',                   1431388.153, 'Next up 11c',
  'canopy__C_to_N_ratio__min',                       6.655,     'Next up 11c',
  'canopy__C_to_N_ratio__max',                       240.7,     'Next up 11c',
  'canopy__C_to_N_ratio__n_blank',                   120,       'Next up 11c',
  'quality_flag__canopy__canopy_magnitude',          98,        'Next up 11c',
  'leaf__leaf_area_cm2__blank_added_vs_intermediate',        111, 'Next up 11d',
  'leaf__leaf_length_cm__blank_added_vs_intermediate',       111, 'Next up 11d',
  'leaf__avg_leaf_width_cm__blank_added_vs_intermediate',    111, 'Next up 11d',
  'leaf__max_leaf_width_cm__blank_added_vs_intermediate',    111, 'Next up 11d',
  'leaf__leaf_thickness_mm__blank_added_vs_intermediate',    244, 'Next up 11d',
  'leaf__leaf_fresh_wgt_g__blank_added_vs_intermediate',     209, 'Next up 11d',
  'leaf__leaf_dry_wgt_g__blank_added_vs_intermediate',       172, 'Next up 11d',
  'leaf__twig_fresh_g__blank_added_vs_intermediate',         135, 'Next up 11d',
  'leaf__twig_dry_g__blank_added_vs_intermediate',           177, 'Next up 11d',
  'leaf__lma__blank_added_vs_intermediate',                  266, 'Next up 11d',
  'leaf__lwc__blank_added_vs_intermediate',                  322, 'Next up 11d',
  'leaf__twig_fwc__blank_added_vs_intermediate',             282, 'Next up 11d',
  'releve__share_plots_cover_over_threshold_pct_all',        31.9, 'Next up 11e (about)',
  'vnir__spectra__baviaanskloof',                    1243,      'Next up 11f',
  'vnir__spectra__cape_point',                       462,       'Next up 11f',
  'vnir__spectra__hangklip',                         350,       'Next up 11f',
  'vnir__spectra__langeberg',                        296,       'Next up 11f',
  'vnir__spectra__cederberg',                        280,       'Next up 11f',
  'vnir__spectra__htr',                              445,       'Next up 11f',
  'vnir__link_canopy__one_trait_sample',             2836,      'Next up 11f',
  'vnir__link_canopy__two_trait_samples',            240,       'Next up 11f',
  'vnir__link_canopy__one_trait_sample_species_differ', 59,     'pass doc (spectra-to-trait linking)',
  'locations__unique_lat_lon__canopy_leaf_vnir',     448,       'pass doc / README (was 445)',
  'releve__surveys__cape_point__1966',               81,        'pass doc',
  'releve__surveys__cape_point__1996',               81,        'pass doc',
  'releve__surveys__cape_point__2010',               67,        'pass doc',
  'qc__zero_cover_recode_log__rows',                 14,        'pass doc / response letter',
  'releve__records_percent_cover_below_0.1',         354,       'pass doc (354 = 304 + 50)',
  'share_rows_Asteraceae_pct__canopy__cederberg',    14,        'manuscript (rounded to integer)',
  'share_rows_Asteraceae_pct__canopy__htr',          32,        'manuscript (rounded to integer)',
  'share_rows_Asteraceae_pct__canopy__cederberg',    14.9,      'response letter (about)',
  'share_rows_Asteraceae_pct__canopy__htr',          33.3,      'response letter (about)'
)
tolerance      <- 0.0005    # numeric comparison tolerance (values are reported to 3 decimals)
tolerance_about <- 0.05     # for items expected only "about" (rounded to 1 decimal)


# ==== READ DATA ==============================================================
canopy   <- read_csv(paste0(out_path, released_files[['canopy']]),  show_col_types = FALSE)
leafstruc <- read_csv(paste0(out_path, released_files[['leaf']]),   show_col_types = FALSE)
vnir     <- read_csv(paste0(out_path, released_files[['vnir']]),    show_col_types = FALSE)
species  <- read_csv(paste0(out_path, released_files[['species']]), show_col_types = FALSE)
releve   <- read_csv(paste0(out_path, released_files[['releve']]),  show_col_types = FALSE)

# Replicate labels include letters (e.g. '1C', '4l'), so read `replicate` as
# character wherever it exists, as Workflow3_Data_Polish.R does.
read_qc <- function(file) {
  has_rep <- 'replicate' %in% names(read_csv(file, n_max = 0, show_col_types = FALSE))
  if (has_rep) read_csv(file, col_types = cols(replicate = col_character()))
  else read_csv(file, show_col_types = FALSE)
}
qc <- map(qc_files, ~ read_qc(paste0(qc_path, .x)))
interm <- map(interm_files, ~ read_csv(paste0(interm_path, .x), show_col_types = FALSE))
prov   <- map(prov_files,   ~ read_csv(paste0(prov_path, .x),   show_col_types = FALSE))

# Alignment guard: the intermediate files must have the same rows as the
# released files for the before/after blank counts to be meaningful.
stopifnot(nrow(interm$leaf)   == nrow(leafstruc),
          nrow(interm$canopy) == nrow(canopy))


# ==== HELPERS ================================================================
items <- list()

# record(): evaluates `expr`, stores value + the expression text, prints a line
record_expr <- function(item, e, used_in, source_file, env = globalenv()) {
  val <- eval(e, env)
  stopifnot(length(val) == 1)
  txt <- paste(trimws(deparse(e, width.cutoff = 500L)), collapse = ' ')
  items[[length(items) + 1]] <<- tibble(item = item, value = as.character(val),
                                        used_in = used_in, source_file = source_file,
                                        expression = txt)
  shown <- if (is.numeric(val)) format(val, big.mark = ',', scientific = FALSE, trim = TRUE) else val
  cat(sprintf('%-62s %s\n', item, shown))
}
record <- function(item, expr, used_in, source_file) {
  record_expr(item, substitute(expr), used_in, source_file)
}

# Number of observations = non-missing cells across a set of columns
count_obs <- function(df, cols) sum(!is.na(as.matrix(df[cols])))

# Quality flags are semicolon-separated; count rows containing one category
flag_has <- function(flags, category) {
  sum(vapply(strsplit(flags, ';\\s*'), function(x) category %in% trimws(x), logical(1)))
}
flag_categories <- function(flags) {
  sort(unique(trimws(unlist(strsplit(na.omit(flags), ';')))))
}

# Species / Asteraceae pooled across the leaf and canopy trait files
pooled_species <- function(fam = NULL) {
  d <- bind_rows(leafstruc[c(species_col, family_col)], canopy[c(species_col, family_col)])
  if (!is.null(fam)) d <- d[which(d[[family_col]] == fam), ]
  n_distinct(d[[species_col]], na.rm = TRUE)
}

# Rebuild the outlier removal map from the QC files exactly as
# Workflow3_Data_Polish.R does (sections "NA out measurements ..."), so the
# per-column counts can be checked against the released data.
build_removal_map <- function(qc) {
  expand_ratio_flags <- function(df, comp1, comp2) {
    df <- df %>% distinct(sample_id, replicate, suspected_bad)
    bind_rows(
      df %>% filter(suspected_bad != 'ambiguous') %>%
        transmute(sample_id, replicate, raw_col = suspected_bad),
      df %>% filter(suspected_bad == 'ambiguous') %>%
        tidyr::crossing(raw_col = c(comp1, comp2)) %>%
        transmute(sample_id, replicate, raw_col))
  }
  lma_df <- qc$bad_lma %>% distinct(sample_id, replicate, suspected_bad)
  rm_lma <- bind_rows(
    lma_df %>% filter(suspected_bad != 'ambiguous') %>%
      transmute(sample_id, replicate, raw_col = unname(lma_component_map[suspected_bad])),
    lma_df %>% filter(suspected_bad == 'ambiguous') %>%
      tidyr::crossing(raw_col = unname(lma_component_map)) %>%
      transmute(sample_id, replicate, raw_col))
  rm_ratio <- imap(ratio_components, ~ expand_ratio_flags(qc[[paste0('bad_', .y)]], .x[1], .x[2]))
  rm_thick <- qc$bad_thickness %>% distinct(sample_id, replicate) %>%
    mutate(raw_col = 'leaf_thickness_mm')
  rm_all <- bind_rows(rm_lma, !!!rm_ratio, rm_thick) %>%
    rename(sample_ID = sample_id) %>%
    distinct(sample_ID, replicate, raw_col)
  area_keys <- rm_all %>% filter(raw_col == 'leaf_area_cm2') %>% distinct(sample_ID, replicate)
  bind_rows(rm_all, tidyr::crossing(area_keys, raw_col = scanner_cols)) %>%
    distinct(sample_ID, replicate, raw_col)
}
removal_map <- build_removal_map(qc)
leaf_row_key <- paste(leafstruc$sample_ID, leafstruc$replicate)

# Plot-level (one row per survey) table for the relevé numbers
releve_plots <- releve %>%
  distinct(releve_ID, plot, year, subregion, cover_method, plot_percent_cover)
stopifnot(!anyDuplicated(releve_plots$releve_ID))

# VNIR -> trait sample link: trait sample_ID = VNIR sample_ID + '_' + collector.
# Count how many distinct trait sample_IDs each VNIR spectrum links to.
link_counts <- function(trait_df) {
  trait_df %>%
    mutate(link_key = str_remove(sample_ID, paste0('_', .data[[collector_col]], '$'))) %>%
    distinct(link_key, sample_ID) %>%
    count(link_key, name = 'n_trait_samples')
}
vnir_link_canopy <- vnir %>%
  select(sample_ID) %>%
  left_join(link_counts(canopy), by = c('sample_ID' = 'link_key')) %>%
  mutate(n_trait_samples = coalesce(n_trait_samples, 0L))
vnir_link_leaf <- vnir %>%
  select(sample_ID) %>%
  left_join(link_counts(leafstruc), by = c('sample_ID' = 'link_key')) %>%
  mutate(n_trait_samples = coalesce(n_trait_samples, 0L))
stopifnot(nrow(vnir_link_canopy) == nrow(vnir), nrow(vnir_link_leaf) == nrow(vnir))

# Spectra that link to exactly one canopy-file trait sample: do the species names agree?
canopy_keyed <- canopy %>%
  mutate(link_key = str_remove(sample_ID, paste0('_', .data[[collector_col]], '$')))
canopy_one_match <- canopy_keyed %>% count(link_key) %>% filter(n == 1) %>% pull(link_key)
vnir_species_check <- vnir %>%
  transmute(link_key = sample_ID, vnir_species = .data[[species_col]]) %>%
  inner_join(canopy_keyed %>% filter(link_key %in% canopy_one_match) %>%
               transmute(link_key, trait_species = .data[[species_col]]),
             by = 'link_key')

# Unique sampling locations across the three sample-level files
locations <- bind_rows(canopy[c('latitude', 'longitude')], leafstruc[c('latitude', 'longitude')],
                       vnir[c('latitude', 'longitude')]) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct()


# ==== A. Row counts and trait-observation counts =============================
cat('\n---- A. Row counts and trait observations ----\n')
record('rows__canopy_leaf_chemistry', nrow(canopy),    'Table 1 / DAAC doc', 'Data_Outputs/canopy_leaf_chemistry.csv')
record('rows__leaf_struct_water_traits', nrow(leafstruc), 'Table 1 / DAAC doc', 'Data_Outputs/leaf_struct_water_traits.csv')
record('rows__vnir_spectra', nrow(vnir),               'Table 1 / DAAC doc', 'Data_Outputs/vnir_spectra.csv')
record('cols__vnir_spectra', ncol(vnir),               'Table 1 / DAAC doc', 'Data_Outputs/vnir_spectra.csv')
record('rows__species_traits', nrow(species),          'Table 1 / DAAC doc', 'Data_Outputs/species_traits.csv')
record('rows__releve', nrow(releve),                   'Table 1 / DAAC doc', 'Data_Outputs/releve.csv')

record('trait_obs__leaf_twig',
       count_obs(leafstruc, leaf_struc_cols) + sum(!is.na(canopy[[pubescence_col]])),
       'abstract', 'Data_Outputs/leaf_struct_water_traits.csv; canopy_leaf_chemistry.csv')
record('trait_obs__canopy_structure', count_obs(canopy, canopy_struc_cols),
       'abstract', 'Data_Outputs/canopy_leaf_chemistry.csv')
record('trait_obs__chemistry', count_obs(canopy, chem_cols),
       'abstract', 'Data_Outputs/canopy_leaf_chemistry.csv')
record('trait_obs__total',
       count_obs(leafstruc, leaf_struc_cols) + sum(!is.na(canopy[[pubescence_col]])) +
         count_obs(canopy, canopy_struc_cols) + count_obs(canopy, chem_cols),
       'abstract', 'Data_Outputs/leaf_struct_water_traits.csv; canopy_leaf_chemistry.csv')


# ==== B. Species, families, Asteraceae =======================================
cat('\n---- B. Species and families ----\n')
species_sources <- list(leaf_struct_water_traits = 'leafstruc', canopy_leaf_chemistry = 'canopy',
                        vnir_spectra = 'vnir', species_traits = 'species', releve = 'releve')
for (nm in names(species_sources)) {
  ds <- as.name(species_sources[[nm]])
  src <- paste0('Data_Outputs/', nm, '.csv')
  record_expr(paste0('species__', nm), bquote(n_distinct(.(ds)[[species_col]], na.rm = TRUE)),
              'abstract / Results / DAAC doc', src)
  record_expr(paste0('families__', nm), bquote(n_distinct(.(ds)[[family_col]], na.rm = TRUE)),
              'abstract / Results / DAAC doc', src)
  record_expr(paste0('missing_species_name__', nm), bquote(sum(is.na(.(ds)[[species_col]]))),
              'Methods (check)', src)
}
record('species__leaf_and_canopy_pooled', pooled_species(),
       'abstract / Results', 'Data_Outputs/leaf_struct_water_traits.csv; canopy_leaf_chemistry.csv')
record(paste0('species_', family_of_interest, '__leaf_and_canopy_pooled'), pooled_species(family_of_interest),
       'Results / response letter', 'Data_Outputs/leaf_struct_water_traits.csv; canopy_leaf_chemistry.csv')
record(paste0('share_species_', family_of_interest, '_pct__leaf_and_canopy_pooled'),
       100 * pooled_species(family_of_interest) / pooled_species(),
       'Results / response letter', 'Data_Outputs/leaf_struct_water_traits.csv; canopy_leaf_chemistry.csv')
for (nm in names(species_sources)) {
  ds <- as.name(species_sources[[nm]])
  src <- paste0('Data_Outputs/', nm, '.csv')
  record_expr(paste0('share_species_', family_of_interest, '_pct__', nm),
              bquote(100 * n_distinct(.(ds)[[species_col]][which(.(ds)[[family_col]] == family_of_interest)], na.rm = TRUE) /
                       n_distinct(.(ds)[[species_col]], na.rm = TRUE)),
              'Results / response letter', src)
  record_expr(paste0('share_rows_', family_of_interest, '_pct__', nm),
              bquote(100 * sum(.(ds)[[family_col]] == family_of_interest, na.rm = TRUE) / nrow(.(ds))),
              'Results / response letter', src)
}

record('locations__unique_lat_lon__canopy_leaf_vnir', nrow(locations), 'Methods / response letter',
       'Data_Outputs/canopy_leaf_chemistry.csv; leaf_struct_water_traits.csv; vnir_spectra.csv')
for (nm in c('canopy', 'leafstruc')) {
  ds <- as.name(nm)
  src <- paste0('Data_Outputs/', if (nm == 'canopy') released_files[['canopy']] else released_files[['leaf']])
  for (sr in sort(unique(get(nm)$subregion))) {
    record_expr(paste0('share_rows_', family_of_interest, '_pct__', nm, '__', sr),
                bquote(100 * sum(.(ds)[[family_col]][.(ds)$subregion == .(sr)] == family_of_interest, na.rm = TRUE) /
                         sum(.(ds)$subregion == .(sr))),
                'Results / response letter', src)
    record_expr(paste0('share_species_', family_of_interest, '_pct__', nm, '__', sr),
                bquote(100 * n_distinct(.(ds)[[species_col]][.(ds)$subregion == .(sr) & .(ds)[[family_col]] == family_of_interest], na.rm = TRUE) /
                         n_distinct(.(ds)[[species_col]][.(ds)$subregion == .(sr)], na.rm = TRUE)),
                'Results / response letter', src)
  }
}


# ==== C. Per-trait summaries and quality flags ===============================
cat('\n---- C. Trait ranges, blanks, quality flags ----\n')
trait_sets <- list(canopy = list(df = 'canopy',    cols = canopy_trait_cols, file = 'canopy_leaf_chemistry.csv',
                                 interm = 'canopy', rename = canopy_interm_rename),
                   leaf   = list(df = 'leafstruc', cols = leaf_trait_cols,   file = 'leaf_struct_water_traits.csv',
                                 interm = 'leaf',   rename = leaf_interm_rename))
for (set in names(trait_sets)) {
  ts  <- trait_sets[[set]]
  ds  <- as.name(ts$df)
  src <- paste0('Data_Outputs/', ts$file)
  for (tr in ts$cols) {
    pre <- paste0(set, '__', tr, '__')
    record_expr(paste0(pre, 'n_nonblank'), bquote(sum(!is.na(.(ds)[[.(tr)]]))), 'Table 2-3 / DAAC doc', src)
    record_expr(paste0(pre, 'min'),        bquote(min(.(ds)[[.(tr)]], na.rm = TRUE)), 'Table 2-3 / DAAC doc', src)
    record_expr(paste0(pre, 'max'),        bquote(max(.(ds)[[.(tr)]], na.rm = TRUE)), 'Table 2-3 / DAAC doc', src)
    record_expr(paste0(pre, 'n_blank'),    bquote(sum(is.na(.(ds)[[.(tr)]]))),        'Table 2-3 / DAAC doc', src)

    # blanks added between the intermediate (pre-removal) file and the release
    int_col <- if (tr %in% names(ts$rename)) ts$rename[[tr]] else tr
    if (int_col %in% names(interm[[ts$interm]])) {
      idf <- bquote(interm[[.(ts$interm)]])
      record_expr(paste0(pre, 'blank_added_vs_intermediate'),
                  bquote(sum(is.na(.(ds)[[.(tr)]])) - sum(is.na(.(idf)[[.(int_col)]]))),
                  'Methods / Usage Notes / response letter',
                  paste0('Data_Outputs/', ts$file, '; Intermediate_Outputs/', interm_files[[ts$interm]]))
    }
  }
}

# pubescence is categorical
for (lv in sort(unique(na.omit(canopy[[pubescence_col]])))) {
  record_expr(paste0('canopy__pubescence__n_', lv), bquote(sum(canopy[[pubescence_col]] == .(lv), na.rm = TRUE)),
              'Table 2 / DAAC doc', 'Data_Outputs/canopy_leaf_chemistry.csv')
}
record('canopy__pubescence__n_blank', sum(is.na(canopy[[pubescence_col]])),
       'Table 2 / DAAC doc', 'Data_Outputs/canopy_leaf_chemistry.csv')

# quality_flag categories (semicolon-separated, so one row can carry several)
for (set in names(trait_sets)) {
  ts  <- trait_sets[[set]]
  ds  <- as.name(ts$df)
  src <- paste0('Data_Outputs/', ts$file)
  record_expr(paste0('quality_flag__', set, '__rows_with_any_flag'), bquote(sum(!is.na(.(ds)$quality_flag))),
              'Methods / Usage Notes / DAAC doc', src)
  for (cat_name in flag_categories(eval(bquote(.(ds)$quality_flag)))) {
    record_expr(paste0('quality_flag__', set, '__', cat_name),
                bquote(flag_has(.(ds)$quality_flag[!is.na(.(ds)$quality_flag)], .(cat_name))),
                'Methods / Usage Notes / DAAC doc', src)
  }
}


# ==== D. Outlier QC counts ===================================================
cat('\n---- D. Outlier QC counts ----\n')
for (nm in names(qc_files)) {
  d <- as.name(nm)
  src <- paste0('Quality_Check_Outputs/', qc_files[[nm]])
  record_expr(paste0('qc__', nm, '__rows'), bquote(nrow(qc[[.(nm)]])), 'Methods / Usage Notes / response letter', src)
  if (all(c('sample_id', 'replicate') %in% names(qc[[nm]]))) {
    record_expr(paste0('qc__', nm, '__distinct_sample_replicates'),
                bquote(n_distinct(paste(qc[[.(nm)]]$sample_id, qc[[.(nm)]]$replicate))),
                'Methods / Usage Notes / response letter', src)
  }
  if ('sample_id' %in% names(qc[[nm]])) {
    record_expr(paste0('qc__', nm, '__distinct_samples'), bquote(n_distinct(qc[[.(nm)]]$sample_id)),
                'Methods / Usage Notes / response letter', src)
  }
  for (colname in intersect(c('confidence', 'suspected_bad', 'trait_flagged'), names(qc[[nm]]))) {
    for (lv in sort(unique(na.omit(qc[[nm]][[colname]])))) {
      record_expr(paste0('qc__', nm, '__', colname, '_', lv),
                  bquote(sum(qc[[.(nm)]][[.(colname)]] == .(lv), na.rm = TRUE)),
                  'Methods / Usage Notes / response letter', src)
    }
  }
}

# Removal map rebuilt from the QC files (mirrors Workflow3_Data_Polish.R):
# distinct sample-replicates removed per raw column, and a check that those
# values really are blank in the released leaf file (should be 0).
for (col in sort(unique(removal_map$raw_col))) {
  record_expr(paste0('removal_map__', col, '__sample_replicates'),
              bquote(sum(removal_map$raw_col == .(col))),
              'Methods / Usage Notes / response letter',
              'Quality_Check_Outputs/bad_*_values.csv (rebuilt as in Workflow3_Data_Polish.R)')
  record_expr(paste0('removal_map__', col, '__still_present_in_release'),
              bquote(sum(!is.na(leafstruc[[.(col)]][leaf_row_key %in% paste(removal_map$sample_ID[removal_map$raw_col == .(col)],
                                                                          removal_map$replicate[removal_map$raw_col == .(col)])]))),
              'check (expect 0)', 'Data_Outputs/leaf_struct_water_traits.csv')
}

# Key-alignment checks between the removal map and the released leaf file.
# A key is sample_ID + replicate. If the release holds repeated replicate
# labels within a sample, one key blanks several rows; if the QC files use
# different (resequenced) labels, some keys match no row at all.
record('release_leaf__rows_sharing_a_sample_replicate_key',
       sum(duplicated(leaf_row_key) | duplicated(leaf_row_key, fromLast = TRUE)),
       'check', 'Data_Outputs/leaf_struct_water_traits.csv')
record('release_leaf__sample_IDs_with_repeated_replicate_labels',
       n_distinct(leafstruc$sample_ID[duplicated(leaf_row_key) | duplicated(leaf_row_key, fromLast = TRUE)]),
       'check', 'Data_Outputs/leaf_struct_water_traits.csv')
record('removal_map__distinct_keys', n_distinct(paste(removal_map$sample_ID, removal_map$replicate)),
       'check', 'Quality_Check_Outputs/bad_*_values.csv')
record('removal_map__keys_matching_no_release_row',
       sum(!unique(paste(removal_map$sample_ID, removal_map$replicate)) %in% leaf_row_key),
       'check', 'Quality_Check_Outputs/bad_*_values.csv; Data_Outputs/leaf_struct_water_traits.csv')


# ==== E. Relevé ==============================================================
cat('\n---- E. Relevé ----\n')
rel_src <- 'Data_Outputs/releve.csv'
record('releve__surveys_releve_ID', n_distinct(releve$releve_ID), 'abstract / Table 1 / Results', rel_src)
record('releve__distinct_plot_ids', n_distinct(releve$plot), 'Results / Methods', rel_src)
record('releve__plots_with_total_cover', sum(!is.na(releve_plots$plot_percent_cover)), 'Results / response letter', rel_src)
record('releve__plots_without_total_cover', sum(is.na(releve_plots$plot_percent_cover)), 'Results / response letter', rel_src)
record('releve__plots_cover_over_threshold', sum(releve_plots$plot_percent_cover > cover_threshold, na.rm = TRUE),
       'Results / response letter', rel_src)
record('releve__share_plots_cover_over_threshold_pct_all',
       100 * sum(releve_plots$plot_percent_cover > cover_threshold, na.rm = TRUE) / nrow(releve_plots),
       'Results / response letter', rel_src)
record('releve__share_plots_cover_over_threshold_pct_with_cover',
       100 * sum(releve_plots$plot_percent_cover > cover_threshold, na.rm = TRUE) / sum(!is.na(releve_plots$plot_percent_cover)),
       'Results / response letter', rel_src)
record('releve__max_plot_percent_cover', max(releve_plots$plot_percent_cover, na.rm = TRUE), 'Results / response letter', rel_src)
for (cm in sort(unique(na.omit(releve_plots$cover_method)))) {
  record_expr(paste0('releve__plots_cover_method__', cm), bquote(sum(releve_plots$cover_method == .(cm), na.rm = TRUE)),
              'Methods / Data Records / DAAC doc', rel_src)
}
record('releve__abundance_class_values', paste(sort(unique(na.omit(releve$abundance_class))), collapse = ', '),
       'Methods / DAAC doc', rel_src)
record('releve__records_with_abundance_class', sum(!is.na(releve$abundance_class)), 'Methods / DAAC doc', rel_src)
record('releve__records_with_percent_cover', sum(!is.na(releve$percent_cover)), 'Methods / DAAC doc', rel_src)

cape_point <- releve %>% filter(subregion == cape_point_name)
for (yr in sort(unique(cape_point$year))) {
  record_expr(paste0('releve__cape_point__', yr, '__surveys'), bquote(n_distinct(cape_point$releve_ID[cape_point$year == .(yr)])),
              'Results / response letter', rel_src)
  record_expr(paste0('releve__cape_point__', yr, '__species_records'), bquote(sum(cape_point$year == .(yr))),
              'Results / response letter', rel_src)
  record_expr(paste0('releve__cape_point__', yr, '__records_with_percent_cover'),
              bquote(sum(!is.na(cape_point$percent_cover[cape_point$year == .(yr)]))),
              'Results / response letter', rel_src)
  record_expr(paste0('releve__cape_point__', yr, '__cover_method'),
              bquote(paste(sort(unique(cape_point$cover_method[cape_point$year == .(yr)])), collapse = ' + ')),
              'Methods / response letter', rel_src)
}
record('releve__records_percent_cover_below_0.1', sum(releve$percent_cover < 0.1, na.rm = TRUE),
       'response letter', rel_src)
record('releve__records_percent_cover_equal_0.1', sum(releve$percent_cover == 0.1, na.rm = TRUE),
       'response letter', rel_src)
record('releve__min_percent_cover', min(releve$percent_cover, na.rm = TRUE), 'response letter', rel_src)
for (sr in sort(unique(releve_plots$subregion))) {
  for (yr in sort(unique(releve_plots$year[releve_plots$subregion == sr]))) {
    record_expr(paste0('releve__surveys__', sr, '__', yr),
                bquote(sum(releve_plots$subregion == .(sr) & releve_plots$year == .(yr))),
                'Methods / Results', rel_src)
  }
}
record('releve__cape_point__max_percent_cover', max(cape_point$percent_cover, na.rm = TRUE), 'Results / response letter', rel_src)
record('releve__cape_point__max_plot_percent_cover', max(cape_point$plot_percent_cover, na.rm = TRUE), 'Results / response letter', rel_src)


# ==== F. VNIR ================================================================
cat('\n---- F. VNIR ----\n')
vn_src <- 'Data_Outputs/vnir_spectra.csv'
for (sr in sort(unique(vnir$subregion))) {
  record_expr(paste0('vnir__spectra__', sr), bquote(sum(vnir$subregion == .(sr))), 'Table 1 / Data Records / DAAC doc', vn_src)
}
record('vnir__distinct_sample_ID', n_distinct(vnir$sample_ID), 'Data Records / DAAC doc', vn_src)
record('vnir__sample_ID_range_first', range(vnir$sample_ID)[1], 'DAAC doc', vn_src)
record('vnir__sample_ID_range_last',  range(vnir$sample_ID)[2], 'DAAC doc', vn_src)
record('vnir__wavelength_first_col',  grep('^X[0-9]+$', names(vnir), value = TRUE)[1], 'Methods / DAAC doc', vn_src)
record('vnir__wavelength_last_col',   rev(grep('^X[0-9]+$', names(vnir), value = TRUE))[1], 'Methods / DAAC doc', vn_src)
for (nm in c('canopy', 'leaf')) {
  lk <- as.name(paste0('vnir_link_', nm))
  src <- paste0(vn_src, '; Data_Outputs/', released_files[[nm]])
  record_expr(paste0('vnir__link_', nm, '__no_trait_sample'),   bquote(sum(.(lk)$n_trait_samples == 0)), 'Data Records / response letter', src)
  record_expr(paste0('vnir__link_', nm, '__one_trait_sample'),  bquote(sum(.(lk)$n_trait_samples == 1)), 'Data Records / response letter', src)
  record_expr(paste0('vnir__link_', nm, '__two_trait_samples'), bquote(sum(.(lk)$n_trait_samples == 2)), 'Data Records / response letter', src)
  record_expr(paste0('vnir__link_', nm, '__more_than_two_trait_samples'), bquote(sum(.(lk)$n_trait_samples > 2)), 'Data Records / response letter', src)
}

record('vnir__link_canopy__one_trait_sample_species_agree',
       sum(vnir_species_check$vnir_species == vnir_species_check$trait_species, na.rm = TRUE),
       'Usage Notes / response letter', 'Data_Outputs/vnir_spectra.csv; canopy_leaf_chemistry.csv')
record('vnir__link_canopy__one_trait_sample_species_differ',
       sum(vnir_species_check$vnir_species != vnir_species_check$trait_species, na.rm = TRUE),
       'Usage Notes / response letter', 'Data_Outputs/vnir_spectra.csv; canopy_leaf_chemistry.csv')


# ==== G. species_traits: leaf categories and recodes =========================
cat('\n---- G. species_traits leaf categories and recodes ----\n')
st_src <- 'Data_Outputs/species_traits.csv'
# leaf_type was retired from the release; the archived pre-retirement values
# live in the provenance log.
for (lv in sort(unique(na.omit(prov$leaf_type_archive$leaf_type)))) {
  record_expr(paste0('leaf_type_archived__', lv), bquote(sum(prov$leaf_type_archive$leaf_type == .(lv), na.rm = TRUE)),
              'Methods / response letter', 'Provenance/leaf_type_full_archive_and_removal_log.csv')
}
record('leaf_type_archived__rows', nrow(prov$leaf_type_archive), 'Methods / response letter',
       'Provenance/leaf_type_full_archive_and_removal_log.csv')
for (lv in sort(unique(na.omit(species$functional_leaf)))) {
  record_expr(paste0('functional_leaf__', lv), bquote(sum(species$functional_leaf == .(lv), na.rm = TRUE)),
              'Methods / Table 4 / DAAC doc', st_src)
}
record('functional_leaf__blank', sum(is.na(species$functional_leaf)), 'Methods / Table 4 / DAAC doc', st_src)
for (fl in c('lvs_frond', 'lvs_cladode', 'lvs_phyllode', 'lvs_microphyll', 'virtually_no_leaves')) {
  record_expr(paste0('species_traits__', fl, '__n_1'), bquote(sum(species[[.(fl)]] == 1, na.rm = TRUE)),
              'Methods / Table 4 / DAAC doc', st_src)
}
record('species_traits__evergreen_1', sum(species$evergreen == 1, na.rm = TRUE), 'Methods / response letter', st_src)
record('species_traits__deciduous_1', sum(species$deciduous == 1, na.rm = TRUE), 'Methods / response letter', st_src)
record('species_traits__evergreen_and_deciduous_both_1', sum(species$evergreen == 1 & species$deciduous == 1, na.rm = TRUE),
       'Methods / response letter', st_src)
record('species_traits__evergreen_and_deciduous_both_0', sum(species$evergreen == 0 & species$deciduous == 0, na.rm = TRUE),
       'Methods / response letter', st_src)
record('species_traits__evergreen_and_deciduous_both_blank', sum(is.na(species$evergreen) & is.na(species$deciduous)),
       'Methods / response letter', st_src)
record('evergreen_deciduous__recoded_0_0_to_blank', nrow(prov$evergreen_deciduous_na),
       'Methods / response letter', 'Provenance/evergreen_deciduous_na_recode_log.csv')
record('evergreen_deciduous__conflict_1_1_recoded', nrow(prov$evergreen_deciduous_conflict),
       'Methods / response letter', 'Provenance/evergreen_deciduous_conflict_recode_log.csv')


# ==== OUTPUT =================================================================
dir.create(numbers_path, showWarnings = FALSE, recursive = TRUE)
numbers <- bind_rows(items)
stopifnot(!anyDuplicated(numbers$item))
write_csv(numbers, paste0(numbers_path, 'manuscript_numbers.csv'))

# Comparison against the numbers Henry expected (items not found are reported too)
check <- expected %>%
  left_join(numbers %>% select(item, value), by = 'item') %>%
  mutate(value_num = suppressWarnings(as.numeric(value)),
         tol = case_when(str_detect(from, 'integer') ~ 0.5,
                         str_detect(from, 'about')   ~ tolerance_about,
                         TRUE                        ~ tolerance),
         status = case_when(is.na(value)  ~ 'ITEM NOT COMPUTED',
                            abs(value_num - expected) <= tol ~ 'matches',
                            TRUE ~ 'DIFFERS')) %>%
  select(item, value, expected, status, from)
write_csv(check, paste0(numbers_path, 'manuscript_numbers_check.csv'))

cat('\n---- Comparison with expected values ----\n')
cat(sprintf('%d of %d expected items match\n', sum(check$status == 'matches'), nrow(check)))
print(check %>% filter(status != 'matches'), n = Inf)
cat(sprintf('\nWrote %d items to %smanuscript_numbers.csv\n', nrow(numbers), numbers_path))
