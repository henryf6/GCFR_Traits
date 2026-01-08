########################################################
# Workflow4_Analysis.R
#
# Purpose: Make summary figures describing data
#
# Date Created: July 2025
# Most recent modification: 
# Author(s): Henry Frye, Copilot
########################################################

# Read libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(patchwork)
library(forcats)
library(sf)

# Read in data
leafstruc <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/leaf_struct_water_traits.csv')
chem_canop <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/canopy_leaf_chemistry.csv') 
vnir <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/vnir_spectra.csv')
releve <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/releve.csv')
spectrait <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/species_traits.csv')

# create figure path
figpath <- 'GCFR_Traits/Data_Workflow_4_Figure_Creation/Figures/'

# Figures for leafstruc data ####

# re-level the subregion categories for nice display
leafstruc$subregion <- factor(x = leafstruc$subregion, levels = sort(unique(leafstruc$subregion)))
subregion_labels = c("Baviaanskloof", "Hantam-Tanqua-\nRoggeveld", "Hangklip", "Langeberg", "Cederberg", "Cape Point")


leafstruc <- leafstruc %>% rename('Subregion' = 'subregion') 
       
# Percent N distribution by subregion
N_plot <- ggplot(leafstruc, aes(x = percent_N, color = Subregion)) + geom_density(linewidth =1) + theme_classic() +
  xlab('Percent Nitrogen') +
  scale_color_brewer(palette = "Dark2") +
  ylab('Density')
N_plot

# Percent Carbon distribution by subregion
C_plot<- ggplot(leafstruc, aes(x = percent_C, color = Subregion)) + 
  geom_density(linewidth =1)+ theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  xlab('Percent Carbon') +
  ylab('Density')
C_plot

# N isotope distribution by subregion
N_iso <- ggplot(leafstruc, aes(x = d_15N_14N, color = Subregion)) + geom_density(linewidth =1) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  ylab('Density') +
  xlab(expression(delta^15 * "Nitrogen ("*"\u2030"*")"))
N_iso

# C isotope distribution by subregion
C_iso <- ggplot(leafstruc, aes(x = d_13C_12C, color = Subregion)) + geom_density(linewidth =1) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  ylab('Density') +
  xlab(expression(delta^13 * "Carbon ("*"\u2030"*")"))

C_iso

# Combine plots into a single figure
combined_foliar_chem <-ggarrange(N_plot, C_plot, N_iso, C_iso, labels = c('A','B','C','D'), ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "right"
) + theme(text = element_text(size = 12, family = "Arial"))
combined_foliar_chem

# Save as tiff and jpeg
ggsave(filename = paste0(figpath, 'foliar_summary_plot.tiff'), plot = combined_foliar_chem, width = 8, dpi = 300, units = "in",
       bg= 'white')
ggsave(filename = paste0(figpath, 'foliar_summary_plot.jpeg'), plot = combined_foliar_chem, width = 8, dpi = 300, units = "in",
       bg= 'white')


# Figures for chem_canop data ####

# Re-level the subregion categories for graphic display
chem_canop$subregion <- factor(x = chem_canop$subregion, levels = sort(unique(chem_canop$subregion)),
                          labels = c("Baviaanskloof","Cape Point","Cederberg", "Hangklip", "Hantam-Tanqua-\nRoggeveld", "Langeberg"))

chem_canop <- chem_canop %>% rename('Subregion' = 'subregion')

# Leaf mass per area distribution by region
lma <- ggplot(chem_canop, aes(x = lma, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Leaf mass per area') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,0.1) +
  ylab('Density')
lma

# Leaf water content distribution by region
lwc <- ggplot(chem_canop, aes(x = lwc, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Leaf water content') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,15) +
  ylab('Density')
lwc

# Leaf thickness distribution by subregion
thick <- ggplot(chem_canop, aes(x = leaf_thickness_mm, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Thickness') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,3) +
  ylab('Density')
thick

# Length to width ratio (lwr) by subregion
lwr <- ggplot(chem_canop, aes(x = lwr, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Leaf length to width ratio') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,40) +
  ylab('Density')
lwr

# Make a combined plot
combined_foliar_struct <-ggarrange(lma, lwc, thick, lwr, ncol = 2, nrow = 2,
                                   labels = c('A','B','C', 'D'),
                                 common.legend = TRUE,
                                 legend = "right"
) + theme(text = element_text(size = 12, family = "Arial"))
combined_foliar_struct

# Save as tiff and jpeg
ggsave(filename = paste0(figpath, 'foliar_summary_plot_struct.tiff'), plot = combined_foliar_struct, width = 8, dpi = 300, units = "in",
       bg= 'white')
ggsave(filename = paste0(figpath, 'foliar_summary_plot_struct.jpeg'), plot = combined_foliar_struct, width = 8, dpi = 300, units = "in",
       bg= 'white')

# An alternative visualization by family (not used in manuscript)
# major_fams <- c('Restionaceae', 'Ericaceae', 'Proteaceae','Aizoaceae')
# chem_canop_fams <- chem_canop %>% dplyr::filter(family_WFO %in% major_fams)
# succulence_fam <- ggplot(chem_canop_fams, aes(x = succulence, color = family_WFO)) + geom_density() + theme_classic() +
#   xlab('Leaf succulence') + 
#   xlim(0,0.5) +
#   ylab('Density')
# succulence_fam
# 
# lma_fam <- ggplot(chem_canop_fams, aes(x = lma, color = family_WFO)) + geom_density() + theme_classic() +
#   xlab('Leaf mass per area') + 
# #  xlim(0,0.5) +
#   ylab('Density')
# lma_fam

# Figures for species data ####

# Figure summarizing growth form tallies
# Subset just the relevant binary columns
growth_types <- spectrait[, c("herb", 'geophyte', 'graminoid','low_shrub', 'mid_shrub',
                             'tall_shrub','tree', 'scrambler','liana','hemiparasite',
                             'parasite')]

# Sum each column to get the count of species per category
type_counts <- colSums(growth_types)

# Convert the result to a data frame for ggplot
type_df <- data.frame(
  category = names(type_counts),
  count = as.numeric(type_counts)
)

type_df$category <- recode(type_df$category,
                           "tall_shrub" = "tall shrub",
                           "mid_shrub" = "medium shrub",
                           "low_shrub" = "low shrub")

# Reorder categories by count (from highest to lowest)
type_df$category <- factor(type_df$category, 
                           levels = type_df$category[order(-type_df$count)])

# Create a bar chart
growth_form_plot <- ggplot(type_df, aes(y = category, x = count)) +
  geom_bar(stat = "identity") +
  # Add labels above bars
  # geom_text(aes(label = count),
  #           vjust = -0.4,      # nudge text above bar
  #           size = 3.5,# text size
  #         ) +
  
  labs(y = "", x = "Number of Species") +
  theme_minimal() +
  theme(text = element_text(size= 20))

growth_form_plot

# now summarize flowering period

# Create a named vector to convert month abbreviations to numbers
month_map <- setNames(1:12, month.abb)

# Apply to your dataframe
spectrait$flower_begin_num <- month_map[spectrait$flower_begin]
spectrait$flower_end_num   <- month_map[spectrait$flower_end]

# Create a helper to generate a month sequence
get_flowering_months <- function(start, end) {
  if (start <= end) {
    return(start:end)
  } else {
    return(c(start:12, 1:end))  # Wrap around calendar year
  }
}

expanded <- spectrait %>%
  filter(!is.na(flower_begin_num) & !is.na(flower_end_num)) %>%
  rowwise() %>%
  mutate(months = list(get_flowering_months(flower_begin_num, flower_end_num))) %>%
  unnest(months)

month_counts <- expanded %>%
  count(months) %>%
  mutate(month_label = month.abb[months])

ggplot(month_counts, aes(x = factor(month_label, levels = month.abb), y = n)) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  coord_polar(start = 0) +
  theme_minimal() +
  labs(title = "Flowering Abundance Across Months",
       x = "", y = "Species Count") +
  theme(text = element_text(size = 20),
        title= element_text(size = 16),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"))

expanded_filtered <- expanded %>%
  # Remove the i,b entry
  filter(pollination != "i,b") %>%
  # Recode short codes to full labels
  mutate(pollinator = recode(pollination,
                             i = "Insect",
                             w = "Wind",
                             b = "Bird",
                             m = "Mammal",
                             u = "Unknown"
  ))

month_poll_counts <- expanded_filtered %>%
  count(months, pollinator) %>%
  mutate(
    month_label = factor(month.abb[months], levels = month.abb)
  )

flower_phen <- ggplot(month_poll_counts,
       aes(x = month_label, y = n, fill = pollinator)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  scale_fill_brewer(palette = "Set1") +
  labs(
   # title = "Flowering Abundance by Pollination Syndrome",
    x = "", y = "Species Count", fill = "Pollinator"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold")
  )


flower_phen <- flower_phen +
  theme(text = element_text(size =20),
   legend.position = 'bottom',  # Adjust position inside plot (x, y from 0 to 1)
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))


print(flower_phen)

# Figure summarizing leaf tallies

# Replace NAs in the two trouble columns with 0  
leaf_types_clean <- spectrait %>%
  mutate(
    lvs_lobed     = replace_na(lvs_lobed,     0),
    lvs_dissected = replace_na(lvs_dissected, 0)
  ) %>%
  select(lvs_linear, lvs_intermediate, lvs_oval,
         lvs_compound, lvs_lobed, lvs_dissected,
         virtually_no_leaves)

# Re-sum with NA removed
type_counts_lvs <- colSums(leaf_types_clean, na.rm = TRUE)

type_df_lvs <- data.frame(
  category = names(type_counts_lvs),
  count    = as.numeric(type_counts_lvs)
)

type_df_lvs$category <- recode(type_df_lvs$category,
                           "lvs_linear" = "linear",
                           "lvs_intermediate" = "intermediate",
                           "lvs_oval" = "oval",
                           "lvs_compound" = "compound",
                           'virtually_no_leaves' = 'no leaves',
                           'lvs_lobed' = 'lobed',
                           'lvs_dissected' = 'dissected')

# Reorder categories by count (from highest to lowest)
type_df_lvs$category <- factor(type_df_lvs$category, 
                           levels = type_df_lvs$category[order(-type_df_lvs$count)])

# Create a bar chart
leaf_form_plot <- ggplot(type_df_lvs, aes(y = category, x = count)) +
  geom_bar(stat = "identity") +
  # Add labels above bars
  # geom_text(aes(label = count),
  #           vjust = -0.4,      # nudge text above bar
  #           
  #           size = 3.5,# text size
  #           ) +
  
  
  labs(x = "Number of Species", y = "") +
  theme_minimal() +
  theme(text = element_text(size = 20))

leaf_form_plot

# create plot focusing on aspects of flammability and dispersal


# Step 1: Clean and summarize data
spectrait_clean <- spectrait %>%
  filter(!is.na(flammability)) %>%
  count(flammability, dispersal) %>%
  group_by(flammability) %>%
  mutate(
    proportion = n / sum(n),
    dispersal = fct_reorder(dispersal, n, .desc = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    flammability = fct_recode(flammability,
                              "Low" = "l", "Medium" = "m", "High" = "h"
    ),
    dispersal = fct_recode(dispersal,
                           "Passive" = "p", "Wind" = "w", "Mammal" = "m",
                           "Insect" = "i", "Unknown" = "u"
    )
  )

# Step 2: Plot with proportion labels
spectrait_clean2 <- spectrait_clean %>% filter(flammability != 'i') # remove an odd value
flam_disp <- ggplot(spectrait_clean2, aes(x = flammability, y = n, fill = dispersal)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    
    size = 4
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(text = element_text(size =20),
    legend.position = 'bottom',
        legend.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  labs(x = "Flammability", y = "", fill = "Dispersal Type") +
   guides(fill = guide_legend(nrow = 2, byrow = TRUE))
flam_disp

library(patchwork)

combined_test <- (growth_form_plot + leaf_form_plot) / (flam_disp + flower_phen) +
  #plot_layout(guides = 'collect') &
  theme(text = element_text(size = 12, family = "Arial"))

combined_test <- combined_test + plot_annotation(tag_levels = 'A')
combined_test
ggsave(
  filename = paste0(figpath, 'species_all_summary_plot.tiff'),
  plot = combined_test,
  width = 14, height = 10, dpi = 300, units = "in",
  bg = 'white'
)

ggsave(
  filename = paste0(figpath, 'species_all_summary_plot.jpeg'),
  plot = combined_test,
  width = 14, height = 10, dpi = 300, units = "in",
  bg = 'white'
)

# Figures for vnir spectroscopy data #####


# Add a per-row identifier to taken into account replicate measure with unique sample ID's
vnir_tagged <- vnir %>%
  mutate(obs_id = row_number())

# Pivot to long format
vnir_long <- vnir_tagged %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "wavelength",
    names_prefix = "X",
    names_transform = list(wavelength = as.integer),
    values_to = "reflectance"
  )

# Summarize for median (you can drop IQR if not plotting it)
summary_stats <- vnir_long %>%
  group_by(wavelength) %>%
  summarise(
    median = median(reflectance, na.rm = TRUE),
    .groups = "drop"
  )

# Plot: individual measurements (transparent) + median (black)
p1 <- ggplot() +
  geom_line(
    data = vnir_long,
    aes(x = wavelength, y = reflectance, group = obs_id),
    color = "gray40",
    alpha = 0.10,
    linewidth = 0.3
  ) +
  geom_line(
    data = summary_stats,
    aes(x = wavelength, y = median),
    color = "black",
    linewidth = 1.2
  ) +
  labs(x = "Wavelength (nm)", y = "Reflectance (%)") +
  theme_minimal() +
  theme(text = element_text(size = 20))

p1

# Calculate Coefficient of Variation (CV)
cv_stats <- vnir_long %>%
  group_by(wavelength) %>%
  summarise(
    mean = mean(reflectance, na.rm = TRUE),
    sd = sd(reflectance, na.rm = TRUE),
    cv = sd / mean,
    .groups = "drop"
  )

# Plot 2: Coefficient of Variation
p2 <- ggplot(cv_stats, aes(x = wavelength, y = cv)) +
  geom_line(color = "darkorange", size = 1.75) +
  labs(#title = "Coefficient of Variation by Wavelength",
       x = "Wavelength (nm)",
       y = "CV") +
  theme_minimal() +
  theme(text = element_text(size = 20))

# Grouped median reflectance by subregion
grouped_median <- vnir_long %>%
  group_by(subregion, wavelength) %>%
  summarise(median = median(reflectance, na.rm = TRUE), .groups = "drop")
grouped_median$subregion <- factor(x = grouped_median$subregion, levels = sort(unique(grouped_median$subregion)))

# Plot 3: Median reflectance by subregion with Dark2 palette and internal legend
p3 <- ggplot(grouped_median, aes(x = wavelength, y = median, color = subregion)) +
  geom_line(size = 1.75) +
  scale_color_brewer(palette = "Dark2") +
  labs(#title = "Median Reflectance by Subregion",
       x = "Wavelength (nm)",
       y = "Reflectance (%)",
       color = "Subregion") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.position = c(0.8,0.3),#'bottom',  # Adjust position as needed
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))
  
p3
# Combine plots: p1 and p2 stacked, p3 to the right
combined_spec <- p1 / p2 / p3 #(p1 / p2) | p3
combined_spec <- combined_spec + plot_annotation(tag_levels = 'A')
print(combined_spec) 
ggsave(
  filename = paste0(figpath, 'vnir_spec_sumary.tiff'),
  plot = combined_spec,
  width = 10, height = 14, dpi = 300, units = "in",
  bg = 'white'
)

ggsave(
  filename = paste0(figpath, 'vnir_spec_sumary.jpeg'),
  plot = combined_spec,
  width = 10, height = 14, dpi = 300, units = "in",
  bg = 'white'
)


# releve summary figure, compute species richness by year and subregion ####

# Compute species richness per plot per year
richness <- releve %>%
  group_by(plot, year) %>%
  summarise(species_richness = n_distinct(scientific_name_original), .groups = "drop")

richness <- richness %>%
  left_join(releve %>% select(plot, subregion) %>% distinct(), by = "plot")


# Code for old summary plot: 
# Summarize by subregion and year
# summary <- richness %>%
#   group_by(subregion, year) %>%
#   summarise(
#     median_richness = median(species_richness),
#     lower_iqr = quantile(species_richness, 0.25),
#     upper_iqr = quantile(species_richness, 0.75),
#     .groups = "drop"
#   )

# Create a combined factor for subregion and year
# summary <- summary %>%
#   mutate(subregion = str_replace(subregion, "_", " ")) %>%
#   mutate(subregion = str_to_title(subregion)) %>%
#    mutate(subregion = case_when(subregion == 'Htr' ~ 'HTR',
#                                 TRUE ~ subregion)) %>%
#   mutate(
#     year = factor(year, levels = sort(unique(year))),
#     subregion_year = interaction(subregion, year, sep = " ")
#   ) %>%
#   mutate(
#     subregion_year = factor(subregion_year, levels = unique(subregion_year))
#   )


# Plot with subregion-year on x-axis
# sp_rich_plot <- ggplot(summary, aes(x = subregion_year, y = median_richness)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   geom_errorbar(
#     aes(ymin = lower_iqr, ymax = upper_iqr),
#     width = 0.2
#   ) +
#   labs(
#     x = "Subregion and Year",
#     y = "Median Species Richness"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# sp_rich_plot
# 
# ggsave(
#   filename = paste0(figpath, 'releve_sumary.tiff'),
#   plot = sp_rich_plot,
#   width = 8, height = 6, dpi = 300, units = "in",
#   bg = 'white'
# )

# New plot of richness flip axes and make violin
richness_clean <- richness %>%
  mutate(subregion = str_replace(subregion, "_", " ")) %>%
  mutate(subregion = str_to_title(subregion)) %>%
  mutate(subregion = case_when(subregion == 'Htr' ~ 'HTR',
                               TRUE ~ subregion)) %>%
  mutate(
    year = factor(year, levels = sort(unique(year))),
    subregion_year = interaction(subregion, year, sep = " ")
  ) %>%
  mutate(
    subregion_year = factor(subregion_year, levels = unique(subregion_year))
  )


sp_rich_plot <- ggplot(richness_clean,
       aes(y = reorder(subregion_year, species_richness, FUN = median),
           x = species_richness)) +
  geom_violin(fill = "grey85", color = "grey40") +
  stat_summary(fun = \(x) median(x, na.rm = TRUE),
               geom = "point",
               color = "black",
               size = 2) +
  labs(y = "", x  = "Species richness") + theme_minimal()


ggsave(
  filename = paste0(figpath, 'releve_sumary.tiff'),
  plot = sp_rich_plot,
  width = 8, height = 6, dpi = 300, units = "in",
  bg = 'white'
)

ggsave(
  filename = paste0(figpath, 'releve_sumary.jpeg'),
  plot = sp_rich_plot,
  width = 8, height = 6, dpi = 300, units = "in",
  bg = 'white'
)

# write out releve richness layer
richness_gis <- richness %>%
  left_join(releve %>% select(plot, latitude, longitude) %>% distinct(), by = "plot") 

# remove missing lat/longs (1966 Cape Point)
releve_sf <- st_as_sf(richness_gis %>% na.omit(latitude) , coords = c("longitude", "latitude"), crs = 4326)

# choose only most recent surveys for the repeated surveys
non_temp_rep_subs <- c('htr', 'hangklip','langeberg', 'cederberg')
releve_sf_output <- releve_sf %>% dplyr::filter(subregion %in% non_temp_rep_subs | subregion == 'cape_point' & year == '2010' |
                                                  subregion == 'baviaanskloof' & year == '2011')

write_sf(releve_sf_output, 'GCFR_Traits/Spatial_Data/sp_rich_releve.geojson', delete_dsn = TRUE )


# summary numbers #####

# Describe the number of structural leaf and twig traits measured 
colnames(chem_canop)

chem_canopflat <- purrr::flatten_dbl(chem_canop[,16:30])
missingchem_canop <- which(is.na(chem_canopflat) == TRUE)

chem_canoptraitobs <- chem_canopflat[-missingchem_canop]

# include pubescence category from leafstruc traits
pub_complete <- leafstruc$pubescence %>% na.omit()

# number of measured and derived structural
print(length(chem_canoptraitobs) + length(pub_complete))

# number of unique species from chem_canop traits
length(unique(chem_canop$scientific_name_WFO))

# number of unique families from chem_canop traits
length(unique(chem_canop$family_WFO))

# number of average replicates and s.d./range
chem_canop_count <- chem_canop %>% group_by(scientific_name_WFO, Subregion) %>%
  summarise(SampleCounts = n()) %>% ungroup()

# check variation by region
chem_canop_count %>% group_by(Subregion)   %>% summarise(mean = mean(SampleCounts))

# overall summaries
median(chem_canop_count$SampleCounts)
mean(chem_canop_count$SampleCounts)
sd(chem_canop_count$SampleCounts)
range(chem_canop_count$SampleCounts)

#### Describe the number of canopy structural traits measured ####
colnames(leafstruc)

leafstruccanopyflat <- purrr::flatten_dbl(leafstruc[,c(14,17,18)])
missingcanopyflat <- which(is.na(leafstruccanopyflat) == TRUE)

leafstruccanopytraitobs <- leafstruccanopyflat[-missingcanopyflat]

print(length(leafstruccanopytraitobs))

# number of average replicates and s.d./rangef
leafstruc_count <- leafstruc %>% group_by(scientific_name_WFO, Subregion) %>%
  summarise(SampleCounts = n()) %>% ungroup()

# check variation by region
leafstruc_count %>% group_by(Subregion)   %>% summarise(mean = mean(SampleCounts))

# overall summaries
median(leafstruc_count$SampleCounts)
mean(leafstruc_count$SampleCounts)
sd(leafstruc_count$SampleCounts)
range(leafstruc_count$SampleCounts)

#### Describe the number of elemental/isotope traits measured ####
leafstrucelemflat <- purrr::flatten_dbl(leafstruc[,20:24])
missingelemflat <- which(is.na(leafstrucelemflat) == TRUE)

leafstrucelemtraitobs <- leafstrucelemflat[-missingelemflat]

print(length(leafstrucelemtraitobs))

# number of species
length(unique(leafstruc$scientific_name_WFO))

# number of families
length(unique(leafstruc$family_WFO))


#### Describe the number of spectral measurements measured ####
# number of measurements
dim(vnir)[1]

# number of unique species
length(unique(vnir$scientific_name_WFO))

# number of unique families 
length(unique(vnir$family_WFO))


# number of average replicates and s.d./rangef
spec_count <- vnir %>% group_by(scientific_name_WFO, subregion) %>%
  summarise(SampleCounts = n()) %>% ungroup()

# check variation by region
spec_count %>% group_by(subregion)   %>% summarise(mean = mean(SampleCounts))

# overall summaries
median(spec_count$SampleCounts)
mean(spec_count$SampleCounts)
sd(spec_count$SampleCounts)
range(spec_count$SampleCounts)

spec_count_rep <- vnir %>% group_by(unique_ID, subregion) %>%
  summarise(SampleCounts = n()) %>% ungroup()

mean(spec_count_rep$SampleCounts)
