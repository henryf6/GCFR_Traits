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
library(forcats)

# Read in data
field <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Field_Traits_Final.csv')
lab <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Lab_Traits_Final.csv') 
spectrait <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Species_Traits_Final.csv')

# create figure path
figpath<- 'GCFR_Traits/Data_Workflow_4_Figure_Creation/Figures/'

# Figures for field data ####

# re-level the subregion categories for nice display
field$subregion <- factor(x = field$subregion, levels = unique(field$subregion),
labels = c("Baviaanskloof", "Hantam-Tanqua-\nRoggeveld", "Hangklip", "Langeberg", "Cederberg", "Cape Point"))

field <- field %>% rename('Subregion' = 'subregion')
       
# ggplot(field, aes(x = height_cm, color = Subregion)) + geom_density() + #canopy_area_cm2
#   xlim(0, 500)

# percent N
N_plot <- ggplot(field, aes(x = percent_N, color = Subregion)) + geom_density(linewidth =1) + theme_classic() +
  xlab('Percent Nitrogen') +
  scale_color_brewer(palette = "Dark2") +
  ylab('Density')
N_plot

# plot percent Carbon, but remove outlier
field_no_outlier <- field %>% dplyr::filter(percent_C < 300)
C_plot<- ggplot(field_no_outlier, aes(x = percent_C, color = Subregion)) + 
  geom_density(linewidth =1)+ theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  xlab('Percent Carbon') +
  ylab('Density')
C_plot


# plot N isotope
N_iso <- ggplot(field, aes(x = d_15N_14N, color = Subregion)) + geom_density(linewidth =1) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  ylab('Density') +
  xlab(expression(delta^15 * "Nitrogen ("*"\u2030"*")"))
  

N_iso

# plot C isotope
C_iso <- ggplot(field, aes(x = d_13C_12C, color = Subregion)) + geom_density(linewidth =1) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  ylab('Density') +
  xlab(expression(delta^13 * "Carbon ("*"\u2030"*")"))

C_iso

combined_foliar_chem <-ggarrange(N_plot, C_plot, N_iso, C_iso, ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "right"
) + theme(text = element_text(size = 12, family = "Arial"))
combined_foliar_chem

ggsave(filename = paste0(figpath, 'foliar_summary_plot.tiff'), plot = combined_foliar_chem, width = 8, dpi = 300, units = "in",
       bg= 'white')

# Figures for lab data ####

# re-level the subregion categories for nice display
lab$subregion <- factor(x = lab$subregion, levels = unique(lab$subregion),
                          labels = c("Baviaanskloof", "Hantam-Tanqua-\nRoggeveld", "Hangklip", "Langeberg", "Cederberg", "Cape Point"))

lab <- lab %>% rename('Subregion' = 'subregion')

# leaf mass per area
lma <- ggplot(lab, aes(x = lma, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Leaf mass per area') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,0.1) +
  ylab('Density')
lma

# leaf water content
lwc <- ggplot(lab, aes(x = fwc, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Leaf water content') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,20) +
  ylab('Density')
lwc

# leaf thickness
thick <- ggplot(lab, aes(x = leaf_thickness_mm, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Thickness') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,2) +
  ylab('Density')
thick

# lwr
lwr <- ggplot(lab, aes(x = lwr, color = Subregion)) + geom_density(linewidth = 1) + theme_classic() +
  xlab('Leaf length to width ratio') + 
  scale_color_brewer(palette = "Dark2") +
  xlim(0,20) +
  ylab('Density')
lwr


combined_foliar_struct <-ggarrange(lma, lwc, thick, lwr, ncol = 2, nrow = 2,
                                 common.legend = TRUE,
                                 legend = "right"
) + theme(text = element_text(size = 12, family = "Arial"))
combined_foliar_struct

ggsave(filename = paste0(figpath, 'foliar_summary_plot_struct.tiff'), plot = combined_foliar_struct, width = 8, dpi = 300, units = "in",
       bg= 'white')

# An alternative visualization
major_fams <- c('Restionaceae', 'Ericaceae', 'Proteaceae','Aizoaceae')
lab_fams <- lab %>% dplyr::filter(family_WFO %in% major_fams)
succulence_fam <- ggplot(lab_fams, aes(x = succulence, color = family_WFO)) + geom_density() + theme_classic() +
  xlab('Leaf succulence') + 
  xlim(0,0.5) +
  ylab('Density')
succulence_fam

lma_fam <- ggplot(lab_fams, aes(x = lma, color = family_WFO)) + geom_density() + theme_classic() +
  xlab('Leaf mass per area') + 
#  xlim(0,0.5) +
  ylab('Density')
lma_fam

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
growth_form_plot <- ggplot(type_df, aes(x = category, y = count)) +
  geom_bar(stat = "identity") +
  # Add labels above bars
  geom_text(aes(label = count),
            vjust = -0.4,      # nudge text above bar
            size = 3.5,# text size
          ) +
  
  labs(x = "Growth Form", y = "Number of Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = .75))

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
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"))

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

library(ggplot2)

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
    axis.text.x = element_text(size = 10, face = "bold")
  )


flower_phen <- flower_phen +
  theme(
   legend.position = 'bottom',  # Adjust position inside plot (x, y from 0 to 1)
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


print(flower_phen )

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
leaf_form_plot <- ggplot(type_df_lvs, aes(x = category, y = count)) +
  geom_bar(stat = "identity") +
  # Add labels above bars
  geom_text(aes(label = count),
            vjust = -0.4,      # nudge text above bar
            
            size = 3.5,# text size
            ) +
  
  
  labs(x = "Leaf Form", y = "Number of Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = .75))

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
flam_disp <- ggplot(spectrait_clean, aes(x = flammability, y = n, fill = dispersal)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    
    size = 4
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) +
  labs(x = "Flammability", y = "Count", fill = "Dispersal Type") +
   guides(fill = guide_legend(nrow = 2, byrow = TRUE))
flam_disp

library(patchwork)

combined_test <- (growth_form_plot + leaf_form_plot) / (flam_disp + flower_phen) +
  #plot_layout(guides = 'collect') &
  theme(text = element_text(size = 12, family = "Arial"))

combined_test

ggsave(
  filename = paste0(figpath, 'species_all_summary_plot.tiff'),
  plot = combined_test,
  width = 14, height = 10, dpi = 300, units = "in",
  bg = 'white'
)

