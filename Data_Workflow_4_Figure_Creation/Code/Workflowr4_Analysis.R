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

# Read in data
field <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Field_Traits_Final.csv')

# Figures for field data ####
ggplot(field, aes(x = canopy_area_cm2, color = subregion)) + geom_density() +
  xlim(0, 10000)

# percent N
N_plot <- ggplot(field, aes(x = percent_N, color = subregion)) + geom_density() + theme_classic()
N_plot

# plot percent Carbon, but remove outlier
field_no_outlier <- field %>% dplyr::filter(percent_C < 300)
C_plot<- ggplot(field_no_outlier, aes(x = percent_C, color = subregion)) + 
  geom_density()+ theme_classic()
C_plot


# plot N isotope
N_iso <- ggplot(field, aes(x = d_15N_14N, color = subregion)) + geom_density() +
  theme_classic()
N_iso

# plot C isotope
C_iso <- ggplot(field, aes(x = d_13C_12C, color = subregion)) + geom_density() +
  theme_classic()
C_iso

ggarrange(N_plot, C_plot, N_iso, C_iso, ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "right"
)
