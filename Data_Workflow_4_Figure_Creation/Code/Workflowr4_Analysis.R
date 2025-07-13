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
N_plot <- ggplot(field, aes(x = percent_N, color = Subregion)) + geom_density() + theme_classic() +
  xlab('Percent Nitrogen') +
  ylab('Density')
N_plot

# plot percent Carbon, but remove outlier
field_no_outlier <- field %>% dplyr::filter(percent_C < 300)
C_plot<- ggplot(field_no_outlier, aes(x = percent_C, color = Subregion)) + 
  geom_density()+ theme_classic() +
  xlab('Percent Carbon') +
  ylab('Density')
C_plot


# plot N isotope
N_iso <- ggplot(field, aes(x = d_15N_14N, color = Subregion)) + geom_density() +
  theme_classic() +
  ylab('Density') +
  xlab(expression(delta^15 * "Nitrogen ("*"\u2030"*")"))
  

N_iso

# plot C isotope
C_iso <- ggplot(field, aes(x = d_13C_12C, color = Subregion)) + geom_density() +
  theme_classic() +
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

