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
lab <- read.csv('GCFR_Traits/Data_Workflow_3_Final_Polishing/Data_Outputs/Lab_Traits_Final.csv') 

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

# Figures for lab data ####

# re-level the subregion categories for nice display
lab$subregion <- factor(x = lab$subregion, levels = unique(lab$subregion),
                          labels = c("Baviaanskloof", "Hantam-Tanqua-\nRoggeveld", "Hangklip", "Langeberg", "Cederberg", "Cape Point"))

lab <- lab %>% rename('Subregion' = 'subregion')

# leaf mass per area
lma <- ggplot(lab, aes(x = lma, color = Subregion)) + geom_density() + theme_classic() +
  xlab('Leaf mass per area') + 
  xlim(0,0.1) +
  ylab('Density')
lma

# leaf water content
lwc <- ggplot(lab, aes(x = fwc, color = Subregion)) + geom_density() + theme_classic() +
  xlab('Leaf water content') + 
  xlim(0,20) +
  ylab('Density')
lwc

# leaf thickness
thick <- ggplot(lab, aes(x = leaf_thickness_mm, color = Subregion)) + geom_density() + theme_classic() +
  xlab('Thickness') + 
  xlim(0,2) +
  ylab('Density')
thick

# lwr
lwr <- ggplot(lab, aes(x = lwr, color = Subregion)) + geom_density() + theme_classic() +
  xlab('Leaf length to width ratio') + 
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


