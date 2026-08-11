library(tidyverse)
raw_reshaped_all <- read_csv("GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/merged_raw_releves/releve_raw_reshaped_legacy_2013logic.csv")

cb_hk_plotName_convert <- read.csv('GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/merged_raw_releves/Comm_CB_HK_PlotNum_Convert.csv', as.is = TRUE)

releve_all_rebuilt <- raw_reshaped_all %>%
  mutate(Species = str_squish(raw_species)) %>%
  left_join(cb_hk_plotName_convert, by = c("plot" = "Releve_Number_Orig")) %>%
  mutate(plot = if_else(!is.na(Relve_Number_New), Relve_Number_New, plot)) %>%
  select(-Relve_Number_New) %>%
  group_by(plot, year) %>%
  mutate(PlotPercCov = sum(percent_cover, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(RelPercCover = percent_cover / PlotPercCov)

write_csv(releve_all_rebuilt, "GCFR_Traits/Data_Workflow_2_Taxonomic_Cleaning/Data_Inputs/Releve_All.csv")
