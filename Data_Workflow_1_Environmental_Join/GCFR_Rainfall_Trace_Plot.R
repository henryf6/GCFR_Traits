########################################################
# GCFR_Rainfall_Trace_Plot.R
#
# Purpose: Figure for reviewer response. Monthly rainfall
# (CHELSA 1981-2010 climatology) at each field-trait collection
# location, drawn as faint traces across the calendar year, with a
# bold subregion median trace. One facet per subregion.
#
# Input: output of GCFR_Rainfall_Climate_Join.R (so the CHELSA
# rasters on the external drive are not needed to run this).
########################################################

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

out_dir <- "Climate_Summary_Data_Outputs"

rain <- read_csv(file.path(out_dir, "Rainfall_field_traits.csv"),
                 show_col_types = FALSE)

# Many trait samples share a collection location, so collapse to one
# trace per unique location to avoid weighting by sample count.
# (Location is the finest 'plot' identifier in this file.)
sites <- rain %>%
  mutate(subregion = recode(region,
                            "Baviaanksloof" = "Baviaanskloof",
                            "HTR" = "Hantam-Tanqua-Roggeveld")) %>%
  distinct(subregion, latitude, longitude, across(starts_with("pr_"))) %>%
  mutate(site_id = row_number())

pr_long <- sites %>%
  pivot_longer(starts_with("pr_"), names_to = "month_chr", values_to = "pr_mm") %>%
  mutate(month = as.integer(sub("pr_", "", month_chr)))

# Subregion medians per month
med <- pr_long %>%
  group_by(subregion, month) %>%
  summarise(median_pr = median(pr_mm, na.rm = TRUE), .groups = "drop")

p <- ggplot() +
  # Austral winter (Jun-Aug), the months used for WinterPrProp in the join script
  annotate("rect", xmin = 6, xmax = 8, ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.12) +
  geom_line(data = pr_long,
            aes(month, pr_mm, group = site_id),
            colour = "grey40", alpha = 0.12, linewidth = 0.3) +
  geom_line(data = med,
            aes(month, median_pr),
            colour = "firebrick", linewidth = 1.4) +
  facet_wrap(~ subregion) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = NULL,
       y = expression("Monthly precipitation (mm month"^-1*")")
       # Caption moved to the figure legend in the manuscript:
       # "Faint lines: individual collection locations. Bold: subregion median.
       #  Blue band: austral winter (Jun-Aug). CHELSA 1981-2010."
       # caption = "Faint lines: individual collection locations. Bold: subregion median. Blue band: austral winter (Jun-Aug). CHELSA 1981-2010."
       ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "Rainfall_Trace_Plot.png"), p,
       width = 9, height = 6, dpi = 300)
p
