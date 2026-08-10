########################################################
# Workflow3_TRY_data_ranges.R
#
# Purpose: Assess TRY data ranges to provide reality checks
# in outlier analysis
#
#
# Date Created: August 2026
# Author(s): Henry Frye, Claude
########################################################

library(data.table)

try_data <- fread("/Volumes/Enspec/users/henry/TRY_Aug_2026/51461.txt", sep = "\t", quote = "")

# quick range check for each trait
trait_ranges <- try_data[!is.na(StdValue), 
                         .(min_val = min(StdValue), 
                           max_val = max(StdValue),
                           n = .N,
                           unit = first(UnitName)),
                         by = .(TraitID, TraitName)]

print(trait_ranges)

traits_of_interest <- c("Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded",
                        "Leaf water content per leaf dry mass (not saturated)",
                        "Leaf thickness",
                        "Leaf length",
                        "Leaf width",
                        "Leaf nitrogen (N) content per leaf dry mass", 
                        "Leaf carbon (C) content per leaf dry mass",
                        "Leaf nitrogen (N) isotope signature (delta 15N)", 
                        "Leaf carbon isotope signature (delta 13C)")

# ErrorRisk is a standardized score (roughly z-score-like) flagging outliers within
# each trait/species combo -- TRY docs suggest values > 3-4 are often flagged as
# suspect, but there's no hard universal cutoff. Adjust threshold as needed.
error_risk_cutoff <- 4

isotope_traits <- c("Leaf nitrogen (N) isotope signature (delta 15N)",
                    "Leaf carbon isotope signature (delta 13C)")

subset_data <- try_data[TraitName %in% traits_of_interest & 
                          !is.na(StdValue) &
                          (is.na(ErrorRisk) | ErrorRisk <= error_risk_cutoff) &
                          (TraitName %in% isotope_traits | StdValue > 0)]

trait_summary <- subset_data[, .(min_val = min(StdValue),
                                 max_val = max(StdValue),
                                 n = .N,
                                 unit = first(UnitName)),
                             by = TraitName]
print(trait_summary)
