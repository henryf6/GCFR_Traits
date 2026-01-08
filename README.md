# Plant traits of the Greater Cape Floristic Region

The Greater Cape Floristic Region of South Africa is a megadiverse region threatened by climate change, human-driven habitat loss, and invasive species. Trait-based ecology offers a mechanistic approach to understand and predict how ecosystems function and respond to disturbance. Leveraging the interactions between plants and incoming solar radiation, remote sensing offers a way to measure foliar traits at landscape scales.  

This repository contains code that provides data cleaning steps and summary information of a large plant trait dataset collected across 445 locations with supporting foliar spectroscopy measurements, an aggregation of species life history data, and relevé survey data. The repository has four workflows:

## Workflow 1: Environmental Join

This workflow joins various environmental data, e.g., elevation and annual precipitation, to the locations where plant trait data was collected. It then summarizes the extracted variables by subregions in which the data were collected

## Workflow 2: Taxonomic Cleaning

This workflow harmonizes the original taxonomic designations to a common taxonomic backbone from World Flora Online.

## Workflow 3: Final data cleaning & data dictionary creation

This workflow ensures common column names across data files and conducts data cleaning to produce the final version of the data. Initial data dictionaries are created to summarize data ranges and the amount of missingness per column.

## Workflow 4: Figure creation

This workflow creates summary figures and information about each dataset.
