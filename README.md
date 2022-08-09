
# R code for "Intra-annual timing and magnitude of drought impacts to carbon uptake across a grassland biome"

This R project contains the key scripts and datasets to run the core analyses of the
research project, specifically the impacts and dynamics of carbon uptake (GPP) during drought
across the two ecoregions. Note this specific R project does not contain every script and dataset to do every analysis, such as with the weather data. Those scripts and data are part of a larger project and 
are currently available at https://github.com/felt0134/seasonal_signature and
https://github.com/felt0134/NIFA_CDD_Data_Import but may be subject to changes in the future.
The R project outlined here contains the key scripts to do the main analyses of the study.

# Datasets: 

ppt_gpp.shortgrass_steppe.zip and ppt_gpp.northern_mixed_prairies.zip contain .rds
files with 16-day GPP for 20 years for all pixels. Will need to unzip these files for analysis.

# Scripts:

01_build.R: Always the first script to run. This script sets the workspace, 
downloads the key libraries, and calls from source all the project's in-house functions from 
02_Functions.R.

analyses_master.R: This script calls from source other scripts to run analyses and generate
derived datasets (saved to the output folder) that can later be summarized and analyzed.

day_of_25.R, day_of_50.R, day_of_75.R: These scripts take the ppt_gpp datasets and
estimate the day of 25%, 50%, or 75% of total carbon uptake (GPP) in non-drought
and drought years. Saves the data to the output folder.

drought_reduction_spline.R: This script takes the ppt_gpp datasets and calculates the
daily differences in estimated 16-day carbon uptake (GPP) between drought and non-drought
years for each pixel. Saves the data to the output folder.

growth_curve_spline.R: This script estimates the cumulative carbon uptake (GPP) in drought and non-drought years
across each ecoregion. Saves the data to the output folder.

summary_stats.R: Quantifies and describes the distribution of drought impacts, tests for
statistical differences between ecoregions, and quantifies spatial autocorrelation. 

Figure.R: Creates and saves key manuscript figure to Figures folder.


