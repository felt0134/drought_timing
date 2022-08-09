

#master analysis script that calls on other scripts. The ultimate outputs are
#datasets.

#get data and track progress
library(future.apply)
library(progressr)

#pick which ecoregion to run the script for. You could loop and run each ecoregion
#in one batch but some of these take a while.

#Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 

# day by which 25%, 50%, and 75% of growth has occurred during average and drought years -----


source('day_of_75.R')

source('day_of_50.R')

source('day_of_25.R')


#-------------------------------------------------------------------------------
# carbon uptake splines in average and drought years -----


source('growth_curve_splines.R')

source('drought_reduction_spline.R')

#-------------------------------------------------------------------------------
# get the max and total carbon uptake reduction from drought -------


source('max_total_reduction.R')


#-------------------------------------------------------------------------------
