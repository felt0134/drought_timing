
# key summary statistics of C uptake responses to drought

#-------------------------------------------------------------------------------
# changes to cumulative and maximum C uptake  ------

#shortgrass steppe
max_total_reduction_sgs_df <- 
  readRDS('Output/max_total_reduction_shortgrass_steppe.rds')
head(max_total_reduction_sgs_df,1)

#total reduction
total_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'total')
quantile(total_reduction_sgs$reduction,c(0.25,0.5,0.75)) #absolute
quantile(total_reduction_sgs$perc_reduction,c(0.25,0.5,0.75)) #relative

#max reduction
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') #max actually implies 'most negative' for our purposes
quantile(max_reduction_sgs$reduction,c(0.25,0.5,0.75)) 
quantile(max_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$doy,c(0.25,0.5,0.75)) #day of year of max absolute reduction

#max increase
min_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'min')
quantile(min_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#northern mixed prairies
max_total_reduction_nmp_df <- 
  readRDS('Output/max_total_reduction_northern_mixed_prairies.rds')
head(max_total_reduction_nmp_df,1)

#total reduction
total_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'total')
quantile(total_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(total_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))

#max reduction
max_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max')
quantile(max_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(max_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_nmp$doy,c(0.25,0.5,0.75))

#max increase
min_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'min')
quantile(min_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))
quantile(min_reduction_nmp$doy,c(0.25,0.5,0.75))

#cleanup
rm(max_reduction_nmp,max_reduction_sgs,max_total_reduction_nmp_df,
   max_total_reduction_sgs_df,min_reduction_nmp,min_reduction_sgs,
   total_reduction_nmp,total_reduction_sgs)




#-------------------------------------------------------------------------------
# compare distributions of day 75 by ecoregion  ------

#shortgrass steppe
day_75_drought_sgs <-
  raster('Output/day_75_drought_impact_shortgrass_steppe.tif')
day_75_drought_sgs <- data.frame(rasterToPoints(day_75_drought_sgs))
day_75_drought_sgs <- day_75_drought_sgs %>%
  dplyr::rename('day_75' = 'day_75_drought_impact_shortgrass_steppe')

quantile(day_75_drought_sgs$day_75,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_75_drought_nmp <-
  raster('Output/day_75_drought_impact_northern_mixed_prairies.tif')
day_75_drought_nmp <- data.frame(rasterToPoints(day_75_drought_nmp))
day_75_drought_nmp <- day_75_drought_nmp %>%
  dplyr::rename('day_75' = 'day_75_drought_impact_northern_mixed_prairies')

quantile(day_75_drought_nmp$day_75,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_75 <- ks_test_bootstrap(day_75_drought_nmp,day_75_drought_sgs)

#look at left tail of the D statistic
quantile(compare_day_75$test.statistic,probs = c(0.01,0.5))

#look at correlation with latitude
day_75_drought_sgs_nmp <- rbind(day_75_drought_nmp,day_75_drought_sgs)
cor.test(day_75_drought_sgs_nmp$y,day_75_drought_sgs_nmp$day_75 ,method = 'spearman',exact=FALSE)

#cleanup
rm(day_75_drought_nmp,day_75_drought_sgs,day_75_drought_sgs_nmp,
   compare_day_75)


#-------------------------------------------------------------------------------
# compare distribution of day 50  ------


#shortgrass stppe
day_50_drought_sgs <-
  raster('Output/day_50_drought_impact_shortgrass_steppe.tif')
day_50_drought_sgs <- data.frame(rasterToPoints(day_50_drought_sgs))
day_50_drought_sgs <- day_50_drought_sgs %>%
  dplyr::rename('day_50' = 'day_50_drought_impact_shortgrass_steppe')

quantile(day_50_drought_sgs$day_50,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_50_drought_nmp <-
  raster('Output/day_50_drought_impact_northern_mixed_prairies.tif')
day_50_drought_nmp <- data.frame(rasterToPoints(day_50_drought_nmp))
day_50_drought_nmp <- day_50_drought_nmp %>%
  dplyr::rename('day_50' = 'day_50_drought_impact_northern_mixed_prairies')

quantile(day_50_drought_nmp$day_50,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_50 <- ks_test_bootstrap(day_50_drought_nmp,day_50_drought_sgs)

#look at left tail of D statistic
quantile(compare_day_50$test.statistic,probs= c(0.01,0.5))

#look at correlation with latitude
day_50_all <- rbind(day_50_drought_nmp,day_50_drought_sgs)
cor.test(day_50_all$y,day_50_all$day_50 ,method='spearman',exact=FALSE)

#cleanup
rm(compare_day_50,day_50_all,day_50_drought_nmp,day_50_drought_sgs)


#spatial autocorrelation analysis
library(ape)
library(gstat)

#sgs
day_50_drought_sgs <-
  raster('Output/day_50_drought_impact_shortgrass_steppe.tif')

#test for autocorrelation
day_50_drought_sgs_df <- data.frame(rasterToPoints(day_50_drought_sgs))
sgs.dists <- as.matrix(dist(cbind(day_50_drought_sgs_df$x,day_50_drought_sgs_df$y)))

sgs.dists.inv <- 1/sgs.dists
diag(sgs.dists.inv) <- 0

Moran.I(day_50_drought_sgs_df$day_50_drought_impact_shortgrass_steppe,sgs.dists.inv,
        alternative = "two.sided")

rm(day_50_drought_sgs_df,sgs.dists,sgs.dists.inv)

#significant
#coefficient = 0.15

rm(sgs.dists,sgs.dists.inv,day_50_drought_sgs_df)

#variogram analysis
point_data_50_sgs <- as(day_50_drought_sgs,
                        'SpatialPointsDataFrame')

TheVariogram_50_sgs = gstat::variogram(day_50_drought_impact_shortgrass_steppe ~1,
                                       data = point_data_50_sgs,width = 10)

#visually estimate parameters from plot
plot(TheVariogram_50_sgs)

#nugget: 50
#psill: 350
#range: 300

FittedModel_50_sgs = gstat::fit.variogram(object = TheVariogram_50_sgs,
                                          model = gstat::vgm(psill = 350,
                                                             nugget = 40,
                                                             range = 300,
                                                             model = 'Exp'))

plot(TheVariogram_50_sgs,model=FittedModel_50_sgs)

FittedModel_50_sgs
#Modeled range parameter = 44.7
#range at saturation of psill: 44.7*3 = 134.1 km

rm(day_50_drought_sgs,FittedModel_50_sgs,point_data_50_sgs,TheVariogram_50_sgs)

#northern mixed prairies
day_50_drought_nmp <-
  raster('Output/day_50_drought_impact_northern_mixed_prairies.tif')

#test for spatial autocorrelation
day_50_drought_nmp_df <- data.frame(rasterToPoints(day_50_drought_nmp))
nmp.dists <- as.matrix(dist(cbind(day_50_drought_nmp_df$x,day_50_drought_nmp_df$y)))

nmp.dists.inv <- 1/nmp.dists
diag(nmp.dists.inv) <- 0

Moran.I(day_50_drought_nmp_df$day_50_drought_impact_northern_mixed_prairies,nmp.dists.inv,
        alternative = "two.sided")

#significant
#coefficient = 0.20

rm(day_50_drought_nmp_df,nmp.dists,nmp.dists.inv)

#variogram
point_data_50_nmp <- as(day_50_drought_nmp, 'SpatialPointsDataFrame')
TheVariogram_50_nmp = variogram(day_50_drought_impact_northern_mixed_prairies ~1,
                                data = point_data_50_nmp,width = 10)

plot(TheVariogram_50_nmp)

#nugget: 10
#psill: 80
#range: 400

FittedModel_50_nmp = gstat::fit.variogram(object = TheVariogram_50_nmp,
                                          model = gstat::vgm(psill = 10,
                                                             nugget = 80,
                                                             range = 450,
                                                             model = 'Exp'))

plot(TheVariogram_50_nmp,model=FittedModel_50_nmp)
FittedModel_50_nmp

#Modeled range parameter = 161.7
#range at saturation of psill: 161.7*3 =  km

rm(day_50_drought_nmp,FittedModel_50_nmp,point_data_50_nmp,TheVariogram_50_nmp)

#-------------------------------------------------------------------------------
# compare distribution of day 25  ------


#shortgrass stppe
day_25_drought_sgs <-
  raster('Output/day_25_drought_impact_shortgrass_steppe.tif')
day_25_drought_sgs <- data.frame(rasterToPoints(day_25_drought_sgs))
day_25_drought_sgs <- day_25_drought_sgs %>%
  dplyr::rename('day_25' = 'day_25_drought_impact_shortgrass_steppe')

quantile(day_25_drought_sgs$day_25,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_25_drought_nmp <-
  raster('Output/day_25_drought_impact_northern_mixed_prairies.tif')
day_25_drought_nmp <- data.frame(rasterToPoints(day_25_drought_nmp))
day_25_drought_nmp <- day_25_drought_nmp %>%
  dplyr::rename('day_25' = 'day_25_drought_impact_northern_mixed_prairies')

quantile(day_25_drought_nmp$day_25,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_25 <- ks_test_bootstrap(day_25_drought_nmp,day_25_drought_sgs)

#look at left tail of D statistic 
quantile(compare_day_25$test.statistic,probs= c(0.01,0.5))

#look at correlation with latitude
day_25_all <- rbind(day_25_drought_nmp,day_25_drought_sgs)
cor.test(day_25_all$y,day_25_all$day_25,method='spearman',exact=FALSE)

#cleanup
rm(compare_day_25,day_25_all,day_25_drought_nmp,day_25_drought_sgs)


#-------------------------------------------------------------------------------
