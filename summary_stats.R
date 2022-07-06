
# key summary statistics of C uptake responses to drought

#-------------------------------------------------------------------------------
# changes to cumulative and maximum C uptake (need to test)  ------

#shortgrass steppe
max_total_reduction_sgs_df <- 
  read.csv('Output/max_total_reduction_shortgrass_steppe.csv')
head(max_total_reduction_sgs_df,1)

#total reduction
total_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'total')
quantile(total_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(total_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#max reduction
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max')
quantile(max_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$doy,c(0.25,0.5,0.75))

#max increase
min_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'min')
quantile(min_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#northern mixed prairies
max_total_reduction_nmp_df <- 
  read.csv('Output/max_total_reduction_northern_mixed_prairies.csv')
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


#-------------------------------------------------------------------------------
# follow-up analysis of amount of GPP by season GPP (need to test) ------

#use cumulative GPP for this part

#shortgrass steppe

Ecoregion <- "shortgrass_steppe"
growth_curve_sgs <- read.csv(paste0('Output/average_growth_curve_',Ecoregion,'.csv'))

#median GPP during spring
end_spring <- growth_curve_sgs %>%
  dplyr::filter(doy == '151') #day 151 is the end of may

spring_gpp = end_spring$mean

end_summer <- growth_curve_sgs %>%
  dplyr::filter(doy == '243') #day 243 is the end of august

summer_gpp = end_summer$mean - spring_gpp

#get differences
(summer_gpp - spring_gpp)/spring_gpp
summer_gpp/spring_gpp

#see how this changes during drought
drought_growth_curve_sgs <- read.csv(paste0('Output/drought_growth_curve_',Ecoregion,'.csv'))
end_spring_drought <- drought_growth_curve_sgs %>%
  dplyr::filter(doy == '151') 

spring_gpp_drought = end_spring_drought$mean

end_summer_drought <- drought_growth_curve_sgs %>%
  dplyr::filter(doy == '243')

#get difference during drought
summer_gpp_drought = end_summer_drought$mean - spring_gpp_drought
(spring_gpp_drought - summer_gpp_drought)/summer_gpp_drought
summer_gpp_drought/spring_gpp_drought


#
#

#northern mixed prairies
Ecoregion <- "northern_mixed_prairies"

growth_curve_nmp <- read.csv(paste0('Output/average_growth_curve_',Ecoregion,'.csv'))

#median total GPP during spring
end_spring <- growth_curve_nmp %>%
  dplyr::filter(doy == '151') 

spring_gpp = end_spring$mean

end_summer <- growth_curve_nmp %>%
  dplyr::filter(doy == '243')

summer_gpp = end_summer$mean - spring_gpp

#get difference
(summer_gpp - spring_gpp)/spring_gpp
summer_gpp/spring_gpp

#compare to drought year
drought_growth_curve_nmp <- read.csv(paste0('Output/drought_growth_curve_',Ecoregion,'.csv'))
end_spring_drought <- drought_growth_curve_nmp %>%
  dplyr::filter(doy == '151') 

spring_gpp_drought = end_spring_drought$mean

end_summer_drought <- drought_growth_curve_nmp %>%
  dplyr::filter(doy == '243')

summer_gpp_drought = end_summer_drought$mean - spring_gpp_drought

#get difference
(summer_gpp_drought - spring_gpp_drought)/spring_gpp_drought
summer_gpp_drought/spring_gpp_drought

#-------------------------------------------------------------------------------
# compare distributions of day 75 by ecoregion (need to test) ------

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
quantile(compare_day_75$test.statistic,probs= c(0.01,0.5))

#look at correlation with latitude
day_75_drought_sgs_nmp <- rbind(day_75_drought_nmp,day_75_drought_sgs)
cor.test(day_75_drought_sgs_nmp$y,day_75_drought_sgs_nmp$day_75 ,method='spearman',exact=FALSE)

#cleanup
rm(day_75_drought_nmp,day_75_drought_sgs)


#-------------------------------------------------------------------------------
# compare distribution of day 50 (need to test)  ------


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


#variogram analysis to assess spatial autocorrelation in day 50

#sgs
day_50_drought_sgs <-
  raster('Output/day_50_drought_impact_shortgrass_steppe.tif')
point_data_50_sgs <- as(day_50_drought_sgs, 'SpatialPointsDataFrame')

TheVariogram_50_sgs = variogram(day_50_drought_impact_shortgrass_steppe ~1,
                                data = point_data_50_sgs,width = 10)

# summary(TheVariogram_50_sgs)
# plot(TheVariogram_50_sgs)

TheVariogramModel_50_sgs <- vgm(psill=300, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs) 
FittedModel_50_sgs <- fit.variogram(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs)
FittedModel_50_sgs

#northern mixed prairies
day_50_drought_nmp <-
  raster('Output/day_50_drought_impact_northern_mixed_prairies.tif')

point_data_50_nmp <- as(day_50_drought_nmp, 'SpatialPointsDataFrame')

TheVariogram_50_nmp = variogram(day_50_drought_impact_northern_mixed_prairies ~1,
                                data = point_data_50_nmp,width = 10)

# summary(TheVariogram_50_nmp)
# plot(TheVariogram_50_nmp)

TheVariogramModel_50_nmp <- vgm(psill=300, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_nmp, model=TheVariogramModel_50_nmp) 
FittedModel_50_nmp <- fit.variogram(TheVariogram_50_nmp, model=TheVariogramModel_50_nmp)
FittedModel_50_nmp

#cleanup
rm(day_50_drought_nmp,day_50_drought_sgs,point_data_50_sgs,point_data_50_nmp,
   TheVariogram_50_nmp,TheVariogram_50_sgs,FittedModel_50_nmp,FittedModel_50_sgs)

#-------------------------------------------------------------------------------
# compare distribution of day 25 (need to test) ------


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
hist(compare_day_25$test.statistic)

#look at left tail of D statistic 
quantile(compare_day_25$test.statistic,probs= c(0.01,0.5))

#look at correlation with latitude
day_25_all <- rbind(day_25_drought_nmp,day_25_drought_sgs)
cor.test(day_25_all$y,day_25_all$day_25,method='spearman',exact=FALSE)


#-------------------------------------------------------------------------------
