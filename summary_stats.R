
# key summary statistics

#-------------------------------------------------------------------------------
# driest years (works) ------

#Three most common 'driest years'

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/drought_precip_year_',Ecoregion,'.csv'))

driest_year_sgs_count <- aggregate(ppt_min ~ year,length,data = driest_year_sgs)
driest_year_sgs_count <- driest_year_sgs_count %>% arrange(desc(ppt_min)) 
driest_year_sgs_count <- driest_year_sgs_count[c(1:3),]
driest_year_sgs_count$perc <- round((driest_year_sgs_count$ppt_min/nrow(driest_year_sgs))*100,1)

rm(driest_year_sgs)

#
#

#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/drought_precip_year_',Ecoregion,'.csv'))

driest_year_nmp_count <- aggregate(ppt_min ~ year,length,data = driest_year_nmp)
driest_year_nmp_count <- driest_year_nmp_count %>% arrange(desc(ppt_min)) 
driest_year_nmp_count <- driest_year_nmp_count[c(1:3),]
driest_year_nmp_count$perc <- round((driest_year_nmp_count$ppt_min/nrow(driest_year_nmp))*100,1)

rm(driest_year_nmp)

#-------------------------------------------------------------------------------
# change in growing season precipitation during drought (works)  -----
Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/drought_precip_year_',Ecoregion,'.csv'))
driest_year_sgs <- driest_year_sgs %>%
  dplyr::select(x,y,ppt_min)

#pixel-year time series
as.numeric(length(seq(2003:2020)))*as.numeric(nrow(driest_year_sgs))

#import annual ppt, merge, and get % reduction during drought years
mean_precip_sgs <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/mean_precip_drought_removed',Ecoregion,'.csv'))
mean_precip_sgs <- aggregate(ppt ~ x + y,mean,data = mean_precip_sgs)
drought_reduction_sgs <- merge(driest_year_sgs,mean_precip_sgs,by=c('x','y'))

#relative (% reduction)
drought_reduction_sgs$perc_reduction <- 
  ((drought_reduction_sgs$ppt_min - drought_reduction_sgs$ppt)/
  drought_reduction_sgs$ppt)*100

quantile(drought_reduction_sgs$perc_reduction,c(.25,0.5,0.75))

#absolute
drought_reduction_sgs$abs_reduction <- 
  (drought_reduction_sgs$ppt_min - drought_reduction_sgs$ppt)

quantile(drought_reduction_sgs$abs_reduction,c(.25,0.5,0.75))

#cleanup
rm(mean_precip_sgs,driest_year_sgs,drought_reduction_sgs)

#
#

Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/drought_precip_year_',Ecoregion,'.csv'))
driest_year_nmp <- driest_year_nmp %>%
  dplyr::select(x,y,ppt_min)

#pixel-year time series
as.numeric(length(seq(2003:2020)))*as.numeric(nrow(driest_year_nmp))

#import annual ppt, merge, and get % reduction during drought years
mean_precip_nmp <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/mean_precip_drought_removed',Ecoregion,'.csv'))
mean_precip_nmp <- aggregate(ppt ~ x + y,mean,data = mean_precip_nmp)
drought_reduction_nmp <- merge(driest_year_nmp,mean_precip_nmp,by=c('x','y'))

#relative
drought_reduction_nmp$perc_reduction <- 
  ((drought_reduction_nmp$ppt_min - drought_reduction_nmp$ppt)/
  drought_reduction_nmp$ppt)*100

quantile(drought_reduction_nmp$perc_reduction,c(.25,0.5,0.75))

#absolute
drought_reduction_nmp$abs_reduction <- 
  (drought_reduction_nmp$ppt_min - drought_reduction_nmp$ppt)

quantile(drought_reduction_nmp$abs_reduction,c(.25,0.5,0.75))

#cleanup
rm(mean_precip_nmp,driest_year_nmp,drought_reduction_nmp)

#-------------------------------------------------------------------------------
# change in growing season temperature during years of drought (works) -----

#shortgrass steppe

Ecoregion <- 'shortgrass_steppe'
average_temp_sgs <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,
                   '/Temperature/mean_temp_drought_removed',Ecoregion,'.csv'))
average_temp_sgs <- average_temp_sgs %>%
  dplyr::select(x,y,average_temp)

temp_drought_sgs <-
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,
                  '/Temperature/drought_temp_year_',Ecoregion,'.csv'))
temp_drought_sgs <- temp_drought_sgs %>%
  dplyr::select(x,y,temp_drought)

temp_drought_sgs <- merge(average_temp_sgs,temp_drought_sgs,by=c('x','y'))
rm(average_temp_sgs)

#relative change
temp_drought_sgs$perc_change <- 
  ((temp_drought_sgs$temp_drought - temp_drought_sgs$average_temp)/temp_drought_sgs$average_temp)*100

quantile(temp_drought_sgs$perc_change,c(0.25,0.5,0.75))

#absolute change
temp_drought_sgs$abs_change <- 
  (temp_drought_sgs$temp_drought - temp_drought_sgs$average_temp)

quantile(temp_drought_sgs$abs_change,c(0.25,0.5,0.75))

rm(temp_drought_sgs)

#
#

Ecoregion <- 'northern_mixed_prairies'
average_temp_nmp <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,
                  '/Temperature/mean_temp_drought_removed',Ecoregion,'.csv'))
average_temp_nmp <- average_temp_nmp %>%
  dplyr::select(x,y,average_temp)

temp_drought_nmp <-
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,
                  '/Temperature/drought_temp_year_',Ecoregion,'.csv'))
temp_drought_nmp <- temp_drought_nmp %>%
  dplyr::select(x,y,temp_drought)

temp_drought_nmp <- merge(average_temp_nmp,temp_drought_nmp,by=c('x','y'))
rm(average_temp_nmp)

#relative change
temp_drought_nmp$perc_change <- 
  ((temp_drought_nmp$temp_drought - temp_drought_nmp$average_temp)/temp_drought_nmp$average_temp)*100

quantile(temp_drought_nmp$perc_change,c(0.25,0.5,0.75))

#absolute change
temp_drought_nmp$abs_change <- 
  (temp_drought_nmp$temp_drought - temp_drought_nmp$average_temp)

quantile(temp_drought_nmp$abs_change,c(0.25,0.5,0.75))

rm(temp_drought_nmp)

#-------------------------------------------------------------------------------
# changes to cumulative and maximum C uptake (need to test)  ------

#shortgrass steppe
max_total_reduction_sgs_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_shortgrass_steppe.csv')
head(max_total_reduction_sgs_df,1)

#total reduction
total_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='total')
quantile(total_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(total_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#max reduction
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='max')
quantile(max_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$doy,c(0.25,0.5,0.75))

#max increase
min_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='min')
quantile(min_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#northern mixed prairies
max_total_reduction_nmp_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')
head(max_total_reduction_nmp_df,1)

#total reduction
total_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='total')
quantile(total_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(total_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))

#max reduction
max_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='max')
quantile(max_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(max_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_nmp$doy,c(0.25,0.5,0.75))

#max increase
min_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='min')
quantile(min_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))
quantile(min_reduction_nmp$doy,c(0.25,0.5,0.75))



#-------------------------------------------------------------------------------
# seasonal changes in precipitation (works)  -------

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
seasonal_precip_sgs <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/seasonal_change_PPT.csv'))

#abs change in spring ppt
quantile(seasonal_precip_sgs$abs_change_spring_precipitation,c(0.25,0.5,0.75))

#abs change in summer ppt
quantile(seasonal_precip_sgs$abs_change_summer_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_sgs$perc_change_spring_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_sgs$perc_change_summer_precipitation,c(0.25,0.5,0.75))


#
#


#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
seasonal_precip_nmp <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/seasonal_change_PPT.csv'))

#abs change in spring ppt
quantile(seasonal_precip_nmp$abs_change_spring_precipitation,c(0.25,0.5,0.75))

#abs change in summer ppt
quantile(seasonal_precip_nmp$abs_change_summer_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_nmp$perc_change_spring_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_nmp$perc_change_summer_precipitation,c(0.25,0.5,0.75))

#cleanup
rm(seasonal_precip_nmp,seasonal_precip_sgs)


#-------------------------------------------------------------------------------
# seasonal changes in temperature (works) -----

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
seasonal_temp_sgs <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Temperature/seasonal_change_temperature.csv'))

#abs change in spring temp
quantile(seasonal_temp_sgs$abs_change_spring_temperature,c(0.25,0.5,0.75))

#abs change in summer temp
quantile(seasonal_temp_sgs$abs_change_summer_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_sgs$perc_change_spring_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_sgs$perc_change_summer_temperature,c(0.25,0.5,0.75))


#
#


#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
seasonal_temp_nmp <- 
  read.csv(paste0('Data/Climate/Ecoregion/',Ecoregion,'/Temperature/seasonal_change_temperature.csv'))
head(seasonal_temp_nmp,1)

#abs change in spring temp
quantile(seasonal_temp_nmp$abs_change_spring_temperature,c(0.25,0.5,0.75))

#abs change in summer temp
quantile(seasonal_temp_nmp$abs_change_summer_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_nmp$perc_change_spring_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_nmp$perc_change_summer_temperature,c(0.25,0.5,0.75))

#cleanup
rm(seasonal_temp_nmp,seasonal_temp_sgs)

#-------------------------------------------------------------------------------
# seasonal change in VPD (works) -------

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- 
  read.csv(paste0( 'Data/Climate/Ecoregion/',
                   Ecoregion,
                   '/VPD/VPD_change.csv'))

#subset by season
seasonal_vpd_sgs_spring <- subset(seasonal_vpd_sgs,season == 'spring')
seasonal_vpd_sgs_summer <- subset(seasonal_vpd_sgs,season == 'summer')

#abs change in spring vpd
quantile(seasonal_vpd_sgs_spring$abs_change,c(0.25,0.5,0.75))

#abs change in summer vpd
quantile(seasonal_vpd_sgs_summer$abs_change,c(0.25,0.5,0.75))

#percent change in spring vpd
quantile(seasonal_vpd_sgs_spring$perc_change,c(0.25,0.5,0.75))

#percent change in summer vpd
quantile(seasonal_vpd_sgs_summer$perc_change,c(0.25,0.5,0.75))


#
#


#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- 
  read.csv(paste0( 'Data/Climate/Ecoregion/',
                   Ecoregion,
                   '/VPD/VPD_change.csv'))

#subset by season
seasonal_vpd_nmp_spring <- subset(seasonal_vpd_nmp,season == 'spring')
seasonal_vpd_nmp_summer <- subset(seasonal_vpd_nmp,season == 'summer')

#abs change in spring vpd
quantile(seasonal_vpd_nmp_spring$abs_change,c(0.25,0.5,0.75))

#abs change in summer vpd
quantile(seasonal_vpd_nmp_summer$abs_change,c(0.25,0.5,0.75))

#percent change in spring vpd
quantile(seasonal_vpd_nmp_spring$perc_change,c(0.25,0.5,0.75))

#percent change in summer vpd
quantile(seasonal_vpd_nmp_summer$perc_change,c(0.25,0.5,0.75))

#cleaup
rm(seasonal_vpd_nmp_spring,seasonal_vpd_nmp_summer,seasonal_vpd_sgs_spring,
   seasonal_vpd_sgs_summer,seasonal_vpd_nmp,seasonal_vpd_sgs)

#-------------------------------------------------------------------------------
# follow-up analysis of amount of GPP by season GPP (need to test) ------

#use cumulative GPP for this part

#shortgrass steppe

Ecoregion <- "shortgrass_steppe"
growth_curve_sgs <- read.csv(paste0('Data/GPP/Ecoregion/',Ecoregion,'average_growth_curve_',Ecoregion,'.csv'))

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
drought_growth_curve_sgs <- read.csv(paste0('Data/GPP/Ecoregion/',Ecoregion,'drought_growth_curve_',Ecoregion,'.csv'))
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

growth_curve_nmp <- read.csv(paste0('Data/GPP/Ecoregion/',Ecoregion,'/average_growth_curve_',Ecoregion,'.csv'))

#median total GPP during spring
end_spring <- growth_curve_nmp %>%
  dplyr::filter(doy=='151') 

spring_gpp = end_spring$mean

end_summer <- growth_curve_nmp %>%
  dplyr::filter(doy=='243')

summer_gpp = end_summer$mean - spring_gpp

#get difference
(summer_gpp - spring_gpp)/spring_gpp
summer_gpp/spring_gpp

#compare to drought year
drought_growth_curve_nmp <- read.csv(paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv'))
end_spring_drought <- drought_growth_curve_nmp %>%
  dplyr::filter(doy=='151') 

spring_gpp_drought = end_spring_drought$mean

end_summer_drought <- drought_growth_curve_nmp %>%
  dplyr::filter(doy=='243')

summer_gpp_drought = end_summer_drought$mean - spring_gpp_drought

#get difference
(summer_gpp_drought - spring_gpp_drought)/spring_gpp_drought
summer_gpp_drought/spring_gpp_drought

#-------------------------------------------------------------------------------
# compare distributions of day 75 by ecoregion (need to test) ------

#shortgrass steppe
day_75_drought_sgs <-
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_75_drought_impact_shortgrass_steppe.tif')
day_75_drought_sgs <- data.frame(rasterToPoints(day_75_drought_sgs))
day_75_drought_sgs <- day_75_drought_sgs %>%
  dplyr::rename('day_75' = 'day_75_drought_impact_shortgrass_steppe')

quantile(day_75_drought_sgs$day_75,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_75_drought_nmp <-
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_75_drought_impact_northern_mixed_prairies.tif')
day_75_drought_nmp <- data.frame(rasterToPoints(day_75_drought_nmp))
day_75_drought_nmp <- day_75_drought_nmp %>%
  dplyr::rename('day_75' = 'day_75_drought_impact_northern_mixed_prairies')

quantile(day_75_drought_nmp$day_75,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_75 <- ks_test_bootstrap(day_75_drought_nmp,day_75_drought_sgs)
#hist(compare_day_75$test.statistic)

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
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_50_drought_impact_shortgrass_steppe.tif')
day_50_drought_sgs <- data.frame(rasterToPoints(day_50_drought_sgs))
day_50_drought_sgs <- day_50_drought_sgs %>%
  dplyr::rename('day_50' = 'day_50_drought_impact_shortgrass_steppe')

quantile(day_50_drought_sgs$day_50,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_50_drought_nmp <-
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_50_drought_impact_northern_mixed_prairies.tif')
day_50_drought_nmp <- data.frame(rasterToPoints(day_50_drought_nmp))
day_50_drought_nmp <- day_50_drought_nmp %>%
  dplyr::rename('day_50' = 'day_50_drought_impact_northern_mixed_prairies')

quantile(day_50_drought_nmp$day_50,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_50 <- ks_test_bootstrap(day_50_drought_nmp,day_50_drought_sgs)
#hist(compare_day_50$test.statistic)

#look at left tail of D statistic
quantile(compare_day_50$test.statistic,probs= c(0.01,0.5))

#look at correlation with latitude
day_50_all <- rbind(day_50_drought_nmp,day_50_drought_sgs)
cor.test(day_50_all$y,day_50_all$day_50 ,method='spearman',exact=FALSE)


#variogram analysis to assess spatial autocorrelation in day 50

#sgs
day_50_drought_sgs <-
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_50_drought_impact_shortgrass_steppe.tif')
point_data_50_sgs <- as(day_50_drought_sgs, 'SpatialPointsDataFrame')

TheVariogram_50_sgs = variogram(day_50_drought_impact_shortgrass_steppe ~1,
                                data = point_data_50_sgs,width = 10)

summary(TheVariogram_50_sgs)
plot(TheVariogram_50_sgs)

TheVariogramModel_50_sgs <- vgm(psill=300, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs) 
FittedModel_50_sgs <- fit.variogram(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs)
FittedModel_50_sgs

#northern mixed prairies
day_50_drought_nmp <-
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_50_drought_impact_northern_mixed_prairies.tif')

point_data_50_nmp <- as(day_50_drought_nmp, 'SpatialPointsDataFrame')

TheVariogram_50_nmp = variogram(day_50_drought_impact_northern_mixed_prairies ~1,
                                data = point_data_50_nmp,width = 10)

summary(TheVariogram_50_nmp)
plot(TheVariogram_50_nmp)

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
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_25_drought_impact_shortgrass_steppe.tif')
day_25_drought_sgs <- data.frame(rasterToPoints(day_25_drought_sgs))
day_25_drought_sgs <- day_25_drought_sgs %>%
  dplyr::rename('day_25' = 'day_25_drought_impact_shortgrass_steppe')

quantile(day_25_drought_sgs$day_25,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_25_drought_nmp <-
  raster('Data/GPP/Ecoregion/',Ecoregion,'/day_25_drought_impact_northern_mixed_prairies.tif')
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
# % of GPP in months not analyzed (works) ----

#shortgrass
Ecoregion <- 'shortgrass_steppe'
sgs_annual_gpp <-
  read.csv(paste0('Data/GPP/Ecoregion/',Ecoregion,'/full_year_subset.csv'))

#full year
sgs_annual_gpp_full_year <- aggregate(gpp_mean ~ x + y + year,sum,data = sgs_annual_gpp)

#growing season subset
sgs_annual_gpp_subset <- sgs_annual_gpp %>%
  dplyr::filter(doy < 57 | doy > 297) %>%
  dplyr::group_by(x,y,year) %>%
  dplyr::summarise(shoulder_season_sum = sum(gpp_mean))

sgs_gs_annual_gpp_sums <- merge(sgs_annual_gpp_subset,sgs_annual_gpp_full_year,
                                by=c('x','y','year'))

sgs_gs_annual_gpp_sums$perc <- 
  (sgs_gs_annual_gpp_sums$shoulder_season_sum/sgs_gs_annual_gpp_sums$gpp_mean)*100

#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
nmp_annual_gpp <-
  read.csv(paste0('Data/GPP/Ecoregion/',Ecoregion,'/full_year_subset.csv'))

#full year
nmp_annual_gpp_full_year <- aggregate(gpp_mean ~ x + y + year,sum,data = nmp_annual_gpp)

#growing season subset
nmp_annual_gpp_subset <- nmp_annual_gpp %>%
  dplyr::filter(doy < 57 | doy > 297) %>%
  dplyr::group_by(x,y,year) %>%
  summarise(shoulder_season_sum = sum(gpp_mean))

nmp_gs_annual_gpp_sums <- merge(nmp_annual_gpp_subset,nmp_annual_gpp_full_year,
                                by=c('x','y','year'))

nmp_gs_annual_gpp_sums$perc <- 
  (nmp_gs_annual_gpp_sums$shoulder_season_sum/nmp_gs_annual_gpp_sums$gpp_mean)*100


#-------------------------------------------------------------------------------
