
# this script estimates the day by which ~25% of total productivity
# has been reached between day of year 57 and 297 and also explores
# how that changes during years of low precipitation

#setup----
plan(multisession, workers = 10)
options(future.globals.maxSize= 8000 * 1024^2)
#https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters

#import data
ppt_gpp <- readRDS(paste0('ppt_gpp_',Ecoregion))

#create a vector of unique sites IDs
id_list <- unique(ppt_gpp$id_value)

#get typical day by which 25% of total GPP has occurred -----

with_progress({
  p <- progressor(along = id_list)
  gpp_25_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_25_gpp_no_drought(i)
  })
})


#collapse to dataframe
gpp_25 <- list_to_df(gpp_25_list)
rm(gpp_25_list)

#filter out extreme high and low values
gpp_25 <- gpp_25 %>%
  dplyr::filter(doy_25 < 297) %>%
  dplyr::filter(doy_25 > 65)

#save as raster
gpp_25 <- rasterFromXYZ(gpp_25)
crs(gpp_25) <- "+proj=longlat +datum=WGS84"

#find drought years for each pixel and see how day of 25% gpp changes ----

#split workload into two batches (half of dataset each)
midpoint <- round(length(id_list) / 2)
id_list_1 <- 1:midpoint

#options(future.globals.maxSize= 1000)
#first half of data:
with_progress({
  p <- progressor(along = id_list_1)
  gpp_25_drought_list <- future_lapply(id_list_1, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_25_gpp_drought(i)
  })
})

#collapse to dataframe
gpp_25_drought <- list_to_df(gpp_25_drought_list)
rm(gpp_25_drought_list)

#second half of data:
id_list_2 <- (midpoint + 1):length(id_list)

with_progress({
  p <- progressor(along = id_list_2)
  gpp_25_drought_list_2 <- future_lapply(id_list_2, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_25_gpp_drought(i)
  })
})

#collapse to dataframe
gpp_25_drought_2 <- list_to_df(gpp_25_drought_list_2)
rm(gpp_25_drought_list_2)

#combine two dataframes
gpp_25_drought_3 <- rbind(gpp_25_drought_2, gpp_25_drought)
rm(gpp_25_drought_2, gpp_25_drought)

#filter out extreme high and low values
gpp_25_drought_3 <- gpp_25_drought_3 %>%
  dplyr::filter(doy_25_drought < 297) %>%
  dplyr::filter(doy_25_drought > 65)

#save as raster
gpp_25_drought <- rasterFromXYZ(gpp_25_drought_3)
crs(gpp_25_drought) <- "+proj=longlat +datum=WGS84"

#calculate the difference between the two
day_25_diff <- raster::stack(gpp_25_drought, gpp_25)
day_25_diff_2 <- day_25_diff$doy_25_drought - day_25_diff$doy_25
rm(day_25_diff)

#save to file as raster 
filename <-
  paste0(paste0('Data/GPP/Ecoregion/',Ecoregion,'/day_25_drought_impact_',Ecoregion,'.tif'))
writeRaster(day_25_diff_2, filename,overwrite=TRUE)

#done ------

