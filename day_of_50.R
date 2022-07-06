
# this script estimates the day by which ~50% of total productivity
# has been reached between day of year 57 and 297 and also explores
# how that changes during years of low precipitation

#setup----
plan(multisession, workers = 10)
options(future.globals.maxSize= 8000 * 1024^2)
#https://github.com/satijalab/seurat/issues/1845
#options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
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

#get typical day at which 50% of total gpp has ocurred -----

with_progress({
  p <- progressor(along = id_list)
  gpp_50_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_50_gpp_no_drought(i)
  })
})


#collapse to dataframe
gpp_50 <- list_to_df(gpp_50_list)
rm(gpp_50_list)

#filter out extreme high and low values
gpp_50 <- gpp_50 %>%
  dplyr::filter(doy_50 < 297) %>%
  dplyr::filter(doy_50 > 65)

#save as raster
gpp_50 <- rasterFromXYZ(gpp_50)
crs(gpp_50) <- "+proj=longlat +datum=WGS84"

#find drought years for each pixel and see how day of 50% gpp changes ----

#split workload into two batches (half of dataset each)
midpoint <- round(length(id_list) / 2)
id_list_1 <- 1:midpoint

#options(future.globals.maxSize= 1000)
#first half of data:
with_progress({
  p <- progressor(along = id_list_1)
  gpp_50_drought_list <- future_lapply(id_list_1, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_50_gpp_drought(i)
  })
})

#collapse to dataframe
gpp_50_drought <- list_to_df(gpp_50_drought_list)
rm(gpp_50_drought_list)

#second half of data:
id_list_2 <- (midpoint + 1):length(id_list)

with_progress({
  p <- progressor(along = id_list_2)
  gpp_50_drought_list_2 <- future_lapply(id_list_2, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_50_gpp_drought(i)
  })
})

#collapse to dataframe
gpp_50_drought_2 <- list_to_df(gpp_50_drought_list_2)
rm(gpp_50_drought_list_2)

#combine two dataframes
gpp_50_drought_3 <- rbind(gpp_50_drought_2, gpp_50_drought)
rm(gpp_50_drought_2, gpp_50_drought)

#filter out extreme high and low values
gpp_50_drought_3 <- gpp_50_drought_3 %>%
  dplyr::filter(doy_50_drought < 297) %>%
  dplyr::filter(doy_50_drought > 65)

#save as raster
gpp_50_drought <- rasterFromXYZ(gpp_50_drought_3)
crs(gpp_50_drought) <- "+proj=longlat +datum=WGS84"

#calculate the difference between the two
day_50_diff <- raster::stack(gpp_50_drought, gpp_50)
day_50_diff_2 <- day_50_diff$doy_50_drought - day_50_diff$doy_50
rm(day_50_diff)

#save to file as raster 
filename <-
  paste0(paste0('Output/day_50_drought_impact_',Ecoregion,'.tif'))
writeRaster(day_50_diff_2, filename,overwrite=TRUE)

#cleanup
rm(gpp_50,gpp_50_drought,gpp_50_drought_3)

# end ------


