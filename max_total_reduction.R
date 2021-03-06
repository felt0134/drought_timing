
#get max and total c upktake reductions during drought for each pixel

# setup
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters

#import data
ppt_gpp <- readRDS(paste0('ppt_gpp_',Ecoregion))

id_list <- unique(ppt_gpp$id_value)

# run function on each pixel
with_progress({
  p <- progressor(along = id_list)
  max_total_reduction_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_max_total_reduction(i)
  })
})

max_total_reduction_df <- list_to_df(max_total_reduction_list)
rm(max_total_reduction_list)

filename <- paste0('Output/max_total_reduction_',Ecoregion,'.rds')
saveRDS(max_total_reduction_df, filename)

#cleanup
rm(ppt_gpp,max_total_reduction_df)
