

#get absolute and % reduction in GPP throughout the growing season

# setup----
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

# get splines -----

#create a vector of unique sites IDs
id_list <- unique(ppt_gpp$id_value)

# average growth 
with_progress({
  p <- progressor(along = id_list)
  growth_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline(i)
  })
})


# drought growth curve
with_progress({
  p <- progressor(along = id_list)
  growth_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline_drought(i)
  })
})

#get IQR for each day of the prediction
doy_list <- c(65:297)

#loop
gpp_predicted_list_average <- list()
gpp_predicted_list_drought <- list()
gpp_reduction_list <- list()
gpp_reduction_list_2 <- list()

for(i in doy_list){
  
  for(j in id_list){
    
    #average
    gpp_predicted_average <- data.frame(predict(growth_spline_list[[j]], i))
    gpp_predicted_average$id_val <- j
    gpp_predicted_list_average[[j]] <- gpp_predicted_average
    
    #drought
    gpp_predicted_drought <- data.frame(predict(growth_drought_spline_list[[j]], i))
    gpp_predicted_drought$id_val <- j
    gpp_predicted_list_drought[[j]] <- gpp_predicted_drought
    
  }
  
  #convert to dataframe and remove values/pixels below zero and extreme high values/outliers
  
  #average
  gpp_predicted_list_average_df <- list_to_df(gpp_predicted_list_average)
  colnames(gpp_predicted_list_average_df) <- c('doy','gpp_average','id_val')
  gpp_predicted_list_average_df <- gpp_predicted_list_average_df %>%
    dplyr::filter(gpp_average > 0) 
  
  #drought
  gpp_predicted_list_drought_df <- list_to_df(gpp_predicted_list_drought)
  colnames(gpp_predicted_list_drought_df) <- c('doy','gpp_drought','id_val')
  gpp_predicted_list_drought_df <- gpp_predicted_list_drought_df %>%
    dplyr::filter(gpp_drought > 0)
  
  hist(gpp_predicted_list_drought_df$gpp_drought,main = i)

  gpp_predicted_drought_average <- merge(gpp_predicted_list_drought_df,gpp_predicted_list_average_df,
                                         by=c('doy','id_val'))
  
  ss <- nrow(gpp_predicted_drought_average)
  
  gpp_predicted_drought_average_3 <- gpp_predicted_drought_average #use for absolute change calculation
  
  #relative
  gpp_predicted_drought_average$perc_change <- ((gpp_predicted_drought_average$gpp_drought -
    gpp_predicted_drought_average$gpp_average)/gpp_predicted_drought_average$gpp_average)*100

  #get average
  gpp_predicted_drought_average_2 <- aggregate(perc_change~doy,median,data=gpp_predicted_drought_average)

  #get and add IQR
  gpp_predicted_drought_average_2$ci_75 <- quantile(gpp_predicted_drought_average$perc_change,probs=0.75)
  gpp_predicted_drought_average_2$ci_25 <- quantile(gpp_predicted_drought_average$perc_change,probs=0.25)
  gpp_predicted_drought_average_2$sample_size <- ss
  gpp_reduction_list[[i]] <- gpp_predicted_drought_average_2
  
  #absolute
  gpp_predicted_drought_average_3$abs_change <- gpp_predicted_drought_average_3$gpp_drought -
                                                   gpp_predicted_drought_average$gpp_average
  
  #get mean
  gpp_predicted_drought_average_4 <- aggregate(abs_change~doy,median,data=gpp_predicted_drought_average_3)
  
  #get and add IQR
  gpp_predicted_drought_average_4$ci_75 <- quantile(gpp_predicted_drought_average_3$abs_change,probs=0.75)
  gpp_predicted_drought_average_4$ci_25 <- quantile(gpp_predicted_drought_average_3$abs_change,probs=0.25)
  gpp_predicted_drought_average_4$sample_size <- ss
  gpp_reduction_list_2[[i]] <- gpp_predicted_drought_average_4
  
}

gpp_reduction_list_df <- list_to_df(gpp_reduction_list)

filename <- paste0('Output/drought_gpp_reduction_',Ecoregion,'.rds')
saveRDS(gpp_reduction_list_df,filename)

gpp_reduction_list_df_2 <- list_to_df(gpp_reduction_list_2)

filename <- paste0('Output/drought_gpp_reduction_absolute_',Ecoregion,'.rds')
saveRDS(gpp_reduction_list_df_2,filename)

#cleanup
rm(gpp_predicted_average,gpp_predicted_drought,gpp_predicted_drought_average,
   gpp_predicted_drought_average_2,gpp_predicted_drought_average_3,
   gpp_predicted_drought_average_4,gpp_predicted_list_average,
   gpp_predicted_list_average_df,gpp_predicted_list_drought,
   gpp_predicted_list_drought_df,gpp_reduction_list,gpp_reduction_list_2,
   gpp_reduction_list_df,gpp_reduction_list_df_2,growth_spline_list,
   growth_drought_spline_list,growth_drought_spline_list,ppt_gpp)


# end------


