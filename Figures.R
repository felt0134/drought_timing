

# Key Figures 
library(scico)
library(broom)
library(sp)
library(grid)

#color schemes
#https://www.data-imaginist.com/2018/scico-and-the-colour-conundrum/

#prep ------

#get shapefiles of states and update projection for maps
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c(
  'Wyoming','Colorado','Oklahoma','Kansas',
  'Montana','New Mexico','Texas',
  'North Dakota','South Dakota','Nebraska'),]

states_all_sites <- sp::spTransform(states_all_sites, 
                                    CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')) 

#prep for ggplot
#https://stackoverflow.com/questions/62435609/plot-shapefile-with-ggplot2

states_all_sites_tidy <- tidy(states_all_sites)

#-------------------------------------------------------------------------------
# day of 75% carbon uptake in normal and dry years  -----

#import:

#sgs
day_75_drought_sgs <-
  raster('Output/day_75_drought_impact_shortgrass_steppe.tif')

#nmp
day_75_drought_nmp <-
  raster('Output/day_75_drought_impact_northern_mixed_prairies.tif')

#prep:

#combine
day_75_drought <-
  raster::merge(day_75_drought_nmp, day_75_drought_sgs,tolerance = 0.20)
day_75_drought <-projectRaster(day_75_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_75_drought_df <- data.frame(rasterToPoints(day_75_drought))

# % of pixels with negative values (advanced day75)
(day_75_drought_df %>%
    dplyr::filter(layer < 0) %>%
    dplyr::summarise(length(layer)))/(length(day_75_drought_df$layer))

#plot:

day_75_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_75_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought on day by which\n75% of total C uptake occurs (days)',
                   palette = 'roma',direction = -1,midpoint = 0) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#distribution of drought impact to 75% day

#prep

#nmp
day_75_drought_nmp_2_df <- data.frame(rasterToPoints(day_75_drought_nmp))
day_75_drought_nmp_2_df$region <- 'Northern mixed prairies'
colnames(day_75_drought_nmp_2_df) <- c('x','y','day_75','region')

#sgs
day_75_drought_sgs_2_df <- data.frame(rasterToPoints(day_75_drought_sgs))
day_75_drought_sgs_2_df$region <- 'Shortgrass steppe'
colnames(day_75_drought_sgs_2_df) <- c('x','y','day_75','region')

#join
day_75_drought_nmp_sgs_2_df <- rbind(day_75_drought_nmp_2_df,day_75_drought_sgs_2_df)

#plot
drought_day75_pdf <- ggplot(day_75_drought_nmp_sgs_2_df, aes(x = day_75, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day by which\n75% of total C uptake occurs (days)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_75_sgs_nmp_drought_map)
  print(drought_day75_pdf , vp = vp)
}

#save
png(height = 1700,width=2000,res=300,'Figures/day75_drought_inset_plot.png')

full()

dev.off()

#cleanup
rm(day_75_drought,day_75_drought_df,day_75_drought_nmp,day_75_drought_nmp_2_df,
   day_75_drought_sgs_2_df,day_75_drought_sgs, day_75_sgs_nmp_drought_map,
   drought_day75_pdf)


#follow up: plot the correlation of day 75 impact and latitude 

#cor.test(day_75_drought_nmp_sgs_2_df$y,day_75_drought_nmp_sgs_2_df$day_75 ,method='spearman',exact=FALSE)


day_75_lat_plot <- ggplot(day_75_drought_nmp_sgs_2_df, aes(x = y, y = day_75,color = region)) +
  geom_point(alpha=0.3) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  ylab("Effect of drought on day by which\n75% of total C uptake occurs (days)") +
  xlab('Latitude') +
  annotate("text", x=46, y=40, label= "Delayed") +
  annotate("text", x=46, y=-50, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save
png(height = 1500,width=2000,res=300,'Figures/day75_drought_latitude_relationship.png')

day_75_lat_plot

dev.off()

#cleanup
rm(day_75_lat_plot,day_75_drought_nmp_sgs_2_df)


#-------------------------------------------------------------------------------
# day of 50% carbon uptake in normal and dry years  -----

#import:

#sgs
day_50_drought_sgs <-
  raster('Output/day_50_drought_impact_shortgrass_steppe.tif')

#nmp
day_50_drought_nmp <-
  raster('Output/day_50_drought_impact_northern_mixed_prairies.tif')

#prep:

#combine
day_50_drought <-
  raster::merge(day_50_drought_nmp, day_50_drought_sgs,tolerance = 0.20)
day_50_drought <-projectRaster(day_50_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_drought_df <- data.frame(rasterToPoints(day_50_drought))

# % of pixels with negative values (advanced day50)
(day_50_drought_df %>%
    dplyr::filter(layer < 0) %>%
    dplyr::summarise(length(layer)))/(length(day_50_drought_df$layer))

#plot:

day_50_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_50_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought on day by which\nhalf of total C uptake occurs (days)',
                   palette = 'roma',direction = -1,midpoint = 0) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#distribution of drought impact to 50% day

#prep

#nmp
day_50_drought_nmp_2_df <- data.frame(rasterToPoints(day_50_drought_nmp))
day_50_drought_nmp_2_df$region <- 'Northern mixed prairies'
colnames(day_50_drought_nmp_2_df) <- c('x','y','day_50','region')

#sgs
day_50_drought_sgs_2_df <- data.frame(rasterToPoints(day_50_drought_sgs))
day_50_drought_sgs_2_df$region <- 'Shortgrass steppe'
colnames(day_50_drought_sgs_2_df) <- c('x','y','day_50','region')

#join
day_50_drought_nmp_sgs_2_df <- rbind(day_50_drought_nmp_2_df,day_50_drought_sgs_2_df)


#plot
drought_day50_pdf <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = day_50, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day by which\nhalf of total C uptake occurs (days)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_50_sgs_nmp_drought_map)
  print(drought_day50_pdf , vp = vp)
}

#save
png(height = 1700,width=2000,res=300,'Figures/day50_drought_inset_plot.png')

full()

dev.off()

#cleanup
rm(day_50_drought,day_50_drought_df,day_50_drought_nmp,day_50_drought_nmp_2_df,
   day_50_drought_sgs_2_df,day_50_drought_sgs, day_50_sgs_nmp_drought_map,
   drought_day50_pdf)


#follow up: correlation of day 50 impact and latitude 

#cor.test(day_50_drought_nmp_sgs_2_df$y,day_50_drought_nmp_sgs_2_df$day_50 ,method='spearman',exact=FALSE)

#plot
day_50_lat_plot <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = y, y = day_50,color = region)) +
  geom_point(alpha=0.3) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  ylab('Effect of drought by day by which half of\ntotal carbon uptake is achieved (days)') +
  xlab('Latitude') +
  annotate("text", x=46, y=40, label= "Delayed") +
  annotate("text", x=46, y=-50, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    #legend.position = c(0.82, 0.95),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save
png(height = 1500,width=2000,res=300,'Figures/day50_drought_latitude_relationship.png')

day_50_lat_plot

dev.off()

#cleanup
rm(day_50_lat_plot,day_50_drought_nmp_sgs_2_df,day_50_drought_nmp_sgs_2_df)

#-------------------------------------------------------------------------------
# day of 25% carbon uptake in normal and dry years  -----

#import:

#sgs
day_25_drought_sgs <-
  raster('Output/day_25_drought_impact_shortgrass_steppe.tif')

#nmp
day_25_drought_nmp <-
  raster('Output/day_25_drought_impact_northern_mixed_prairies.tif')

#prep:

#combine
day_25_drought <-
  raster::merge(day_25_drought_nmp, day_25_drought_sgs,tolerance = 0.20)
day_25_drought <-projectRaster(day_25_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_drought_df <- data.frame(rasterToPoints(day_25_drought))

# % of pixels with negative values (advanced day25)
(day_25_drought_df %>%
    dplyr::filter(layer < 0) %>%
    dplyr::summarise(length(layer)))/(length(day_25_drought_df$layer))

#plot:

day_25_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_25_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought on day by which\n25% of total C uptake occurs (days)',
                   palette = 'roma',direction=-1,midpoint = 0) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#distribution of drought impact to 25% day

#nmp
day_25_drought_nmp_2_df <- data.frame(rasterToPoints(day_25_drought_nmp))
day_25_drought_nmp_2_df$region <- 'Northern mixed prairies'
colnames(day_25_drought_nmp_2_df) <- c('x','y','day_25','region')

#sgs
day_25_drought_sgs_2_df <- data.frame(rasterToPoints(day_25_drought_sgs))
day_25_drought_sgs_2_df$region <- 'Shortgrass steppe'
colnames(day_25_drought_sgs_2_df) <- c('x','y','day_25','region')

#join
day_25_drought_nmp_sgs_2_df <- rbind(day_25_drought_nmp_2_df,day_25_drought_sgs_2_df)

#plot
drought_day25_pdf <- ggplot(day_25_drought_nmp_sgs_2_df, aes(x = day_25, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day by which\n25% of total C uptake occurs (days)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_25_sgs_nmp_drought_map)
  print(drought_day25_pdf , vp = vp)
}

#save
png(height = 1700,width=2000,res=300,'Figures/day25_drought_inset_plot.png')

full()

dev.off()

#cleanup
rm(day_25_drought,day_25_drought_df,day_25_drought_nmp,day_25_drought_nmp_2_df,
   day_25_drought_sgs_2_df,day_25_drought_sgs, day_25_sgs_nmp_drought_map,
   drought_day25_pdf)


#follow up: correlation of day 25 impact and latitude 

#cor.test(day_25_drought_nmp_sgs_2_df$y,day_25_drought_nmp_sgs_2_df$day_25 ,method='spearman',exact=FALSE)

#save
day_25_lat_plot <- ggplot(day_25_drought_nmp_sgs_2_df, aes(x = y, y = day_25,color = region)) +
  geom_point(alpha=0.5) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  ylab("Effect of drought on day by which\n25% of total C uptake occurs (days)") +
  xlab('Latitude') +
  annotate("text", x=46, y=40, label= "Delayed") +
  annotate("text", x=46, y=-50, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

png(height = 1500,width=2000,res=300,'Figures/day25_drought_latitude_relationship.png')

day_25_lat_plot

dev.off()

#cleanup
rm(day_25_lat_plot,day_25_drought_nmp_sgs_2_df,day_25_drought_nmp_sgs_2_df)

#-------------------------------------------------------------------------------
# cumulative carbon uptake in average and dry years ------

#import:

#import SGS

#mean
growth_curve_absolute_mean_sgs <- 
  readRDS('Output/average_growth_curve_shortgrass_steppe.rds')

#drought
growth_curve_drought_absolute_mean_sgs <- 
  readRDS('Output/drought_growth_curve_shortgrass_steppe.rds')

#import NMP

#mean
growth_curve_absolute_mean_nmp <- 
  readRDS('Output/average_growth_curve_northern_mixed_prairies.rds')

#drought
growth_curve_drought_absolute_mean_nmp <- 
  readRDS('Output/drought_growth_curve_northern_mixed_prairies.rds')

#plot:

png(height = 1500,width=1500,res=300,'Figures/multi_panel_growth_curves_2')

par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))

# plot it out panel A: sgs
plot(mean ~ doy, growth_curve_absolute_mean_sgs,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,391))
polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
        c(growth_curve_absolute_mean_sgs$spatial_ci_25,rev(growth_curve_absolute_mean_sgs$spatial_ci_75)),
        col=adjustcolor("grey",alpha.f=0.50), border = F)
polygon(c(growth_curve_drought_absolute_mean_sgs$doy,rev(growth_curve_drought_absolute_mean_sgs$doy)),
        c(growth_curve_drought_absolute_mean_sgs$spatial_25,rev(growth_curve_drought_absolute_mean_sgs$spatial_75)),
        col=adjustcolor("red",alpha.f=0.40), border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_sgs,col='red',pch=19,lwd=1.25)
lines(mean ~ doy, growth_curve_absolute_mean_sgs,col='black',pch=19,lwd=1.25)
legend(75, 350, legend=c("Average year", "Drought year"),       
       col=c("black", "red"), lty=1.1,lwd=4,cex=1.25,box.lty=0)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1)
mtext('a',side=3,line=0.5,cex=1,adj=-0.05)

# plot it out panel B: nmp
plot(mean ~ doy, growth_curve_absolute_mean_nmp,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,580))
polygon(c(growth_curve_absolute_mean_nmp$doy,rev(growth_curve_absolute_mean_nmp$doy)),
        c(growth_curve_absolute_mean_nmp$spatial_ci_25,rev(growth_curve_absolute_mean_nmp$spatial_ci_75)),
        col=adjustcolor("grey",alpha.f=0.50), border = F)
polygon(c(growth_curve_drought_absolute_mean_nmp$doy,rev(growth_curve_drought_absolute_mean_nmp$doy)),
        c(growth_curve_drought_absolute_mean_nmp$spatial_25,rev(growth_curve_drought_absolute_mean_nmp$spatial_75)),
        col=adjustcolor("red",alpha.f=0.40), border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_nmp,col='red',pch=19,lwd=1.25)
lines(mean ~ doy, growth_curve_absolute_mean_nmp,col='black',pch=19,lwd=1.25)
mtext('Julian day of year',side=1,line=3.75,cex=1.25)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1)
mtext('b',side=3,line=0.5,cex=1,adj=-0.05)
mtext(expression("Cumulative carbon uptake " (g~C~m^-2)),side=2,line=4.5,adj=-.15,cex=1.25)

dev.off()


#-------------------------------------------------------------------------------
# absolute change in carbon uptake dynamics  ------
  
#import:
  
#import sgs
  growth_drynamics_absolute_sgs <- 
    readRDS('Output/drought_gpp_reduction_absolute_shortgrass_steppe.rds')
  
  #import NMP
  growth_drynamics_absolute_nmp <- 
    readRDS('Output/drought_gpp_reduction_absolute_northern_mixed_prairies.rds')
  
  #plot/save:
  
  #filepath
  png(height = 3000,width=3000,res=300,'Figures/multi_panel_gpp_impacts_absolute.png')
  
  #setup
  par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))
  
  #sgs
  plot(abs_change~doy,data=growth_drynamics_absolute_sgs,type='l',
       xlab='',ylab='',las=1,cex.axis=2,ylim=c(-26,5))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_drynamics_absolute_sgs$doy,rev(growth_drynamics_absolute_sgs$doy)),
          c(growth_drynamics_absolute_sgs$ci_75,rev(growth_drynamics_absolute_sgs$ci_25)),
          col = "black", border = F)
  text(100, -20, "Spring",cex=3)
  text(201, -5, "Summer",cex=3)
  text(275, -20, "Fall",cex=3)
  text(201, 1, "Median carbon uptake",cex=2.5)
  abline(h=0,col='black',lty='dashed')
  mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)
  mtext('a',side=3,line=0.5,cex=2,adj=0.0)
  lines(abs_change~doy,data=growth_drynamics_absolute_sgs,type='l',col='white',lwd=2)
  
  #nmp
  plot(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',
       xlab='',ylab='',las=1,cex.axis=2,ylim=c(-35,10))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_drynamics_absolute_nmp$doy,rev(growth_drynamics_absolute_nmp$doy)),
          c(growth_drynamics_absolute_nmp$ci_25,rev(growth_drynamics_absolute_nmp$ci_75)),
          col = "black", border = F)
  abline(h=0,col='black',lty='dashed')
  mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)
  mtext('Day of year',side=1,line=4.5,cex=2.5)
  mtext(expression("Drought impact to carbon uptake "(g~C~m^-2~'16 days')),side=2,line=4,adj = -0.1, cex=2.25)
  mtext('b',side=3,line=0.5,cex=2,adj=0.0)
  lines(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',col='white',lwd=4)
  
  dev.off()


#-------------------------------------------------------------------------------
# % change in carbon uptake dynamics ------

#import:

#import sgs
  growth_drynamics_sgs <- 
    readRDS('Output/drought_gpp_reduction_shortgrass_steppe.rds')
  
#import NMP
  growth_drynamics_nmp <- 
    readRDS('Output/drought_gpp_reduction_northern_mixed_prairies.rds')
  

#plot and save:
  
png(height = 3000,width=3000,res=300,'Figures/multi_panel_gpp_impacts_relative.png')

#setup
par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))


#sgs
plot(perc_change~doy,data=growth_drynamics_sgs,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-80,55))
rect(151,-90,243,350,col = 'grey95')
rect(60,-90,151,350,col = 'grey')
polygon(c(growth_drynamics_sgs$doy,rev(growth_drynamics_sgs$doy)),
        c(growth_drynamics_sgs$ci_75,rev(growth_drynamics_sgs$ci_25)),
        col = "black", border = F)
text(100, -50, "Spring",cex=3)
text(200, -20, "Summer",cex=3)
text(275, 20, "Fall",cex=3)
text(200, 5, "Median carbon uptake",cex=2)
abline(h=0,col='black',lty='dashed')
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)
mtext('a',side=3,line=0.5,cex=2,adj=0.0)
lines(perc_change~doy,data=growth_drynamics_sgs,type='l',col='white',lwd=2)

#nmp
plot(perc_change~doy,data=growth_drynamics_nmp,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-60,120))
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_nmp$doy,rev(growth_drynamics_nmp$doy)),
        c(growth_drynamics_nmp$ci_25,rev(growth_drynamics_nmp$ci_75)),
        col = "black", border = F)
abline(h=0,col='black',lty='dashed')
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)
mtext('Day of year',side=1,line=4.5,cex=2.5)
mtext('% Change in carbon uptake',side=2,line=4.5,adj = -1.75, cex=2.5)
mtext('b',side=3,line=0.5,cex=2,adj=0.0)
lines(perc_change~doy,data=growth_drynamics_nmp,type='l',col='white',lwd=2)

dev.off()

#-------------------------------------------------------------------------------
# total reduction in carbon uptake (%)  -----

#import:

#import sgs
max_total_reduction_sgs_df <- 
  readRDS('Output/max_total_reduction_shortgrass_steppe.rds')

#import nmp
max_total_reduction_nmp_df <- 
  readRDS('Output/max_total_reduction_northern_mixed_prairies.rds')

#prep:

#import and get total for sgs
total_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'total') %>%
  dplyr::select(x,y,perc_reduction)

total_reduction_sgs <- rasterFromXYZ(total_reduction_sgs)
crs(total_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
total_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'total') %>%
  dplyr::select(x,y,perc_reduction)

total_reduction_nmp <- rasterFromXYZ(total_reduction_nmp)
crs(total_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
total_reduction <- merge(total_reduction_nmp,total_reduction_sgs,tolerance = 0.20)

total_reduction <-projectRaster(total_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
total_reduction <- data.frame(rasterToPoints(total_reduction))

#plot:

total_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data = total_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Change in total carbon uptake (%)',
                   palette = 'roma',direction = 1,midpoint = 0) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#now do distributions 

total_reduction_nmp <- data.frame(rasterToPoints(total_reduction_nmp))
total_reduction_nmp$Ecoregion <- 'Northern mixed prairies'

total_reduction_sgs <- data.frame(rasterToPoints(total_reduction_sgs))
total_reduction_sgs$Ecoregion <- 'Shortgrass steppe'

total_reduction_rbind <- rbind(total_reduction_nmp,total_reduction_sgs)

total_reduction_pdf <- ggplot(total_reduction_rbind, aes(x = perc_reduction, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab("Change in total carbon uptake (%)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 9),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(total_reduction_map)
  print(total_reduction_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/total_reduction_map.png')

full()

dev.off()

#cleanup
rm(max_total_reduction_nmp_df,max_total_reduction_sgs_df,
   total_reduction_map,total_reduction_nmp,total_reduction_sgs,
   total_reduction_rbind,total_reduction_pdf,vp,total_reduction)



#-------------------------------------------------------------------------------
# peak reduction in carbon uptake  UPDATE  ------

#import:

#import sgs
max_total_reduction_sgs_df <- 
  readRDS('Output/max_total_reduction_shortgrass_steppe.rds')

#import nmp
max_total_reduction_nmp_df <- 
  readRDS('Output/max_total_reduction_northern_mixed_prairies.rds')

#prep:

#import and get total for sgs
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297) %>%
  dplyr::select(x,y,perc_reduction)

max_reduction_sgs <- rasterFromXYZ(max_reduction_sgs)
crs(max_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#magnitude of peak reduction

#import and get total for nmp
max_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297) %>%
  dplyr::select(x,y,perc_reduction)

max_reduction_nmp <- rasterFromXYZ(max_reduction_nmp)
crs(max_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
max_reduction <- merge(max_reduction_nmp,max_reduction_sgs,tolerance = 0.20)

max_reduction <-projectRaster(max_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_reduction <- data.frame(rasterToPoints(max_reduction))

#plot:

max_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Maximum reduction\nin carbon uptake (%)',
                   palette = 'batlow',direction = 1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#day of peak reduction

#import and get total for sgs
max_reduction_doy_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::select(x,y,doy) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297)

max_reduction_doy_sgs <- rasterFromXYZ(max_reduction_doy_sgs)
crs(max_reduction_doy_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
max_reduction_doy_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::select(x,y,doy) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297)

max_reduction_doy_nmp <- rasterFromXYZ(max_reduction_doy_nmp)
crs(max_reduction_doy_nmp) <- "+proj=longlat +datum=WGS84"

#combine
max_reduction_doy <- merge(max_reduction_doy_nmp,max_reduction_doy_sgs,tolerance = 0.20)

max_reduction_doy <-projectRaster(max_reduction_doy, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_reduction_doy <- data.frame(rasterToPoints(max_reduction_doy))

max_reduction_doy_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_reduction_doy, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of maximum reduction\nin carbon uptake',
                   palette = 'batlow',direction = 1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#now do distributions

max_reduction_doy_nmp <- data.frame(rasterToPoints(max_reduction_doy_nmp))
max_reduction_doy_nmp$ecoregion <- 'Northern mixed prairies'

max_reduction_doy_sgs <- data.frame(rasterToPoints(max_reduction_doy_sgs))
max_reduction_doy_sgs$ecoregion <- 'Shortgrass steppe'

max_reduction_doy_rbind <- rbind(max_reduction_doy_nmp,max_reduction_doy_sgs)

#peak reduction day of year distribution
max_reduction_doy_pdf <- ggplot(max_reduction_doy_rbind, aes(x = doy, fill = ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab("Day of maximum reduction in carbon uptake") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


max_reduction_nmp <- data.frame(rasterToPoints(max_reduction_nmp))
max_reduction_nmp$ecoregion <- 'Northern mixed prairies'

max_reduction_sgs <- data.frame(rasterToPoints(max_reduction_sgs))
max_reduction_sgs$ecoregion <- 'Shortgrass steppe'

max_reduction_rbind <- rbind(max_reduction_nmp,max_reduction_sgs)

#peak reduction distributions
max_reduction_pdf <- ggplot(max_reduction_rbind, aes(x = perc_reduction, fill = ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab("Maximum reduction in carbon uptake (%)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

library(patchwork)

#save
png(height = 3000,width=2500,res=300,'Figures/day_of_max_reduction.png')

p123 <- max_reduction_map + max_reduction_doy_map + max_reduction_pdf +
  max_reduction_doy_pdf + plot_layout(ncol = 2)
p123 + plot_annotation(tag_levels = "a")

dev.off()

#cleanup
rm(max_reduction,max_reduction_doy,max_reduction_doy_map,
   max_reduction_doy_nmp,max_reduction_doy_pdf,max_reduction_doy_rbind,
   max_reduction_doy_sgs,max_reduction_map,max_reduction_nmp,
   max_reduction_pdf,max_reduction_rbind,
   max_total_reduction_nmp_df,max_total_reduction_sgs_df,p123,max_reduction_sgs)

#-------------------------------------------------------------------------------
# total reduction in carbon uptake (absolute) -------

#import:

#import sgs
max_total_reduction_sgs_df <- 
  readRDS('Output/max_total_reduction_shortgrass_steppe.rds')

#import nmp
max_total_reduction_nmp_df <- 
  readRDS('Output/max_total_reduction_northern_mixed_prairies.rds')

#prep:

total_abs_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'total') %>%
  dplyr::select(x,y,reduction)

total_abs_reduction_sgs <- rasterFromXYZ(total_abs_reduction_sgs)
crs(total_abs_reduction_sgs) <- "+proj=longlat +datum=WGS84"


total_abs_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'total') %>%
  dplyr::select(x,y,reduction)

total_abs_reduction_nmp <- rasterFromXYZ(total_abs_reduction_nmp)
crs(total_abs_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
total_abs_reduction <- merge(total_abs_reduction_nmp,total_abs_reduction_sgs,tolerance = 0.20)

total_abs_reduction <-projectRaster(total_abs_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
total_abs_reduction <- data.frame(rasterToPoints(total_abs_reduction))

#plot:

total_abs_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data = total_abs_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico(bquote('Change in total carbon uptake ('*'g C'~ m^-2*')'),
                   palette = 'roma',direction = 1,midpoint = 0) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#now do distributions

total_abs_reduction_nmp <- data.frame(rasterToPoints(total_abs_reduction_nmp))
total_abs_reduction_nmp$Ecoregion <- 'Northern mixed prairies'

total_abs_reduction_sgs <- data.frame(rasterToPoints(total_abs_reduction_sgs))
total_abs_reduction_sgs$Ecoregion <- 'Shortgrass steppe'

total_abs_reduction_rbind <- rbind(total_abs_reduction_nmp,total_abs_reduction_sgs)

total_abs_reduction_pdf <- ggplot(total_abs_reduction_rbind, aes(x = reduction, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab(bquote('Change in total carbon uptake ('*'g C'~ m^-2*')')) +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 9),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(total_abs_reduction_map)
  print(total_abs_reduction_pdf , vp = vp)
}

#save
png(height = 1700,width=2000,res=300,'Figures/total_abs_reduction_map.png')

full()

dev.off()

#cleanup
rm(max_total_reduction_nmp_df,max_total_reduction_sgs_df,
   total_abs_reduction_map,total_abs_reduction_nmp,total_abs_reduction_sgs,
   total_abs_reduction_rbind,total_abs_reduction_pdf,vp,total_abs_reduction)


#-------------------------------------------------------------------------------
# peak reduction in carbon uptake (absolute)  ------


#import:

#import sgs
max_total_reduction_sgs_df <- 
  readRDS('Output/max_total_reduction_shortgrass_steppe.rds')

#import nmp
max_total_reduction_nmp_df <- 
  readRDS('Output/max_total_reduction_northern_mixed_prairies.rds')

#import and get total for sgs
peak_abs_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::select(x,y,reduction)

peak_abs_reduction_sgs <- rasterFromXYZ(peak_abs_reduction_sgs)
crs(peak_abs_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get peak for nmp
peak_abs_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::select(x,y,reduction)

peak_abs_reduction_nmp <- rasterFromXYZ(peak_abs_reduction_nmp)
crs(peak_abs_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
peak_abs_reduction <- merge(peak_abs_reduction_nmp,peak_abs_reduction_sgs,tolerance = 0.20)

peak_abs_reduction <-projectRaster(peak_abs_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
peak_abs_reduction <- data.frame(rasterToPoints(peak_abs_reduction))

peak_abs_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data = peak_abs_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  ggtitle('Peak absolute reduction in carbon uptake') +
  scale_fill_scico(bquote(' '*'g C'~ m^-2~'16 days'*''),palette = 'batlow',direction = 1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    plot.title = element_text(size = 10, face = "bold"),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#now do PDF 

peak_abs_reduction_nmp <- data.frame(rasterToPoints(peak_abs_reduction_nmp))
peak_abs_reduction_nmp$Ecoregion <- 'Northern mixed prairies'

peak_abs_reduction_sgs <- data.frame(rasterToPoints(peak_abs_reduction_sgs))
peak_abs_reduction_sgs$Ecoregion <- 'Shortgrass steppe'

peak_abs_reduction_rbind <- rbind(peak_abs_reduction_nmp,peak_abs_reduction_sgs)

peak_abs_reduction_pdf <- ggplot(peak_abs_reduction_rbind, aes(x = reduction, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab (bquote('Peak reduction ('*'g C'~ m^-2~'16 days'*')')) +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 9),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#
#

#relative reductions

#import and get total for sgs
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297) %>%
  dplyr::select(x,y,perc_reduction)


max_reduction_sgs <- rasterFromXYZ(max_reduction_sgs)
crs(max_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
max_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297) %>%
  dplyr::select(x,y,perc_reduction)

max_reduction_nmp <- rasterFromXYZ(max_reduction_nmp)
crs(max_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
max_reduction <- merge(max_reduction_nmp,max_reduction_sgs,tolerance = 0.20)

max_reduction <-projectRaster(max_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_reduction <- data.frame(rasterToPoints(max_reduction))

max_reduction_rel_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  ggtitle('Peak relative reduction in carbon uptake') +
  scale_fill_scico('%',
                   palette = 'batlow',direction = 1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#now to PDFs
max_reduction_sgs <- data.frame(rasterToPoints(max_reduction_sgs))
max_reduction_sgs$ecoregion <- 'Shortgrass steppe'

max_reduction_nmp <- data.frame(rasterToPoints(max_reduction_nmp))
max_reduction_nmp$ecoregion <- 'Northern mixed prairies'

max_reduction_rbind <- rbind(max_reduction_nmp,max_reduction_sgs)

#peak reduction PDF
max_reduction_rel_pdf <- ggplot(max_reduction_rbind, aes(x = perc_reduction, fill = ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab("Peak reduction (%)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

library(patchwork)

png(height = 3000,width=2500,res=300,'Figures/rel_abs_max_reduction.png')

p123 <- peak_abs_reduction_map + max_reduction_rel_map +  
  peak_abs_reduction_pdf + max_reduction_rel_pdf +  plot_layout(ncol = 2)
p123 + plot_annotation(tag_levels = "a")

dev.off()

#cleanup
rm(max_total_reduction_nmp_df,max_total_reduction_sgs_df,
   peak_abs_reduction_map,peak_abs_reduction_nmp,peak_abs_reduction_sgs,
   peak_abs_reduction_rbind,peak_abs_reduction_pdf,peak_abs_reduction,max_reduction,
   max_reduction_nmp,max_reduction_rbind,max_reduction_rel_map,max_reduction_rel_pdf,
   max_reduction_sgs,p123)


#-------------------------------------------------------------------------------
# day of peak reduction map -------

#import:

#import sgs
max_total_reduction_sgs_df <- 
  readRDS('Output/max_total_reduction_shortgrass_steppe.rds')

#import nmp
max_total_reduction_nmp_df <- 
  readRDS('Output/max_total_reduction_northern_mixed_prairies.rds')


#import and get total for sgs
max_reduction_doy_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::select(x,y,doy) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297)

max_reduction_doy_sgs <- rasterFromXYZ(max_reduction_doy_sgs)
crs(max_reduction_doy_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
max_reduction_doy_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max') %>%
  select(x,y,doy) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297)

max_reduction_doy_nmp <- rasterFromXYZ(max_reduction_doy_nmp)
crs(max_reduction_doy_nmp) <- "+proj=longlat +datum=WGS84"

#combine
max_reduction_doy <- merge(max_reduction_doy_nmp,max_reduction_doy_sgs,tolerance = 0.20)

max_reduction_doy <-projectRaster(max_reduction_doy, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_reduction_doy <- data.frame(rasterToPoints(max_reduction_doy))

max_reduction_doy_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_reduction_doy, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of peak reduction\nin carbon uptake',
                   palette = 'batlow',direction = 1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#now do PDFs 

max_reduction_doy_nmp <- data.frame(rasterToPoints(max_reduction_doy_nmp))
max_reduction_doy_nmp$ecoregion <- 'Northern mixed prairies'

max_reduction_doy_sgs <- data.frame(rasterToPoints(max_reduction_doy_sgs))
max_reduction_doy_sgs$ecoregion <- 'Shortgrass steppe'

max_reduction_doy_rbind <- rbind(max_reduction_doy_nmp,max_reduction_doy_sgs)

#peak reduction day of year PDF
max_reduction_doy_pdf <- ggplot(max_reduction_doy_rbind, aes(x = doy, fill = ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  xlab("Day of peak reduction") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#make and save the inset plot
library(grid)
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(max_reduction_doy_map)
  print(max_reduction_doy_pdf , vp = vp)
}

png(height = 1700,width=2000,res=300,'Figures/day_of_max_reduction.png')

full()

dev.off()

#cleanup
rm(max_reduction_doy,max_reduction_doy_map,max_reduction_doy_nmp,
   max_reduction_doy_pdf,max_reduction_doy_rbind,max_reduction_doy_sgs,
   max_total_reduction_nmp_df,max_total_reduction_sgs_df)

#-------------------------------------------------------------------------------