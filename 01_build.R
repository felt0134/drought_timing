
#setup workspace

#check version
#R.version

#clearworkspace
rm(list=ls())

#load packages
pkgs <- c("raster",'tidyverse','data.table',
          'rstudioapi','readr')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#load functions
source('02_Functions.R')

#have to streamline this analysis to the main ones of gpp and precip, save ppt-gpp as .rds zip files
#and 