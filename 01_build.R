
#setup workspace

#check version
#R.version

#clearworkspace
rm(list=ls())

#load packages
pkgs <- c("raster",'tidyverse','data.table','readr','rstudioapi')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#load functions
source('02_Functions.R')

