library(data.table)
library(ggplot2)
library(cowplot)
library(stringr)
library(sf)
library(sp)
library(raster)
library(terra)
library(rNOMADS)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(rasterVis)
library(RColorBrewer)
library(ncdf4)
library(rgdal)

rm(list=ls())
gc()

# you will need to have downloaded relevant .nc files from the
# CPC Global Unified Gauge-Based Analysis of Daily Precipitation
# website available at 
# https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html
# and stored these .nc files in a folder called prec that is 
# a subfolder within the same folder where the present R file is;
# then proceed as follows.

# load the previusly stired conflict data (01-getacled.r) to select 
# the relevant southeast asia countries 
load("acled.RData")

countries <- unique(acled_dt$country)

# load the southeast asia raster
southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

yr_vec <- 1979:2023

yrs <- vector("list",length(yr_vec))

for(r in 1:length(yr_vec)){
  
  precipitation_nc <- brick(paste0("prec/precip.",yr_vec[r],".nc"))
  
  lst <- vector("list",nlayers(precipitation_nc))
  
  for(i in 1:nlayers(precipitation_nc)){
    
    p01 <- subset(precipitation_nc,i)
    p02 <- rotate(p01)
    p02a <- aggregate(p02,2,mean) # to get 1-degree cells, otherwise p02a <- p02
    p03 <- crop(p02a,seasia)
    dm_p <- data.table(rasterToPoints(p03))
    dm_p[,`:=`(year=substr(colnames(dm_p)[3],2,5),mo=substr(colnames(dm_p)[3],7,8),dy=substr(colnames(dm_p)[3],10,11))]
    colnames(dm_p)[3] <- "prec"
    dm_p <- dm_p[order(x,y)]
    lst[[i]] <- dm_p
    
  }
  
  precipitation_dt <- data.table(Reduce(rbind,lst))
  precipitation_dt <- precipitation_dt[,.(prec=sum(prec)),by=.(x,y,year,mo)]
  
  yrs[[r]] <- precipitation_dt
  
  print(r)
  
}

prec_dt <- data.table(Reduce(rbind,yrs))

save(prec_dt,file="precipitation.RData")

