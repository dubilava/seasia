library(data.table)
library(ggplot2)
library(cowplot)
library(stringr)
library(sf)
library(sp)
library(raster)
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

"%!in%" <- Negate("%in%")

## load the map of se asia
load("acled_seasia.RData")

countries <- unique(acled_dt$country)

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# africa <- ne_countries(continent = "africa",returnclass = "sf")

era5_nc <- brick("ERA5/era5prec.nc")

lst <- vector("list",nlayers(era5_nc))

rain_extent <- raster(extent(floor(extent(southeastasia))))
res(rain_extent) <- c(1,1)

for(i in 1:nlayers(era5_nc)){
  
  rain01 <- subset(era5_nc,i)
  rain02 <- aggregate(rain01,fact=4,fun=sum)
  rain03 <- resample(x=rain02,y=rain_extent,method="ngb")
  
  raster_mask <- mask(rain03,southeastasia)
  rm <- rasterToPoints(raster_mask)
  
  year <- substr(colnames(rm)[3],2,5)
  mo <- substr(colnames(rm)[3],7,8)
  dy <- substr(colnames(rm)[3],10,11)
  
  rm[,1:2] <- round(rm[,1:2],1)
  dm_p <- data.table(year,mo,dy,rm)
  colnames(dm_p)[6] <- "rain"
  dm_p <- dm_p[order(x,y)]
  
  lst[[i]] <- dm_p
  
  print(i)
  
}


yrs <- 1997:2022

yrs_dt <- data.table(year=yrs,mo=rep(12,length(yrs)))
yrs_dt$cummo <- cumsum(yrs_dt$mo)
yrs_dt$begmo <- yrs_dt$cummo-yrs_dt$mo+1

rain_ls <- vector("list",nrow(yrs_dt))

for(i in 1:nrow(yrs_dt)){
  
  rain1_dt <- Reduce(rbind,lst[yrs_dt$begmo[i]:yrs_dt$cummo[i]])
  rain1_dt <- rain1_dt[order(year,mo,x,y)]
  
  rain_ls[[i]] <- rain1_dt
  
  print(i)
  
}

rain_dt <- Reduce(rbind,rain_ls)


save(rain_dt,file="precipitation_new.RData")

