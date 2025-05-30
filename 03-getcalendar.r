# load the packages
library(data.table)
library(ggplot2)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(R.utils)

rm(list=ls())
gc()

# you will need to have downloaded the relevant netCDF file 
# (for rice, main season, filled) from 
# https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/netcdf-0-5-degree/
# and stored it in the same folder as the present R file;
# then proceed as follows.

# load the conflict data to select the relevant southeast asia countries 
load("acled.RData")

countries <- unique(acled_dt$country)

# load the southeast asia raster
southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load and sort out rice production rasters
crop <- "Rice"

gunzip("Rice.crop.calendar.fill.nc.gz",remove=F,skip=T)

plant_srt <- raster("Rice.crop.calendar.fill.nc",varname="plant.start")
plant <- raster("Rice.crop.calendar.fill.nc",varname="plant")
plant_end <- raster("Rice.crop.calendar.fill.nc",varname="plant.end")

harvest_srt <- raster("Rice.crop.calendar.fill.nc",varname="harvest.start")
harvest <- raster("Rice.crop.calendar.fill.nc",varname="harvest")
harvest_end <- raster("Rice.crop.calendar.fill.nc",varname="harvest.end")

plant_srt10 <- aggregate(plant_srt,fact=2,fun=modal)
raster_mask <- mask(plant_srt10,southeastasia)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
dm_ps <- data.table(rm)
colnames(dm_ps)[3] <- "plant_srt"
dm_ps$plant_srt_date <- as.Date(dm_ps$plant_srt,origin="2000-01-01")
dm_ps$plant_srt <- as.numeric(substr(dm_ps$plant_srt_date,6,7))
dm_ps$plant_srt_date <- NULL
dm_ps <- dm_ps[order(x,y)]

plant10 <- aggregate(plant,fact=2,fun=modal)
raster_mask <- mask(plant10,southeastasia)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
dm_p <- data.table(rm)
colnames(dm_p)[3] <- "plant"
dm_p$plant_date <- as.Date(dm_p$plant,origin="2000-01-01")
dm_p$plant <- as.numeric(substr(dm_p$plant_date,6,7))
dm_p$plant_date <- NULL
dm_p <- dm_p[order(x,y)]

plant_end10 <- aggregate(plant_end,fact=2,fun=modal)
raster_mask <- mask(plant_end10,southeastasia)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
dm_pe <- data.table(rm)
colnames(dm_pe)[3] <- "plant_end"
dm_pe$plant_end_date <- as.Date(dm_pe$plant_end,origin="2000-01-01")
dm_pe$plant_end <- as.numeric(substr(dm_pe$plant_end_date,6,7))
dm_pe$plant_end_date <- NULL
dm_pe <- dm_pe[order(x,y)]

harvest_srt10 <- aggregate(harvest_srt,fact=2,fun=modal)
raster_mask <- mask(harvest_srt10,southeastasia)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
dm_hs <- data.table(rm)
colnames(dm_hs)[3] <- "harvest_srt"
dm_hs$harvest_srt_date <- as.Date(dm_hs$harvest_srt,origin="2000-01-01")
dm_hs$harvest_srt <- as.numeric(substr(dm_hs$harvest_srt_date,6,7))
dm_hs$harvest_srt_date <- NULL
dm_hs <- dm_hs[order(x,y)]

harvest10 <- aggregate(harvest,fact=2,fun=modal)
raster_mask <- mask(harvest10,southeastasia)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
dm_h <- data.table(rm)
colnames(dm_h)[3] <- "harvest"
dm_h$harvest_date <- as.Date(dm_h$harvest,origin="2000-01-01")
dm_h$harvest <- as.numeric(substr(dm_h$harvest_date,6,7))
dm_h$harvest_date <- NULL
dm_h <- dm_h[order(x,y)]

harvest_end10 <- aggregate(harvest_end,fact=2,fun=modal)
raster_mask <- mask(harvest_end10,southeastasia)
rm <- rasterToPoints(raster_mask)
rm[,1:2] <- round(rm[,1:2],1)
dm_he <- data.table(rm)
colnames(dm_he)[3] <- "harvest_end"
dm_he$harvest_end_date <- as.Date(dm_he$harvest_end,origin="2000-01-01")
dm_he$harvest_end <- as.numeric(substr(dm_he$harvest_end_date,6,7))
dm_he$harvest_end_date <- NULL
dm_he <- dm_he[order(x,y)]

h_dt <- Reduce(function(...) merge(...,by=c("x","y"),all=T),list(dm_ps,dm_p,dm_pe,dm_hs,dm_h,dm_he))
h_dt[,`:=`(plant_length=ifelse(plant_end-plant_srt+1>0,plant_end-plant_srt+1,plant_end-plant_srt+1+12),harvest_length=ifelse(harvest_end-harvest_srt+1>0,harvest_end-harvest_srt+1,harvest_end-harvest_srt+1+12))]

crop_dt <- Reduce(function(...) merge(..., by=c("x","y"),all=T),list(h_dt))

plant_dt <- crop_dt[,.(x,y,plant_srt,plant,plant_end,plant)]
colnames(plant_dt)[3:6] <- paste0(crop,c("_plant_srt","_plant","_plant_end","_plant"),sep="")

## plant calendars
plant_srt_dt <- matrix(0,nrow=nrow(plant_dt),ncol=12)
colnames(plant_srt_dt) <- as.character(1:12)
for(j in 1:nrow(plant_srt_dt)){
  if(!is.na(plant_dt[j,3])){
    plant_srt_dt[j,((as.numeric(plant_dt[j,3])):12)] <- 1:(12-as.numeric(plant_dt[j,3])+1)
    if(as.numeric(plant_dt[j,3])!=1){
      plant_srt_dt[j,(1:(as.numeric(plant_dt[j,3])-1))] <- (12-as.numeric(plant_dt[j,3])+2):12
    }
  }
}
plant_srt_dt <- as.data.table(plant_srt_dt)

plant_mid_dt <- matrix(0,nrow=nrow(plant_dt),ncol=12)
colnames(plant_mid_dt) <- as.character(1:12)
for(j in 1:nrow(plant_mid_dt)){
  if(!is.na(plant_dt[j,4])){
    plant_mid_dt[j,((as.numeric(plant_dt[j,4])):12)] <- 1:(12-as.numeric(plant_dt[j,4])+1)
    if(as.numeric(plant_dt[j,4])!=1){
      plant_mid_dt[j,(1:(as.numeric(plant_dt[j,4])-1))] <- (12-as.numeric(plant_dt[j,4])+2):12
    }
  }
}
plant_mid_dt <- as.data.table(plant_mid_dt)

plant_end_dt <- matrix(0,nrow=nrow(plant_dt),ncol=12)
colnames(plant_end_dt) <- as.character(1:12)
for(j in 1:nrow(plant_end_dt)){
  if(!is.na(plant_dt[j,5])){
    plant_end_dt[j,((as.numeric(plant_dt[j,5])):12)] <- 1:(12-as.numeric(plant_dt[j,5])+1)
    if(as.numeric(plant_dt[j,5])!=1){
      plant_end_dt[j,(1:(as.numeric(plant_dt[j,5])-1))] <- (12-as.numeric(plant_dt[j,5])+2):12
    }
  }
}
plant_end_dt <- as.data.table(plant_end_dt)


harvest_dt <- crop_dt[,.(x,y,harvest_srt,harvest,harvest_end,plant)]
colnames(harvest_dt)[3:6] <- paste0(crop,c("_harvest_srt","_harvest","_harvest_end","_plant"),sep="")

## harvest calendars
harvest_srt_dt <- matrix(0,nrow=nrow(harvest_dt),ncol=12)
colnames(harvest_srt_dt) <- as.character(1:12)
for(j in 1:nrow(harvest_srt_dt)){
  if(!is.na(harvest_dt[j,3])){
    harvest_srt_dt[j,((as.numeric(harvest_dt[j,3])):12)] <- 1:(12-as.numeric(harvest_dt[j,3])+1)
    if(as.numeric(harvest_dt[j,3])!=1){
      harvest_srt_dt[j,(1:(as.numeric(harvest_dt[j,3])-1))] <- (12-as.numeric(harvest_dt[j,3])+2):12
    }
  }
}
harvest_srt_dt <- as.data.table(harvest_srt_dt)

harvest_mid_dt <- matrix(0,nrow=nrow(harvest_dt),ncol=12)
colnames(harvest_mid_dt) <- as.character(1:12)
for(j in 1:nrow(harvest_mid_dt)){
  if(!is.na(harvest_dt[j,4])){
    harvest_mid_dt[j,((as.numeric(harvest_dt[j,4])):12)] <- 1:(12-as.numeric(harvest_dt[j,4])+1)
    if(as.numeric(harvest_dt[j,4])!=1){
      harvest_mid_dt[j,(1:(as.numeric(harvest_dt[j,4])-1))] <- (12-as.numeric(harvest_dt[j,4])+2):12
    }
  }
}
harvest_mid_dt <- as.data.table(harvest_mid_dt)

harvest_end_dt <- matrix(0,nrow=nrow(harvest_dt),ncol=12)
colnames(harvest_end_dt) <- as.character(1:12)
for(j in 1:nrow(harvest_end_dt)){
  if(!is.na(harvest_dt[j,5])){
    harvest_end_dt[j,((as.numeric(harvest_dt[j,5])):12)] <- 1:(12-as.numeric(harvest_dt[j,5])+1)
    if(as.numeric(harvest_dt[j,5])!=1){
      harvest_end_dt[j,(1:(as.numeric(harvest_dt[j,5])-1))] <- (12-as.numeric(harvest_dt[j,5])+2):12
    }
  }
}
harvest_end_dt <- as.data.table(harvest_end_dt)

plant_srt_comb_dt <- data.table(plant_dt[,1:2],plant_srt_dt)
plant_srt_long_dt <- data.table::melt(plant_srt_comb_dt,id.vars=c("x","y"),variable.name="mo",value.name=paste(crop,"_plant_srt",sep=""))
plant_srt_dt <- plant_srt_long_dt[order(x,y,mo)]

plant_mid_comb_dt <- data.table(plant_dt[,1:2],plant_mid_dt)
plant_mid_long_dt <- data.table::melt(plant_mid_comb_dt,id.vars=c("x","y"),variable.name="mo",value.name=paste(crop,"_plant_mid",sep=""))
plant_mid_dt <- plant_mid_long_dt[order(x,y,mo)]

plant_end_comb_dt <- data.table(plant_dt[,1:2],plant_end_dt)
plant_end_long_dt <- data.table::melt(plant_end_comb_dt,id.vars=c("x","y"),variable.name="mo",value.name=paste(crop,"_plant_end",sep=""))
plant_end_dt <- plant_end_long_dt[order(x,y,mo)]

harvest_srt_comb_dt <- data.table(harvest_dt[,1:2],harvest_srt_dt)
harvest_srt_long_dt <- data.table::melt(harvest_srt_comb_dt,id.vars=c("x","y"),variable.name="mo",value.name=paste(crop,"_harvest_srt",sep=""))
harvest_srt_dt <- harvest_srt_long_dt[order(x,y,mo)]

harvest_mid_comb_dt <- data.table(harvest_dt[,1:2],harvest_mid_dt)
harvest_mid_long_dt <- data.table::melt(harvest_mid_comb_dt,id.vars=c("x","y"),variable.name="mo",value.name=paste(crop,"_harvest_mid",sep=""))
harvest_mid_dt <- harvest_mid_long_dt[order(x,y,mo)]

harvest_end_comb_dt <- data.table(harvest_dt[,1:2],harvest_end_dt)
harvest_end_long_dt <- data.table::melt(harvest_end_comb_dt,id.vars=c("x","y"),variable.name="mo",value.name=paste(crop,"_harvest_end",sep=""))
harvest_end_dt <- harvest_end_long_dt[order(x,y,mo)]
  

plant_srt_dt$Month <- month.abb[plant_srt_dt$mo]
plant_srt_dt$Month <- factor(plant_srt_dt$Month,levels=unique(plant_srt_dt$Month))

plant_mid_dt$Month <- month.abb[plant_mid_dt$mo]
plant_mid_dt$Month <- factor(plant_mid_dt$Month,levels=unique(plant_mid_dt$Month))

plant_end_dt$Month <- month.abb[plant_end_dt$mo]
plant_end_dt$Month <- factor(plant_end_dt$Month,levels=unique(plant_end_dt$Month))


harvest_srt_dt$Month <- month.abb[harvest_srt_dt$mo]
harvest_srt_dt$Month <- factor(harvest_srt_dt$Month,levels=unique(harvest_srt_dt$Month))

harvest_mid_dt$Month <- month.abb[harvest_mid_dt$mo]
harvest_mid_dt$Month <- factor(harvest_mid_dt$Month,levels=unique(harvest_mid_dt$Month))

harvest_end_dt$Month <- month.abb[harvest_end_dt$mo]
harvest_end_dt$Month <- factor(harvest_end_dt$Month,levels=unique(harvest_end_dt$Month))


save(plant_srt_dt,plant_mid_dt,plant_end_dt,harvest_srt_dt,harvest_mid_dt,harvest_end_dt,file="calendar.RData")



