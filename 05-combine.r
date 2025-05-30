library(data.table)
library(ggplot2)
library(cowplot)
library(stringr)
library(sf)
library(sp)
library(raster)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)

rm(list=ls())
gc()

"%!in%" <- Negate("%in%")


load("acled.RData")
load("calendar.RData")


# acled ----

acled_dt$lat <- as.numeric(acled_dt$latitude)
acled_dt$lon <- as.numeric(acled_dt$longitude)

acled_dt$latitude <- round((acled_dt$lat-0.5)*2)/2+0.5
acled_dt$longitude <- round((acled_dt$lon-0.5)*2)/2+0.5

acled_dt$yearmo <- as.factor(substr(acled_dt$event_date,1,7))
acled_dt$year <- as.factor(substr(acled_dt$event_date,1,4))
acled_dt$mo <- as.factor(substr(acled_dt$event_date,6,7))

acled_dt <- acled_dt[event_date >= "2010-01-01" & event_date <= "2023-12-31"]
acled_dt <- acled_dt[geo_precision %in% c(1,2)]

acled_sum_dt <- acled_dt[,.(incidents=.N,fatalities=sum(fatalities)),by=.(yearmo,year,mo,longitude,latitude,event_type)]

acled_sum_dt <- acled_sum_dt[order(year,mo,longitude,latitude)]

acled_sum_dt$xy <- as.factor(paste(acled_sum_dt$longitude,acled_sum_dt$latitude,sep=","))


## harvest ----

## harvest start
harvest_srt_dt <- harvest_srt_dt[,.(x,y,mo,Rice_harvest_srt)]
harvest_srt_dt$latitude <- as.numeric(harvest_srt_dt$y)
harvest_srt_dt$longitude <- as.numeric(harvest_srt_dt$x)

harvest_srt_dt[,`:=`(mo=as.factor(str_pad(mo,2,pad="0")),Rice_harvest_srt=as.factor(Rice_harvest_srt))]

harvest_srt_dt$xy <- as.factor(paste(harvest_srt_dt$longitude,harvest_srt_dt$latitude,sep=","))

## harvest mid
harvest_mid_dt <- harvest_mid_dt[,.(x,y,mo,Rice_harvest_mid)]

harvest_mid_dt$latitude <- as.numeric(harvest_mid_dt$y)
harvest_mid_dt$longitude <- as.numeric(harvest_mid_dt$x)

harvest_mid_dt[,`:=`(mo=as.factor(str_pad(mo,2,pad="0")),Rice_harvest_mid=as.factor(Rice_harvest_mid))]

harvest_mid_dt$xy <- as.factor(paste(harvest_mid_dt$longitude,harvest_mid_dt$latitude,sep=","))

## harvest end
harvest_end_dt <- harvest_end_dt[,.(x,y,mo,Rice_harvest_end)]

harvest_end_dt$latitude <- as.numeric(harvest_end_dt$y)
harvest_end_dt$longitude <- as.numeric(harvest_end_dt$x)

harvest_end_dt[,`:=`(mo=as.factor(str_pad(mo,2,pad="0")),Rice_harvest_end=as.factor(Rice_harvest_end))]

harvest_end_dt$xy <- as.factor(paste(harvest_end_dt$longitude,harvest_end_dt$latitude,sep=","))

harvest_dt <- Reduce(function(x,y) merge(x=x,y=y,by=c("x","y","mo","longitude","latitude","xy")),list(harvest_srt_dt,harvest_mid_dt,harvest_end_dt))


harvest_rice_dt <- harvest_mid_dt[Rice_harvest_mid%in%c(0,1),.(xy,x,y,Rice_harvest=Rice_harvest_mid)]
harvest_rice_dt <- unique(harvest_rice_dt)
harvest_rice_dt <- harvest_rice_dt[order(x,y,xy)]


crops_dt <- harvest_rice_dt


## plant ----

## plant start
plant_srt_dt <- plant_srt_dt[,.(x,y,mo,Rice_plant_srt)]

plant_srt_dt$latitude <- as.numeric(plant_srt_dt$y)
plant_srt_dt$longitude <- as.numeric(plant_srt_dt$x)

plant_srt_dt[,`:=`(mo=as.factor(str_pad(mo,2,pad="0")),Rice_plant_srt=as.factor(Rice_plant_srt))]

plant_srt_dt$xy <- as.factor(paste(plant_srt_dt$longitude,plant_srt_dt$latitude,sep=","))

## plant mid
plant_mid_dt <- plant_mid_dt[,.(x,y,mo,Rice_plant_mid)]

plant_mid_dt$latitude <- as.numeric(plant_mid_dt$y)
plant_mid_dt$longitude <- as.numeric(plant_mid_dt$x)

plant_mid_dt[,`:=`(mo=as.factor(str_pad(mo,2,pad="0")),Rice_plant_mid=as.factor(Rice_plant_mid))]

plant_mid_dt$xy <- as.factor(paste(plant_mid_dt$longitude,plant_mid_dt$latitude,sep=","))

## plant end
plant_end_dt <- plant_end_dt[,.(x,y,mo,Rice_plant_end)]

plant_end_dt$latitude <- as.numeric(plant_end_dt$y)
plant_end_dt$longitude <- as.numeric(plant_end_dt$x)

plant_end_dt[,`:=`(mo=as.factor(str_pad(mo,2,pad="0")),Rice_plant_end=as.factor(Rice_plant_end))]

plant_end_dt$xy <- as.factor(paste(plant_end_dt$longitude,plant_end_dt$latitude,sep=","))

plant_dt <- Reduce(function(x,y) merge(x=x,y=y,by=c("x","y","mo","longitude","latitude","xy")),list(plant_srt_dt,plant_mid_dt,plant_end_dt))


plant_rice_dt <- plant_mid_dt[Rice_plant_mid%in%c(0,1),.(xy,x,y,Rice_plant=Rice_plant_mid)]
plant_rice_dt <- unique(plant_rice_dt)
plant_rice_dt <- plant_rice_dt[order(x,y,xy)]

crops_dt <- merge(crops_dt,plant_rice_dt,by=c("xy","x","y"))


crops_dt$Rice_harvest <- NULL
crops_dt$Rice_plant <- NULL


calendar_dt <- merge(harvest_dt,plant_dt,by=c("x","y","xy","longitude","latitude","mo"))

cereals_dt <- merge(crops_dt,calendar_dt,by=c("x","y","xy"))


merge(acled_sum_dt,cereals_dt,by=c("xy","longitude","latitude","mo"),all.x=T)


# doing this next bit (until ###) to ensure that 
# the geocoordinates of the conflict and crop data match 
# (inefficient coding but gets the stuff done)

xy_acled_dt <- acled_sum_dt[,.(xy)]
xy_acled_dt <- unique(xy_acled_dt)

xy_cereals_dt <- cereals_dt[,.(xy)]
xy_cereals_dt <- unique(xy_cereals_dt)

xy_cereals_dt$longitude <- as.numeric(unlist(strsplit(as.character(xy_cereals_dt$xy),","))[c(T,F)])
xy_cereals_dt$latitude <- as.numeric(unlist(strsplit(as.character(xy_cereals_dt$xy),","))[c(F,T)])

xy_acled_dt$longitude <- as.numeric(unlist(strsplit(as.character(xy_acled_dt$xy),","))[c(T,F)])
xy_acled_dt$latitude <- as.numeric(unlist(strsplit(as.character(xy_acled_dt$xy),","))[c(F,T)])

xy_acled_dt <- xy_acled_dt[xy %!in% xy_cereals_dt$xy]

d <- pointDistance(xy_acled_dt[,.(longitude,latitude)],xy_cereals_dt[,.(longitude,latitude)],lonlat=T)

r <- apply(d,1,which.min)

p <- data.table(acled=xy_acled_dt$xy,cereal=xy_cereals_dt$xy[r])

colnames(p) <- c("xy","xy_cereals")

xy_acled_dt <- acled_sum_dt[,.(xy)]
xy_acled_dt <- unique(xy_acled_dt)

acled_xy_dt <- merge(acled_sum_dt,p,by="xy",all.x=T)
acled_xy_dt[!is.na(xy_cereals)]$xy <- acled_xy_dt[!is.na(xy_cereals)]$xy_cereals

acled_xy_dt$longitude <- as.numeric(unlist(strsplit(as.character(acled_xy_dt$xy),","))[c(T,F)])
acled_xy_dt$latitude <- as.numeric(unlist(strsplit(as.character(acled_xy_dt$xy),","))[c(F,T)])

acled_xy_dt$xy_cereals <- NULL

acled_sum_dt <- acled_xy_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,yearmo,year,mo,longitude,latitude,event_type)]

rm(d)

###

# making sure the location-periods when no conflict happens are not missing
xy_acled_dt <- acled_sum_dt[,.(xy)]
xy_acled_dt <- unique(xy_acled_dt)

xy_cereals_dt <- cereals_dt[,.(xy)]
xy_cereals_dt <- unique(xy_cereals_dt)

xy_dt <- merge(xy_acled_dt,xy_cereals_dt,all=T)
xy_dt <- unique(xy_dt)
xy <- as.character(xy_dt$xy)

yearmo <- substr(as.character(seq(as.Date("2010-01-01"),as.Date("2023-12-31"),by="month")),1,7)

xy_yearmo <- CJ(xy,yearmo)
xy_yearmo_dt <- unique(xy_yearmo)

xy_yearmo <- merge(xy_yearmo_dt,xy_dt,by="xy")

acled_sum_dt$event_type <- as.factor(acled_sum_dt$event_type)

acled_protests_dt <- acled_sum_dt[event_type %in% "Protests"]
acled_violence_dt <- acled_sum_dt[event_type %in% "Violence against civilians"]
acled_riots_dt <- acled_sum_dt[event_type %in% "Riots"]
acled_battles_dt <- acled_sum_dt[event_type %in% c("Battles","Explosions/Remote violence")]
acled_battles_dt[,`:=`(event_type="Battles")]
acled_battles_dt <- acled_battles_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,yearmo,year,mo,longitude,latitude,event_type)]

acled_protests_all_dt <- merge(xy_yearmo,acled_protests_dt,by=c("xy","yearmo"),all.x=T)
acled_violence_all_dt <- merge(xy_yearmo,acled_violence_dt,by=c("xy","yearmo"),all.x=T)
acled_riots_all_dt <- merge(xy_yearmo,acled_riots_dt,by=c("xy","yearmo"),all.x=T)
acled_battles_all_dt <- merge(xy_yearmo,acled_battles_dt,by=c("xy","yearmo"),all.x=T)

acled_protests_all_dt$event <- "protests"
acled_violence_all_dt$event <- "violence"
acled_riots_all_dt$event <- "riots"
acled_battles_all_dt$event <- "battles"

acled_all_dt <- rbind(acled_battles_all_dt,acled_violence_all_dt,acled_riots_all_dt,acled_protests_all_dt)

rm(acled_protests_all_dt)
rm(acled_riots_all_dt)
rm(acled_violence_all_dt)
rm(acled_battles_all_dt)

acled_all_dt$event_type <- NULL


acled_all_dt$year <- as.factor(substr(acled_all_dt$yearmo,1,4))
acled_all_dt$mo <- as.factor(substr(acled_all_dt$yearmo,6,7))

acled_all_dt$longitude <- as.numeric(unlist(strsplit(as.character(acled_all_dt$xy),","))[c(T,F)])
acled_all_dt$latitude <- as.numeric(unlist(strsplit(as.character(acled_all_dt$xy),","))[c(F,T)])

acled_all_dt[is.na(acled_all_dt)] <- 0


## merge conflict and crops
acled_crop_dt <- merge(acled_all_dt,cereals_dt,by=c("xy","longitude","latitude","mo"),all.x=T)
rm(acled_all_dt)

aggregate_dt <- acled_crop_dt[,.(incidents=sum(incidents)),by=.(xy,longitude,latitude)]

aggregate_dt <- aggregate_dt[order(xy,longitude,latitude)]

points_sf <- st_as_sf(aggregate_dt[,.(longitude,latitude)],coords=c("longitude","latitude"),crs=st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

countries_sf <- ne_countries(scale="large",returnclass="sf",continent="Asia")
countries_sf <- st_set_crs(countries_sf,st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

indices <- st_join(points_sf,countries_sf,join=st_intersects,left=T)
aggregate_dt$country <- indices$name

aggregate_dt$incidents <- NULL

dataset_dt <- merge(acled_crop_dt,aggregate_dt,by=c("xy","longitude","latitude"))


dataset_dt$x <- NULL
dataset_dt$y <- NULL

dataset_dt$month <- month.abb[dataset_dt$mo]

dataset_dt[,`:=`(harvest=Rice_harvest_mid,planting=Rice_plant_mid)]


dataset_dt[is.na(harvest)]$harvest <- 0
dataset_dt[is.na(planting)]$planting <- 0

dataset_dt$year <- as.factor(dataset_dt$year)
dataset_dt$mo <- as.factor(dataset_dt$mo)
dataset_dt$harvest <- as.factor(dataset_dt$harvest)
dataset_dt$planting <- as.factor(dataset_dt$planting)

dataset_dt <- dataset_dt[order(country,longitude,latitude,yearmo)]

## aggregate events into the single category
datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,yearmo,mo,month,Rice_harvest_srt,Rice_harvest_mid,Rice_harvest_end,Rice_plant_srt,Rice_plant_mid,Rice_plant_end,harvest,planting)]


save(dataset_dt,datacomb_dt,file="agconflict.RData")
