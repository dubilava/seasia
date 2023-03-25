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

xy2cty = function(points,continent=NULL,country=NULL){
  countries_sf <- ne_countries(scale="large",returnclass="sf",continent=continent,country=country)
  points_sf <- st_as_sf(sp::SpatialPoints(points))
  points_sf <- st_set_crs(points_sf,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  countries_sf <- st_set_crs(countries_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  indices <- st_join(points_sf,countries_sf,join=st_intersects,left=T)
  countries <- indices$name
  coords <- as.data.table(st_coordinates(indices))
  colnames(coords) <- colnames(points)
  return(list(countries,coords))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# load the map of se asia
load("acled_seasia.RData")
load("ged_seasia.RData")
load("calendar.RData")

# southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
# southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


# ged ----

ged_dt[,`:=`(date=as.Date(date_start),length=as.numeric(as.Date(ged_dt$date_end)-as.Date(ged_dt$date_start)+1))]
ged_dt$length <- as.numeric(as.Date(ged_dt$date_end)-as.Date(ged_dt$date_start)+1)

ged_dt$lat <- as.numeric(ged_dt$latitude)
ged_dt$lon <- as.numeric(ged_dt$longitude)

ged_dt$latitude <- round(round(ged_dt$lat,2)-.499)+.5
ged_dt$longitude <- round(round(ged_dt$lon,2)-.499)+.5

ged_dt$yearmo <- as.factor(substr(ged_dt$date,1,7))
ged_dt$year <- as.factor(substr(ged_dt$date,1,4))
ged_dt$mo <- as.factor(substr(ged_dt$date,6,7))

ged_dt <- ged_dt[where_prec %in% c(1:4)]

ged_sum_dt <- ged_dt[,.(incidents=.N,fatalities=sum(deaths_a+deaths_b+deaths_civilians+deaths_unknown)),by=.(yearmo,year,mo,longitude,latitude,type_of_violence)]

ged_sum_dt <- ged_sum_dt[order(year,mo,longitude,latitude)]

ged_sum_dt$xy <- as.factor(paste(ged_sum_dt$longitude,ged_sum_dt$latitude,sep=","))


# cereals ----

crops_dt <- crops_dt[,.(x,y,Crop,Max_area,Crop_Rice,Rice_area)]

crops_dt$latitude <- as.numeric(crops_dt$y)
crops_dt$longitude <- as.numeric(crops_dt$x)

# crops_dt[,`:=` (Rice_dum=1)] 

crops_dt$xy <- as.factor(paste(crops_dt$longitude,crops_dt$latitude,sep=","))


## harvest ----

## harvest start
harvest_srt_dt <- harvest_srt_dt[,.(x,y,mo,Rice_harvest_srt,Rice.2_harvest_srt,Maize_harvest_srt,Maize.2_harvest_srt)]

harvest_srt_dt$latitude <- as.numeric(harvest_srt_dt$y)
harvest_srt_dt$longitude <- as.numeric(harvest_srt_dt$x)

harvest_srt_dt$mo <- as.factor(str_pad(harvest_srt_dt$mo,2,pad="0"))

harvest_srt_dt$Rice_harvest_srt <- as.factor(harvest_srt_dt$Rice_harvest_srt)
harvest_srt_dt$Rice.2_harvest_srt <- as.factor(harvest_srt_dt$Rice.2_harvest_srt)
harvest_srt_dt$Maize_harvest_srt <- as.factor(harvest_srt_dt$Maize_harvest_srt)
harvest_srt_dt$Maize.2_harvest_srt <- as.factor(harvest_srt_dt$Maize.2_harvest_srt)

harvest_srt_dt$xy <- as.factor(paste(harvest_srt_dt$longitude,harvest_srt_dt$latitude,sep=","))

## harvest mid
harvest_mid_dt <- harvest_mid_dt[,.(x,y,mo,Rice_harvest_mid,Rice.2_harvest_mid,Maize_harvest_mid,Maize.2_harvest_mid)]

harvest_mid_dt$latitude <- as.numeric(harvest_mid_dt$y)
harvest_mid_dt$longitude <- as.numeric(harvest_mid_dt$x)

harvest_mid_dt$mo <- as.factor(str_pad(harvest_mid_dt$mo,2,pad="0"))

harvest_mid_dt$Rice_harvest_mid <- as.factor(harvest_mid_dt$Rice_harvest_mid)
harvest_mid_dt$Rice.2_harvest_mid <- as.factor(harvest_mid_dt$Rice.2_harvest_mid)
harvest_mid_dt$Maize_harvest_mid <- as.factor(harvest_mid_dt$Maize_harvest_mid)
harvest_mid_dt$Maize.2_harvest_mid <- as.factor(harvest_mid_dt$Maize.2_harvest_mid)

harvest_mid_dt$xy <- as.factor(paste(harvest_mid_dt$longitude,harvest_mid_dt$latitude,sep=","))

## harvest end
harvest_end_dt <- harvest_end_dt[,.(x,y,mo,Rice_harvest_end,Rice.2_harvest_end,Maize_harvest_end,Maize.2_harvest_end)]

harvest_end_dt$latitude <- as.numeric(harvest_end_dt$y)
harvest_end_dt$longitude <- as.numeric(harvest_end_dt$x)

harvest_end_dt$mo <- as.factor(str_pad(harvest_end_dt$mo,2,pad="0"))

harvest_end_dt$Rice_harvest_end <- as.factor(harvest_end_dt$Rice_harvest_end)
harvest_end_dt$Rice.2_harvest_end <- as.factor(harvest_end_dt$Rice.2_harvest_end)
harvest_end_dt$Maize_harvest_end <- as.factor(harvest_end_dt$Maize_harvest_end)
harvest_end_dt$Maize.2_harvest_end <- as.factor(harvest_end_dt$Maize.2_harvest_end)

harvest_end_dt$xy <- as.factor(paste(harvest_end_dt$longitude,harvest_end_dt$latitude,sep=","))

harvest_dt <- Reduce(function(x,y) merge(x=x,y=y,by=c("x","y","mo","longitude","latitude","xy")),list(harvest_srt_dt,harvest_mid_dt,harvest_end_dt))


rice_dt <- crops_dt[Crop=="Rice"]
maize_dt <- crops_dt[Crop=="Maize"]

harvest_rice_dt <- harvest_mid_dt[Rice_harvest_mid%in%c(0,1),.(xy,x,y,Rice_harvest=Rice_harvest_mid)]
harvest_rice_dt <- unique(harvest_rice_dt)
harvest_rice.2_dt <- harvest_mid_dt[Rice.2_harvest_mid%in%c(0,1),.(xy,x,y,Rice.2_harvest=Rice.2_harvest_mid)]
harvest_rice.2_dt <- unique(harvest_rice.2_dt)
harvest_rice_dt <- merge(harvest_rice_dt,harvest_rice.2_dt,by=c("xy","x","y"))
harvest_rice_dt <- harvest_rice_dt[order(x,y,xy)]

harvest_maize_dt <- harvest_mid_dt[Maize_harvest_mid%in%c(0,1),.(xy,x,y,Maize_harvest=Maize_harvest_mid)]
harvest_maize_dt <- unique(harvest_maize_dt)
harvest_maize.2_dt <- harvest_mid_dt[Maize.2_harvest_mid%in%c(0,1),.(xy,x,y,Maize.2_harvest=Maize.2_harvest_mid)]
harvest_maize.2_dt <- unique(harvest_maize.2_dt)
harvest_maize_dt <- merge(harvest_maize_dt,harvest_maize.2_dt,by=c("xy","x","y"))
harvest_maize_dt <- harvest_maize_dt[order(x,y,xy)]

crops_dt <- merge(crops_dt,harvest_rice_dt,by=c("xy","x","y"))
crops_dt <- merge(crops_dt,harvest_maize_dt,by=c("xy","x","y"))


## plant ----

## plant start
plant_srt_dt <- plant_srt_dt[,.(x,y,mo,Rice_plant_srt,Rice.2_plant_srt,Maize_plant_srt,Maize.2_plant_srt)]

plant_srt_dt$latitude <- as.numeric(plant_srt_dt$y)
plant_srt_dt$longitude <- as.numeric(plant_srt_dt$x)

plant_srt_dt$mo <- as.factor(str_pad(plant_srt_dt$mo,2,pad="0"))

plant_srt_dt$Rice_plant_srt <- as.factor(plant_srt_dt$Rice_plant_srt)
plant_srt_dt$Rice.2_plant_srt <- as.factor(plant_srt_dt$Rice.2_plant_srt)
plant_srt_dt$Maize_plant_srt <- as.factor(plant_srt_dt$Maize_plant_srt)
plant_srt_dt$Maize.2_plant_srt <- as.factor(plant_srt_dt$Maize.2_plant_srt)

plant_srt_dt$xy <- as.factor(paste(plant_srt_dt$longitude,plant_srt_dt$latitude,sep=","))

## plant mid
plant_mid_dt <- plant_mid_dt[,.(x,y,mo,Rice_plant_mid,Rice.2_plant_mid,Maize_plant_mid,Maize.2_plant_mid)]

plant_mid_dt$latitude <- as.numeric(plant_mid_dt$y)
plant_mid_dt$longitude <- as.numeric(plant_mid_dt$x)

plant_mid_dt$mo <- as.factor(str_pad(plant_mid_dt$mo,2,pad="0"))

plant_mid_dt$Rice_plant_mid <- as.factor(plant_mid_dt$Rice_plant_mid)
plant_mid_dt$Rice.2_plant_mid <- as.factor(plant_mid_dt$Rice.2_plant_mid)
plant_mid_dt$Maize_plant_mid <- as.factor(plant_mid_dt$Maize_plant_mid)
plant_mid_dt$Maize.2_plant_mid <- as.factor(plant_mid_dt$Maize.2_plant_mid)

plant_mid_dt$xy <- as.factor(paste(plant_mid_dt$longitude,plant_mid_dt$latitude,sep=","))

## plant end
plant_end_dt <- plant_end_dt[,.(x,y,mo,Rice_plant_end,Rice.2_plant_end,Maize_plant_end,Maize.2_plant_end)]

plant_end_dt$latitude <- as.numeric(plant_end_dt$y)
plant_end_dt$longitude <- as.numeric(plant_end_dt$x)

plant_end_dt$mo <- as.factor(str_pad(plant_end_dt$mo,2,pad="0"))

plant_end_dt$Rice_plant_end <- as.factor(plant_end_dt$Rice_plant_end)
plant_end_dt$Rice.2_plant_end <- as.factor(plant_end_dt$Rice.2_plant_end)
plant_end_dt$Maize_plant_end <- as.factor(plant_end_dt$Maize_plant_end)
plant_end_dt$Maize.2_plant_end <- as.factor(plant_end_dt$Maize.2_plant_end)

plant_end_dt$xy <- as.factor(paste(plant_end_dt$longitude,plant_end_dt$latitude,sep=","))

plant_dt <- Reduce(function(x,y) merge(x=x,y=y,by=c("x","y","mo","longitude","latitude","xy")),list(plant_srt_dt,plant_mid_dt,plant_end_dt))


rice_dt <- crops_dt[Crop=="Rice"]
maize_dt <- crops_dt[Crop=="Maize"]

plant_rice_dt <- plant_mid_dt[Rice_plant_mid%in%c(0,1),.(xy,x,y,Rice_plant=Rice_plant_mid)]
plant_rice_dt <- unique(plant_rice_dt)
plant_rice.2_dt <- plant_mid_dt[Rice.2_plant_mid%in%c(0,1),.(xy,x,y,Rice.2_plant=Rice.2_plant_mid)]
plant_rice.2_dt <- unique(plant_rice.2_dt)
plant_rice_dt <- merge(plant_rice_dt,plant_rice.2_dt,by=c("xy","x","y"))
plant_rice_dt <- plant_rice_dt[order(x,y,xy)]

plant_maize_dt <- plant_mid_dt[Maize_plant_mid%in%c(0,1),.(xy,x,y,Maize_plant=Maize_plant_mid)]
plant_maize_dt <- unique(plant_maize_dt)
plant_maize.2_dt <- plant_mid_dt[Maize.2_plant_mid%in%c(0,1),.(xy,x,y,Maize.2_plant=Maize.2_plant_mid)]
plant_maize.2_dt <- unique(plant_maize.2_dt)
plant_maize_dt <- merge(plant_maize_dt,plant_maize.2_dt,by=c("xy","x","y"))
plant_maize_dt <- plant_maize_dt[order(x,y,xy)]

crops_dt <- merge(crops_dt,plant_rice_dt,by=c("xy","x","y"))
crops_dt <- merge(crops_dt,plant_maize_dt,by=c("xy","x","y"))



crops_dt[,`:=`(Crop_area=ifelse(Crop=="Rice" & Rice.2_harvest==1,Max_area/2,ifelse(Crop=="Maize" & Maize.2_harvest==1,Max_area/2,Max_area)),Rice_area=ifelse(Rice.2_harvest==1,Rice_area/2,Rice_area))]

crops_dt$Rice_harvest <- NULL
crops_dt$Rice.2_harvest <- NULL
crops_dt$Maize_harvest <- NULL
crops_dt$Maize.2_harvest <- NULL

crops_dt$Rice_plant <- NULL
crops_dt$Rice.2_plant <- NULL
crops_dt$Maize_plant <- NULL
crops_dt$Maize.2_plant <- NULL


calendar_dt <- merge(harvest_dt,plant_dt,by=c("x","y","xy","longitude","latitude","mo"))

cereals_dt <- merge(crops_dt,calendar_dt,by=c("x","y","xy","longitude","latitude"))

cereals_dt[,`:=`(Crop=ifelse(Crop_area==0,"None",Crop))]
cereals_dt[,`:=`(Crop_Rice=ifelse(Rice_area==0,"None",Crop_Rice))]

## I am doing this to ensure that ged and cereal centroids overlap -- super inefficient coding but gets the stuff done (I think)
xy_ged_dt <- ged_sum_dt[,.(xy)]
xy_ged_dt <- unique(xy_ged_dt)

xy_cereals_dt <- cereals_dt[,.(xy)]
xy_cereals_dt <- unique(xy_cereals_dt)

xy_cereals_dt$longitude <- as.numeric(unlist(strsplit(as.character(xy_cereals_dt$xy),","))[c(T,F)])
xy_cereals_dt$latitude <- as.numeric(unlist(strsplit(as.character(xy_cereals_dt$xy),","))[c(F,T)])

xy_ged_dt$longitude <- as.numeric(unlist(strsplit(as.character(xy_ged_dt$xy),","))[c(T,F)])
xy_ged_dt$latitude <- as.numeric(unlist(strsplit(as.character(xy_ged_dt$xy),","))[c(F,T)])

xy_ged_dt <- xy_ged_dt[xy %!in% xy_cereals_dt$xy]

d <- pointDistance(xy_ged_dt[,.(longitude,latitude)],xy_cereals_dt[,.(longitude,latitude)],lonlat=TRUE)

r <- apply(d, 1, which.min)

p <- data.table(ged=xy_ged_dt$xy,cereal=xy_cereals_dt$xy[r])

colnames(p) <- c("xy","xy_cereals")

xy_ged_dt <- ged_sum_dt[,.(xy)]
xy_ged_dt <- unique(xy_ged_dt)

ged_xy_dt <- merge(ged_sum_dt,p,by="xy",all.x=T)
ged_xy_dt[!is.na(xy_cereals)]$xy <- ged_xy_dt[!is.na(xy_cereals)]$xy_cereals

ged_xy_dt$longitude <- as.numeric(unlist(strsplit(as.character(ged_xy_dt$xy),","))[c(T,F)])
ged_xy_dt$latitude <- as.numeric(unlist(strsplit(as.character(ged_xy_dt$xy),","))[c(F,T)])

ged_xy_dt$xy_cereals <- NULL

ged_sum_dt <- ged_xy_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,yearmo,year,mo,longitude,latitude,type_of_violence)]
##---

rm(d)

xy_ged_dt <- ged_sum_dt[,.(xy)]
xy_ged_dt <- unique(xy_ged_dt)

xy_cereals_dt <- cereals_dt[,.(xy)]
xy_cereals_dt <- unique(xy_cereals_dt)

xy_dt <- merge(xy_ged_dt,xy_cereals_dt,all=T)
xy_dt <- unique(xy_dt)
xy <- as.character(xy_dt$xy)

yearmo <- substr(as.character(seq(as.Date("1989-01-01"),as.Date("2021-12-31"),by="month")),1,7)

xy_yearmo <- CJ(xy,yearmo)
xy_yearmo_dt <- unique(xy_yearmo)

xy_yearmo <- merge(xy_yearmo_dt,xy_dt,by="xy")


ged_sum_dt$type_of_violence <- as.factor(ged_sum_dt$type_of_violence)

ged_state_dt <- ged_sum_dt[type_of_violence %in% 1]
ged_nonstate_dt <- ged_sum_dt[type_of_violence %in% 2]
ged_onesided_dt <- ged_sum_dt[type_of_violence %in% 3]

ged_state_all_dt <- merge(xy_yearmo,ged_state_dt,by=c("xy","yearmo"),all.x=T)
ged_nonstate_all_dt <- merge(xy_yearmo,ged_nonstate_dt,by=c("xy","yearmo"),all.x=T)
ged_onesided_all_dt <- merge(xy_yearmo,ged_onesided_dt,by=c("xy","yearmo"),all.x=T)

ged_state_all_dt$event <- "state"
ged_nonstate_all_dt$event <- "nonstate"
ged_onesided_all_dt$event <- "onesided"

ged_all_dt <- rbind(ged_state_all_dt,ged_nonstate_all_dt,ged_onesided_all_dt)
rm(ged_state_all_dt)
rm(ged_nonstate_all_dt)
rm(ged_onesided_all_dt)

ged_all_dt$type_of_violence <- NULL


ged_all_dt$year <- as.factor(substr(ged_all_dt$yearmo,1,4))
ged_all_dt$mo <- as.factor(substr(ged_all_dt$yearmo,6,7))

ged_all_dt$longitude <- as.numeric(unlist(strsplit(as.character(ged_all_dt$xy),","))[c(T,F)])
ged_all_dt$latitude <- as.numeric(unlist(strsplit(as.character(ged_all_dt$xy),","))[c(F,T)])

ged_all_dt[is.na(ged_all_dt)] <- 0



# ged_all_dt <- merge(xy_yearmo,ged_sum_dt,by=c("xy","yearmo"),all.x=T)
# 
# ged_all_dt$year <- as.factor(substr(ged_all_dt$yearmo,1,4))
# ged_all_dt$mo <- as.factor(substr(ged_all_dt$yearmo,6,7))
# 
# ged_all_dt$longitude <- as.numeric(unlist(strsplit(as.character(ged_all_dt$xy),","))[c(T,F)])
# ged_all_dt$latitude <- as.numeric(unlist(strsplit(as.character(ged_all_dt$xy),","))[c(F,T)])
# 
# ged_all_dt[is.na(ged_all_dt)] <- 0





## merge conflict crops and prices
ged_crop_dt <- merge(ged_all_dt,cereals_dt,by=c("xy","longitude","latitude","mo"),all.x=T)

aggregate_dt <- ged_crop_dt[,.(incidents=sum(incidents)),by=.(xy,longitude,latitude)]

aggregate_dt <- aggregate_dt[order(xy,longitude,latitude)]

aggregate_dt$country <- xy2cty(aggregate_dt[,.(longitude,latitude)])[[1]]

aggregate_dt$incidents <- NULL

dataset_dt <- merge(ged_crop_dt,aggregate_dt,by=c("xy","longitude","latitude"))


dataset_dt$x <- NULL
dataset_dt$y <- NULL

dataset_dt$month <- month.abb[dataset_dt$mo]

dataset_dt[,`:=`(season=ifelse(Crop=="Rice" & Crop_area>0,as.numeric(as.character(Rice_harvest_mid)),ifelse(Crop=="Maize" & Crop_area>0,as.numeric(as.character(Maize_harvest_mid)),0)),season2=ifelse(Crop=="Rice" & Crop_area>0,as.numeric(as.character(Rice.2_harvest_mid)),ifelse(Crop=="Maize" & Crop_area>0,as.numeric(as.character(Maize.2_harvest_mid)),0)),season_rice=ifelse(Crop_Rice=="Rice" & Rice_area>0,as.numeric(as.character(Rice_harvest_mid)),0),season2_rice=ifelse(Crop_Rice=="Rice" & Rice_area>0,as.numeric(as.character(Rice.2_harvest_mid)),0),planting=ifelse(Crop=="Rice" & Crop_area>0,as.numeric(as.character(Rice_plant_mid)),ifelse(Crop=="Maize" & Crop_area>0,as.numeric(as.character(Maize_plant_mid)),0)),planting2=ifelse(Crop=="Rice" & Crop_area>0,as.numeric(as.character(Rice.2_plant_mid)),ifelse(Crop=="Maize" & Crop_area>0,as.numeric(as.character(Maize.2_plant_mid)),0)),planting_rice=ifelse(Crop_Rice=="Rice" & Rice_area>0,as.numeric(as.character(Rice_plant_mid)),0),planting2_rice=ifelse(Crop_Rice=="Rice" & Rice_area>0,as.numeric(as.character(Rice.2_plant_mid)),0))]


dataset_dt[is.na(Crop)]$Crop <- "None"
dataset_dt[is.na(Crop)]$Crop_Rice <- "None"
dataset_dt[is.na(season)]$season <- 0
dataset_dt[is.na(season2)]$season2 <- 0
dataset_dt[is.na(season_rice)]$season_rice <- 0
dataset_dt[is.na(season2_rice)]$season2_rice <- 0
dataset_dt[is.na(season)]$planting <- 0
dataset_dt[is.na(season2)]$planting2 <- 0
dataset_dt[is.na(season_rice)]$planting_rice <- 0
dataset_dt[is.na(season2_rice)]$planting2_rice <- 0

dataset_dt$year <- as.factor(dataset_dt$year)
dataset_dt$mo <- as.factor(dataset_dt$mo)
dataset_dt$season <- as.factor(dataset_dt$season)
dataset_dt$season2 <- as.factor(dataset_dt$season2)
dataset_dt$season_rice <- as.factor(dataset_dt$season_rice)
dataset_dt$season2_rice <- as.factor(dataset_dt$season2_rice)
dataset_dt$planting <- as.factor(dataset_dt$planting)
dataset_dt$planting2 <- as.factor(dataset_dt$planting2)
dataset_dt$planting_rice <- as.factor(dataset_dt$planting_rice)
dataset_dt$planting2_rice <- as.factor(dataset_dt$planting2_rice)


dataset_dt <- dataset_dt[order(country,longitude,latitude,yearmo)]

# some finishing touches
area_trs <- .01

dataset_dt[,`:=` (incidents_dum=ifelse(incidents>0,1,0),area_dum=ifelse(Crop_area>=area_trs,1,0))]

dataset_dt[,`:=` (trend=as.numeric(as.factor(yearmo)))]

## aggregate events into the single category
datacomb_dt <- dataset_dt[,.(incidents=sum(incidents),fatalities=sum(fatalities)),by=.(xy,longitude,latitude,country,year,yearmo,mo,month,trend,Crop,Crop_Rice,Max_area,Crop_area,Rice_area,Rice_harvest_srt,Rice.2_harvest_srt,Maize_harvest_srt,Maize.2_harvest_srt,Rice_harvest_mid,Rice.2_harvest_mid,Maize_harvest_mid,Maize.2_harvest_mid,Rice_harvest_end,Rice.2_harvest_end,Maize_harvest_end,Maize.2_harvest_end,Rice_plant_srt,Rice.2_plant_srt,Maize_plant_srt,Maize.2_plant_srt,Rice_plant_mid,Rice.2_plant_mid,Maize_plant_mid,Maize.2_plant_mid,Rice_plant_end,Rice.2_plant_end,Maize_plant_end,Maize.2_plant_end,season,season2,season_rice,season2_rice,planting,planting2,planting_rice,planting2_rice,area_dum)]


datacomb_dt[,`:=` (incidents_dum=ifelse(incidents>0,1,0))]

save(dataset_dt,datacomb_dt,file="data_violence_ged.RData")
