library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(Cairo)
library(stringr)
library(sf)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")
library(kableExtra)
library(zoo)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")


# data management ----
load("Local/Data/agconflict.RData")
load("Local/Data/spam.RData")

countries <- unique(datacomb_dt$country)

southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


# crop area stuff
colnames(spam_dt)[1:2] <- c("longitude","latitude")

spam_dt[,`:=`(area_spam=area_spam/100000,area_i=area_i/100000,area_r=area_r/100000,area_h=area_h/100000,area_l=area_l/100000,area_s=area_s/100000)]

datacomb_dt <- merge(datacomb_dt,spam_dt,by=c("longitude","latitude"),all.x=T)

dataset_dt <- merge(dataset_dt,spam_dt,by=c("longitude","latitude"),all.x=T)

datacomb_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]
dataset_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]

## combined ----

### harvest months ----
datacomb_dt[,`:=`(harvest=ifelse(season==1 | season2==1,1,0),harvest_rice=ifelse(season_rice==1 | season2_rice==1,1,0))]

### planting months ----
datacomb_dt[,`:=`(plant=ifelse(planting==1 | planting2==1,1,0),plant_rice=ifelse(planting_rice==1 | planting2_rice==1,1,0))]


### growing season (rice, main) ----
datacomb_dt[,`:=`(rice_p=ifelse(Rice_plant_mid==1,1,0),rice_h=ifelse(Rice_harvest_mid==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_mid))>1 & as.numeric(as.character(Rice_harvest_mid))>as.numeric(as.character(Rice_plant_mid))]$rice_p <- 1

datacomb_dt[,`:=`(rice_growing_season=cumsum(rice_p-rice_h)),by=.(xy)]

datacomb_dt[,`:=`(rice_growing_season=rice_growing_season+rice_h)]


### harvest season (rice, main) ----
datacomb_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_m=ifelse(Rice_harvest_mid==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

datacomb_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy)]

datacomb_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]

### harvest season ----
datacomb_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0))]

datacomb_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]



## event-specific ----

### harvest months ----
dataset_dt[,`:=`(harvest=ifelse(season==1 | season2==1,1,0),harvest_rice=ifelse(season_rice==1 | season2_rice==1,1,0))]

### planting months ----
dataset_dt[,`:=`(plant=ifelse(planting==1 | planting2==1,1,0),plant_rice=ifelse(planting_rice==1 | planting2_rice==1,1,0))]


### growing season (rice, main) ----
dataset_dt[,`:=`(rice_p=ifelse(Rice_plant_mid==1,1,0),rice_h=ifelse(Rice_harvest_mid==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_mid))>1 & as.numeric(as.character(Rice_harvest_mid))>as.numeric(as.character(Rice_plant_mid))]$rice_p <- 1

dataset_dt[,`:=`(rice_growing_season=cumsum(rice_p-rice_h)),by=.(xy)]

dataset_dt[,`:=`(rice_growing_season=rice_growing_season+rice_h)]

### harvest season (rice, main) ----
dataset_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_m=ifelse(Rice_harvest_mid==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

dataset_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy,event)]

dataset_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]


### harvest season ----
dataset_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0))]

dataset_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]


## finishing touches ----
datacomb_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]
dataset_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]

## conflict types
dataset_dt$event <- factor(dataset_dt$event,levels=unique(dataset_dt$event))


## rainfall ----


standardize <- function(x,ln=TRUE){
  if(ln==T){
    x <- log(x)
  }
  z=(x-mean(x))/sd(x)
  return(z)
}

load("Local/Data/precipitation_new.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]

datacomb_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)

battles_dt <- dataset_dt[event=="battles",.(year,mo,longitude,latitude,conflict=incidents)]

datacomb_dt <- merge(datacomb_dt,battles_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,battles_dt,by=c("year","mo","longitude","latitude"),all.x=T)

season_dt <- datacomb_dt[,.(longitude,latitude,mo,Rice_plant_mid,season_rice)]
season_dt <- unique(season_dt)

datarain_dt <- merge(rain_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)
datarain_dt[is.na(rain)]$rain <- 0

dataconf_dt <- merge(battles_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)

subrain_dt <- unique(datarain_dt)
subconf_dt <- unique(dataconf_dt)

subset_dt <- merge(subrain_dt,subconf_dt,by=c("longitude","latitude","year","mo","Rice_plant_mid","season_rice"),all=T)

subset_dt[is.na(conflict)]$conflict <- 0

# number of months in the growing season
subset_dt[,`:=`(gsm=ifelse(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid))<0,12-(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid))+12),12-(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid)))))]

subset_dt <- subset_dt[order(longitude,latitude,year,mo)]

subset_dt[season_rice==0]$gsm <- 0

# select data on planted months
planted_dt <- subset_dt[Rice_plant_mid == 1]
planted_dt$myr <- planted_dt$year

planted_dt <- planted_dt[,.(year,myr,longitude,latitude,Rice_plant_mid,season_rice,gsm)]

# merge the weather data with the growing season data
submerge_dt <- merge(subset_dt,planted_dt,by=c("year","longitude","latitude","Rice_plant_mid","season_rice","gsm"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

# fill in the NAs
submerge_dt$myr <- as.numeric(as.character(submerge_dt$myr))
submerge_dt[,myr := nafill(myr,type="locf"),by=.(longitude,latitude)]
submerge_dt[,myr := nafill(myr,type="nocb"),by=.(longitude,latitude)]

# so some other stuff (no longer necessary, I believe, but may as well keep it around)
submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward

subseason_dt <- submerge_dt[dif >= 0]

subseason_dt <- subseason_dt[,.(gsrain=sum(rain),gsconflict=sum(conflict)),by=.(longitude,latitude,myr)]

subseason_dt <- merge(submerge_dt,subseason_dt,by=c("longitude","latitude","myr"),all.x=T)

subseason_dt <- subseason_dt[year%in%2010:2022]
subseason_dt$myr <- NULL
subseason_dt$gsm <- NULL
subseason_dt$backward <- NULL
subseason_dt$dif <- NULL

datacomb_dt <- merge(datacomb_dt,subseason_dt,by=c("longitude","latitude","year","mo","Rice_plant_mid","season_rice","rain","conflict"),all.x=T)
datacomb_dt[is.na(gsrain)]$gsrain <- 0
datacomb_dt[is.na(gsconflict)]$gsconflict <- 0

datacomb_dt[,`:=`(gsrain_stand=standardize(gsrain,ln=F),rain_stand=standardize(rain,ln=F),gsconflict_stand=standardize(gsconflict,ln=F),conflict_stand=standardize(conflict,ln=F)),by=.(xy)]

dataset_dt <- merge(dataset_dt,subseason_dt,by=c("longitude","latitude","year","mo","Rice_plant_mid","season_rice","rain","conflict"),all.x=T)
dataset_dt[is.na(gsrain)]$gsrain <- 0
dataset_dt[is.na(gsconflict)]$gsconflict <- 0

dataset_dt[,`:=`(gsrain_stand=standardize(gsrain,ln=F),rain_stand=standardize(rain,ln=F),gsconflict_stand=standardize(gsconflict,ln=F),conflict_stand=standardize(conflict,ln=F)),by=.(xy,event)]



datacomb_dt <- datacomb_dt[,.(country,longitude,latitude,xy,year,mo,yearmo,month,incidents,fatalities,harvest_month=rice_m,harvest_season,area_spam,area_i,area_r,area_h,area_l,area_s,area_hi,area_lo,prop_i,rain,conflict,gsrain,gsconflict,rain_stand,conflict_stand,gsrain_stand,gsconflict_stand)]

datacomb_dt <- datacomb_dt[order(country,longitude,latitude,year,mo)]

dataset_dt <- dataset_dt[,.(country,longitude,latitude,xy,year,mo,yearmo,month,event,incidents,fatalities,harvest_month=rice_m,harvest_season,area_spam,area_i,area_r,area_h,area_l,area_s,area_hi,area_lo,prop_i,rain,conflict,gsrain,gsconflict,rain_stand,conflict_stand,gsrain_stand,gsconflict_stand)]

dataset_dt <- dataset_dt[order(country,longitude,latitude,event,year,mo)]

save(datacomb_dt,dataset_dt,file="masterdata.RData")

