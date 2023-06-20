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
library(raster)
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
load("agconflict.RData")
load("spam.RData")

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

### planting months ----
datacomb_dt[,`:=`(planting_rice=ifelse(planting==1,1,0))]

### harvest months ----
datacomb_dt[,`:=`(harvest_rice=ifelse(harvest==1,1,0))]

### growing season (rice, main) ----
datacomb_dt[,`:=`(rice_p=ifelse(Rice_plant_mid==1,1,0),rice_h=ifelse(Rice_harvest_mid==1,1,0))]

# datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(planting))<as.numeric(as.character(harvest))]$rice_p <- 1

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_mid))>1 & as.numeric(as.character(Rice_harvest_mid))>as.numeric(as.character(Rice_plant_mid))]$rice_p <- 1

datacomb_dt[yearmo==min(yearmo) & rice_p-rice_h<0]$rice_p <- 1

datacomb_dt[,`:=`(rice_growing_season=cumsum(rice_p-rice_h)),by=.(xy)]
datacomb_dt[,`:=`(rice_growing_season=rice_growing_season+rice_h)]


### planting season (rice, main) ----
datacomb_dt[,`:=`(rice_ps=ifelse(Rice_plant_srt==1,1,0),rice_pm=ifelse(Rice_plant_mid==1,1,0),rice_pe=ifelse(Rice_plant_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_srt))>1 & as.numeric(as.character(Rice_plant_end))>as.numeric(as.character(Rice_plant_srt))]$rice_ps <- 1

datacomb_dt[yearmo==min(yearmo) & rice_ps-rice_pe<0]$rice_ps <- 1

datacomb_dt[,`:=`(rice_planting_season=cumsum(rice_ps-rice_pe)),by=.(xy)]
datacomb_dt[,`:=`(rice_planting_season=rice_planting_season+rice_pe)]


### harvest season (rice, main) ----
datacomb_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_m=ifelse(Rice_harvest_mid==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

datacomb_dt[yearmo==min(yearmo) & rice_s-rice_e<0]$rice_s <- 1

datacomb_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy)]
datacomb_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]

### finalizing the seasons ----
datacomb_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0),planting_season=ifelse(Crop_Rice=="Rice",rice_planting_season,0))]

# datacomb_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season),planting_season=ifelse(planting_season==2,1,planting_season))]



## event-specific ----

### planting months ----
dataset_dt[,`:=`(planting_rice=ifelse(planting==1,1,0))]

### harvest months ----
dataset_dt[,`:=`(harvest_rice=ifelse(harvest==1,1,0))]

### growing season (rice, main) ----
dataset_dt[,`:=`(rice_p=ifelse(planting==1,1,0),rice_h=ifelse(harvest==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(planting))>1 & as.numeric(as.character(harvest))>as.numeric(as.character(planting))]$rice_p <- 1

dataset_dt[yearmo==min(yearmo) & rice_p-rice_h<0]$rice_p <- 1

dataset_dt[,`:=`(rice_growing_season=cumsum(rice_p-rice_h)),by=.(xy,event)]
dataset_dt[,`:=`(rice_growing_season=rice_growing_season+rice_h)]

### plant season (rice, main) ----
dataset_dt[,`:=`(rice_ps=ifelse(Rice_plant_srt==1,1,0),rice_pm=ifelse(Rice_plant_mid==1,1,0),rice_pe=ifelse(Rice_plant_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_srt))>1 & as.numeric(as.character(Rice_plant_end))>as.numeric(as.character(Rice_plant_srt))]$rice_ps <- 1

dataset_dt[yearmo==min(yearmo) & rice_ps-rice_pe<0]$rice_ps <- 1

dataset_dt[,`:=`(rice_planting_season=cumsum(rice_ps-rice_pe)),by=.(xy,event)]
dataset_dt[,`:=`(rice_planting_season=rice_planting_season+rice_pe)]

### harvest season (rice, main) ----
dataset_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_m=ifelse(Rice_harvest_mid==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

dataset_dt[yearmo==min(yearmo) & rice_s-rice_e<0]$rice_s <- 1

dataset_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy,event)]
dataset_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]


### finalizing the seasons ----
dataset_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0),planting_season=ifelse(Crop_Rice=="Rice",rice_planting_season,0))]

# dataset_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season),planting_season=ifelse(planting_season==2,1,planting_season))]


## finishing touches ----
datacomb_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]
dataset_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]

## conflict types
dataset_dt$event <- factor(dataset_dt$event,levels=unique(dataset_dt$event))


list_of_periods <- unique(datacomb_dt$yearmo)

# datacomb
lst <- list()

for(i in 1:length(list_of_periods)){
  
  sub_dt <- datacomb_dt[yearmo==list_of_periods[i],.(longitude,latitude,incidents)]
  sub_pt <- st_as_sf(sub_dt,coords=c("latitude","longitude"),remove=F)
  agg_pt <- aggregate(sub_pt,sub_pt,FUN=sum,join=function(x,y) st_is_within_distance(x,y,dist=1))
  
  coords <- st_coordinates(agg_pt)
  
  agg_dt <- data.table(agg_pt)
  agg_dt[,`:=`(longitude=coords[,2],latitude=coords[,1])]
  agg_dt$geometry <- NULL
  agg_dt$yearmo <- list_of_periods[i]
  
  lst[[i]] <- agg_dt
  
  print(i)
  
}

neighbours_dt <- Reduce(rbind,lst)

colnames(neighbours_dt)[3] <- "incidents_n"

datacomb_dt <- merge(datacomb_dt,neighbours_dt,by=c("longitude","latitude","yearmo"))

# dataset

list_of_events <- unique(dataset_dt$event)

evt <- list()

for(j in 1:length(list_of_events)){
  
  lst <- list()
  
  for(i in 1:length(list_of_periods)){
    
    sub_dt <- dataset_dt[yearmo==list_of_periods[i] & event==list_of_events[j],.(longitude,latitude,incidents)]
    sub_pt <- st_as_sf(sub_dt,coords=c("latitude","longitude"),remove=F)
    agg_pt <- aggregate(sub_pt,sub_pt,FUN=sum,join=function(x,y) st_is_within_distance(x,y,dist=1))
    
    coords <- st_coordinates(agg_pt)
    
    agg_dt <- data.table(agg_pt)
    agg_dt[,`:=`(longitude=coords[,2],latitude=coords[,1])]
    agg_dt$geometry <- NULL
    agg_dt$yearmo <- list_of_periods[i]
    
    lst[[i]] <- agg_dt
    
  }
  
  neighbours_dt <- Reduce(rbind,lst)
  
  neighbours_dt$event <- list_of_events[j]
  
  evt[[j]] <- neighbours_dt
  
  print(j)
  
}

eventneighbours_dt <- Reduce(rbind,evt)

colnames(eventneighbours_dt)[3] <- "incidents_n"

dataset_dt <- merge(dataset_dt,eventneighbours_dt,by=c("longitude","latitude","yearmo","event"))


## rainfall ----

standardize <- function(x,ln=TRUE){
  if(ln==T){
    x <- log(x)
  }
  z=(x-mean(x))/sd(x)
  return(z)
}

load("precipitation_new.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]


datacomb_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)

battles_dt <- dataset_dt[event=="battles",.(year,mo,longitude,latitude,conflict=incidents,conflict_n=incidents_n)]

datacomb_dt <- merge(datacomb_dt,battles_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,battles_dt,by=c("year","mo","longitude","latitude"),all.x=T)

season_dt <- datacomb_dt[,.(longitude,latitude,mo,planting_season,growing_season,harvest_season,Rice_harvest_end)]
season_dt <- unique(season_dt)

season_dt <- season_dt[order(longitude,latitude,mo)]

datarain_dt <- merge(rain_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)
datarain_dt[is.na(rain)]$rain <- 0

dataconf_dt <- merge(battles_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)

subrain_dt <- unique(datarain_dt)
subconf_dt <- unique(dataconf_dt)

subset_dt <- merge(subrain_dt,subconf_dt,by=c("longitude","latitude","year","mo","planting_season","growing_season","harvest_season","Rice_harvest_end"),all=T)

subset_dt[is.na(conflict)]$conflict <- 0
subset_dt[is.na(conflict_n)]$conflict_n <- 0


#############

# STOPPED HERE -- i think some of the next bits of code can be finetuned


subset_dt[,`:=`(ps_rain=planting_season*rain,gs_rain=growing_season*rain,hs_rain=harvest_season*rain,ps_conflict=planting_season*conflict,gs_conflict=growing_season*conflict,hs_conflict=harvest_season*conflict,ps_conflict_n=planting_season*conflict_n,gs_conflict_n=growing_season*conflict_n,hs_conflict_n=harvest_season*conflict_n)]



#########

# # number of months in the growing season
# subset_dt[,`:=`(gsm=ifelse(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid))<0,12-(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid))+12),12-(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid)))))]
# 
# subset_dt <- subset_dt[order(longitude,latitude,year,mo)]
# 
# subset_dt[season_rice==0]$gsm <- 0


###########################

# select data on planted months
harvest_dt <- subset_dt[Rice_harvest_end == 1]
harvest_dt[,`:=`(crop_year=year)]

harvest_dt <- harvest_dt[,.(year,crop_year,longitude,latitude,Rice_harvest_end)]

# merge the weather data with the growing season data
submerge_dt <- merge(subset_dt,harvest_dt,by=c("year","longitude","latitude","Rice_harvest_end"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

x = 1:10
x[c(1:2, 5:6, 9:10)] = NA
nafill(x, "nocb")

# fill in the NAs
submerge_dt$crop_year <- as.numeric(as.character(submerge_dt$crop_year))
submerge_dt[,crop_year := nafill(crop_year,type="nocb"),by=.(longitude,latitude)]
submerge_dt[,crop_year := nafill(crop_year,type="locf"),by=.(longitude,latitude)]

# # so some other stuff (no longer necessary, I believe, but may as well keep it around)
# submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
# submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward
# 
# subseason_dt <- submerge_dt[dif >= 0]

subseason_dt <- submerge_dt[,.(ps_rain=sum(ps_rain),gs_rain=sum(gs_rain),hs_rain=sum(hs_rain),ps_conflict=sum(ps_conflict),gs_conflict=sum(gs_conflict),hs_conflict=sum(hs_conflict),ps_conflict_n=sum(ps_conflict_n),gs_conflict_n=sum(gs_conflict_n),hs_conflict_n=sum(hs_conflict_n)),by=.(longitude,latitude,crop_year)]

subseason_dt <- merge(submerge_dt[,.(longitude,latitude,year,mo,planting_season,growing_season,harvest_season,rain,conflict,conflict_n,crop_year)],subseason_dt,by=c("longitude","latitude","crop_year"),all.x=T)

# subseason_dt[is.na(gsrain)]$gsrain <- 0 # not needed, really

# generate standartized rainfall based on 1997-2022 data
subseason_dt[,`:=`(rain_stand_long=standardize(rain,ln=F),gs_rain_stand_long=standardize(gs_rain,ln=F)),by=.(longitude,latitude)]

subseason_dt <- subseason_dt[year%in%2010:2022]

# subseason_dt$myr <- NULL
# subseason_dt$gsm <- NULL
# subseason_dt$backward <- NULL
# subseason_dt$dif <- NULL

datacomb_dt <- merge(datacomb_dt,subseason_dt,by=c("longitude","latitude","year","mo","planting_season","growing_season","harvest_season","rain","conflict","conflict_n"),all.x=T)
# datacomb_dt[is.na(gs_rain)]$gs_rain <- 0
# datacomb_dt[is.na(gs_conflict)]$gsconflict <- 0
# datacomb_dt[is.na(gs_conflict_n)]$gsconflict_n <- 0

datacomb_dt[,`:=`(rain_stand=standardize(rain,ln=F),gs_rain_stand=standardize(gs_rain,ln=F),conflict_stand=standardize(conflict,ln=F),gs_conflict_stand=standardize(gs_conflict,ln=F),conflict_n_stand=standardize(conflict_n,ln=F),gs_conflict_n_stand=standardize(gs_conflict_n,ln=F)),by=.(xy)]

datacomb_dt[is.na(conflict_stand)]$conflict_stand <- 0
datacomb_dt[is.na(gs_conflict_stand)]$gs_conflict_stand <- 0
datacomb_dt[is.na(conflict_n_stand)]$conflict_n_stand <- 0
datacomb_dt[is.na(gs_conflict_n_stand)]$gs_conflict_n_stand <- 0

dataset_dt <- merge(dataset_dt,subseason_dt,by=c("longitude","latitude","year","mo","planting_season","growing_season","harvest_season","rain","conflict","conflict_n"),all.x=T)
# dataset_dt[is.na(gsrain)]$gsrain <- 0
# dataset_dt[is.na(gsconflict)]$gsconflict <- 0
# dataset_dt[is.na(gsconflict_n)]$gsconflict_n <- 0

dataset_dt[,`:=`(rain_stand=standardize(rain,ln=F),gs_rain_stand=standardize(gs_rain,ln=F),conflict_stand=standardize(conflict,ln=F),gs_conflict_stand=standardize(gs_conflict,ln=F),conflict_n_stand=standardize(conflict_n,ln=F),gs_conflict_n_stand=standardize(gs_conflict_n,ln=F)),by=.(xy,event)]

dataset_dt[is.na(conflict_stand)]$conflict_stand <- 0
dataset_dt[is.na(gs_conflict_stand)]$gs_conflict_stand <- 0
dataset_dt[is.na(conflict_n_stand)]$conflict_n_stand <- 0
dataset_dt[is.na(gs_conflict_n_stand)]$gs_conflict_n_stand <- 0


datacomb_dt <- datacomb_dt[,.(country,longitude,latitude,xy,year,mo,yearmo,month,incidents,fatalities,harvest_month=rice_m,harvest_season,planting_month=rice_pm,planting_season,growing_season,area_spam,area_i,area_r,area_h,area_l,area_s,area_hi,area_lo,prop_i,conflict,conflict_n,gs_conflict,gs_conflict_n,conflict_stand,conflict_n_stand,gs_conflict_stand,gs_conflict_n_stand,rain,gs_rain,rain_stand,gs_rain_stand,rain_stand_long,gs_rain_stand_long)]

datacomb_dt <- datacomb_dt[order(country,longitude,latitude,year,mo)]

dataset_dt <- dataset_dt[,.(country,longitude,latitude,xy,year,mo,yearmo,month,event,incidents,fatalities,harvest_month=rice_m,harvest_season,planting_month=rice_pm,planting_season,growing_season,area_spam,area_i,area_r,area_h,area_l,area_s,area_hi,area_lo,prop_i,conflict,conflict_n,gs_conflict,gs_conflict_n,conflict_stand,conflict_n_stand,gs_conflict_stand,gs_conflict_n_stand,rain,gs_rain,rain_stand,gs_rain_stand,rain_stand_long,gs_rain_stand_long)]

dataset_dt <- dataset_dt[order(country,longitude,latitude,event,year,mo)]



## add cities

cities_dt <- fread("Cities/worldcities.csv")

secities_dt <- cities_dt[country %in% unique(datacomb_dt$country) & population!="NA"]

secities_dt <- secities_dt[order(-population)]

secities_dt$latitude <- round(round(secities_dt$lat,2)-.499)+.5
secities_dt$longitude <- round(round(secities_dt$lng,2)-.499)+.5


secities_sub1 <- secities_dt[,.(population=sum(population)),by=.(longitude,latitude)]#dropped country

secities_sub2 <- secities_dt[secities_dt[,.I[population==max(population)],by=.(longitude,latitude)]$V1,.(longitude,latitude,city=city_ascii,capital,city_population=population)]#dropped country

secities_sub <- merge(secities_sub1,secities_sub2,by=c("longitude","latitude"))




## making sure that the population cells align with the main dataset cells

secities_sub[,xy:=paste(longitude,latitude,sep=",")]

xy_city_dt <- secities_sub[,.(xy)]
xy_city_dt <- unique(xy_city_dt)

xy_datacomb_dt <- datacomb_dt[,.(xy)]
xy_datacomb_dt <- unique(xy_datacomb_dt)

xy_datacomb_dt$longitude <- as.numeric(unlist(strsplit(as.character(xy_datacomb_dt$xy),","))[c(T,F)])
xy_datacomb_dt$latitude <- as.numeric(unlist(strsplit(as.character(xy_datacomb_dt$xy),","))[c(F,T)])

xy_city_dt$longitude <- as.numeric(unlist(strsplit(as.character(xy_city_dt$xy),","))[c(T,F)])
xy_city_dt$latitude <- as.numeric(unlist(strsplit(as.character(xy_city_dt$xy),","))[c(F,T)])

xy_city_dt <- xy_city_dt[xy %!in% xy_datacomb_dt$xy]

d <- pointDistance(xy_city_dt[,.(longitude,latitude)],xy_datacomb_dt[,.(longitude,latitude)],lonlat=T)

r <- apply(d,1,which.min)

p <- data.table(city=xy_city_dt$xy,datacomb=xy_datacomb_dt$xy[r])

colnames(p) <- c("xy","xy_datacomb")

xy_city_dt <- secities_sub[,.(xy)]
xy_city_dt <- unique(xy_city_dt)

city_xy_dt <- merge(secities_sub,p,by="xy",all.x=T)
city_xy_dt[!is.na(xy_datacomb)]$xy <- city_xy_dt[!is.na(xy_datacomb)]$xy_datacomb

city_xy_dt$longitude <- as.numeric(unlist(strsplit(as.character(city_xy_dt$xy),","))[c(T,F)])
city_xy_dt$latitude <- as.numeric(unlist(strsplit(as.character(city_xy_dt$xy),","))[c(F,T)])

city_xy_dt$xy_datacomb <- NULL

rm(d)

secities_sub1 <- city_xy_dt[city_xy_dt[,.I[city_population==max(city_population)],by=xy]$V1][,.(xy,longitude,latitude,city,capital)]

secities_sub2 <- city_xy_dt[,.(population=sum(population),city_population=sum(city_population)),by=.(xy,longitude,latitude)]

secities_sub <- merge(secities_sub1,secities_sub2,by=c("xy","longitude","latitude"))


## merge datasets

datacomb_dt <- merge(datacomb_dt,secities_sub,by=c("xy","longitude","latitude"),all.x=T)

datacomb_dt[is.na(population)]$population <- 0
datacomb_dt[is.na(city_population)]$city_population <- 0
datacomb_dt[is.na(city)]$city <- ""
datacomb_dt[is.na(capital)]$capital <- ""


dataset_dt <- merge(dataset_dt,secities_sub,by=c("xy","longitude","latitude"),all.x=T)

dataset_dt[is.na(population)]$population <- 0
dataset_dt[is.na(city_population)]$city_population <- 0
dataset_dt[is.na(city)]$city <- ""
dataset_dt[is.na(capital)]$capital <- ""


## drop years in countries for which acled data are unavailable
datacomb_dt <- datacomb_dt[country!="Brunei" | (country=="Brunei" & as.numeric(as.character(year))>=2020)]
datacomb_dt <- datacomb_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>=2015)]
datacomb_dt <- datacomb_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>=2018)]
datacomb_dt <- datacomb_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>=2016)]
datacomb_dt <- datacomb_dt[country!="Timor-Leste" | (country=="Timor-Leste" & as.numeric(as.character(year))>=2020)]

dataset_dt <- dataset_dt[country!="Brunei" | (country=="Brunei" & as.numeric(as.character(year))>=2020)]
dataset_dt <- dataset_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>=2015)]
dataset_dt <- dataset_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>=2018)]
dataset_dt <- dataset_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>=2016)]
dataset_dt <- dataset_dt[country!="Timor-Leste" | (country=="Timor-Leste" & as.numeric(as.character(year))>=2020)]

save(datacomb_dt,dataset_dt,file="masterdata.RData")



