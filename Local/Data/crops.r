library(data.table)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(R.utils)


rm(list=ls())
gc()


"%!in%" <- Negate("%in%")

# load the map of se asia
load("acled_seasia.RData")

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


rice_i_phys <- raster("SPAM/dataverse_files/spam2010v2r0_global_phys_area.geotiff/spam2010V2r0_global_A_RICE_I.tif")

rice_i_rain <- raster("SPAM/dataverse_files/spam2010v2r0_global_phys_area.geotiff/spam2010V2r0_global_H_RICE_R.tif")



rice_i_phys05 <- aggregate(rice_i_phys,fact=6,fun=sum)

rice_i_harv05 <- aggregate(rice_i_harv,fact=6,fun=sum)


rm <- mask(rice_i_phys05,southeastasia)
rp <- rasterToPoints(rm)
rp[,1:2] <- round(rp[,1:2],2)
rice_i_phys_dt <- data.table(rp)

rm <- mask(rice_i_harv05,southeastasia)
rp <- rasterToPoints(rm)
rp[,1:2] <- round(rp[,1:2],2)
rice_i_harv_dt <- data.table(rp)


colnames(dm_ps)[3] <- "plant_srt"
dm_ps$plant_srt_date <- as.Date(dm_ps$plant_srt,origin="2000-01-01")
dm_ps$plant_srt <- as.numeric(substr(dm_ps$plant_srt_date,6,7))
dm_ps$plant_srt_date <- NULL
dm_ps <- dm_ps[order(x,y)]


ggplot(data = southeastasia) +
  geom_sf(color="gray",fill="NA",size=.25)+
  geom_point(data=rice_i_phys_dt,aes(x=x,y=y,size=spam2010V2r0_global_A_RICE_I))+
  scale_size(range=c(.1,2.5),guide="none")+
  coord_sf(xlim=c(91.75,141.25),ylim=c(-10.75,28.75))+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none")








# gunzip("goethe/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT.gz",remove=FALSE)

dt <- fread("goethe/growing_periods_listed/CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT")

rice_dt <- dt[crop %in% c(3,29)]



# load the map of se asia
load("acled_seasia.RData")

countries <- unique(acled_dt$country)

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


r <- rasterFromXYZ(rice_dt[subcrop==1,.(x=lon,y=lat,crop,subcrop,area,start,end)])

rm <- mask(r,southeastasia)
rp <- rasterToPoints(rm)
# rp[,1:2] <- round(rp[,1:2],2)
dt <- data.table(rp)


fourseasons <- colorRampPalette(colors=c("skyblue","seagreen","forestgreen","tan","indianred","goldenrod","darkgray","skyblue"),interpolate="spline")

fourseasons_col <- fourseasons(13)[c(13,2:12)]

rice_dt$start <- factor(rice_dt$start,levels=1:12)
rice_dt$end <- factor(rice_dt$end,levels=1:12)

ggplot(data = southeastasia) +
  geom_sf(color="gray",fill="NA",size=.25)+
  geom_point(data=rice_dt[crop==29 & subcrop==1],aes(x=lon,y=lat,size=area,color=end))+
  scale_size(range=c(.5,3.5),guide="none")+
  scale_color_manual(values=fourseasons_col,breaks=1:12,guide="none")+
  coord_sf(xlim=c(91.75,141.25),ylim=c(-10.75,28.75))+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none")







