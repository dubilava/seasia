library(data.table)
library(ggplot2)
library(cowplot)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(R.utils)


rm(list=ls())
gc()


"%!in%" <- Negate("%in%")

getmode <- function(v){
  uniqv <- unique(v)[!is.na(unique(v))]
  uniqv[base::which.max(tabulate(match(v, uniqv)))]
}


# load the map of se asia
load("acled_seasia.RData")

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


travel_r <- raster("Cities/201501_Global_Travel_Time_to_Cities_2015/201501_Global_Travel_Time_to_Cities_2015.tif")

travel1_r <- aggregate(travel_r,fact=120,fun=mean)

mm <- mask(travel1_r,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
travel1_dt <- data.table(round(rp,1))
colnames(travel1_dt) <- c("x","y","minutes")

save(travel1_dt,file="travel.RData")


gg_map <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,size=.25)+
  geom_point(data=travel1_dt,aes(x=x,y=y,size=minutes),color="steelblue")+
  scale_size(range=c(.5,3.5),guide="none")+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none")

gg_hist <- ggplot(travel1_dt,aes(x=minutes/60)) +
  geom_histogram(bins=36,color="white",fill="steelblue")+
  labs(x="Hours",y="Count")+
  theme_classic()+
  theme(axis.title = element_text(size=9),axis.text = element_text(size=8),panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA))

aligned <- align_plots(gg_map,gg_hist,align="hv",axis="tblr")
gg_maplegend <- ggdraw(aligned[[1]]) + draw_plot(aligned[[2]],x=.64,y=.60,width=.36,height=.36)

ggsave("../../Figures/map_travel.png",gg_maplegend,width=6.5,height=5.5,dpi="retina",device="png")

ggsave("../../Figures/map_travel.eps",gg_maplegend,width=6.5,height=5.5,dpi="retina",device="eps")

