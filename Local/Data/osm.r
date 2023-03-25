library(data.table)
library(ggplot2)
library(osmdata)
library(osmextract)
library(sf)

# load the map of se asia
load("acled_seasia.RData")

# unique(acled_dt$country)
cambodia <- ne_countries(country="Cambodia",returnclass="sf",scale="large")
cambodia <- st_set_crs(cambodia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

cambodia_oe <- oe_get("Cambodia",quiet = FALSE)

cambodia_sub <- st_crop(cambodia_oe["highway"][cambodia_oe["highway"]$highway %in% c("motorway","trunk","primary"),],cambodia)

ggplot(data = cambodia) +
  geom_sf(color="black",fill=NA,size=1)+
  geom_sf(data = cambodia_sub,color="red")



# unique(acled_dt$country)
philippines <- ne_countries(country="Philippines",returnclass="sf",scale="large")
philippines <- st_set_crs(philippines,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

philippines_oe <- oe_get("Philippines",quiet = FALSE)

philippines_sub <- st_crop(philippines_oe["highway"][philippines_oe["highway"]$highway %in% c("motorway","trunk","primary"),],philippines)

ggplot(data = philippines) +
  geom_sf(color="black",fill=NA,size=1)+
  geom_sf(data =philippines_sub,color="red")





  geom_point(data=cal_dt[season=="Main harvest"],aes(x=longitude,y=latitude,size=Crop_area,color=month))+
  scale_size(range=c(.1,1.8),guide="none")+
  scale_color_manual(values=fourseasons_col,breaks=month.abb,guide="none")+
  theme_void()+
  # theme_white()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none")
