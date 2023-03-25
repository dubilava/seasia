library(acled.api)
library(data.table)
library(ggplot2)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

load("acled_seasia.RData")
load("ged_seasia.RData")



# philippines_dt <- acled_dt[country=="Philippines"]


# load the map of Philippines
southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


# aggregate geo-precision to 0.25 degree
acled_dt$lon1deg <- round(as.numeric(acled_dt$longitude)/.25)*.25
acled_dt$lat1deg <- round(as.numeric(acled_dt$latitude)/.25)*.25

acled1deg_dt <- acled_dt[,.(Incidents=.N),by=.(lon1deg,lat1deg,event_type)]

# plot the map of incidents
ggplot(data = southeastasia) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents,alpha=Incidents,color=event_type))+
  scale_alpha(range=c(.5,.85),guide="none")+
  scale_size(range=c(.5,4.5),guide="none")+
  scale_color_brewer(type="qual",palette="Set3",guide="none")+
  facet_wrap(~ event_type)+
  labs(title="Conflict and Violence in Southeast Asia",subtitle="all incidents: 2010-2021",caption="Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))


acled1deg_dt <- acled_dt[,.(Incidents=.N),by=.(lon1deg,lat1deg,year,country)]

# plot the map of incidents
ggplot(data = southeastasia) +
  geom_sf(color="gray",fill="white")+
  geom_point(data=acled1deg_dt,aes(x=lon1deg,y=lat1deg,size=Incidents,alpha=Incidents,color=country))+
  scale_alpha(range=c(.5,.85),guide="none")+
  scale_size(range=c(.5,4.5),guide="none")+
  scale_color_brewer(type="qual",palette="Set3")+
  facet_wrap(~ year)+
  labs(title="Conflict and Violence in Southeast Asia",subtitle="all incidents: 2010-2021",caption="Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_void()+
  theme(legend.position="right",legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))


sub_dt <- acled_dt[,.(Incidents=.N),by=.(year,event_type)]

ggplot(sub_dt,aes(x=factor(year),y=Incidents,color=event_type,linetype=event_type,group=event_type))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_brewer(type="qual",palette="Set3")+
  labs(title="Conflict and Violence in Southeast Asia",subtitle="all incidents: 2016-2021",caption="Armed Conflict Location & Event Data \n Project (ACLED); www.acleddata.com")+
  theme_classic()+
  theme(legend.position=c(.03,.97),legend.justification=c(0,1),legend.title=element_blank(),legend.text=element_text(size=14,hjust=0),plot.caption=element_text(color="gray50",face="italic",size=10),plot.title=element_text(size=16))
