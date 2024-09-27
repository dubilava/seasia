library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scatterpie)
library(cowplot)
library(gridExtra)
library(Cairo)
library(stringr)
library(sf)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")
library(kableExtra)
library(scales)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

theme_paper <- function(base_size=11,border=F){
  theme_foundation(base_size=base_size) +
    theme(
      line = element_line(linetype=1,linewidth=.4,colour="dimgray"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="black"),
      panel.background=element_rect(fill="white",color=NA),
      panel.grid = element_line(colour=NULL,linetype=3,linewidth=.3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.subtitle=element_text(colour="black",hjust=0,size=rel(0.9),margin=margin(t=2,r=2,b=2,l=2)),
      plot.caption = element_text(size=rel(0.7),colour="dimgray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,0.25),"lines"),
      plot.title.position="plot",
      plot.caption.position="plot",
      axis.title = element_text(size=rel(0.9),margin=margin(t=2,r=2,b=2,l=2)),
      axis.text = element_text(size=rel(0.8),margin=margin(t=1,r=1,b=1,l=1)),
      axis.text.x = element_text(colour = NULL),
      axis.text.y = element_text(colour = NULL),
      axis.ticks = element_blank(),
      axis.line = element_line(linetype=1,linewidth=.2,colour="black"),
      axis.line.y = element_blank(),
      legend.background=element_rect(fill="transparent",color=NA),
      legend.position="none",
      legend.title=element_blank(),
      legend.text=element_text(size=rel(0.8),colour="black"),
      legend.key = element_rect(fill="transparent"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(size=rel(.9),colour="black",margin=margin(2,0,8,0))
    )
}

# data management ----


# load acled to source the map
load("acled.RData")

countries <- unique(acled_dt$country)

southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


## load the main dataset
load("masterdata.RData")

## drop Brunei and Timor-Leste
datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Timor-Leste")]
dataset_dt <- dataset_dt[country %!in% c("Brunei","Timor-Leste")]




# Tab 2: Descriptive stats ----

datacomb_dt[,.(incidents=sum(incidents))]
datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
datacomb_dt[yearmo=="2020-01",.(cells=.N),by=.(country)]

tab1 <- datacomb_dt[,.(event="all events",obs=.N,incidents=sum(incidents),incidents_mean=round(mean(incidents),3),incidents_sd=round(sd(incidents),3),incidents_min=round(min(incidents),3),incidents_max=round(max(incidents),3),incidence_mean=round(mean(ifelse(incidents>0,1,0)),3),incidence_sd=round(sd(ifelse(incidents>0,1,0)),3))]

dataset_dt$event <- factor(dataset_dt$event,levels=unique(dataset_dt$event)[c(1,4,3,2,5)])

tab2 <- dataset_dt[,.(obs=.N,incidents=sum(incidents),incidents_mean=round(mean(incidents),3),incidents_sd=round(sd(incidents),3),incidents_min=round(min(incidents),3),incidents_max=round(max(incidents),3),incidence_mean=round(mean(ifelse(incidents>0,1,0)),3),incidence_sd=round(sd(ifelse(incidents>0,1,0)),3)),by=.(event)]

## Table 2 (top panel)
kable_styling(kable(rbind(tab1[,.(event,incidents,incidents_mean,incidents_sd,incidents_min,incidents_max,incidence_mean)],tab2[,.(event,incidents,incidents_mean,incidents_sd,incidents_min,incidents_max,incidence_mean)])))


tab3a <- datacomb_dt[yearmo=="2020-01",.(country="all countries",obs=.N,Crop_area_mean=round(mean(area_spam),3),Crop_area_sd=round(sd(area_spam),3),Crop_area_min=round(min(area_spam),3),Crop_area_max=round(max(area_spam),3))]

tab3i <- datacomb_dt[yearmo=="2020-01",.(country="all countries",obs=.N,Crop_area_mean=round(mean(area_i),3),Crop_area_sd=round(sd(area_i),3),Crop_area_min=round(min(area_i),3),Crop_area_max=round(max(area_i),3))]

tab3r <- datacomb_dt[yearmo=="2020-01",.(country="all countries",obs=.N,Crop_area_mean=round(mean(area_r),3),Crop_area_sd=round(sd(area_r),3),Crop_area_min=round(min(area_r),3),Crop_area_max=round(max(area_r),3))]


## Table 2 (bottom panel)
kable_styling(kable(rbind(tab3a[,.(Crop_area_mean,Crop_area_sd,Crop_area_min,Crop_area_max)],tab3i[,.(Crop_area_mean,Crop_area_sd,Crop_area_min,Crop_area_max)],tab3r[,.(Crop_area_mean,Crop_area_sd,Crop_area_min,Crop_area_max)])))



# Fig 1: Conflict map ----

conflict_dt <- dataset_dt[event %in% c("battles","violence","riots","protests"),.(incidents=sum(incidents),city,capital,city_population=mean(city_population),population=mean(population)),by=.(country,xy,longitude,latitude,event)]

conflict_dt <- unique(conflict_dt)
conflict_dt <- conflict_dt[incidents>0]

countries_dt <- dataset_dt[event %in% c("battles","protests","riots","violence"),.(incidents=sum(incidents)),by=.(country,event)]

ccum_dt <- dataset_dt[event %in% c("battles","protests","riots","violence"),.(incidents=sum(incidents)),by=.(country)]

ccum_dt <- ccum_dt[order(-incidents)]

countries_dt$country <- factor(countries_dt$country,levels=ccum_dt$country)

ccum_dt[,`:=`(country_incidents=paste0(country," (",incidents,")"))]

conflict_dt <- dcast(conflict_dt,country+xy+longitude+latitude+population+city_population+city+capital~event,value.var="incidents")
conflict_dt[is.na(conflict_dt)] <- 0

conflict_dt[,`:=`(battles=battles,unrest=protests+riots,comb=battles+violence+protests+riots,both=log(battles+violence+protests+riots))]

countries_dt[,`:=`(event_new=ifelse(event%in%c("protests","riots"),"unrest",ifelse(event=="battles","battles","violence")))]

countries_dt <- countries_dt[,.(incidents=sum(incidents)),by=.(country,event=event_new)]

conflict_dt[,urban:=ifelse(capital=="primary" | population>=2500000 | city_population>=1000000,"urban","")]

gg_conflict <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,linewidth=.25)+
  geom_scatterpie(data=conflict_dt,aes(x=longitude,y=latitude,r=both*.06),cols=c("battles","violence","unrest"),color=NA)+
  scale_fill_manual(values=c("gray","indianred","goldenrod"))+
  geom_point(data=conflict_dt[capital=="primary" | city_population>=2500000],aes(x=longitude,y=latitude),color="dimgray",fill="white",shape=21,size=1,stroke=.3,na.rm=T)+
  geom_text_repel(data=conflict_dt[capital=="primary" | city_population>=2500000],aes(x=longitude,y=latitude,label=city),color="black",size=3,seed=100)+
  theme_void()+
  guides(fill=guide_legend(order=2),shape=guide_legend(order=1))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.title = element_blank(),legend.text = element_text(hjust=0),legend.position = c(.87,.48),plot.background=element_rect(fill="white",color=NA))

countries_dt$ylab <- 0
countries_dt$event <- factor(countries_dt$event,levels=c("battles","violence","unrest"))

gg_bars <- ggplot(countries_dt,aes(x=reorder(country,incidents),y=incidents))+
  geom_bar(aes(fill=event),stat="identity",color="white",linewidth=.25)+
  geom_text(data=ccum_dt,aes(y=incidents+5000,label=country_incidents),vjust=0.5,hjust=1,nudge_y=300,size=3)+
  scale_fill_manual(values=c("gray","indianred","goldenrod"))+
  scale_y_reverse()+
  coord_flip(clip="off")+
  labs(y="",x="")+
  theme_void()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank(),panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),legend.position="none",legend.title = element_blank(),legend.text=element_text(size=9))

aligned <- align_plots(gg_conflict,gg_bars,align="hv", axis="tblr")
gg_maplegend <- ggdraw(aligned[[1]]) + draw_plot(aligned[[2]],x=.60,y=.60,width=.35,height=.35)


ggsave("Figures/map_conflict.png",gg_maplegend,width=6.25,height=5.25,dpi="retina",device="png")
ggsave("Figures/map_conflict.eps",gg_maplegend,width=6.25,height=5.25,dpi="retina",device="eps")



# Fig 2: Conflict series ----

conflict_ts <- dataset_dt[,.(incidents=sum(incidents),locations=uniqueN(xy)),by=.(event,date=as.Date(paste0(yearmo,"-01")))]

conflict_ts[,`:=`(rate=incidents/locations)]

conflict_ts$event <- factor(conflict_ts$event,levels=c("battles","violence","riots","protests"))

gg1 <- ggplot(conflict_ts,aes(x=date,y=rate,color=event,linetype=event))+
  geom_line()+
  scale_color_manual(values=c("darkgray","indianred","steelblue","goldenrod"))+
  labs(x="Year",y="",subtitle="Average number of incidents per cell")+
  theme_paper()+
  theme(legend.position=c(.5,.85),legend.direction="horizontal")

gg2 <- ggplot(conflict_ts[,.(cells=mean(locations)),by=date],aes(x=date,y=cells))+
  geom_col(fill="darkgray",color="white")+
  scale_y_reverse()+
  labs(x="",y="",subtitle="Cells")+
  theme_paper()+
  theme(axis.line = element_blank(),panel.grid.major=element_blank(),axis.ticks = element_blank(),axis.title.x = element_blank(),axis.text.x=element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="v",axis="tb",rel_heights = c(10,3))

gg_comb

ggsave("Figures/series_conflict.png",gg_comb,width=6.25,height=6.25*9/16,dpi="retina",device="png")
ggsave("Figures/series_conflict.eps",gg_comb,width=6.25,height=6.25*9/16,dpi="retina",device="eps")



# Fig 3: Harvest map ----

maps_dt <- datacomb_dt[year==2020]

maps_dt[,`:=`(irrig=ifelse(prop_i>=.5,"irrigated",""))]

cal_dt <- maps_dt[harvest_month==1,.(longitude,latitude,month,area_spam,area_i,area_r,irrig)]

cal_dt[,`:=`(xy=paste(longitude,latitude,sep=","))]

cal_dt <- cal_dt[order(longitude,latitude,month)]

cal_dt$month <- factor(cal_dt$month,levels = month.abb)

country_dt <- datacomb_dt[yearmo=="2020-01",.(xy,country)]

cal_dt <- merge(cal_dt,country_dt,by="xy")
cal_dt[,prop_i:=area_i/area_spam]

## seasonal color scheme
fourseasons <- colorRampPalette(colors=c("skyblue","seagreen","coral","indianred","goldenrod","skyblue"),interpolate="spline")

fourseasons_col <- fourseasons(13)[c(13,2:12)]

gg_map <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,linewidth=.25)+
  geom_point(data=cal_dt,aes(x=longitude,y=latitude,size=area_spam,color=month))+
  geom_point(data=cal_dt,aes(x=longitude,y=latitude,alpha=prop_i,size=area_spam),shape=21,color="dimgray",fill=NA,size=3.2,na.rm=T)+
  scale_size(range=c(.2,2.6),guide="none")+
  scale_alpha(breaks=c(0,.5,1),limits=c(0,1))+
  scale_color_manual(values=fourseasons_col,breaks=month.abb,guide="none")+
  theme_void()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = c(.87,.43),plot.background=element_rect(fill="white",color=NA))+
  guides(alpha=guide_legend(title="irrigated"))

calsum_dt <- cal_dt[,.(Cells=.N),by=.(month)]
no_months <- month.abb[month.abb %!in% as.character(calsum_dt$month)]

calsum_dt <- rbind(calsum_dt,data.table(month=no_months,Cells=0))

calsum_dt <- calsum_dt[order(month)]
calsum_dt$month <- factor(calsum_dt$month,levels=month.abb)

# the map legend
gg_legend <- ggplot(calsum_dt,aes(x=month,y=Cells,fill=month)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=fourseasons_col,guide="none") +
  ylim(0,140)+
  coord_polar() +
  theme_void() +
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text.x = element_text(size=10),axis.text.y = element_blank())

aligned <- align_plots(gg_map,gg_legend,align="hv", axis="tblr")
gg_maplegend <- ggdraw(aligned[[1]]) + draw_plot(aligned[[2]],x=.63,y=.60,width=.35,height=.35)


ggsave("Figures/map_harvest.png",gg_maplegend,width=6.25,height=5.25,dpi="retina",device="png")
ggsave("Figures/map_harvest.eps",gg_maplegend,width=6.25,height=5.25,dpi="retina",device="eps")



# Fig A1: GS rainfall ----

sub1_dt <- datacomb_dt[,.(rain=gs_rain)]

gg_rain <- ggplot(sub1_dt,aes(x=rain/100))+
  geom_histogram(binwidth=.2,fill="darkgray",color="white",boundary=0)+
  labs(x="Monthly growing season rainfall (100mm)",y="",subtitle="Count (cell-years)")+
  theme_paper()


ggsave("Figures/appendix_rainfall.png",gg_rain,width=6.25,height=6.25*9/16,dpi="retina")
ggsave("Figures/appendix_rainfall.eps",gg_rain,width=6.25,height=6.25*9/16,dpi="retina",device="eps")

# Fig A2: Irrigation ----

sub2_dt <- datacomb_dt[yearmo=="2020-01",.(area=area_spam,irri=prop_i,country)]

gg_irri <- ggplot(sub2_dt,aes(x=irri))+
  geom_histogram(binwidth=.05,fill="darkgray",color="white",boundary=0)+
  labs(x="Proportion of irrigated land",y="",subtitle="Count (cells)")+
  theme_paper()

ggsave("Figures/appendix_irrigation.png",gg_irri,width=6.25,height=6.25*9/16,dpi="retina")
ggsave("Figures/appendix_irrigation.eps",gg_rain,width=6.25,height=6.25*9/16,dpi="retina",device="eps")






