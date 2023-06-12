library(data.table)
library(ggplot2)
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
      panel.grid = element_line(colour=NULL,linetype=3,linewidth=.3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background=element_rect(fill="white",color=NA),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.caption = element_text(size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,0.25),"lines"),
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
      strip.text=element_text(size=rel(.9),colour="slategray",margin=margin(2,0,8,0))
    )
}

# data management ----

## load the main dataset
load("masterdata.RData")

countries <- unique(datacomb_dt$country)

southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


## drop Brunei and Timor-Leste
datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Timor-Leste")]
dataset_dt <- dataset_dt[country %!in% c("Brunei","Timor-Leste")]


# some graphs
sub_dt <- datacomb_dt[yearmo=="2020-01",.(area=area_spam,irri=prop_i,country)]

sum_dt <- datacomb_dt[,.(incidents=mean(incidents),area=mean(area_spam),irri=mean(ifelse(prop_i>=.5,1,0))),by=.(xy,country)]
sum_dt[,irri:=ifelse(irri==1,"irrigated","rainfed")]

## cropland area
gg_area <- ggplot(sub_dt,aes(x=area*100000))+
  geom_histogram(binwidth=20000,fill="seagreen",color="white",boundary=0)+
  labs(x="Rice cropland area (ha) in a cell",y="Count (cells)")+
  theme_paper()

ggsave("Figures/area.png",gg_area,width=6.5,height=4.0,dpi="retina")
ggsave("Figures/area.eps",gg_area,width=6.5,height=4.0,dpi="retina",device="eps")

## irrigation
gg_irri <- ggplot(sub_dt,aes(x=irri))+
  geom_histogram(binwidth=.05,fill="steelblue",color="white",boundary=0)+
  labs(x="Proportion of irrigated rice in a cell",y="Count (cells)")+
  theme_paper()

ggsave("Figures/irri.png",gg_irri,width=6.5,height=4.0,dpi="retina")
ggsave("Figures/irri.eps",gg_irri,width=6.5,height=4.0,dpi="retina",device="eps")


color_palette <- colorRampPalette(colors=c("indianred","coral","goldenrod","forestgreen","seagreen","steelblue","dimgray"),interpolate="spline")

sample_of_colors <- color_palette(16)[seq(2,16,2)]

sample_of_shapes <- c(21:24,21:24)

gg_scatter <- ggplot(sub_dt,aes(x=log(area*100000),y=irri))+
  geom_point(aes(color=country,fill=country,shape=country),size=1.5,stroke=.5,alpha=.75,na.rm=T)+
  scale_colour_manual(values=sample_of_colors)+
  scale_fill_manual(values=sample_of_colors)+
  scale_shape_manual(values=sample_of_shapes)+
  labs(x="Rice cropland area (ha), natural log",y="Proportion of irrigated rice")+
  theme_paper()+
  theme(legend.position="top")

ggsave("Figures/area_irri.png",gg_scatter,width=6.5,height=5.5,dpi="retina")
ggsave("Figures/area_irri.eps",gg_scatter,width=6.5,height=5.5,dpi="retina",device=cairo_ps)


gg_cor <- ggplot(sum_dt,aes(x=log(area*100000),y=log(incidents),shape=irri,color=irri))+
  geom_point(size=1.5,stroke=.7,na.rm=T)+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","coral"))+
  labs(x="Cropland area (ha), natural log",y="Average number of conflict incidents, natural log")+
  theme_paper()+
  theme(legend.position="top")

gg_den <- ggplot(sum_dt,aes(x=log(incidents),color=irri))+
  geom_density(na.rm=T)+
  scale_color_manual(values=c("steelblue","coral"))+
  labs(x="",y="Density")+
  coord_flip(ylim=c(0,.2))+
  theme_paper()

gg_comb <- plot_grid(gg_cor,gg_den,ncol=2,align="hv",axis="tb",rel_widths=c(3,1))

ggsave("Figures/area_conflict.png",gg_comb,width=6.5,height=4.0,dpi="retina")
ggsave("Figures/area_conflict.eps",gg_comb,width=6.5,height=4.0,dpi="retina",device="eps")





## A1: irrigation histogram ----

datacomb_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]
dataset_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]

check_dt <- datacomb_dt[year==2020 & month=="Jan"]
# check_dt <- check_dt[country %!in% c("Brunei","Laos","Timor-Leste")]

check_dt[,.(proportion=mean(prop_i)),by=.(country)]

mean(check_dt$area_spam)
sd(check_dt$area_spam)
min(check_dt$area_spam)
max(check_dt$area_spam)

gg <- ggplot(check_dt,aes(x=prop_i))+
  geom_histogram(fill="steelblue",color="white",bins=20)+
  labs(x="Proportion of Irrigated Croplands",y="Count")+
  theme_paper()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))

ggsave("Figures/irrigated.png",gg,width=6.5,height=3.5,dpi="retina")



# 01 - harvest calendar ----

## F3: production map ----

maps_dt <- datacomb_dt[year==2020]

maps_dt[,`:=`(irrig=ifelse(prop_i>=.5,1,0))]

cal_dt <- maps_dt[harvest_month==1,.(longitude,latitude,month,area_spam,area_i,area_r,irrig)]

cal_dt[,`:=`(xy=paste(longitude,latitude,sep=","))]

cal_dt <- cal_dt[order(longitude,latitude,month)]

cal_dt$month <- factor(cal_dt$month,levels = month.abb)

country_dt <- datacomb_dt[yearmo=="2020-01",.(xy,country)]

cal_dt <- merge(cal_dt,country_dt,by="xy")

## seasonal color scheme
fourseasons <- colorRampPalette(colors=c("skyblue2","seagreen","coral","indianred","goldenrod","skyblue2"),interpolate="spline")

fourseasons_col <- fourseasons(13)[c(13,2:12)]

gg_map <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,linewidth=.25)+
  geom_point(data=cal_dt,aes(x=longitude,y=latitude,size=area_spam,color=month))+
  geom_point(data=cal_dt[irrig==1],aes(x=longitude,y=latitude,size=area_spam,color="black"),shape=1,size=3)+
  scale_size(range=c(.3,2.5),guide="none")+
  scale_color_manual(values=fourseasons_col,breaks=month.abb,guide="none")+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none",plot.background=element_rect(fill="white",color=NA))

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
gg_maplegend <- ggdraw(aligned[[1]]) + draw_plot(aligned[[2]],x=.64,y=.60,width=.36,height=.36)

ggsave("Figures/map_harvest_main.png",gg_maplegend,width=6.5,height=5.5,dpi="retina",device="png")

ggsave("Figures/map_harvest_main.eps",gg_maplegend,width=6.5,height=5.5,dpi="retina",device="eps")


# 02 - conflict ----

datasub_dt <- dataset_dt

## F1: conflict map ----

conflict_dt <- dataset_dt[event %in% c("battles","violence","riots","protests"),.(incidents=sum(incidents)),by=.(country,xy,longitude,latitude,event)]

conflict_dt <- conflict_dt[incidents>0]


cities_dt <- fread("Local/Data/Cities/worldcities.csv")

secities_dt <- cities_dt[country %in% unique(datacomb_dt$country) & population!="NA"]

secities_dt <- secities_dt[order(-population)]

secities_dt$latitude <- round(round(secities_dt$lat,2)-.499)+.5
secities_dt$longitude <- round(round(secities_dt$lng,2)-.499)+.5

secities_sub1 <- secities_dt[,.(population=sum(population)),by=.(longitude,latitude,country)]

secities_sub2 <- secities_dt[secities_dt[,.I[population==max(population)],by=.(longitude,latitude,country)]$V1,.(longitude,latitude,country,city=city_ascii,capital,city_population=population)]

secities_sub <- merge(secities_sub1,secities_sub2,by=c("longitude","latitude","country"))

secities_sub <- secities_sub[order(-population,-city_population)]

secities_sub <- secities_sub[capital %in% c("admin","primary")]


countries_dt <- dataset_dt[event %in% c("battles","protests","riots","violence"),.(incidents=sum(incidents)),by=.(country,event)]

ccum_dt <- dataset_dt[event %in% c("battles","protests","riots","violence"),.(incidents=sum(incidents)),by=.(country)]

ccum_dt <- ccum_dt[order(-incidents)]

# countries_dt <- countries_dt[order(-incidents)]
countries_dt$country <- factor(countries_dt$country,levels=ccum_dt$country)

ccum_dt[,`:=`(country_incidents=paste0(country," (",incidents,")"))]


conflict_dt <- dcast(conflict_dt,country+xy+longitude+latitude~event,value.var="incidents")
conflict_dt[is.na(conflict_dt)] <- 0

conflict_dt[,`:=`(battles=battles,unrest=protests+riots,comb=battles+violence+protests+riots,both=log(battles+violence+protests+riots))]


countries_dt[,`:=`(event_new=ifelse(event%in%c("protests","riots"),"unrest",ifelse(event=="battles","battles","violence")))]

countries_dt <- countries_dt[,.(incidents=sum(incidents)),by=.(country,event=event_new)]


gg_conflict <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,linewidth=.25)+
  geom_scatterpie(data=conflict_dt,aes(x=longitude,y=latitude,r=both*.07),cols=c("battles","violence","unrest"),color="white",size=.05)+
  scale_fill_manual(values=c("darkgray","indianred","goldenrod"))+
  geom_point(data=secities_sub[population>=2000000 & city_population>=1000000],aes(x=longitude,y=latitude),color="black",fill=NA,shape=21,size=2)+
  geom_text_repel(data=secities_sub[population>=2000000 & city_population>=1000000],aes(x=longitude,y=latitude,label=city))+
  scale_size(range=c(.3,3.5))+
  theme_void()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.title = element_blank(),legend.text = element_text(hjust=0),legend.position = c(.5,.85),plot.background=element_rect(fill="white",color=NA))

countries_dt$ylab <- 0
countries_dt$event <- factor(countries_dt$event,levels=c("battles","violence","unrest"))

gg_bars <- ggplot(countries_dt,aes(x=reorder(country,incidents),y=incidents))+
  geom_bar(aes(fill=event),stat="identity",color="white",linewidth=.25)+
  geom_text(data=ccum_dt,aes(y=incidents+32000,label=country_incidents),vjust=0.5,hjust=0,nudge_y=300,size=3)+
  scale_fill_manual(values=c("darkgray","indianred","goldenrod"))+
  scale_y_reverse()+
  coord_flip(clip="off")+
  labs(y="",x="")+
  theme_void()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank(),panel.background=element_rect(fill="transparent",color=NA),plot.background=element_rect(fill="transparent",color=NA),legend.position="none",legend.title = element_blank(),legend.text=element_text(size=9))


aligned <- align_plots(gg_conflict,gg_bars,align="hv", axis="tblr")
gg_maplegend <- ggdraw(aligned[[1]]) + draw_plot(aligned[[2]],x=.62,y=.60,width=.36,height=.36)

ggsave("Figures/map_conflict.png",gg_maplegend,width=6.5,height=5.5,dpi="retina",device="png")

ggsave("Figures/map_conflict.eps",gg_maplegend,width=6.5,height=5.5,dpi="retina",device="eps")


## F2: conflict time series ----

conflict_ts <- dataset_dt[,.(incidents=sum(incidents),locations=uniqueN(xy)),by=.(event,date=as.Date(paste0(yearmo,"-01")))]

conflict_ts[,`:=`(rate=incidents/locations)]

conflict_ts$event <- factor(conflict_ts$event,levels=c("battles","violence","riots","protests"))

gg1 <- ggplot(conflict_ts,aes(x=date,y=rate,color=event,linetype=event))+
  geom_line()+
  scale_color_manual(values=c("darkgray","indianred","steelblue","goldenrod"))+
  labs(x="Year",y="Incidents per cell")+
  theme_paper()+
  theme(legend.position = "top")

gg2 <- ggplot(conflict_ts[,.(cells=mean(locations)),by=date],aes(x=date,y=cells))+
  geom_col(fill="darkgray",color="white")+
  scale_y_reverse()+
  labs(x="",y="Cells")+
  theme_paper()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.title.x = element_blank(),axis.text.x=element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",axis="lr",rel_heights = c(7,2))

ggsave("Figures/ts_conflict.png",gg_comb,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/ts_conflict.eps",gg_comb,width=6.5,height=4.5,dpi="retina",device="eps")

save(gg_map,gg_legend,gg_conflict,gg_bars,gg1,gg2,file="graphs.RData")


temp_dt <- datacomb_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]

conflict_ts <- dataset_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022)),.(incidents=sum(incidents),locations=uniqueN(xy)),by=.(event,date=as.Date(paste0(yearmo,"-01")))]

conflict_ts[,`:=`(rate=incidents/locations)]

conflict_ts$event <- factor(conflict_ts$event,levels=c("battles","violence","riots","protests"))

gg1 <- ggplot(conflict_ts,aes(x=date,y=rate,color=event,linetype=event))+
  geom_line()+
  scale_color_manual(values=c("darkgray","indianred","steelblue","goldenrod"))+
  labs(x="Year",y="Incidents per cell")+
  theme_paper()+
  theme(legend.position = "top")

gg2 <- ggplot(conflict_ts[,.(cells=mean(locations)),by=date],aes(x=date,y=cells))+
  geom_col(fill="darkgray",color="white")+
  scale_y_reverse()+
  labs(x="",y="Cells")+
  theme_paper()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.title.x = element_blank(),axis.text.x=element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",axis="lr",rel_heights = c(7,2))

ggsave("Figures/ts_conflict_myanmar.png",gg_comb,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/ts_conflict_myanmar.eps",gg_comb,width=6.5,height=4.5,dpi="retina",device="eps")

save(gg_map,gg_legend,gg_conflict,gg_bars,gg1,gg2,file="graphs.RData")


