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

theme_paper <- function(){
  theme_classic+theme(
    panel.background=element_rect(fill="white",color=NA),
    plot.background=element_rect(fill="white",color=NA),
    panel.grid=element_blank(),
    plot.title=element_text(size=12,colour="gray20",family="sans"),
    axis.title=element_text(size=11,colour="black",family="sans"),
    axis.text=element_text(size=9,colour="black",family="sans",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line.x=element_line(colour="gray20"),
    axis.line.y=element_line(colour="gray20"),
    axis.ticks=element_line(colour="gray20"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="black",family="sans"),
    legend.key.size=unit(.75,'lines'),
    legend.background=element_rect(fill="transparent",color=NA),
    strip.background=element_blank(),
    strip.text=element_text(size=11,colour="gray20",family="sans",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}


# data management ----

## load the map of se asia
load("Local/Data/acled_seasia.RData")

countries <- unique(acled_dt$country)

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


## load the main dataset and the new production dataset
load("Local/Data/data_violence_acled.RData")
load("Local/Data/spam.RData")

colnames(spam_dt)[1:2] <- c("longitude","latitude")

spam_dt[,`:=`(area_spam=area_spam/100000,area_i=area_i/100000,area_r=area_r/100000,area_h=area_h/100000,area_l=area_l/100000,area_s=area_s/100000)]

datacomb_dt <- merge(datacomb_dt,spam_dt,by=c("longitude","latitude"),all.x=T)

dataset_dt <- merge(dataset_dt,spam_dt,by=c("longitude","latitude"),all.x=T)


## drop the countries with virtually no conflict
datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Laos","Timor-Leste")]

dataset_dt <- dataset_dt[country %!in% c("Brunei","Laos","Timor-Leste")]


## delete time-frames with no available conflict data
datacomb_dt <- datacomb_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datacomb_dt <- datacomb_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datacomb_dt <- datacomb_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]


dataset_dt <- dataset_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

dataset_dt <- dataset_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

dataset_dt <- dataset_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]


## some descriptive stats
datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
datacomb_dt[yearmo=="2020-01",.(cells=.N),by=.(country)]

tab1 <- datacomb_dt[,.(obs=.N,indicence_mean=round(mean(incidents_dum),3),indicence_sd=round(sd(incidents_dum),3),indicents_mean=round(mean(incidents),3),indicents_sd=round(sd(incidents),3),indicents_min=round(min(incidents),3),indicents_max=round(max(incidents),3))]

kable_styling(kable(tab1))

dataset_dt$event <- factor(dataset_dt$event,levels=unique(dataset_dt$event)[c(6,4,2,5,1,3)])

tab2 <- dataset_dt[,.(obs=.N,indicence_mean=round(mean(incidents_dum),3),indicence_sd=round(sd(incidents_dum),3),indicents_mean=round(mean(incidents),3),indicents_sd=round(sd(incidents),3),indicents_min=round(min(incidents),3),indicents_max=round(max(incidents),3)),by=.(event)]

kable_styling(kable(tab2))


tab3a <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_spam),3),Crop_area_sd=round(sd(area_spam),3),Crop_area_min=round(min(area_spam),3),Crop_area_max=round(max(area_spam),3))]

tab3i <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_i),3),Crop_area_sd=round(sd(area_i),3),Crop_area_min=round(min(area_i),3),Crop_area_max=round(max(area_i),3))]

tab3r <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_r),3),Crop_area_sd=round(sd(area_r),3),Crop_area_min=round(min(area_r),3),Crop_area_max=round(max(area_r),3))]

kable_styling(kable(rbind(tab3a,tab3i,tab3r)))


## A1: irrigation histogram ----

datacomb_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]
dataset_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]

check_dt <- datacomb_dt[year==2020 & month=="Jan"]
check_dt <- check_dt[country %!in% c("Brunei","Laos","Timor-Leste")]

check_dt[,.(proportion=mean(prop_i)),by=.(country)]

mean(check_dt$area_spam)
sd(check_dt$area_spam)
min(check_dt$area_spam)
max(check_dt$area_spam)

gg <- ggplot(check_dt,aes(x=prop_i))+
  geom_histogram(fill="steelblue",color="white",bins=20)+
  labs(x="Proportion of Irrigated Croplands",y="Count")+
  theme_paper()+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))

ggsave("Figures/irrigated.png",gg,width=6.5,height=3.5,dpi="retina")



# 01 - harvest calendar ----

## F3: production map ----

maps_dt <- datacomb_dt[year==2020]

maps_dt[,`:=`(irrig=ifelse(prop_i>=.5,1,0))]

maps_dt <- maps_dt[country %!in% c("Brunei","Laos","Timor-Leste")]

cal_dt <- maps_dt[season_rice==1 & Crop_Rice=="Rice",.(longitude,latitude,month,area_spam,area_i,area_r,irrig)]

cal_dt[,`:=`(xy=paste(longitude,latitude,sep=","))]

cal_dt <- cal_dt[order(longitude,latitude,month)]

cal_dt$month <- factor(cal_dt$month,levels = month.abb)

country_dt <- datacomb_dt[yearmo=="2020-01",.(xy,country)]

cal_dt <- merge(cal_dt,country_dt,by="xy")

## seasonal color scheme
fourseasons <- colorRampPalette(colors=c("skyblue","seagreen","forestgreen","tan","indianred","goldenrod","darkgray","skyblue"),interpolate="spline")

fourseasons_col <- fourseasons(13)[c(13,2:12)]


gg_map <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,size=.25)+
  geom_point(data=cal_dt,aes(x=longitude,y=latitude,size=area_spam,color=month))+
  geom_point(data=cal_dt[irrig==1],aes(x=longitude,y=latitude,size=area_spam,color="black"),shape=1,size=3)+
  scale_size(range=c(.3,2.5),guide="none")+
  scale_color_manual(values=fourseasons_col,breaks=month.abb,guide="none")+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none")

calsum_dt <- cal_dt[,.(Cells=.N),by=.(month)]
no_months <- month.abb[month.abb %!in% as.character(calsum_dt$month)]

calsum_dt <- rbind(calsum_dt,data.table(month=no_months,Cells=0))

calsum_dt <- calsum_dt[order(month)]
calsum_dt$month <- factor(calsum_dt$month,levels=month.abb)


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

# datawide_dt <- dcast(datasub_dt[,.(longitude,latitude,xy,country,yearmo,event,incidents)],longitude+latitude+xy+country+yearmo~event)
# 
# datawide_dt[,`:=`(conflict=battles+explosion,date=as.Date(paste0(yearmo,"-01")))]

## descriptive stats ----

# conflict_dt <- dataset_dt[event %in% c("conflict","violence","riots","protests"),.(incidents=sum(incidents)),by=.(xy,longitude,latitude,country,event,season_rice,date=as.Date(paste0(yearmo,"-01")))]
# 
# conflict_dt[incidents>50 & season_rice==1]
# 
# conflict_dt[country=="Indonesia" & event=="protests" & incidents>0 & season_rice==1]
# 
# conflict_dt[country=="Indonesia" & event=="protests" & incidents>0 & season_rice!=1]
# 
# harvest_dt <- conflict_dt[season_rice==1,.(incidents_at_harvest=mean(incidents)),by=.(xy,longitude,latitude,event)]
# 
# nonharv_dt <- conflict_dt[season_rice!=1,.(incidents_nonharvest=mean(incidents)),by=.(xy,longitude,latitude,event)]
# 
# compare_dt <- merge(harvest_dt,nonharv_dt,by=c("xy","longitude","latitude","event"))
# 
# compare_dt[,`:=`(conflict_dif=incidents_at_harvest-incidents_nonharvest)]
# 
# compare_dt[,.(incidents_at_harvest=mean(incidents_at_harvest),incidents_nonharvest=mean(incidents_nonharvest),conflict_dif=mean(conflict_dif)),by=event]
# 
# ggplot(compare_dt,aes(x=conflict_dif,color=event,fill=event))+
#   stat_density(alpha=.5,position="identity",bw=.1)+
#   coord_cartesian(xlim=c(-1,1))

## F1: conflict map ----

conflict_dt <- dataset_dt[event %in% c("conflict","violence","riots","protests"),.(incidents=sum(incidents)),by=.(xy,longitude,latitude,event)]

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


countries_dt <- dataset_dt[event %in% c("conflict","protests","riots","violence"),.(incidents=sum(incidents)),by=.(country,event)]

ccum_dt <- dataset_dt[event %in% c("conflict","protests","riots","violence"),.(incidents=sum(incidents)),by=.(country)]

ccum_dt <- ccum_dt[order(-incidents)]

# countries_dt <- countries_dt[order(-incidents)]
countries_dt$country <- factor(countries_dt$country,levels=ccum_dt$country)

ccum_dt[,`:=`(country_incidents=paste0(country," (",incidents,")"))]


conflict_dt <- dcast(conflict_dt,xy+longitude+latitude~event,value.var="incidents")
conflict_dt[is.na(conflict_dt)] <- 0

# colnames(conflict_dt) <- c("xy","longitude","latitude","violence","protests","battles")

conflict_dt[,`:=`(battles=conflict,unrest=protests+riots,comb=conflict+violence+protests+riots,both=log(conflict+violence+protests+riots))]


countries_dt[,`:=`(event_new=ifelse(event%in%c("protests","riots"),"unrest",ifelse(event=="conflict","battles","violence")))]

countries_dt <- countries_dt[,.(incidents=sum(incidents)),by=.(country,event=event_new)]


gg_conflict <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,linewidth=.25)+
  geom_scatterpie(data=conflict_dt,aes(x=longitude,y=latitude,r=both*.07),cols=c("battles","violence","unrest"),color="white",linewidth=.1)+
  scale_fill_manual(values=c("darkgray","indianred","goldenrod"))+
  geom_point(data=secities_sub[population>=2000000 & city_population>=1000000],aes(x=longitude,y=latitude),color="black",fill=NA,shape=21,size=2)+
  geom_text_repel(data=secities_sub[population>=2000000 & city_population>=1000000],aes(x=longitude,y=latitude,label=city))+
  scale_size(range=c(.3,3.5))+
  theme_void()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.title = element_blank(),legend.text = element_text(hjust=0),legend.position = c(.5,.85))

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

conflict_ts <- dataset_dt[event %in% c("conflict","violence","riots","protests"),.(incidents=sum(incidents),locations=uniqueN(xy)),by=.(event,date=as.Date(paste0(yearmo,"-01")))]

conflict_ts[,`:=`(rate=incidents/locations)]

conflict_ts$event <- factor(conflict_ts$event,levels=c("conflict","violence","riots","protests"),labels=c("Battles and Explosions","Violence Against Civilians","Riots","Protests"))

gg1 <- ggplot(conflict_ts,aes(x=date,y=rate,color=event,linetype=event))+
  geom_line()+
  scale_color_manual(values=c("darkgray","indianred","steelblue","goldenrod"))+
  labs(x="Year",y="Incidents per cell")+
  theme_classic()+
  theme_paper()+
  theme(legend.title=element_blank(),legend.position = c(.18,.82))

gg2 <- ggplot(conflict_ts[,.(cells=mean(locations)),by=date],aes(x=date,y=cells))+
  geom_col(fill="darkgray",color="white")+
  scale_y_reverse()+
  labs(x="",y="Cells")+
  theme_classic()+
  theme_paper()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.title.x = element_blank(),axis.text.x=element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",rel_heights = c(7,2))

ggsave("Figures/ts_conflict.png",gg_comb,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/ts_conflict.eps",gg_comb,width=6.5,height=4.5,dpi="retina",device="eps")


# #----------------------#
# #--  Harvest Season  --#
# #----------------------#
# 
# datasrt_dt <- dataset_dt[year==2020]
# datasrt_dt <- datasrt_dt[season_srt %in% 1]
# datasrt_dt <- datasrt_dt[,.(xy,country,harvest_srt=month,crop,area=max_area,season_srt)]
# datasrt_dt$season_srt <- NULL
# datasrt_dt <- unique(datasrt_dt)
# 
# datamid_dt <- dataset_dt[year==2020]
# datamid_dt <- datamid_dt[season %in% 1]
# datamid_dt <- datamid_dt[,.(xy,country,harvest_mid=month,crop,area=max_area,season)]
# datamid_dt$season <- NULL
# datamid_dt <- unique(datamid_dt)
# 
# dataend_dt <- dataset_dt[year==2020]
# dataend_dt <- dataend_dt[season_end %in% 1]
# dataend_dt <- dataend_dt[,.(xy,country,harvest_end=month,crop,area=max_area,season_end)]
# dataend_dt$season_end <- NULL
# dataend_dt <- unique(dataend_dt)
# 
# datahs_dt <- merge(datasrt_dt,dataend_dt,by=c("xy","country","crop","area"))
# datahs_dt <- merge(datamid_dt,datahs_dt,by=c("xy","country","crop","area"))
# 
# datahs_dt$harvest_srt <- factor(datahs_dt$harvest_srt,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# datahs_dt$harvest_mid <- factor(datahs_dt$harvest_mid,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# datahs_dt$harvest_end <- factor(datahs_dt$harvest_end,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# country_xy_dt <- datahs_dt[,.(xy,country,crop,area,harvest_srt,harvest_mid,harvest_end)]
# country_xy_dt <- unique(country_xy_dt)
# 
# country_dt <- country_xy_dt[,.(gridcells=.N,area=mean(area)),by=.(country,crop,harvest_srt,harvest_mid,harvest_end)]
# 
# country <- as.character(country_dt$country)
# month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# 
# country_month <- CJ(country,month)
# country_month_dt <- unique(country_month)
# 
# country_month_dt <- merge(country_dt,country_month_dt,by=c("country"),allow.cartesian=T)
# 
# country_month_dt <- country_month_dt[order(country,crop,-gridcells,month)]
# 
# country_month_dt$month <- factor(country_month_dt$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# country_month_dt$crop <- factor(country_month_dt$crop,levels=c("Maize","Sorghum","Wheat","Rice"))
# 
# # adjust instances when the starting and ending months are 'backwards'
# country_month_dt[,`:=`(m_dif=as.numeric(as.Date(paste("2020",harvest_end,"01",sep="-"),format="%Y-%b-%d")-as.Date(paste("2020",harvest_srt,"01",sep="-"),format="%Y-%b-%d")))]
# 
# temp_dt <- country_month_dt[m_dif<0]
# 
# temp1_dt <- temp_dt
# temp1_dt$harvest_end <- "Dec"
# temp2_dt <- temp_dt
# temp2_dt$harvest_srt <- "Jan"
# 
# dbl_dt <- rbind(temp1_dt,temp2_dt)
# 
# country_month_dt <- rbind(country_month_dt[m_dif>=0],dbl_dt)
# country_month_dt$m_dif <- NULL
# 
# 
# gg_hs_white <- ggplot(country_month_dt,aes(x=month,y=crop))+
#   geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
#   geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
#   facet_wrap(~country,nrow=10)+
#   scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
#   scale_alpha(range=c(.05,.35))+
#   scale_size(range=c(.5,3.5))+
#   scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
#   labs(x="Months",y="Crops")+
#   theme_classic()+
#   theme_white()+
#   theme(strip.text=element_text(size=8,colour="gray55",family="sans",face="bold",margin=margin(.1,0,.1,0,"cm")))
# 
# ggsave("Figures/harvestseasons.png",gg_hs_white,width=6.5,height=7.5,dpi=200)
# 
# country_sub <- unique(country_month_dt$country)[seq(1,45,4)]
# 
# gg_hs_white <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
#   geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
#   geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
#   facet_wrap(~country,ncol=4)+
#   scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
#   scale_alpha(range=c(.05,.35))+
#   scale_size(range=c(.5,3.5))+
#   scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
#   labs(x="Months",y="Crops")+
#   theme_classic()+
#   theme_white()
# 
# gg_hs_black <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
#   geom_linerange(aes(xmin=harvest_srt,xmax=harvest_end,size=gridcells,color=crop,alpha=area))+
#   geom_point(aes(x=harvest_mid,y=crop,size=gridcells,color=crop,alpha=area),shape=21)+
#   facet_wrap(~country,ncol=4)+
#   scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
#   scale_alpha(range=c(.05,.35))+
#   scale_size(range=c(.5,3.5))+
#   scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
#   labs(x="Months",y="Crops")+
#   theme_classic()+
#   theme_black()
# 
# ggsave("Presentation/harvestseasons_sub.png",gg_hs_white,width=6.5,height=3.5,dpi="retina")
# ggsave("Online/harvestseasons_sub.png",gg_hs_black,width=6.5,height=3.5,dpi="retina")
# 
# #----------------------#
# #--  Growing Season  --#
# #----------------------#
# 
# dataharv_dt <- datacomb_dt[year==2020]
# dataharv_dt <- dataharv_dt[season %in% 1]
# dataharv_dt <- dataharv_dt[,.(xy,country,harvest=month,crop,area=max_area,season)]
# dataharv_dt$season <- NULL
# dataharv_dt <- unique(dataharv_dt)
# 
# dataplant_dt <- datacomb_dt[year==2020]
# dataplant_dt <- dataplant_dt[planted %in% 1]
# dataplant_dt <- dataplant_dt[,.(xy,country,plant=month,crop,area=max_area,planted)]
# dataplant_dt$planted <- NULL
# dataplant_dt <- unique(dataplant_dt)
# 
# datags_dt <- merge(dataplant_dt,dataharv_dt,by=c("xy","country","crop","area"))
# 
# datags_dt$plant <- factor(datags_dt$plant,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# datags_dt$harvest <- factor(datags_dt$harvest,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# 
# country_xy_dt <- datags_dt[,.(xy,country,crop,area,plant,harvest)]
# country_xy_dt <- unique(country_xy_dt)
# 
# country_dt <- country_xy_dt[,.(gridcells=.N,area=mean(area)),by=.(country,crop,plant,harvest)]
# 
# country <- as.character(country_dt$country)
# month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# 
# country_month <- CJ(country,month)
# country_month_dt <- unique(country_month)
# 
# country_month_dt <- merge(country_dt,country_month_dt,by=c("country"),allow.cartesian=T)
# 
# country_month_dt <- country_month_dt[order(country,crop,-gridcells,month)]
# 
# country_month_dt$month <- factor(country_month_dt$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# country_month_dt$crop <- factor(country_month_dt$crop,levels=c("Maize","Sorghum","Wheat","Rice"))
# 
# # adjust instances when the starting and ending months are 'backwards'
# country_month_dt[,`:=`(m_dif=as.numeric(as.Date(paste("2020",harvest,"01",sep="-"),format="%Y-%b-%d")-as.Date(paste("2020",plant,"01",sep="-"),format="%Y-%b-%d")))]
# 
# temp_dt <- country_month_dt[m_dif<0]
# 
# temp1_dt <- temp_dt
# temp1_dt$harvest <- "Dec"
# temp2_dt <- temp_dt
# temp2_dt$plant <- "Jan"
# 
# dbl_dt <- rbind(temp1_dt,temp2_dt)
# 
# country_month_dt <- rbind(country_month_dt[m_dif>=0],dbl_dt)
# country_month_dt$m_dif <- NULL
# 
# country_month_dt <- country_month_dt[order(country,crop)]
# 
# gg_gs_white <- ggplot(country_month_dt,aes(x=month,y=crop))+
#   geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
#   facet_wrap(~ country,nrow=10)+
#   scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
#   scale_alpha(range=c(.05,.35))+
#   scale_size(range=c(.5,3.5))+
#   scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
#   labs(x="Months",y="Crops")+
#   theme_classic()+
#   theme_white()+
#   theme(strip.text=element_text(size=8,colour="gray55",family="sans",face="bold",margin=margin(.1,0,.1,0,"cm")))
# 
# ggsave("Figures/growingseasons.png",gg_gs_white,width=6.5,height=7.5,dpi=200)
# 
# 
# gg_gs_white <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
#   geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
#   facet_wrap(~ country,ncol=4)+
#   scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
#   scale_alpha(range=c(.05,.35))+
#   scale_size(range=c(.5,3.5))+
#   scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
#   labs(x="Months",y="Crops")+
#   theme_classic()+
#   theme_white()
# 
# gg_gs_black <- ggplot(country_month_dt[country %in% country_sub],aes(x=month,y=crop))+
#   geom_linerange(aes(xmin=plant,xmax=harvest,size=gridcells,color=crop,alpha=area))+
#   facet_wrap(~ country,ncol=4)+
#   scale_color_manual(values=c("forestgreen","indianred","goldenrod","steelblue"))+
#   scale_alpha(range=c(.05,.35))+
#   scale_size(range=c(.5,3.5))+
#   scale_x_discrete(labels=c("Jan" = "J","Feb" = "F","Mar" = "M","Apr" = "A","May" = "M","Jun" = "J","Jul" = "J","Aug" = "A","Sep" = "S","Oct" = "O","Nov" = "N","Dec" = "D"))+
#   labs(x="Months",y="Crops")+
#   theme_classic()+
#   theme_black()
# 
# ggsave("Presentation/growingseasons_sub.png",gg_gs_white,width=6.5,height=3.5,dpi="retina")
# ggsave("Online/growingseasons_sub.png",gg_gs_black,width=6.5,height=3.5,dpi="retina")





