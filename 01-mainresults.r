library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(Cairo)
library(scales)
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

"%!in%" <- Negate("%in%")

# these bits are for tables
pstars <- function(ps){
  p_stars <- ifelse(ps<.01,"***",ifelse(ps<.05,"**",ifelse(ps<.1,"*","")))
  return(p_stars)
}

f1 <- function(x) format(round(x,3),big.mark=",")
f2 <- function(x) format(round(x,0),big.mark=",")
gm <- list(list("raw"="nobs","clean"="Obs.","fmt"=f2),
           list("raw"="r.squared","clean"="R2","fmt"=f1))


## load the main dataset
load("masterdata.RData")

countries <- c(unique(datacomb_dt$country),"Singapore")

southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## drop Brunei and Timor-Leste
datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Timor-Leste")]
dataset_dt <- dataset_dt[country %!in% c("Brunei","Timor-Leste")]

## create conflict incidence variable
datacomb_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]
dataset_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]

# re-label the contemporaneous rain differently
datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]


# 00 - descriptive stats

datacomb_dt[,.(incidents=sum(incidents))]
datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
datacomb_dt[yearmo=="2020-01",.(cells=.N),by=.(country)]

tab1 <- datacomb_dt[,.(event="all events",obs=.N,incidents=sum(incidents),incidents_mean=round(mean(incidents),3),incidents_sd=round(sd(incidents),3),incidents_min=round(min(incidents),3),incidents_max=round(max(incidents),3),incidence_mean=round(mean(ifelse(incidents>0,1,0)),3),incidence_sd=round(sd(ifelse(incidents>0,1,0)),3))]

dataset_dt$event <- factor(dataset_dt$event,levels=unique(dataset_dt$event)[c(1,4,3,2,5)])

tab2 <- dataset_dt[,.(obs=.N,incidents=sum(incidents),incidents_mean=round(mean(incidents),3),incidents_sd=round(sd(incidents),3),incidents_min=round(min(incidents),3),incidents_max=round(max(incidents),3),incidence_mean=round(mean(ifelse(incidents>0,1,0)),3),incidence_sd=round(sd(ifelse(incidents>0,1,0)),3)),by=.(event)]

## TABLE 2 (top panel)
kable_styling(kable(rbind(tab1[,.(event,incidents,incidents_mean,incidents_sd,incidents_min,incidents_max,incidence_mean)],tab2[,.(event,incidents,incidents_mean,incidents_sd,incidents_min,incidents_max,incidence_mean)])))


tab3a <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_spam),3),Crop_area_sd=round(sd(area_spam),3),Crop_area_min=round(min(area_spam),3),Crop_area_max=round(max(area_spam),3))]

tab3i <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_i),3),Crop_area_sd=round(sd(area_i),3),Crop_area_min=round(min(area_i),3),Crop_area_max=round(max(area_i),3))]

tab3r <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_r),3),Crop_area_sd=round(sd(area_r),3),Crop_area_min=round(min(area_r),3),Crop_area_max=round(max(area_r),3))]

## TABLE 2 (bottom panel)
kable_styling(kable(rbind(tab3a[,.(Crop_area_mean,Crop_area_sd,Crop_area_min,Crop_area_max)],tab3i[,.(Crop_area_mean,Crop_area_sd,Crop_area_min,Crop_area_max)],tab3r[,.(Crop_area_mean,Crop_area_sd,Crop_area_min,Crop_area_max)])))


# 01 - main results ----
impact1 <- function(x){
  r <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  s <- 100*m$cropland/m$incidence
  h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
  h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
  h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std),output=c(h_coef,h_se)))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef0_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef1_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot the results
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)
dt_cn[1] <- "all events"

dt <- as.data.table(t(dt))

colnames(dt) <- c("est","se")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

gg_baseline <- ggplot(dt,aes(x=event,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color="dimgray")+
  geom_point(shape=dt$pch,size=2,stroke=1,color="dimgray",fill="white")+
  coord_flip()+
  labs(title="",x="Conflict Type",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/Extra/results_baseline.png",gg_baseline,width=6.5,height=4.0,dpi="retina")
ggsave("Figures/Extra/results_baseline.eps",gg_baseline,width=6.5,height=4.0,dpi="retina")


## 01a - Check: balanced panel (2018:2022) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))>=2018]

## effect
coef0_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))>2017]

## effect
coef1_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot the results
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)
dt_cn[1] <- "all events"

dt <- as.data.table(t(dt))

colnames(dt) <- c("est","se")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

gg_balanced1 <- ggplot(dt,aes(x=event,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color="dimgray")+
  geom_point(color="dimgray",shape=dt$pch,size=2,stroke=1,fill="white")+
  coord_flip()+
  labs(title="",x="Conflict Type",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/Extra/results_balanced_short.png",gg_balanced1,width=6.5,height=4.0,dpi="retina")
ggsave("Figures/Extra/results_balanced_short.eps",gg_balanced1,width=6.5,height=4.0,dpi="retina")


## 01b - Check: balanced panel (2010:2022) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef0_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef1_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot the results
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)
dt_cn[1] <- "all events"

dt <- as.data.table(t(dt))

colnames(dt) <- c("est","se")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

gg_balanced2 <- ggplot(dt,aes(x=event,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color="dimgray")+
  geom_point(color="dimgray",shape=dt$pch,size=2,stroke=1,fill="white")+
  coord_flip()+
  labs(title="",x="Conflict Type",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/Extra/results_balanced_long.png",gg_balanced2,width=6.5,height=4.0,dpi="retina")
ggsave("Figures/Extra/results_balanced_long.eps",gg_balanced2,width=6.5,height=4.0,dpi="retina")


## 01c - Check: drop one country at a time ----
list_of_countries <- unique(datacomb_dt$country)

lst <- list()

for(i in 1:length(list_of_countries)){

  ## combined effect ----
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[country!=list_of_countries[i]]

  ## impact
  c_comb <- impact1(datasub_dt)

  ## event-specific effects ----
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[country!=list_of_countries[i]]

  ## impact
  c_battles <- impact1(datasub_dt[event=="battles"])
  c_violence <- impact1(datasub_dt[event=="violence"])
  c_riots <- impact1(datasub_dt[event=="riots"])
  c_protests <- impact1(datasub_dt[event=="protests"])

  dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

  dt_cn <- colnames(dt)
  dt_cn[1] <- "all events"

  dt <- as.data.table(t(dt))

  colnames(dt) <- c("est","se")
  dt$event <- dt_cn

  dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

  dt$country <- list_of_countries[i]

  lst[[i]] <- dt

}

# combine the list elements into a data table
dropone_dt <- Reduce(rbind,lst)
dropone_dt <- dropone_dt[order(country)]

dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event),labels=c("all events","battles","violence","riots","protests"))

dropone_dt$country <- factor(dropone_dt$country,levels=unique(dropone_dt$country)[length(unique(dropone_dt$country)):1])

gg_dropone <- ggplot(dropone_dt,aes(x=country,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,shape=dropone_dt$pch,color=dropone_dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  facet_grid(.~event)+
  coord_flip()+
  labs(title="",x="Omitted country",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))

country_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
country_dt <- country_dt[order(country)]
country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country)[length(unique(country_dt$country)):1])
country_dt[,share:=(incidents/sum(incidents))]

gg_incidents <- ggplot(country_dt,aes(x=country,y=share))+
  geom_col(fill="slategray",width=.5)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  coord_flip()+
  labs(title="",x="",y="Share of incidents")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_blank())

gg_dropacountry <- plot_grid(gg_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))

ggsave("Figures/results_dropacountry.png",gg_dropacountry,width=6.5,height=4.0,dpi="retina",device="png")
ggsave("Figures/results_dropacountry.eps",gg_dropacountry,width=6.5,height=4.0,dpi="retina",device="eps")


## 01d - Check: drop one year at a time ----

list_of_years <- unique(datacomb_dt$year)

lst <- list()

for(i in 1:length(list_of_years)){

  ## combined effect ----
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[year!=list_of_years[i]]

  ## impact
  c_comb <- impact1(datasub_dt)

  ## event-specific effects ----
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[year!=list_of_years[i]]

  ## impact
  c_battles <- impact1(datasub_dt[event=="battles"])
  c_violence <- impact1(datasub_dt[event=="violence"])
  c_riots <- impact1(datasub_dt[event=="riots"])
  c_protests <- impact1(datasub_dt[event=="protests"])

  dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

  dt_cn <- colnames(dt)

  dt <- as.data.table(t(dt))

  colnames(dt) <- c("est","se")
  dt$event <- dt_cn

  dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

  dt$year <- list_of_years[i]

  lst[[i]] <- dt

}

# combine the list elements into a data table
dropone_dt <- Reduce(rbind,lst)
dropone_dt <- dropone_dt[order(year)]

dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event),labels=c("all events","battles","violence","riots","protests"))

dropone_dt$year <- factor(dropone_dt$year,levels=unique(dropone_dt$year)[length(unique(dropone_dt$year)):1])

gg_dropone <- ggplot(dropone_dt,aes(x=year,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,shape=dropone_dt$pch,color=dropone_dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  facet_grid(.~event)+
  coord_flip()+
  labs(title="",x="Omitted year",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))


year_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(year)]
year_dt <- year_dt[order(year)]
year_dt$year <- factor(year_dt$year,levels=unique(year_dt$year)[length(unique(year_dt$year)):1])
year_dt[,share:=(incidents/sum(incidents))]

gg_incidents <- ggplot(year_dt,aes(x=year,y=share))+
  geom_col(fill="slategray",width=.5)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  coord_flip()+
  labs(title="",x="",y="Share of incidents")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_blank())

gg_dropayear <- plot_grid(gg_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))


ggsave("Figures/results_dropayear.png",gg_dropayear,width=6.5,height=4.0,dpi="retina",device="png")
ggsave("Figures/results_dropayear.eps",gg_dropayear,width=6.5,height=4.0,dpi="retina",device="eps")


## 01e - Check: randomize harvest seasons ----
list_of_iter <- 1:100

lst <- list()

for(i in 1:length(list_of_iter)){

  ## combined effect ----
  datasub_dt <- datacomb_dt

  random_dt <- unique(datasub_dt[,.(xy,mo,harvest_season)])
  random_dt[,`:=`(id=as.numeric(factor(xy)))]
  sub_dt <- unique(random_dt[,.(id,xy)])
  set.seed(i)
  sub_dt[,`:=`(xy=sample(sub_dt$xy))]
  random_dt$xy <- NULL
  random_dt <- merge(random_dt,sub_dt,by="id",all.x=T)
  random_dt$id <- NULL

  datasub_dt$harvest_season <- NULL
  datasub_dt <- merge(datasub_dt,random_dt,by=c("xy","mo"),all.x=T)

  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  ## impact
  c_comb <- impact1(datasub_dt)

  ## event-specific effects ----
  datasub_dt <- dataset_dt

  random_dt <- unique(datasub_dt[,.(xy,mo,harvest_season)])
  random_dt[,`:=`(id=as.numeric(factor(xy)))]
  sub_dt <- unique(random_dt[,.(id,xy)])
  set.seed(i)
  xy_i <- sample(sub_dt$xy)
  sub_dt[,`:=`(xy=xy_i)]
  random_dt$xy <- NULL
  random_dt <- merge(random_dt,sub_dt,by="id",all.x=T)
  random_dt$id <- NULL

  datasub_dt$harvest_season <- NULL
  datasub_dt <- merge(datasub_dt,random_dt,by=c("xy","mo"),all.x=T)

  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  ## impact
  c_battles <- impact1(datasub_dt[event=="battles"])
  c_violence <- impact1(datasub_dt[event=="violence"])
  c_riots <- impact1(datasub_dt[event=="riots"])
  c_protests <- impact1(datasub_dt[event=="protests"])

  dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

  dt_cn <- colnames(dt)

  dt <- as.data.table(t(dt))

  colnames(dt) <- c("est","se")
  dt$event <- dt_cn

  dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

  dt$iter <- list_of_iter[i]

  lst[[i]] <- dt

  print(i)

}

shuffle_dt <- Reduce(rbind,lst)

shuffle_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

shuffle_dt$event <- factor(shuffle_dt$event,levels=unique(shuffle_dt$event),labels=c("all events","battles","violence","riots","protests"))

shuffle_dt$iter <- factor(shuffle_dt$iter,levels=unique(shuffle_dt$iter)[length(unique(shuffle_dt$iter)):1])

gg_shuffle <- ggplot(shuffle_dt,aes(x=iter,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.3,width=NA,color=shuffle_dt$col)+
  geom_point(size=0.8,shape=shuffle_dt$pch,color=shuffle_dt$col,fill="white",stroke=.4)+
  scale_x_discrete(breaks=seq(5,100,by=5))+
  facet_grid(.~event)+
  coord_flip(ylim=c(-30,30))+
  labs(title="",x="Iteration",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

gg_den <- ggplot(shuffle_dt,aes(x=est))+
  geom_density(adjust=1.3,color="slategray",linewidth=.5)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  facet_grid(.~event)+
  coord_cartesian(xlim=c(-30,30))+
  labs(title="",x="",y="Density")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

gg_comb <- plot_grid(gg_shuffle,gg_den,ncol=1,align="hv",axis="tblr",rel_heights=c(4,1))

ggsave("Figures/results_shuffleharvest.png",gg_comb,width=6.5,height=7.0,dpi="retina",device="png")
ggsave("Figures/results_shuffleharvest.eps",gg_comb,width=6.5,height=7.0,dpi="retina",device="eps")


# 02a - Irrigation ----
impact2a <- function(x){
  r <- feols(incidence~area:seas:irri+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area)),by=.(irri)]
  s <- 100*m$cropland/m$incidence
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long,irri=factor(ifelse(prop_i<.5,0,1)))]

## effect
coef0_fe <- feols(incidence~area:seas:irri+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2a(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long,irri=factor(ifelse(prop_i<.5,0,1)))]

## effect
coef1_fe <- feols(incidence~area:seas:irri+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:irri+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:irri+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas:irri+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact2a(datasub_dt[event=="battles"])
c_protests <- impact2a(datasub_dt[event=="protests"])
c_riots <- impact2a(datasub_dt[event=="riots"])
c_violence <- impact2a(datasub_dt[event=="violence"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot the results
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$plot <- rep(c("rainfed","irrigated"),each=2)

dt$parameter <- rep(c("est","se"),2)

long_dt <- melt(dt,id.vars=c("plot","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("plot","event","est")
colnames(long2_dt) <- c("plot","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","plot"))

long_dt$event <- factor(long_dt$event,levels=dt_cn[length(dt_cn):1],labels=c(dt_cn[length(dt_cn):2],"all events"))
long_dt$plot <- factor(long_dt$plot,levels=c("rainfed","irrigated"))

long_dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

gg_irrigated <- ggplot(long_dt,aes(x=event,y=est,color=plot,linetype=plot,group=plot))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.6,width=NA,position = position_dodge(.5))+
  geom_point(size=2,shape=long_dt$pch,fill="white",stroke=1,position = position_dodge(.5))+
  scale_color_manual(values=c("coral","steelblue"))+
  scale_linetype_manual(values=c(1,5))+
  coord_flip()+
  labs(title="",x="Month from harvest",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(.5,'in'))

ggsave("Figures/Extra/results_irrigated.png",gg_irrigated,width=6.5,height=5,dpi="retina")
ggsave("Figures/Extra/results_irrigated.eps",gg_irrigated,width=6.5,height=5,dpi="retina")



# 02b - Cities ----
impact2b <- function(x){
  r <- feols(incidence~area:seas:city_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area)),by=.(city_cat)]
  s <- 100*m$cropland/m$incidence
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long,city_cat=factor(ifelse(capital=="primary" | city_population>=1000000 | population>=2000000,1,0)))]

## effect
coef0_fe <- feols(incidence~area:seas:city_cat+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2b(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long,city_cat=factor(ifelse(capital=="primary" | city_population>=1000000 | population>=2000000,1,0)))]

## effect
coef1_fe <- feols(incidence~area:seas:city_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:city_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:city_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas:city_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact2b(datasub_dt[event=="battles"])
c_protests <- impact2b(datasub_dt[event=="protests"])
c_riots <- impact2b(datasub_dt[event=="riots"])
c_violence <- impact2b(datasub_dt[event=="violence"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot the results
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$plot <- rep(c("rural","urban"),each=2)

dt$parameter <- rep(c("est","se"),2)

long_dt <- melt(dt,id.vars=c("plot","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("plot","event","est")
colnames(long2_dt) <- c("plot","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","plot"))

long_dt$event <- factor(long_dt$event,levels=dt_cn[length(dt_cn):1],labels=c(dt_cn[length(dt_cn):2],"all events"))
long_dt$plot <- factor(long_dt$plot,levels=c("rural","urban"))

long_dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

gg_urban <- ggplot(long_dt,aes(x=event,y=est,color=plot,linetype=plot,group=plot))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.6,width=NA,position = position_dodge(.5))+
  geom_point(size=2,shape=long_dt$pch,fill="white",stroke=1,position = position_dodge(.5))+
  scale_color_manual(values=c("coral","steelblue"))+
  scale_linetype_manual(values=c(1,5))+
  coord_flip()+
  labs(title="",x="Month from harvest",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(.5,'in'))

ggsave("Figures/Extra/results_urban.png",gg_urban,width=6.5,height=5,dpi="retina")
ggsave("Figures/Extra/results_urban.eps",gg_urban,width=6.5,height=5,dpi="retina")


# 03a - Monthly ----
impact3a <- function(x){
  r <- feols(incidence~area:(l2+l1+s0+f1+f2)+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  s <- 100*m$cropland/m$incidence
  h_coef <- round(r$coeftable[,"Estimate"][2:6]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][2:6]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][2:6])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3],h_est[4],h_std[4],h_est[5],h_std[5]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3],h_coef[4],h_se[4],h_coef[5],h_se[5])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead")),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef0_fe <- feols(incidence~area:(l2+l1+s0+f1+f2)+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3a(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead")),by=.(xy,event)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef1_fe <- feols(incidence~area:(l2+l1+s0+f1+f2)+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:(l2+l1+s0+f1+f2)+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:(l2+l1+s0+f1+f2)+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:(l2+l1+s0+f1+f2)+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact3a(datasub_dt[event=="battles"])
c_violence <- impact3a(datasub_dt[event=="violence"])
c_riots <- impact3a(datasub_dt[event=="riots"])
c_protests <- impact3a(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# for plotting
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$period <- rep(c("-2","-1","0","+1","+2"),each=2)

dt$parameter <- rep(c("est","se"),5)

long_dt <- melt(dt,id.vars=c("period","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("period","event","est")
colnames(long2_dt) <- c("period","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","period"))

long_dt$event <- factor(long_dt$event,levels=dt_cn)
long_dt$period <- factor(long_dt$period,levels=unique(long_dt$period)[c(4,3,5,1,2)])

long_dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

long_dt <- long_dt[event!="combined"]

gg_monthly <- ggplot(long_dt,aes(x=period,y=est,group=event))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color="dimgray")+
  geom_point(color="dimgray",size=2,stroke=1,shape=long_dt$pch,fill="white")+
  facet_wrap(.~event,ncol=2)+
  labs(title="",x="Month from harvest",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()

ggsave("Figures/Extra/results_monthly.png",gg_monthly,width=6.5,height=5.5,dpi="retina")
ggsave("Figures/Extra/results_monthly.eps",gg_monthly,width=6.5,height=5.5,dpi="retina")



# 03b - Dose-response ----
impact3b <- function(x){
  r <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo,data=x,vcov=~xy)
  m <- x[,.(incidence=mean(incidence)),by=.(area)]
  m <- m[order(area)]
  s <- 100/m$incidence
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=factor(ifelse(area_spam>=.1 & area_spam<.2,1,ifelse(area_spam<.5,2,3))),seas=harvest_season)]

## effect
coef0_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3b(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=factor(ifelse(area_spam>=.1 & area_spam<.2,1,ifelse(area_spam<.5,2,3))),seas=harvest_season)]

## effect
coef1_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact3b(datasub_dt[event=="battles"])
c_violence <- impact3b(datasub_dt[event=="violence"])
c_riots <- impact3b(datasub_dt[event=="riots"])
c_protests <- impact3b(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# for plotting
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$dose <- rep(c("small","medium","large"),each=2)

dt$parameter <- rep(c("est","se"),3)

long_dt <- melt(dt,id.vars=c("dose","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("dose","event","est")
colnames(long2_dt) <- c("dose","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","dose"))

long_dt$event <- factor(long_dt$event,levels=dt_cn)
long_dt$dose <- factor(long_dt$dose,levels=unique(long_dt$dose)[3:1])

long_dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

long_dt <- long_dt[event!="combined"]

gg_dose <- ggplot(long_dt,aes(x=dose,y=est,group=event))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color="dimgray")+
  geom_point(color="dimgray",size=2,stroke=1,shape=long_dt$pch,fill="white")+
  facet_wrap(.~event,ncol=2)+
  labs(title="",x="Size of the cropland",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()

ggsave("Figures/Extra/results_dose.png",gg_dose,width=6.5,height=5.5,dpi="retina")
ggsave("Figures/Extra/results_dose.eps",gg_dose,width=6.5,height=5.5,dpi="retina")


# 03c - Rainfall ----

impact3c <- function(x){
  r <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  s <- 100*m$cropland/m$incidence
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long)]
datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef0_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3c(datasub_dt)

## event-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long)]

datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef1_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_battles <- impact3c(datasub_dt[event=="battles"])
c_protests <- impact3c(datasub_dt[event=="protests"])
c_riots <- impact3c(datasub_dt[event=="riots"])
c_violence <- impact3c(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$rain <- rep(c("dry","normal","wet"),each=2)

dt$parameter <- rep(c("est","se"),3)

long_dt <- melt(dt,id.vars=c("rain","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("rain","event","est")
colnames(long2_dt) <- c("rain","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","rain"))

long_dt$event <- factor(long_dt$event,levels=dt_cn)
long_dt$rain <- factor(long_dt$rain,levels=unique(long_dt$rain))

long_dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

long_dt <- long_dt[event!="combined"]

gg_rain <- ggplot(long_dt,aes(x=rain,y=est,group=event))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color="dimgray")+
  geom_point(color="dimgray",size=2,stroke=1,shape=long_dt$pch,fill="white")+
  facet_wrap(.~event,ncol=2)+
  labs(title="",x="Growing season rainfall",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()

ggsave("Figures/Extra/results_rain.png",gg_rain,width=6.5,height=5.5,dpi="retina")
ggsave("Figures/Extra/results_rain.eps",gg_rain,width=6.5,height=5.5,dpi="retina")


# 03d - Rainfall/Monthly ----

impact3d <- function(x){
  r <- feols(incidence~area:(l2+l1+s0+f1+f2):rain_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  s <- 100*m$cropland/m$incidence
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3],h_est[4],h_std[4],h_est[5],h_std[5],h_est[6],h_std[6],h_est[7],h_std[7],h_est[8],h_std[8],h_est[9],h_std[9],h_est[10],h_std[10],h_est[11],h_std[11],h_est[12],h_std[12],h_est[13],h_std[13],h_est[14],h_std[14],h_est[15],h_std[15]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3],h_coef[4],h_se[4],h_coef[5],h_se[5],h_coef[6],h_se[6],h_coef[7],h_se[7],h_coef[8],h_se[8],h_coef[9],h_se[9],h_coef[10],h_se[10],h_coef[11],h_se[11],h_coef[12],h_se[12],h_coef[13],h_se[13],h_coef[14],h_se[14],h_coef[15],h_se[15])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead")),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long)]
datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef0_fe <- feols(incidence~area:(l2+l1+s0+f1+f2):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3d(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead")),by=.(xy,event)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gs_rain_stand_long)]
datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef1_fe <- feols(incidence~area:(l2+l1+s0+f1+f2):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:(l2+l1+s0+f1+f2):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:(l2+l1+s0+f1+f2):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:(l2+l1+s0+f1+f2):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact3d(datasub_dt[event=="battles"])
c_violence <- impact3d(datasub_dt[event=="violence"])
c_riots <- impact3d(datasub_dt[event=="riots"])
c_protests <- impact3d(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# for plotting
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$period <- rep(c("-2","-1","0","+1","+2"),each=6)

dt$weather <- rep(rep(c("dry","normal","wet"),each=2),5)

dt$parameter <- rep(c("est","se"),15)

long_dt <- melt(dt,id.vars=c("period","weather","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("period","weather","event","est")
colnames(long2_dt) <- c("period","weather","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","period","weather"))

long_dt$event <- factor(long_dt$event,levels=dt_cn)
long_dt$period <- factor(long_dt$period,levels=c("-2","-1","0","+1","+2"))

long_dt[,`:=`(pch=ifelse(abs(est/se) > 1.96,16,21))]

long_dt <- long_dt[event!="combined"]

gg_rainmonthly <- ggplot(long_dt,aes(x=period,y=est,color=weather,linetype=weather,group=weather))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.6,width=NA,position=position_dodge(.5))+
  geom_point(size=2,shape=long_dt$pch,fill="white",stroke=1,position=position_dodge(.5))+
  scale_color_manual(values=c("coral","seagreen","steelblue"))+
  scale_linetype_manual(values=c(5,1,4))+
  facet_wrap(.~event,ncol=2)+
  labs(title="",x="Month from harvest",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
  theme_paper()+
  theme(legend.position="top",legend.key.width=unit(.5,'in'))

ggsave("Figures/results_rain_monthly.png",gg_rainmonthly,width=6.5,height=5.5,dpi="retina")
ggsave("Figures/results_rain_monthly.eps",gg_rainmonthly,width=6.5,height=5.5,dpi="retina")





