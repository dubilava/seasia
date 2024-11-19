library(data.table)
library(fixest)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(scales)
library(stringr)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")
library(kableExtra)
library(zoo)
library(modelsummary)
library(marginaleffects)


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

## create conflict incidence variable
datacomb_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]
dataset_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]

# re-label the contemporaneous rain differently
datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]

# load("Data/travel10.RData")
# colnames(travel10_dt)[1:2] <- c("longitude","latitude")
# 
# datacomb_dt <- merge(datacomb_dt,travel10_dt,by=c("longitude","latitude"))
# dataset_dt <- merge(dataset_dt,travel10_dt,by=c("longitude","latitude"))


# 01 - baseline ----

## Tab 3: baseline results ----

impact <- function(x){
  r <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[,.(incidents=mean(incidents),cropland=mean(area_spam))]
  s <- 100*m$cropland/m$incidents
  h_coef <- round(r$coeftable["area_spam:seas","Estimate"]*s,1)
  h_se <- round(r$coeftable["area_spam:seas","Std. Error"]*s,1)
  h_stars <- pstars(r$coeftable["area_spam:seas","Pr(>|t|)"])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std),output=c(h_coef,h_se)))
}

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef0_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## event-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef1_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef4_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


## Tab A1: balanced panel (2018:2023) ----

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))>=2018]

## effect
coef0_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## evens-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))>2017]

## effect
coef1_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))



## Tab A2: balanced panel (2010:2023) ----

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef0_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## event-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef1_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area_spam:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


## Fig B3: drop one country at a time ----

list_of_countries <- unique(datacomb_dt$country)

lst <- list()

for(i in 1:length(list_of_countries)){

  ## combined effect
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[country!=list_of_countries[i]]

  ## impact
  c_comb <- impact(datasub_dt)

  ## event-specific effects
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[country!=list_of_countries[i]]

  ## impact
  c_battles <- impact(datasub_dt[event=="battles"])
  c_violence <- impact(datasub_dt[event=="violence"])
  c_riots <- impact(datasub_dt[event=="riots"])
  c_protests <- impact(datasub_dt[event=="protests"])

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
  coord_flip(ylim=c(-35,35))+
  labs(title="",x="",subtitle="Omitted country",y="Harvest-time change in conflict relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))

country_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
country_dt <- country_dt[order(country)]
country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country)[length(unique(country_dt$country)):1])
country_dt[,share:=(incidents/sum(incidents))]

gg_incidents <- ggplot(country_dt,aes(x=country,y=share))+
  geom_col(fill="darkgray",width=.5)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  coord_flip()+
  labs(title="",x="",y="Share of incidents")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_blank())

gg_dropacountry <- plot_grid(gg_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))

ggsave("Figures/results_dropacountry.png",gg_dropacountry,width=6.25,height=6.25*9/16,dpi=350,device="png")
ggsave("Figures/results_dropacountry.eps",gg_dropacountry,width=6.25,height=6.25*9/16,dpi=350,device="eps")


dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"gray40",ifelse(est/se < -1.96,"gray40","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

bw_dropone <- ggplot(dropone_dt,aes(x=country,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,shape=dropone_dt$pch,color=dropone_dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  facet_grid(.~event)+
  coord_flip(ylim=c(-35,35))+
  labs(title="",x="",subtitle="Omitted country",y="Harvest-time change in conflict relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))


bw_dropacountry <- plot_grid(bw_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))

ggsave("Figures/results_dropacountry_bw.png",bw_dropacountry,width=6.25,height=6.25*9/16,dpi=350,device="png")
ggsave("Figures/results_dropacountry_bw.eps",bw_dropacountry,width=6.25,height=6.25*9/16,dpi=350,device="eps")


## Fig B4: drop one year at a time ----

list_of_years <- as.numeric(as.character(unique(datacomb_dt$year)[order(unique(datacomb_dt$year))]))

lst <- list()

for(i in 1:length(list_of_years)){

  ## combined effect
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[year!=list_of_years[i]]

  ## impact
  c_comb <- impact(datasub_dt)

  ## event-specific effects
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

  datasub_dt <- datasub_dt[year!=list_of_years[i]]

  ## impact
  c_battles <- impact(datasub_dt[event=="battles"])
  c_violence <- impact(datasub_dt[event=="violence"])
  c_riots <- impact(datasub_dt[event=="riots"])
  c_protests <- impact(datasub_dt[event=="protests"])

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
  coord_flip(ylim=c(-35,35))+
  labs(title="",x="",subtitle="Omitted year",y="Harvest-time change in conflict relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))


year_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(year)]
year_dt <- year_dt[order(year)]
year_dt$year <- factor(year_dt$year,levels=unique(year_dt$year)[length(unique(year_dt$year)):1])
year_dt[,share:=(incidents/sum(incidents))]

gg_incidents <- ggplot(year_dt,aes(x=year,y=share))+
  geom_col(fill="darkgray",width=.5)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  coord_flip()+
  labs(title="",x="",y="Share of incidents")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_blank())

gg_dropayear <- plot_grid(gg_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))

ggsave("Figures/results_dropayear.png",gg_dropayear,width=6.25,height=6.25*9/16,dpi=350,device="png")
ggsave("Figures/results_dropayear.eps",gg_dropayear,width=6.25,height=6.25*9/16,dpi=350,device="eps")



dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"gray40",ifelse(est/se < -1.96,"gray40","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

bw_dropone <- ggplot(dropone_dt,aes(x=year,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,shape=dropone_dt$pch,color=dropone_dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  facet_grid(.~event)+
  coord_flip(ylim=c(-35,35))+
  labs(title="",x="",subtitle="Omitted year",y="Harvest-time change in conflict relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))

bw_dropayear <- plot_grid(bw_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))

ggsave("Figures/results_dropayear_bw.png",bw_dropayear,width=6.25,height=6.25*9/16,dpi=350,device="png")
ggsave("Figures/results_dropayear_bw.eps",bw_dropayear,width=6.25,height=6.25*9/16,dpi=350,device="eps")

## Fig B6: randomize harvest seasons ----
list_of_iter <- 1:100

lst <- list()

for(i in 1:length(list_of_iter)){

  ## combined effect
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
  c_comb <- impact(datasub_dt)

  ## event-specific effects
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
  c_battles <- impact(datasub_dt[event=="battles"])
  c_violence <- impact(datasub_dt[event=="violence"])
  c_riots <- impact(datasub_dt[event=="riots"])
  c_protests <- impact(datasub_dt[event=="protests"])

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
  coord_flip(ylim=c(-35,35))+
  labs(title="",x="",subtitle="Iteration",y="Harvest-time change in conflict relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

gg_den <- ggplot(shuffle_dt,aes(x=est))+
  geom_density(color="dimgray",linewidth=.5)+
  scale_y_continuous(breaks=pretty_breaks(n=3))+
  facet_grid(.~event)+
  coord_cartesian(xlim=c(-35,35))+
  labs(title="",x="",y="",subtitle="Density")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

gg_comb <- plot_grid(gg_shuffle,gg_den,ncol=1,align="hv",axis="tblr",rel_heights=c(5,2))

ggsave("Figures/results_shuffleharvest.png",gg_comb,width=6.25,height=5.25,dpi=350,device="png")
ggsave("Figures/results_shuffleharvest.eps",gg_comb,width=6.25,height=5.25,dpi=350,device="eps")



shuffle_dt[,`:=`(col=ifelse(est/se > 1.96,"gray40",ifelse(est/se < -1.96,"gray40","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

bw_shuffle <- ggplot(shuffle_dt,aes(x=iter,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.3,width=NA,color=shuffle_dt$col)+
  geom_point(size=0.8,shape=shuffle_dt$pch,color=shuffle_dt$col,fill="white",stroke=.4)+
  scale_x_discrete(breaks=seq(5,100,by=5))+
  facet_grid(.~event)+
  coord_flip(ylim=c(-35,35))+
  labs(title="",x="",subtitle="Iteration",y="Harvest-time change in conflict relative to the baseline (%)")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

bw_comb <- plot_grid(bw_shuffle,gg_den,ncol=1,align="hv",axis="tblr",rel_heights=c(5,2))

ggsave("Figures/results_shuffleharvest_bw.png",bw_comb,width=6.25,height=5.25,dpi=350,device="png")
ggsave("Figures/results_shuffleharvest_bw.eps",bw_comb,width=6.25,height=5.25,dpi=350,device="eps")

# 02 - heterogeneity ----

## Fig 4/Tab A4 - Irrigated ----

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,irri=factor(ifelse(prop_i<.5,0,1)),treat=area_spam*harvest_season)]

## effect
coef0_fe <- feols(incidents~area_spam:seas+area_spam:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## evens-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,irri=factor(ifelse(prop_i<.5,0,1)),treat=area_spam*harvest_season)]

## effect
coef1_fe <- feols(incidents~area_spam:seas+area_spam:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area_spam:seas+area_spam:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area_spam:seas+area_spam:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area_spam:seas+area_spam:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)


eff1 <- as.data.table(plot_slopes(coef1_fe,variables="seas",condition = "prop_i",draw=F))
eff2 <- as.data.table(plot_slopes(coef2_fe,variables="seas",condition = "prop_i",draw=F))
eff3 <- as.data.table(plot_slopes(coef3_fe,variables="seas",condition = "prop_i",draw=F))
eff4 <- as.data.table(plot_slopes(coef4_fe,variables="seas",condition = "prop_i",draw=F))


eff1$incidents <- mean(datasub_dt[event=="battles"]$incidents)
eff2$incidents <- mean(datasub_dt[event=="violence"]$incidents)
eff3$incidents <- mean(datasub_dt[event=="riots"]$incidents)
eff4$incidents <- mean(datasub_dt[event=="protests"]$incidents)


eff1[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

eff2[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

eff3[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

eff4[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

gg1 <- ggplot(eff1,aes(x=prop_i,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  labs(y="",x="Proportion of irrigated land",subtitle="Harvest-time change in battles (%)")+
  coord_cartesian(ylim=c(-50,25))+
  theme_paper()

gg2 <- ggplot(eff2,aes(x=prop_i,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  labs(y="",x="Proportion of irrigated land",subtitle="Harvest-time change in violence (%)")+
  coord_cartesian(ylim=c(-50,25))+
  theme_paper()

gg3 <- ggplot(eff3,aes(x=prop_i,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  labs(y="",x="Proportion of irrigated land",subtitle="Harvest-time change in riots (%)")+
  coord_cartesian(ylim=c(-50,25))+
  theme_paper()

gg4 <- ggplot(eff4,aes(x=prop_i,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  labs(y="",x="Proportion of irrigated land",subtitle="Harvest-time change in protests (%)")+
  coord_cartesian(ylim=c(-50,25))+
  theme_paper()

gg_irri <- plot_grid(gg1,gg2,gg3,gg4,align="hv",axis="lr",ncol=2)

ggsave("Figures/results_irrigation.png",gg_irri,width=6.25,height=5.25,dpi=350,device="png")
ggsave("Figures/results_irrigation.eps",gg_irri,width=6.25,height=5.25,dpi=350,device=cairo_ps)


## Fig 5/Tab A5 - Cities ----

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,urban=ifelse(capital=="primary" | city_population>=2500000 | population>=2500000,1,0),treat=area_spam*harvest_season)]
datasub_dt[,`:=`(rural=1-urban)]

sub_dt <- datacomb_dt[yearmo=="2020-01"]
sub_dt[,.N,by=.(rural)]

### for table

## effect
coef0_fe <- feols(incidents~treat:urban+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## evens-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,urban=ifelse(capital=="primary" | city_population>=2500000 | population>=2500000,1,0),treat=area_spam*harvest_season)]
datasub_dt[,`:=`(rural=1-urban)]

## effect
coef1_fe <- feols(incidents~treat:urban+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~treat:urban+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~treat:urban+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~treat:urban+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)


### for plot

## effect
coef0_fe <- feols(incidents~treat+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## evens-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,urban=ifelse(capital=="primary" | city_population>=2500000 | population>=2500000,1,0),treat=area_spam*harvest_season)]
datasub_dt[,`:=`(rural=1-urban)]

## effect
coef1_fe <- feols(incidents~treat+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~treat+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~treat+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~treat+treat:rural+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


eff1 <- as.data.table(plot_slopes(coef1_fe,variables="treat",condition = "rural",draw=F))
eff2 <- as.data.table(plot_slopes(coef2_fe,variables="treat",condition = "rural",draw=F))
eff3 <- as.data.table(plot_slopes(coef3_fe,variables="treat",condition = "rural",draw=F))
eff4 <- as.data.table(plot_slopes(coef4_fe,variables="treat",condition = "rural",draw=F))


means_dt <- datasub_dt[,.(area_spam=mean(area_spam),incidents=mean(incidents)),by=.(event)]


eff1[,`:=`(estimate1=100*estimate*means_dt[event=="battles"]$area_spam/means_dt[event=="battles"]$incidents,conf.low1=100*conf.low*means_dt[event=="battles"]$area_spam/means_dt[event=="battles"]$incidents,conf.high1=100*conf.high*means_dt[event=="battles"]$area_spam/means_dt[event=="battles"]$incidents)]

eff2[,`:=`(estimate1=100*estimate*means_dt[event=="violence"]$area_spam/means_dt[event=="violence"]$incidents,conf.low1=100*conf.low*means_dt[event=="violence"]$area_spam/means_dt[event=="violence"]$incidents,conf.high1=100*conf.high*means_dt[event=="violence"]$area_spam/means_dt[event=="violence"]$incidents)]

eff3[,`:=`(estimate1=100*estimate*means_dt[event=="riots"]$area_spam/means_dt[event=="riots"]$incidents,conf.low1=100*conf.low*means_dt[event=="riots"]$area_spam/means_dt[event=="riots"]$incidents,conf.high1=100*conf.high*means_dt[event=="riots"]$area_spam/means_dt[event=="riots"]$incidents)]

eff4[,`:=`(estimate1=100*estimate*means_dt[event=="protests"]$area_spam/means_dt[event=="protests"]$incidents,conf.low1=100*conf.low*means_dt[event=="protests"]$area_spam/means_dt[event=="protests"]$incidents,conf.high1=100*conf.high*means_dt[event=="protests"]$area_spam/means_dt[event=="protests"]$incidents)]


gg1 <- ggplot(eff1,aes(x=rural,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("urban","rural"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="",subtitle="Harvest-time change in battles (%)")+
  coord_cartesian(ylim=c(-10,30))+
  theme_paper()

gg2 <- ggplot(eff2,aes(x=rural,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("urban","rural"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="",subtitle="Harvest-time change in violence (%)")+
  coord_cartesian(ylim=c(-10,30))+
  theme_paper()

gg3 <- ggplot(eff3,aes(x=rural,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("urban","rural"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="",subtitle="Harvest-time change in riots (%)")+
  coord_cartesian(ylim=c(-90,30))+
  theme_paper()

gg4 <- ggplot(eff4,aes(x=rural,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("urban","rural"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="",subtitle="Harvest-time change in protests (%)")+
  coord_cartesian(ylim=c(-90,30))+
  theme_paper()

gg_rural <- plot_grid(gg1,gg2,gg3,gg4,align="hv",axis="lr",ncol=2)

ggsave("Figures/results_population.png",gg_rural,width=6.25,height=5.25,dpi=350,device="png")
ggsave("Figures/results_population.eps",gg_rural,width=6.25,height=5.25,dpi=350,device=cairo_ps)


# 03 - mechanisms ----

## Fig 6/Tab A6 - Rainfall ----

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,treat=area_spam*harvest_season,gs_rain100=gs_rain/100)]

## effect
coef0_fe <- feols(incidents~treat+treat:(gs_rain100+I(gs_rain100^2))+rain_t | xy+yearmo, datasub_dt,vcov=~xy)

## event-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,treat=area_spam*harvest_season,gs_rain100=gs_rain/100)]


## effect
coef1_fe <- feols(incidents~treat+treat:(gs_rain100+I(gs_rain100^2))+rain_t | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~treat+treat:(gs_rain100+I(gs_rain100^2))+rain_t | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~treat+treat:(gs_rain100+I(gs_rain100^2))+rain_t | xy+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef4_fe <- feols(incidents~treat+treat:(gs_rain100+I(gs_rain100^2))+rain_t | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)


eff1 <- as.data.table(plot_slopes(coef1_fe,variables="treat",condition = "gs_rain100",draw=F))[gs_rain100<=2]
eff2 <- as.data.table(plot_slopes(coef2_fe,variables="treat",condition = "gs_rain100",draw=F))[gs_rain100<=2]
eff3 <- as.data.table(plot_slopes(coef3_fe,variables="treat",condition = "gs_rain100",draw=F))[gs_rain100<=2]
eff4 <- as.data.table(plot_slopes(coef4_fe,variables="treat",condition = "gs_rain100",draw=F))[gs_rain100<=2]


means_dt <- datasub_dt[,.(area_spam=mean(area_spam),incidents=mean(incidents)),by=.(event)]


eff1[,`:=`(estimate1=100*estimate*means_dt[event=="battles"]$area_spam/means_dt[event=="battles"]$incidents,conf.low1=100*conf.low*means_dt[event=="battles"]$area_spam/means_dt[event=="battles"]$incidents,conf.high1=100*conf.high*means_dt[event=="battles"]$area_spam/means_dt[event=="battles"]$incidents)]

eff2[,`:=`(estimate1=100*estimate*means_dt[event=="violence"]$area_spam/means_dt[event=="violence"]$incidents,conf.low1=100*conf.low*means_dt[event=="violence"]$area_spam/means_dt[event=="violence"]$incidents,conf.high1=100*conf.high*means_dt[event=="violence"]$area_spam/means_dt[event=="violence"]$incidents)]

eff3[,`:=`(estimate1=100*estimate*means_dt[event=="riots"]$area_spam/means_dt[event=="riots"]$incidents,conf.low1=100*conf.low*means_dt[event=="riots"]$area_spam/means_dt[event=="riots"]$incidents,conf.high1=100*conf.high*means_dt[event=="riots"]$area_spam/means_dt[event=="riots"]$incidents)]

eff4[,`:=`(estimate1=100*estimate*means_dt[event=="protests"]$area_spam/means_dt[event=="protests"]$incidents,conf.low1=100*conf.low*means_dt[event=="protests"]$area_spam/means_dt[event=="protests"]$incidents,conf.high1=100*conf.high*means_dt[event=="protests"]$area_spam/means_dt[event=="protests"]$incidents)]


gg1 <- ggplot(eff1,aes(x=gs_rain100,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Monthly growing season rainfall (100mm)",subtitle="Harvest-time change in battles (%)")+
  coord_cartesian(ylim=c(-70,120))+
  theme_paper()

gg2 <- ggplot(eff2,aes(x=gs_rain100,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Monthly growing season rainfall (100mm)",subtitle="Harvest-time change in violence (%)")+
  coord_cartesian(ylim=c(-70,120))+
  theme_paper()

gg3 <- ggplot(eff3,aes(x=gs_rain100,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Monthly growing season rainfall (100mm)",subtitle="Harvest-time change in riots (%)")+
  coord_cartesian(ylim=c(-120,70))+
  theme_paper()

gg4 <- ggplot(eff4,aes(x=gs_rain100,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Monthly growing season rainfall (100mm)",subtitle="Harvest-time change in protests (%)")+
  coord_cartesian(ylim=c(-120,70))+
  theme_paper()

gg_rain <- plot_grid(gg1,gg2,gg3,gg4,align="hv",axis="lr",ncol=2)

ggsave("Figures/results_rainfall.png",gg_rain,width=6.25,height=5.25,dpi=350,device="png")
ggsave("Figures/results_rainfall.eps",gg_rain,width=6.25,height=5.25,dpi=350,device=cairo_ps)




## Fig 7/Tab A7 - Conflict ----

## combined effect
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,conf=ifelse(gs_conflict>0,1,0))]

## effect
coef0_fe <- feols(incidents~area_spam:seas+conf+area_spam:seas:conf+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)


## event-specific effects
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,conf=ifelse(gs_conflict>0,1,0))]

## effect
coef1_fe <- feols(incidents~area_spam:seas+conf+area_spam:seas:conf+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area_spam:seas+conf+area_spam:seas:conf+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area_spam:seas+conf+area_spam:seas:conf+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area_spam:seas+conf+area_spam:seas:conf+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)


eff1 <- as.data.table(plot_slopes(coef1_fe,variables="seas",condition = "conf",draw=F))
eff2 <- as.data.table(plot_slopes(coef2_fe,variables="seas",condition = "conf",draw=F))
eff3 <- as.data.table(plot_slopes(coef3_fe,variables="seas",condition = "conf",draw=F))
eff4 <- as.data.table(plot_slopes(coef4_fe,variables="seas",condition = "conf",draw=F))


eff1$incidents <- mean(datasub_dt[event=="battles"]$incidents)
eff2$incidents <- mean(datasub_dt[event=="violence"]$incidents)
eff3$incidents <- mean(datasub_dt[event=="riots"]$incidents)
eff4$incidents <- mean(datasub_dt[event=="protests"]$incidents)


eff1[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

eff2[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

eff3[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]

eff4[,`:=`(estimate1=100*estimate*area_spam/incidents,conf.low1=100*conf.low*area_spam/incidents,conf.high1=100*conf.high*area_spam/incidents)]


gg1 <- ggplot(eff1,aes(x=conf,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("no","yes"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Growing season conflict incidence",subtitle="Harvest-time change in battles (%)")+
  coord_cartesian(ylim=c(-10,90))+
  theme_paper()

gg2 <- ggplot(eff2,aes(x=conf,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("no","yes"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Growing season conflict incidence",subtitle="Harvest-time change in violence (%)")+
  coord_cartesian(ylim=c(-10,90))+
  theme_paper()

gg3 <- ggplot(eff3,aes(x=conf,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("no","yes"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Growing season conflict incidence",subtitle="Harvest-time change in riots (%)")+
  coord_cartesian(ylim=c(-160,80))+
  theme_paper()

gg4 <- ggplot(eff4,aes(x=conf,y=estimate1,ymin=conf.low1,ymax=conf.high1)) +
  geom_col(alpha=.5,width=.5) +
  geom_errorbar(width=.1) +
  scale_x_discrete(breaks=c(0,1),labels=c("no","yes"))+
  scale_y_continuous(breaks=breaks_pretty(n=5))+
  labs(y="",x="Growing season conflict incidence",subtitle="Harvest-time change in protests (%)")+
  coord_cartesian(ylim=c(-160,80))+
  theme_paper()

gg_conf <- plot_grid(gg1,gg2,gg3,gg4,align="hv",axis="lr",ncol=2)

ggsave("Figures/results_conflict.png",gg_conf,width=6.25,height=5.25,dpi=350,device="png")
ggsave("Figures/results_conflict.eps",gg_conf,width=6.25,height=5.25,dpi=350,device=cairo_ps)



