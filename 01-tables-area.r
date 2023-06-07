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
library(msm)

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
                                         

# 01 - main effect ----

impact1 <- function(x){
  r <- feols(incidents~(area+I(area^2)):seas | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  # v <- unique(x[,.(area,incidents=mean(incidents)),by=.(xy)])
  # v <- v[area>0]
  
  i_vec <- seq(.1,5,.1)#v$area#
  dt <- data.table(area_i=i_vec,est=as.numeric(NA),se=as.numeric(NA))
  for(i in i_vec){
    x[,`:=`(area_d1=area/i,area_d2=area^2-area*i)]
    
    r1 <- feols(incidents~(area_d1+area_d2):seas | xy+country^year+yearmo, data=x,vcov=~xy)
    dt[area_i==i]$est <- 100*r1$coeftable["area_d1:seas","Estimate"]/m$incidents
    dt[area_i==i]$se <- 100*r1$coeftable["area_d1:seas","Std. Error"]/m$incidents
  }

  return(dt)
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

## effect
coef0_fe <- feols(incidents~(area+I(area^2)):seas | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

## effect
coef1_fe <- feols(incidents~(area+I(area^2)):seas | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~(area+I(area^2)):seas | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~(area+I(area^2)):seas | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~(area+I(area^2)):seas | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

### battles ----

gg1 <- ggplot(c_battles,aes(x=area_i,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),color=NA,fill="gray",alpha=.5)+
  geom_line(size=.6,color="dimgray")+
  labs(title="Harvest-time battles",x="Rice cropland area (ha)",y="Change relative to the baseline (%)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()

sub_dt <- datacomb_dt[yearmo=="2020-01",.(area)]

gg2 <- ggplot(sub_dt,aes(x=area))+
  geom_histogram(bins=40,color="white",fill="seagreen",linewidth=.1,boundary=0)+
  labs(x="Rice cropland area (ha)",y="Count (cells)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()+
  theme(axis.line.x=element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",axis="lr",rel_heights=c(7,3))

ggsave("Figures/battles.png",gg_comb,width=6.5,height=4.0,dpi="retina")


### violence ----

gg1 <- ggplot(c_violence,aes(x=area_i,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),color=NA,fill="gray",alpha=.5)+
  geom_line(size=.6,color="dimgray")+
  labs(title="Harvest-time violence",x="Rice cropland area (ha)",y="Change relative to the baseline (%)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()

sub_dt <- datacomb_dt[yearmo=="2020-01",.(area)]

gg2 <- ggplot(sub_dt,aes(x=area))+
  geom_histogram(bins=40,color="white",fill="seagreen",linewidth=.1,boundary=0)+
  labs(x="Rice cropland area (ha)",y="Count (cells)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()+
  theme(axis.line.x=element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",axis="lr",rel_heights=c(7,3))

ggsave("Figures/violence.png",gg_comb,width=6.5,height=4.0,dpi="retina")


### riots ----

gg1 <- ggplot(c_riots,aes(x=area_i,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),color=NA,fill="gray",alpha=.5)+
  geom_line(size=.6,color="dimgray")+
  labs(title="Harvest-time riots",x="Rice cropland area (ha)",y="Change relative to the baseline (%)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()

sub_dt <- datacomb_dt[yearmo=="2020-01",.(area)]

gg2 <- ggplot(sub_dt,aes(x=area))+
  geom_histogram(bins=40,color="white",fill="seagreen",linewidth=.1,boundary=0)+
  labs(x="Rice cropland area (ha)",y="Count (cells)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()+
  theme(axis.line.x=element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",axis="lr",rel_heights=c(7,3))

ggsave("Figures/riots.png",gg_comb,width=6.5,height=4.0,dpi="retina")


### protests ----

gg1 <- ggplot(c_protests,aes(x=area_i,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),color=NA,fill="gray",alpha=.5)+
  geom_line(size=.6,color="dimgray")+
  labs(title="Harvest-time protests",x="Rice cropland area (ha)",y="Change relative to the baseline (%)")+
  coord_cartesian(xlim=c(0,9))+
  theme_paper()

sub_dt <- datacomb_dt[yearmo=="2020-01",.(area)]

gg2 <- ggplot(sub_dt,aes(x=area))+
  geom_histogram(bins=40,color="white",fill="seagreen",linewidth=.1,boundary=0)+
  labs(x="Rice cropland area (ha)",y="Count (cells)")+
  coord_cartesian(xlim=c(0,5))+
  theme_paper()+
  theme(axis.line.x=element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank())

gg_comb <- plot_grid(gg1,gg2,ncol=1,align="hv",axis="lr",rel_heights=c(7,3))

ggsave("Figures/protests.png",gg_comb,width=6.5,height=4.0,dpi="retina")

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)


