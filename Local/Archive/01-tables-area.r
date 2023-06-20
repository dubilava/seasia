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
# library(msm)

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

datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]

datacomb_dt <- datacomb_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]
dataset_dt <- dataset_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]


# 01 - main effect ----

impact1 <- function(x){
  r <- feols(incidents~area_cat:seas+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[,.(incidents=mean(incidents),cropland=mean(area)),by=.(area_cat)]
  
  m[,s:=100/incidents]
  
  h_coef <- round(r$coeftable[,"Estimate"][-1]*m$s,1)
  
  h_se <- round(r$coeftable[,"Std. Error"][-1]*m$s,1)
  
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3],h_est[4],h_std[4]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3],h_coef[4],h_se[4])))
}


impact2 <- function(x){
  r1 <- feols(incidents~area_cat:seas+area_cat:seas:prop_i+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidents~area_cat:seas+area_cat:seas:I(1-prop_i)+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[,.(incidents=mean(incidents),cropland=mean(area)),by=.(area_cat)]
  
  m[,s:=100/incidents]
  
  h1_coef <- round(r1$coeftable[,"Estimate"][2:5]*m$s,1)
  h1_se <- round(r1$coeftable[,"Std. Error"][2:5]*m$s,1)
  h1_stars <- pstars(r1$coeftable[,"Pr(>|t|)"][2:5])
  
  h1_est <- paste0(format(round(h1_coef,1),nsmall=1),h1_stars)
  h1_std <- paste0("(",format(round(h1_se,1),nsmall=1),")")
  
  h2_coef <- round(r2$coeftable[,"Estimate"][2:5]*m$s,1)
  h2_se <- round(r2$coeftable[,"Std. Error"][2:5]*m$s,1)
  h2_stars <- pstars(r2$coeftable[,"Pr(>|t|)"][2:5])
  
  h2_est <- paste0(format(round(h2_coef,1),nsmall=1),h2_stars)
  h2_std <- paste0("(",format(round(h2_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2)),effect=c(h1_est[1],h1_std[1],h1_est[2],h1_std[2],h1_est[3],h1_std[3],h1_est[4],h1_std[4],h2_est[1],h2_std[1],h2_est[2],h2_std[2],h2_est[3],h2_std[3],h2_est[4],h2_std[4]),output=c(h1_coef[1],h1_se[1],h1_coef[2],h1_se[2],h1_coef[3],h1_se[3],h1_coef[4],h1_se[4],h2_coef[1],h2_se[1],h2_coef[2],h2_se[2],h2_coef[3],h2_se[3],h2_coef[4],h2_se[4])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
datasub_dt[,`:=`(area_cat=factor(ifelse(area<quantile(area,.25),1,ifelse(area<quantile(area,.5),2,ifelse(area<quantile(area,.75),3,4)))))]
# datasub_dt[,`:=`(area_cat=factor(ifelse(area<.1,0,1)))]
datasub_dt <- datasub_dt[order(area_cat)]

## effect
coef0_fe <- feols(incidents~area_cat:seas+area_cat:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
datasub_dt[,`:=`(area_cat=factor(ifelse(area<quantile(area,.25),1,ifelse(area<quantile(area,.5),2,ifelse(area<quantile(area,.75),3,4)))))]
# datasub_dt[,`:=`(area_cat=factor(ifelse(area<.1,0,1)))]
datasub_dt <- datasub_dt[order(area_cat)]

## effect
coef1_fe <- feols(incidents~area_cat:seas+area_cat:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area_cat:seas+area_cat:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area_cat:seas+area_cat:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area_cat:seas+area_cat:seas:prop_i+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_battles <- impact2(datasub_dt[event=="battles"])
c_violence <- impact2(datasub_dt[event=="violence"])
c_riots <- impact2(datasub_dt[event=="riots"])
c_protests <- impact2(datasub_dt[event=="protests"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))





# for plotting
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt$size <- rep(c("tiny","small","medium","large"),each=2)

dt$parameter <- rep(c("est","se"),4)

long_dt <- melt(dt,id.vars=c("size","parameter"))

long1_dt <- long_dt[parameter=="est"]
long2_dt <- long_dt[parameter=="se"]

long1_dt$parameter <- NULL
long2_dt$parameter <- NULL

colnames(long1_dt) <- c("size","event","est")
colnames(long2_dt) <- c("size","event","se")

long_dt <- merge(long1_dt,long2_dt,by=c("event","size"))


# dt <- as.data.table(t(dt))
# 
# colnames(long_dt) <- c("size","parameter","event","impact")
# dt$event <- dt_cn

long_dt$event <- factor(long_dt$event,levels=dt_cn)
long_dt$size <- factor(long_dt$size,levels=unique(long_dt$size)[4:1])

long_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]

long_dt <- long_dt[event!="combined"]

# main_dt <- long_dt

gg_dose <- ggplot(long_dt,aes(x=size,y=est,group=event))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color=long_dt$col)+
  geom_point(size=2,shape=long_dt$pch,color=long_dt$col,fill="white",stroke=1)+
  facet_wrap(.~event,ncol=2)+
  labs(title="",x="Rice cropland area (size)",y="Harvest-time change in conflict relative to the baseline (%)")+
  # coord_cartesian(xlim=c(0,9))+
  theme_paper()

ggsave("Figures/dose.png",gg_dose,width=6.5,height=4.5,dpi="retina")


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


