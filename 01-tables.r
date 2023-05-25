library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(Cairo)
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

theme_paper <- function(base_size=12,border=F){
  theme_foundation(base_size=base_size) +
    theme(
      line = element_line(linetype=1,colour="black"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="black"),
      # title = element_text(family=title_family,size=rel(1.1)),
      # panel.background=element_rect(fill="transparent",color=NA),
      panel.grid = element_line(colour=NULL,linetype=3,linewidth=.3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.background=element_rect(fill="transparent",color=NA),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.caption = element_text(size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,0.25),"lines"),
      # axis.title = element_blank(),
      axis.text = element_text(size=rel(0.9),margin=margin(t=1,r=1,b=1,l=1)),
      axis.text.x = element_text(colour = NULL),
      axis.text.y = element_text(colour = NULL),
      axis.ticks = element_blank(),
      axis.line = element_line(),
      axis.line.y = element_blank(),
      legend.background=element_rect(fill="transparent",color=NA),
      legend.position="none",
      legend.title=element_blank(),
      legend.text=element_text(size=rel(0.9),colour="slategray"),
      legend.key = element_rect(fill="transparent"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(size=rel(.8),colour="slategray",margin=margin(.1,0,.1,0,"cm"))
    )
}

"%!in%" <- Negate("%in%")

pstars <- function(ps){
  p_stars <- ifelse(ps<.01,"***",ifelse(ps<.05,"**",ifelse(ps<.1,"*","")))
  return(p_stars)
}

impact <- function(x){
  r <- feols(incidents~area:seas | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents

  h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std),output=c(h_coef,h_se)))
}


f1 <- function(x) format(round(x,3),big.mark=",")
f2 <- function(x) format(round(x,0),big.mark=",")
gm <- list(list("raw"="nobs","clean"="Obs.","fmt"=f2),
           list("raw"="r.squared","clean"="R2","fmt"=f1))


# data management ----

load("Local/Data/data_violence_acled.RData")
load("Local/Data/spam.RData")

colnames(spam_dt)[1:2] <- c("longitude","latitude")

spam_dt[,`:=`(area_spam=area_spam/100000,area_i=area_i/100000,area_r=area_r/100000,area_h=area_h/100000,area_l=area_l/100000,area_s=area_s/100000)]

datacomb_dt <- merge(datacomb_dt,spam_dt,by=c("longitude","latitude"),all.x=T)

dataset_dt <- merge(dataset_dt,spam_dt,by=c("longitude","latitude"),all.x=T)


datacomb_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]
dataset_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]

# check_dt <- datacomb_dt[year==2020 & month=="Jan"]
# check_dt <- check_dt[country %!in% c("Brunei","Laos","Timor-Leste")]
# 
# check_dt[,.(proportion=mean(prop_i)),by=.(country)]
# 
# mean(check_dt$area_spam)
# sd(check_dt$area_spam)
# min(check_dt$area_spam)
# max(check_dt$area_spam)
# 
# gg <- ggplot(check_dt,aes(x=prop_i))+
#   geom_histogram(fill="steelblue",color="white",bins=20)+
#   labs(x="Proportion of Irrigated Croplands",y="Count")+
#   theme_paper()+
#   theme_classic()+
#   theme(axis.title = element_text(size=12),axis.text = element_text(size=10))
# 
# ggsave("Figures/irrigated.png",gg,width=6.5,height=3.5,dpi="retina")

## combined ----

### harvest months ----
datacomb_dt[,`:=`(harvest=ifelse(season==1 | season2==1,1,0),harvest_rice=ifelse(season_rice==1 | season2_rice==1,1,0))]

### planting months ----
datacomb_dt[,`:=`(plant=ifelse(planting==1 | planting2==1,1,0),plant_rice=ifelse(planting_rice==1 | planting2_rice==1,1,0))]


### growing season (rice, main) ----
datacomb_dt[,`:=`(rice_p=ifelse(Rice_plant_mid==1,1,0),rice_h=ifelse(Rice_harvest_mid==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_mid))>1 & as.numeric(as.character(Rice_harvest_mid))>as.numeric(as.character(Rice_plant_mid))]$rice_p <- 1

datacomb_dt[,`:=`(rice_growing_season=cumsum(rice_p-rice_h)),by=.(xy)]

datacomb_dt[,`:=`(rice_growing_season=rice_growing_season+rice_h)]


### harvest season (rice, main) ----
datacomb_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_m=ifelse(Rice_harvest_mid==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

datacomb_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy)]

datacomb_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]

### harvest season (maize, main) ----
datacomb_dt[,`:=`(maize_s=ifelse(Maize_harvest_srt==1,1,0),maize_e=ifelse(Maize_harvest_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Maize_harvest_srt))>1 & as.numeric(as.character(Maize_harvest_end))>as.numeric(as.character(Maize_harvest_srt))]$maize_s <- 1

datacomb_dt[,`:=`(maize_harvest_season=cumsum(maize_s-maize_e)),by=.(xy)]

datacomb_dt[,`:=`(maize_harvest_season=maize_harvest_season+maize_e)]

### harvest season ----
datacomb_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0))]

datacomb_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]



## event-specific ----

### harvest months ----
dataset_dt[,`:=`(harvest=ifelse(season==1 | season2==1,1,0),harvest_rice=ifelse(season_rice==1 | season2_rice==1,1,0))]

### planting months ----
dataset_dt[,`:=`(plant=ifelse(planting==1 | planting2==1,1,0),plant_rice=ifelse(planting_rice==1 | planting2_rice==1,1,0))]


### growing season (rice, main) ----
dataset_dt[,`:=`(rice_p=ifelse(Rice_plant_mid==1,1,0),rice_h=ifelse(Rice_harvest_mid==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_plant_mid))>1 & as.numeric(as.character(Rice_harvest_mid))>as.numeric(as.character(Rice_plant_mid))]$rice_p <- 1

dataset_dt[,`:=`(rice_growing_season=cumsum(rice_p-rice_h)),by=.(xy)]

dataset_dt[,`:=`(rice_growing_season=rice_growing_season+rice_h)]

### harvest season (rice, main) ----
dataset_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_m=ifelse(Rice_harvest_mid==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

dataset_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy,event)]

dataset_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]

### harvest season (maize, main) ----
dataset_dt[,`:=`(maize_s=ifelse(Maize_harvest_srt==1,1,0),maize_e=ifelse(Maize_harvest_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Maize_harvest_srt))>1 & as.numeric(as.character(Maize_harvest_end))>as.numeric(as.character(Maize_harvest_srt))]$maize_s <- 1

dataset_dt[,`:=`(maize_harvest_season=cumsum(maize_s-maize_e)),by=.(xy,event)]

dataset_dt[,`:=`(maize_harvest_season=maize_harvest_season+maize_e)]

### harvest season ----
dataset_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0))]

dataset_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]


## finishing touches ----
datacomb_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]
dataset_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]

datacomb_dt[,`:=`(Rice_dum=ifelse(Rice_area>.01,1,0),Area_dum=ifelse(Crop_area>.01,1,0))]
dataset_dt[,`:=`(Rice_dum=ifelse(Rice_area>.01,1,0),Area_dum=ifelse(Crop_area>.01,1,0))]

dataset_dt$event <- factor(dataset_dt$event,levels=c("conflict","protests","riots","violence","strategic"))

# datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Laos","Timor-Leste")]
# dataset_dt <- dataset_dt[country %!in% c("Brunei","Laos","Timor-Leste")]


# 01 - main effect ----

## combined effect ----

# datacomb_dt[country=="Myanmar" & year==2021 & rice_m==1]

datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]#harvest_season

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]#harvest_season

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_conflict <- impact(datasub_dt[event=="conflict"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est","se")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

main_dt <- dt

# ggplot(main_dt,aes(x=event,y=est))+
#   geom_hline(yintercept=0,color="lightgray",linetype=5,linewidth=.6)+
#   geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=1,width=NA,color=main_dt$col)+
#   geom_point(size=3,color=main_dt$col)+
#   labs(x="",y="Estimated Impact (%)")+
#   coord_flip()+
#   theme_classic()+
#   theme(axis.ticks = element_blank(),axis.text.y=element_text(hjust=0))


# 01a - Check: balanced panel (2016:2022) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

# datasub_dt <- datasub_dt[country %!in% c("Malaysia")]
datasub_dt <- datasub_dt[as.numeric(as.character(year))>2017]



## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

# datasub_dt <- datasub_dt[country %!in% c("Malaysia")]
datasub_dt <- datasub_dt[as.numeric(as.character(year))>2017]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_conflict <- impact(datasub_dt[event=="conflict"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# 01b - Check: balanced panel (2010:2022) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_conflict <- impact(datasub_dt[event=="conflict"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))



# 01c - Check: drop 2020-2022 ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

# datasub_dt <- datasub_dt[country %!in% c("Malaysia")]
datasub_dt <- datasub_dt[as.numeric(as.character(year))<2020]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

# datasub_dt <- datasub_dt[country %!in% c("Malaysia")]
datasub_dt <- datasub_dt[as.numeric(as.character(year))<2020]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_conflict <- impact(datasub_dt[event=="conflict"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# 01d - Check: drop one country at a time ----

list_of_countries <- unique(datacomb_dt$country)

lst <- list()

for(i in 1:length(list_of_countries)){
  
  ## combined effect ----
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=area_spam,seas=rice_m)]#harvest_season
  
  datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
  
  datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
  
  datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
  ## effect
  coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)
  
  ## impact
  c_comb <- impact(datasub_dt)
  
  ## event-specific effects ----
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=area_spam,seas=rice_m)]#harvest_season
  
  datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
  
  datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
  
  datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
  ## effect
  coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
  coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
  coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
  coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
  
  ## impact
  c_conflict <- impact(datasub_dt[event=="conflict"])
  c_violence <- impact(datasub_dt[event=="violence"])
  c_riots <- impact(datasub_dt[event=="riots"])
  c_protests <- impact(datasub_dt[event=="protests"])
  
  dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)
  
  dt_cn <- colnames(dt)
  
  dt <- as.data.table(t(dt))
  
  colnames(dt) <- c("est","se")
  dt$event <- dt_cn
  
  dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])
  
  dt$country <- list_of_countries[i]
  
  lst[[i]] <- dt
  
}

dropone_dt <- Reduce(rbind,lst)

dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event))

gg_dropone <- ggplot(dropone_dt,aes(x=country,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,color=dropone_dt$col)+
  facet_grid(.~event)+
  coord_flip()+
  labs(title="",x="",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.x=element_text(size=7),axis.text.y=element_text(hjust=0,size=9),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/dropacountry.png",gg_dropone,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/dropacountry.eps",gg_dropone,width=6.5,height=4.5,dpi="retina",device="eps")


# 02 - Prices ----

igc_dt <- fread("Local/Data/IGC/Prices_Rice.csv")

igc_dt$Date <- as.Date(igc_dt$Date,format="%d/%m/%Y")

igc_dt[,`:=`(year=substr(Date,1,4),mo=substr(Date,6,7))]

igc_dt <- igc_dt[,.(India=mean(India,na.rm=T),Pakistan=mean(Pakistan,na.rm=T),Thailand=mean(Thailand,na.rm=T),USA=mean(USA,na.rm=T),Vietnam=mean(Vietnam,na.rm=T),Maize=mean(Maize,na.rm=T)),by=.(year,mo)]

igc_dt[,`:=`(Thailand=na.approx(Thailand),USA=na.approx(USA),Vietnam=na.approx(Vietnam),Maize=na.approx(Maize))]

price_dt <- igc_dt

price_dt[,`:=`(ln_tha=log(Thailand),ln_usa=log(USA),ln_vnm=log(Vietnam),ln_mai=log(Maize))]
price_dt[,`:=`(lg_tha=shift(ln_tha,12),lg_usa=shift(ln_usa,12),lg_vnm=shift(ln_vnm,12),lg_mai=shift(ln_mai,12))]
price_dt[,`:=`(infl_tha=ln_tha-lg_tha,infl_usa=ln_usa-lg_usa,infl_vnm=ln_vnm-lg_vnm,infl_mai=ln_mai-lg_mai)]

price_dt <- price_dt[year %in% c(2010:2022)]

standardize <- function(x,ln=TRUE){
  if(ln==T){
    x <- log(x)
  }
  z=(x-mean(x))/sd(x)
  return(z)
}

price_dt[,`:=`(price_tha=standardize(Thailand),price_usa=standardize(USA),price_vnm=standardize(Vietnam),price_mai=standardize(Maize),infl_tha=standardize(infl_tha,ln=F),infl_usa=standardize(infl_usa,ln=F),infl_vnm=standardize(infl_vnm,ln=F),infl_mai=standardize(infl_mai,ln=F))]

# price_dt[,`:=`(Date=as.Date(paste0(Year,"-",Month,"-01")))]

datacomb_dt <- merge(datacomb_dt,price_dt,by=c("year","mo"),all.x=T)
dataset_dt <- merge(dataset_dt,price_dt,by=c("year","mo"),all.x=T)

impact1 <- function(x){
  r <- feols(incidents~area:seas + area:seas:price | xy+yearmo, data=x,vcov=~xy)
  r1 <- feols(incidents~area:seas + area:seas:I(price-1) | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
  h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
  h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
  h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
  
  p_coef <- round(r1$coeftable["area:seas","Estimate"]*s,1)
  p_se <- round(r1$coeftable["area:seas","Std. Error"]*s,1)
  p_stars <- pstars(r1$coeftable["area:seas","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std,p_est,p_std),output=c(h_coef,h_se,p_coef,p_se)))
}


## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,price=infl_tha)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt,vcov=~xy)

# ## impact
c_comb <- impact1(datasub_dt)


## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,price=infl_tha)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_conflict <- impact1(datasub_dt[event=="conflict"])
c_protests <- impact1(datasub_dt[event=="protests"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_violence <- impact1(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))



# plot impact
dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

price_dt <- dt



# 02a - Vietnam price ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,price=infl_vnm)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt,vcov=~xy)

# ## impact
c_comb <- impact1(datasub_dt)


## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,price=infl_vnm)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_conflict <- impact1(datasub_dt[event=="conflict"])
c_protests <- impact1(datasub_dt[event=="protests"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_violence <- impact1(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# 02b - USA price ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,price=infl_usa)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt,vcov=~xy)

# ## impact
c_comb <- impact1(datasub_dt)


## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,price=infl_usa)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_conflict <- impact1(datasub_dt[event=="conflict"])
c_protests <- impact1(datasub_dt[event=="protests"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_violence <- impact1(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# 03 - Rainfall ----

load("Local/Data/precipitation_new.RData")

rain_dt <- rain_dt[,.(year=as.factor(year),mo=as.factor(mo),longitude=x,latitude=y,rain=as.numeric(rain))]

datacomb_dt <- merge(datacomb_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)
dataset_dt <- merge(dataset_dt,rain_dt,by=c("year","mo","longitude","latitude"),all.x=T)


season_dt <- datacomb_dt[,.(longitude,latitude,mo,Rice_plant_mid,season_rice)]
season_dt <- unique(season_dt)

datarain_dt <- merge(rain_dt,season_dt,by=c("longitude","latitude","mo"),all.x=T)
datarain_dt[is.na(rain)]$rain <- 0

subset_dt <- unique(datarain_dt)


# number of months in the growing season
subset_dt[,`:=`(gsm=ifelse(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid))<0,12-(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid))+12),12-(as.numeric(as.character(season_rice))-as.numeric(as.character(Rice_plant_mid)))))]

subset_dt <- subset_dt[order(longitude,latitude,year,mo)]

subset_dt[season_rice==0]$gsm <- 0

# select data on planted months
planted_dt <- subset_dt[Rice_plant_mid == 1]
planted_dt$myr <- planted_dt$year

planted_dt <- planted_dt[,.(year,myr,longitude,latitude,Rice_plant_mid,season_rice,gsm)]

# merge the weather data with the growing season data
submerge_dt <- merge(subset_dt,planted_dt,by=c("year","longitude","latitude","Rice_plant_mid","season_rice","gsm"),all.x=T)
submerge_dt <- submerge_dt[order(longitude,latitude,year,mo)]

# fill in the NAs
submerge_dt$myr <- as.numeric(as.character(submerge_dt$myr))
submerge_dt[,myr := nafill(myr,type="locf"),by=.(longitude,latitude)]
submerge_dt[,myr := nafill(myr,type="nocb"),by=.(longitude,latitude)]

# so some other stuff (no longer necessary but may as well keep it around)
submerge_dt$backward <- 12-as.numeric(as.character(submerge_dt$season))+1
submerge_dt$dif <- submerge_dt$gsm-submerge_dt$backward

subseason_dt <- submerge_dt[dif >= 0]

subseason_dt <- subseason_dt[,.(gsrain=sum(rain)),by=.(longitude,latitude,myr)]

subseason_dt <- merge(submerge_dt,subseason_dt,by=c("longitude","latitude","myr"),all.x=T)

subseason_dt <- subseason_dt[year%in%2010:2022]
subseason_dt$myr <- NULL
subseason_dt$gsm <- NULL
subseason_dt$backward <- NULL
subseason_dt$dif <- NULL

datacomb2_dt <- merge(datacomb_dt,subseason_dt,by=c("longitude","latitude","year","mo","Rice_plant_mid","season_rice","rain"),all.x=T)
datacomb2_dt[is.na(gsrain)]$gsrain <- 0

datacomb2_dt[,`:=`(gsrain_stand=standardize(gsrain,ln=F)),by=.(xy)]


dataset2_dt <- merge(dataset_dt,subseason_dt,by=c("longitude","latitude","year","mo","Rice_plant_mid","season_rice","rain"),all.x=T)
dataset2_dt[is.na(gsrain)]$gsrain <- 0

dataset2_dt[,`:=`(gsrain_stand=standardize(gsrain,ln=F)),by=.(xy,event)]


impact2 <- function(x){
  r <- feols(incidents~area:seas + area:seas:gsrain_stand | xy+yearmo, data=x,vcov=~xy)
  r1 <- feols(incidents~area:seas + area:seas:I(gsrain_stand-1) | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
  h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
  h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
  h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
  
  p_coef <- round(r1$coeftable["area:seas","Estimate"]*s,1)
  p_se <- round(r1$coeftable["area:seas","Std. Error"]*s,1)
  p_stars <- pstars(r1$coeftable["area:seas","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
  p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std,p_est,p_std),output=c(h_coef,h_se,p_coef,p_se)))
}


## combined effect ----

datasub_dt <- datacomb2_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset2_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_conflict <- impact2(datasub_dt[event=="conflict"])
c_protests <- impact2(datasub_dt[event=="protests"])
c_riots <- impact2(datasub_dt[event=="riots"])
c_violence <- impact2(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

rain_dt <- dt


# 03a - Check: drop one country at a time ----

list_of_countries <- unique(datacomb_dt$country)

lst <- list()

for(i in 1:length(list_of_countries)){
  
  datasub_dt <- datacomb2_dt
  datasub_dt[,`:=`(area=area_spam,seas=rice_m)]
  
  datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
  
  datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
  
  datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
  ## effect
  coef0_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)
  
  ## impact
  c_comb <- impact2(datasub_dt)
  
  
  ## evens-specific effects ----
  
  datasub_dt <- dataset2_dt
  datasub_dt[,`:=`(area=area_spam,seas=rice_m)]
  
  datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
  
  datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
  
  datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
  ## effect
  coef1_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
  coef2_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
  coef3_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
  coef4_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
  
  
  ## impact
  c_conflict <- impact2(datasub_dt[event=="conflict"])
  c_protests <- impact2(datasub_dt[event=="protests"])
  c_riots <- impact2(datasub_dt[event=="riots"])
  c_violence <- impact2(datasub_dt[event=="violence"])
  
  dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)
  
  dt_cn <- colnames(dt)
  
  dt <- as.data.table(t(dt))
  dt <- rbind(dt[,.(est=V1,se=V2)],dt[,.(est=V3,se=V4)])
  
  dt$regime <- rep(c("normal","extreme"),each=5)
  
  dt$event <- rep(dt_cn,2)
  
  dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])
  
  dt$country <- list_of_countries[i]
  
  lst[[i]] <- dt
  
}

dropone_dt <- Reduce(rbind,lst)

dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event))

gg_dropone <- ggplot(dropone_dt,aes(x=country,y=est,linetype=regime))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,color=dropone_dt$col)+
  facet_grid(.~event)+
  coord_flip()+
  labs(title="",x="",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.x=element_text(size=7),axis.text.y=element_text(hjust=0,size=9),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/dropacountry_rain.png",gg_dropone,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/dropacountry_rain.eps",gg_dropone,width=6.5,height=4.5,dpi="retina",device="eps")



# 04 - Prices/irrigation ----

impact5 <- function(x){
  r1 <- feols(incidents~area:seas + area:seas:irri+(area:seas + area:seas:irri):price | xy+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidents~area:seas + area:seas:irri+(area:seas + area:seas:irri):I(price-1) | xy+yearmo, data=x,vcov=~xy)
  r3 <- feols(incidents~area:seas + area:seas:I(1-irri)+(area:seas + area:seas:I(1-irri)):price | xy+yearmo, data=x,vcov=~xy)
  r4 <- feols(incidents~area:seas + area:seas:I(1-irri)+(area:seas + area:seas:I(1-irri)):I(price-1) | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
  h1_coef <- round(r1$coeftable["area:seas","Estimate"]*s,1)
  h1_se <- round(r1$coeftable["area:seas","Std. Error"]*s,1)
  h1_stars <- pstars(r1$coeftable["area:seas","Pr(>|t|)"])
  
  h2_coef <- round(r2$coeftable["area:seas","Estimate"]*s,1)
  h2_se <- round(r2$coeftable["area:seas","Std. Error"]*s,1)
  h2_stars <- pstars(r2$coeftable["area:seas","Pr(>|t|)"])
  
  h3_coef <- round(r3$coeftable["area:seas","Estimate"]*s,1)
  h3_se <- round(r3$coeftable["area:seas","Std. Error"]*s,1)
  h3_stars <- pstars(r3$coeftable["area:seas","Pr(>|t|)"])
  
  h4_coef <- round(r4$coeftable["area:seas","Estimate"]*s,1)
  h4_se <- round(r4$coeftable["area:seas","Std. Error"]*s,1)
  h4_stars <- pstars(r4$coeftable["area:seas","Pr(>|t|)"])
  
  h1_est <- paste0(format(round(h1_coef,1),nsmall=1),h1_stars)
  h1_std <- paste0("(",format(round(h1_se,1),nsmall=1),")")
  
  h2_est <- paste0(format(round(h2_coef,1),nsmall=1),h2_stars)
  h2_std <- paste0("(",format(round(h2_se,1),nsmall=1),")")
  
  h3_est <- paste0(format(round(h3_coef,1),nsmall=1),h3_stars)
  h3_std <- paste0("(",format(round(h3_se,1),nsmall=1),")")
  
  h4_est <- paste0(format(round(h4_coef,1),nsmall=1),h4_stars)
  h4_std <- paste0("(",format(round(h4_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h1_est,h1_std,h2_est,h2_std,h3_est,h3_std,h4_est,h4_std),output=c(h1_coef,h1_se,h2_coef,h2_se,h3_coef,h3_se,h4_coef,h4_se)))
}

## combined effect ----

datasub_dt <- datacomb2_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,irri=prop_i,price=infl_tha)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):price | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact5(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset2_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,irri=prop_i,price=infl_tha)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):price | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):price | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):price | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):price | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_conflict <- impact5(datasub_dt[event=="conflict"])
c_protests <- impact5(datasub_dt[event=="protests"])
c_riots <- impact5(datasub_dt[event=="riots"])
c_violence <- impact5(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2","est3","se3","est4","se4")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

irriprice_dt <- dt


# 05 - Rainfall/irrigation ----

impact4 <- function(x){
  r1 <- feols(incidents~area:seas + area:seas:irri+(area:seas + area:seas:irri):gsrain_stand | xy+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidents~area:seas + area:seas:irri+(area:seas + area:seas:irri):I(gsrain_stand-1) | xy+yearmo, data=x,vcov=~xy)
  r3 <- feols(incidents~area:seas + area:seas:I(1-irri)+(area:seas + area:seas:I(1-irri)):gsrain_stand | xy+yearmo, data=x,vcov=~xy)
  r4 <- feols(incidents~area:seas + area:seas:I(1-irri)+(area:seas + area:seas:I(1-irri)):I(gsrain_stand-1) | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
  h1_coef <- round(r1$coeftable["area:seas","Estimate"]*s,1)
  h1_se <- round(r1$coeftable["area:seas","Std. Error"]*s,1)
  h1_stars <- pstars(r1$coeftable["area:seas","Pr(>|t|)"])
  
  h2_coef <- round(r2$coeftable["area:seas","Estimate"]*s,1)
  h2_se <- round(r2$coeftable["area:seas","Std. Error"]*s,1)
  h2_stars <- pstars(r2$coeftable["area:seas","Pr(>|t|)"])
  
  h3_coef <- round(r3$coeftable["area:seas","Estimate"]*s,1)
  h3_se <- round(r3$coeftable["area:seas","Std. Error"]*s,1)
  h3_stars <- pstars(r3$coeftable["area:seas","Pr(>|t|)"])
  
  h4_coef <- round(r4$coeftable["area:seas","Estimate"]*s,1)
  h4_se <- round(r4$coeftable["area:seas","Std. Error"]*s,1)
  h4_stars <- pstars(r4$coeftable["area:seas","Pr(>|t|)"])
  
  h1_est <- paste0(format(round(h1_coef,1),nsmall=1),h1_stars)
  h1_std <- paste0("(",format(round(h1_se,1),nsmall=1),")")
  
  h2_est <- paste0(format(round(h2_coef,1),nsmall=1),h2_stars)
  h2_std <- paste0("(",format(round(h2_se,1),nsmall=1),")")
  
  h3_est <- paste0(format(round(h3_coef,1),nsmall=1),h3_stars)
  h3_std <- paste0("(",format(round(h3_se,1),nsmall=1),")")
  
  h4_est <- paste0(format(round(h4_coef,1),nsmall=1),h4_stars)
  h4_std <- paste0("(",format(round(h4_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h1_est,h1_std,h2_est,h2_std,h3_est,h3_std,h4_est,h4_std),output=c(h1_coef,h1_se,h2_coef,h2_se,h3_coef,h3_se,h4_coef,h4_se)))
}

## combined effect ----

datasub_dt <- datacomb2_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,irri=prop_i)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact4(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset2_dt
datasub_dt[,`:=`(area=area_spam,seas=rice_m,irri=prop_i)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="conflict"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_conflict <- impact4(datasub_dt[event=="conflict"])
c_protests <- impact4(datasub_dt[event=="protests"])
c_riots <- impact4(datasub_dt[event=="riots"])
c_violence <- impact4(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),conflict=c(c_conflict$descriptive,c_conflict$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_conflict$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2","est3","se3","est4","se4")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

irrirain_dt <- dt


# 06 - conditional on battles ----

impact6 <- function(x){
  r1 <- feols(incidents~area:seas+area:seas:conflict+conflict | xy+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidents~area:seas+area:seas:I(conflict-conflict_mean)+I(conflict-conflict_mean) | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area),conflict=mean(conflict))]
  
  s <- 100*m$cropland/m$incidents
  
  h1_coef <- round(r1$coeftable["area:seas","Estimate"]*s,1)
  h1_se <- round(r1$coeftable["area:seas","Std. Error"]*s,1)
  h1_stars <- pstars(r1$coeftable["area:seas","Pr(>|t|)"])
  
  h2_coef <- round(r2$coeftable["area:seas","Estimate"]*s,1)
  h2_se <- round(r2$coeftable["area:seas","Std. Error"]*s,1)
  h2_stars <- pstars(r2$coeftable["area:seas","Pr(>|t|)"])
  
  
  h1_est <- paste0(format(round(h1_coef,1),nsmall=1),h1_stars)
  h1_std <- paste0("(",format(round(h1_se,1),nsmall=1),")")
  
  h2_est <- paste0(format(round(h2_coef,1),nsmall=1),h2_stars)
  h2_std <- paste0("(",format(round(h2_se,1),nsmall=1),")")
  

  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h1_est,h1_std,h2_est,h2_std),output=c(h1_coef,h1_se,h2_coef,h2_se)))
}

## evens-specific effects ----

datasub_dt <- dataset_dt

# datawide_dt <- dcast(datasub_dt[,.(longitude,latitude,xy,country,yearmo,event,incidents)],longitude+latitude+xy+country+yearmo~event)
# 
# datawide_dt[,`:=`(conflict=conflict,date=as.Date(paste0(yearmo,"-01")))]

datawide_dt <- datasub_dt[event=="conflict",.(longitude,latitude,xy,yearmo,conflict=incidents)]

datasub_dt <- merge(datasub_dt,datawide_dt,by=c("longitude","latitude","xy","yearmo"),all.x=T)

datasub_dt[,`:=`(area=area_spam,seas=rice_m)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

datasub_dt[,`:=`(conflict_mean=mean(conflict))]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:conflict+conflict | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:conflict+conflict | xy+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:conflict+conflict | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_protests <- impact6(datasub_dt[event=="protests"])
c_riots <- impact6(datasub_dt[event=="riots"])
c_violence <- impact6(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef1_fe,coef2_fe,coef3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))



# plot impact
dt <- data.table(violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

regime_dt <- dt


save(main_dt,price_dt,rain_dt,irriprice_dt,irrirain_dt,regime_dt,file="estimates.RData")


# commented out stuff ----

# # irrigated/rainfed ----
# 
# impact3 <- function(x){
#   r <- feols(incidents~area:seas + area:seas:irri | xy+yearmo, data=x,vcov=~xy)
#   r1 <- feols(incidents~area:seas + area:seas:I(1-irri) | xy+yearmo, data=x,vcov=~xy)
#   
#   m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
#   
#   s <- 100*m$cropland/m$incidents
#   
#   h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
#   h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
#   h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
#   
#   p_coef <- round(r1$coeftable["area:seas","Estimate"]*s,1)
#   p_se <- round(r1$coeftable["area:seas","Std. Error"]*s,1)
#   p_stars <- pstars(r1$coeftable["area:seas","Pr(>|t|)"])
#   
#   h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
#   h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
#   
#   p_est <- paste0(format(round(p_coef,1),nsmall=1),p_stars)
#   p_std <- paste0("(",format(round(p_se,1),nsmall=1),")")
#   
#   return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std,p_est,p_std)))
# }
# 
# ## combined effect ----
# 
# datasub_dt <- datacomb_dt
# datasub_dt[,`:=`(area=area_spam,seas=rice_m,irri=prop_i)]
# 
# datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
# 
# datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
# 
# datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
# 
# ## effect
# coef0_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt,vcov=~xy)
# 
# ## impact
# c_comb <- impact3(datasub_dt)
# 
# 
# ## evens-specific effects ----
# 
# datasub_dt <- dataset_dt
# datasub_dt[,`:=`(area=area_spam,seas=rice_m,irri=prop_i)]
# 
# datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
# 
# datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
# 
# datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
# 
# ## effect
# coef1_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
# coef2_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
# coef3_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
# coef4_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
# coef5_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
# coef6_fe <- feols(incidents~area:seas+area:seas:irri | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
# 
# 
# ## impact
# c_battles <- impact3(datasub_dt[event=="battles"])
# c_explosion <- impact3(datasub_dt[event=="explosion"])
# c_strategic <- impact3(datasub_dt[event=="strategic"])
# c_violence <- impact3(datasub_dt[event=="violence"])
# c_protests <- impact3(datasub_dt[event=="protests"])
# c_riots <- impact3(datasub_dt[event=="riots"])
# 
# 
# ## estimated effect
# modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)
# 
# ## calculated impact
# kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))


# # population ----
# 
# cities_dt <- fread("Local/Data/Cities/worldcities.csv")
# 
# secities_dt <- cities_dt[country %in% unique(datacomb_dt$country) & population!="NA"]
# 
# secities_dt <- secities_dt[order(-population)]
# 
# secities_dt$latitude <- round(round(secities_dt$lat,2)-.499)+.5
# secities_dt$longitude <- round(round(secities_dt$lng,2)-.499)+.5
# 
# secities_sub1 <- secities_dt[,.(population=sum(population)),by=.(longitude,latitude,country)]
# 
# secities_sub2 <- secities_dt[secities_dt[,.I[population==max(population)],by=.(longitude,latitude,country)]$V1,.(longitude,latitude,country,city=city_ascii,capital,city_population=population)]
# 
# secities_sub <- merge(secities_sub1,secities_sub2,by=c("longitude","latitude","country"))
# 
# secities_sub <- secities_sub[order(-population,-city_population)]
# 
# secities_sub <- secities_dt[,.(population=sum(population)),by=.(longitude,latitude)]

# secities_sub <- secities_sub[capital %in% c("admin","primary")]
#
# secities_sub <- unique(secities_sub)
# 
# secities_sub <- secities_sub[,.(longitude,latitude,population)]
#
# secities_sub <- unique(secities_sub)
# secities_sub <- secities_sub[order(longitude,latitude)]
# secities_sub[,`:=`(xy=paste0(longitude,",",latitude))]
#
#
# dc_sub <- datacomb_dt[,.(longitude,latitude)]
# dc_sub <- unique(dc_sub)
# dc_sub <- dc_sub[order(longitude,latitude)]
# dc_sub[,`:=`(xy=paste0(longitude,",",latitude))]
#
# secities_sub <- secities_sub[xy %in% dc_sub$xy]
#
# dc_merge <- merge(dc_sub,secities_sub[,.(longitude,latitude,population)],by=c("longitude","latitude"),all.x=T)
#
# dc_merge[xy %in% dc_sub$xy]
# 
# datacomb_dt <- merge(datacomb_dt,secities_sub,by=c("longitude","latitude"),all.x=T)
# 
# datacomb_dt[is.na(population)]$population <- 0
# 
# datacomb_dt <- datacomb_dt[complete.cases(datacomb_dt)]
# 
# datacomb_dt[,`:=`(pop_norm=population/max(population))]
# 
# dataset_dt <- merge(dataset_dt,secities_sub,by=c("longitude","latitude"),all.x=T)
# 
# dataset_dt[is.na(population)]$population <- 0
# 
# dataset_dt <- dataset_dt[complete.cases(dataset_dt)]
# 
# dataset_dt[,`:=`(pop_norm=population/max(population))]
# 
# 
# 
# check_dt <- datacomb_dt[,.(Rice_area=mean(Rice_area),population=mean(population),incidents=sum(incidents)),by=.(longitude,latitude,xy)]
# 
# check_dt <- check_dt[order(-population,-Rice_area)]
# 
# # main effect: unbalanced panel ----
# 
# ## combined effect ----
# 
# datasub_dt <- datacomb_dt
# 
# datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
# 
# datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
# 
# datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
# 
# ## effect
# coef0_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt,vcov=~xy)
# 
# ## impact
# c_comb <- impact(datasub_dt)
# 
# 
# ## evens-specific effects ----
# 
# datasub_dt <- dataset_dt
# 
# datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
# 
# datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
# 
# datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
# 
# ## effect
# coef1_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
# coef2_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
# coef3_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
# coef4_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
# coef5_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
# coef6_fe <- feols(incidents~Crop_area:i(season,keep=1)+Crop_area:i(season,keep=1):pop_norm | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
# 
# ## impact
# c_protests <- impact(datasub_dt[event=="protests"])
# c_riots <- impact(datasub_dt[event=="riots"])
# c_violence <- impact(datasub_dt[event=="violence"])
# c_strategic <- impact(datasub_dt[event=="strategic"])
# c_explosion <- impact(datasub_dt[event=="explosion"])
# c_battles <- impact(datasub_dt[event=="battles"])
# 
# 
# ## estimated effect
# modelsummary(list(coef0_fe,coef6_fe,coef5_fe,coef3_fe,coef1_fe,coef2_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")
# 
# ## calculated impact
# kable_styling(kable(data.table(comb=c_comb$effect,battles=c_battles$effect,explosion=c_explosion$effect,violence=c_violence$effect,protests=c_protests$effect,riots=c_riots$effect,strategic=c_strategic$effect)))
# 
# 
# # Check: balanced panel (2016:2021) ----
# 
# ## combined effect ----
# 
# datasub_dt <- datacomb_dt[country %!in% c("Malaysia")]
# datasub_dt <- datasub_dt[as.numeric(as.character(year))>2015]
# 
# ## effect
# coef0_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt,vcov=~xy)
# 
# ## impact
# c_comb <- impact(datasub_dt)
# 
# ## evens-specific effects ----
# 
# datasub_dt <- dataset_dt[country %!in% c("Malaysia")]
# datasub_dt <- datasub_dt[as.numeric(as.character(year))>2015]
# 
# ## effect
# coef1_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
# coef2_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
# coef3_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
# coef4_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
# coef5_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
# coef6_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
# 
# ## impact
# c_protests <- impact(datasub_dt[event=="protests"])
# c_riots <- impact(datasub_dt[event=="riots"])
# c_violence <- impact(datasub_dt[event=="violence"])
# c_strategic <- impact(datasub_dt[event=="strategic"])
# c_explosion <- impact(datasub_dt[event=="explosion"])
# c_battles <- impact(datasub_dt[event=="battles"])
# 
# 
# ## estimated effect
# modelsummary(list(coef0_fe,coef6_fe,coef5_fe,coef3_fe,coef1_fe,coef2_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/balanced_short.docx")
# 
# ## calculated impact
# kable_styling(kable(data.table(comb=c_comb$effect,battles=c_battles$effect,explosion=c_explosion$effect,violence=c_violence$effect,protests=c_protests$effect,riots=c_riots$effect,strategic=c_strategic$effect)))
# 
# 
# # check: balanced panel (2010:2021) ----
# 
# ## combined effect ----
# 
# datasub_dt <- datacomb_dt[country %!in% c("Indonesia","Malaysia","Philippines")]
# 
# ## effect
# coef0_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt,vcov=~xy)
# 
# ## impact
# c_comb <- impact(datasub_dt)
# 
# ## evens-specific effects ----
# 
# datasub_dt <- dataset_dt[country %!in% c("Indonesia","Malaysia","Philippines")]
# 
# ## effect
# coef1_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
# coef2_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
# coef3_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
# coef4_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
# coef5_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
# coef6_fe <- feols(incidents~Crop_area:i(season,keep=1) | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
# 
# ## impact
# c_protests <- impact(datasub_dt[event=="protests"])
# c_riots <- impact(datasub_dt[event=="riots"])
# c_violence <- impact(datasub_dt[event=="violence"])
# c_strategic <- impact(datasub_dt[event=="strategic"])
# c_explosion <- impact(datasub_dt[event=="explosion"])
# c_battles <- impact(datasub_dt[event=="battles"])
# 
# 
# ## estimated effect
# modelsummary(list(coef0_fe,coef6_fe,coef5_fe,coef3_fe,coef1_fe,coef2_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/balanced_narrow.docx")
# 
# ## calculated impact
# kable_styling(kable(data.table(comb=c_comb$effect,battles=c_battles$effect,explosion=c_explosion$effect,violence=c_violence$effect,protests=c_protests$effect,riots=c_riots$effect,strategic=c_strategic$effect)))
# 
# 
# 


