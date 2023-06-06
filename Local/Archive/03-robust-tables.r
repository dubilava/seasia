library(data.table)
library(fixest)
library(modelsummary)
library(ggplot2)
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

"%!in%" <- Negate("%in%")

pstars <- function(ps){
  p_stars <- ifelse(ps<.01,"***",ifelse(ps<.05,"**",ifelse(ps<.1,"*","")))
  return(p_stars)
}

impact <- function(x){
  r <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice+q4):i(season_rice,keep=1) | xy+country^year+mo, data=x,vcov=~xy)
  
  m <- x[Rice_dum==1,.(incidents=mean(incidents),Rice_dum=mean(Rice_dum))]
  
  s <- 100*m$Rice_dum/m$incidents
  
  h_coef <- round(r$coeftable["Rice_dum:season_rice::1","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["Rice_dum:season_rice::1","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["Rice_dum:season_rice::1","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,1)),effect=c(h_est,h_std)))
}


f1 <- function(x) format(round(x,3),big.mark=",")
f2 <- function(x) format(round(x,0),big.mark=",")
gm <- list(list("raw"="nobs","clean"="Obs.","fmt"=f2),
           list("raw"="r.squared","clean"="R2","fmt"=f1))


# data management ----

load("Local/Data/data_violence_acled.RData")


## combined ----

### harvest months ----
datacomb_dt[,`:=`(harvest=ifelse(season==1 | season2==1,1,0),harvest_rice=ifelse(season_rice==1 | season2_rice==1,1,0))]

### harvest season (rice, main) ----
datacomb_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

datacomb_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy)]

datacomb_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]

### harvest season (maize, main) ----
datacomb_dt[,`:=`(maize_s=ifelse(Maize_harvest_srt==1,1,0),maize_e=ifelse(Maize_harvest_end==1,1,0))]

datacomb_dt[yearmo==min(yearmo) & as.numeric(as.character(Maize_harvest_srt))>1 & as.numeric(as.character(Maize_harvest_end))>as.numeric(as.character(Maize_harvest_srt))]$maize_s <- 1

datacomb_dt[,`:=`(maize_harvest_season=cumsum(maize_s-maize_e)),by=.(xy)]

datacomb_dt[,`:=`(maize_harvest_season=maize_harvest_season+maize_e)]

### harvest season ----
datacomb_dt[,`:=`(harvest_season=ifelse(Crop=="Rice",rice_harvest_season,maize_harvest_season),rice_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0))]

datacomb_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]



## event-specific ----

### harvest months ----
dataset_dt[,`:=`(harvest=ifelse(season==1 | season2==1,1,0),harvest_rice=ifelse(season_rice==1 | season2_rice==1,1,0))]

### harvest season (rice, main) ----
dataset_dt[,`:=`(rice_s=ifelse(Rice_harvest_srt==1,1,0),rice_e=ifelse(Rice_harvest_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Rice_harvest_srt))>1 & as.numeric(as.character(Rice_harvest_end))>as.numeric(as.character(Rice_harvest_srt))]$rice_s <- 1

dataset_dt[,`:=`(rice_harvest_season=cumsum(rice_s-rice_e)),by=.(xy,event)]

dataset_dt[,`:=`(rice_harvest_season=rice_harvest_season+rice_e)]

### harvest season (maize, main) ----
dataset_dt[,`:=`(maize_s=ifelse(Maize_harvest_srt==1,1,0),maize_e=ifelse(Maize_harvest_end==1,1,0))]

dataset_dt[yearmo==min(yearmo) & as.numeric(as.character(Maize_harvest_srt))>1 & as.numeric(as.character(Maize_harvest_end))>as.numeric(as.character(Maize_harvest_srt))]$maize_s <- 1

dataset_dt[,`:=`(maize_harvest_season=cumsum(maize_s-maize_e)),by=.(xy,event)]

dataset_dt[,`:=`(maize_harvest_season=maize_harvest_season+maize_e)]

### harvest season ----
dataset_dt[,`:=`(harvest_season=ifelse(Crop=="Rice",rice_harvest_season,maize_harvest_season),rice_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0))]

dataset_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]


## finishing touches ----
datacomb_dt[,`:=`(Rice_dum=ifelse(Rice_area>.01,1,0),Area_dum=ifelse(Crop_area>.01,1,0))]
dataset_dt[,`:=`(Rice_dum=ifelse(Rice_area>.01,1,0),Area_dum=ifelse(Crop_area>.01,1,0))]

dataset_dt$event <- factor(dataset_dt$event,levels=c("protests","riots","violence","strategic","explosion","battles"))

# datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Laos","Timor-Leste")]
# dataset_dt <- dataset_dt[country %!in% c("Brunei","Laos","Timor-Leste")]


# main effect: unbalanced panel ----

## combined effect ----

datacomb_dt[,`:=`(Rice_q1=ifelse(Rice_area<quantile(Rice_area,.25),1,0),Rice_q2=ifelse(Rice_area>=quantile(Rice_area,.25) & Rice_area<quantile(Rice_area,.5),1,0),Rice_q3=ifelse(Rice_area>=quantile(Rice_area,.5) & Rice_area<quantile(Rice_area,.75),1,0),Rice_q4=ifelse(Rice_area>=quantile(Rice_area,.75),1,0))]

datasub_dt <- datacomb_dt

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1) | xy+country^year+mo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)


## evens-specific effects ----

dataset_dt[,`:=`(Rice_q1=ifelse(Rice_area<quantile(Rice_area,.25),1,0),Rice_q2=ifelse(Rice_area>=quantile(Rice_area,.25) & Rice_area<quantile(Rice_area,.5),1,0),Rice_q3=ifelse(Rice_area>=quantile(Rice_area,.5) & Rice_area<quantile(Rice_area,.75),1,0),Rice_q4=ifelse(Rice_area>=quantile(Rice_area,.75),1,0))]

datasub_dt <- dataset_dt

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1)| xy+country^year+mo, datasub_dt[event=="protests"],vcov=~xy)
coef2_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1) | xy+country^year+mo, datasub_dt[event=="riots" ],vcov=~xy)
coef3_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1) | xy+country^year+mo, datasub_dt[event=="violence"],vcov=~xy)
coef4_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1) | xy+country^year+mo, datasub_dt[event=="strategic"],vcov=~xy)
coef5_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1) | xy+country^year+mo, datasub_dt[event=="explosion"],vcov=~xy)
coef6_fe <- feols(incidents~(Rice_q1+Rice_q2+Rice_q3+Rice_q4):i(season_rice,keep=1) | xy+country^year+mo, datasub_dt[event=="battles"],vcov=~xy)

## impact
c_protests <- impact(datasub_dt[event=="protests"])
c_riots <- impact(datasub_dt[event=="riots"])
c_violence <- impact(datasub_dt[event=="violence"])
c_strategic <- impact(datasub_dt[event=="strategic"])
c_explosion <- impact(datasub_dt[event=="explosion"])
c_battles <- impact(datasub_dt[event=="battles"])


## estimated effect
modelsummary(list(coef0_fe,coef6_fe,coef5_fe,coef3_fe,coef1_fe,coef2_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c_comb$effect,battles=c_battles$effect,explosion=c_explosion$effect,violence=c_violence$effect,protests=c_protests$effect,riots=c_riots$effect,strategic=c_strategic$effect)))



