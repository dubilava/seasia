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

# re-label the contemporaneous rain differently
datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]


## the impact function
impact <- function(x){
  r <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  s <- 100*m$cropland/m$incidents
  h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
  h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
  h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std),output=c(h_coef,h_se)))
}

# B3 - full sample ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef0_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef1_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))



# B4 - without Myanmar 2021-22 ----

## drop Myanmar 2021-22 observations
datacomb_dt <- datacomb_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]
dataset_dt <- dataset_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef0_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef1_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_violence <- impact(datasub_dt[event=="violence"])
c_riots <- impact(datasub_dt[event=="riots"])
c_protests <- impact(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


