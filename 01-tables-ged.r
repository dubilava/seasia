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
  r <- feols(incidents~area:seas | xy+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents

  h_coef <- round(r$coeftable["area:seas","Estimate"]*s,1)
  
  h_se <- round(r$coeftable["area:seas","Std. Error"]*s,1)
  
  h_stars <- pstars(r$coeftable["area:seas","Pr(>|t|)"])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std)))
}


f1 <- function(x) format(round(x,3),big.mark=",")
f2 <- function(x) format(round(x,0),big.mark=",")
gm <- list(list("raw"="nobs","clean"="Obs.","fmt"=f2),
           list("raw"="r.squared","clean"="R2","fmt"=f1))


# data management ----

load("Local/Data/data_violence_ged.RData")
load("Local/Data/spam.RData")

colnames(spam_dt)[1:2] <- c("longitude","latitude")

spam_dt[,`:=`(area_spam=area_spam/100000,area_i=area_i/100000,area_r=area_r/100000,area_h=area_h/100000,area_l=area_l/100000,area_s=area_s/100000)]

datacomb_dt <- merge(datacomb_dt,spam_dt,by=c("longitude","latitude"),all.x=T)

dataset_dt <- merge(dataset_dt,spam_dt,by=c("longitude","latitude"),all.x=T)


datacomb_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]
dataset_dt[,`:=`(prop_i=ifelse(area_i==0,0,area_i/area_spam))]

# check_dt <- datacomb_dt[year==2020 & month=="Jan"]
# 
# ggplot(check_dt,aes(x=prop_i))+
#   geom_histogram()

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
dataset_dt[,`:=`(harvest_season=ifelse(Crop_Rice=="Rice",rice_harvest_season,0),growing_season=ifelse(Crop_Rice=="Rice",rice_growing_season,0))]

dataset_dt[,`:=`(harvest_season=ifelse(harvest_season==2,1,harvest_season))]


## finishing touches ----
datacomb_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]
dataset_dt[,`:=`(area_hi=area_i+area_h,area_lo=area_l+area_s)]

datacomb_dt[,`:=`(Rice_dum=ifelse(Rice_area>.01,1,0),Area_dum=ifelse(Crop_area>.01,1,0))]
dataset_dt[,`:=`(Rice_dum=ifelse(Rice_area>.01,1,0),Area_dum=ifelse(Crop_area>.01,1,0))]

dataset_dt$event <- factor(dataset_dt$event,levels=c("state","nonstate","onesided"))

datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Laos","Timor-Leste","Vietnam")]
dataset_dt <- dataset_dt[country %!in% c("Brunei","Laos","Timor-Leste","Vietnam")]




# main effect: unbalanced panel ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

# datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
# 
# datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
# 
# datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

# datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
# 
# datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
# 
# datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="state"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="nonstate"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="onesided"],vcov=~xy)


## impact
c_state <- impact(datasub_dt[event=="state"])
c_nonstate <- impact(datasub_dt[event=="nonstate"])
c_onesided <- impact(datasub_dt[event=="onesided"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),state=c(c_state$descriptive,c_state$effect),nonstate=c(c_nonstate$descriptive,c_nonstate$effect),onesided=c(c_onesided$descriptive,c_onesided$effect))))


# Check: balanced panel (2016:2021) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Malaysia")]
datasub_dt <- datasub_dt[as.numeric(as.character(year))>2015]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Malaysia")]
datasub_dt <- datasub_dt[as.numeric(as.character(year))>2015]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef5_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
coef6_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)


## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_explosion <- impact(datasub_dt[event=="explosion"])
c_strategic <- impact(datasub_dt[event=="strategic"])
c_violence <- impact(datasub_dt[event=="violence"])
c_protests <- impact(datasub_dt[event=="protests"])
c_riots <- impact(datasub_dt[event=="riots"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))


# check: balanced panel (2010:2021) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef5_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
coef6_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)


## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_explosion <- impact(datasub_dt[event=="explosion"])
c_strategic <- impact(datasub_dt[event=="strategic"])
c_violence <- impact(datasub_dt[event=="violence"])
c_protests <- impact(datasub_dt[event=="protests"])
c_riots <- impact(datasub_dt[event=="riots"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))








# prices ----

igc_dt <- fread("Local/Data/IGC/Prices_Rice.csv")

igc_dt$Date <- as.Date(igc_dt$Date,format="%d/%m/%Y")

igc_dt[,`:=`(year=substr(Date,1,4),mo=substr(Date,6,7))]

igc_dt <- igc_dt[,.(India=mean(India,na.rm=T),Pakistan=mean(Pakistan,na.rm=T),Thailand=mean(Thailand,na.rm=T),USA=mean(USA,na.rm=T),Vietnam=mean(Vietnam,na.rm=T),Maize=mean(Maize,na.rm=T)),by=.(year,mo)]

igc_dt[,`:=`(Thailand=na.approx(Thailand),USA=na.approx(USA),Vietnam=na.approx(Vietnam),Maize=na.approx(Maize))]

price_dt <- igc_dt

price_dt[,`:=`(ln_tha=log(Thailand),ln_usa=log(USA),ln_vnm=log(Vietnam),ln_mai=log(Maize))]
price_dt[,`:=`(lg_tha=shift(ln_tha,12),lg_usa=shift(ln_usa,12),lg_vnm=shift(ln_vnm,12),lg_mai=shift(ln_mai,12))]
price_dt[,`:=`(infl_tha=ln_tha-lg_tha,infl_usa=ln_usa-lg_usa,infl_vnm=ln_vnm-lg_vnm,infl_mai=ln_mai-lg_mai)]

price_dt <- price_dt[year %in% c(2010:2021)]

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
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std,p_est,p_std)))
}


## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season,price=infl_tha)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt,vcov=~xy)

# ## impact
c_comb <- impact1(datasub_dt)


## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season,price=infl_tha)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
coef4_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef5_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
coef6_fe <- feols(incidents~area:seas + area:seas:price | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_explosion <- impact1(datasub_dt[event=="explosion"])
c_strategic <- impact1(datasub_dt[event=="strategic"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_protests <- impact1(datasub_dt[event=="protests"])
c_riots <- impact1(datasub_dt[event=="riots"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))




# rainfall ----

load("Local/Data/precipitation.RData")

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

# these are the data on rainfall, temperature and extreme degree days
# during the growing season of the major crop in a given cell; the weather variables
# are kept constant for the duration of one calendar year beginning from the
# planting month; this means that for given cell, in a given year, the 
# pre-harvest and post-harvest periods are 'treated' with the same
# growing season weather; this makes the interpretation more straightforward,
# say, when we want to argue that there is some increase in violence
# just before the harvest due to bad weather, or something of that nature.
subseason_dt <- subseason_dt[year%in%2010:2021]
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
  
  return(list(descriptive=c(incidents=round(m$incidents,2),cropland=round(m$cropland,2)),effect=c(h_est,h_std,p_est,p_std)))
}

## combined effect ----

datasub_dt <- datacomb2_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset2_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef5_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
coef6_fe <- feols(incidents~area:seas+area:seas:gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)


## impact
c_battles <- impact2(datasub_dt[event=="battles"])
c_explosion <- impact2(datasub_dt[event=="explosion"])
c_strategic <- impact2(datasub_dt[event=="strategic"])
c_violence <- impact2(datasub_dt[event=="violence"])
c_protests <- impact2(datasub_dt[event=="protests"])
c_riots <- impact2(datasub_dt[event=="riots"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))


# irrigated/rainfed ----

## combined effect ----

datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_i,seas=harvest_season)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_i,seas=harvest_season)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef5_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
coef6_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)


## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_explosion <- impact(datasub_dt[event=="explosion"])
c_strategic <- impact(datasub_dt[event=="strategic"])
c_violence <- impact(datasub_dt[event=="violence"])
c_protests <- impact(datasub_dt[event=="protests"])
c_riots <- impact(datasub_dt[event=="riots"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))



## combined effect ----

datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_r,seas=harvest_season)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_r,seas=harvest_season)]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="explosion"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="strategic"],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef5_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)
coef6_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)


## impact
c_battles <- impact(datasub_dt[event=="battles"])
c_explosion <- impact(datasub_dt[event=="explosion"])
c_strategic <- impact(datasub_dt[event=="strategic"])
c_violence <- impact(datasub_dt[event=="violence"])
c_protests <- impact(datasub_dt[event=="protests"])
c_riots <- impact(datasub_dt[event=="riots"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe,coef5_fe,coef6_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),explosion=c(c_explosion$descriptive,c_explosion$effect),strategic=c(c_strategic$descriptive,c_strategic$effect),violence=c(c_violence$descriptive,c_violence$effect),protests=c(c_protests$descriptive,c_protests$effect),riots=c(c_riots$descriptive,c_riots$effect))))



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


