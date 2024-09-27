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
load("masterdata05.RData")

## drop Brunei and Timor-Leste
datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Timor-Leste")]
dataset_dt <- dataset_dt[country %!in% c("Brunei","Timor-Leste")]

## create conflict incidence variable
datacomb_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]
dataset_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]

# re-label the contemporaneous rain differently
datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]


# Tab A3: 0.5-degree resolution ----

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

