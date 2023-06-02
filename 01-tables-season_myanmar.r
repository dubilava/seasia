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


# 01 - main effect ----

impact1 <- function(x){
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

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef1_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# for plotting
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est","se")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

main_dt <- dt


# 01d - Check: drop one country at a time ----

list_of_countries <- unique(datacomb_dt$country)

lst <- list()

for(i in 1:length(list_of_countries)){
  
  ## combined effect ----
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
  datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
  ## impact
  c_comb <- impact1(datasub_dt)
  
  ## event-specific effects ----
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
  datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
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
  
  dt$country <- list_of_countries[i]
  
  lst[[i]] <- dt
  
}

# combine the list elements into a data table
dropone_dt <- Reduce(rbind,lst)
dropone_dt <- dropone_dt[order(country)]

dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event))

dropone_dt$country <- factor(dropone_dt$country,levels=unique(dropone_dt$country)[length(unique(dropone_dt$country)):1])

gg_dropone <- ggplot(dropone_dt,aes(x=country,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,shape=21,color=dropone_dt$col,fill="white",stroke=.8)+
  facet_grid(.~event)+
  coord_flip()+
  labs(title="",x="Omitted country",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))


ggsave("Figures/dropacountry_myanmar.png",gg_dropone,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/dropacountry_myanmar.eps",gg_dropone,width=6.5,height=4.5,dpi="retina",device="eps")



# 01d - Check: drop one country at a time ----

list_of_years <- unique(datacomb_dt$year)

lst <- list()

for(i in 1:length(list_of_years)){
  
  ## combined effect ----
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
  datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]
  
  datasub_dt <- datasub_dt[year!=list_of_years[i]]
  
  ## impact
  c_comb <- impact1(datasub_dt)
  
  ## event-specific effects ----
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
  datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]
  
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

dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event))

dropone_dt$year <- factor(dropone_dt$year,levels=unique(dropone_dt$year)[length(unique(dropone_dt$year)):1])

gg_dropone <- ggplot(dropone_dt,aes(x=year,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
  geom_point(size=1.5,shape=21,color=dropone_dt$col,fill="white",stroke=.8)+
  facet_grid(.~event)+
  coord_flip()+
  labs(title="",x="Omitted year",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))


ggsave("Figures/dropayear_myanmar.png",gg_dropone,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/dropayear_myanmar.eps",gg_dropone,width=6.5,height=4.5,dpi="retina",device="eps")


# 02 - Rainfall ----

impact2 <- function(x){
  r <- feols(incidents~area:seas + area:seas:gsrain_stand + gsrain_stand| xy+yearmo, data=x,vcov=~xy)
  r1 <- feols(incidents~area:seas + area:seas:I(gsrain_stand-1) + I(gsrain_stand-1) | xy+yearmo, data=x,vcov=~xy)
  
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
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:gsrain_stand+gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2(datasub_dt)

## event-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:gsrain_stand+gsrain_stand | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:gsrain_stand+gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:gsrain_stand+gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:gsrain_stand+gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_battles <- impact2(datasub_dt[event=="battles"])
c_protests <- impact2(datasub_dt[event=="protests"])
c_riots <- impact2(datasub_dt[event=="riots"])
c_violence <- impact2(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)#,output="Tables/unbalanced.docx")

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

rain_dt <- dt


# 03 - Rainfall/irrigation ----

impact3 <- function(x){
  r1 <- feols(incidents~area:seas + area:seas:irri+(area:seas + area:seas:irri):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidents~area:seas + area:seas:irri+(area:seas + area:seas:irri):I(gsrain_stand-1)+I(gsrain_stand-1)+irri:I(gsrain_stand-1) | xy+yearmo, data=x,vcov=~xy)
  r3 <- feols(incidents~area:seas + area:seas:I(1-irri)+(area:seas + area:seas:I(1-irri)):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, data=x,vcov=~xy)
  r4 <- feols(incidents~area:seas + area:seas:I(1-irri)+(area:seas + area:seas:I(1-irri)):I(gsrain_stand-1)+I(gsrain_stand-1)+irri:I(gsrain_stand-1) | xy+yearmo, data=x,vcov=~xy)
  
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

datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season,irri=prop_i)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season,irri=prop_i)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand+gsrain_stand+irri:gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


## impact
c_battles <- impact3(datasub_dt[event=="battles"])
c_protests <- impact3(datasub_dt[event=="protests"])
c_riots <- impact3(datasub_dt[event=="riots"])
c_violence <- impact3(datasub_dt[event=="violence"])


## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))


# plot impact
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est1","se1","est2","se2","est3","se3","est4","se4")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

irrirain_dt <- dt


# 04 - conditional on battles ----

impact4 <- function(x){
  r1 <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidents~area:seas+area:seas:I(conf-1)+I(conf-1) | xy+yearmo, data=x,vcov=~xy)
  
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

datawide_dt <- datasub_dt[event=="battles",.(longitude,latitude,xy,yearmo,battles=incidents)]

datasub_dt <- merge(datasub_dt,datawide_dt,by=c("longitude","latitude","xy","yearmo"),all.x=T)

datasub_dt[,`:=`(area=area_spam,seas=harvest_season,conf=gsconflict_stand)]

datasub_dt <- datasub_dt[(country!="Myanmar" & as.numeric(as.character(year))!=2021) | (country=="Myanmar" & as.numeric(as.character(year))!=2021) | (country!="Myanmar" & as.numeric(as.character(year))==2021)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_protests <- impact4(datasub_dt[event=="protests"])
c_riots <- impact4(datasub_dt[event=="riots"])
c_violence <- impact4(datasub_dt[event=="violence"])


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






