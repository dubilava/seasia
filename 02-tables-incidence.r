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

datacomb_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]
dataset_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]

# label the contemporanous rain differently
datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]

# 01 - main effect ----

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


# for plotting
dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)

dt_cn <- colnames(dt)

dt <- as.data.table(t(dt))

colnames(dt) <- c("est","se")
dt$event <- dt_cn

dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])

main_dt <- dt



# 01a - Check: balanced panel (2018:2022) ----

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


# 01b - Check: balanced panel (2010:2022) ----

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



# # 01c - Check: drop one country at a time ----
# 
# list_of_countries <- unique(datacomb_dt$country)
# 
# lst <- list()
# 
# for(i in 1:length(list_of_countries)){
#   
#   ## combined effect ----
#   datasub_dt <- datacomb_dt
#   datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]
#   
#   datasub_dt <- datasub_dt[country!=list_of_countries[i]]
#   
#   ## impact
#   c_comb <- impact1(datasub_dt)
#   
#   ## event-specific effects ----
#   datasub_dt <- dataset_dt
#   datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]
#   
#   datasub_dt <- datasub_dt[country!=list_of_countries[i]]
#   
#   ## impact
#   c_battles <- impact1(datasub_dt[event=="battles"])
#   c_violence <- impact1(datasub_dt[event=="violence"])
#   c_riots <- impact1(datasub_dt[event=="riots"])
#   c_protests <- impact1(datasub_dt[event=="protests"])
#   
#   dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)
#   
#   dt_cn <- colnames(dt)
#   
#   dt <- as.data.table(t(dt))
#   
#   colnames(dt) <- c("est","se")
#   dt$event <- dt_cn
#   
#   dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])
#   
#   dt$country <- list_of_countries[i]
#   
#   lst[[i]] <- dt
#   
# }
# 
# # combine the list elements into a data table
# dropone_dt <- Reduce(rbind,lst)
# dropone_dt <- dropone_dt[order(country)]
# 
# dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]
# 
# dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event),labels=c("all events","battles","violence","riots","protests"))
# 
# dropone_dt$country <- factor(dropone_dt$country,levels=unique(dropone_dt$country)[length(unique(dropone_dt$country)):1])
# 
# gg_dropone <- ggplot(dropone_dt,aes(x=country,y=est))+
#   geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
#   geom_point(size=1.5,shape=dropone_dt$pch,color=dropone_dt$col,fill="white",stroke=.8)+
#   scale_y_continuous(breaks=pretty_breaks(n=3))+
#   facet_grid(.~event)+
#   coord_flip()+
#   labs(title="",x="Omitted country",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
#   theme_paper()+
#   theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))
# 
# country_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
# country_dt <- country_dt[order(country)]
# country_dt$country <- factor(country_dt$country,levels=unique(country_dt$country)[length(unique(country_dt$country)):1])
# country_dt[,share:=(incidents/sum(incidents))]
# 
# gg_incidents <- ggplot(country_dt,aes(x=country,y=share))+
#   geom_col(fill="slategray",width=.5)+
#   scale_y_continuous(breaks=pretty_breaks(n=3))+
#   coord_flip()+
#   labs(title="",x="",y="Share of incidents")+
#   theme_paper()+
#   theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_blank())
# 
# gg_comb <- plot_grid(gg_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))
# 
# 
# ggsave("Figures/dropacountry_incidence.png",gg_comb,width=6.5,height=4.5,dpi="retina",device="png")
# 
# ggsave("Figures/dropacountry_incidence.eps",gg_comb,width=6.5,height=4.5,dpi="retina",device="eps")


# # 01d - Check: drop one country at a time ----
# 
# list_of_years <- unique(datacomb_dt$year)
# 
# lst <- list()
# 
# for(i in 1:length(list_of_years)){
#   
#   ## combined effect ----
#   datasub_dt <- datacomb_dt
#   datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]
#   
#   datasub_dt <- datasub_dt[year!=list_of_years[i]]
#   
#   ## impact
#   c_comb <- impact1(datasub_dt)
#   
#   ## event-specific effects ----
#   datasub_dt <- dataset_dt
#   datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]
#   
#   datasub_dt <- datasub_dt[year!=list_of_years[i]]
#   
#   ## impact
#   c_battles <- impact1(datasub_dt[event=="battles"])
#   c_violence <- impact1(datasub_dt[event=="violence"])
#   c_riots <- impact1(datasub_dt[event=="riots"])
#   c_protests <- impact1(datasub_dt[event=="protests"])
#   
#   dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)
#   
#   dt_cn <- colnames(dt)
#   
#   dt <- as.data.table(t(dt))
#   
#   colnames(dt) <- c("est","se")
#   dt$event <- dt_cn
#   
#   dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])
#   
#   dt$year <- list_of_years[i]
#   
#   lst[[i]] <- dt
#   
# }
# 
# # combine the list elements into a data table
# dropone_dt <- Reduce(rbind,lst)
# dropone_dt <- dropone_dt[order(year)]
# 
# dropone_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]
# 
# dropone_dt$event <- factor(dropone_dt$event,levels=unique(dropone_dt$event),labels=c("all events","battles","violence","riots","protests"))
# 
# dropone_dt$year <- factor(dropone_dt$year,levels=unique(dropone_dt$year)[length(unique(dropone_dt$year)):1])
# 
# gg_dropone <- ggplot(dropone_dt,aes(x=year,y=est))+
#   geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.5,width=NA,color=dropone_dt$col)+
#   geom_point(size=1.5,shape=dropone_dt$pch,color=dropone_dt$col,fill="white",stroke=.8)+
#   scale_y_continuous(breaks=pretty_breaks(n=3))+
#   facet_grid(.~event)+
#   coord_flip()+
#   labs(title="",x="Omitted year",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
#   theme_paper()+
#   theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))
# 
# 
# year_dt <- datacomb_dt[,.(incidents=sum(incidents)),by=.(year)]
# year_dt <- year_dt[order(year)]
# year_dt$year <- factor(year_dt$year,levels=unique(year_dt$year)[length(unique(year_dt$year)):1])
# year_dt[,share:=(incidents/sum(incidents))]
# 
# gg_incidents <- ggplot(year_dt,aes(x=year,y=share))+
#   geom_col(fill="slategray",width=.5)+
#   scale_y_continuous(breaks=pretty_breaks(n=3))+
#   coord_flip()+
#   labs(title="",x="",y="Share of incidents")+
#   theme_paper()+
#   theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_blank())
# 
# gg_comb <- plot_grid(gg_dropone,gg_incidents,align = "hv",axis="tb",ncol=2,rel_widths=c(10,3))
# 
# 
# ggsave("Figures/dropayear_incidence.png",gg_comb,width=6.5,height=4.5,dpi="retina",device="png")
# 
# ggsave("Figures/dropayear_incidence.eps",gg_comb,width=6.5,height=4.5,dpi="retina",device="eps")


# # 01f - Check: randomize harvest seasons ----
# 
# list_of_iter <- 1:100
# 
# lst <- list()
# 
# for(i in 1:length(list_of_iter)){
# 
#   ## combined effect ----
#   datasub_dt <- datacomb_dt
# 
#   random_dt <- unique(datasub_dt[,.(xy,mo,harvest_season)])
#   random_dt[,`:=`(id=as.numeric(factor(xy)))]
#   sub_dt <- unique(random_dt[,.(id,xy)])
#   set.seed(i)
#   sub_dt[,`:=`(xy=sample(sub_dt$xy))]
#   random_dt$xy <- NULL
#   random_dt <- merge(random_dt,sub_dt,by="id",all.x=T)
#   random_dt$id <- NULL
# 
#   datasub_dt$harvest_season <- NULL
#   datasub_dt <- merge(datasub_dt,random_dt,by=c("xy","mo"),all.x=T)
# 
#   datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]
# 
#   ## impact
#   c_comb <- impact1(datasub_dt)
# 
#   ## event-specific effects ----
#   datasub_dt <- dataset_dt
# 
#   random_dt <- unique(datasub_dt[,.(xy,mo,harvest_season)])
#   random_dt[,`:=`(id=as.numeric(factor(xy)))]
#   sub_dt <- unique(random_dt[,.(id,xy)])
#   set.seed(i)
#   xy_i <- sample(sub_dt$xy)
#   sub_dt[,`:=`(xy=xy_i)]
#   random_dt$xy <- NULL
#   random_dt <- merge(random_dt,sub_dt,by="id",all.x=T)
#   random_dt$id <- NULL
# 
#   datasub_dt$harvest_season <- NULL
#   datasub_dt <- merge(datasub_dt,random_dt,by=c("xy","mo"),all.x=T)
# 
#   datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]
# 
#   ## impact
#   c_battles <- impact1(datasub_dt[event=="battles"])
#   c_violence <- impact1(datasub_dt[event=="violence"])
#   c_riots <- impact1(datasub_dt[event=="riots"])
#   c_protests <- impact1(datasub_dt[event=="protests"])
# 
#   dt <- data.table(combined=c_comb$output,battles=c_battles$output,violence=c_violence$output,riots=c_riots$output,protests=c_protests$output)
# 
#   dt_cn <- colnames(dt)
# 
#   dt <- as.data.table(t(dt))
# 
#   colnames(dt) <- c("est","se")
#   dt$event <- dt_cn
# 
#   dt$event <- factor(dt$event,levels=dt_cn[length(dt_cn):1])
# 
#   dt$iter <- list_of_iter[i]
# 
#   lst[[i]] <- dt
# 
#   print(i)
# 
# }
# 
# shuffle_dt <- Reduce(rbind,lst)
# 
# shuffle_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(est/se) > 1.96,16,21))]
# 
# shuffle_dt$event <- factor(shuffle_dt$event,levels=unique(shuffle_dt$event),labels=c("all events","battles","violence","riots","protests"))
# 
# shuffle_dt$iter <- factor(shuffle_dt$iter,levels=unique(shuffle_dt$iter)[length(unique(shuffle_dt$iter)):1])
# 
# gg_shuffle <- ggplot(shuffle_dt,aes(x=iter,y=est))+
#   geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.3,width=NA,color=shuffle_dt$col)+
#   geom_point(size=0.8,shape=shuffle_dt$pch,color=shuffle_dt$col,fill="white",stroke=.4)+
#   scale_x_discrete(breaks=seq(5,100,by=5))+
#   facet_grid(.~event)+
#   coord_flip(ylim=c(-30,30))+
#   labs(title="",x="Iteration",y="Harvest-time change in conflict incidence relative to the baseline (%)")+
#   theme_paper()+
#   theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))
# 
# gg_den <- ggplot(shuffle_dt,aes(x=est))+
#   geom_density(adjust=1.3,color="slategray",linewidth=.5)+
#   facet_grid(.~event)+
#   coord_cartesian(xlim=c(-30,30))+
#   labs(title="",x="",y="Density")+
#   theme_paper()+
#   theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))
# 
# gg_comb <- plot_grid(gg_shuffle,gg_den,ncol=1,align="hv",axis="tblr",rel_heights=c(4,1))
# 
# ggsave("Figures/shuffleharvest_incidence.png",gg_comb,width=6.5,height=7.0,dpi="retina",device="png")
# 
# ggsave("Figures/shuffleharvest_incidence.eps",gg_comb,width=6.5,height=7.0,dpi="retina",device="eps")



# 02 - Rainfall ----

impact2 <- function(x){
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
datasub_dt[,`:=`(gsrain_dev=gsrain-mean(gsrain)),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long)]
datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef0_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2(datasub_dt)

## event-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(gsrain_dev=gsrain-mean(gsrain)),by=.(xy,event)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long)]

datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef1_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


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
  r1 <- feols(incidence~area:seas:rain_cat + area:seas:irri:rain_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  r2 <- feols(incidence~area:seas:rain_cat+area:seas:I(1-irri):rain_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area)),by=.(irri)]
  
  s <- 100*m$cropland/m$incidence
  
  h1_coef <- round(r1$coeftable[,"Estimate"][2:4]*s[1],1)
  h1_se <- round(r1$coeftable[,"Std. Error"][2:4]*s[1],1)
  h1_stars <- pstars(r1$coeftable[,"Pr(>|t|)"][2:4])
  
  h2_coef <- round(r2$coeftable[,"Estimate"][2:4]*s[2],1)
  h2_se <- round(r2$coeftable[,"Std. Error"][2:4]*s[2],1)
  h2_stars <- pstars(r2$coeftable[,"Pr(>|t|)"][2:4])
  
  h1_est <- paste0(format(round(h1_coef,1),nsmall=1),h1_stars)
  h1_std <- paste0("(",format(round(h1_se,1),nsmall=1),")")
  
  h2_est <- paste0(format(round(h2_coef,1),nsmall=1),h2_stars)
  h2_std <- paste0("(",format(round(h2_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h1_est[1],h1_std[1],h1_est[2],h1_std[2],h1_est[3],h1_std[3],h2_est[1],h2_std[1],h2_est[2],h2_std[2],h2_est[3],h2_std[3]),output=c(h1_coef[1],h1_se[1],h1_coef[2],h1_se[2],h1_coef[3],h1_se[3],h2_coef[1],h2_se[1],h2_coef[2],h2_se[2],h2_coef[3],h2_se[3])))
}

## combined effect ----

datasub_dt <- datacomb_dt
datasub_dt[,`:=`(gsrain_dev=gsrain-mean(gsrain)),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long,irri=ifelse(prop_i<.5,0,1))]

## effect
coef0_fe <- feols(incidence~area:seas:rain_cat+area:seas:irri:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(gsrain_dev=gsrain-mean(gsrain)),by=.(xy,event)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long,irri=ifelse(prop_i<.5,0,1))]

## effect
coef1_fe <- feols(incidence~area:seas:rain_cat+area:seas:irri:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:rain_cat+area:seas:irri:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:rain_cat+area:seas:irri:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:seas:rain_cat+area:seas:irri:rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)


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
  r <- feols(incidence~area:seas:conf_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2])))
}

## evens-specific effects ----

datasub_dt <- dataset_dt

datawide_dt <- datasub_dt[event=="battles",.(longitude,latitude,xy,yearmo,battles=incidence)]

datasub_dt <- merge(datasub_dt,datawide_dt,by=c("longitude","latitude","xy","yearmo"),all.x=T)

datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long,conf=gsconflict)]

datasub_dt[,conf_cat:=factor(ifelse(gsconflict==0,"peace","war"))]

## effect
coef1_fe <- feols(incidence~area:seas:rain_cat+area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:rain_cat+area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:rain_cat+area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

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




# 05 - battles/rainfall ----

impact5 <- function(x){
  r <- feols(incidence~area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  h_coef <- round(r$coeftable[,"Estimate"][-1]*s,1)
  h_se <- round(r$coeftable[,"Std. Error"][-1]*s,1)
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][-1])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3],h_est[4],h_std[4],h_est[5],h_std[5],h_est[6],h_std[6]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3],h_coef[4],h_se[4],h_coef[5],h_se[5],h_coef[6],h_se[6])))
}


## evens-specific effects ----

datasub_dt <- dataset_dt

datawide_dt <- datasub_dt[event=="battles",.(longitude,latitude,xy,yearmo,battles=incidence)]

datasub_dt <- merge(datasub_dt,datawide_dt,by=c("longitude","latitude","xy","yearmo"),all.x=T)

datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,conf_cat=ifelse(gsconflict>0,"war","peace"))]

## effect
coef1_fe <- feols(incidence~area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef2_fe <- feols(incidence~area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef3_fe <- feols(incidence~area:seas:rain_cat:conf_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_protests <- impact5(datasub_dt[event=="protests"])
c_riots <- impact5(datasub_dt[event=="riots"])
c_violence <- impact5(datasub_dt[event=="violence"])


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




# 01 - main effect ----

impact1 <- function(x){
  r <- feols(incidence~area:(l2+l1+s0+f1+f2+f3)+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  h_coef <- round(r$coeftable[,"Estimate"][2:7]*s,1)
  
  h_se <- round(r$coeftable[,"Std. Error"][2:7]*s,1)
  
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][2:7])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3],h_est[4],h_std[4],h_est[5],h_std[5],h_est[6],h_std[6]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3],h_coef[4],h_se[4],h_coef[5],h_se[5],h_coef[6],h_se[6])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead"),f3=shift(harvest_month,3,type="lead")),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef0_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3)+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead"),f3=shift(harvest_month,3,type="lead")),by=.(xy,event)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect
coef1_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3)+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3)+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3)+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3)+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))




impact2 <- function(x){
  r <- feols(incidence~area:(l2+l1+s0+f1+f2+f3):rain_cat+rain_t | xy+country^year+yearmo, data=x,vcov=~xy)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  h_coef <- round(r$coeftable[,"Estimate"][2:7]*s,1)
  
  h_se <- round(r$coeftable[,"Std. Error"][2:7]*s,1)
  
  h_stars <- pstars(r$coeftable[,"Pr(>|t|)"][2:7])
  
  h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
  h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
  
  return(list(descriptive=c(incidence=round(m$incidence,2),cropland=round(m$cropland,2)),effect=c(h_est[1],h_std[1],h_est[2],h_std[2],h_est[3],h_std[3],h_est[4],h_std[4],h_est[5],h_std[5],h_est[6],h_std[6]),output=c(h_coef[1],h_se[1],h_coef[2],h_se[2],h_coef[3],h_se[3],h_coef[4],h_se[4],h_coef[5],h_se[5],h_coef[6],h_se[6])))
}

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead"),f3=shift(harvest_month,3,type="lead")),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long)]
datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef0_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(l2=shift(harvest_month,2),l1=shift(harvest_month,1),s0=harvest_month,f1=shift(harvest_month,1,type="lead"),f2=shift(harvest_month,2,type="lead"),f3=shift(harvest_month,3,type="lead")),by=.(xy)]
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season,rain=gsrain_stand_long)]
datasub_dt[,rain_cat:=factor(ifelse(rain<=-1,"dry",ifelse(rain>=1,"wet","normal")))]

## effect
coef1_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidence~area:(l2+l1+s0+f1+f2+f3):rain_cat+rain_t | xy+country^year+yearmo, datasub_dt[event=="protests"],vcov=~xy)

## impact
c_battles <- impact1(datasub_dt[event=="battles"])
c_violence <- impact1(datasub_dt[event=="violence"])
c_riots <- impact1(datasub_dt[event=="riots"])
c_protests <- impact1(datasub_dt[event=="protests"])

## estimated effect
modelsummary(list(coef0_fe,coef1_fe,coef2_fe,coef3_fe,coef4_fe),estimate="{estimate}{stars}",stars=c('*'=.1,'**'=.05,'***'=.01),gof_map=gm)

## calculated impact
kable_styling(kable(data.table(comb=c(c_comb$descriptive,c_comb$effect),battles=c(c_battles$descriptive,c_battles$effect),violence=c(c_violence$descriptive,c_violence$effect),riots=c(c_riots$descriptive,c_riots$effect),protests=c(c_protests$descriptive,c_protests$effect))))





save(main_dt,rain_dt,irrirain_dt,regime_dt,file="results_incidence.RData")



