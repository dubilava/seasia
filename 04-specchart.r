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

datacomb_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]
dataset_dt[,`:=`(incidence=ifelse(incidents>0,1,0))]

datacomb_dt[,rain_t:=rain]
dataset_dt[,rain_t:=rain]
                                         

impact1i <- function(x,n){
  r <- feols(incidence~area:seas+rain_t | sw(xy+country^year+yearmo,xy+country^year+mo,xy+yearmo,xy+year+mo), data=x,vcov=~xy)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  lst <- list()
  
  for(i in 1:n){
    
    h_coef <- round(r[[i]]$coeftable["area:seas","Estimate"]*s,1)
    
    h_se <- round(r[[i]]$coeftable["area:seas","Std. Error"]*s,1)
    
    h_stars <- pstars(r[[i]]$coeftable["area:seas","Pr(>|t|)"])
    
    h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
    h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
    
    lst[[i]] <- c(h_coef,h_se,paste(r[[i]]$fixef_vars,collapse=', '))
    
  }
  
  coef_dt <- as.data.table(Reduce(rbind,lst))
  colnames(coef_dt) <- c("impact","se","fe")
  
  return(coef_dt)
}
impact1c150 <- function(x,n){
  r <- feols(incidence~area:seas+rain_t | sw(xy+country^year+yearmo,xy+country^year+mo,xy+yearmo,xy+year+mo), data=x,vcov=vcov_conley(lat="latitude",lon="longitude",cutoff=150,distance="spherical"))
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  lst <- list()
  
  for(i in 1:n){
    
    h_coef <- round(r[[i]]$coeftable["area:seas","Estimate"]*s,1)
    
    h_se <- round(r[[i]]$coeftable["area:seas","Std. Error"]*s,1)
    
    h_stars <- pstars(r[[i]]$coeftable["area:seas","Pr(>|t|)"])
    
    h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
    h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
    
    lst[[i]] <- c(h_coef,h_se,paste(r[[i]]$fixef_vars,collapse=', '))
    
  }
  
  coef_dt <- as.data.table(Reduce(rbind,lst))
  colnames(coef_dt) <- c("impact","se","fe")
  
  return(coef_dt)
}
impact1c300 <- function(x,n){
  r <- feols(incidence~area:seas+rain_t | sw(xy+country^year+yearmo,xy+country^year+mo,xy+yearmo,xy+year+mo), data=x,vcov=vcov_conley(lat="latitude",lon="longitude",cutoff=300,distance="spherical"))
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  lst <- list()
  
  for(i in 1:n){
    
    h_coef <- round(r[[i]]$coeftable["area:seas","Estimate"]*s,1)
    
    h_se <- round(r[[i]]$coeftable["area:seas","Std. Error"]*s,1)
    
    h_stars <- pstars(r[[i]]$coeftable["area:seas","Pr(>|t|)"])
    
    h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
    h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
    
    lst[[i]] <- c(h_coef,h_se,paste(r[[i]]$fixef_vars,collapse=', '))
    
  }
  
  coef_dt <- as.data.table(Reduce(rbind,lst))
  colnames(coef_dt) <- c("impact","se","fe")
  
  return(coef_dt)
}
impact1c <- function(x,n){
  r <- feols(incidence~area:seas+rain_t | sw(xy+country^year+yearmo,xy+country^year+mo,xy+yearmo,xy+year+mo), data=x,vcov=~country)
  
  m <- x[area>0,.(incidence=mean(incidence),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidence
  
  lst <- list()
  
  for(i in 1:n){
    
    h_coef <- round(r[[i]]$coeftable["area:seas","Estimate"]*s,1)
    
    h_se <- round(r[[i]]$coeftable["area:seas","Std. Error"]*s,1)
    
    h_stars <- pstars(r[[i]]$coeftable["area:seas","Pr(>|t|)"])
    
    h_est <- paste0(format(round(h_coef,1),nsmall=1),h_stars)
    h_std <- paste0("(",format(round(h_se,1),nsmall=1),")")
    
    lst[[i]] <- c(h_coef,h_se,paste(r[[i]]$fixef_vars,collapse=', '))
    
  }
  
  coef_dt <- as.data.table(Reduce(rbind,lst))
  colnames(coef_dt) <- c("impact","se","fe")
  
  return(coef_dt)
}


# Spec: main ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## effect

## impact
c_comb1 <- impact1i(datasub_dt,4)
c_comb1$cluster <- "Cell"

c_comb2 <- impact1i(datasub_dt,4)
c_comb2$cluster <- "Country"

c_comb3 <- impact1c150(datasub_dt,4)
c_comb3$cluster <- "Conley (150km)"

c_comb4 <- impact1c300(datasub_dt,4)
c_comb4$cluster <- "Conley (300km)"

c_comb <- rbind(c_comb1,c_comb2,c_comb3,c_comb4)
c_comb$event <- "combined"

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=ifelse(area_spam<.1,0,1),seas=harvest_season)]

## impact
c_battles1 <- impact1i(datasub_dt[event=="battles"],4)
c_violence1 <- impact1i(datasub_dt[event=="violence"],4)
c_riots1 <- impact1i(datasub_dt[event=="riots"],4)
c_protests1 <- impact1i(datasub_dt[event=="protests"],4)

c_battles1$cluster <- "Cell"
c_violence1$cluster <- "Cell"
c_riots1$cluster <- "Cell"
c_protests1$cluster <- "Cell"


c_battles2 <- impact1c(datasub_dt[event=="battles"],4)
c_violence2 <- impact1c(datasub_dt[event=="violence"],4)
c_riots2 <- impact1c(datasub_dt[event=="riots"],4)
c_protests2 <- impact1c(datasub_dt[event=="protests"],4)

c_battles2$cluster <- "Country"
c_violence2$cluster <- "Country"
c_riots2$cluster <- "Country"
c_protests2$cluster <- "Country"


c_battles3 <- impact1c150(datasub_dt[event=="battles"],4)
c_violence3 <- impact1c150(datasub_dt[event=="violence"],4)
c_riots3 <- impact1c150(datasub_dt[event=="riots"],4)
c_protests3 <- impact1c150(datasub_dt[event=="protests"],4)

c_battles3$cluster <- "Conley (150km)"
c_violence3$cluster <- "Conley (150km)"
c_riots3$cluster <- "Conley (150km)"
c_protests3$cluster <- "Conley (150km)"


c_battles4 <- impact1c300(datasub_dt[event=="battles"],4)
c_violence4 <- impact1c300(datasub_dt[event=="violence"],4)
c_riots4 <- impact1c300(datasub_dt[event=="riots"],4)
c_protests4 <- impact1c300(datasub_dt[event=="protests"],4)

c_battles4$cluster <- "Conley (300km)"
c_violence4$cluster <- "Conley (300km)"
c_riots4$cluster <- "Conley (300km)"
c_protests4$cluster <- "Conley (300km)"


c_battles <- rbind(c_battles1,c_battles2,c_battles3,c_battles4)
c_battles$event <- "battles"

c_violence <- rbind(c_violence1,c_violence2,c_violence3,c_violence4)
c_violence$event <- "violence"

c_riots <- rbind(c_riots1,c_riots2,c_riots3,c_riots4)
c_riots$event <- "riots"

c_protests <- rbind(c_protests1,c_protests2,c_protests3,c_protests4)
c_protests$event <- "protests"


dt <- data.table(rbind(c_comb,c_battles,c_violence,c_riots,c_protests))

dt$impact <- as.numeric(dt$impact)
dt$se <- as.numeric(dt$se)

dt[,`:=`(col=ifelse(impact/se > 1.96,"coral",ifelse(impact/se < -1.96,"steelblue","darkgray")),pch=ifelse(abs(impact/se) > 1.96,16,21))]

dt$event <- factor(dt$event,levels=unique(dt$event),labels=c("all events","battles","violence","riots","protests"))

dt$fe <- factor(dt$fe,levels=unique(dt$fe))

characters <- unlist(strsplit(as.character(dt$fe),", "))

unique_chars <- unique(characters)

mt <- matrix(NA,nrow=length(dt$fe),ncol=length(unique_chars), dimnames=list(NULL,unique_chars))

for(i in 1:length(dt$fe)){
  for(j in 1:length(colnames(mt))){
    if(any(unlist(strsplit(as.character(dt$fe[i]),", "))==colnames(mt)[j]))
      mt[i,j] <- 1
  }
}


mt_dt <- data.table(mt)

dt <- cbind(dt,mt_dt)

mt <- matrix(NA,nrow=length(dt$cluster),ncol=length(unique(dt$cluster)), dimnames=list(NULL,unique(dt$cluster)))

for(i in 1:length(dt$cluster)){
  for(j in 1:length(unique(dt$cluster))){
    if(unique(dt$cluster)[j]==dt$cluster[i])
      mt[i,j] <- 1
  }
}

mt_dt <- data.table(mt)

dt <- cbind(dt,mt_dt)

dt[,group_id:=paste(cluster,fe,sep="/")]

dt <- dt[order(cluster,fe)]
dt$cluster <- factor(dt$cluster,levels=unique(dt$cluster))

gg_est <- ggplot(dt,aes(x=group_id,y=impact,group=group_id))+
  geom_errorbar(aes(ymin=impact-1.96*se,ymax=impact+1.96*se),linewidth=.5,width=NA,color=dt$col)+
  geom_point(size=1.5,shape=dt$pch,color=dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=4))+
  facet_wrap(.~event,ncol=1,strip.position="left")+
  labs(title="Harvest-time change in conflict incidence relative to the baseline (%)",x="",y="")+
  theme_paper()+
  theme(axis.text.x=element_blank(),strip.placement="outside")

spec_dt <- melt(dt[,.(group_id,xy,`country^year`,yearmo,year,mo,Cell,`Conley (150km)`,`Conley (300km)`,Country)],id.vars="group_id")

spec_dt$variable <- factor(spec_dt$variable,levels=rev(unique(spec_dt$variable)))

spec_dt <- spec_dt[order(group_id)]

spec1_dt <- spec_dt[variable %in% c("xy","country^year","yearmo","year","mo")]
spec1_dt$variable <- factor(spec1_dt$variable,levels=unique(spec1_dt$variable),labels=rev(c("Month","Year","Year-Month","Country-Year","Cell")))

spec2_dt <- spec_dt[variable %in% c("Cell","Conley (150km)","Conley (300km)","Country")]
spec2_dt$variable <- factor(spec2_dt$variable,levels=rev(unique(spec2_dt$variable)))

gg_spec1 <- ggplot(spec1_dt,aes(x=group_id,y=variable)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(rev(levels(spec1_dt$variable))) + 
  geom_point(aes(size=value),na.rm=T,shape=15,color="slategray")+
  scale_size_continuous(range=c(0,3))+
  labs(title="Fixed effects",x="",y="")+
  theme_paper()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust=0))

gg_spec2 <- ggplot(spec2_dt,aes(x=group_id,y=variable)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(levels(spec2_dt$variable)) + 
  geom_point(aes(size=value),na.rm=T,shape=15,color="slategray")+
  scale_size_continuous(range=c(0,3))+
  labs(title="Level of clustering",x="",y="")+
  theme_paper()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust=0))

gg_comb <- plot_grid(gg_est,gg_spec1,gg_spec2,ncol=1,align="hv",axis="lr",rel_heights=c(27,7,6))


ggsave("Figures/spec_incidence.png",gg_comb,width=6.5,height=6.5,dpi="retina",device="png")

ggsave("Figures/spec_incidence.eps",gg_comb,width=6.5,height=6.5,dpi="retina",device="eps")







# Spec: myanmar ----

datacomb_dt <- datacomb_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]
dataset_dt <- dataset_dt[(country!="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country=="Myanmar" & as.numeric(as.character(year))%!in%c(2021,2022)) | (country!="Myanmar" & as.numeric(as.character(year))%in%c(2021,2022))]

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

## effect

## impact
c_comb1 <- impact1i(datasub_dt,4)
c_comb1$cluster <- "Cell"

c_comb2 <- impact1i(datasub_dt,4)
c_comb2$cluster <- "Country"

c_comb3 <- impact1c150(datasub_dt,4)
c_comb3$cluster <- "Conley (150km)"

c_comb4 <- impact1c300(datasub_dt,4)
c_comb4$cluster <- "Conley (300km)"

c_comb <- rbind(c_comb1,c_comb2,c_comb3,c_comb4)
c_comb$event <- "combined"

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

## impact
c_battles1 <- impact1i(datasub_dt[event=="battles"],4)
c_violence1 <- impact1i(datasub_dt[event=="violence"],4)
c_riots1 <- impact1i(datasub_dt[event=="riots"],4)
c_protests1 <- impact1i(datasub_dt[event=="protests"],4)

c_battles1$cluster <- "Cell"
c_violence1$cluster <- "Cell"
c_riots1$cluster <- "Cell"
c_protests1$cluster <- "Cell"


c_battles2 <- impact1c(datasub_dt[event=="battles"],4)
c_violence2 <- impact1c(datasub_dt[event=="violence"],4)
c_riots2 <- impact1c(datasub_dt[event=="riots"],4)
c_protests2 <- impact1c(datasub_dt[event=="protests"],4)

c_battles2$cluster <- "Country"
c_violence2$cluster <- "Country"
c_riots2$cluster <- "Country"
c_protests2$cluster <- "Country"


c_battles3 <- impact1c150(datasub_dt[event=="battles"],4)
c_violence3 <- impact1c150(datasub_dt[event=="violence"],4)
c_riots3 <- impact1c150(datasub_dt[event=="riots"],4)
c_protests3 <- impact1c150(datasub_dt[event=="protests"],4)

c_battles3$cluster <- "Conley (150km)"
c_violence3$cluster <- "Conley (150km)"
c_riots3$cluster <- "Conley (150km)"
c_protests3$cluster <- "Conley (150km)"


c_battles4 <- impact1c300(datasub_dt[event=="battles"],4)
c_violence4 <- impact1c300(datasub_dt[event=="violence"],4)
c_riots4 <- impact1c300(datasub_dt[event=="riots"],4)
c_protests4 <- impact1c300(datasub_dt[event=="protests"],4)

c_battles4$cluster <- "Conley (300km)"
c_violence4$cluster <- "Conley (300km)"
c_riots4$cluster <- "Conley (300km)"
c_protests4$cluster <- "Conley (300km)"


c_battles <- rbind(c_battles1,c_battles2,c_battles3,c_battles4)
c_battles$event <- "battles"

c_violence <- rbind(c_violence1,c_violence2,c_violence3,c_violence4)
c_violence$event <- "violence"

c_riots <- rbind(c_riots1,c_riots2,c_riots3,c_riots4)
c_riots$event <- "riots"

c_protests <- rbind(c_protests1,c_protests2,c_protests3,c_protests4)
c_protests$event <- "protests"


dt <- data.table(rbind(c_comb,c_battles,c_violence,c_riots,c_protests))

dt$impact <- as.numeric(dt$impact)
dt$se <- as.numeric(dt$se)

dt[,`:=`(col=ifelse(impact/se > 1.96,"coral",ifelse(impact/se < -1.96,"steelblue","darkgray")))]

dt$event <- factor(dt$event,levels=unique(dt$event))

dt$fe <- factor(dt$fe,levels=unique(dt$fe))

characters <- unlist(strsplit(as.character(dt$fe),", "))

unique_chars <- unique(characters)

mt <- matrix(NA,nrow=length(dt$fe),ncol=length(unique_chars), dimnames=list(NULL,unique_chars))

for(i in 1:length(dt$fe)){
  for(j in 1:length(colnames(mt))){
    if(any(unlist(strsplit(as.character(dt$fe[i]),", "))==colnames(mt)[j]))
      mt[i,j] <- 1
  }
}


mt_dt <- data.table(mt)

dt <- cbind(dt,mt_dt)

mt <- matrix(NA,nrow=length(dt$cluster),ncol=length(unique(dt$cluster)), dimnames=list(NULL,unique(dt$cluster)))

for(i in 1:length(dt$cluster)){
  for(j in 1:length(unique(dt$cluster))){
    if(unique(dt$cluster)[j]==dt$cluster[i])
      mt[i,j] <- 1
  }
}

mt_dt <- data.table(mt)

dt <- cbind(dt,mt_dt)

dt[,group_id:=paste(cluster,fe,sep="/")]

dt <- dt[order(cluster,fe)]
dt$cluster <- factor(dt$cluster,levels=unique(dt$cluster))

gg_est <- ggplot(dt,aes(x=group_id,y=impact,group=group_id))+
  geom_errorbar(aes(ymin=impact-1.96*se,ymax=impact+1.96*se),linewidth=.5,width=NA,color=dt$col)+
  geom_point(size=1.5,shape=21,color=dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=4))+
  facet_wrap(.~event,ncol=1,strip.position="left")+
  labs(title="Harvest-time change in conflict relative to the baseline (%)",x="",y="")+
  theme_paper()+
  theme(axis.text.x=element_blank(),strip.placement="outside")

spec_dt <- melt(dt[,.(group_id,xy,`country^year`,yearmo,year,mo,Cell,`Conley (150km)`,`Conley (300km)`,Country)],id.vars="group_id")

spec_dt$variable <- factor(spec_dt$variable,levels=rev(unique(spec_dt$variable)))

spec_dt <- spec_dt[order(group_id)]

spec1_dt <- spec_dt[variable %in% c("xy","country^year","yearmo","year","mo")]
spec1_dt$variable <- factor(spec1_dt$variable,levels=unique(spec1_dt$variable),labels=rev(c("Month","Year","Year-Month","Country-Year","Cell")))

spec2_dt <- spec_dt[variable %in% c("Cell","Conley (150km)","Conley (300km)","Country")]
spec2_dt$variable <- factor(spec2_dt$variable,levels=rev(unique(spec2_dt$variable)))

gg_spec1 <- ggplot(spec1_dt,aes(x=group_id,y=variable)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(rev(levels(spec1_dt$variable))) + 
  geom_point(aes(size=value),na.rm=T,shape=15,color="slategray")+
  scale_size_continuous(range=c(0,3))+
  labs(title="Fixed effects",x="",y="")+
  theme_paper()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust=0))

gg_spec2 <- ggplot(spec2_dt,aes(x=group_id,y=variable)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(levels(spec2_dt$variable)) + 
  geom_point(aes(size=value),na.rm=T,shape=15,color="slategray")+
  scale_size_continuous(range=c(0,3))+
  labs(title="Level of clustering",x="",y="")+
  theme_paper()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust=0))

gg_comb <- plot_grid(gg_est,gg_spec1,gg_spec2,ncol=1,align="hv",axis="lr",rel_heights=c(23,7,6))

ggsave("Figures/spec_myanmar.png",gg_comb,width=6.5,height=6.0,dpi="retina",device="png")

ggsave("Figures/spec_myanmar.eps",gg_comb,width=6.5,height=6.0,dpi="retina",device="eps")



