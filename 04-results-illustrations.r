library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scatterpie)
library(cowplot)
library(gridExtra)
library(Cairo)
library(stringr)
library(sf)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")
library(kableExtra)

## clean up the environment (just in case)
rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

theme_guess <- function(base_size=12,base_family="sans",title_family="sans",border=F){
  theme_foundation(base_size=base_size,base_family=base_family) +
    theme(
      line = element_line(linetype=1,colour="black"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="black"),
      # title = element_text(family=title_family,size=rel(1.1)),
      # panel.background=element_rect(fill="transparent",color=NA),
      panel.grid = element_line(colour=NULL,linetype=3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.background=element_rect(fill="transparent",color=NA),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.caption = element_text(family=base_family,size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,1.25),"lines"),
      # axis.title = element_blank(),
      axis.text = element_text(family=base_family,size=rel(0.9),margin=margin(t=1,r=1,b=1,l=1)),
      axis.text.x = element_text(colour = NULL),
      axis.text.y = element_text(colour = NULL),
      axis.ticks = element_blank(),
      axis.line = element_line(),
      axis.line.y = element_blank(),
      legend.background=element_rect(fill="transparent",color=NA),
      legend.position="none",
      legend.title=element_blank(),
      legend.text=element_text(family=base_family,size=rel(0.9),colour="slategray"),
      legend.key = element_rect(fill="transparent"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(size=rel(.8),colour="slategray",margin=margin(.1,0,.1,0,"cm"))
    )
}

# data management ----

## load the map of se asia
load("estimates.RData")

main_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_main <- ggplot(main_dt,aes(x=event,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color=main_dt$col)+
  geom_point(size=2,color=main_dt$col)+
  coord_flip()+
  labs(title="",x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_guess()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/main_results.png",gg_main,width=6.5,height=3.5,dpi="retina")


rain_dt <- rbind(rain_dt[,.(event,est=est1,se=se1,rain="average")],rain_dt[,.(event,est=est2,se=se2,rain="rainy")])

gg_rain <- ggplot(rain_dt,aes(x=event,y=est,color=rain,linetype=rain))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.3))+
  geom_point(size=2,position= position_dodge(width=0.3))+
  scale_color_manual(values=c("darkgray","steelblue"))+
  scale_linetype_manual(values=c(1,5))+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_guess()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(2,'lines'))

ggsave("Figures/rain_results.png",gg_rain,width=6.5,height=3.5,dpi="retina")



price_dt <- rbind(price_dt[,.(event,est=est1,se=se1,rain="average")],price_dt[,.(event,est=est2,se=se2,rain="pricey")])

gg_price <- ggplot(price_dt,aes(x=event,y=est,color=rain,linetype=rain))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.3))+
  geom_point(size=2,position= position_dodge(width=0.3))+
  scale_color_manual(values=c("darkgray","coral"))+
  scale_linetype_manual(values=c(1,5))+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_guess()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(2,'lines'))

ggsave("Figures/price_results.png",gg_price,width=6.5,height=3.5,dpi="retina")



irri_dt <- rbind(irrirain_dt[,.(event,est=est1,se=se1,rain="rainfed, average")],irrirain_dt[,.(event,est=est2,se=se2,rain="rainfed, rainy")],irrirain_dt[,.(event,est=est3,se=se3,rain="irrigated, average")],irrirain_dt[,.(event,est=est4,se=se4,rain="irrigated, rainy")])

gg_irri <- ggplot(irri_dt,aes(x=event,y=est,color=rain,linetype=rain))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.6))+
  geom_point(size=2,position= position_dodge(width=0.6))+
  scale_color_manual(values=c("coral","seagreen","darkgray","steelblue"))+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_guess()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(2,'lines'),legend.text=element_text(size=8))

ggsave("Figures/irri_results.png",gg_irri,width=6.5,height=3.5,dpi="retina")








