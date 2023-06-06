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

theme_paper <- function(base_size=13,border=F){
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

# data management ----

## load the map of se asia
load("results.RData")

main_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_main <- ggplot(main_dt,aes(x=event,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color=main_dt$col)+
  geom_point(size=2,color=main_dt$col)+
  coord_flip()+
  labs(title="",x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/results_main.png",gg_main,width=6.5,height=4.0,dpi="retina")


rain_dt <- rbind(rain_dt[,.(event,est=est1,se=se1,rain="average")],rain_dt[,.(event,est=est2,se=se2,rain="rainy")])

rain_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_rain <- ggplot(rain_dt,aes(x=event,y=est,linetype=rain))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.4),color=rain_dt$col)+
  geom_point(size=2,position= position_dodge(width=0.4),color=rain_dt$col)+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(3,'lines'))

ggsave("Figures/results_rain.png",gg_rain,width=6.5,height=4.0,dpi="retina")



irri_dt <- rbind(irrirain_dt[,.(event,est=est1,se=se1,rain="average",system="rainfed")],irrirain_dt[,.(event,est=est2,se=se2,rain="rainy",system="rainfed")],irrirain_dt[,.(event,est=est3,se=se3,rain="average",system="irrigated")],irrirain_dt[,.(event,est=est4,se=se4,rain="rainy",system="irrigated")])

irri_dt <- irri_dt[order(rain,-system)]
irri_dt$rain <- factor(irri_dt$rain,levels=unique(irri_dt$rain))
irri_dt$system <- factor(irri_dt$system,levels=unique(irri_dt$system))

irri_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_irri <- ggplot(irri_dt,aes(x=event,y=est,linetype=rain,color=col))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.4))+
  geom_point(size=2,position= position_dodge(width=0.4))+
  scale_color_manual(values=c("coral","darkgray","steelblue"),guide="none")+
  facet_grid(.~system)+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(3,'lines'))

ggsave("Figures/results_irri.png",gg_irri,width=6.5,height=4.0,dpi="retina")


conflict_dt <- rbind(regime_dt[,.(event,est=est1,se=se1,conflict="average")],regime_dt[,.(event,est=est2,se=se2,conflict="elevated")])

conflict_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_conflict <- ggplot(conflict_dt,aes(x=event,y=est,linetype=conflict))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=conflict),linewidth=.8,width=NA,position= position_dodge(width=0.3),color=conflict_dt$col)+
  geom_point(size=2,position= position_dodge(width=0.3),color=conflict_dt$col)+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(3,'lines'))

ggsave("Figures/results_conflict.png",gg_rain,width=6.5,height=4.0,dpi="retina")









## load the map of se asia
load("results_myanmar.RData")

main_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_main <- ggplot(main_dt,aes(x=event,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.8,width=NA,color=main_dt$col)+
  geom_point(size=2,color=main_dt$col)+
  coord_flip()+
  labs(title="",x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

ggsave("Figures/results_main_myanmar.png",gg_main,width=6.5,height=4.0,dpi="retina")


rain_dt <- rbind(rain_dt[,.(event,est=est1,se=se1,rain="average")],rain_dt[,.(event,est=est2,se=se2,rain="rainy")])

rain_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_rain <- ggplot(rain_dt,aes(x=event,y=est,linetype=rain))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.3),color=rain_dt$col)+
  geom_point(size=2,position= position_dodge(width=0.3),color=rain_dt$col)+
  scale_linetype_manual(values=c(1,5))+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(3,'lines'))

ggsave("Figures/results_rain_myanmar.png",gg_rain,width=6.5,height=4.0,dpi="retina")





irri_dt <- rbind(irrirain_dt[,.(event,est=est1,se=se1,rain="average",system="rainfed")],irrirain_dt[,.(event,est=est2,se=se2,rain="rainy",system="rainfed")],irrirain_dt[,.(event,est=est3,se=se3,rain="average",system="irrigated")],irrirain_dt[,.(event,est=est4,se=se4,rain="rainy",system="irrigated")])

irri_dt <- irri_dt[order(rain,-system)]
irri_dt$rain <- factor(irri_dt$rain,levels=unique(irri_dt$rain))
irri_dt$system <- factor(irri_dt$system,levels=unique(irri_dt$system))

irri_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_irri <- ggplot(irri_dt,aes(x=event,y=est,linetype=rain,color=col))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=rain),linewidth=.8,width=NA,position= position_dodge(width=0.6))+
  geom_point(size=2,position= position_dodge(width=0.6))+
  scale_color_manual(values=c("coral","darkgray","steelblue"),guide="none")+
  facet_grid(.~system)+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(3,'lines'))

ggsave("Figures/results_irri_myanmar.png",gg_irri,width=6.5,height=4.0,dpi="retina")



conflict_dt <- rbind(regime_dt[,.(event,est=est1,se=se1,conflict="average")],regime_dt[,.(event,est=est2,se=se2,conflict="elevated")])

conflict_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

gg_conflict <- ggplot(conflict_dt,aes(x=event,y=est,linetype=conflict))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se,group=conflict),linewidth=.8,width=NA,position= position_dodge(width=0.3),color=conflict_dt$col)+
  geom_point(size=2,position= position_dodge(width=0.3),color=conflict_dt$col)+
  scale_linetype_manual(values=c(1,5))+
  coord_flip()+
  labs(x="Conflict Type",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(axis.text.y=element_text(hjust=0),axis.title.y=element_text(margin=margin(t=0,r=5,b=0,l=0)),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),legend.position="top",legend.key.width=unit(3,'lines'))

ggsave("Figures/results_conflict_myanmar.png",gg_rain,width=6.5,height=4.0,dpi="retina")



