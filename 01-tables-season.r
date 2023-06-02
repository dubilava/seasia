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


# 00 - descriptive stats ----

## drop Brunei and Timor-Leste
datacomb_dt <- datacomb_dt[country %!in% c("Brunei","Timor-Leste")]
dataset_dt <- dataset_dt[country %!in% c("Brunei","Timor-Leste")]

datacomb_dt[,.(incidents=sum(incidents))]
datacomb_dt[,.(incidents=sum(incidents)),by=.(country)]
datacomb_dt[yearmo=="2020-01",.(cells=.N),by=.(country)]

tab1 <- datacomb_dt[,.(obs=.N,indicence_mean=round(mean(ifelse(incidents>0,1,0)),3),indicence_sd=round(sd(ifelse(incidents>0,1,0)),3),indicents_mean=round(mean(incidents),3),indicents_sd=round(sd(incidents),3),indicents_min=round(min(incidents),3),indicents_max=round(max(incidents),3))]

kable_styling(kable(tab1))

dataset_dt$event <- factor(dataset_dt$event,levels=unique(dataset_dt$event)[c(1,4,3,2,5)])


tab2 <- dataset_dt[,.(obs=.N,indicence_mean=round(mean(ifelse(incidents>0,1,0)),3),indicence_sd=round(sd(ifelse(incidents>0,1,0)),3),indicents_mean=round(mean(incidents),3),indicents_sd=round(sd(incidents),3),indicents_min=round(min(incidents),3),indicents_max=round(max(incidents),3)),by=.(event)]

kable_styling(kable(tab2))


tab3a <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_spam),3),Crop_area_sd=round(sd(area_spam),3),Crop_area_min=round(min(area_spam),3),Crop_area_max=round(max(area_spam),3))]

tab3i <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_i),3),Crop_area_sd=round(sd(area_i),3),Crop_area_min=round(min(area_i),3),Crop_area_max=round(max(area_i),3))]

tab3r <- datacomb_dt[yearmo=="2020-01",.(obs=.N,Crop_area_mean=round(mean(area_r),3),Crop_area_sd=round(sd(area_r),3),Crop_area_min=round(min(area_r),3),Crop_area_max=round(max(area_r),3))]

kable_styling(kable(rbind(tab3a,tab3i,tab3r)))

# some graphs
sub_dt <- datacomb_dt[yearmo=="2020-01",.(area=area_spam,irri=prop_i,country)]

sum_dt <- datacomb_dt[,.(incidents=mean(incidents),area=mean(area_spam),irri=mean(ifelse(prop_i>=.5,1,0))),by=.(xy,country)]
sum_dt[,irri:=ifelse(irri==1,"irrigated","rainfed")]

gg_area <- ggplot(sub_dt,aes(x=area*100000))+
  geom_histogram(bins=20,fill="coral",color="white")+
  labs(x="Cropland area (ha)",y="Count")+
  theme_paper()

ggsave("Figures/area.png",gg_area,width=6.5,height=4.0,dpi="retina")


gg_irri <- ggplot(sub_dt,aes(x=irri))+
  geom_histogram(bins=20,fill="steelblue",color="white")+
  labs(x="Proportion of irrigated croplands",y="Count")+
  theme_paper()

ggsave("Figures/irri.png",gg_area,width=6.5,height=4.0,dpi="retina")


color_palette <- colorRampPalette(colors=c("indianred","coral","goldenrod","forestgreen","seagreen","steelblue","dimgray"),interpolate="spline")

sample_of_colors <- color_palette(16)[seq(2,16,2)]

sample_of_shapes <- c(21:24,21:24)

gg_scatter <- ggplot(sub_dt,aes(x=log(area*100000),y=irri))+
  geom_point(aes(color=country,fill=country,shape=country),size=1.5,stroke=.5,alpha=.6,na.rm=T)+
  scale_colour_manual(values=sample_of_colors)+
  scale_fill_manual(values=sample_of_colors)+
  scale_shape_manual(values=sample_of_shapes)+
  labs(x="Cropland area (ha), natural log",y="Proportion of Irrigated Croplands")+
  theme_paper()+
  theme(legend.position="top")

ggsave("Figures/scatter.png",gg_scatter,width=6.5,height=5.5,dpi="retina")


gg_cor <- ggplot(sum_dt,aes(x=log(area*100000),y=log(incidents),shape=irri,color=irri))+
  geom_point(size=1.5,stroke=.5,na.rm=T)+
  scale_shape_manual(values=c(16,1))+
  scale_color_manual(values=c("steelblue","coral"))+
  labs(x="Cropland area (ha), natural log",y="Average number of conflict incidents, natural log")+
  theme_paper()+
  theme(legend.position="top")

gg_den <- ggplot(sum_dt,aes(x=log(incidents),color=irri))+
  geom_density(na.rm=T)+
  scale_color_manual(values=c("steelblue","coral"))+
  labs(x="",y="Density")+
  coord_flip(ylim=c(0,.2))+
  theme_paper()

gg_comb <- plot_grid(gg_cor,gg_den,ncol=2,align="hv",axis="tb",rel_widths=c(3,1))

ggsave("Figures/cor.png",gg_comb,width=6.5,height=4.0,dpi="retina")




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

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

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


# 01a - Check: balanced panel (2018:2022) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))>=2018]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))>2017]

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


# 01b - Check: balanced panel (2010:2022) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[country %!in% c("Indonesia","Malaysia","Philippines")]

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



# 01c - Check: pre-pandemic (2010-2019) ----

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))<2020]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

## effect
coef0_fe <- feols(incidents~area:seas | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact1(datasub_dt)

## evens-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

datasub_dt <- datasub_dt[as.numeric(as.character(year))<2020]

datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]

datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]

datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]

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


# 01d - Check: drop one country at a time ----

list_of_countries <- unique(datacomb_dt$country)

lst <- list()

for(i in 1:length(list_of_countries)){
  
  ## combined effect ----
  datasub_dt <- datacomb_dt
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
  datasub_dt <- datasub_dt[country!=list_of_countries[i]]
  
  ## impact
  c_comb <- impact1(datasub_dt)
  
  ## event-specific effects ----
  datasub_dt <- dataset_dt
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
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
  labs(title="",x="",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"),axis.text.y=element_text(hjust=0))


ggsave("Figures/dropacountry.png",gg_dropone,width=6.5,height=4.5,dpi="retina",device="png")

ggsave("Figures/dropacountry.eps",gg_dropone,width=6.5,height=4.5,dpi="retina",device="eps")




# 01e - Check: randomize harvest seasons ----

list_of_iter <- 1:100

lst <- list()

for(i in 1:length(list_of_iter)){
  
  ## combined effect ----
  datasub_dt <- datacomb_dt
  
  datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
  
  datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
  
  datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
  
  random_dt <- unique(datasub_dt[,.(xy,mo,harvest_season)])
  random_dt[,`:=`(id=as.numeric(factor(xy)))]
  sub_dt <- unique(random_dt[,.(id,xy)])
  set.seed(i)
  sub_dt[,`:=`(xy=sample(sub_dt$xy))]
  random_dt$xy <- NULL
  random_dt <- merge(random_dt,sub_dt,by="id",all.x=T)
  random_dt$id <- NULL
  
  datasub_dt$harvest_season <- NULL
  datasub_dt <- merge(datasub_dt,random_dt,by=c("xy","mo"),all.x=T)
  
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
  ## impact
  c_comb <- impact1(datasub_dt)
  
  ## event-specific effects ----
  datasub_dt <- dataset_dt
  
  datasub_dt <- datasub_dt[country!="Indonesia" | (country=="Indonesia" & as.numeric(as.character(year))>2014)]
  
  datasub_dt <- datasub_dt[country!="Philippines" | (country=="Philippines" & as.numeric(as.character(year))>2015)]
  
  datasub_dt <- datasub_dt[country!="Malaysia" | (country=="Malaysia" & as.numeric(as.character(year))>2017)]
  
  random_dt <- unique(datasub_dt[,.(xy,mo,harvest_season)])
  random_dt[,`:=`(id=as.numeric(factor(xy)))]
  sub_dt <- unique(random_dt[,.(id,xy)])
  set.seed(i)
  xy_i <- sample(sub_dt$xy)
  sub_dt[,`:=`(xy=xy_i)]
  random_dt$xy <- NULL
  random_dt <- merge(random_dt,sub_dt,by="id",all.x=T)
  random_dt$id <- NULL
  
  datasub_dt$harvest_season <- NULL
  datasub_dt <- merge(datasub_dt,random_dt,by=c("xy","mo"),all.x=T)
  
  datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]
  
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
  
  dt$iter <- list_of_iter[i]
  
  lst[[i]] <- dt
  
  print(i)
  
}

shuffle_dt <- Reduce(rbind,lst)

shuffle_dt[,`:=`(col=ifelse(est/se > 1.96,"coral",ifelse(est/se < -1.96,"steelblue","darkgray")))]

shuffle_dt$event <- factor(shuffle_dt$event,levels=unique(shuffle_dt$event))

shuffle_dt$iter <- factor(shuffle_dt$iter,levels=unique(shuffle_dt$iter)[length(unique(shuffle_dt$iter)):1])

gg_shuffle <- ggplot(shuffle_dt,aes(x=iter,y=est))+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),linewidth=.2,width=NA,color=shuffle_dt$col)+
  geom_point(size=0.5,color=shuffle_dt$col)+
  scale_x_discrete(breaks=seq(5,100,by=5))+
  facet_grid(.~event)+
  coord_flip(ylim=c(-30,30))+
  labs(title="",x="Iteration",y="Estimated impact (%) relative to the baseline")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

gg_den <- ggplot(shuffle_dt,aes(x=est))+
  geom_density(adjust=1.3,color="dimgray",linewidth=.5)+
  facet_grid(.~event)+
  coord_cartesian(xlim=c(-30,30))+
  labs(title="",x="",y="Density")+
  theme_paper()+
  theme(panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="darkgray"))

gg_comb <- plot_grid(gg_shuffle,gg_den,ncol=1,align="hv",axis="tblr",rel_heights=c(8,2))

ggsave("Figures/shuffleharvest.png",gg_comb,width=6.5,height=7.0,dpi="retina",device="png")

ggsave("Figures/shuffleharvest.eps",gg_comb,width=6.5,height=7.0,dpi="retina",device="eps")




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

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:gsrain_stand+gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact2(datasub_dt)

## event-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

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

datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season,irri=prop_i)]

## effect
coef0_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt,vcov=~xy)

## impact
c_comb <- impact3(datasub_dt)


## evens-specific effects ----

datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season,irri=prop_i)]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="battles"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="riots" ],vcov=~xy)
coef4_fe <- feols(incidents~area:seas+area:seas:irri+(area:seas+area:seas:irri):gsrain_stand | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)


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

# datasub_dt[,`:=`(conflict_mean=mean(conflict))]
# datasub_dt[,`:=`(battles_mean=mean(battles))]

## effect
coef1_fe <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, datasub_dt[event=="violence"],vcov=~xy)
coef2_fe <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, datasub_dt[event=="riots"],vcov=~xy)
coef3_fe <- feols(incidents~area:seas+area:seas:conf+conf | xy+yearmo, datasub_dt[event=="protests"],vcov=~xy)

coef1_fe
coef2_fe
coef3_fe

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




















# 01f - Check: specification ----

impact1i <- function(x,n){
  r <- feols(incidents~area:seas | sw(xy+yearmo,xy+year+mo), data=x,vcov=~xy)
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
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
  r <- feols(incidents~area:seas | sw(xy+yearmo,xy+year+mo), data=x,vcov=vcov_conley(lat="latitude",lon="longitude",cutoff=150,distance="spherical"))
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
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
  r <- feols(incidents~area:seas | sw(xy+yearmo,xy+year+mo), data=x,vcov=vcov_conley(lat="latitude",lon="longitude",cutoff=300,distance="spherical"))
  
  m <- x[area>0,.(incidents=mean(incidents),cropland=mean(area))]
  
  s <- 100*m$cropland/m$incidents
  
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

## combined effect ----
datasub_dt <- datacomb_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

## effect

## impact
c_comb1 <- impact1i(datasub_dt,2)
c_comb1$cluster <- "Cell"

c_comb3 <- impact1c150(datasub_dt,2)
c_comb3$cluster <- "Conley (150km)"

c_comb4 <- impact1c300(datasub_dt,2)
c_comb4$cluster <- "Conley (300km)"

c_comb <- rbind(c_comb1,c_comb3,c_comb4)
c_comb$event <- "combined"

## event-specific effects ----
datasub_dt <- dataset_dt
datasub_dt[,`:=`(area=area_spam,seas=harvest_season)]

## impact
c_battles1 <- impact1i(datasub_dt[event=="battles"],2)
c_violence1 <- impact1i(datasub_dt[event=="violence"],2)
c_riots1 <- impact1i(datasub_dt[event=="riots"],2)
c_protests1 <- impact1i(datasub_dt[event=="protests"],2)

c_battles1$cluster <- "Cell"
c_violence1$cluster <- "Cell"
c_riots1$cluster <- "Cell"
c_protests1$cluster <- "Cell"


c_battles3 <- impact1c150(datasub_dt[event=="battles"],2)
c_violence3 <- impact1c150(datasub_dt[event=="violence"],2)
c_riots3 <- impact1c150(datasub_dt[event=="riots"],2)
c_protests3 <- impact1c150(datasub_dt[event=="protests"],2)

c_battles3$cluster <- "Conley (150km)"
c_violence3$cluster <- "Conley (150km)"
c_riots3$cluster <- "Conley (150km)"
c_protests3$cluster <- "Conley (150km)"


c_battles4 <- impact1c300(datasub_dt[event=="battles"],2)
c_violence4 <- impact1c300(datasub_dt[event=="violence"],2)
c_riots4 <- impact1c300(datasub_dt[event=="riots"],2)
c_protests4 <- impact1c300(datasub_dt[event=="protests"],2)

c_battles4$cluster <- "Conley (300km)"
c_violence4$cluster <- "Conley (300km)"
c_riots4$cluster <- "Conley (300km)"
c_protests4$cluster <- "Conley (300km)"


c_battles <- rbind(c_battles1,c_battles3,c_battles4)
c_battles$event <- "battles"

c_violence <- rbind(c_violence1,c_violence3,c_violence4)
c_violence$event <- "violence"

c_riots <- rbind(c_riots1,c_riots3,c_riots4)
c_riots$event <- "riots"

c_protests <- rbind(c_protests1,c_protests3,c_protests4)
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
  for(j in 1:length(unique_chars)){
    if(any(grepl(paste0("^",unique_chars[j],"$"),unlist(strsplit(as.character(dt$fe[i]),", ")))))
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
dt$country <- "All"

# dt[,fecl:=paste(cluster,fe,sep="/")]

# dt <- dt[order(cluster,fe)]
# dt$cluster <- factor(dt$cluster,levels=unique(dt$cluster))


mt <- matrix(NA,nrow=nrow(dropone_dt),ncol=length(unique(dropone_dt$country)),dimnames=list(NULL,unique(dropone_dt$country)))

for(i in 1:length(dropone_dt$country)){
  for(j in 1:length(unique(dropone_dt$country))){
    if(unique(dropone_dt$country)[j]==dropone_dt$country[i])
      mt[i,j] <- 1
  }
}

mt_dt <- data.table(mt)

dropone_dt[,`:=`(fe="xy, yearmo",cluster="Cell")]

dropone_dt <- dropone_dt[,.(impact=est,se,fe,cluster,event,col,xy=1,yearmo=1,year=NA,mo=NA,Cell=1,`Conley (150km)`=NA,`Conley (300km)`=NA,country)]

dropone_dt <- cbind(dropone_dt,mt_dt)
dropone_dt$All <- NA

dt <- dt[,.(impact,se,fe,cluster,event,col,xy,yearmo,year,mo,Cell,`Conley (150km)`,`Conley (300km)`,country)]

dt$All <- 1

dt[,c(names(mt_dt)):=NA]

dt <- rbind(dt,dropone_dt)

dt[,group_id:=paste(country,cluster,fe,sep="/")]

dt <- dt[order(country,cluster,fe)]
dt$country <- factor(dt$country,levels=unique(dt$country))
dt$cluster <- factor(dt$cluster,levels=unique(dt$cluster))

gg_est <- ggplot(dt,aes(x=group_id,y=impact,group=group_id))+
  geom_errorbar(aes(ymin=impact-1.96*se,ymax=impact+1.96*se),linewidth=.5,width=NA,color=dt$col)+
  geom_point(size=1.5,shape=21,color=dt$col,fill="white",stroke=.8)+
  scale_y_continuous(breaks=pretty_breaks(n=4))+
  facet_wrap(.~event,ncol=1,strip.position="left",scales="free_y")+
  labs(title="Estimated impact (%) relative to the baseline",x="",y="")+
  theme_paper()+
  theme(axis.text.x=element_blank(),strip.placement="outside")

spec_dt <- melt(dt[,.(group_id,xy,yearmo,year,mo,Cell,`Conley (150km)`,`Conley (300km)`,None=All,Cambodia,Indonesia,Laos,Malaysia,Myanmar,Philippines,Thailand,Vietnam)],id.vars="group_id")

spec_dt$variable <- factor(spec_dt$variable,levels=rev(unique(spec_dt$variable)))

spec_dt <- spec_dt[order(group_id)]

spec1_dt <- spec_dt[variable %in% c("xy","yearmo","year","mo")]
spec1_dt$variable <- factor(spec1_dt$variable,levels=unique(spec1_dt$variable))

spec2_dt <- spec_dt[variable %in% c("Cell","Conley (150km)","Conley (300km)")]
spec2_dt$variable <- factor(spec2_dt$variable,levels=rev(unique(spec2_dt$variable)))

spec3_dt <- spec_dt[variable %in% names(mt_dt)]
spec3_dt$variable <- factor(spec3_dt$variable,levels=rev(unique(spec3_dt$variable)))

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

gg_spec3 <- ggplot(spec3_dt,aes(x=group_id,y=variable)) + 
  geom_tile(na.rm=T,color=NA,fill=NA) + 
  ylim(levels(spec3_dt$variable)) + 
  geom_point(aes(size=value),na.rm=T,shape=15,color="slategray")+
  scale_size_continuous(range=c(0,3))+
  labs(title="Dropped country",x="",y="")+
  theme_paper()+
  theme(axis.title.x=element_blank(),axis.line.x = element_blank(),axis.text.x = element_blank(),axis.text.y = element_text(hjust=0))

gg_comb <- plot_grid(gg_est,gg_spec1,gg_spec2,gg_spec3,ncol=1,align="hv",axis="lr",rel_heights=c(30,7,6,11))

ggsave("Figures/spec.png",gg_comb,width=6.5,height=7.5,dpi="retina",device="png")

ggsave("Figures/spec.eps",gg_comb,width=6.5,height=7.5,dpi="retina",device="eps")



