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


igc_dt <- fread("Local/Data/IGC/Prices_Rice.csv")

igc_dt$Date <- as.Date(igc_dt$Date,format="%d/%m/%Y")

igc_dt[,`:=`(year=substr(Date,1,4),mo=substr(Date,6,7))]

igc_dt <- igc_dt[,.(India=mean(India,na.rm=T),Pakistan=mean(Pakistan,na.rm=T),Thailand=mean(Thailand,na.rm=T),USA=mean(USA,na.rm=T),Vietnam=mean(Vietnam,na.rm=T),Maize=mean(Maize,na.rm=T)),by=.(year,mo)]

igc_dt[,`:=`(Thailand=na.approx(Thailand),USA=na.approx(USA),Vietnam=na.approx(Vietnam),Maize=na.approx(Maize))]

igc_dt[,`:=`(Date=as.Date(paste0(year,"-",mo,"-01")))]

igc_dt[,`:=`(ln_tha=log(Thailand),ln_usa=log(USA),ln_vnm=log(Vietnam),ln_mai=log(Maize))]
igc_dt[,`:=`(lg_tha=shift(ln_tha,12),lg_usa=shift(ln_usa,12),lg_vnm=shift(ln_vnm,12),lg_mai=shift(ln_mai,12))]
igc_dt[,`:=`(infl_tha=ln_tha-lg_tha,infl_usa=ln_usa-lg_usa,infl_vnm=ln_vnm-lg_vnm,infl_mai=ln_mai-lg_mai)]


fpma_dt <- fread("Local/Data/FPMA_wholesale_clean.csv")

list_of_countries <- c("Cambodia","Myanmar","Philippines","Thailand","Viet Nam")


fpma_dt <- fpma_dt[Country%in%list_of_countries]

fpma_dt <- fpma_dt[Commodity %like% "Rice"]

fpma_dt <- fpma_dt[order(Country,Market,Commodity,Date)]

fpma_dt[,`:=`(ln_price=log(Price_USD_Tonne))]
fpma_dt[,`:=`(lg_price=shift(ln_price,12))]
fpma_dt[,`:=`(infl=ln_price-lg_price)]


price_dt <- merge(fpma_dt,igc_dt,by="Date",all.x=T)

price_dt[,`:=`(Market_Commodity=paste0(Market,"-",Commodity))]



cor_dt <- unique(price_dt[,.(Country,Market,Commodity,Market_Commodity)])

cor_dt <- cor_dt[order(Country,Market,Commodity)]

cor_dt[,`:=`(THA=as.numeric(NA),VNM=as.numeric(NA),USA=as.numeric(NA),tha=as.numeric(NA),vnm=as.numeric(NA),usa=as.numeric(NA))]

for(i in cor_dt$Market_Commodity){
  cor_dt[Market_Commodity==i]$THA <- round(cor(price_dt[Market_Commodity==i,.(Price_USD_Tonne,Thailand)],use="complete.obs")[1,2],2)
  cor_dt[Market_Commodity==i]$VNM <- round(cor(price_dt[Market_Commodity==i,.(Price_USD_Tonne,Vietnam)],use="complete.obs")[1,2],2)
  cor_dt[Market_Commodity==i]$USA <- round(cor(price_dt[Market_Commodity==i,.(Price_USD_Tonne,USA)],use="complete.obs")[1,2],2)
  cor_dt[Market_Commodity==i]$tha <- round(cor(price_dt[Market_Commodity==i,.(infl,infl_tha)],use="complete.obs")[1,2],2)
  cor_dt[Market_Commodity==i]$vnm <- round(cor(price_dt[Market_Commodity==i,.(infl,infl_vnm)],use="complete.obs")[1,2],2)
  cor_dt[Market_Commodity==i]$usa <- round(cor(price_dt[Market_Commodity==i,.(infl,infl_usa)],use="complete.obs")[1,2],2)
}

cor_dt$Market_Commodity <- NULL

kable_styling(kable(cor_dt))
