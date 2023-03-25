library(data.table)
library(ggplot2)
library(stringr)

rm(list=ls())
gc()

dt_khm <- fread("WFP/wfp_food_prices_khm.csv")[-1,]
dt_lao <- fread("WFP/wfp_food_prices_lao.csv")[-1,]
dt_mmr <- fread("WFP/wfp_food_prices_mmr.csv")[-1,]
dt_phl <- fread("WFP/wfp_food_prices_phl.csv")[-1,]
dt_tha <- fread("WFP/wfp_food_prices_tha.csv")[-1,]
dt_tls <- fread("WFP/wfp_food_prices_tls.csv")[-1,]
dt_vnm <- fread("WFP/wfp_food_prices_vnm.csv")[-1,]

dt_khm <- dt_khm[str_detect(dt_khm$commodity,"Rice")]
dt_lao <- dt_lao[str_detect(dt_lao$commodity,"Rice")]
dt_mmr <- dt_mmr[str_detect(dt_mmr$commodity,"Rice")]
dt_phl <- dt_phl[str_detect(dt_phl$commodity,"Rice")]
dt_tha <- dt_tha[str_detect(dt_tha$commodity,"Rice")]
dt_tls <- dt_tls[str_detect(dt_tls$commodity,"Rice")]
dt_vnm <- dt_vnm[str_detect(dt_vnm$commodity,"Rice")]

dt_khm[,`:=`(country="Cambodia")]
dt_lao[,`:=`(country="Laos")]
dt_mmr[,`:=`(country="Myanmar")]
dt_phl[,`:=`(country="Philippines")]
dt_tha[,`:=`(country="Thailand")]
dt_tls[,`:=`(country="TimorLeste")]
dt_vnm[,`:=`(country="VietNam")]


wfp_dt <- Reduce(rbind,list(dt_khm,dt_lao,dt_mmr,dt_phl,dt_tha,dt_tls,dt_vnm))
wfp_dt$category <- NULL
wfp_dt <- wfp_dt[order(country,admin1,admin2,market,commodity,pricetype,date)]

wfp_dt$date <- as.Date(wfp_dt$date)
wfp_dt$usdprice <- as.numeric(wfp_dt$usdprice)

wfp_dt[,`:=`(obs=.N),by=.(country,admin1,admin2,market,commodity,pricetype)]

wfp_dt <- wfp_dt[obs>=120]

save(wfp_dt,file="wfp.RData")



