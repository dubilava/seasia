library(data.table)
library(ggplot2)
library(stringr)

rm(list=ls())
gc()

giews_dt <- fread("GIEWS/GIEWS_Data_SEA.csv")
giews_dt[,`:=`(Date=as.Date(Date,format="%d/%m/%Y"))]

giews_dt <- giews_dt[str_detect(giews_dt$Commodity,"Rice")]

giews_dt$V1 <- NULL

giews_dt <- giews_dt[complete.cases(giews_dt)]

giews_dt <- giews_dt[order(Country,Location,Commodity,Level,Date)]

giews_dt$Date <- as.Date(giews_dt$Date)
giews_dt$Price <- as.numeric(giews_dt$Price)

giews_dt[,`:=`(obs=.N),by=.(Country,Location,Commodity,Level)]

giews_dt <- giews_dt[obs>=120]

save(giews_dt,file="giews.RData")



