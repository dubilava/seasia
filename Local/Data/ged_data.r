library(data.table)

rm(list=ls())
gc()

"%!in%" <- Negate("%in%")

load("GEDEvent_v22_1.RData")

ged_dt <- as.data.table(GEDEvent_v22_1)

unique(ged_dt$country)[order(unique(ged_dt$country))]

list_of_countries <- c("Cambodia (Kampuchea)","Indonesia","Laos","Malaysia","Myanmar (Burma)","Philippines","Thailand")

ged_dt <- ged_dt[country %in% list_of_countries]

ged_dt[country=="Cambodia (Kampuchea)"]$country <- "Cambodia"
ged_dt[country=="Myanmar (Burma)"]$country <- "Myanmar"

save(ged_dt,file="ged_seasia.RData")
