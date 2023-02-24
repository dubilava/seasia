library(data.table)
library(ggplot2)
library(stringr)

dt <- fread("Local/Data/FPMA_retail.csv")

commodities <- unique(dt$Commodity)[order(unique(dt$Commodity))]

list_of_commodities <- c("Maize","Maize (white)","Maize (white, imported)","Maize (yellow)","Sorghum","Sorghum (Feterita)","Sorghum (Maicillo)","Sorghum (red)","Sorghum (white)")

grains_dt <- dt[Commodity %in% list_of_commodities]

sub_dt <- grains_dt[,.N,by=.(Country,Market,Commodity)]
ggplot(sub_dt,aes(x=N))+
  geom_histogram()

sub2_dt <- sub_dt[N>118]

grains2_dt <- merge(grains_dt,sub2_dt,by=c("Country","Market","Commodity"),all.x=T)

grains2_dt <- grains2_dt[N!="NA"]

countries <- unique(grains2_dt$Country)

africa <- countries[c(1,3:5,7,10,12:15,19:21,23:24)]

grains3_dt <- grains2_dt[Country %in% africa & Market != "National Average" & Market != "National average"]

locations <- unique(grains3_dt[,.(Country,Market)])

# grains4_dt <- grains3_dt[Commodity %in% unique(grains3_dt$Commodity)[c(1:3,5,7:9,13:16)]]

# unique(grains4_dt[,.(Country,Market)])
