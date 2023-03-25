library(data.table)
library(ggplot2)
library(stringr)

rm(list=ls())
gc()

# clean and merge ----
load("giews.RData")
load("wfp.RData")

# keep the relevant columns in giews data
giews_dt <- giews_dt[,.(date=as.Date(Date),price_giews=as.numeric(Price),unit_giews=Unit,country=Country,market=Location,commodity=Commodity,level=tolower(Level))]

wfp_dt <- wfp_dt[,.(date=as.Date(date),price_wfp=as.numeric(usdprice),unit_wfp="USD/Kg",price_lcu=as.numeric(price),lcu=currency,country,market,commodity=commodity,level=tolower(pricetype),longitude=round(as.numeric(longitude),3),latitude=round(as.numeric(latitude),3))]

wfp_dt[,`:=`(xr=price_lcu/price_wfp)]

# rename some countries in giews to make it wfp compatible
giews_dt[country=="Lao People's Democratic Republic"]$country <- "Laos"
giews_dt[country=="Viet Nam"]$country <- "Vietnam"
wfp_dt[country=="VietNam"]$country <- "Vietnam"

unique(wfp_dt$market)[order(unique(wfp_dt$market))]
unique(giews_dt$market)[order(unique(giews_dt$market))]

# rename some markets in giews or wfp to make them compatible
wfp_dt[market=="Cebu City"]$market <- "Cebu"
wfp_dt[market=="Davao City"]$market <- "Davao"
giews_dt[market=="Davao City"]$market <- "Davao"
wfp_dt[market=="Iloilo City"]$market <- "Iloilo"
wfp_dt[market=="Metro Manila"]$market <- "Manila"
giews_dt[market=="MetroManila"]$market <- "Manila"
wfp_dt[market=="Vientiane Municipality"]$market <- "Vientiane"
giews_dt[market=="Vientiane Capital"]$market <- "Vientiane"
giews_dt[market=="Champasack"]$market <- "Champassack"
wfp_dt[market=="Zamboanga City"]$market <- "Zamboanga"


unique(wfp_dt$commodity)[order(unique(wfp_dt$commodity))]
unique(giews_dt$commodity)[order(unique(giews_dt$commodity))]


giews_dt[commodity=="Rice (Glutinous,first quality)"]$commodity <- "Rice (glutinous, first quality)"
giews_dt[commodity=="Rice (Glutinous,second quality)"]$commodity <- "Rice (glutinous, second quality)"
giews_dt[commodity=="Rice (Ordinary,first quality)"]$commodity <- "Rice (ordinary, first quality)"
giews_dt[commodity=="Rice (Ordinary,second quality)"]$commodity <- "Rice (ordinary, second quality)"
giews_dt[commodity=="Rice (regular milled)"]$commodity <- "Rice (regular, milled)"
giews_dt[commodity=="Rice (well milled)"]$commodity <- "Rice (milled, superior)"
giews_dt[commodity=="Rice (Mix)"]$commodity <- "Rice (mixed, low quality)"
giews_dt[commodity=="Rice (Emata Manawthukha-FQ)"]$commodity <- "Rice (emata manawthukha)"
giews_dt[commodity=="Rice (Emata Medium)"]$commodity <- "Rice (emata medium)"
giews_dt[commodity=="Rice (Ordinary)"]$commodity <- "Rice (ordinary)"



# ggplot(wfp_dt[country=="Cambodia"],aes(x=date,y=xr,color=market))+
#   geom_line()


prices_dt <- merge(wfp_dt,giews_dt,by=c("date","country","market","level","commodity"),all=T)

prices_dt <- prices_dt[order(country,market,commodity,level,date)]

places_dt <- prices_dt[,.(latitude=round(as.numeric(unique(latitude)),3),longitude=round(as.numeric(unique(longitude)),3)),by=.(country,market)]

places_dt <- places_dt[complete.cases(places_dt)]


countries <- unique(places_dt$country)


library(sf)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)


# load the map of se asia
load("acled_seasia.RData")

countries <- unique(acled_dt$country)

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

gg_map <- ggplot(data = southeastasia) +
  geom_sf(color="gray",fill=NA,size=.25)+
  geom_point(data=places_dt,aes(x=longitude,y=latitude),size=1,color="indianred")+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),legend.position = "none")#,panel.background=element_rect(fill="white",color=NA),plot.background=element_rect(fill="white",color=NA))

ggsave("local_prices.png",gg_map,width=6.5,height=5.5,dpi="retina")




prices_dt[,`:=`(price=ifelse(!is.na(price_wfp),price_wfp,price_giews),unit=ifelse(!is.na(unit_wfp),unit_wfp,unit_giews))]


places_wfp_dt <- wfp_dt[,.(market=unique(market),latitude1=round(as.numeric(unique(latitude)),3),longitude1=round(as.numeric(unique(longitude)),3)),by=.(country,level)]

prices_dt <- merge(prices_dt,places_wfp_dt,by=c("country","level","market"),all.x=T)

prices_dt[,`:=`(longitude=ifelse(!is.na(longitude),longitude,longitude1),latitude=ifelse(!is.na(latitude),latitude,latitude1))]

prices_dt <- prices_dt[,.(date,country,market,longitude,latitude,commodity,level,price,unit)]

# prices_dt <- prices_dt[!complete.cases(prices_dt)]

average_dt <- prices_dt[,.(date,country,market,commodity,level,price,unit)]

average_dt <- average_dt[unit=="USD/Kg"]

# giews_dt[Location%in%test & Date>="2006-01-01"]
# wfp_dt[market%in%test & date>="2006-01-01"]
