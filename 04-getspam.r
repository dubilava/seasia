library(data.table)
library(ggplot2)
library(sp)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(R.utils)
library(stars)

rm(list=ls())
gc()

# you will need to have downloaded the relevant zipped folder from 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V
# and stored it in a folder called SPAM within the same folder as the present R file.
# unzip/extract all files from this zipped folder (called dataverse files); 
# then proceed as follows.

# load the conflict data to select the relevant southeast asia countries 
load("acled.RData")

countries <- unique(acled_dt$country)

# load the southeast asia raster
southeastasia <- ne_countries(country=countries,returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


rice_a <- raster("SPAM/dataverse_files/spam2010V2r0_global_A_RICE_A.tif")
rice_i <- raster("SPAM/dataverse_files/spam2010V2r0_global_A_RICE_I.tif")
rice_r <- raster("SPAM/dataverse_files/spam2010V2r0_global_A_RICE_R.tif")
rice_h <- raster("SPAM/dataverse_files/spam2010V2r0_global_A_RICE_H.tif")
rice_l <- raster("SPAM/dataverse_files/spam2010V2r0_global_A_RICE_L.tif")
rice_s <- raster("SPAM/dataverse_files/spam2010V2r0_global_A_RICE_S.tif")


rice_a05 <- aggregate(rice_a,fact=12,fun=sum)
rice_i05 <- aggregate(rice_i,fact=12,fun=sum)
rice_r05 <- aggregate(rice_r,fact=12,fun=sum)
rice_h05 <- aggregate(rice_h,fact=12,fun=sum)
rice_l05 <- aggregate(rice_l,fact=12,fun=sum)
rice_s05 <- aggregate(rice_s,fact=12,fun=sum)


mm <- mask(rice_a05,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
rice_a05_dt <- data.table(round(rp,1))
colnames(rice_a05_dt) <- c("x","y","area_spam")

mm <- mask(rice_i05,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
rice_i05_dt <- data.table(round(rp,1))
colnames(rice_i05_dt) <- c("x","y","area_i")

mm <- mask(rice_r05,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
rice_r05_dt <- data.table(round(rp,1))
colnames(rice_r05_dt) <- c("x","y","area_r")

mm <- mask(rice_h05,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
rice_h05_dt <- data.table(round(rp,1))
colnames(rice_h05_dt) <- c("x","y","area_h")

mm <- mask(rice_l05,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
rice_l05_dt <- data.table(round(rp,1))
colnames(rice_l05_dt) <- c("x","y","area_l")

mm <- mask(rice_s05,southeastasia)
cr <- crop(mm,extent(southeastasia))
rp <- rasterToPoints(cr)
rice_s05_dt <- data.table(round(rp,1))
colnames(rice_s05_dt) <- c("x","y","area_s")

spam_dt <- Reduce(function(...) merge(...,by=c("x","y"),all=T),list(rice_a05_dt,rice_i05_dt,rice_r05_dt,rice_h05_dt,rice_l05_dt,rice_s05_dt))


save(spam_dt,file="spam.RData")
