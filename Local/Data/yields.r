library(data.table)
library(ggplot2)
library(cowplot)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(R.utils)


rm(list=ls())
gc()


"%!in%" <- Negate("%in%")

getmode <- function(v){
  uniqv <- unique(v)[!is.na(unique(v))]
  uniqv[base::which.max(tabulate(match(v, uniqv)))]
}


# load the map of se asia
load("acled_seasia.RData")

southeastasia <- ne_countries(country=unique(acled_dt$country),returnclass="sf",scale="large")
southeastasia <- st_set_crs(southeastasia,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")



crop_list <- c("rice_major","rice_second","maize_major","maize_second")
years_list <- paste0(1981:2016)

yield_list <- list()

for(i in 1:length(years_list)){
  
  year <- years_list[i]
  
  # Rice
  r1_nc <- nc_open(paste0('Yields/rice_major/yield_',year,'.nc4'))
  
  lon <- ncvar_get(r1_nc,"lon")
  lat <- ncvar_get(r1_nc,"lat")
  yld <- ncvar_get(r1_nc,"var")
  
  fillvalue <- ncatt_get(r1_nc,"var","_FillValue")
  
  nc_close(r1_nc) 
  
  yld[yld == fillvalue$value] <- NA
  
  lonlat <- as.matrix(expand.grid(lon,lat))
  
  yld_vec <- as.vector(yld)
  
  yld_dt <- data.table(cbind(lonlat,yld_vec))
  names(yld_dt) <- c("x","y","yield")
  
  yld_r <- rasterFromXYZ(yld_dt) 
  
  yld_r10 <- aggregate(yld_r,fact=2,fun=mean,na.rm=T)
  raster_mask <- mask(yld_r10,southeastasia)
  rm <- rasterToPoints(raster_mask)
  rm[,1:2] <- round(rm[,1:2],1)
  r1_dt <- data.table(rm)
  colnames(r1_dt) <- c("x","y","Rice_yield")
  r1_dt <- r1_dt[order(x,y)]
  
  
  # Rice.2
  r2_nc <- nc_open(paste0('Yields/rice_second/yield_',year,'.nc4'))
  
  lon <- ncvar_get(r2_nc,"lon")
  lat <- ncvar_get(r2_nc,"lat")
  yld <- ncvar_get(r2_nc,"var")
  
  fillvalue <- ncatt_get(r2_nc,"var","_FillValue")
  
  nc_close(r2_nc) 
  
  yld[yld == fillvalue$value] <- NA
  
  lonlat <- as.matrix(expand.grid(lon,lat))
  
  yld_vec <- as.vector(yld)
  
  yld_dt <- data.table(cbind(lonlat,yld_vec))
  names(yld_dt) <- c("x","y","yield")
  
  yld_r <- rasterFromXYZ(yld_dt) 
  
  yld_r10 <- aggregate(yld_r,fact=2,fun=mean,na.rm=T)
  raster_mask <- mask(yld_r10,southeastasia)
  rm <- rasterToPoints(raster_mask)
  rm[,1:2] <- round(rm[,1:2],1)
  r2_dt <- data.table(rm)
  colnames(r2_dt) <- c("x","y","Rice.2_yield")
  r2_dt <- r2_dt[order(x,y)]
  
  
  # Maize
  m1_nc <- nc_open(paste0('Yields/maize_major/yield_',year,'.nc4'))
  
  lon <- ncvar_get(m1_nc,"lon")
  lat <- ncvar_get(m1_nc,"lat")
  yld <- ncvar_get(m1_nc,"var")
  
  fillvalue <- ncatt_get(m1_nc,"var","_FillValue")
  
  nc_close(m1_nc) 
  
  yld[yld == fillvalue$value] <- NA
  
  lonlat <- as.matrix(expand.grid(lon,lat))
  
  yld_vec <- as.vector(yld)
  
  yld_dt <- data.table(cbind(lonlat,yld_vec))
  names(yld_dt) <- c("x","y","yield")
  
  yld_r <- rasterFromXYZ(yld_dt) 
  
  yld_r10 <- aggregate(yld_r,fact=2,fun=mean,na.rm=T)
  raster_mask <- mask(yld_r10,southeastasia)
  rm <- rasterToPoints(raster_mask)
  rm[,1:2] <- round(rm[,1:2],1)
  m1_dt <- data.table(rm)
  colnames(m1_dt) <- c("x","y","Maize_yield")
  m1_dt <- m1_dt[order(x,y)]
  
  
  # Rice.2
  m2_nc <- nc_open(paste0('Yields/maize_second/yield_',year,'.nc4'))
  
  lon <- ncvar_get(m2_nc,"lon")
  lat <- ncvar_get(m2_nc,"lat")
  yld <- ncvar_get(m2_nc,"var")
  
  fillvalue <- ncatt_get(m2_nc,"var","_FillValue")
  
  nc_close(m2_nc) 
  
  yld[yld == fillvalue$value] <- NA
  
  lonlat <- as.matrix(expand.grid(lon,lat))
  
  yld_vec <- as.vector(yld)
  
  yld_dt <- data.table(cbind(lonlat,yld_vec))
  names(yld_dt) <- c("x","y","yield")
  
  yld_r <- rasterFromXYZ(yld_dt) 
  
  yld_r10 <- aggregate(yld_r,fact=2,fun=mean,na.rm=T)
  raster_mask <- mask(yld_r10,southeastasia)
  rm <- rasterToPoints(raster_mask)
  rm[,1:2] <- round(rm[,1:2],1)
  m2_dt <- data.table(rm)
  colnames(m2_dt) <- c("x","y","Maize.2_yield")
  m2_dt <- m2_dt[order(x,y)]
  
  
  yield_dt <- Reduce(function(...) merge(..., by=c("x","y"),all=T),list(r1_dt,r2_dt,m1_dt,m2_dt))
  
  yield_dt[,`:=`(Year=as.numeric(years_list[i]))]

  yield_list[[i]] <- yield_dt
  
}

yields_dt <- Reduce(rbind,yield_list)

save(yields_dt,file="yields.RData")



