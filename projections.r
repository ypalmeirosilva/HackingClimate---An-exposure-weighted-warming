#################################################
#################################################
# Projection datasets
#################################################
#################################################

# Possible datasets for temperature:
#################################################
# tas_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Mean air temperature at 1.5m °C - Salma
# tasmax_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Max air temperature at 1.5m °C - Marcin
# tasmin_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Min air temperature at 1.5m °C - Marcin

# Possible datasets for humidity:
#################################################
# hurs_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Relative humidity at 1.5m %

# Possible datasets for wind:
#################################################

# Question: How can we combine temp, humidity, and wind? (Temp+Humid = Heat Index)
# Precipitation - Wetter/Dryer


# Libraries we need
#################################################
library(raster)   
library(sf)
library(sp)
library(ncdf4)
library(ggplot2)
library(rgdal)
library(rlang)
library(dplyr)
library(lubridate)
library(zoo)


#################################################
# .nc to raster
#################################################

tasmin.nc <- nc_open("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/mon/tasmin_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc")
print(tasmin.nc)
names(tasmin.nc$var) #"tasmin"


tasmin <- brick("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/mon/tasmin_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc")
tasmin
# Information
#     class      : RasterBrick 
#     dimensions : 112, 82, 9184, 1200  (nrow, ncol, ncell, nlayers) -> 100 years
#     resolution : 12000, 12000  (x, y)
#     extent     : -216000, 768000, -108000, 1236000  (xmin, xmax, ymin, ymax)
#     crs        : +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +a=6377563.396 +b=6356256.909 +units=m +no_defs 
#     source     : tasmin_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc 
#     names      : X1980.10.20, X1980.11.19, X1980.12.19, X1981.01.18, X1981.02.17, X1981.03.19, X1981.04.18, X1981.05.18, X1981.06.17, X1981.07.17, X1981.08.16, X1981.09.15, X1981.10.15, X1981.11.14, X1981.12.14, ... 
#     Date/time  : 1980-10-20, 2079-04-14 (min, max)
#     varname    : tasmin 
#     level      : 1 

tasmax.nc <- nc_open("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/mon/tasmax_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc")
print(tasmax.nc)
names(tasmax.nc$var) #"tasmax"

tasmax <- brick("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/mon/tasmax_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc")
tasmax
# Information
#     class      : RasterBrick 
#     dimensions : 112, 82, 9184, 1200  (nrow, ncol, ncell, nlayers)
#     resolution : 12000, 12000  (x, y)
#     extent     : -216000, 768000, -108000, 1236000  (xmin, xmax, ymin, ymax)
#     crs        : +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +a=6377563.396 +b=6356256.909 +units=m +no_defs 
#     source     : tasmax_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc 
#     names      : X1980.10.20, X1980.11.19, X1980.12.19, X1981.01.18, X1981.02.17, X1981.03.19, X1981.04.18, X1981.05.18, X1981.06.17, X1981.07.17, X1981.08.16, X1981.09.15, X1981.10.15, X1981.11.14, X1981.12.14, ... 
#     Date/time  : 1980-10-20, 2079-04-14 (min, max)
#     varname    : tasmax 
#     level      : 1 

tas.nc <- nc_open("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/mon/tas_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc")
print(tas.nc)
names(tas.nc$var) #"tasmin"

tas <- brick("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/mon/tas_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc")
tas
# Information
#     class      : RasterBrick 
#     dimensions : 112, 82, 9184, 1200  (nrow, ncol, ncell, nlayers)
#     resolution : 12000, 12000  (x, y)
#     extent     : -216000, 768000, -108000, 1236000  (xmin, xmax, ymin, ymax)
#     crs        : +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +a=6377563.396 +b=6356256.909 +units=m +no_defs 
#     source     : tas_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc 
#     names      : X1980.10.20, X1980.11.19, X1980.12.19, X1981.01.18, X1981.02.17, X1981.03.19, X1981.04.18, X1981.05.18, X1981.06.17, X1981.07.17, X1981.08.16, X1981.09.15, X1981.10.15, X1981.11.14, X1981.12.14, ... 
#     Date/time  : 1980-10-20, 2079-04-14 (min, max)
#     varname    : tas 
#     level      : 1 


#################################################
# Some manipulations
#################################################

# Masking and cropping some grids
tasminM <- mask(tasmin, uk_borders) %>% crop(uk_borders)
tasmaxM <- mask(tasmax, uk_borders) %>% crop(uk_borders)
tasM <- mask(tas, uk_borders) %>% crop(uk_borders)

plot(tasminM[[1]])

# Standardized layers' dates - From .nc to rasterbrick there are some problems with layers' names. 
head(names(tasminM)) # "X1980.10.20" "X1980.11.19" "X1980.12.19" "X1981.01.18" "X1981.02.17" "X1981.03.19"
tail(names(tasminM)) # "X2078.11.15" "X2078.12.15" "X2079.01.14" "X2079.02.13" "X2079.03.15" "X2079.04.14"

time_layers <- seq(as.Date("1980/12/01"), as.Date("2080/11/30"), by = "month")
tasminM <- setZ(tasminM, time_layers, "months")
names(tasminM) <- time_layers

tasmaxM <- setZ(tasmaxM, time_layers, "months")
names(tasmaxM) <- time_layers

tasM <- setZ(tasM, time_layers, "months")
names(tasM) <- time_layers


#################################################
# Anomaly 
#################################################


# Calculate averages by month to get "historical" baseline - 1980 Dec (nl= 1) to 2000 Nov (nl= 240)
tasmin1980_2000 <- dropLayer(tasminM, c(241:1200))
tasmax1980_2000 <- dropLayer(tasmaxM, c(241:1200))
tas1980_2000 <- dropLayer(tasM, c(241:1200))

month_layers <- seq(as.Date("2000/12/1"), by = "month", length.out = 12)

tasminSA1980_2000  <-  zApply(tasmin1980_2000, by=month, fun= mean, name = "months") # Mean of 20 Jans, 20 Febs...20 Decs. It worked and stars from Dec-Nov
tasminSA1980_2000  <- setZ(tasminSA1980_2000, month_layers, "months")
names(tasminSA1980_2000) <- month_layers 

tasmaxSA1980_2000  <-  zApply(tasmax1980_2000, by=month, fun= mean, name = "months") 
tasmaxSA1980_2000 <- setZ(tasmaxSA1980_2000, month_layers, "months")
names(tasmaxSA1980_2000) <- month_layers 

tasSA1980_2000  <-  zApply(tas1980_2000, by=month, fun= mean, name = "months")
tasSA1980_2000  <- setZ(tasSA1980_2000, month_layers, "months")
names(tasSA1980_2000) <- month_layers

# For current temperature: We're using from 2000 onward (Dec 1999 (nl= 229))
# Drop layer we do not need (Dec 1980 to Nov 1999)
tasmin2000_2080 <- dropLayer(tasminM, c(1:228))
tasmax2000_2080 <- dropLayer(tasmaxM, c(1:228))
tas2000_2080 <- dropLayer(tasM, c(1:228))

# Now, I need to subtract current temp - historical temp by month

#AnomTime_layers <- as.character(seq(as.Date("1999/12/01"), as.Date("2080/11/30"), by = "month"))
#AnomMin2000_2080 <- brick(nrows=109, ncols=55, xmn=0, xmx=660000, ymn=-84000, ymx=1224000 , nl=972)
#names(AnomMin2000_2080) <- AnomTime_layers

#for(i in 1:2){
#  (subset(AnomMin2000_2080, which(substr(names(tasmin2000_2080),6,7) %in% c(i)))) <- (subset(tasmin2000_2080, which(substr(names(tasmin2000_2080),6,7) %in% c(i)))) - tasminSA1980_2000[[i]]
#}

# TasMin
TasMinAnomDec <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("12")))) - tasminSA1980_2000[[1]]
Dec_layers <- seq(as.Date("1999/12/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomDec) <- Dec_layers

TasMinAnomJan <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("01")))) - tasminSA1980_2000[[2]]
Jan_layers <- seq(as.Date("2000/01/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomJan) <- Jan_layers

TasMinAnomFeb <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("02")))) - tasminSA1980_2000[[3]]
Feb_layers <- seq(as.Date("2000/02/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomFeb) <- Feb_layers

TasMinAnomMar <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("03")))) - tasminSA1980_2000[[4]]
Mar_layers <- seq(as.Date("2000/03/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomMar) <- Mar_layers

TasMinAnomApr <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("04")))) - tasminSA1980_2000[[5]]
Apr_layers <- seq(as.Date("2000/04/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomApr) <- Apr_layers

TasMinAnomMay <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("05")))) - tasminSA1980_2000[[6]]
May_layers <- seq(as.Date("2000/05/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomMay) <- May_layers

TasMinAnomJun <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("06")))) - tasminSA1980_2000[[7]]
Jun_layers <- seq(as.Date("2000/06/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomJun) <- Jun_layers

TasMinAnomJul <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("07")))) - tasminSA1980_2000[[8]]
Jul_layers <- seq(as.Date("2000/07/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomJul) <- Jul_layers

TasMinAnomAug <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("08")))) - tasminSA1980_2000[[9]]
Aug_layers <- seq(as.Date("2000/08/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomAug) <- Aug_layers

TasMinAnomSep <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("09")))) - tasminSA1980_2000[[10]]
Sep_layers <- seq(as.Date("2000/09/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomSep) <- Sep_layers

TasMinAnomOct <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("10")))) - tasminSA1980_2000[[11]]
Oct_layers <- seq(as.Date("2000/10/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomOct) <- Oct_layers

TasMinAnomNov <- (subset(tasmin2000_2080, which(substr(getZ(tasmin2000_2080),6,7) %in% c("11")))) - tasminSA1980_2000[[12]]
Nov_layers <- seq(as.Date("2000/11/01"), as.Date("2080/11/30"), by = "year")
names(TasMinAnomNov) <- Nov_layers


TasMinAnom2000_2080 <- stack(TasMinAnomDec, TasMinAnomJan, TasMinAnomFeb, TasMinAnomMar, TasMinAnomApr, TasMinAnomMay,
                             TasMinAnomJun, TasMinAnomJul, TasMinAnomAug, TasMinAnomSep, TasMinAnomOct, TasMinAnomNov)


# TasMax
TasMaxAnomDec <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("12")))) - tasmaxSA1980_2000[[1]]
names(TasMaxAnomDec) <- Dec_layers

TasMaxAnomJan <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("01")))) - tasmaxSA1980_2000[[2]]
names(TasMaxAnomJan) <- Jan_layers

TasMaxAnomFeb <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("02")))) - tasmaxSA1980_2000[[3]]
names(TasMaxAnomFeb) <- Feb_layers

TasMaxAnomMar <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("03")))) - tasmaxSA1980_2000[[4]]
names(TasMaxAnomMar) <- Mar_layers

TasMaxAnomApr <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("04")))) - tasmaxSA1980_2000[[5]]
names(TasMaxAnomApr) <- Apr_layers

TasMaxAnomMay <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("05")))) - tasmaxSA1980_2000[[6]]
names(TasMaxAnomMay) <- May_layers

TasMaxAnomJun <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("06")))) - tasmaxSA1980_2000[[7]]
names(TasMaxAnomJun) <- Jun_layers

TasMaxAnomJul <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("07")))) - tasmaxSA1980_2000[[8]]
names(TasMaxAnomJul) <- Jul_layers

TasMaxAnomAug <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("08")))) - tasmaxSA1980_2000[[9]]
names(TasMaxAnomAug) <- Aug_layers

TasMaxAnomSep <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("09")))) - tasmaxSA1980_2000[[10]]
names(TasMaxAnomSep) <- Sep_layers

TasMaxAnomOct <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("10")))) - tasmaxSA1980_2000[[11]]
names(TasMaxAnomOct) <- Oct_layers

TasMaxAnomNov <- (subset(tasmax2000_2080, which(substr(getZ(tasmax2000_2080),6,7) %in% c("11")))) - tasmaxSA1980_2000[[12]]
names(TasMaxAnomNov) <- Nov_layers

TasMaxAnom2000_2080 <- stack(TasMaxAnomDec, TasMaxAnomJan, TasMaxAnomFeb, TasMaxAnomMar, TasMaxAnomApr, TasMaxAnomMay,
                             TasMaxAnomJun, TasMaxAnomJul, TasMaxAnomAug, TasMaxAnomSep, TasMaxAnomOct, TasMaxAnomNov)


# Tas
TasAnomDec <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("12")))) - tasSA1980_2000[[1]]
names(TasAnomDec) <- Dec_layers

TasAnomJan <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("01")))) - tasSA1980_2000[[2]]
names(TasAnomJan) <- Jan_layers

TasAnomFeb <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("02")))) - tasSA1980_2000[[3]]
names(TasAnomFeb) <- Feb_layers

TasAnomMar <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("03")))) - tasSA1980_2000[[4]]
names(TasAnomMar) <- Mar_layers

TasAnomApr <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("04")))) - tasSA1980_2000[[5]]
names(TasAnomApr) <- Apr_layers

TasAnomMay <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("05")))) - tasSA1980_2000[[6]]
names(TasAnomMay) <- May_layers

TasAnomJun <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("06")))) - tasSA1980_2000[[7]]
names(TasAnomJun) <- Jun_layers

TasAnomJul <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("07")))) - tasSA1980_2000[[8]]
names(TasAnomJul) <- Jul_layers

TasAnomAug <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("08")))) - tasSA1980_2000[[9]]
names(TasAnomAug) <- Aug_layers

TasAnomSep <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("09")))) - tasSA1980_2000[[10]]
names(TasAnomSep) <- Sep_layers

TasAnomOct <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("10")))) - tasSA1980_2000[[11]]
names(TasAnomOct) <- Oct_layers

TasAnomNov <- (subset(tas2000_2080, which(substr(getZ(tas2000_2080),6,7) %in% c("11")))) - tasSA1980_2000[[12]]
names(TasAnomNov) <- Nov_layers

TasAnom2000_2080 <- stack(TasAnomDec, TasAnomJan, TasAnomFeb, TasAnomMar, TasAnomApr, TasAnomMay,
                             TasAnomJun, TasAnomJul, TasAnomAug, TasAnomSep, TasAnomOct, TasAnomNov)


#################################################
# Creating averages by seasons
#################################################
# Winter (December-January-February, DJF); 
# Spring (March-April-May, MAM); 
# Summer (June-July-August, JJA);
# Autumn (September-October-November, SON)





# 2020 avg temp for winter... -> 2080

# Same thing for decades 







