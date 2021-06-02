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


# Drop layer we do not need (1980-12 to 2020-11)
tasminM <- dropLayer(tasminM, c(1:480))

s <- dropLayer(s, c(3, 5))

#################################################
# Creating averages by seasons
#################################################
# Winter (December-January-February, DJF); 
# Spring (March-April-May, MAM); 
# Summer (June-July-August, JJA);
# Autumn (September-October-November, SON)

names(tasminM)

pig <- (tasminM[[1]] + tasminM[[2]] + tasminM[[3]])/3
plot(pig)




# 2020 avg temp for winter... -> 2080

# Same thing for decades 







