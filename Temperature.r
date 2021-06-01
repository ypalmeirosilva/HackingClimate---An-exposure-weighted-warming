#################################################
# Projection datasets
#################################################

# Possible datasets for temperature:
#################################################
# tas_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Mean air temperature at 1.5m °C
# tasmax_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Max air temperature at 1.5m °C
# tasmin_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Min air temperature at 1.5m °C

# Possible datasets for humidity:
#################################################
# hurs_rcp85_land-rcm_uk_12km_01_mon_198012-208011.nc --> Relative humidity at 1.5m %

# Possible datasets for wind:
#################################################

# Question: How can we combine temp, humidity, and wind? (Temp+Humid = Heat Index)


# Libraries we need
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

tasmin <- ncvar_get(tasmin.nc, "tasmin")
dim(tasmin) # Monthly UKCP18 data is stored in arrays of 82 · 112 · 1200, where the dimensions are eastings · northings · date.

