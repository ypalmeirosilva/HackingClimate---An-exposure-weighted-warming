#################################################
#################################################
# Populations datasets
#################################################
#################################################

#########################################################################################
# Population projections
# Researchers from NCAR’s IAM group and the City University of New York Institute 
# for Demographic Research have developed a set of global, spatially explicit population 
# scenarios that are consistent with the new Shared Socioeconomic Pathways (SSPs). 
# The SSPs describe alternative future pathways of societal change that were developed to 
# facilitate global change research, with SSP5 corresponding most closely to the RCP8.5 scenario 
# modelled by the UKCP18 projections. A copy of the SSP5 population projections on the OSGB grid 
# is available on the data science platform at ~/UKCP18/population/. The original data was 
# produced on a slightly different grid, so we’d recommend using the prepared version.
#########################################################################################

pop.nc <- nc_open("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/population/population-projections-ssp5-total.nc")
print(pop.nc)
names(pop.nc$var) #"crs" "ssp"

pop <- brick("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/population/population-projections-ssp5-total.nc")
pop 
# Useful information:
#     class      : RasterBrick 
#     dimensions : 112, 82, 9184, 11  (nrow, ncol, ncell, nlayers)
#     resolution : 12000, 12000  (x, y)
#     extent     : -216000, 768000, -108000, 1236000  (xmin, xmax, ymin, ymax)
#     crs        : NA 
#     source     : population-projections-ssp5-total.nc 
#     names      : X20000101, X20100101, X20200101, X20300101, X20400101, X20500101, X20600101, X20700101, X20800101, X20900101, X21000101 
#     z-value    : 20000101, 20100101, 20200101, 20300101, 20400101, 20500101, 20600101, 20700101, 20800101, 20900101, 21000101 
#     varname    : ssp 

plot(popM[[1]])


#################################################
# Manipulation
#################################################

# Masking and cropping according to uk_borders
popM <- mask(pop, uk_borders) %>% crop(uk_borders)

# See population over time
pop_TS <- c(cellStats(popM, sum, na.rm=T))
pop_TS <- ts(pop_TS)
plot(pop_TS) # Population (absolute number) is +/- linear

# Now check the distribution of population over time
# First, I need to get the total pop (pop_TS)
# Then I need to: Dist = pop in each grid / pop_TS
popD2000_2100 <- brick(nrows=109, ncols=55, xmn=0, xmx=660000, ymn=-84000, ymx=1224000 , nl=11)

for (i in 1:nlayers(popM)){
  popD2000_2100[[i]] <- (popM[[i]]  / pop_TS[i] )
}

plot(popD2000_2100[[1]]) # proportion of people per grid cell

# We need to produce other layers for intermediate years
# First layer we have and will use = 2020 (nl = 3)
# Last layer we'll use = 2080 (nl = 9)

2020
2030
2040
2050
2060
2070
2080








