#################################################
#################################################
# Shapefiles
#################################################
#################################################

# We need:
# ukcp18-uk-land-country-hires 
# contains the borders of the UK, and the internal borders between Wales, England and Scotland

uk_borders <- st_read("/nfs/cfs/home4/rejb/rejbypa/UKCP18_R/ukcp-spatial-files/spatial-files/ukcp18-uk-land-country-hires/ukcp18-uk-land-country-hires.shp")
as_Spatial(uk_borders)
plot(uk_borders)
