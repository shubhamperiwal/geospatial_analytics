
##Loading library packages
packages = c('shiny','leaflet','markdown','raster', 'rgdal', 'sf', 'sp', 'ClustGeo', 'spdep','tmap','readr','ggmap','spatstat','qdapTools','tidyverse','ggplot2','DT')
for (p in packages){ 
  if(!require(p, character.only = T)){
    install.packages(p) 
  }
  library(p,character.only = T) 
}

##Load Data into R
mpsz <- readOGR(dsn='data/mpsz', layer='MP14_SUBZONE_WEB_PL')
houses <- read.csv('data/resale-flat-prices/all_flats.csv')
busStops <- readOGR(dsn='data/BusStop', layer='BusStop')
parks <- st_read(dsn='data/parks', layer='NATIONALPARKS')
busStops.data <- busStops@data
busStops.data <- distinct(busStops@data, 'BUS_sTOP_N')
taxis <- readOGR(dsn='data/taxistand', layer='TaxiStop')
gpclinics <- read_csv('data/clinics/gpclinics.csv')
clinics <- readOGR(dsn="data/clinics/MOH_CHAS_CLINICS.kml", layer="MOH_CHAS_CLINICS")
ogrListLayers('data/mrtstation/lta-mrt-station-exit-kml.kml')
mrt <- readOGR(dsn='data/mrtstation/lta-mrt-station-exit-kml.kml',layer='MRT_EXITS')
pre_schools <- readOGR(dsn="data/schools/pre-schools-location-kml.kml", layer="PRESCHOOLS_LOCATION")
ogrListLayers(dsn='data/singapore-police-force-establishment/singapore-police-force-establishments-2018-kml.kml')
spfs <- readOGR(dsn='data/singapore-police-force-establishment/singapore-police-force-establishments-2018-kml.kml', layer='SPF_ESTABLISHMENTS_2018')
ogrListLayers(dsn='data/hawker-centres/hawker-centres-kml.kml')
hawkers <- readOGR(dsn='data/hawker-centres/hawker-centres-kml.kml', layer='HAWKERCENTRE')
schools <- read_csv('data/schools/schools.csv')


##Data preparation
busStops_sf <- st_as_sf(busStops)
busStops_sf <- st_transform(busStops_sf, 3414)

clinics_sf <- st_as_sf(gpclinics, coords = c("lon", "lat"))
clinics_sf <- st_set_crs(clinics_sf, 4326)
clinics_sf <- st_transform(clinics_sf, 3414)

hawkers_sf <- st_as_sf(hawkers)
hawkers_sf <- st_set_crs(hawkers_sf, 4326)
hawkers_sf <- st_transform(hawkers_sf, 3414)

mrts_sf <- st_as_sf(mrt)
mrts_sf <- st_set_crs(mrts_sf, 4326)
mrts_sf <- st_transform(mrts_sf, 3414)

schools_sf <- st_as_sf(schools, coords = c("lon", "lat"))
schools_sf <- st_set_crs(schools_sf, 4326)
schools_sf <- st_transform(schools_sf, 3414)

spfs_sf <- st_as_sf(spfs)
spfs_sf <- st_set_crs(spfs_sf, 4326)
spfs_sf <- st_transform(spfs_sf, 3414)

houses_sf <- st_as_sf(houses, coords = c("lon", "lat"))
hc_sf <- st_as_sf(hawkers)
hc_sf <- st_set_crs(hc_sf, 4326)
hc_sf <- st_transform(hc_sf, 3414)

taxis_sf <- st_as_sf(taxis)
st_crs(taxis_sf) <- 4757
proj4string(as(taxis_sf, 'Spatial'))
st_crs(taxis_sf)
taxis_sf <- st_set_crs(taxis_sf, 4326)
taxis_sf <- st_transform(taxis_sf, 3414)





##Find Distance matrix
houses_group <- read_csv('data/resale-flat-prices/all_flats_grouped.csv')
houses_group_sf <- st_as_sf(houses_group, coords = c("lon", "lat"))
min_clinic <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_clinic))
min_hawker <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_hawker))
min_MRT <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_MRT))
min_school <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_school))






