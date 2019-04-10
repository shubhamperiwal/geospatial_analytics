#Loading library packages
packages = c('shiny','leaflet','ahp','ahpsurvey','markdown','httr','raster', 'rgdal', 'sf', 'sp', 'ClustGeo', 'spdep','tmap','readr','ggmap','spatstat','qdapTools','tidyverse','ggplot2','DT', 'shinythemes')
for (p in packages){ 
  if(!require(p, character.only = T)){
    install.packages(p) 
  }
  library(p,character.only = T) 
}

ogrListLayers(dsn='data/mp_region/MP14_REGION_WEB_PL.kml')

##Load Data into R
mpsz <- readOGR(dsn='data/mpsz', layer='MP14_SUBZONE_WEB_PL')
mpr <- readOGR(dsn='data/mp_region/MP14_REGION_WEB_PL.kml', layer='MP14_REGION_WEB_PL')
mpa <- readOGR(dsn='data/mp_planning_area/MP14_PLNG_AREA_WEB_PL.kml', layer='MP14_PLNG_AREA_WEB_PL')

schools <- read_csv('data/schools/schools.csv')
busStops <- read_csv('data/busStops/bus-stops.csv')
gpclinics <- read_csv('data/clinics/clinics.csv')
hawkers <- read_csv('data/hawker-centres/hawker-centres.csv')
mrt <- read_csv('data/mrtstation/mrt.csv')
spfs <- read_csv('data/singapore-police-force-establishment/spf.csv')
houses <- read_csv('data/resale-flat-prices/finalCSVforplots.csv')


##Data preparation
busStops_sf <- st_as_sf(busStops, coords=c("lon", "lat"))
busStops_sf <- st_set_crs(busStops_sf, 4326)
busStops_sf <- st_transform(busStops_sf, 3414)

clinics_sf <- st_as_sf(gpclinics, coords = c("lon", "lat"))
clinics_sf <- st_set_crs(clinics_sf, 4326)
clinics_sf <- st_transform(clinics_sf, 3414)

hawkers_sf <- st_as_sf(hawkers, coords=c("lon", "lat"))
hawkers_sf <- st_set_crs(hawkers_sf, 4326)
hawkers_sf <- st_transform(hawkers_sf, 3414)

mrts_sf <- st_as_sf(mrt, coords=c("lon", "lat"))
mrts_sf <- st_set_crs(mrts_sf, 4326)
mrts_sf <- st_transform(mrts_sf, 3414)

schools_sf <- st_as_sf(schools, coords = c("lon", "lat"))
schools_sf <- st_set_crs(schools_sf, 4326)
schools_sf <- st_transform(schools_sf, 3414)

spfs_sf <- st_as_sf(spfs, coords=c("lon", "lat"))
spfs_sf <- st_set_crs(spfs_sf, 4326)
spfs_sf <- st_transform(spfs_sf, 3414)

houses_sf <- st_as_sf(houses, coords = c("lon", "lat"))
houses_sf <- st_set_crs(houses_sf, 4326)
houses_sf <- st_transform(houses_sf, 3414)




#AHP Calculation

facilities <- c('busStop', 'clinics','hawkers','mrt','schools','spf')

facility_sf_vector <- vector(mode="list", length=6)
names(facility_sf_vector) <- facilities

facility_sf_vector[[1]] <- busStops_sf
facility_sf_vector[[2]] <- clinics_sf
facility_sf_vector[[3]] <- hawkers_sf
facility_sf_vector[[4]] <- mrts_sf
facility_sf_vector[[5]] <- schools_sf
facility_sf_vector[[6]] <- spfs_sf

facility_dist_vector <- vector(mode="list", length=6)
names(facility_dist_vector) <- facilities

facility_dist_vector[[1]] <- "min_dist_busStop"
facility_dist_vector[[2]] <- "min_dist_clinic"
facility_dist_vector[[3]] <- "min_dist_hawker"
facility_dist_vector[[4]] <- "min_dist_mrt"
facility_dist_vector[[5]] <- "min_dist_school"
facility_dist_vector[[6]] <- "min_dist_spf"


weight_b_c <- 0 #Weight busstop_clinic. Bus stop is much higher priority than clinic
weight_b_h <- 0
weight_b_m <- 0
weight_b_sc <- 0
weight_b_sp <- 0
weight_c_h <- 0
weight_c_m <- 0
weight_c_sc <- 0
weight_c_sp <- 0
weight_h_m <- 0
weight_h_sc <- 0
weight_h_sp <- 0
weight_m_sc <- 0
weight_m_sp <- 0
weight_sc_sp <- 0

#Breaks for distance
breaks_fac <- c(0, 100 ,200, 300, 400, 500, 600, 700, 800, 8200)

#Add colour palette
colour_palette <- 'OrRd'

#Dot Plot
theme_dotplot <- 
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        panel.background = element_rect(fill = "#e4e9ed",
                                        size = 0.25, linetype = "solid"),
        panel.grid.major = element_line(size = 0.3, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid',
                                        colour = "white"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

busicon <- tmap_icons(file = 'ICONs/bus.png', width = 48, height = 48, 
                      keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

clinicicon <- tmap_icons(file = 'ICONs/hospital.png', width = 48, height = 48, 
                         keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

hawkericon <- tmap_icons(file = 'ICONs/hawker.png', width = 48, height = 48, 
                         keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

spficon <- tmap_icons(file = 'ICONs/police.png', width = 48, height = 48, 
                      keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

schoolicon <- tmap_icons(file = 'ICONs/school.png', width = 48, height = 48, 
                         keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

mrticon <- tmap_icons(file = 'ICONs/train.png', width = 48, height = 48, 
                      keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

houseicon <- tmap_icons(file = 'ICONs/house.png', width = 48, height = 48, 
                      keep.asp = TRUE, just = c("center", "center"), as.local = TRUE)

#Icon Vector
facility_icon_vector <- vector(mode="list", length=6)
names(facility_icon_vector) <- facilities

facility_icon_vector[[1]] <- busicon
facility_icon_vector[[2]] <- clinicicon
facility_icon_vector[[3]] <- hawkericon
facility_icon_vector[[4]] <- mrticon
facility_icon_vector[[5]] <- schoolicon
facility_icon_vector[[6]] <- spficon