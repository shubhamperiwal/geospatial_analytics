# Created by XccessPoint
#
#
library(shiny)
library(raster)
library(rgdal)
library(sf)
library(sp)
library(ClustGeo)
library(spdep)
library(tmap)
library(readr)
library(ggmap)
library(spatstat)
library(qdapTools)
library(tidyverse)
library(ggplot2)
library(DT)
library(leaflet)
library(markdown)

mpsz <- readOGR(dsn='data/mpsz', layer='MP14_SUBZONE_WEB_PL')
houses <- read.csv('data/resale-flat-prices/all_flats.csv')
busStop <- readOGR(dsn='data/BusStop', layer='BusStop')
parks <- st_read(dsn='data/parks', layer='NATIONALPARKS')
busStop.data <- busStop@data
busStop.data <- distinct(busStop@data, 'BUS_sTOP_N')
taxiStop <- readOGR(dsn='data/taxistand', layer='TaxiStop')
clinics <- readOGR(dsn="data/clinics/MOH_CHAS_CLINICS.kml", layer="MOH_CHAS_CLINICS")
houses_sf <- st_as_sf(houses, coords = c("lon", "lat"))
mrt_station <- readOGR(dsn="data/mrtstation/lta-mrt-station-exit-kml.kml", layer="MRT_EXITS")
pre_schools <- readOGR(dsn="data/schools/pre-schools-location-kml.kml", layer="PRESCHOOLS_LOCATION")
singapore_police <- readOGR(dsn="data/singapore-police-force-establishment/singapore-police-force-establishments-2018-kml.kml", layer="SPF_ESTABLISHMENTS_2018")
ogrListLayers("data/hawker-centres/hawker-centres-kml.kml")
hawker_centres <- readOGR(dsn="data/hawker-centres/hawker-centres-kml.kml", layer='HAWKERCENTRE')
hc_sf <- st_as_sf(hawker_centres)
hc_sf <- st_set_crs(hc_sf, 4326)
hc_sf <- st_transform(hc_sf, 3414)
houses_group <- read_csv('data/resale-flat-prices/all_flats_grouped.csv')
houses_group_sf <- st_as_sf(houses_group, coords = c("lon", "lat"))
min_clinic <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_clinic))
min_hawker <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_hawker))
min_MRT <- houses_group_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_MRT))
min_school <- houses_grouped_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_school))


# Define UI ----
ui <- navbarPage(
  title=("XccessPoint"),
  tabPanel("Our Project",   
           img(src = "XccessPoint_Logo.png", height = 100, width = 150, align="left"),
           h1("Project XccessPoint"),
           p("To analyse and determine how these facilities such as transportation, school and healthcare services would impact the accessibility level around HDB. "), 
           br(),
           br(),
           h2("Project Motivation"),
           p("- Social inequality has been the hot topic in recent years as government start to find sustainable ways to tackle the increasing inequality and stratification in Singapore.However, with constant development and improving infrastructure around Singapore, the impact on accessibility has not really been research upon.")
           # p("- Shiny applications are automatically 'live' in the same way that ", 
           #   strong("spreadsheets"),
           #   " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
  ),
  tabPanel("Acessibility",
           sidebarLayout(
             sidebarPanel(  
               fluidRow(
                 column(10,
                        radioButtons("radio", h3("Display map by:"),
                                     choices = list("Default" = "default", 
                                                    "Bus Stop" = "busStop",
                                                    "Taxi" = "taxiStop",
                                                    "Singapore Police Force" = "singapore_police",
                                                    "Pre-School" = "pre_schools",
                                                    "MRT-Station" = "mrt_station",
                                                    "Clinic"= "clinics",
                                                    "Houses" = "houses",
                                                    "Hawker" = "Hawker"),
                                     selected = "default"))
               )),
             mainPanel(
               leafletOutput("allPlot", height = 400, width = 600),
               #plotOutput("SHPplot", height = 400, width = 600),
               br(),
               h5("Data table information"),
               DT::dataTableOutput("table")
             ))
  ),
  tabPanel("View Accessibility Scores",
           sidebarLayout(
             sidebarPanel(  
               fluidRow(
                 column(10,
                        radioButtons("test", h3("Display map by:"),
                                     choices = list("Default" = "default", 
                                                    "Clinic"= "clinics",
                                                    "MRT-Station" = "mrt_station",
                                                    "Pre-School" = "pre_schools",
                                                    "Hawker" = "hawker"),
                                     selected = "default"))
               )),
             mainPanel(
               leafletOutput("xscore", height = 400, width = 600),
               br(),
               plotOutput("barchart", height = 300, width = 600)
             ))
  )
)

# Define server logic ----
server <- function(input, output) {
  # output$SHPplot <-  renderPlot({
  #   tm_shape(mpsz) + tm_polygons() +
  #     tm_shape(busStop) + tm_bubbles() +
  #     tm_shape(houses_sf) + tm_dots(col='flat_type')+
  #     tm_shape(parks) + tm_dots()
  # })
  
  # output$allPlot <-  renderLeaflet({
  #   map <- tm_shape(mpsz) + tm_polygons() +
  #     tm_shape(busStop) + tm_bubbles() +
  #     tm_shape(houses_grouped_sf) + tm_dots(col='min_hawker',style='quantile',size=0.01)+
  #     tm_shape(houses_sf) + tm_dots(col='flat_type')+
  #     tm_shape(parks) + tm_dots()
  #   tmap_leaflet(map)
  # })
  # mydata <- as.data.frame(houses)
  # output$table <- DT::renderDataTable(DT::datatable({mydata}))
  
  ### For Accessibility Score###
  output$xscore <-  renderLeaflet({
    if(input$test == "clinics"){
      mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_group_sf) +tm_dots(col='min_clinic',style='quantile', size=0.01)
    }else if(input$test == "mrt_station"){
      mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_group_sf) +tm_dots(col='min_MRT',style='quantile', size=0.01)
    }else if(input$test == "pre_schools"){
      mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_grouped_sf) +tm_dots(col='min_school',style='quantile',size=0.01)
    }else if(input$test == "hawker"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(houses_grouped_sf) + tm_dots(col='min_hawker',style='quantile',size=0.01)
    }else{
      mydata <- tm_shape(mpsz) + tm_polygons()
    }
    tmap_leaflet(mydata)
  })
  
  # ### For Barchart ###
  output$barchart <-  renderPlot({
    if(input$test == "clinics"){
      plot(ggplot(min_clinic, aes(x = SUBZONE_N, mean_dist)) +
             geom_col() +
             theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    }else if(input$test == "mrt_station"){
      plot(ggplot(min_MRT, aes(x = SUBZONE_N, mean_dist)) +
             geom_col() +
             theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    }else if(input$test == "pre_schools"){
      plot(ggplot(min_school, aes(x = SUBZONE_N, mean_dist)) +
             geom_col() +
             theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    }else if(input$test == "hawker"){
      plot(ggplot(min_hawker, aes(x = SUBZONE_N, mean_dist)) +
             geom_col() +
             theme(axis.text.x = element_text(angle = 90, hjust = 1)))
    }else{
    }
  })
  
  ### For Initial Plot###
  output$allPlot <-  renderLeaflet({
    if(input$radio == "busStop"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(busStop) + tm_dots(size = 0.000001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "taxiStop"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(taxiStop) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "singapore_police"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(singapore_police) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "clinics"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(clinics) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "mrt_station"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(mrt_station) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "pre_schools"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(pre_schools) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "houses"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(houses_sf) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "Hawker"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(hawker_centres) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else{
      mydata <- tm_shape(mpsz) + tm_polygons() + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }
    tmap_leaflet(mydata)
  })
  ### Data table for Initial Plot###
  output$table <- DT::renderDataTable({
    if(input$radio == "busStop"){
      mydata <- as.data.frame(busStop)
    }else if(input$radio == "taxiStop"){
      mydata <- as.data.frame(taxiStop)
    }else if(input$radio == "singapore_police"){
      mydata <- as.data.frame(singapore_police)
    }else if(input$radio == "clinics"){
      mydata <- as.data.frame(clinics)
    }else if(input$radio == "mrt_station"){
      mydata <- as.data.frame(mrt_station)
    }else if(input$radio == "pre_schools"){
      mydata <- as.data.frame(pre_schools)
    }else if(input$radio == "houses"){
      mydata <- as.data.frame(houses)
    }else if(input$radio == "Hawker"){
      mydata <- as.data.frame(hawker_centres)
    }else{
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)