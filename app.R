# Created by XccessPoint

source('global.R', local = TRUE)

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
                                                    "Schools" = "pre_schools",
                                                    "MRT-Station" = "mrt_station",
                                                    "Clinic"= "clinics",
                                                    "Houses" = "houses",
                                                    "Hawker" = "Hawker"),
                                     selected = "default"))
               )),
             mainPanel(
               leafletOutput("allPlot", height = 400, width = 600),
               br(),
               h5("Data table information"),
               DT::dataTableOutput("table", width="100%")
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
                                                    "Schools" = "pre_schools",
                                                    "Hawker" = "hawker"),
                                     selected = "default"))
               )),
             mainPanel(
               column(6,leafletOutput("xscore", height = 400, width = "100%")),
               column(6,plotOutput("barchart", height = 400, width = "100%"))
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
      mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_group_sf) +tm_dots(col='min_clinic',style='quantile', size=0.01)+
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$test == "mrt_station"){
      mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_group_sf) +tm_dots(col='min_MRT',style='quantile', size=0.01)+
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$test == "pre_schools"){
      mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_group_sf) +tm_dots(col='min_school',style='quantile',size=0.01)+
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$test == "hawker"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(houses_group_sf) + tm_dots(col='min_hawker',style='quantile',size=0.01)+
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else{
      mydata <- tm_shape(mpsz) + tm_polygons() + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
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
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(busStops) + tm_dots(size = 0.000001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "taxiStop"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(taxiStop) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "singapore_police"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(spfs) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "clinics"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(clinics) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "mrt_station"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(mrt) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "pre_schools"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(pre_schools) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "houses"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(houses_sf) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "Hawker"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(hawkers) + tm_dots(size = 0.01, col = "#7d4627") + 
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
      mydata <- as.data.frame(busStops_sf)
    }else if(input$radio == "taxiStop"){
      mydata <- as.data.frame(taxis_sf)
    }else if(input$radio == "singapore_police"){
      mydata <- as.data.frame(spfs_sf)
    }else if(input$radio == "clinics"){
      mydata <- as.data.frame(clinics_sf)
    }else if(input$radio == "mrt_station"){
      mydata <- as.data.frame(mrts_sf)
    }else if(input$radio == "pre_schools"){
      mydata <- as.data.frame(schools_sf)
    }else if(input$radio == "houses"){
      mydata <- as.data.frame(houses)
    }else if(input$radio == "Hawker"){
      mydata <- as.data.frame(hawkers_sf)
    }else{
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)