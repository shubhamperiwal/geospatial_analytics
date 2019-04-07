# Created by XccessPoint

source('global.R', local = TRUE)

# Define UI ----
<<<<<<< HEAD
ui <- fluidPage(theme = shinytheme("cosmo"),
  navbarPage(
    title=("XccessPoint"),
    tabPanel(" Info",
      withTags(
        div(class = "Issue_problem",
            style = "width: 50%; margin-top:-35px;",
            img(src = "XccessPoint_Logo.png", height = 350, width = 600,style="position:fixed;top: 500;right: 0;"),
            h3("Issues and Problem"),
            p("Our team's objective is to analyse and determine how these facilities such as transportation, 
              school and healthcare services would impact the accessibility level around HDB.")
        )
      ),
      tags$hr(align="left",width="50%"),
      withTags(
        div(class = "Motivation",
            h3("Motivation"),
            style = "width: 50%;",
            p("The government has been finding sustainable ways to tackle the increasing inequality and stratification 
              in Singapore. With constant development and improvement of infrastructure around Singapore, the impact on 
              accessibility has not really been research upon.")
        )
      ),
      tags$hr(align="left",width="50%"),
      withTags(
        div(class = "Team_approach",
            style = "width: 50%;",
            h3("Team Approach"),
            tags$ol(
              tags$li("Data Preparation:"),
              tags$ul(
                tags$li(" Convert the address into langtitude and longtitude"), 
                tags$li("Transform all spatial reference system to WGS 84")
              ),
              tags$li("Set range of accessibility To avoid calculating distance between all facilities and houses, "), 
              tags$li("Euclidean distance calculation Used to find the closest facility of each HDB and give a score for accessibility."),
              tags$li("User Interaction User can choose the accessibility they wish to observe and the appliaction will display the maps 
              and graph to them")
            )
        )
      ),
      tags$hr(align="left",width="50%"),
      withTags(
        div(class = "Future_work",
            h3("Future Work"),
            style = "width: 50%;",
            p("Our  can be expanded through the following:"),
            tags$ol(
              tags$li("Allowing user to upload their own sets of data for analysis on the impact of 
              accessibilities in Singapore or other parts of the world."),
              tags$li("Allow adding of a single spatial point on the map and see how the point would impact
              the accessibility across Singapore.")
            )
        )
      )
      
    ),
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(  
                 fluidRow(
                   h3("Filter your output")
                 ),
                 br(),
                 fluidRow(
                   column(10,
                          radioButtons("radio", label = "Select Facility to plot:",
                                       choices = list("Default" = "default", 
                                                      "Bus Stop" = "busStop",
                                                      "Clinic"= "clinics",
                                                      "MRT-Station" = "mrt",
                                                      "Hawker" = "hawkers",
                                                      "Schools" = "schools",
                                                      "Singapore Police Force" = "spf",
                                                      "Houses" = "houses"),
                                       selected = "default"))
                 ),width=3),
               mainPanel(
                   fluidRow(
                     column(10,h3("Facilities Spatial Distribution and Chrolopleth Map"))
                   ),
                   fluidRow(
                     column(10,h4("Select facility to plot it's location and quantity per subzone"))
                   ),
                 fluidRow(
                   column(6,leafletOutput("allPlot", height = 500, width = "100%")),
                   column(6,leafletOutput("cmap", height = 500, width = "100%"))
=======
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
  tabPanel("Data",
           sidebarLayout(
             sidebarPanel(  
               fluidRow(
                 column(10,
                        radioButtons("radio", h3("Display map by:"),
                                     choices = list("Default" = "default", 
                                                    "Bus Stop" = "busStop",
                                                    "Singapore Police Force" = "singapore_police",
                                                    "Schools" = "school",
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
  tabPanel("Accessbility",
           sidebarLayout(
             sidebarPanel(  
               fluidRow(
                 selectInput("test", label="Select Facility Type:",
                             choices = list("All" = "all", 
                                            "Bus Stop" = "busStop",
                                            "Singapore Police Force" = "spf",
                                            "Schools" = "schools",
                                            "MRT-Station" = "mrt",
                                            "Clinic"= "clinics",
                                            "Hawker" = "hawkers"),
                             selected = "all")
               ),
               br(),
               fluidRow(
                 selectInput("region", 
                             label = "Select Region:",
                             choices = list("WHOLE SINGAPORE",
                                            "NORTH REGION", 
                                            "CENTRAL REGION",
                                            "NORTH-EAST REGION", 
                                            "WEST REGION",
                                            "EAST REGION"),
                             selected = "WHOLE SINGAPORE")
               ),
               br(),
               fluidRow(
                 conditionalPanel(
                   condition = "input.test != 'all'",
                   selectInput("upperLimit", "Select BarChart Upper Limit", 
                               choices = list("5" = 5, "10" = 10,
                                              "15" = 15,"20" = 20,"30"=30,"50"=50), selected = 15)
>>>>>>> 4e1b5fb6490adfdcb0c8d3f4aad55366f3ad4b00
                 )
               ),
               br(),
               fluidRow(
                 conditionalPanel(
                   condition = "input.region != 'WHOLE SINGAPORE'",
                   selectInput("type", 
                               label = "View by",
                               choices = list("Region",
                                              "Subzone",
                                              "Planning Area"),
                               selected= "Region"))
               ),
               br(),
               fluidRow(
                 conditionalPanel(
                   condition = "input.region != 'WHOLE SINGAPORE'",
                   uiOutput("cc")
                 )
               )
               ,width=2),
             mainPanel(
               fluidRow(
                 column(6,leafletOutput("xscore", height = 400, width = "100%")),
                 column(6,
                        conditionalPanel(
                          condition = "input.test !='all'",
                          plotOutput("planning_area_barchart", height = 350, width = "100%"))
                 ),
                 column(6,
                        conditionalPanel(
                          condition = "input.test == 'all'",
                          uiOutput("slide1"))
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        conditionalPanel(
                          condition = "input.test !='all'",
                          plotOutput("sz_barchart", height = 350, width = "100%"))
                 ),
                 column(12,
                        conditionalPanel(
                          condition = "input.test =='all'",
                          plotOutput("AHP", height = 350, width = "100%"))
                 )
               )
               ,width=10)
           )
           
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$cc <- renderUI({
    if(input$type == "Subzone"){
      c <- houses_sf[houses_sf$REGION_N==input$region, ]
      selectInput("userinput", "Select Subzone:",  choices = c$SUBZONE_N,selected = "")
    }else if(input$type=="Planning Area"){
      c <- houses_sf[houses_sf$REGION_N==input$region, ]
      selectInput("userinput", "Select Planning Area:",  choices = c$PLN_AREA_N,selected = "")
    }else{
      
    }
  })
  
  output$slide1 <- renderUI({
    tagList(
      fluidRow(
        column(h3("Enter AHP Criteria (-9 to 9)"),width=12)
      ),
      fluidRow(
        column(numericInput("bs_clinic", "Bus stop - Clinic:", min=-9, max=9, value=9, step=1),width=4),
        column(numericInput("clinic_hawker", "Clinic - Hawker:", min=-9, max=9, value=7, step=1),width=4),
        column(numericInput("hawker_school", "Hawker - School:", min=-9, max=9, value=5, step=1),width=4)
      ),
      fluidRow(
        column(numericInput("bs_hawker", "Bus stop - Hawker:", min=-9, max=9, value=3, step=1),width=4),
        column(numericInput("clinic_mrt", "Clinic - MRT:", min=-9, max=9, value=1, step=1),width=4),
        column(numericInput("hawker_spf", "Hawker - Police Post:", min=-9, max=9, value=-3, step=1),width=4)
      ),
      fluidRow(
        column(numericInput("bs_mrt", "Bus stop - MRT:", min=-9, max=9, value=-4, step=1),width=4),
        column(numericInput("clinic_school", "Clinic - School:", min=-9, max=9, value=-7, step=1),width=4),
        column(numericInput("mrt_school", "MRT - School:", min=-9, max=9, value=-9, step=1),width=4)
      ),
      fluidRow(
        column(numericInput("bs_school", "Bus stop - School:", min=-9, max=9, value=-3, step=1),width=4),
        column(numericInput("clinic_spf", "Clinic - Police Post:", min=-9, max=9, value=-5, step=1),width=4),
        column(numericInput("mrt_spf", "MRT - Police Post:", min=-9, max=9, value=-6, step=1),width=4)
      ),
      fluidRow(
        column(numericInput("bs_spf", "Bus stop - Police Post:", min=-9, max=9, value=-7, step=1),width=4),
        column(numericInput("hawker_mrt", "Hawker - MRT:", min=-9, max=9, value=-9, step=1),width=4),
        column(numericInput("school_spf", "School - Police Post:", min=-9, max=9, value=-3, step=1),width=4)
      )
    )
  })
  
  
  
  ##Display map based on user input
  output$xscore <-  renderLeaflet({
    
    if(input$test == "all"){
      ##load AHP Weight
      weight_b_c <- input$bs_clinic 
      weight_b_h <- input$bs_hawker
      weight_b_m <- input$bs_mrt
      weight_b_sc <- input$bs_school
      weight_b_sp <- input$bs_spf
      weight_c_h <- input$clinic_hawker
      weight_c_m <- input$clinic_mrt
      weight_c_sc <- input$clinic_school
      weight_c_sp <- input$clinic_spf
      weight_h_m <- input$hawker_mrt
      weight_h_sc <- input$hawker_school
      weight_h_sp <- input$hawker_spf
      weight_m_sc <- input$mrt_school
      weight_m_sp <- input$mrt_spf
      weight_sc_sp <- input$school_spf
      
      if(input$region == "WHOLE SINGAPORE" ){
        df <- data.frame(c(weight_b_c, weight_b_h, weight_b_m, weight_b_sc, weight_b_sp,
                           weight_c_h, weight_c_m, weight_c_sc, weight_c_sp, 
                           weight_h_m, weight_h_sc, weight_h_sp,
                           weight_m_sc, weight_m_sp,
                           weight_sc_sp))
        transpose <- as.data.frame(t(df))
        
        colnames(transpose) <- c("b_c", "b_h", "b_m", "b_sc", "b_sp",
                                 "c_h", "c_m", "c_sc", "c_sc",
                                 "h_m", "h_sc", "h_sp",
                                 "m_sc", "m_sp",
                                 "sc_sp")
        rownames(transpose) <- "row"
        
        ahp_mat <- transpose %>% ahp.mat(df = transpose, atts = facilities, negconvert = T) %>% head(3)
        priorities <- ahp.indpref(ahp_mat, facilities)
        
        houses_sf$ahp<- 
          houses_sf$min_dist_busStop*priorities$busStop + houses_sf$min_dist_clinic*priorities$clinics +
          houses_sf$min_dist_hawker*priorities$hawkers  + houses_sf$min_dist_mrt*priorities$mrt +
          houses_sf$min_dist_school*priorities$schools  + houses_sf$min_dist_spf*priorities$spf
        
        mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(houses_sf) +
          tm_dots(col='ahp',style='quantile',size=0.01)+
          tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
      }else{
        if(input$type == "Region"){
          df <- data.frame(c(weight_b_c, weight_b_h, weight_b_m, weight_b_sc, weight_b_sp,
                             weight_c_h, weight_c_m, weight_c_sc, weight_c_sp, 
                             weight_h_m, weight_h_sc, weight_h_sp,
                             weight_m_sc, weight_m_sp,
                             weight_sc_sp))
          transpose <- as.data.frame(t(df))
          
          colnames(transpose) <- c("b_c", "b_h", "b_m", "b_sc", "b_sp",
                                   "c_h", "c_m", "c_sc", "c_sc",
                                   "h_m", "h_sc", "h_sp",
                                   "m_sc", "m_sp",
                                   "sc_sp")
          rownames(transpose) <- "row"
          
          ahp_mat <- transpose %>% ahp.mat(df = transpose, atts = facilities, negconvert = T) %>% head(3)
          priorities <- ahp.indpref(ahp_mat, facilities)
          
          houses_sf$ahp<- 
            houses_sf$min_dist_busStop*priorities$busStop + houses_sf$min_dist_clinic*priorities$clinics +
            houses_sf$min_dist_hawker*priorities$hawkers  + houses_sf$min_dist_mrt*priorities$mrt +
            houses_sf$min_dist_school*priorities$schools  + houses_sf$min_dist_spf*priorities$spf
          
          
          mydata <- tm_shape(mpr[mpr$REGION_N==input$region, ]) + tm_polygons() + tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) +
            tm_dots(col='ahp',style='quantile',size=0.01) +
            tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor") 
        }else if(input$type =="Subzone"){
          df <- data.frame(c(weight_b_c, weight_b_h, weight_b_m, weight_b_sc, weight_b_sp,
                             weight_c_h, weight_c_m, weight_c_sc, weight_c_sp, 
                             weight_h_m, weight_h_sc, weight_h_sp,
                             weight_m_sc, weight_m_sp,
                             weight_sc_sp))
          transpose <- as.data.frame(t(df))
          
          colnames(transpose) <- c("b_c", "b_h", "b_m", "b_sc", "b_sp",
                                   "c_h", "c_m", "c_sc", "c_sc",
                                   "h_m", "h_sc", "h_sp",
                                   "m_sc", "m_sp",
                                   "sc_sp")
          rownames(transpose) <- "row"
          
          ahp_mat <- transpose %>% ahp.mat(df = transpose, atts = facilities, negconvert = T) %>% head(3)
          priorities <- ahp.indpref(ahp_mat, facilities)
          
          houses_sf$ahp<- 
            houses_sf$min_dist_busStop*priorities$busStop + houses_sf$min_dist_clinic*priorities$clinics +
            houses_sf$min_dist_hawker*priorities$hawkers  + houses_sf$min_dist_mrt*priorities$mrt +
            houses_sf$min_dist_school*priorities$schools  + houses_sf$min_dist_spf*priorities$spf
          
          
          mydata <- tm_shape(mpsz[mpsz$SUBZONE_N==input$userinput, ]) + tm_polygons() + tm_shape(houses_sf[houses_sf$SUBZONE_N==input$userinput, ]) +
            tm_dots(col='ahp',style='quantile',size=0.01) +
            tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
          
        }else if(input$type == "Planning Area"){
          df <- data.frame(c(weight_b_c, weight_b_h, weight_b_m, weight_b_sc, weight_b_sp,
                             weight_c_h, weight_c_m, weight_c_sc, weight_c_sp, 
                             weight_h_m, weight_h_sc, weight_h_sp,
                             weight_m_sc, weight_m_sp,
                             weight_sc_sp))
          transpose <- as.data.frame(t(df))
          
          colnames(transpose) <- c("b_c", "b_h", "b_m", "b_sc", "b_sp",
                                   "c_h", "c_m", "c_sc", "c_sc",
                                   "h_m", "h_sc", "h_sp",
                                   "m_sc", "m_sp",
                                   "sc_sp")
          rownames(transpose) <- "row"
          
          ahp_mat <- transpose %>% ahp.mat(df = transpose, atts = facilities, negconvert = T) %>% head(3)
          priorities <- ahp.indpref(ahp_mat, facilities)
          
          houses_sf$ahp<- 
            houses_sf$min_dist_busStop*priorities$busStop + houses_sf$min_dist_clinic*priorities$clinics +
            houses_sf$min_dist_hawker*priorities$hawkers  + houses_sf$min_dist_mrt*priorities$mrt +
            houses_sf$min_dist_school*priorities$schools  + houses_sf$min_dist_spf*priorities$spf
          
          
          mydata <- tm_shape(mpa[mpa$PLN_AREA_N==input$userinput, ]) + tm_polygons() + tm_shape(houses_sf[houses_sf$PLN_AREA_N==input$userinput, ]) +
            tm_dots(col='ahp',style='quantile',size=0.01) +
            tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
        }else{
          
        }
      }
    }else{
      if(input$region == "WHOLE SINGAPORE" ){
        h3(facility_dist_vector[input$test])
        h3(input$test)
        mydata <-tm_shape(mpsz) + tm_polygons() + tm_shape(houses_sf) +tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),style='quantile', size=0.01)+
          tm_shape(facility_sf_vector[input$test][[1]]) + tm_dots(size=0.01)+
          tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
      }else{
        if(input$type == "Subzone"){
          if(is.null(input$userinput)){
            fac_sf <- facility_sf_vector[input$test][[1]]
            mydata <- tm_shape(mpr[mpr$REGION_N==input$region, ]) + tm_polygons() +
              tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) +
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),
                      style='quantile',
                      size=0.1)+
              tm_shape(fac_sf[fac_sf$REGION_N==input$region, ]) + tm_dots(size=0.01)+
              tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
          }else{
            fac_sf <- facility_sf_vector[input$test][[1]]
            mydata <-tm_shape(mpsz[mpsz$SUBZONE_N==input$userinput, ]) + tm_polygons() +
              tm_shape(houses_sf[houses_sf$SUBZONE_N==input$userinput, ]) +
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),style='quantile',size=0.1)+
              tm_shape(fac_sf[fac_sf$SUBZONE_N==input$userinput, ]) + tm_dots(size=0.01)+
              tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
          }
        }else if(input$type == "Planning Area"){
          if(is.null(input$userinput)){
            fac_sf <- facility_sf_vector[input$test][[1]]
            mydata <- tm_shape(mpr[mpr$REGION_N==input$region, ]) + tm_polygons() +
              tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) +
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),style='quantile',size=0.1)+
              tm_shape(fac_sf[fac_sf$REGION_N==input$region, ]) + tm_dots(size=0.01)+
              tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
          }else{
            fac_sf <- facility_sf_vector[input$test][[1]]
            mydata <- tm_shape(mpa[mpa$PLN_AREA_N==input$userinput, ]) + tm_polygons() +
              tm_shape(houses_sf[houses_sf$PLN_AREA_N==input$userinput, ]) +
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),style='quantile',size=0.1)+
              tm_shape(fac_sf[fac_sf$PLN_AREA_N==input$userinput, ]) + tm_dots(size=0.01)+
              tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
          }
        }else{
          fac_sf <- facility_sf_vector[input$test][[1]]
          mydata <- tm_shape(mpr[mpr$REGION_N==input$region, ]) + tm_polygons() +
            tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) +
            tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),
                    style='quantile',
                    size=0.1)+
            tm_shape(fac_sf[fac_sf$REGION_N==input$region, ]) + tm_dots(size=0.01)+
            tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
        }
      }
      
    }
    tmap_leaflet(mydata)
  })
  
  # ### For Barchart ###
  output$sz_barchart <-  renderPlot({
    if(input$region == "WHOLE SINGAPORE"){
      if(input$test == "all"){
        
      }else{
        if(input$test == "busStop"){
          min_busStop <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_busStop))
          min_busStop <- min_busStop[order(min_busStop$mean_dist),][c(1:input$upperLimit),]
          plot(ggplot(min_busStop, aes(x= reorder(SUBZONE_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }else if(input$test == "spf"){
          min_spf <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_spf))
          min_spf <- min_spf[order(min_spf$mean_dist),][c(1:input$upperLimit),]
          
          plot(ggplot(min_spf, aes(x= reorder(SUBZONE_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }else if(input$test == "clinics"){
          min_clinic <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_clinic))
          min_clinic <- min_clinic[order(min_clinic$mean_dist),][c(1:input$upperLimit),]
          
          plot(ggplot(min_clinic, aes(x= reorder(SUBZONE_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }else if(input$test == "mrt"){
          min_mrt <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_mrt))
          min_mrt <- min_mrt[order(min_mrt$mean_dist),][c(1:input$upperLimit),]
          
          plot(ggplot(min_mrt, aes(x= reorder(SUBZONE_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }else if(input$test == "schools"){
          min_school <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_preschool))
          min_school <- min_school[order(min_school$mean_dist),][c(1:input$upperLimit),]
          
          plot(ggplot(min_school, aes(x= reorder(SUBZONE_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }else if(input$test == "hawkers"){
          min_hawker <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_hawker))
          min_hawker <- min_hawker[order(min_hawker$mean_dist),][c(1:input$upperLimit),]
          
          plot(ggplot(min_hawker, aes(x= reorder(SUBZONE_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }
      }
    }else{
      if(input$test == "all"){
        
      }else{
        if(input$test == "busStop"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_busStop))
        }else if(input$test == "spf"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_spf))
        }else if(input$test == "clinics"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_clinic))
        }else if(input$test == "mrt"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_mrt))
        }else if(input$test == "schools"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_school))
        }else if(input$test == "hawkers"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(SUBZONE_N) %>% summarise(mean_dist=mean(min_dist_hawker))
        }
        houses_agg <- houses_agg[order(houses_agg$mean_dist),][c(1:input$upperLimit),]
        
        if(input$type =="Region"){
          plot(ggplot(houses_agg, aes(x = reorder(SUBZONE_N, mean_dist),y = mean_dist,
                                      fill=("red"))) + 
                 geom_col() + 
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Mean distance"))
        }else{
          if(is.null(input$userinput)){
            plot(ggplot(houses_agg, aes(x = reorder(SUBZONE_N, mean_dist),y = mean_dist,
                                        fill=("red"))) + 
                   geom_col() + 
                   theme(axis.text.x = element_text(angle = 90, hjust = 1),
                         legend.title = element_blank(),
                         legend.position = "none") +
                   labs(x = "Subzone", y = "Mean distance"))           
          }else{
            plot(ggplot(houses_agg, aes(x = reorder(SUBZONE_N, mean_dist),y = mean_dist,
                                        fill=ifelse(SUBZONE_N==input$userinput,"green","red"))) + 
                   geom_col() + 
                   theme(axis.text.x = element_text(angle = 90, hjust = 1),
                         legend.title = element_blank(),
                         legend.position = "none") +
                   labs(x = "Subzone", y = "Mean distance"))
          }
        }
        # plot(ggplot(houses_agg, aes(x = reorder(SUBZONE_N, mean_dist),y = mean_dist)) + 
        #        geom_col() + 
        #        theme(axis.text.x = element_text(angle = 90, hjust = 1)))
        
      }
    }
  })
  
  # ### For Barchart ###
  output$planning_area_barchart <-  renderPlot({
    min_clinic2 <- houses_sf %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_clinic))
    min_clinic2 <- min_clinic2[order(min_clinic2$mean_dist),][c(1:input$upperLimit),]
    min_hawker2 <- houses_sf %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_hawker))
    min_hawker2 <- min_hawker2[order(min_hawker2$mean_dist),][c(1:input$upperLimit),]
    min_mrt2 <- houses_sf %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_mrt))
    min_mrt2 <- min_mrt2[order(min_mrt2$mean_dist),][c(1:input$upperLimit),]
    min_spf2 <- houses_sf %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_spf))
    min_spf2 <- min_spf2[order(min_spf2$mean_dist),][c(1:input$upperLimit),]
    min_school2 <- houses_sf %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_preschool))
    min_school2 <- min_school2[order(min_school2$mean_dist),][c(1:input$upperLimit),]
    min_busStop2 <- houses_sf %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_busStop))
    min_busStop2 <- min_busStop2[order(min_busStop2$mean_dist),][c(1:input$upperLimit),]
    if(input$region == "WHOLE SINGAPORE"){
      if(input$test == "all"){
        
      }else{
        if(input$test == "busStop"){
          plot(ggplot(min_busStop2, aes(x = reorder(PLN_AREA_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }else if(input$test == "spf"){
          plot(ggplot(min_spf2, aes(x = reorder(PLN_AREA_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }else if(input$test == "clinics"){
          plot(ggplot(min_clinic2, aes(x = reorder(PLN_AREA_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }else if(input$test == "mrt"){
          plot(ggplot(min_mrt2, aes(x = reorder(PLN_AREA_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }else if(input$test == "schools"){
          plot(ggplot(min_school2, aes(x = reorder(PLN_AREA_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }else if(input$test == "hawkers"){
          plot(ggplot(min_hawker2, aes(x = reorder(PLN_AREA_N, mean_dist), mean_dist,fill=("red"))) +
                 geom_col() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }
      }
    }else{
      if(input$test == "all"){
        
      }else{
        if(input$test == "busStop"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_busStop))
        }else if(input$test == "spf"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_spf))
        }else if(input$test == "clinics"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_clinic))
        }else if(input$test == "mrt"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_mrt))
        }else if(input$test == "schools"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_school))
        }else if(input$test == "hawkers"){
          houses_agg <- houses_sf[houses_sf$REGION_N==input$region, ] %>% group_by(PLN_AREA_N) %>% summarise(mean_dist=mean(min_dist_hawker))
        }
        if(input$type =="Region"){
          plot(ggplot(houses_agg, aes(x = reorder(PLN_AREA_N, mean_dist),y = mean_dist,
                                      fill=("red"))) + 
                 geom_col() + 
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Mean distance"))
        }else{
          if(is.null(input$userinput)){
            plot(ggplot(houses_agg, aes(x = reorder(PLN_AREA_N, mean_dist),y = mean_dist,
                                        fill=("red"))) + 
                   geom_col() + 
                   theme(axis.text.x = element_text(angle = 90, hjust = 1),
                         legend.title = element_blank(),
                         legend.position = "none") +
                   labs(x = "Planning Area", y = "Mean distance"))           
          }else{
            plot(ggplot(houses_agg, aes(x = reorder(PLN_AREA_N, mean_dist),y = mean_dist,
                                        fill=ifelse(PLN_AREA_N==input$userinput,"green","red"))) + 
                   geom_col() + 
                   theme(axis.text.x = element_text(angle = 90, hjust = 1),
                         legend.title = element_blank(),
                         legend.position = "none") +
                   labs(x = "Planning Area", y = "Mean distance"))
          }
        }
        
        
      }
    }
  })
  
  ### For Initial Plot###
  output$allPlot <-  renderLeaflet({
<<<<<<< HEAD
    facilities_map <- houses_sf 
    if(input$radio %in% facilities){
      facilities_map <- facility_sf_vector[input$radio][[1]]
=======
    if(input$radio == "busStop"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(busStops_sf) + tm_dots(size = 0.000001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "singapore_police"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(spfs_sf) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "clinics"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(clinics_sf) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "mrt_station"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(mrts_sf) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "school"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(schools_sf) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "houses"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(houses_sf) + tm_dots(size = 0.001, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else if(input$radio == "Hawker"){
      mydata <- tm_shape(mpsz) + tm_polygons() + tm_shape(hawkers_sf) + tm_dots(size = 0.01, col = "#7d4627") + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
    }else{
      mydata <- tm_shape(mpsz) + tm_polygons() + 
        tm_style("classic", bg.color="white") + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
>>>>>>> 4e1b5fb6490adfdcb0c8d3f4aad55366f3ad4b00
    }
      mydata <- tm_shape(mpsz) + tm_polygons() + 
        tm_shape(facilities_map) + 
          tm_dots(size = 0.01, col = "#7d4627") + 
          tm_style("classic", bg.color="white") +
          tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
      
    tmap_leaflet(mydata)
  })
  
<<<<<<< HEAD
  ###Chrolopleth map###
  output$cmap <- renderLeaflet({
    if(input$radio %in% facilities){
      facilities_agg <- facility_sf_vector[input$radio][[1]] %>% group_by(SUBZONE_N) %>% summarise(count = n())
    } else{
      facilities_agg <- houses_sf %>% group_by(SUBZONE_N) %>% summarise(count = n())
    }

      mpsz_data <- mpsz@data
      mpsz_count <- left_join(mpsz_data, facilities_agg)
      mpsz_count$count[is.na(mpsz_count$count)] <- 0
      mpsz@data <- mpsz_count
      mydata <-tm_shape(mpsz) +
                  tm_fill("count", style = "jenks", palette = colour_palette, 
                          title = "Count in each subzone") +
                  tm_layout(legend.show = FALSE,title.position = c("center", "center"),
                          title.size = 20) +
                  tm_borders(alpha = 0.5) 
      
      tmap_leaflet(mydata)
  })
  
  
  ### Data table for Initial Plot###
  output$table <- DT::renderDataTable({
    if(input$databutton == "busStop"){
      mydata <- busStops %>% select(c(Description, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
    }else if(input$databutton == "spf"){
      mydata <- spfs %>% select(c(desc, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name'='desc', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
    }else if(input$databutton == "clinics"){
      mydata <- gpclinics %>% select(c(name, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name'='name','Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
    }else if(input$databutton == "mrt"){
      mydata <- mrt %>% select(c(desc, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name' = 'desc', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
    }else if(input$databutton == "schools"){
      mydata <- schools %>% select(c(school_name, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name' = 'school_name', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
    }else if(input$databutton == "houses"){
      mydata <- houses %>% select(c(address, town, flat_type, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Address' = 'address', 'Town' = 'town', 'Flat Type'='flat_type', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
    }else if(input$databutton == "hawkers"){
      mydata <- hawkers %>% select(c(desc, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name' = 'desc', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
      
=======
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
    }else if(input$radio == "school"){
      mydata <- as.data.frame(schools_sf)
    }else if(input$radio == "houses"){
      mydata <- as.data.frame(houses)
    }else if(input$radio == "Hawker"){
      mydata <- as.data.frame(hawkers_sf)
    }else{
>>>>>>> 4e1b5fb6490adfdcb0c8d3f4aad55366f3ad4b00
    }
  })
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)