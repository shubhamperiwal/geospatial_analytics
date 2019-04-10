source('global.R', local = TRUE)

# Define UI ----
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
                                   tags$li("Euclidean distance calculation is used to find the closest facility of each HDB and subsequently utilized to compute accessibility score."),
                                   tags$li("Use Analytic Hierarchy Process to calculate Accessibility Score")
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
                                   tags$li("Allow users to upload their own sets of data for analysis on the impact of 
                                           accessibilities in Singapore or other parts of the world."),
                                   tags$li("Allow addition of a single spatial point on the map and see how the point would impact
                                           the accessibility across Singapore."),
                                   tags$li("Account for capacity and demand of facilities for calculation of more realistic accessibility score."),
                                   tags$li("Consider network distance for calculation of minimum distance between HDB flats and facility.")
                                   )))
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
                                                     choices = list(
                                                       "Houses" = "houses",
                                                       "Bus Stop" = "busStop",
                                                        "Clinic"= "clinics",
                                                        "MRT-Station" = "mrt",
                                                        "Hawker" = "hawkers",
                                                        "Schools" = "schools",
                                                        "Singapore Police Force" = "spf"),
                                                     selected = "houses"))
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
                               )
                               ,width=9)
                           )
                  ),
                  tabPanel("Accessbility",
                           sidebarLayout(
                             sidebarPanel(  
                               fluidRow(
                                 h3("Filter your output")
                               ),
                               br(),
                               fluidRow(
                                 selectInput("test", label="Select Facility Type:",
                                             choices = list("AHP" = "all", 
                                                            "Bus Stop" = "busStop",
                                                            "Clinic"= "clinics",
                                                            "MRT-Station" = "mrt",
                                                            "Hawker" = "hawkers",
                                                            "Schools" = "schools",
                                                            "Singapore Police Force" = "spf"),
                                             selected = "all")
                               ),
                               br(),
                               fluidRow(
                                 selectInput("region", 
                                             label = "Select Region:",
                                             choices = list("EAST REGION",
                                                            "NORTH REGION", 
                                                            "CENTRAL REGION",
                                                            "NORTH-EAST REGION", 
                                                            "WEST REGION",
                                                            "WHOLE SINGAPORE"),
                                             selected = "EAST REGION")
                               ),
                               br(),
                               fluidRow(
                                 conditionalPanel(
                                   condition = "input.test != 'all'",
                                   selectInput("upperLimit", "Select BarChart Upper Limit", 
                                               choices = list("5" = 5, "10" = 10,
                                                              "15" = 15,"20" = 20,"30"=30,"50"=50), selected = 15)
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
                                 column(6,h3("Accessibility Plot"),leafletOutput("xscore", height = 400, width = "100%")),
                                 column(6,
                                        conditionalPanel(
                                          condition = "input.test !='all'",
                                          h3("Boxplot by Planning Area"),
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
                                          h3("Bar Chart of Subzones"),
                                          plotOutput("sz_barchart", height = 350, width = "100%"))
                                 ),
                                 column(6,
                                        conditionalPanel(
                                          condition = "input.test =='all'",
                                          h3("About Analytic hierarchy process - AHP"),
                                          p("AHP is utilised to give weightage to different criteria (like minimum distance from bus stops, clinics, etc."),
                                          p("First a user enters his personal priority scores. Then overall weightage is given to the critera.
                                            This weightage is then used in a formula to make the final decision AHP = SUM(Weight*MinDist for each feature)"),
                                          p("The metric for scoring is provided to the right. The negative scores mean the same as the positive, except for the second criteria in the pair"))
                                 ),
                                 column(6,
                                        conditionalPanel(
                                          condition = "input.test =='all'",
                                          h3("AHP - Scoring metrics"),
                                          p("1 - Two characteristics are equally important"),
                                          p("3 - The preferred characteristics are slightly more important"),
                                          p("5 - The preferred characteristics are moderately more important"),
                                          p("7 - The preferred characteristics are strongly more important"),
                                          p("9 - The preferred characteristics are absolutely more important"))
                                 )
                               )
                               ,width=10)
                           )
                           
                  ),
                  tabPanel("Sandbox",
                           sidebarLayout(
                             sidebarPanel(
                               fluidRow(
                                 h3("Filter your output")
                               ),
                               br(),
                               fluidRow(
                                 column(10,
                                        radioButtons("databutton", label = "Show data of:",
                                                     choices = list("Bus Stop" = "busStop",
                                                                    "Clinic"= "clinics",
                                                                    "MRT-Station" = "mrt",
                                                                    "Hawker" = "hawkers",
                                                                    "Schools" = "schools",
                                                                    "Singapore Police Force" = "spf",
                                                                    "Houses" = "houses"),
                                                     selected = "busStop"))
                               ),width=3),
                             mainPanel(
                               fluidRow(
                                 column(10,h3("Data table information")),
                                 column(12,DT::dataTableOutput("table", width="100%"))
                               )
                               ,width=9))
                  ), tabPanel("About the team",
                              withTags(
                                div(class = "team_member3",
                                    img(src = "Raynie.jpg", height = 150, width = 200,style="float:left;"),
                                    h3(" Raynie Moo Wee Lim"),
                                    p("Year 4 graduating student majoring in Information Systems and Organisational Behaviour and Human Resources.")
                                )
                              )
                             ,
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             tags$hr(),
                             withTags(
                               div(class = "team_member1",
                                   img(src = "Shubham.jpg", height = 150, width = 200,style="float:left;"),
                                   h3("Shubham  Periwal"),
                                   p("Junior year Analytics student in School of Information Systems. "),
                                   p("Still thinks Python > R."))
                             ),
                             br(),
                             br(),
                             br(),
                             br(),
                             tags$hr(align="left"),
                             withTags(
                               div(class = "team_member2",
                                   img(src = "Kaelyn.jpg", height = 150, width = 200,style="float:left;"),
                                   h3("Zhuo Yunying(Kaelyn)"),
                                   p("Junior student studying Lee Kong Chain School of Business and currently double majoring in Operations Management and Analytics."),
                                   p("She lives and breathes data analytics.")
                               )
                             )
                             )))
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
      
      if(input$region == "WHOLE SINGAPORE" ){ #AHP Whole SG
        mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
          tm_basemap(server = NA, group = "Clear", alpha = 1) +
          tm_shape(mpsz) + tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
          tm_shape(mpsz) + tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 1) +
          tm_shape(houses_sf) +
          tm_dots(col='ahp',style='quantile',size=0.05, palette = colour_palette,
                  popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                               "AHP Score" = "ahp"))
          
      }else{
        if(input$type == "Region"){#AHP Region
          mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
            tm_basemap(server = NA, group = "Clear", alpha = 1) +
            tm_shape(mpr[mpr$Name==input$region, ]) + tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
            tm_shape(mpr[mpr$Name==input$region, ]) + tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 1) +
            tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) +
            tm_dots(col='ahp',style='quantile',size=0.05, palette = colour_palette,
                    popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                 "AHP Score" = "ahp"))
          
        }else if(input$type =="Subzone"){#AHP Subzone
          mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
            tm_basemap(server = NA, group = "Clear", alpha = 1) +
            tm_shape(mpsz[mpsz$SUBZONE_N==input$userinput, ]) + 
              tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
            tm_shape(mpsz[mpsz$SUBZONE_N==input$userinput, ]) +
              tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 1) +
            tm_shape(houses_sf[houses_sf$SUBZONE_N==input$userinput, ]) +
              tm_dots(col='ahp',style='quantile',size=0.05, palette = colour_palette,
                      popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                   "AHP Score" = "ahp"))
          
        }else if(input$type == "Planning Area"){#AHP Planning
          
          mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
            tm_basemap(server = NA, group = "Clear", alpha = 1) +
            tm_shape(mpa[mpa$Name==input$userinput, ]) + 
              tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
            tm_shape(mpa[mpa$Name==input$userinput, ]) + 
              tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 1) +
            tm_shape(houses_sf[houses_sf$PLN_AREA_N==input$userinput, ]) +
             tm_dots(col='ahp',style='quantile',size=0.05, palette = colour_palette,
                     popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                  "AHP Score" = "ahp"))
        }else{
          
        }
      }
    }else{
      if(input$region == "WHOLE SINGAPORE" ){
        fac_sf <- facility_sf_vector[input$test][[1]]
        
        mydata<-  tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
          tm_basemap(server = NA, group = "Clear", alpha = 1) +
          tm_shape(mpsz) + 
          tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
          tm_shape(mpsz) + 
          tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 3) +
          tm_shape(houses_sf) + 
          tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''), 
                  style='fixed', breaks =breaks_fac, size=0.05,
                  palette = colour_palette,
                  popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                               "Distance" = paste(unlist(facility_dist_vector[input$test]), collapse='')))+
          tm_shape(fac_sf) + 
          tm_symbols(shape=2, size = 0.3, alpha = .5, border.col='black', col='white', scale=4/3) 
        
       
      }else{
        fac_sf <- facility_sf_vector[input$test][[1]]
        if(input$type == "Subzone"){
          if(is.null(input$userinput)){
            mydata<-  tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
              tm_basemap(server = NA, group = "Clear", alpha = 1) +
              tm_shape(mpr[mpr$Name==input$region, ]) + 
              tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
              tm_shape(mpr[mpr$Name==input$region,]) + 
              tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 3) +
              tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) + 
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''), 
                      style='fixed', breaks =breaks_fac, size=0.05,
                      palette = colour_palette,
                      popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                   "Distance" = paste(unlist(facility_dist_vector[input$test]), collapse='')))+
              tm_shape(fac_sf[fac_sf$REGION_N==input$region, ]) + 
              tm_dots(size = .1, alpha = .5, shape = facility_icon_vector[input$test][[1]]) 
            
          }else{
            mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
              tm_basemap(server = NA, group = "Clear", alpha = 1) +
              tm_shape(mpsz[mpsz$SUBZONE_N==input$userinput, ]) + 
              tm_borders(lty = "dashed",col = '#d35400',lwd = 1) +
              tm_shape(mpsz[mpsz$SUBZONE_N==input$userinput,]) + 
              tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 3) +
              tm_shape(houses_sf[houses_sf$SUBZONE_N==input$userinput, ]) + 
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''), 
                      style='fixed', breaks =breaks_fac, size=0.05,
                      palette = colour_palette,
                      popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                   "Distance" = paste(unlist(facility_dist_vector[input$test]), collapse=''))) +
              tm_shape(fac_sf[fac_sf$SUBZONE_N==input$userinput, ])  + 
              tm_dots(size = .3, alpha = .5,shape = facility_icon_vector[input$test][[1]]) 
            
          }
        }else if(input$type == "Planning Area"){
          if(is.null(input$userinput)){
            mydata<-  tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
              tm_basemap(server = NA, group = "Clear", alpha = 1) +
              tm_shape(mpr[mpr$Name==input$region, ]) + 
              tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
              tm_shape(mpr[mpr$Name==input$region,]) + 
              tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 3) +
              tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) + 
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''), 
                      style='fixed', breaks = breaks_fac, size=0.05,
                      palette = colour_palette,
                      popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                   "Distance" = paste(unlist(facility_dist_vector[input$test]), collapse=''))) +
              tm_shape(fac_sf[fac_sf$REGION_N==input$region, ]) + 
              tm_dots(size = .1, alpha = .5, shape = facility_icon_vector[input$test][[1]]) 
          }else{
            mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
              tm_basemap(server = NA, group = "Clear", alpha = 1) +
              tm_shape(mpa[mpa$Name==input$userinput,]) + 
              tm_borders(lty = "dashed",col = '#d35400',lwd = 1) +
              tm_shape(mpa[mpa$Name==input$userinput,]) + 
              tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 3) +
              tm_shape(houses_sf[houses_sf$PLN_AREA_N==input$userinput, ]) + 
              tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''), 
                      style='fixed', breaks =breaks_fac, size=0.05,
                      palette = colour_palette,
                      popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                   "Distance" = paste(unlist(facility_dist_vector[input$test]), collapse=''))) +
              tm_shape(fac_sf[fac_sf$PLN_AREA_N==input$userinput, ])  + 
              tm_dots(size = .3, alpha = .5,shape = facility_icon_vector[input$test][[1]]) 
          }
        }else{
          mydata<-  tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
            tm_basemap(server = NA, group = "Clear", alpha = 1) +
            tm_shape(mpr[mpr$Name==input$region, ]) + 
            tm_borders(lty = "dashed",col = '#d35400',lwd = 1)+
            tm_shape(mpr[mpr$Name==input$region,]) + 
            tm_polygons(col = '#227093', alpha = 0.3, border.col = '#2f3542', lwd = 3) +
            tm_shape(houses_sf[houses_sf$REGION_N==input$region, ]) + 
            tm_dots(col=paste(unlist(facility_dist_vector[input$test]), collapse=''),
                    style='fixed', breaks = breaks_fac, size=0.05,
                    palette = colour_palette,
                    popup.vars=c("Address"="address", "Flat Type"="flat_type", "Town"="town",
                                 "Distance" = paste(unlist(facility_dist_vector[input$test]), collapse=''))) +
            tm_shape(fac_sf[fac_sf$REGION_N==input$region, ]) + 
            tm_dots(size = .1, alpha = .5, shape = facility_icon_vector[input$test][[1]]) 
        }
      }
      
    }
    tmap_leaflet(mydata)
  })
  
  # ### For SUBZONE Boxplot ###
  output$sz_barchart <-  renderPlot({
   
    houses_box_sg <- houses_sf %>% rename('distance' = paste(unlist(facility_dist_vector[input$test]), collapse=''))
    
    #WHOLE SINGAPORE
    if(input$region == "WHOLE SINGAPORE"){
      if(input$test != "all"){
          plot(ggplot(houses_box_sg, aes(x= SUBZONE_N, y= distance))+
                 geom_boxplot( fill=c('#336e7b')) +
                 labs(x = "Subzone", y = "Distance"))
      }
    }else{
      
      #SELECTED REGION
      if(input$test != "all"){
        houses_sf_region <- houses_sf[houses_sf$REGION_N==input$region, ]
        houses_box_region <- houses_sf_region %>% rename('distance' = paste(unlist(facility_dist_vector[input$test]), collapse=''))
          plot(ggplot(houses_box_region, aes(x = SUBZONE_N, y=distance)) + 
                 geom_boxplot( fill=c('#336e7b')) + 
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Subzone", y = "Distance"))
        }
      }
  })
  
  # ### For Planning area Boxplot on top right ###
  output$planning_area_barchart <-  renderPlot({
    houses_box_sg <- houses_sf %>% rename('distance' = paste(unlist(facility_dist_vector[input$test]), collapse=''))
    
    if(input$region == "WHOLE SINGAPORE"){
      if(input$test != "all"){
        plot(ggplot(houses_box_sg, aes(x = PLN_AREA_N, y=distance)) + 
               geom_boxplot( fill=c('#336e7b')) + 
               labs(x = "Planning Area", y = "Distance")+
               scale_fill_manual(values = c('#336e7b')))
      }
    }else{
      
      houses_sf_region <- houses_sf[houses_sf$REGION_N==input$region, ]
      houses_box_region <- houses_sf_region %>% rename('distance' = paste(unlist(facility_dist_vector[input$test]), collapse=''))
      
      if(input$test != "all"){
        
          plot(ggplot(houses_box_region, aes(x = PLN_AREA_N, y=distance)) + 
                 geom_boxplot( fill=c('#336e7b')) + 
                 coord_flip() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.title = element_blank(),
                       legend.position = "none") +
                 labs(x = "Planning Area", y = "Distance")+
                 scale_fill_manual(values = c('#336e7b')))
      }
    }
  })
  
  ### Map of our Data Plot###
  output$allPlot <-  renderLeaflet({
    facilities_map <- houses_sf 
    if(input$radio %in% facilities){
      facilities_map <- facility_sf_vector[input$radio][[1]]
    }
    mydata <- tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
      tm_basemap(server = NA, group = "Clear", alpha = 1) +
      tm_shape(mpsz) + tm_polygons(alpha=0, border.col = '#2f3542', lwd = 1) + 
      tm_shape(facilities_map) + 
      tm_dots(size = 0.03, col = '#82ccdd', border.col='black')
    
      tmap_leaflet(mydata)
  })
  
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
    mydata <-tm_basemap(server = "OpenStreetMap", group = "Street", alpha = 1) +
      tm_basemap(server = NA, group = "Clear", alpha = 1) +
      tm_shape(mpsz) +
      tm_fill("count", style = "jenks", palette = 'GnBu', 
              title = "Count in each subzone") +
      tm_layout(legend.show = FALSE,title.position = c("center", "center"),
                title.size = 20) +
      tm_borders(lty = "dashed",col = '#192a56',lwd = 1)
    
    tmap_leaflet(mydata)
  })
  
  
  ### Data table for Initial Plot###
  output$table <- DT::renderDataTable({
    if(input$databutton == "busStop"){
      mydata <- busStops %>% dplyr::select(c(Description, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }else if(input$databutton == "spf"){
      mydata <- spfs %>% dplyr::select(c(desc, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name'='desc', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }else if(input$databutton == "clinics"){
      mydata <- gpclinics %>% dplyr::select(c(name, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name'='name','Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }else if(input$databutton == "mrt"){
      mydata <- mrt %>% dplyr::select(c(desc, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name' = 'desc', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }else if(input$databutton == "schools"){
      mydata <- schools %>% dplyr::select(c(school_name, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name' = 'school_name', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }else if(input$databutton == "houses"){
      mydata <- houses %>% dplyr::select(c(address, town, flat_type, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Address' = 'address', 'Town' = 'town', 'Flat Type'='flat_type', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }else if(input$databutton == "hawkers"){
      mydata <- hawkers %>% dplyr::select(c(desc, SUBZONE_N, PLN_AREA_N, REGION_N))
      mydata %>% rename('Name' = 'desc', 'Subzone'='SUBZONE_N', 'Planning Area'='PLN_AREA_N', 'Region'='REGION_N')
    }
  })
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)