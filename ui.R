library(shinydashboard)
library(leaflet)

source("Untitled.R")

header <- dashboardHeader(
  title = "Home Sharing"
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Maps", tabName = "map", icon = icon("map")),
    menuItem("Graphs", icon = icon("bar-chart"), tabName = "Graphs"),
    menuItem("Analysis", icon = icon("list-alt"), tabName = "Analysis"),
    menuItem("About Us", icon = icon("address-book"), tabName = "About",badgeColor = "green")
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            fluidRow(
              tabsetPanel(
                     tabPanel("By Location",
                              column(width = 9,
                                     box(width = NULL, solidHeader = TRUE,
                                         leafletOutput("roomtype", height = 500)
                                     ),
                                     box(width = NULL,
                                         dataTableOutput("numVehiclesTable")
                                     )
                              ),
                              column(width = 3,
                                     box(width = NULL, status = "warning",
                                           selectInput("neighbour", "Neighbourhood", choices = unique(AirSL$neighbourhood)),
                                         sliderInput("range", "Price Range:",
                                                     min = 1, max = 100000,
                                                     value = c(10,50)),
                                         p(
                                           class = "text-muted",
                                           paste("Note: Select the neighbourhood and price range to see the apartments"
                                           )
                                         )
                                     ))),
                     tabPanel("By Room Type",
                                       column(width = 9,
                                              box(width = NULL, solidHeader = TRUE,
                                                  leafletOutput("price_dist", height = 500)
                                              ),
                                              box(width = NULL,
                                                  dataTableOutput("chk")
                                              )
                                       ),
                              column(width = 3,
                                     box(width = NULL, status = "warning",
                                         selectInput("room_type", "room_type", choices = unique(AirSL$room_type))                                     ))
                              )
              )
            )
    ),
    
    #####part2 in menuprop bed room hostsince cancepo
    tabItem(
      tabName = "Graphs", 
        tabPanel("Tab1", column(width = 2,
                                box(width = NULL,radioButtons("yaxis", "Choose y Axis:", choices = c("price","number_of_reviews"))),
                                box(width = NULL,radioButtons("xaxis", "Choose x Axis:",
                                                             choices = c("bedrooms","room_type","accommodates","bathrooms","minimum_nights")),height = 200)
        ),
        column(width = 8,
               plotlyOutput("event",height = 650)
        ),
        column(width = 2,
               box(width = NULL,selectInput("bedrooms", "#Bedrooms", choices = unique(AirSL$bedrooms_bin)),height = 120),
               box(width = NULL,selectInput("bathrooms", "#Bathrooms", choices = unique(AirSL$bathrooms_bin)),height = 120),
               box(width = NULL,selectInput("accommodates", "#Guests", choices = unique(AirSL$accommodates_bin)),height = 120)
        ))
      
    ),
    
    tabItem(tabName = "Analysis",
            
            plotOutput("plot")
            
    ),
    
    tabItem(
      tabName = "About",
      mainPanel(
        h2("142078 || Methun Perera"),
        h2("142055 || Nuwan Gajanayake"),
        h2("|||___|||___|||___|||___|||")
        #tableOutput("chk"),
        #verbatimTextOutput("room_type"),
        #verbatimTextOutput("price")
      )
    )
    
    
    
    
    
    )
)
dashboardPage(
  skin = "green",
  header,
  sidebar,
  body
)


