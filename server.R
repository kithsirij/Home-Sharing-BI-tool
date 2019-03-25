library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl)# make the jsonlite suggested dependency explicit
library(car)
library(dplyr)
library(psych)
library(dplyr)
library(leaflet)
library(gender)
library(rockchalk)
library(gdata)
library(plyr)
library(corrplot)
library(gmodels)
library(ggplot2)
library(ggmap)



shinyServer(function(input, output, session) {
  
  output$plot<-renderPlot({
    AirSL %>%
      dplyr::count(host_id, sort = TRUE) %>%
      filter(n > 150) %>%
      mutate(host_id = reorder(host_id,n)) %>%
      ggplot(aes(host_id,n)) +
      geom_col() + 
      coord_flip() + 
      labs (x = "host_id", y = "Frequency", title = "host_id Frequencies With Greater than 150 Occurences")+
      theme_classic()
  })
  
  
  
  output$numVehiclesTable <- renderDataTable({
    price_group <- AirSL %>% filter(neighbourhood == input$neighbour) %>% group_by(room_type) %>% summarise(total_listings = length(room_type),
                                                                              avg_price = mean(price))
    
  },options = list(iDisplayLength = 10))

  
  output$chk <- renderDataTable({
    price_group_room <- AirSL %>% group_by(room_type) %>% summarise(total_listings = length(room_type),
                                                                              avg_price = mean(price))
    
  },options = list(iDisplayLength = 10))
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  ##map1
  output$roomtype <- renderLeaflet({
    factpal <- colorFactor(c("darkgreen","red","darkblue"), 
                           AirSL$room_type)
    
    center_lon = median(AirSL$long,na.rm = TRUE)
    center_lat = median(AirSL$lat,na.rm = TRUE)
    
    leaflet(AirSL %>% filter(neighbourhood==input$neighbour)  %>% filter(price>input$range[1] & price<input$range[2])) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircles(lng = ~longitude, lat = ~latitude, 
                 color = ~factpal(room_type))  %>%
      # controls
      setView(lng=center_lon, lat=center_lat,zoom = 11) %>%
      
      addLegend("bottomright", pal = factpal, values = ~room_type,
                title = "House Price Distribution",
                opacity = 10)
    
    
  })
  ##map2
  AirSL$PriceBin<-cut(AirSL$price, c(-0.01,63,99,136,155,250,6000))
  
  center_lon = median(AirSL$long,na.rm = TRUE)
  center_lat = median(AirSL$lat,na.rm = TRUE)
  
  output$price_dist <- renderLeaflet({
    factpal1 <- colorFactor(c("black","orange","darkgreen","red","blue","darkblue"), 
                           AirSL$PriceBin)
    leaflet(AirSL %>% filter(room_type==input$room_type)) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircles(lng = ~longitude, lat = ~latitude, 
                 color = ~factpal1(PriceBin))  %>%
      # controls
      setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
      
      addLegend("bottomright", pal = factpal1, values = ~PriceBin,
                title = "House Price Distribution",
                opacity = 1)
    
    
  })
  
  ###plotly
  
  
  output$event <- renderPlotly({
    
    x <- list(title = input$xaxis)
    y <- list(title = input$yaxis)
    
    if(input$xaxis!="host_since")
    {
      plot_ly(AirSL %>% filter(bedrooms_bin == input$bedrooms) %>%
                filter(bathrooms_bin == input$bathrooms) %>% filter(accommodates_bin == input$accommodates),
                  x = ~get(input$xaxis), y = ~get(input$yaxis), color = ~get(input$xaxis), type = "box") %>%
                    layout(xaxis = x, yaxis = y)
    }
    else
    {
      plot_ly(AirSL %>% filter(bedrooms_bin == input$bedrooms) %>%
                    filter(bathrooms_bin == input$bathrooms) %>% filter(accommodates_bin == input$accommodates), 
                          x = ~minimum_nights, y = ~room_type, color = ~number_of_reviews, type = "scatter") %>%
                            layout(xaxis = x, yaxis = y)
    }
  })
  
})
