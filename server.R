# function(input, output) {
#   output$value <- renderText({as.character(input$dates[1])})
#   
#   output$barPlot <- renderPlot({
#     df_hr <- df %>% 
#     filter(df$start.date>=input$dates[1] & df$start.date<=input$dates[2]) %>% 
#        group_by(hr,weekday) %>% 
#        summarise(ntrip=n()) 
#     print(df_hr)
#      ggplot(df_hr,aes(x=hr,y=ntrip)) +geom_bar(aes(fill=weekday),stat = "Identity")
#      
#     # df_hr2 <- df_hr %>% filter(weekday=='Weekday')
#     # ggplot(df_hr2,aes(x=hr,y=ntrip)) +geom_bar(aes(fill=ntrip),stat = "Identity") + coord_polar() +scale_colour_brewer()
#     
#     
#     # x <- faithful[, 2]
#     # bins <- input$bins
#     # hist(x, breaks = bins,
#     #      col = 'darkgray',
#     #      border = 'white')
#   })
# }



#########
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

temp <- distinct(select(df,Start.Station.ID,Start.Total.Docks,Start.Station.Latitude,Start.Station.Longitude))
temp2 <- distinct(select(df,End.Station.ID,End.Total.Docks,End.Station.Latitude,End.Station.Longitude))
groupColors <- colorRampPalette(c("red", "#ffa500","green"))
df$hours <- as.numeric(df$hr)

  function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  #map <- createLeafletMap(session, "map")
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
        attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        setView(lng = -73.960, lat = 40.753, zoom = 12) 
    })
  
    


  df_map <- reactive({
    #aggregated data for stations
    df2 <- filter(df, start.date >= input$dates[1] & stop.date <= input$dates[2] &
                   hours>=input$hrfrom  & hours<=input$hrto)
    nstart <- df2 %>%  group_by(Start.Station.ID) %>% summarise(nstart=n())
    nend <- df2 %>%  group_by(End.Station.ID) %>% summarise(nend=n())
    df_station <- full_join(nstart,nend,by=c("Start.Station.ID"="End.Station.ID"))
    
    dfmap <- left_join(df_station,temp,by = c("Start.Station.ID"="Start.Station.ID")) %>%
      mutate(abs_change_perc=round(100*(nend-nstart)/nstart,1)) %>%
      filter(!is.na(Start.Station.Latitude)) %>% 
      mutate(rank=rank(abs_change_perc))
    return(dfmap)
  })
  
  
df_route <- reactive({
  df2 <- filter(df, start.date >= input$dates[1] & stop.date <= input$dates[2] &
                  hours>=input$hrfrom  & hours<=input$hrto)
  #aggregated data for routes
  nroute <- df2%>%  
    group_by(Start.Station.ID, End.Station.ID) %>% 
    summarise(nroute=n()) %>% 
    arrange(desc(nroute)) 
  #join lat,lng for start and end stations
  nroute$route_id <- rownames(nroute)
  routes <- left_join(nroute,temp,by = c("Start.Station.ID"="Start.Station.ID")) 
  routes2 <- left_join(routes,temp2,by = c("End.Station.ID"="End.Station.ID")) 
  
  #take top 30% popular routes
  top_route=head(routes2,n=dim(nroute)[1]*0.3)
  return(top_route)
  })  

observe({  
  #circles for staions
  leafletProxy("map", data = df_map()) %>%
    clearShapes() %>%
    addCircles(~Start.Station.Longitude, ~Start.Station.Latitude,
               #layerId=~withcircle,
               stroke=FALSE, fillOpacity=~(nstart/75+nend/75), color=~groupColors(rank),
               radius=70,
               popup =~paste('<b><font color="Red">', 'LINE1', '</font></b><br/>',
                              'acceptance rate: ', 'LINE2', '<br/>',
                              'undergrads: ', 'LINE3', '<br/>',
                              '4 year graduation rate: ', 'LINE4', '<br/>',
                              'median earnings: ', 'LINE5', '<br/>') 
                 )
  #add polyline for routes
  
  proxy <- leafletProxy("map", data = df_route()) 
  
    
  if (input$routes) {
    route_tbl <- df_route()
    pointlist <- route_tbllist list(list(~Start.Station.Longitude,~Start.Station.Latitude),
                                    list(~End.Station.Longitude,~End.Station.Latitude)
                                    )
    
    polyline = L.Polyline(pointlist, polylineOptions)
    polylineOptions = c(
      color='ffa500',
      weight=5,
      opacity=0.5
      )
    
    proxy %>% addLayer(polyline,polylineOptions)
  } 
  # else {
  #   proxy %>% removeShape(layerId = LETTERS[1:6])
  # }
  
  
  
  # addPolylines(~Start.Station.Longitude, ~Start.Station.Latitude,
  #                color = "ffa500", weight = 5, opacity = 0.5,
  #                fill = FALSE, fillColor = color, fillOpacity = 0.2, dashArray = NULL,
  #                smoothFactor = 1, noClip = FALSE, popup = NULL, popupOptions = NULL,
  #                label = NULL, labelOptions = NULL, options = pathOptions(),
  #                highlightOptions = NULL
  #  )
})  


}


  

