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
library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}
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
  top_route=head(routes2,n=dim(nroute)[1]*0.003)
  z <- gather(top_route, measure, val, -route_id) %>% group_by(route_id) %>%
    do(data.frame(   lat=c(.[["val"]][.[["measure"]]=="Start.Station.Latitude"],
                           .[["val"]][.[["measure"]]=="End.Station.Latitude"]),
                     long = c(.[["val"]][.[["measure"]]=="Start.Station.Longitude"],
                            .[["val"]][.[["measure"]]=="End.Station.Longitude"])))
  z <- as.data.frame(z)
  y <- points_to_line(z, "long", "lat", "route_id") 
  return(y)
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
                 ) %>% 
    addPolylines(data = df_route(),color="#ffa500",weight =2, opacity = 0.3)
  #add polyline for routes
  
  
})  


}


  

