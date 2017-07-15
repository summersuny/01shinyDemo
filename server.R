

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
groupColors <- colorRampPalette(c("red", "#ffa500","green"))

  function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  #map <- createLeafletMap(session, "map")
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
  # output$map <- renderLeaflet({ 
  #   
  #   leaflet() %>% addTiles(urlTemplate ='//{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
  #   attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  #   ) %>%
  #     setView(lng=40.753, lat=-73.960,
  #             zoom = 12)
  # })    
      
  # df_map <- reactive({
  #   df2 <- filter(df, start.date >= input$dates[1] & stop.date <= input$dates[2]
  #                 & hr>=input$hrfrom  & hr<=input$hrto)
  #   nstart <- df2 %>%  group_by(Start.Station.ID) %>% summarise(nstart=n())
  #   nend <- df2 %>%  group_by(End.Station.ID) %>% summarise(nend=n())
  #   df_station <- full_join(nstart,nend,by=c("Start.Station.ID"="End.Station.ID")) 
  #   df_map <- left_join(df_station,temp,by = c("Start.Station.ID"="Start.Station.ID")) %>% 
  #     mutate(abs_change_perc=round(100*(nend-nstart)/nstart,1)) %>% 
  #     filter(!is.na(Start.Station.Latitude))
  #   return (df_map)
  # })
  # 
  # 
  # leafletProxy("map", data = df_map) %>%
  #   clearShapes() %>%
  #   addCircles(~Start.Station.Longitude, ~Start.Station.Latitude,
  #              #radius=radius,
  #              #layerId=~withcircle,
  #              stroke=TRUE, fillOpacity=0.8, color=~groupColors(abs_change_perc))
}
  

