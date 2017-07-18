#########
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sp)
library(maptools)
library(scales)

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
temp <- distinct(select(df,Start.Station.ID,Start.Total.Docks,Start.Station.Latitude,Start.Station.Longitude,Start.Station.Name))
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
                   hours>=input$hrs[1]  & hours<=input$hrs[2] )
    if(!is.null(input$sex)) {
      df2 <- filter(df2,Gender %in% input$sex)
    }
    
    nstart <- df2 %>%  group_by(Start.Station.ID) %>% summarise(nstart=n())
    nend <- df2 %>%  group_by(End.Station.ID) %>% summarise(nend=n())
    df_station <- full_join(nstart,nend,by=c("Start.Station.ID"="End.Station.ID"))
    
    dfmap <- left_join(df_station,temp,by = c("Start.Station.ID"="Start.Station.ID")) %>%
      mutate(abs_change_perc=round(100*(nend-nstart)/nstart,1)) %>%
      filter(!is.na(Start.Station.Latitude) & !is.na(abs_change_perc)) %>% 
      mutate(rank=rank(abs_change_perc)) 

    return(dfmap)
  })
  
  
  df_route <- reactive({
    df2 <- filter(df, start.date >= input$dates[1] & stop.date <= input$dates[2] &
                    hours>=input$hrs[1]  & hours<=input$hrs[2] )
    if(!is.null(input$sex)) {
      df2 <- filter(df2,Gender %in% input$sex)
    }
  #aggregated data for routes
  if(input$routes==TRUE){
  nroute <- df2%>%  
    group_by(Start.Station.ID, End.Station.ID) %>% 
    summarise(nroute=n()) %>% 
    arrange(desc(nroute)) 
  #join lat,lng for start and end stations
  nroute$route_id <- rownames(nroute)
  routes <- left_join(nroute,temp,by = c("Start.Station.ID"="Start.Station.ID")) 
  routes2 <- left_join(routes,temp2,by = c("End.Station.ID"="End.Station.ID")) 
  
  #take top 0.6% popular routes
  top_route=head(routes2,n=dim(nroute)[1]*0.006)
  z <- gather(top_route, measure, val, -route_id) %>% group_by(route_id) %>%
    do(data.frame(   lat=c(.[["val"]][.[["measure"]]=="Start.Station.Latitude"],
                           .[["val"]][.[["measure"]]=="End.Station.Latitude"]),
                     long = c(.[["val"]][.[["measure"]]=="Start.Station.Longitude"],
                            .[["val"]][.[["measure"]]=="End.Station.Longitude"])))
  z <- as.data.frame(z)
  y <- points_to_line(z, "long", "lat", "route_id") 
  return(y)
  } 
  

  })
  
  
  observe({  
    pal <- colorNumeric(palette=colorRampPalette(c("red", "#ffa500","green"))(3000),domain = df_map()$rank)
  #circles for staions
  leafletProxy("map", data = df_map()) %>%
    clearShapes() %>%
    addCircles(~Start.Station.Longitude, ~Start.Station.Latitude,
               #layerId=~withcircle,
               stroke=FALSE, fillOpacity=~(nstart/75+nend/75), color=~pal(rank),
               radius=70, 
               popup =~paste('<b><font color="Red">', 'Station: ', Start.Station.Name, '</font></b><br/>',
                              'Number of Trip Starts Here: ', nstart, '<br/>',
                              'Number of Trip End Here: ', nend, '<br/>',
                              'Rate of Bike Changes: ', abs_change_perc,' %', '<br/>') 
                 ) 
  
  
    if (input$routes==TRUE) {
      leafletProxy("map", data = df_map()) %>% 
      addPolylines(data = df_route(),color="#ffa500",weight =2, opacity = 0.3)
      #add polyline for routes
    } 
    
  
  
  })  

  ## Rider Activities##############################################
  
  df_gvis <- reactive({
  ##GoogleVis
  #medium speed,total hours,distance spent by groups of type & age & gender, per month
  #distance and convert to miles, speed miles/hours. Duration in seconds.
  #age interval
  df2 <- filter(df, start.date >= input$dates2[1] & stop.date <= input$dates2[2] &
                  hours>=input$hrs2[1]  & hours<=input$hrs2[2] )
  
  if(!is.null(input$sex2)) {
    df2 <- filter(df2,Gender %in% input$sex2)
  }
  print(names(df2))
  df_ck  <-  df2 %>% mutate(distance_manhn=(abs(End.Station.Latitude-Start.Station.Latitude) + abs(End.Station.Longitude-Start.Station.Longitude))*45.096676
  ) %>%
    mutate(age=cut(Birth.Year, breaks=seq(1917,2017, by=5), right = TRUE, labels = seq(100,5,by=-5))) %>% 
     mutate(distance_eucli=sqrt((End.Station.Latitude-Start.Station.Latitude)^2 +(End.Station.Longitude-Start.Station.Longitude)^2)*45.096676
    ) %>% mutate_(distance=input$distance) %>% 
    mutate(speed=distance*60*60/Trip.Duration) 
    

  ##manhattan distance aggregation table
  df_cust <- df_ck %>% 
    group_by(User.Type, age, Gender, months(start.date)) %>% 
    summarise(median_speed=median(speed),
              median_sum_duration=median(sum(Trip.Duration/360)),
              avg_trip_distance=sum(distance)/n(), 
              count=n())
  
  df_cust$Gender <-  gsub("1", 'Male',df_cust$Gender) 
  df_cust$Gender <- gsub("2", 'Female',df_cust$Gender) 
  df_cust$Gender <- gsub("0", 'Unkown',df_cust$Gender)
  colnames(df_cust)[4]='month'
  
  return(df_cust)
  })
  
  
  output$view <- renderGvis({
    gvisBubbleChart(df_gvis(), idvar="age", 
                              xvar="avg_trip_distance", yvar="median_speed",
                              colorvar="Gender", sizevar="count",
                              options=list(
                                vAxis= "{minValue: 0, maxValue: 6,title: 'Speed (MPH)',
                                gridlines:{color:'transparent'},
                                viewWindowMode:'explicit',
                                viewWindow: {
                                max:6.5,
                                min:4.6}}",
                                hAxis= "{
                                maxValue:3,title:'Average Miles Per Trip' ,
                                gridlines:{color:'transparent'},
                                viewWindowMode:'explicit',
                                viewWindow: {
                                max:2,
                                min:0}}",
                                explorer="{actions:['dragToZoom', 'rightClickToReset']}"
                                ,width=800 
                                ,height=400
                                ,chartArea="{left:80,top:30,width:'100%',height:'75%'}"
                                ,legend="bottom"
                                ,title="Rider's Performance by Gender and Age"
                                ,sizeAxis = '{minValue: 0,  maxSize: 20}'
                                #,backgroundColor='black'
                                #,backgroundColor.stroke='white'
                                
                                
                                
                                
                              )
    )
  })
  
  #ggplot
  rose <- reactive({
    df2 <- filter(df, start.date >= input$dates[1] & stop.date <= input$dates[2] &
                    hours>=input$hrs[1]  & hours<=input$hrs[2] )
    if(!is.null(input$sex)) {
      df2 <- filter(df2,Gender %in% input$sex)
    }
    #side absolut panel charts
    df_hr <- df2 %>% group_by(hr,weekday) %>% summarise(ntrip=n()) 
    #ggplot(df_hr,aes(x=hr,y=ntrip)) +geom_bar(aes(fill=weekday),stat = "Identity")
    df_hr2 <- df_hr %>% filter(weekday=='Weekday')
    df_hr2=ungroup(df_hr2)
    #add 1 trip to each hour to maitain rosemap, group has small bug
    tb=data.frame(hr=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23'),
    weekday=c(rep('weekday',24)),ntrip=c(rep(1,24)))
    df_hr2 <- as.data.frame(rbind(df_hr2,tb) %>% 
                  group_by(hr,weekday) %>%
                  summarise(ntrip=sum(ntrip)))
    return(df_hr2)
  })
  
  bar <- reactive({
    #barpot in rider's tab
    df2 <- filter(df, start.date >= input$dates2[1] & stop.date <= input$dates2[2] &
                    hours>=input$hrs2[1]  & hours<=input$hrs2[2] )
    if(!is.null(input$sex)) {
      df2 <- filter(df2,Gender %in% input$sex)
    }
    
    df_hr <- df2 %>% group_by(hr,weekday) %>% summarise(ntrip=n()) 
    return(df_hr)
  })
  
  
  observe({
    output$roseplot <- renderPlot({
      #if (nrow(zipsInBounds()) == 0)
      # return(NULL)
      p <- ggplot(rose(),aes(x=hr,y=ntrip)) +
        geom_bar(aes(fill=ntrip),stat = "Identity") +coord_polar() +scale_colour_brewer() +
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) 
      print(p)
    })
    
    output$barplot <- renderPlot({
      #if (nrow(zipsInBounds()) == 0)
      # return(NULL)
      p2 <- ggplot(bar(),aes(x=hr,y=ntrip)) +geom_bar(aes(fill=weekday),stat = "Identity") +
        theme_bw() + scale_y_continuous(labels = comma) +
        theme(legend.title = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title=element_text(face="bold")) + labs(title="Trip Volume") +
        xlab("Hours") + ylab("Number of Trips")
      return(p2)
    })
    
    # #table data
    # output$table <- DT::renderDataTable({
    #   action <- DT::dataTableAjax(session, bar())
    #   DT::datatable(bar(), options = list(ajax = list(url = action)), escape = FALSE,height = 200)
    # })
    
    
  })

}


  

