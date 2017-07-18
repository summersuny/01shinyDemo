library(shiny)

# fluidPage(
#   titlePanel(h1("Bike Station")),
#   sidebarLayout(
#     sidebarPanel(
#       dateRangeInput("dates", label = h3("Date range"),start = '2017-03-01', end = '2017-03-31')
#       ,
# 
# 
#        fluidRow(column(4, verbatimTextOutput("value")))
# 
#       # sliderInput("bins", "Number of bins:",
#       #             min = 1, max = 50, value = 30)
#     ),
#     mainPanel(
#       plotOutput("barPlot")
#     )
#   )
# )

##########
# Choices for drop-downs
vars <- c(
  "Female" = "2", 
  "Male" = "1",
  "Unkown" = "0"
)


navbarPage("Summer's Shiny", id="nav", 
                   tabPanel("Interactive map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),

                                leafletOutput("map", width="100%", height="100%"),
                                
                                
                                absolutePanel(
                                  id = "controls", 
                                  #class = "modal-body", 
                                  fixed = TRUE, draggable = TRUE,
                                  top = 60, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h2("Station Explorer"),
                                  
                                  selectInput("sex", "Gender", vars,multiple=TRUE),
                                  #selectInput("size", "Size", vars, selected = "nboth"),
                                  #numericInput("age", "Select age (admit rate less than)", 8),
                                  #numericInput("hrfrom", "Hour Range From", 8),
                                  #numericInput("hrto", "Hour Range To", 10),
                                  sliderInput("hrs", label = h3("Hour Range"), min = 0, 
                                              max = 23, value = c(0, 23)),
                                  checkboxInput("routes", "Show Most Popular Routes", value = FALSE),
                                  
                                  dateRangeInput("dates", label = h3("Date range"),start = '2017-03-01', end = '2017-03-31'),
                                  div(style = "margin: 0 auto;text-align: center;"),
                                  plotOutput("roseplot", height = 200)       
                                  )
                                
                                  
                                  # tags$div(id="cite",
                                  #         HTML('Contact <a href="http://www.richmajerus.com/" target="_blank" >Rich Majerus </a> (rich.majerus@gmail.com) with questions, comments or concerns.  This application was built from code developed by the <a href="https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example" target="_blank" >RStudio Team</a>. Data Source: National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS) 2012-2013.  Due to these data being self-reported by each institution, the quality of the data in this visualization is only as high as the quality of institutional reporting. This visualization presents IPEDS data “as is." '
                                  #         )
                                  #       )
                                  )
                   ),
           tabPanel("Rider Activities",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                      
                        
                        fluidRow(
                          htmlOutput("view",height = 400, width = 800)
                        ),
                        
                        fluidRow(
                          column(12,plotOutput("barplot", height = 200, width = 800))
                          #column(6,DT::dataTableOutput("table"))#, height = 100, width = "40%"))
                        ),
                        
                        absolutePanel(
                          id = "controls", 
                          #class = "modal-body", 
                          fixed = TRUE, draggable = TRUE,
                          top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          h2("Station Explorer"),
                          
                          selectInput("sex2", "Gender", vars,multiple=TRUE),
                          sliderInput("hrs2", label = h3("Hour Range"), min = 0, 
                                      max = 23, value = c(0, 23)),
                          checkboxInput("routes2", "Show Most Popular Routes", value = FALSE),
                          dateRangeInput("dates2", label = h3("Date range"),start = '2017-03-01', end = '2017-03-31'),
                          div(style = "margin: 0 auto;text-align: center;")
                              
                        )
                        
                    )
                    
           )
           
           #conditionalPanel("false", icon("crosshair"))
)
