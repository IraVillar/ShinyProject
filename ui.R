

shinyUI(
  dashboardPage(skin="purple", dashboardHeader(title="Hollywood Films and China", titleWidth = "350"),
                      dashboardSidebar(sidebarUserPanel(""),
                                       sidebarMenu(
                                         menuItem("About the Project", tabName = "AbouttheProject",icon=icon("info-circle")),
                                         menuItem("Author Page",tabName="AboutMe",icon=icon("address-card")),
                                         menuItem("Statistics",tabName = "Stats",icon=icon("table")),
                                         menuItem("Overall Percentages",tabName = "Pie",icon=icon("chart-pie")),
                                         menuItem("Opening Comparison",tabName = "Bar",icon=icon("database")),
                                         menuItem("Trends Over Time",tabName = "LineChart",icon=icon("chart-line")),
                                         menuItem("Returns per Film",tabName = "Scatter",icon=icon("dot-circle")),
                                         menuItem("Conclusion",tabName = "Conclusion",icon=icon("film"))
                                         )),                                       
                
                      dashboardBody(
                        tabItems(
                          tabItem(tabName="AbouttheProject",
                                  h1("Hollywood Films and China", align = "center"),
                                  
                                  HTML('<center><img src="GreatWall.jpg" width="400"></center>'),
                                  
                                  htmlOutput("openingtext")
                                  
                                  
                                  ),
                          
                          
                          tabItem(tabName="AboutMe",
                                  h1("Author", align = "center"),
                                  HTML('<center><img src="iraheadshot.png"></center>'),
                                  htmlOutput("abouttext"),
                                    
                                    fluidRow( img(src="LinkedInLogo.png", height ="5%", width = "5%"), uiOutput("LItab"),
                                    img(src="githublogo.svg", height="5%", width = "5%"), uiOutput("gittab"),align="center")
                                    
                                    ),
                                  
                          
                          tabItem(tabName="Bar",
                                  selectInput("studio", label = h3("Select Studio"),
                                              choices = list("Disney" = "BV",
                                                             "Warner Brothers" = "WB",
                                                             "Universal" = "Uni.",
                                                             "Paramount" = "Par.",
                                                             "20th Century Fox" = "Fox",
                                                             "Sony" = "Sony"),selected = "BV"), 
                                  fluidRow(infoBoxOutput("maxBox", width = 6), infoBoxOutput("minBox",width = 6)),
                                  
                                  fluidRow(htmlOutput("BarChart"))
                                                  
                                  
                                  ),
                          
                          tabItem(tabName = "Pie",
                                  
                                  sliderInput("select", label = h3("Select Year"), min = 2000, 
                                              max = 2018, value = 2000),
                                  
                                  fluidRow( column(6, htmlOutput("PieChart")),
                                            column(6, htmlOutput("PieChartOverseas"))),
                                 
                                  hr(),
                                  fluidRow(
                                    column(4, verbatimTextOutput("value")))
                                  
                                ),
                          
                          tabItem(tabName = "LineChart",
                                  fluidRow(
                                    
                                    column(6,
                                           sliderInput("YearRange", label = h3("Year Range\n for US/China Yearly Gross - Comparison"), min = 1990, 
                                                       max = 2018, value = c(1990, 2018))
                                    ),
                                    column(6,
                                           sliderInput("CinemaRange",label=h3("Year Range\n for Number of Screens in China"), min =2005,
                                                       max = 2017, value = c(2005, 2017)))
                                  ),
                                  fluidRow(column(8, htmlOutput("Line")),
                                          column(8,htmlOutput("CinemaLine")))
                                  
                                  ),
                          
                          tabItem(tabName = "Scatter",
                                  h3("US and China All Time Returns per Film - Value in Millions ($)"),
                                  h5("Return of '1' means no information was available"),
                                  plotlyOutput("ScatterChart"),
                                  h3("Search for a Particular Film:"),
                                  fluidRow(column(6, DT::dataTableOutput("table2")))
                                  ),
                          
                                  
                          tabItem(tabName = "Stats",
                                  h3("Some Statistics on the Chinese Film Industry"),
                                  h4("Locally produced films"),
                                  
                            
                                  fluidRow(htmlOutput("StatChart"))
                                  
                                  
                                  
                                  ),
                          
                          tabItem(tabName = "Conclusion",
                                  h1("Conclusion", align = "center"),
                                  HTML('<center><img src="WorldMap.jpg" width="400"></center>'),
                                  htmlOutput("closingtext")
                                  
                                  )
                          
                         
                              
                          
                        ))
                        
  
  ))

                      
