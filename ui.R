

shinyUI(
  dashboardPage(skin="purple", dashboardHeader(title="Hollywood Movies and China", titleWidth = "350"),
                      dashboardSidebar(sidebarUserPanel(""),
                                       sidebarMenu(
                                         menuItem("About the Project", tabName = "AbouttheProject",icon=icon("info-circle")),
                                         menuItem("About Me",tabName="AboutMe",icon=icon("address-card")),
                                         menuItem("Statistics",tabName = "Stats",icon=icon("table")),
                                         menuItem("Pie Charts",tabName = "Pie",icon=icon("chart-pie")),
                                         menuItem("Bar Chart",tabName = "Bar",icon=icon("database")),
                                         menuItem("Line Charts",tabName = "LineChart",icon=icon("chart-line")),
                                         menuItem("Scatter Charts",tabName = "Scatter",icon=icon("dot-circle")),
                                         menuItem("Conclusion",tabName = "Conclusion",icon=icon("film"))
                                         )),                                       
                
                      dashboardBody(
                        tabItems(
                          tabItem(tabName="AbouttheProject",
                                  h1("Hollywood Movies and China", align = "center"),
                                  
                                  fluidRow(img(src='ManwatchingTv.jpg',width="310",height="180", align = "center")),
                                  
                                  p("\n\tThis is a sentence.", align = "center")),
                          
                          
                          tabItem(tabName="AboutMe",
                                  h1("About Me", align = "center"),
                                  fluidRow(img(src="")),
                                  p("Ira is Ira Villar",align="center")),
                                  
                          
                          tabItem(tabName="Bar",
                                  selectInput("studio", label = h3("Select Studio"),
                                              choices = list("Disney" = "BV",
                                                             "Warner Brothers" = "WB",
                                                             "Universal" = "Uni.",
                                                             "Paramount" = "Par.",
                                                             "20th Century Fox" = "Fox",
                                                             "Sony" = "Sony"),selected = "BV"), 
                                  
                                  fluidRow(htmlOutput("BarChart"))
                                                  
                                  
                                  ),
                          
                          tabItem(tabName = "Pie",
                                  
                                  sliderInput("select", label = h3("Select Year"), min = 1990, 
                                              max = 2019, value = 1990),
                                  
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
                                                       max = 2019, value = c(1990, 2019))
                                    ),
                                    column(6,
                                           sliderInput("CinemaRange",label=h3("Year Range\n for Number of Screens in China"), min =2005,
                                                       max = 2019, value = c(2005, 2019)))
                                  ),
                                  fluidRow(column(8, htmlOutput("Line")),
                                          column(8,htmlOutput("CinemaLine")))
                                  
                                  ),
                          
                          tabItem(tabName = "Scatter",
                                  h3("US and China All Time Returns per Film - Value in Millions ($)"),
                                  h5("Return of '1' means no information was available"),
                                  plotlyOutput("ScatterChart")
                                  ),
                          
                                  
                          tabItem(tabName = "Stats",
                                  h3("Some Statistics on the Chinese Film Industry"),
                                  fluidRow(column(6, DT::dataTableOutput("table"))),
                                  fluidRow(htmlOutput("StatChart"))
                                  
                                  
                                  
                                  ),
                          
                          tabItem(tabName = "Conclusion",
                                  h1("Conclusion", align = "center"),
                                  
                                  p("This is the conclusion")
                                  
                                  )
                          
                         
                              
                          
                        ))
                        
  
  ))

                      
