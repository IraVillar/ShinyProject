

shinyUI(
  dashboardPage(skin="purple", dashboardHeader(title="Hollywood Movies and China", titleWidth = "350"),
                      dashboardSidebar(sidebarUserPanel(""),
                                       sidebarMenu(
                                         menuItem("About the Project", tabName = "AbouttheProject",icon=icon("info-circle")),
                                         menuItem("AboutMe",tabName="AboutMe",icon=icon("address-card")),
                                         menuItem("Bar",tabName = "Bar",icon=icon("database")),
                                         menuItem("Pie",tabName = "Pie",icon=icon("chart-pie")),
                                         menuItem("Line Chart",tabName = "LineChart",icon=icon("chart-line")),
                                         menuItem("Conclusion",tabName = "Conclusion",icon=icon("film"))
                                         )),                                       
                
                      dashboardBody(
                        tabItems(
                          tabItem(tabName="AbouttheProject",
                                  h1("About the Project", align = "center"),
                                  
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
                                  
                                  sliderInput("select", label = h3("Select Year"), min = 2006, 
                                              max = 2019, value = 2006),
                                  
                                  fluidRow( column(6, htmlOutput("PieChart")),
                                            column(6, htmlOutput("PieChartOverseas"))),
                                 
                                  hr(),
                                  fluidRow(
                                    column(4, verbatimTextOutput("value")))
                                  
                                ),
                          
                          tabItem(tabName = "LineChart"),
                          
                          tabItem(tabName = "Conclusion",
                                  h1("Conclusion", align = "center"),
                                  
                                  p("This is the conclusion"))
                              
                          
                        ))
                        
  
  ))

                      
