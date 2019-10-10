

shinyUI(
  dashboardPage(dashboardHeader(title="Ira Villar"),
                      dashboardSidebar(sidebarUserPanel(""),
                                       sidebarMenu(
                                         menuItem("AboutMe",tabName="AboutMe"),
                                         menuItem("About the Project", tabName = "AbouttheProject"),
                                         menuItem("Bar",tabName = "Bar",icon=icon("database")),
                                         menuItem("Pie",tabName = "Pie")
                                         )),                                       
                
                      dashboardBody(
                        tabItems(
                          tabItem(tabName="AboutMe",
                                  h1("About Me", align = "center"),
                                  fluidRow(img(src="")),
                                  p("Ira is Ira Villar",align="center")),
                          
                          tabItem(tabName="AbouttheProject",
                                  h1("About the Project", align = "center"),
                                  
                                  fluidRow(img(src="")),
                                  
                                  p("This is a sentence.", align = "center")
                                  
                                  ),
                                  
                          
                          tabItem(tabName="Bar",
                                  fluidRow(htmlOutput("BarChart")),
                                                  
                                  selectInput("studio", label = h3("Select Studio"),
                                              choices = list("Disney" = "BV", "Warner Brothers" = "WB", "Universal" = "Uni."),
                                              selected = "BV") 
                                  ),
                          
                          tabItem(tabName = "Pie",
                                  fluidRow( column(6, htmlOutput("PieChart")),
                                            column(6, htmlOutput("PieChartOverseas"))),
                                  
                                  sliderInput("select", label = h3("Select Year"), min = 2006, 
                                              max = 2019, value = 2012),
                                  
                                  hr(),
                                  fluidRow(
                                    column(4, verbatimTextOutput("value"))
                              
                          )
                        ))
                        
  
  )))

                      
