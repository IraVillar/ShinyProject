


shinyServer(function(input, output) {
  
  url <- a("LinkedIn", href="https://www.linkedin.com/in/ira-villar-3364514a/")
  url2 <- a("GitHub", href="https://github.com/IraVillar")
  output$LItab <- renderUI({
    tagList(url)})
  output$gittab <- renderUI({
    tagList(url2)})
  
  output$closingtext <- renderUI({
    HTML(paste('<br/><br/> We can see from the data that there is an exponential rise in Chinese screens and box-office numbers for Hollywood films.
              For the purpose of this project, We would also want to find the statistical significance between China and the rest of the overseas returns,
               but without individual data per country, it would be unfair. The next best option would be to see the significance between
               US and China returns. <br/><br/> <p align="center"><img src="DensityCharts.png", width = "40%"><br/>
               <font size="1"><i>DENSITY CHART: China returns are colored RED while US returns are in BLUE<br/>
              <br/> <img src="Ttests.png", height = "50%", width="50%"></br>A two sampled T-test shows that their means are significantly different.</font></i></p><br/>
              While in general, China box office numbers are still lower, we cannot deny that it is growing. It does not hurt that some films even 
              earn more in China. If it means that Hollywood films become more inclusive of other nations and cultures,
              there is no harm in watching this trend continue.<br/><br/><br/>
               <p align="right"><i>"China is a sleeping giant. Let her sleep, for when she wakes she will move the world."<br/>
               - Napoleon Bonaparte</i></p><br/><br/><br/>'))
    
  })
  output$openingtext <- renderUI({
    HTML(paste('<p align="center"><br/><br/>There has been a recent trend of Hollywood films shying away from the
               traditional western narrative and beginning to embrace diversity of cultures. This globalization can 
               be seen in films such as The Meg, Crazy Rich Asians, The Martian, Now You See Me 2, The Transformers films, etc.
                This study aims to explore the growing overseas box office returns, particularly with one of the 
               fastest rising film markets today, China. </p>'))
    
  })
  output$abouttext <- renderUI({
    HTML(paste("<br/><br/><i>Ira Villar is a Data Science Fellow at the NYC Data Science Academy. 
               He has a bachelor's degree in Biology and Chemistry. With his 10 years of experience in the film industry, 
               he tries to dissect and analyze the current trend of
               studios trying to appeal their films to China.</i><br/><br/>"))
  })
  
  output$BarChart <- renderGvis({
    UsChina %>%
      filter(Studio == input$studio) %>%
      arrange(-Worldwide.Opening) %>%
      slice(1:9) -> UsChinaFiltered
      
    opendf = data.frame(
      Title = UsChinaFiltered$Title,
      Domestic_Opening = UsChinaFiltered$Domestic.Opening,
      "Chinese_Opening" = UsChinaFiltered$Chinese.Opening
    )
    
    gvisBarChart(
      opendf,
      xvar = "Title",
      yvar = c("Domestic_Opening", "Chinese_Opening"),
      options = list(legend = "none", title = "Opening Weekend in Millions($)", height =
                       500)
    )
  })
  
  
  output$PieChart <- renderGvis({
    domestic_opening = temp %>%
      mutate(OverseasTotalRegion = "Domestic") %>%
      select(Year, OverseasTotalRegion, Overseasfactor = Domestic) %>%
      group_by(Year, OverseasTotalRegion) %>%
      filter(Year == input$select) %>%
      filter(Overseasfactor != is.na(Overseasfactor)) %>%
      summarise(sum(Overseasfactor))
    
    
    chinaoverall_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>%
      filter(Year == input$select) %>%
      filter(OverseasTotalRegion == "China") %>%
      summarise(sum(Overseasfactor))
    
    overseas_others_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>%
      filter(Year == input$select) %>%
      filter(OverseasTotalRegion == "all_other_overseas_gross") %>%
      summarise(sum(Overseasfactor))
    
    exceptChinadf = data.frame(rbind(overseas_others_df, domestic_opening),
                               stringsAsFactors = F)
    exceptChinadf = tbl_df(exceptChinadf)
    exceptChinadf[nrow(exceptChinadf) + 1,] = chinaoverall_df
    
    percpie = exceptChinadf %>%
      select(-Year)
    percpie = tbl_df(percpie)
    
    
    gvisPieChart(
      data = percpie,
      labelvar = "OverseasTotalRegion",
      numvar = "sum.OverseasFactor.",
      options = list(legend = "none", title = "Percentage of Total Gross per Year \n Values in Millions($)", height = 500)
    )
    
  })
  
  output$PieChartOverseas <- renderGvis({
    chinaoverall_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>%
      filter(Year == input$select) %>%
      filter(OverseasTotalRegion == "China") %>%
      summarise(sum(Overseasfactor))
    
    overseas_others_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>%
      filter(Year == input$select) %>%
      filter(OverseasTotalRegion == "all_other_overseas_gross") %>%
      summarise(sum(Overseasfactor))
    
    
    percpieoverseas = data.frame(rbind(overseas_others_df, chinaoverall_df),
                                 stringsAsFactors = F)
    percpieoverseas = tbl_df(percpieoverseas)
    percpieoverseas = percpieoverseas %>%
      select(-Year)
    
    
    gvisPieChart(
      data = percpieoverseas,
      labelvar = "OverseasTotalRegion",
      numvar = "sum.OverseasFactor.",
      options = list(legend = "none", title = "Percentage of Overseas Gross per Year \n Values in Millions($)", height = 500)
    )
  })
  
  output$Line <- renderGvis({
    UsChinaCompare = UsChinaCompare %>%
      group_by(Year) %>%
      filter(Year %in% c(input$YearRange[1]:input$YearRange[2])) %>%
      mutate(Domestic_Total = sum(Domestic)) %>%
      mutate(Chinese_Total = sum(China)) %>%
      filter(Domestic_Total != is.na(Domestic_Total)) %>%
      arrange(Year) %>%
      select(Year, Domestic_Total, Chinese_Total)
    
    gvisLineChart(data.frame(
      Year = (UsChinaCompare$Year),
      DomesticTotal = (UsChinaCompare$Domestic_Total),
      ChineseTotal = (UsChinaCompare$Chinese_Total)
    ),options=list(legend="none") )
  })
  output$CinemaLine <- renderGvis({
    ns = ns %>%
      filter(Year %in% c(input$CinemaRange[1]:input$CinemaRange[2])) %>%
      mutate(Year = as.character(Year))
    
    
    
    gvisLineChart(ns,options=list(legend="none"))
  })
  
  output$StatChart <- renderGvis({
    gvisLineChart(cs,options=list(
      explorer="{actions: ['dragToZoom','rightClickToReset'],maxZoomIn:0.05}",
      chartArea="{width:'85%',height:'80%'}",
      hAxis="{title: 'YEAR', titleTextStyle: {color: '#000000'}}",
      vAxis="{title: 'COUNT',titleTextStyle: {color: '#000000'}}",
      title="Number of Films by Genre",
      width=750, height=300,
      legend="none", gvis.editor="Edit me!"),
      chartid="ZoomZoom")
  })
  
  # output$table <- DT::renderDataTable({
  #   datatable(cs) 
  # })
  
  output$table2 <- DT::renderDataTable({
    datatable(UsChinaCompare3) })
   
  output$ScatterChart <- renderPlotly({
    UsChinaCompare2 = tbl_df(UsChinaCompare2)
    
    UsChinaCompare2$Region = as.factor(UsChinaCompare2$Region)
   
    plot_ly(data = UsChinaCompare2,
            x = ~Title,
            y = ~Return,
            type = "scatter",
            color = ~Region,
            colors = "RdYlBu",
            hoverinfo = 'text',
            text = ~paste(Title,
                          '</br></br>Region: ', Region,
                          '</br>Domestic Returns: ', Domestic_Returns,
                          '</br>Chinese Returns: ', China_Returns,
                          '</br>Worldwide Returns: ', Total_Worldwide_Gross)) %>%
           layout(xaxis= list(showticklabels = FALSE))
  })
  
  
  
})






