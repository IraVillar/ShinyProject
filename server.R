


shinyServer(function(input, output) {
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
      options = list(title = "Opening Weekend in Millions($)", height =
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
      options = list(title = "Percentage of Total Gross per Year \n Values in Millions($)", height = 500)
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
      options = list(title = "Percentage of Overseas Gross per Year \n Values in Millions($)", height = 500)
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
    ), )
  })
  output$CinemaLine <- renderGvis({
    ns = ns %>%
      filter(Year %in% c(input$CinemaRange[1]:input$CinemaRange[2])) %>%
      mutate(Year = as.character(Year))
    
    
    
    gvisLineChart(ns)
  })
  
  output$StatChart <- renderGvis({
    gvisLineChart(cs,options=list(
      explorer="{actions: ['dragToZoom','rightClickToReset'],maxZoomIn:0.05}",
      chartArea="{width:'85%',height:'80%'}",
      hAxis="{title: 'YEAR', titleTextStyle: {color: '#000000'}}",
      vAxis="{title: 'COUNT',titleTextStyle: {color: '#000000'}}",
      title="Number of Films by Genre",
      width=750, height=300,
      legend="none"),
      chartid="ZoomZoom")
  })
  
  output$table <- DT::renderDataTable({
    datatable(cs) 
  })
   
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






