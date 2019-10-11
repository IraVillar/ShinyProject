
shinyServer(function(input,output){
  
  output$BarChart <- renderGvis({
    UsChina %>% 
      filter(Studio == input$studio) %>% 
      arrange(-Worldwide.Opening) %>% 
      slice(1:9) -> UsChinaFiltered
    
    opendf=data.frame(Title=UsChinaFiltered$Title,
                      Domestic_Opening=UsChinaFiltered$Domestic.Opening, 
                      Chinese_Opening=UsChinaFiltered$Chinese.Opening)
    
    gvisBarChart(opendf, xvar="Title",yvar = c("Domestic_Opening","Chinese_Opening"),
                 options = list(title = "Opening in Millions", height=500))
  })
  
  
  output$PieChart <- renderGvis({
      domestic_opening = temp %>%
      mutate(OverseasTotalRegion = "Domestic") %>% 
      select(Year,OverseasTotalRegion,Overseasfactor = Domestic) %>% 
      group_by(Year,OverseasTotalRegion) %>% 
      filter(Year == input$select) %>%
      filter(Overseasfactor != is.na(Overseasfactor)) %>% 
      summarise(sum(Overseasfactor))
    
    
    chinaoverall_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>% 
      filter(Year == input$select) %>% 
      filter(OverseasTotalRegion == "Total_Chinese_Gross") %>% 
      summarise(sum(Overseasfactor))
    
    overseas_others_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>% 
      filter(Year == input$select) %>% 
      filter(OverseasTotalRegion == "all_other_overseas_gross") %>% 
      summarise(sum(Overseasfactor))
    
    exceptChinadf = data.frame(rbind(overseas_others_df,domestic_opening),stringsAsFactors = F)
    exceptChinadf = tbl_df(exceptChinadf)
    exceptChinadf[nrow(exceptChinadf)+1,] = chinaoverall_df
    
    percpie = exceptChinadf %>% 
      select(-Year)
    percpie = tbl_df(percpie)
    
    
    gvisPieChart(data = percpie, labelvar = "OverseasTotalRegion", numvar = "sum.OverseasFactor.",
                 options=list(title="Percentage of Total Gross per Year \n Values in Millions", height = 500))
    
    })
  
  output$PieChartOverseas <- renderGvis({
    
    chinaoverall_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>% 
      filter(Year == input$select) %>% 
      filter(OverseasTotalRegion == "Total_Chinese_Gross") %>% 
      summarise(sum(Overseasfactor))
    
    overseas_others_df = temp %>%
      group_by(Year, OverseasTotalRegion) %>% 
      filter(Year == input$select) %>% 
      filter(OverseasTotalRegion == "all_other_overseas_gross") %>% 
      summarise(sum(Overseasfactor))
    
    
    percpieoverseas = data.frame(rbind(overseas_others_df,chinaoverall_df),stringsAsFactors = F)
    percpieoverseas = tbl_df(percpieoverseas)
    percpieoverseas = percpieoverseas %>% 
      select(-Year)
    
    
    gvisPieChart(data = percpieoverseas, labelvar = "OverseasTotalRegion", numvar = "sum.OverseasFactor.",
                 options=list(title="Percentage of Overseas Gross per Year \n Values in Millions", height = 500))})
    
    output$Line <- renderGvis({
      UsChinaCompare = UsChinaCompare %>% 
        group_by(Year) %>% 
        filter(Year %in% c(input$YearRange[1]:input$YearRange[2])) %>% 
        mutate(Domestic_Total = sum(Domestic)) %>%
        mutate(Chinese_Total = sum(Total_Chinese_Gross)) %>% 
        filter(Domestic_Total != is.na(Domestic_Total)) %>%
        arrange(Year) %>% 
        select(Year,Domestic_Total,Chinese_Total)
        
                    gvisLineChart(data.frame(country=(UsChinaCompare$Year), 
                                       DomesticTotal=(UsChinaCompare$Domestic_Total), 
                                       ChineseTotal=(UsChinaCompare$Chinese_Total)),
                                )
    })
    
    
    output$CinemaLine <- renderGvis({
      ns = ns %>% 
        filter(Year %in% c(input$CinemaRange[1]:input$CinemaRange[2])) %>% 
        mutate(Year = as.character(Year))
      
      
      
      gvisLineChart(ns,
                    )
    })
    

    
  

  
  # output$table <- DT::renderDataTable(({
  #   datatable(state_stat,rownames=FALSE) %>% 
  #     formatStyle(input$selected,
  #                 background = "skyblue",fontweight="bold")
  })
  
  
