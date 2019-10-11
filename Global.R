library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rsconnect)
library(scales)
library(googleVis)
library(DT)
library(lubridate)
library(shinydashboard)


# Number of Screens Cleaning ####

ns = read.csv("NumberofScreens.csv",stringsAsFactors = FALSE)
ns = tbl_df(ns)
ns = ns %>% 
  filter(Year > 2004) %>% 
  select(Year,Number.of.screens)

ns$Number.of.screens = Number.of.screens = as.numeric(gsub("[[:punct:]]","",ns$Number.of.screens))

ns$Number.of.screens[ns$Year == 2012] = ((ns$Number.of.screens[ns$Year == 2011] + ns$Number.of.screens[ns$Year == 2013])/2)

# China Cleaning ####
co = read.csv("ChinaOpening.csv",stringsAsFactors = FALSE)
co = tbl_df(co)
co %>% 
  select(Title = Movie.Title, Chinese.Opening = Opening, everything(),percent_of_total = X..of.Total, Total_Chinese_Gross = Total.Gross) -> co

co$Total_Chinese_Gross = Total_Chinese_Gross = as.numeric(gsub("[\\$,]", "", co$Total_Chinese_Gross))
co$Chinese.Opening = Chinese.Opening = as.numeric(gsub("[\\$,]", "", co$Chinese.Opening))
co$percent_of_total = percent_of_total = as.numeric(gsub("[[:punct:]]","",co$percent_of_total))

co %>% 
  mutate(Chinese.Opening = Chinese.Opening /1000000) %>% 
  mutate(Total_Chinese_Gross = Total_Chinese_Gross/1000000) %>% 
  mutate(percent_of_total = percent_of_total/10) -> co


# Opening Worldwide Cleaning ####
wo = read.csv("WorldwideOpening.csv",stringsAsFactors = FALSE)
wo = tbl_df(wo)
wo %>% 
  rename(domestic_opening_percent = DO..,
         overseas_opening_percent = OO..) -> wo
head(wo)

wo$Worldwide.Opening = Worldwide.Opening = as.numeric(gsub("[\\$,]", "", wo$Worldwide.Opening))
wo$Domestic.Opening = Domestic.Opening = as.numeric(gsub("[\\$,]", "", wo$Domestic.Opening))
wo$Overseas.Opening = Overseas.Opening = as.numeric(gsub("[\\$,]", "", wo$Overseas.Opening))

wo$domestic_opening_percent = domestic_opening_percent = as.numeric(gsub("[[:punct:]]","",wo$domestic_opening_percent))
wo$overseas_opening_percent = overseas_opening_percent = as.numeric(gsub("[[:punct:]]","",wo$overseas_opening_percent))

wo %>% 
  mutate(domestic_opening_percent = domestic_opening_percent/10) %>% 
  mutate(overseas_opening_percent = overseas_opening_percent/10) -> wo

# Overall Worldwide Cleaning ####
wa1 = read.csv("WorldwideAlltime.csv",stringsAsFactors = FALSE)
wa1 = tbl_df(wa1)

wa2 = read.csv("WorldwideAlltime2.csv",stringsAsFactors = FALSE)
wa2 = tbl_df(wa2)
wa = rbind()

wa3 = read.csv("WorldwideAlltime3.csv",stringsAsFactors = FALSE)
wa3 = tbl_df(wa3)

wa4 = read.csv("WorldwideAlltime4.csv",stringsAsFactors = FALSE)
wa4 = tbl_df(wa4)

wa5 = read.csv("WorldwideAlltime5.csv",stringsAsFactors = FALSE)
wa5 = tbl_df(wa5)

wa6 = read.csv("WorldwideAlltime6.csv",stringsAsFactors = FALSE)
wa6 = tbl_df(wa6)

wa7 = read.csv("WorldwideAlltime7.csv",stringsAsFactors = FALSE)
wa7 = tbl_df(wa7)

wa8 = read.csv("WorldwideAlltime8.csv",stringsAsFactors = FALSE)
wa8 = tbl_df(wa8)

Worldwide.Alltime = do.call("rbind", list(wa1,wa2,wa3,wa4,wa5,wa6,wa7,wa8))
Worldwide.Alltime = tbl_df(Worldwide.Alltime)


colnames(Worldwide.Alltime)
Worldwide.Alltime %>% 
  rename(domestic_percent = X.,
         overseas_percent = X..1,
         Year = Year.,
         Total_Worldwide_Gross = Worldwide) -> Worldwide.Alltime

Worldwide.Alltime$Total_Worldwide_Gross = Total_Worldwide_Gross = as.numeric(gsub("[\\$,]", "", Worldwide.Alltime$Total_Worldwide_Gross))
Worldwide.Alltime$Domestic = Domestic = as.numeric(gsub("[\\$,]", "", Worldwide.Alltime$Domestic))
Worldwide.Alltime$Overseas = Overseas = as.numeric(gsub("[\\$,]", "", Worldwide.Alltime$Overseas))


Worldwide.Alltime$domestic_percent = wa$domestic_percent = as.numeric(gsub("[[:punct:]]","",Worldwide.Alltime$domestic_percent))
Worldwide.Alltime$overseas_percent = wa$overseas_percent = as.numeric(gsub("[[:punct:]]","",Worldwide.Alltime$overseas_percent))
Worldwide.Alltime$Year = Worldwide.Alltime$Year = as.numeric(gsub("[[:punct:]]","",Worldwide.Alltime$Year))

Worldwide.Alltime %>% 
  mutate(domestic_percent = domestic_percent / 10) %>% 
  mutate(overseas_percent = overseas_percent/10) -> Worldwide.Alltime

# UsChinaOpening Comparison ####

UsChina <-merge(x=wo,y=co,by="Title",all.x=TRUE, na.rm = TRUE)
UsChina %>% 
  select(-Rank.y) -> UsChina
UsChina = tbl_df(UsChina)
UsChina %>% 
  select(Rank = Rank.x,everything()) %>%
  filter(Total_Chinese_Gross != is.na(Total_Chinese_Gross)) -> UsChina

# UsChina %>% 
#   filter(Rank <=10) -> UsChinaFiltered
# 
# opendf=data.frame(Title=UsChinaFiltered$Title, 
#               Domestic_Opening=UsChinaFiltered$Domestic.Opening, 
#               Chinese_Opening=UsChinaFiltered$Chinese.Opening)


UsChina %>% 
  group_by(Title,Year) %>% 
  filter(Year > 2009) %>% 
  arrange(Year) %>%
  ggplot(aes(x=Year,y=OpeningComparison)) + geom_point(aes(color=region),position = "jitter")


# Overall Comparison ####

UsChinaAlltime <- merge(x=Worldwide.Alltime,y=co,by="Title",all.x=TRUE, na.rm = TRUE)

UsChinaAlltime %>% 
  filter(Chinese.Opening != is.na(Chinese.Opening)) %>% 
  select(Rank = Rank.x,-Rank.y,everything()) -> UsChinaAlltime

UsChinaAlltime = tbl_df(UsChinaAlltime)
UsChinaAlltime = UsChinaAlltime %>% 
  mutate(all_other_overseas_gross = Total_Worldwide_Gross - (Total_Chinese_Gross + Domestic))

UsChinaAlltime2 = UsChinaAlltime %>% 
  gather(key = "OverseasTotalRegion",value ="Overseasfactor",Total_Chinese_Gross,all_other_overseas_gross ) %>% 
  filter(Overseasfactor != is.na(Overseasfactor))
 

UsChinaAlltime
UsChinaAlltime2  %>% 
  select(Title, Year,Total_Worldwide_Gross,Domestic,OverseasTotalRegion,Overseasfactor,Overseas) %>%
  group_by(Year) %>%
  mutate(TWGperYear = sum(Total_Worldwide_Gross)) %>% 
  group_by(Year, OverseasTotalRegion, TWGperYear)-> temp
temp = tbl_df(temp)


UsChinaCompare %>% 
  group_by(Year) %>% 
  filter(Year %in% c(2006:2019)) %>% 
  mutate(Domestic_Total = sum(Domestic)) %>%
  mutate(Chinese_Total = sum(Total_Chinese_Gross)) %>% 
  filter(Domestic_Total != is.na(Domestic_Total)) %>%
  arrange(Year) %>% 
  select(Year,Domestic_Total,Chinese_Total)


# temp %>% 
#   filter(OverseasTotalRegion == "Total_Chinese_Gross") %>% 
#   select(Overseasfactor) ->ChineseMean
#   
# temp %>% 
#   filter(OverseasTotalRegion=="all_other_overseas_gross") %>% 
#   select(Overseasfactor)-> OverseasMean
# 
# OverseasMean = as.vector(OverseasMean$Overseasfactor)
# ChineseMean = as.vector(ChineseMean$Overseasfactor)
#   
# t.test(ChineseMean,OverseasMean,alternative = "two.sided")






