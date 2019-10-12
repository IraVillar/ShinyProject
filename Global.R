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
library(plotly)


# Chinese Stats Cleaning ####

cs = read.csv("ChineseStats.csv",stringsAsFactors = FALSE)
cs = cs %>% select (-Index)
cs = data.frame(cs)
cs = cs %>% select(-2018)
cs = t(cs)
cs = data.frame(cs) %>%
  filter(Indicators != is.na(Indicators)) %>%
  arrange(Indicators)
class(cs)



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
  select(Title = Movie.Title, Chinese.Opening = Opening, everything(),percent_of_total = X..of.Total, China = Total.Gross) -> co

co$China = China = as.numeric(gsub("[\\$,]", "", co$China))
co$Chinese.Opening = Chinese.Opening = as.numeric(gsub("[\\$,]", "", co$Chinese.Opening))
co$percent_of_total = percent_of_total = as.numeric(gsub("[[:punct:]]","",co$percent_of_total))

co %>% 
  mutate(Chinese.Opening = Chinese.Opening /1000000) %>% 
  mutate(China = China/1000000) %>% 
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
  filter(China != is.na(China)) -> UsChina


# Overall Comparison ####

UsChinaAlltime <- merge(x=Worldwide.Alltime,y=co,by="Title",all=TRUE, na.rm = TRUE)

UsChinaAlltime %>% 
  mutate(China = ifelse(is.na(China),1,China)) %>% 
  select(Rank = Rank.x,-Rank.y,everything()) -> UsChinaAlltime

UsChinaAlltime = tbl_df(UsChinaAlltime)
UsChinaAlltime = UsChinaAlltime %>% 
  mutate(all_other_overseas_gross = Total_Worldwide_Gross - (China + Domestic))

UsChinaAlltime2 = UsChinaAlltime %>% 
  gather(key = "OverseasTotalRegion",value ="Overseasfactor",China,all_other_overseas_gross ) %>% 
  filter(Overseasfactor != is.na(Overseasfactor))

UsChinaAlltime2  %>% 
  select(Title, Year,Total_Worldwide_Gross,Domestic,OverseasTotalRegion,Overseasfactor,Overseas) %>%
  group_by(Year) %>%
  mutate(TWGperYear = sum(Total_Worldwide_Gross)) %>% 
  filter(Domestic != is.na(Domestic)) %>% 
  group_by(Year, OverseasTotalRegion, TWGperYear)-> temp
temp = tbl_df(temp)

UsChinaAlltime %>%
  select(-Rank.y) %>% 
  filter(Domestic != is.na(Domestic)) -> UsChinaCompare

UsChinaCompare = UsChinaCompare %>% 
  mutate(Domestic_Returns = Domestic) %>% 
  mutate(China_Returns = China)

UsChinaCompare$China_Returns = as.numeric(UsChinaCompare$China_Returns)
UsChinaCompare$China_Returns = sprintf(UsChinaCompare$China_Returns, fmt = '%#.2f')


UsChinaCompare2 = UsChinaCompare %>% 
  gather(key = "Region", value = "Return", China, Domestic) %>% 
  arrange(desc(China_Returns))


# temp %>% 
#   filter(OverseasTotalRegion == "China") %>% 
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






