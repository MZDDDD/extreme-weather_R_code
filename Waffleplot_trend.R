library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(lubridate) 
library(gtools)
library(stringr)
library(dlnm)
library(mvmeta)
library(splines)
library(waffle)
library(ggtext)
library(trend)


dt_ew <- read.csv("D:/extreme weather/20241231画图/70-22_95%极端天气数据/70-22_2days_5%_coldspell_.csv")%>%
  mutate(mon1 = ifelse(Month %in% c(1:9),paste0(0,Month),Month),
         day1 = ifelse(Day %in% c(1:9),paste0(0,Day),Day))%>%
  mutate(X = paste0(citycode,Year,mon1,day1),
         X1 = paste0(Year,mon1,day1),
         date = as.Date(paste(Year,Month,Day,sep = "-"),origin = "1970-1-1"))%>%
  select(X,X1,citycode,Year,Month,Day,date,tem,p1)


Date<-as.data.frame(c(as.Date(paste(1970,1,1,sep = "-")):
                        as.Date(paste(2022,12,31,sep = "-"))))
names(Date)<-"date"
Date$date<-as.Date(Date$date,origin = "1970-1-1")

Date<-Date%>% 
  mutate(week = isoweek(date),
         month=month(date)
  ) %>%
  mutate(year = case_when(
    month>11&week<2 ~ year(date)+1,
    month<2&week>50 ~ year(date)-1,
    TRUE~year(date)
  ))%>%
  mutate(yearweek=ifelse(week<10,
                         paste0(year,"-0",week),
                         paste0(year,"-",week))) %>% 
  select(date,yearweek)

dt_ew<-dt_ew%>%
  merge(.,Date,by.x = "date",by.y="date",all.x = TRUE)

dt_ew1<-dt_ew%>%
  group_by(yearweek,citycode)%>%
  filter(p1>0)%>%
  mutate(p_sum=sum(p1),
         status = case_when(
           p_sum == 1  ~ "1 day",
           p_sum == 2  ~ "2 days",
           p_sum == 3  ~ "3 days",
           p_sum >= 4 ~ ">=4 days"
         ))%>%
  arrange(yearweek,p1)
dt_ew2 <- dt_ew1 %>%
  filter(Year > 2013) %>%
  group_by(Year, status) %>%
  summarise(n = n(), .groups = 'drop') %>%
  
  group_by(Year) %>%
  mutate(percentage = round(n / sum(n) * 100))


dt_ew2 <- dt_ew2 %>% 
  mutate(status = factor(status, levels = c(">=4 days","3 days","2 days","1 day")))
dt_ew2 <- dt_ew2 %>%
  arrange(factor(status, levels = c(">=4 days","3 days","2 days","1 day")))




df_summary <- dt_ew2 %>%
  group_by(Year) %>%
  summarise(
    total_n = sum(n),
    total_success = sum(ifelse(status %in% c("3 days", ">=4 days"), n, 0))
  )
result <- prop.trend.test(df_summary$total_success, df_summary$total_n)
print(result)

