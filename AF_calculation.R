
#######
library(magrittr)
library(dplyr)
library(lubridate) 
library(gtools)
library(stringr)
library(tidyverse)
library(lubridate)
library(dlnm)

lag_max <- 5 #VB-ZDs:24

dt_year<-data.frame(matrix(2014:2022,9,1))
names(dt_year)<-"YEAR"

dt_ep_ew_a <- dt_ew %>%
  mutate(
    date = as.Date(paste(Year,Month,Day,sep = "-"),origin = "1970-1-1")) %>% 
  merge(.,Date,by.x = "date",by.y="date",all.x = TRUE) %>% 
  arrange(citycode,Year,Month,Day)

dt_ew_ew_b <- dt_ew_ew_b %>%
  arrange(citycode, yearweek) %>%
  group_by(citycode) %>%
  mutate(
    lag5 = lag(p1,1) + lag(p1,2) + lag(p1,3) +
      lag(p1,4) + lag(p1,5)
  ) %>%
  ungroup()

dt_ew_ew_c<- dt_ew_ew_b %>% 
  filter(Year>2013) %>% 
  mutate(lag5 = ifelse(lag5>0,1,0)) %>% 
  mutate(X1 = paste0(citycode,"-",yearweek))

# citycode-year-week
city_yearweek_lag <- c(dt_ew_ew_c %>%
                         ungroup() %>% 
                         filter(lag5>0) %>%  #VB-ZDs:lag24
                         select(X1)) 

influ_num_d <- data.frame(matrix(NA, nrow = ncol(lag_n), ncol = 4))
colnames(influ_num_d) <- c("lag","n_case","n_strata","n_obs")

lag_type <- city_yearweek_lag[[1]] 
  
dt_ew_d <- dt_ew_d%>%
  mutate(week = as.numeric(str_sub(yearweek,6,7)))
dt_influ_all1<- dt_d %>% 
  mutate(YEAR = str_sub(yearweek,1,4)) %>% 
  group_by(YEAR) %>% 
  summarise(all_case = sum(incidence_n)) %>% 
  ungroup()
dt_influ_all<-merge(dt_year,dt_influ_all1,by.x = "YEAR",by.y = "YEAR" , all.x = TRUE) %>% 
  mutate(all_case = ifelse(is.na(all_case),0,all_case))
  
dt_ew_d2<-dt_ew_d%>%
  group_by(citycode,yearweek)%>%
  summarise(
    case_wn=sum(incidence_n)
  )%>%
  ungroup() %>% 
  mutate(X1 = paste0(citycode,"-",yearweek)) %>% 
  filter(X1 %in% lag_type)  

AF <- sum(dt_ew_d2$case_wn) /sum(dt_ew_d$incidence_n)
 
#year AF  
dt_influ_all2<- dt_ew_d2 %>% 
   mutate(YEAR = str_sub(yearweek,1,4)) %>% 
   group_by(YEAR) %>% 
   summarise(all_influ_case = sum(case_wn)) %>% 
   ungroup() %>% 
   arrange(YEAR) 
 

dt_all_inf<-dt_all_inf1 %>% 
  mutate(all_case =ifelse(is.na(all_case),0,all_case),
          all_influ_case = ifelse(is.na(all_influ_case),0,all_influ_case)) %>%
  mutate(AF = all_influ_case/all_case)


