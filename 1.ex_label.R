# 加载程序包
library(openxlsx)
library(magrittr)
library(dplyr)
library(lubridate) 
library(gtools)
library(stringr)
library(tidyverse)
library(readxl)
library(zoo)
library(officer) 
library(carData)

dir_ew_origin <- paste0("D:/extreme weather/")
dir_save <- "D:/extreme weather/" 

name_ew_origin <- paste0("tem.csv")



#load data
origin_dt <- read.csv(paste0(dir_ew_origin,name_ew_origin))[,-1]

origin_dt$p1 <- 0 

#citycode list
city_code_all <- unique(origin_dt$citycode)
city_list<-list()
dl <- 2 
prob <- 0.95

for(i0 in 1:length(city_code_all)){

  i_citycode <- city_code_all[i0]
  origin_dt1 <- origin_dt[which(origin_dt$citycode==i_citycode),]
  
  
  q <- quantile(origin_dt1[,5],prob)

  t=1
  gate=0
  while(t<=nrow(origin_dt1)){
    
    if(t+dl-1>nrow(origin_dt1)){
      t <- t+1
      next
    }
    
    for(i1 in t:(t+dl-1)){
      if(origin_dt1[i1,5] > q){     #Heatwave/extreme precipitation: >;Cold spell: <      
        gate <- gate +1
      }
    }
    
    if(gate < dl){
      
      gate <- 0
      t <- t+1
      next
      
    }else{                 

      for(i2 in t:(t+gate-1)){
        origin_dt1[i2,6] <- 1
      }
 
      while (TRUE) {

        if((t+gate) > nrow(origin_dt1)){
          break
        }

        if((t+gate) <= nrow(origin_dt1)){
          if(origin_dt1[t+gate,5] > q){     #Heatwave/extreme precipitation: >;Cold spell: < 
            origin_dt1[t+gate,6] <- 1
            gate <- gate +1
          }else{
            break
          }
        }
      }
      
      t <- t+gate+1
      gate <- 0
      
      next
    }
  }
  
  city_list[[i0]] <- origin_dt1
  
}

##组合数据
aim_dt <- city_list[[1]]
for(i3 in 2:length(city_code_all)){
  aim_dt <- rbind(aim_dt,city_list[[i3]])
}

write.csv(aim_dt,file = paste0(dir_save,".csv"))
 




