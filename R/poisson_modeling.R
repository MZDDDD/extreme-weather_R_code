run_poisson <- function(dt_ew_d, dt_cv, lag_n, disease_name = "FLU") {

  qaic <- function(model){
    loglik <- sum(dpois(model$y, model$fitted.values, log = TRUE))
    phi <- summary(model)$dispersion
    qaic0 <- -2 * loglik + 2 * phi * summary(model)$df[3]
    return(qaic0)
  }

  fx <- as.formula(case_wn~offset(log(pop))+ew_lag_01+legal_holidays+
                     gdp+pd+cov_index)

  library(magrittr)
  library(dplyr)
  library(stringr)
  library(tidyverse)
  library(lubridate)
  library(gnm)
  library(mixmeta)

dt_d <- dt_ew_d
dt_ew_d <- dt_d%>%
  mutate(week = as.numeric(str_sub(yearweek,6,7)))

# case crossover design 
dt_ew_1_inf<-dt_ew_d %>% 
  filter(Year>2012) %>% 
  filter(!(Year==2013&Month<7)) %>% 
  group_by(citycode,yearweek) %>% 
  summarise(p1=sum(p1_Pre)) %>% 
  filter(p1>0) %>%
  select(citycode,yearweek,p1)%>% 
  mutate(week = as.numeric(str_sub(yearweek,6,7)))
if (nrow(dt_ew_1_inf) == 0) {
  return(list(coef_m = NULL, meta_m = NULL))
}
v_ew_1_week<-sort(unique(as.numeric(str_sub(dt_ew_1_inf$yearweek,6,7)))) 

dt_ew_0_sup<-dt_ew_d %>% 
  filter(Year>2012) %>%
  filter(!(Year==2013&Month<7)) %>% 
  filter(week %in% v_ew_1_week) %>% 
  group_by(citycode,yearweek) %>% 
  summarise(
    p1 = sum(p1_Pre)
  ) %>% 
  filter(p1==0) %>%                                            
  select(citycode,yearweek,p1) %>% 
  mutate(week = as.numeric(str_sub(yearweek,6,7)))

case_control_list<-list()

t_n_0<-1
for(case1 in 1:nrow(dt_ew_1_inf)){
  t1<-dt_ew_1_inf[case1,1]$citycode    
  t2<-dt_ew_1_inf[case1,4]$week
  dt_sup_t<-dt_ew_0_sup %>% 
    filter(citycode == t1) %>% 
    filter(week == t2)
  if(nrow(dt_sup_t)==0){
    print("11")
    next
  }else{
    case_control_list[[case1]]<-rbind(dt_ew_1_inf[case1,],dt_sup_t) %>% 
      mutate(t_num_0 = t_n_0)
    t_n_0 = t_n_0+1
    names(case_control_list)[case1]<-paste0(dt_ew_1_inf[case1,1]$citycode,"-",dt_ew_1_inf[case1,2]$yearweek)
  }
  
}

dt_case_control<-do.call(rbind,case_control_list)
dt_case_control1<-dt_case_control %>% 
  mutate(ew_citycode_yearweek = paste0(citycode,"-",yearweek)) %>% 
  select(ew_citycode_yearweek,t_num_0) 

dt_ew_d2<-dt_ew_d%>%
  filter(Year>2012) %>% 
  group_by(citycode,yearweek)%>%
  summarise(
    Pre = mean(Pre),
    p1=sum(p1_Pre),
    case_wn=sum(incidence_n),
    pre = mean(Pre),
    cov_index = mean(cov_index),
    legal_holidays = sum(legal_holidays)  
  )%>%
  ungroup() %>% 
  mutate(d_citycode_yearweek = paste0(citycode,"-",yearweek)) %>% 
  select(d_citycode_yearweek,everything()) 


  
coef_m<- as.data.frame(matrix(0,nrow = ncol(lag_n),ncol = 10)) 
names(coef_m)<-c("region","type","name","c_name","lag","RR","RRlow","RRhigh","p_value","qaic"
)
meta_m<-as.data.frame(matrix(0,length(lag_n),2))
names(meta_m)<-c("Beta","var")
  
dt_cv2 <- dt_cv %>%
  mutate(
    key_raw = str_trim(as.character(d_citycode_yearweek)),
    city = str_sub(key_raw, 1, 6),
    cityyear = str_extract(key_raw, "(?<=-)\\d+(?=-)"),  
    year = str_sub(cityyear, 7, 10),                    
    week = str_sub(key_raw, -2, -1),                   
    week = as.character(as.integer(week)),            
    d_citycode_yearweek = str_c(city, "-", year, "-", week)
  ) %>%
  select(-key_raw, -cityyear) 

disease_name <- "FLU" 
coef_m <- data.frame(
  disease = rep(NA_character_, ncol(lag_n)),
  lag     = rep(NA_integer_,   ncol(lag_n)),
  RR      = rep(NA_real_,      ncol(lag_n)),
  RR_low  = rep(NA_real_,      ncol(lag_n)),
  RR_high = rep(NA_real_,      ncol(lag_n)),
  p       = rep(NA_real_,      ncol(lag_n)),
  Qaic    = rep(NA_real_,      ncol(lag_n))
)
#conditionnal pooisson model  
for(jj in 1:ncol(lag_n)){
  
  lag <- lag_n[1, jj]
  
  dt_ew_d1 <- dt_ew_d2%>%
    mutate(ew_citycode_yearweek = lag(d_citycode_yearweek,lag),
           ew_lag_num = lag(p1,lag),
           lag_case = lag(case_wn,1)) %>% 
    mutate(ew_lag_01 = ifelse(ew_lag_num>0,1,0)) %>% 
    select(ew_citycode_yearweek,d_citycode_yearweek,case_wn,ew_lag_01,ew_lag_num,lag_case,Pre,pre,cov_index,legal_holidays)
  
  dt_ew_dd<-merge(dt_case_control1,dt_ew_d1,by.x = "ew_citycode_yearweek",by.y="ew_citycode_yearweek",all.x = TRUE)  
  dt_ew_dd1<-na.omit(dt_ew_dd) %>% 
    mutate(ew_citycode = as.numeric(str_sub(ew_citycode_yearweek,1,6)),
           d_citycode = as.numeric(str_sub(d_citycode_yearweek,1,6)),
           d_year = as.numeric(str_sub(d_citycode_yearweek,8,11))) %>% 
    filter(ew_citycode==d_citycode) %>%
    filter(d_year > 2013) %>% 
    group_by(t_num_0) %>% 
    mutate(t_count = n()) %>% 
    ungroup() %>% 
    filter(t_count>1)
  
  t_num_e<-dt_ew_dd1 %>% 
    group_by(t_num_0) %>% 
    summarise(p1 = sum(ew_lag_01)) %>% 
    filter(p1==0) %>% 
    select(t_num_0)
  
  dt_ew_dd2<-dt_ew_dd1 %>% 
    filter(!(t_num_0 %in% t_num_e$t_num_0))
  
  dt_ew_d_t <- dt_ew_dd2 %>%
    mutate(d_citycode_yearweek = str_trim(as.character(d_citycode_yearweek))) %>%
    left_join(dt_cv2, by = "d_citycode_yearweek") %>%
    mutate(
      week = as.integer(str_extract(d_citycode_yearweek, "\\d+$")),
      legal_holidays = ifelse(legal_holidays > 0, 1, 0)
    )
  

  model1<-tryCatch(
    {gnm(fx,
         data = dt_ew_d_t, family = quasipoisson(link = "log"),  #quasipoisson
         eliminate = factor(t_num_0))},error = function(e){
           if(grepl("Error in ",e$message)){
             message("detetct error")
             return(NULL)
           }else{
             "error"
           }
         }
  )
  

  model1_s<-tryCatch(
    {summary(model1)},error = function(e){
      if(grepl("Error in ",e$message)){
        message("detetct error")
        return(NULL)
      }else{
        "error"
      }
    }
  )

  p <- model1_s$coefficients[1,4] 
  
  CI<-tryCatch(
    {confint(model1,parm = c(1))},error = function(e){
      if(grepl("Error in ",e$message)){
        message("detetct error")
        return(NULL)
      }else{
        "error"
      }
    }
  )

  
  meta_m[jj,1] <- model1_s$coefficients[1,1]
  meta_m[jj,2] <- vcov(model1)[1,1]
  
  C_m<-exp(c(model1$coefficients[1],CI[1],CI[2]))
#calculate Qaic  
  Qaic <- qaic(model1)
  c_a <- c(disease_name, lag, C_m, p, Qaic)
  coef_m$disease[jj] <- disease_name
  coef_m$lag[jj]     <- lag
  coef_m$RR[jj]      <- C_m[1]
  coef_m$RR_low[jj]  <- C_m[2]
  coef_m$RR_high[jj] <- C_m[3]
  coef_m$p[jj]       <- p
  coef_m$Qaic[jj]    <- Qaic
}
  return(list(coef_m = coef_m, meta_m = meta_m))
}






