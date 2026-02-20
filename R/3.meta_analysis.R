library(magrittr)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(stringr)
library(gnm)
library(mixmeta)


#meta analysis
attach(meta_m)
meta_m <- meta_m[which(abs(Beta)>0),]
detach()


model_m_coef <- tryCatch(
  {mixmeta(Beta,var, data = meta_m, method = "reml")},error = function(e){  #,random =  ~ 1|lag
    if(grepl("Error in ",e$message)){
      message("detetct error")
      return(NULL)
    }else{
      "error"
    }
  }
)

m_m_s_coef <- tryCatch(
  {c(exp(summary(model_m_coef)[["coefficients"]][1,c(-2,-3,-4)]),summary(model_m_coef)[["coefficients"]][1,4])},error = function(e){
    if(grepl("Error in ",e$message)){
      message("detetct error")
      return(NULL)
    }else{
      "error"
    }
  }
)

RR_all1 <- as.data.frame(t(m_m_s_coef)) %>% 
  mutate(type = "RDs",
         name = "FLU")
names(RR_all1) <- c("RRall","RRalllow","RRallhigh","p_value","type","name")
