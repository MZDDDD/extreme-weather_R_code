prepare_data <- function(
  demo_dir = "demo_data",
  n = 100,
  lag_n = t(data.frame(c(1,2,3,4,5)))
) {

  library(magrittr)
  library(dplyr)
  library(lubridate)
  library(gtools)
  library(stringr)
  library(tidyverse)

  options(scipen  = 16)

  # file names
  name_ew <- "ex_weather.csv"
  name_d  <- "disease.csv"
  name_cov <- "covid-19-stringency-index.csv"
  name_holiday <- "holidays.csv"
  name_cv <- "covarites.csv"

  # ---------- load extreme weather ----------
  dt_ew <- readr::read_csv(file.path(demo_dir, name_ew), show_col_types = FALSE) %>%
    mutate(mon1 = ifelse(Month %in% c(1:9),paste0(0,Month),Month),
           day1 = ifelse(Day %in% c(1:9),paste0(0,Day),Day))%>%
    mutate(X = paste0(citycode,Year,mon1,day1),
           X1 = paste0(Year,mon1,day1),
           date = format(paste(Year,Month,Day,sep = "-"),format="%Y-%m-%d"))%>%
    select(X,X1,citycode,Year,Month,Day,date,Pre,p1)%>%
    filter(Year>2003) %>%
    rename(p1_Pre = p1)

  # ---------- load Cov index ----------
  dt_cov <- read.csv(file.path(demo_dir, name_cov)) %>%
    mutate(date = as.Date(Day, origin = "1970-1-1"),
           cov_index = Stringency.index..non.vaccinated.) %>%
    select(date, cov_index)

  # ---------- load holidays ----------
  dt_holidays <- read.csv(file.path(demo_dir, name_holiday)) %>%
    mutate(date = as.Date(date, origin = "1970-1-1")) %>%
    select(date, legal_holidays)

  # ---------- produce week lag calendar ----------
  Date <- data.frame(date = as.Date(paste(2004,1,1,sep="-")):as.Date(paste(2022,12,31,sep="-")))
  Date$date <- as.Date(Date$date, origin = "1970-1-1")
  Date <- Date %>%
    mutate(week = isoweek(date),
           month = month(date)) %>%
    mutate(year = case_when(
      month>11 & week<2 ~ year(date)+1,
      month<2 & week>50 ~ year(date)-1,
      TRUE ~ year(date)
    )) %>%
    mutate(yearweek = ifelse(week<10,
                             paste0(year,"-0",week),
                             paste0(year,"-",week))) %>%
    select(date, yearweek)

  Date_cov_holidays <- merge(Date, dt_cov, by = "date", all.x = TRUE) %>%
    mutate(cov_index = ifelse(is.na(cov_index), 0, cov_index)) %>%
    merge(., dt_holidays, by = "date", all.x = TRUE) %>%
    filter(year(date) > 2012)

  # ---------- load covariates ----------
  dt_cv <- read.csv(file.path(demo_dir, name_cv)) %>%
    mutate(gdp = gdp/10000) %>%
    mutate(d_citycode_yearweek = paste0(citycode,"-",yearweek)) %>%
    select(d_citycode_yearweek, everything())

  # ---------- load diseases ----------
  dt_d <- readr::read_csv(file.path(demo_dir, name_d), show_col_types = FALSE) %>%
    mutate(
      mon1 = ifelse(Month %in% c(1:9), paste0(0, Month), Month),
      day1 = ifelse(Day %in% c(1:9), paste0(0, Day), Day)
    ) %>%
    mutate(X = paste0(citycode, Year, mon1, day1)) %>%
    select(X, citycode, Year, Month, Day, VAR21, incidence_n)

  city_code <- dt_d %>%
    group_by(citycode) %>%
    summarise(incidence_n_y = sum(incidence_n), .groups="drop") %>%
    filter(incidence_n_y > n) %>%
    pull(citycode)

  dt_ew_d <- merge(dt_ew, dt_d, by = "X", all.x = TRUE) %>%
    select(-ends_with(".x")) %>%
    rename_with(~ gsub("\\.y$", "", .x)) %>%
    filter(!is.na(incidence_n)) %>%
    mutate(
      incidence_n = ifelse(is.na(incidence_n), 0, incidence_n),
      date = as.Date(paste(Year,Month,Day,sep="-"))
    ) %>%
    merge(., Date_cov_holidays, by.x = "date", by.y = "date", all.x = TRUE) %>%
    filter(citycode %in% city_code)

  # ---- return everything needed downstream ----
  return(list(
    dt_ew   = dt_ew, 
    dt_ew_d = dt_ew_d,
    dt_cv   = dt_cv,
    lag_n   = lag_n,
    n       = n
  ))
}

