calc_af <- function(dt_ew, dt_d, dt_ew_d, Date, lag_n, lag_max = 5, year_min = 2014) {

  library(dplyr)
  library(stringr)
  library(lubridate)

  ew_week <- dt_ew %>%
    mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    left_join(Date, by = "date") %>%   # Date 提供 yearweek
    group_by(citycode, yearweek) %>%
    summarise(p1 = sum(p1_Pre, na.rm = TRUE), .groups = "drop") %>%
    arrange(citycode, yearweek) %>%
    group_by(citycode) %>%
    mutate(
      lagN = 0
    )

  for (k in 1:lag_max) {
    ew_week <- ew_week %>% mutate(lagN = lagN + dplyr::lag(p1, k))
  }

  ew_week <- ew_week %>%
    ungroup() %>%
    mutate(
      Year = as.integer(str_sub(yearweek, 1, 4)),
      lag_flag = ifelse(lagN > 0, 1, 0),
      X1 = paste0(citycode, "-", yearweek)
    ) %>%
    filter(Year >= year_min)

  lag_type <- ew_week %>% filter(lag_flag == 1) %>% pull(X1)

  case_week <- dt_ew_d %>%
    group_by(citycode, yearweek) %>%
    summarise(case_wn = sum(incidence_n, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      X1 = paste0(citycode, "-", yearweek),
      YEAR = str_sub(yearweek, 1, 4)
    )

  case_week_exp <- case_week %>% filter(X1 %in% lag_type)

  AF_overall <- sum(case_week_exp$case_wn, na.rm = TRUE) / sum(dt_ew_d$incidence_n, na.rm = TRUE)

  # 4) year AF
  all_case_year <- dt_ew_d %>%
    mutate(YEAR = str_sub(yearweek, 1, 4)) %>%
    group_by(YEAR) %>%
    summarise(all_case = sum(incidence_n, na.rm = TRUE), .groups = "drop")

  exp_case_year <- case_week_exp %>%
    group_by(YEAR) %>%
    summarise(all_influ_case = sum(case_wn, na.rm = TRUE), .groups = "drop")

  af_year <- all_case_year %>%
    left_join(exp_case_year, by = "YEAR") %>%
    mutate(
      all_influ_case = ifelse(is.na(all_influ_case), 0, all_influ_case),
      AF = all_influ_case / all_case
    ) %>%
    arrange(YEAR)

  af_res <- list(
    AF_overall = AF_overall,
    AF_by_year = af_year
  )

  return(af_res)
}

