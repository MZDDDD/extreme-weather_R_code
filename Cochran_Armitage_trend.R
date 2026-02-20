library(dplyr)
library(purrr)
library(readr)
library(Kendall)   

out_dir <- "D:/extreme weather/250822new/AF/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

load("D:/extreme weather/250822new/AF/AF20250829.RData")


only_sig_years <- TRUE   


trend_on_AF_CA <- function(df_one_name) {
  dat <- df_one_name %>%
    mutate(
      name    = as.character(name),
      year    = suppressWarnings(as.integer(year)),
      p_value = suppressWarnings(as.numeric(p_value)),
      AFall   = suppressWarnings(as.numeric(AFall))
    ) %>%
    filter(!is.na(name), !is.na(year), !is.na(AFall)) %>%
    filter(year != 1000) %>%
    arrange(year)
  
  
  min_AF <- min(dat$AFall, na.rm = TRUE)
  if (min_AF < 0) {
    dat$AFall <- dat$AFall + abs(min_AF) 
  }
  

  out <- tibble(
    name = if (nrow(df_one_name)>0) as.character(df_one_name$name[1]) else NA_character_,
    years_used = if (nrow(dat)>0) paste(dat$year, collapse=",") else NA_character_,
    k_years = nrow(dat),
    chisq = NA_real_,
    p_trend = NA_real_,
    trend_direction = NA_character_
  )
  
 
  if (nrow(dat) < 2 || dplyr::n_distinct(dat$year) < 2) {
    out$trend_direction <- "insufficient_years"
    return(out)
  }
  

  x <- as.integer(round(dat$AFall * 10000)) 
  n <- rep.int(10000, length(x))           
  
  # Cochran–Armitage 
  pt <- suppressWarnings(prop.trend.test(x = x, n = n, score = dat$year))
  out$chisq   <- unname(pt$statistic)
  out$p_trend <- unname(pt$p.value)
  
  
  sp <- suppressWarnings(try(cor.test(dat$year, dat$AFall, method = "spearman", exact = FALSE), silent = TRUE))
  rho <- if (!inherits(sp, "try-error")) unname(sp$estimate) else NA_real_
  out$trend_direction <- dplyr::case_when(
    is.finite(out$p_trend) & out$p_trend < 0.05 & is.finite(rho) & rho > 0  ~ "increasing",
    is.finite(out$p_trend) & out$p_trend < 0.05 & is.finite(rho) & rho < 0  ~ "decreasing",
    is.finite(out$p_trend)                                                       ~ "not_significant",
    TRUE                                                                             ~ "undetermined"
  )
  
  out
}


stopifnot(is.list(dt_af), !is.null(names(dt_af)))
all_weather_results <- list()


safe_trend_CA <- purrr::safely(trend_on_AF_CA, otherwise = NULL)

for (w in names(dt_af)) {
  dat_w <- dt_af[[w]] %>%
    mutate(
      name    = as.character(name),
      year    = suppressWarnings(as.integer(year)),
      AFall   = suppressWarnings(as.numeric(AFall)),
      p_value = suppressWarnings(as.numeric(p_value))
    ) %>%
    filter(!is.na(name), !is.na(year)) %>%
    filter(year != 1000)
  
  if (nrow(dat_w) == 0) { 
    message(sprintf("[跳过] %s：无数据。", w)); 
    next 
  }
  
  
  disease_list <- if (isTRUE(only_sig_years)) {
    dat_w %>%
      filter(!is.na(p_value) & p_value < 0.05) %>%
      count(name, name = "n_sig_years") %>%
      filter(n_sig_years >= 2) %>%
      pull(name)
  } else {
    dat_w %>%
      count(name) %>%
      filter(n >= 2) %>%
      pull(name)
  }
  
  if (length(disease_list) == 0) {
    message(sprintf("[提示] %s：没有疾病满足最小年份条件。", w))
    next
  }
  
 
  lst <- map(disease_list, ~ safe_trend_CA(dat_w %>% filter(name == .x)))
  res_w <- map_dfr(seq_along(lst), function(i) {
    if (!is.null(lst[[i]]$result)) {
      lst[[i]]$result
    } else {
      tibble(
        name = disease_list[i],
        years_used = NA_character_,
        k_years = NA_integer_,
        chisq = NA_real_,
        p_trend = NA_real_,
        trend_direction = paste0("error: ", conditionMessage(lst[[i]]$error))
      )
    }
  }) %>%
    mutate(weather = w, .before = 1)
  
  out_csv_w <- file.path(out_dir, sprintf("AF_trend_CA_%s%s.csv",
                                          w, ifelse(only_sig_years, "_sigOnly", "")))
  write_csv(res_w, out_csv_w)
  all_weather_results[[w]] <- res_w
  message(sprintf("[完成] %s -> %s", w, out_csv_w))
}


if (length(all_weather_results) > 0) {
  trend_all <- bind_rows(all_weather_results)
  out_csv_all <- file.path(out_dir, sprintf("AF_trend_CA_ALL%s.csv",
                                            ifelse(only_sig_years, "_sigOnly", "")))
  write_csv(trend_all, out_csv_all)
  message(sprintf("[汇总完成] %s", out_csv_all))
} else {
  message("未生成汇总：没有任何天气满足最小年份条件。")
}