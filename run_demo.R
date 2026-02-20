if(!dir.exists("output")) dir.create("output")

source("R/data_reading.R")
source("R/poisson_modeling.R")
source("R/3.meta_analysis.R")
source("R/AF_calculation.R")

prep <- prepare_data()

pois <- run_poisson(prep$dt_ew_d, prep$dt_cv, prep$lag_n, disease_name="FLU")
meta_res <- run_meta(pois$meta_m, disease_name="FLU", type="RDs")

af_res <- calc_af(
  dt_ew   = prep$dt_ew,
  dt_d    = prep$dt_d,
  dt_ew_d = prep$dt_ew_d,
  Date    = prep$Date,
  lag_n   = prep$lag_n,
  lag_max = 5,
  year_min = 2014
)

write.csv(pois$coef_m, "output/poisson_coef.csv", row.names=FALSE)
write.csv(pois$meta_m, "output/poisson_meta_input.csv", row.names=FALSE)
write.csv(meta_res,    "output/meta_result.csv", row.names=FALSE)

write.csv(af_res$AF_by_year, "output/af_by_year.csv", row.names=FALSE)
write.csv(data.frame(AF_overall = af_res$AF_overall), "output/af_overall.csv", row.names=FALSE)

message("Demo finished successfully. See /output")
