if(!dir.exists("output")) dir.create("output")

source("R/data_reading.R")
source("R/poisson_modeling.R")
source("R/3.meta_analysis.R")
source("R/AF_calculation.R")

prep <- prepare_data()

pois <- run_poisson(
  dt_ew_d = prep$dt_ew_d,
  dt_cv   = prep$dt_cv,
  lag_n   = prep$lag_n,
  disease_name = "FLU"
)


meta_res <- run_meta(pois$meta_m)


af_res <- calc_af(meta_res)

write.csv(pois$coef_m, "output/poisson_coef_demo.csv", row.names=FALSE)
write.csv(pois$meta_m, "output/poisson_meta_input_demo.csv", row.names=FALSE)
write.csv(meta_res,    "output/meta_demo.csv", row.names=FALSE)
write.csv(af_res,      "output/af_demo.csv", row.names=FALSE)

message("Demo finished successfully.")
