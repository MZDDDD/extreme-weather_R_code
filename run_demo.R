if(!dir.exists("output")) dir.create("output")

source("R/1.ex_label.R")
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

meta <- run_meta(pois$meta_m)     # meta_analysis
af   <- calc_af(meta)            # AF 

write.csv(pois$coef_m, "output/poisson_coef_demo.csv", row.names=FALSE)
write.csv(pois$meta_m, "output/poisson_meta_input_demo.csv", row.names=FALSE)
write.csv(meta,        "output/meta_demo.csv", row.names=FALSE)
write.csv(af,          "output/af_demo.csv", row.names=FALSE)
