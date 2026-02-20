if(!dir.exists("output")) dir.create("output")

source("R/data_reading.R")
source("R/poisson_modeling.R")
source("R/3.meta_analysis.R")
source("R/AF_calculation.R")

prep <- prepare_data()
pois <- run_poisson(prep$dt_ew_d, prep$dt_cv, prep$lag_n, disease_name="FLU")
meta <- run_meta(pois$meta_m)
af   <- calc_af(meta)

write.csv(af, "output/af_demo.csv", row.names=FALSE)
