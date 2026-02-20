if(!dir.exists("output")) dir.create("output")

source("R/1.ex_label.R")
source("R/data_reading.R")
source("R/poisson_modeling.R")
source("R/3.meta_analysis.R")
source("R/AF_calculation.R")

dat          <- prepare_data()
poisson_res  <- run_poisson(dat)
meta_res     <- run_meta(poisson_res)
af_res       <- calc_af(meta_res)

write.csv(af_res,"output/AF_demo_results.csv",row.names=FALSE)
