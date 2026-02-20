# =========================
# Nature demo: one-click run
# =========================

if(!dir.exists("output")) dir.create("output")

# 1) load functions
source("R/1.ex_label,R")
source("R/data_reading.R")       
source("R/poisson_modeling.R")
source("R/3.meta_analysis.R")
source("R/AF_calculation.R")

# 2) read demo data
disease <- read.csv("demo_data/disease.csv")
weather <- read.csv("demo_data/ex_weather.csv")
covars  <- read.csv("demo_data/covarites.csv")
hol     <- read.csv("demo_data/holidays.csv")
string  <- read.csv("demo_data/covid-19-stringency-index.csv")

# 3) build analysis dataset（如果你原来data_reading.R已经做完，就用它）
dat <- merge(disease, weather, by="date")
dat <- merge(dat, covars, by="date", all.x=TRUE)
dat <- merge(dat, hol, by="date", all.x=TRUE)
dat <- merge(dat, string, by="date", all.x=TRUE)

# 4) run pipeline
poisson_res <- run_poisson(dat)
meta_res    <- run_meta(poisson_res)
af_res      <- calc_af(meta_res)

# 5) save outputs
write.csv(poisson_res, "output/poisson_results_demo.csv", row.names=FALSE)
write.csv(meta_res,    "output/meta_results_demo.csv",   row.names=FALSE)
write.csv(af_res,      "output/af_results_demo.csv",     row.names=FALSE)

message("Demo finished successfully. Check /output")
