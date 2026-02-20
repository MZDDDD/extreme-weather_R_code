# =========================
#Demo: one-click run
# =========================

# 0) Setup
if (!dir.exists("output")) dir.create("output")

# 1) Load scripts
source("R/data_reading.R")
source("R/poisson_modeling.R")
source("R/3.meta_analysis.R")
source("R/AF_calculation.R") 

# 2) Read demo data
disease <- read.csv("demo_data/disease.csv")
weather <- read.csv("demo_data/ex_weather.csv")
covars  <- read.csv("demo_data/covarites.csv")
hol     <- read.csv("demo_data/holidays.csv")
string  <- read.csv("demo_data/covid-19-stringency-index.csv")

model <- run_poisson_model()

result <- AF_calculation(model)

write.csv(result, "output/RR_demo_results.csv", row.names=FALSE)

message("Demo finished successfully")
