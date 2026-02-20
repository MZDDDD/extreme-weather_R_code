# extreme-weather_R_code
Code for: Impact of extreme weather events on the dynamics of major infectious diseases in China, 2014‒2022
This repository contains the R code and a small demonstration dataset used to reproduce the core analytical workflow described in our manuscript:

**"Compound Extreme Weather Events and Risk of Infectious Diseases"**

The repository includes all scripts required to perform:

- Data preprocessing
- Case-crossover Poisson regression modeling
- Random-effects meta-analysis
- Attributable Fraction (AF) estimation

---

## 1. System Requirements

### Operating System

The code has been tested on:

- Windows 10

### Software Requirements

- R version ≥ 4.2.0

### Required R Packages

Please install the following R packages before running the demo:
install.packages(c(
  "dplyr",
  "tidyverse",
  "magrittr",
  "lubridate",
  "stringr",
  "gnm",
  "mixmeta",
  "dlnm",
  "gtools"))
Typical installation time on a standard desktop computer:< 5 minutes

2. Installation Guide

Download this repository as a ZIP file or clone it from GitHub.
Extract the contents to a local directory.
Open R or RStudio.
Set the working directory to the root folder of the repository.
Example:
setwd("path/to/your/downloaded")

3. Demo
Run the Demo Analysis
The full demonstration workflow can be executed using the following command:
source("run_demo.R")
This script will automatically:

Load the demonstration dataset located in demo_data/
Perform case-crossover Poisson regression analysis
Conduct random-effects meta-analysis
Estimate attributable fraction (AF)
Save all outputs into the output/ folder

Expected Output

After successful execution, the following files will be generated in the output/ directory:

file name and description
poisson_coef_demo.csv: lag-specific Poisson regression RR estimates
poisson_meta_input_demo.csv: meta-analysis input (Beta and variance)
meta_result_demo.csv: pooled RR estimates from meta-analysis
af_overall_demo.csv: overall attributable fraction (AF)
af_by_year_demo.csv: year-specific AF estimates

Expected run time on a standard desktop computer:~2–3 minutes

4. Instructions for Use on Custom Data

To apply the workflow to user-provided data:
(1) Replace the CSV files in the demo_data/ directory with your own data.
(2) Ensure the data follow the same structure and variable naming convention as the demo dataset.
(3) Run: source("run_demo.R")

All analysis steps will be automatically executed.

5. Reproduction Instructions

The demo dataset included in this repository allows reviewers to reproduce the full analytical pipeline used in the manuscript, including:

Exposure identification
Conditional Poisson regression
Random-effects meta-analysis
Attributable Fraction estimation

6. License
This software is released under the MIT License.

7. Code Availability
The code and demonstration dataset are publicly available at:

https://github.com/MZDDDD/extreme-weather_R_code.git

The repository contains a one-click script (run_demo.R) to reproduce the main analytical workflow and generate all expected outputs.
