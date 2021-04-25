library(readxl)
library(tsibble)
library(tidyverse)

tic_all <- Sys.time()

source("setup.R")

read_from_rds <- FALSE

if(read_from_rds){
  to_model_exploration <- readRDS("to_model_exploration.RDS")
} else {
  suppressMessages(source("extract_data.R"))
}

source("functions.R")
source("modeling_setup.R")
source("model_xgboost_pooled.R")
source("model_rf_pooled.R")
source("model_elastic_pooled.R")
source("model_arima.R")
source("model_xgboost_single.R")
source("model_rf_single.R")
source("model_stack_pooled.R")
source("model_ensemble_pooled.R")
# source("partial_dependence.R")

if(exists("cl")){
  print("Stopping any remaining clusters...")
  stopCluster(cl)
  rm(cl)
}

# 8.7 h
(toc_all <- Sys.time() - tic_all)

#   MAPE |  Min  | Model
# 0.0584 |   NA  | MEAN
# ------------------------------
# 0.0360 | 430.8 | XGBoost
# 0.0280 |  11.9 | Random Forest
# 0.0284 |  11.9 | Elastic net
# 0.0621 |   0.2 | Single ARIMA
# 0.0287 |  71.0 | Single XGBoost
# 0.0329 |  13.6 | Single Random Forest
# 0.0436 |    NA | Ensemble mean
# 0.0437 |    NA | Ensemble median
# 0.0260 |  40.7 | Stack
