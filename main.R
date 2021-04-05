library(readxl)
library(tsibble)
library(tidyverse)

source("setup.R")

read_from_rds <- FALSE

if(read_from_rds){
  to_model_exploration <- readRDS("to_model_exploration.RDS")
} else {
  suppressMessages(source("extract_data.R"))
}

source("modeling_setup.R")
source("model_xgboost_pooled.R")
source("model_rf_pooled.R")
source("model_elastic_pooled.R")
source("model_arima.R")
source("model_stack_pooled.R")
source("model_ensemble_pooled.R")
source("functions.R")
# source("partial_dependence.R")

if(exists("cl")){
  print("Stopping any remaining clusters...")
  stopCluster(cl)
  rm(cl)
}

# 0.0584 MEAN
# 0.0368 XGBoost
# 0.0245 Random Forest
# 0.0234 Elastic net
# 0.0258 Stack
# 0.0320 Ensemble median
# 0.0316 Ensemble mean
