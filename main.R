library(readxl)
library(Matrix)
library(tsibble)
library(tidyverse)
library(patchwork)

tic_all <- Sys.time()

source("setup.R")

read_from_rds <- TRUE

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
source("model_benchmarks.R")
source("model_mean.R")
source("modeling_results.R")
source("tables.R")
source("thesis_graphs.R")
source("financial_performance.R")
source("accumulated_local_effects.R")

if(exists("cl")){
  print("Stopping any remaining clusters...")
  stopCluster(cl)
  rm(cl)
}

# 8.7 h
(toc_all <- Sys.time() - tic_all)

preds_vs_actuals %>% 
  left_join(preds_vs_actuals_stack) %>% 
  left_join(preds_vs_actuals_ensemble) %>% 
  left_join(mean_predictions) %>% 
  na.omit() %>%
  group_by(country) %>% 
  summarise_at(vars(contains("pred")),
               ~median(abs(((actual) - .x) / actual))) %>% 
  ungroup() %>% 
  select(-country) %>% 
  summarise_all(median) %>% 
  rename_all(~str_remove(.x, "_pred$")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(MAPE = 1) %>% 
  rownames_to_column("model") %>% 
  mutate(model = case_when(model == "mean_prediction" ~ "MEAN",
                           model == "naive_prediction" ~ "NAIVE",
                           TRUE ~ model),
         MAPE = round(MAPE, 4)) %>% 
  arrange(MAPE)

#  MAPE |  Min  | Model
# 0.056 |   NA  | MEAN
# 0.061 |   NA  | NAIVE
# -----------------------------
# 0.035 | 430.8 | XGBoost
# 0.026 |  10.9 | Random Forest
# 0.023 |  14.8 | Elastic net
# 0.058 |   0.2 | Single ARIMA
# 0.026 |  35.5 | Single XGBoost
# 0.032 |   7.5 | Single Random Forest
# 0.023 |   NA  | Ensemble mean
# 0.022 |   NA  | Ensemble median
# 0.024 |  16.9 | Stack
