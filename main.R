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

#   MAPE |  Min  | Model
# 0.0584 |   NA  | MEAN
# 0.0559 |   NA  | NAIVE
# ------------------------------
# 0.0360 | 430.8 | XGBoost
# 0.0280 |  11.9 | Random Forest
# 0.0284 |  11.9 | Elastic net
# 0.0621 |   0.2 | Single ARIMA
# 0.0287 |  71.0 | Single XGBoost
# 0.0329 |  13.6 | Single Random Forest
# 0.0435 |   NA  | Ensemble mean
# 0.0433 |   NA  | Ensemble median
# 0.0260 |  40.7 | Stack
