library(vip)
library(furrr)
library(fable)
library(readxl)
library(tsibble)
library(parallel)
library(lubridate)
library(tidyverse)
library(doParallel)
library(ggbeeswarm)
library(ggcorrplot)

# Which CAGR to use
selected_cagr <- 5
cagr_name <- paste0("cagr_", selected_cagr, "_year")

# Which country to create the model for
# c("AUSTRALIA", "CANADA", "USA", "UK", "NETHERLANDS", "GERMANY", "SPAIN")
# TODO CANADA GERMANY SPAIN
selected_country <- "UK"

# How many years ahead to create CAGRs for sets
lead_years <- 1:10

# Data frames and corresponding predictors to use in mapping
dfs <- c("capes_long","rate_10_year_long",
         "unemployment_long", "dividends_long",
         "s_rate_10_year_long", "cpi_long")
cagrs <- paste0("cagr_", lead_years, "_year")
predictors <- c("cape", "rate_10_year",
                "unemployment", "dividend_yield", "s_rate_10_year", "cpi")

suppressMessages(source("extract_data.R"))

# Get maximum date from the data
max_data_date <- to_model_exploration %>% 
  pull(date) %>% 
  max()

# Test set is has a maximum length based on CAGR years
leakage_end_date <- max(as.Date(max_data_date) - months(187), 
                        as.Date(max_data_date) -  years(10) -
                          years(selected_cagr))

leakage_start_date <- leakage_end_date - years(selected_cagr)

to_model <- to_model_exploration %>% 
  filter(country == selected_country) %>% # TODO
  select(date, country, !!cagr_name, !!predictors) %>% 
  rename(cagr_n_year := !!cagr_name) %>% 
  na.omit()

# Split into different sets
training <- to_model %>% 
  filter(date < yearmonth(leakage_start_date)) %>% 
  mutate(source = !!cagr_name,
         set = "training")

leakage_set <- to_model %>% 
  filter(date >= yearmonth(leakage_start_date),
         date < yearmonth(leakage_end_date)) %>% 
  mutate(source = !!cagr_name,
         set = "leakage")

test <- to_model %>% 
  filter(date >= yearmonth(leakage_end_date)) %>% 
  mutate(source = !!cagr_name,
         set = "test")

# Models ------------------------------------------------------------------

normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) -
       min(x, na.rm = TRUE))
}

arima_training <- training %>% 
  mutate_at(vars(cagr_n_year, !!predictors), 
            function(x) normalize(x))

models_ts <- training %>% 
  model(ARIMA = ARIMA(cagr_n_year ~ 
                        cape + 
                        rate_10_year + 
                        dividend_yield + 
                        s_rate_10_year))

arima_pred <- models_ts %>% 
  forecast(test %>% filter(country == selected_country)) %>% 
  pull(.mean)

models_ts %>% 
  pull(ARIMA) %>% 
  pluck(1) %>% 
  tidy() %>% 
  arrange(desc(abs(estimate))) %>% 
  mutate(estimate = (estimate - min(estimate)) / 
           (max(estimate) - min(estimate))) %>% 
  ggplot(aes(estimate, reorder(term, estimate))) +
  geom_col() +
  xlab("Importance") +
  ylab(NULL) +
  ggtitle("ARIMA feature importance") +
  theme_minimal()

cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)

country_training <- training %>% 
  filter(country == selected_country)

country_test <- test %>% 
  filter(country == selected_country)

# Common stuff
model_recipe <- recipe(cagr_n_year ~ 
                         cape + 
                         rate_10_year + 
                         dividend_yield + 
                         s_rate_10_year + 
                         cpi, # TODO unemployment
                       data = country_training) %>% 
  step_range(all_predictors(), min = 0, max = 1)

model_data_prepared <- prep(model_recipe, 
                            training = country_training, 
                            verbose = TRUE)

model_folds <- model_data_prepared %>% 
  juice() %>% 
  rolling_origin(initial = 90,
                 assess = 30,
                 skip = 10, # FIXME 0
                 lag = 60)

# XGBoost -----------------------------------------------------------------

xgboost_grid <- grid_regular(min_n(),
                             tree_depth(),
                             learn_rate(),
                             loss_reduction(),
                             levels = 5)

xgboost_model <- boost_tree(mode = "regression",
                            trees = 1000,
                            min_n = tune(),
                            tree_depth = tune(), #tune(),
                            learn_rate = tune(),
                            loss_reduction = tune()) %>% 
  set_engine("xgboost")

xgboost_wf <- workflow() %>% 
  add_recipe(model_recipe) %>% 
  add_model(xgboost_model)

tic_xgboost <- Sys.time()
xgboost_trained <- xgboost_wf %>% 
  tune_grid(resamples = model_folds,
            grid = xgboost_grid,
            metrics = metric_set(mae, mape, rmse, rsq))
(toc_xgboost <- Sys.time() - tic_xgboost)

xgboost_fit <- xgboost_wf %>% 
  finalize_workflow(xgboost_trained %>%
                      select_best("rmse")) %>% 
  fit(model_data_prepared %>% juice())

xgboost_fit %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("XGBoost feature importance") +
  theme_minimal()

xgb_pred <- xgboost_fit %>% 
  predict(model_data_prepared %>% 
            bake(country_test)) %>% 
  pull(.pred)

tibble(date = country_test$date, 
       actual = country_test$cagr_n_year, 
       pred = xgb_pred) %>% 
  pivot_longer(actual:pred) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  ggtitle("XGBoost predictions vs actuals") +
  theme_minimal()

tibble(date = country_test$date, 
       actual = country_test$cagr_n_year, 
       pred = xgb_pred) %>% 
  summarise(mape = median(abs(((actual) - pred) / actual)))

rf_grid <- grid_regular(finalize(mtry(), country_training),
                        min_n(),
                        levels = 50)

rf_model <- rand_forest(mode = "regression",
                        trees = 1000,
                        mtry = tune(),
                        min_n = tune()) %>% 
  set_engine("randomForest")

rf_wf <- workflow() %>% 
  add_recipe(model_recipe) %>% 
  add_model(rf_model)

tic_rf <- Sys.time()
rf_trained <- rf_wf %>% 
  tune_grid(resamples = model_folds,
            grid = rf_grid,
            metrics = metric_set(mae, mape, rmse, rsq))
(toc_rf <- Sys.time() - tic_rf)

rf_fit <- rf_wf %>% 
  finalize_workflow(rf_trained %>%
                      select_best("rmse")) %>% 
  fit(model_data_prepared %>% juice())

rf_fit %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("Random Forest feature importance") +
  theme_minimal()

rf_pred <- rf_fit %>% 
  predict(model_data_prepared %>% 
            bake(country_test)) %>% 
  pull(.pred)

tibble(date = country_test$date, 
       actual = country_test$cagr_n_year, 
       pred = rf_pred) %>% 
  pivot_longer(actual:pred) %>% 
  ggplot(aes(date, value, color = name)) +
  ggtitle("Random Forest predictions vs actuals") +
  geom_line() + 
  theme_minimal()

tibble(date = country_test$date, 
       actual = country_test$cagr_n_year, 
       pred = rf_pred) %>% 
  summarise(mape = median(abs(((actual) - pred) / actual)))

# Stack -------------------------------------------------------------------

xgb_pred_training <- xgboost_fit %>% 
  predict(model_data_prepared %>% 
            bake(country_training)) %>% 
  pull(.pred)

rf_pred_training <- rf_fit %>% 
  predict(model_data_prepared %>% 
            bake(country_training)) %>% 
  pull(.pred)

# TODO fitted or forecasts?
arima_pred_training <- models_ts %>% 
  pull(ARIMA) %>% 
  pluck(1) %>% 
  fitted() %>% 
  pull(.fitted)

stack_training <- country_training %>% 
  as_tibble() %>% 
  select(date, cagr_n_year) %>% 
  mutate(xgb = xgb_pred_training,
         rf = rf_pred_training,
         arima = arima_pred_training)

stack_test <- country_test %>% 
  as_tibble() %>% 
  select(date, cagr_n_year) %>% 
  mutate(xgb = xgb_pred,
         rf = rf_pred,
         arima = arima_pred)

stack_recipe <- recipe(cagr_n_year ~ 
                         xgb + 
                         rf +
                         arima,
                       data = stack_training)

stack_prepared <- prep(stack_recipe,
                       training = stack_training,
                       verbose = TRUE)

stack_folds <- stack_prepared %>% 
  juice() %>% 
  rolling_origin(initial = 90,
                 assess = 30,
                 skip = 0, # FIXME 0
                 lag = 60)

stack_grid <- grid_regular(penalty(),
                           mixture(),
                           levels = 100)

stack_model <- linear_reg(mode = "regression",
                          penalty = tune(),
                          mixture = tune()) %>%
  set_engine("glmnet")

stack_wf <- workflow() %>% 
  add_recipe(stack_recipe) %>% 
  add_model(stack_model)

tic_stack <- Sys.time()
stack_trained <- stack_wf %>% 
  tune_grid(resamples = stack_folds,
            grid = stack_grid,
            metrics = metric_set(mae, mape, rmse, rsq))
(toc_stack <- Sys.time() - titic_stack)

stack_fit <- stack_wf %>% 
  finalize_workflow(stack_trained %>%
                      select_best("rmse")) %>% 
  fit(stack_prepared %>% juice())

stack_fit %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("Stacked model feature importance") +
  theme_minimal()

stack_pred <- stack_fit %>% 
  predict(stack_prepared %>% 
            bake(stack_test)) %>% 
  pull(.pred)

stack_pred_tibble <- tibble(date = country_test$date, 
                            actual = country_test$cagr_n_year, 
                            stack_pred = stack_pred) %>% 
  full_join(stack_test) %>% 
  mutate(ensemble_pred = (rf_pred + xgb_pred + arima_pred) / 3)

stack_pred_tibble %>% 
  pivot_longer(c(actual, stack_pred)) %>% 
  ggplot(aes(x = date,
             y = value)) +
  geom_line(aes(color = name)) +
  ggtitle("Stacked model predictions vs actuals") +
  theme_minimal()

# Ensemble ----------------------------------------------------------------

stack_pred_tibble %>% 
  pivot_longer(c(actual, ensemble_pred)) %>% 
  ggplot(aes(x = date,
             y = value)) +
  geom_line(aes(color = name)) +
  ggtitle("Ensemble model predictions vs actuals") +
  theme_minimal()

# Ensemble gives 1/3 weight to all models, therefore feature importances are
# equally distributed between the models

mean_pred <- training$cagr_n_year

stack_pred_tibble %>% 
  summarise(mae_mean = median(abs(((actual) - mean(mean_pred)))),
            mape_mean = median(abs(((actual) - mean(mean_pred)) / actual)),
            mae_xgb = median(abs(((actual) - xgb))),
            mape_xgb = median(abs(((actual) - xgb) / actual)),
            mae_rf = median(abs(((actual) - rf))),
            mape_rf = median(abs(((actual) - rf) / actual)),
            mae_arima = median(abs(((actual) - arima))),
            mape_arima = median(abs(((actual) - arima) / actual)),
            mae_ensemble = median(abs(((actual) - ensemble_pred))),
            mape_ensemble = median(abs(((actual) - ensemble_pred) / actual)),
            mae_stack = median(abs(((actual) - stack_pred))),
            mape_stack = median(abs(((actual) - stack_pred) / actual))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(measure = ifelse(str_detect(rowname, "mape"), "MAPE", "MAE"),
         rowname = str_remove(rowname, "mae_|mape_")) %>% 
  pivot_wider(names_from = measure, values_from = V1) %>% 
  rename(model = rowname) %>% 
  arrange(MAPE)

"18:40"
Sys.time()

stack_fit %>% 
  pull_workflow_fit() %>%
  tidy() %>%
  select(-penalty)
