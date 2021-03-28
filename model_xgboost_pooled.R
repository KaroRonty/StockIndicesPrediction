library(vip)
library(useful)
library(readxl)
library(tsibble)
library(parallel)
library(lubridate)
library(tidyverse)
library(tidymodels)
library(doParallel)

if(exists("cl")){
  stopCluster(cl)
}
cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)

# Which CAGR to use
selected_cagr <- 5
cagr_name <- paste0("cagr_", selected_cagr, "_year")

# How many years ahead to create CAGRs for sets
lead_years <- 1:10

# Data frames and corresponding predictors to use in mapping
dfs <- c("capes_long","rate_10_year_long",
         "unemployment_long", "dividends_long",
         "s_rate_10_year_long", "cpi_long")
cagrs <- paste0("cagr_", lead_years, "_year")
predictors <- c("cape", "dividend_yield",
                "rate_10_year", "dividend_yield", "unemployment", 
                "s_rate_10_year", "cpi") 

# FIXME
# suppressMessages(source("extract_data.R"))
to_model_exploration <- readRDS("to_model_exploration.RDS")

to_model_temp <- to_model_exploration %>% 
  # filter(country == selected_country) %>% # TODO
  select(date, country, !!cagr_name, !!predictors) %>% 
  rename(cagr_n_year := !!cagr_name) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.x), 1000, .x)) %>% 
  filter(date > yearmonth(ymd("1981-01-01")))

kept_countries <- to_model_temp %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(not_1000 = mean(cagr_n_year)) %>% 
  filter(not_1000 != 1000) %>% 
  pull(country)

to_model_countries <- to_model_temp %>% 
  filter(country %in% kept_countries)

# TODO only same countries as in training

to_model_mm <- to_model_countries %>% 
  as_tibble() %>% 
  group_by(country, date) %>% 
  mutate(no_data = ifelse(mean(c(cape,
                                 dividend_yield,
                                 rate_10_year,
                                 unemployment,
                                 s_rate_10_year,
                                 cpi)) == 1000 |
                            cagr_n_year == 1000,
                          1,
                          0)) %>% 
  filter(no_data != 1) %>% 
  select(-no_data) %>% 
  arrange(date)

to_model <- build.x(~ .,
                    data = to_model_mm[, -1],
                    contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date)

max_data_date <- to_model %>% 
  pull(date) %>% 
  max()

# Test set is has a maximum length based on CAGR years
leakage_end_date <- max(as.Date(max_data_date) - months(187), 
                        as.Date(max_data_date) -  years(10))

leakage_start_date <- leakage_end_date - years(selected_cagr)

training <- to_model %>% 
  filter(date < yearmonth(leakage_start_date)) %>% 
  mutate(source = !!cagr_name,
         set = "training") %>% 
  select(-1, -date, -source, -set, -countrySAUDI_ARABIA) %>% 
  as.matrix()

leakage_set <- to_model %>% 
  filter(date >= yearmonth(leakage_start_date),
         date < yearmonth(leakage_end_date)) %>% 
  mutate(source = !!cagr_name,
         set = "leakage") %>% 
  select(-1, -date, -source, -set, -countrySAUDI_ARABIA)

test <- to_model %>% 
  filter(date >= yearmonth(leakage_end_date)) %>% 
  mutate(source = !!cagr_name,
         set = "test") %>% 
  select(-1, -source, -set, -countrySAUDI_ARABIA)

model_recipe <- recipe(cagr_n_year ~ # FIXME
                         countryAUSTRALIA + 
                         countryAUSTRIA + 
                         countryBELGIUM + 
                         countryBRAZIL + 
                         countryCANADA + 
                         countryCHILE + 
                         countryCHINA + 
                         countryCOLOMBIA + 
                         countryCZECH_REPUBLIC + 
                         countryDENMARK + 
                         countryFINLAND + 
                         countryFRANCE + 
                         countryGERMANY + 
                         countryGREECE + 
                         countryHONG_KONG + 
                         countryHUNGARY + 
                         countryINDIA + 
                         countryINDONESIA + 
                         countryIRELAND + 
                         countryISRAEL + 
                         countryITALY + 
                         countryJAPAN + 
                         countryKOREA + 
                         countryMEXICO + 
                         countryNETHERLANDS + 
                         countryNEW_ZEALAND + 
                         countryNORWAY + 
                         countryPOLAND + 
                         countryPORTUGAL + 
                         countryRUSSIA + 
                         countrySINGAPORE + 
                         countrySOUTH_AFRICA + 
                         countrySPAIN + 
                         countrySWEDEN + 
                         countrySWITZERLAND + 
                         countryTAIWAN + 
                         countryTHAILAND + 
                         countryTURKEY + 
                         countryUK + 
                         countryUSA + 
                         cape + 
                         dividend_yield + 
                         rate_10_year + 
                         unemployment + 
                         s_rate_10_year + 
                         cpi, # TODO unemployment
                       data = training)


model_data_prepared <- prep(model_recipe, 
                            training = training, 
                            verbose = TRUE)

model_folds <- model_data_prepared %>% 
  juice() %>% 
  rolling_origin(initial = 2700,
                 assess = 900,
                 skip = 300, # FIXME 0
                 lag = 1800)


xgboost_grid <- grid_regular(#trees(c(1500, 3000)),
  #min_n(c(30, 70)),
  #tree_depth(c(15, 60)),
  # learn_rate(c(0.001, 0.1)),
  loss_reduction(),
  levels = 5)

xgboost_grid %>% 
  summarise_all(~list(unique(.x))) %>% 
  unnest()

# TODO check
xgboost_model <- boost_tree(mode = "regression",
                            trees = 3000,
                            min_n = 30,
                            tree_depth = 60, 
                            learn_rate = 0.01,
                            loss_reduction = 1e-10) %>% 
  set_engine("xgboost", nthread = 6)

xgboost_wf <- workflow() %>% 
  add_recipe(model_recipe) %>% 
  add_model(xgboost_model)

# 14 min
tic_xgboost <- Sys.time()
set.seed(42)
xgboost_trained <- xgboost_wf %>% 
  tune_grid(resamples = model_folds,
            grid = xgboost_grid,
            metrics = metric_set(mae, mape, rmse, rsq))
(toc_xgboost <- Sys.time() - tic_xgboost)

xgboost_fit <- xgboost_wf %>% 
  finalize_workflow(xgboost_trained %>%
                      select_best("rmse")) %>% 
  fit(model_data_prepared %>% juice())

importance_xgboost <- xgboost_fit %>% 
  pull_workflow_fit() %>% 
  vip() +
  ggtitle("XGBoost") +
  theme_minimal()

xgb_pred <- xgboost_fit %>% 
  predict(model_data_prepared %>% 
            bake(test)) %>% 
  pull(.pred)

pred_vs_actual_xgboost <- tibble(
  date = test$date, 
  country = to_model_mm %>% 
    filter(date >= yearmonth(leakage_end_date)) %>% 
    pull(country),
  actual = test$cagr_n_year, 
  pred = xgb_pred)

pred_vs_actual_xgboost %>% 
  pivot_longer(actual:pred)  %>% 
  filter(country %in% c("AUSTRALIA", "CANADA", "USA", "UK", 
                        "NETHERLANDS", "GERMANY", "SPAIN", "SWITZERLAND")) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  facet_wrap(~country) +
  ggtitle("XGBoost") +
  xlab("Date") +
  ylab(cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

training_temp <- training %>% 
  as_tibble()

mean_predictions <- tibble(date = to_model_mm %>% 
                             filter(date < yearmonth(leakage_start_date)) %>% 
                             pull(date), 
                           country = to_model_mm %>% 
                             filter(date < yearmonth(leakage_start_date)) %>% 
                             pull(country),
                           actual = training_temp$cagr_n_year) %>% 
  filter(country %in% c("AUSTRALIA", "CANADA", "USA", "UK", 
                        "NETHERLANDS", "GERMANY", "SPAIN", "SWITZERLAND")) %>%
  group_by(country) %>% 
  summarise(mean_prediction = mean(actual, na.rm = TRUE))

# tibble(date = test$date,
#        actual = test$cagr_n_year, 
#        pred = xgb_pred) %>% 
#   summarise(mape = median(abs(((actual) - pred) / actual)))

pred_vs_actual_xgboost %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(xgb_mape = median(abs(((actual) - pred) / actual)),
            mean_mape = median(abs(((actual) - mean_prediction) / actual)))

pred_vs_actual_xgboost %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(xgb_mape = median(abs(((actual) - pred) / actual)),
            mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
  ungroup() %>% 
  summarise_if(is.numeric, median)
