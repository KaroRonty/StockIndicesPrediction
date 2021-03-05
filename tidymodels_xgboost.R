library(vip)
library(tune)
library(dials)
library(tibble)
library(rsample)
library(parsnip)
library(recipes)
library(stringr)
library(parallel)
library(yardstick)
library(workflows)
library(doParallel)

cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)

one_country <- "NETHERLANDS"

# Get unique countries
countries <- training %>% 
  pull(country) %>% 
  unique()

# make_xgboost_models <- function(one_country){
country_training <- training %>% 
  filter(country == one_country)

country_test <- test %>% 
  filter(country == one_country)

xgboost_recipe <- recipe(cagr_5_year ~ 
                           cape + 
                           rate_10_year + 
                           dividend_yield + 
                           s_rate_10_year + 
                           cpi, # TODO unemployment
                         data = country_training) %>% 
  step_range(all_predictors(), min = 0, max = 1)

xgboost_prepared <- prep(xgboost_recipe, training = country_training, verbose = TRUE)

xgboost_folds <- xgboost_prepared %>% 
  juice() %>% 
  rolling_origin(initial = 90,
                 assess = 30,
                 skip = 10, # FIXME 0
                 lag = 60)

# # Plot the folds
# tidy(xgboost_folds) %>% 
#   ggplot(aes(x = Resample, y = Row, fill = Data)) + 
#   geom_tile(alpha = 0.5) + 
#   scale_fill_brewer(palette = "Set1") + 
#   xlab(NULL) + 
#   ylab("Training Set Sample") + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) + 
#   theme(legend.position = "bottom") + 
#   coord_flip()
# }

# xgboost_folds <- vfold_cv(xgboost_prepared %>% juice()) # FIXME 

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

# xgboost_prepared %>% juice

xgboost_wf <- workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_model)

# 10.3 min
xgboost_res <- xgboost_wf %>% 
  tune_grid(resamples = xgboost_folds,
            grid = xgboost_grid,
            metrics = metric_set(mae, mape, rmse, rsq))

# xgboost_res %>% collect_metrics() %>% print(n = 250)

xgboost_fit <- xgboost_wf %>% 
  finalize_workflow(xgboost_trained %>%
                      select_best("rmse")) %>% 
  fit(model_data_prepared %>% juice())

xgboost_fit %>% 
  pull_workflow_fit() %>% 
  vip() +
  theme_minimal()

xgb_pred <- xgboost_fit %>% 
  predict(model_data_prepared %>% 
            bake(country_test)) %>% 
  pull(.pred)

tibble(date = country_test$date, 
       actual = country_test$cagr_5_year, 
       pred = xgb_pred) %>% 
  pivot_longer(actual:pred) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() + 
  theme_minimal()

# 0.0338
tibble(date = country_test$date, 
       actual = country_test$cagr_5_year, 
       pred = xgb_pred) %>% 
  summarise(mape = median(abs(((actual) - pred) / actual)))
