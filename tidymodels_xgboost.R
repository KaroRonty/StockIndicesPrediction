library(vip)
library(tune)
library(dials)
library(rsample)
library(parsnip)
library(recipes)
library(workflows)

one_country <- "USA"

# Get unique countries
countries <- training %>% 
  pull(country) %>% 
  unique()

# make_xgboost_models <- function(one_country){
  country_training <- training %>% 
    filter(country == one_country)
  
  xgboost_recipe <- recipe(cagr_5_year ~ 
                             cape + 
                             unemployment + 
                             rate_10_year,
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
                             # tree_depth(),
                             learn_rate(),
                             loss_reduction(),
                             levels = 5)

xgboost_model <- boost_tree(
  mode = "regression",
  trees = 1000,
  min_n = tune(),
  tree_depth = 6, #tune(),
  learn_rate = tune(),
  loss_reduction = tune()) %>% 
  set_engine("xgboost")

xgboost_prepared %>% juice

xgboost_wf <- workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_model)

xgboost_res <- xgboost_wf %>% 
  tune_grid(resamples = xgboost_folds,
            grid = xgboost_grid)

xgboost_res %>% collect_metrics() %>% print(n = 250)

xgboost_fit <- fit(xgboost_wf, xgboost_prepared %>% 
                     juice())

xgboost_fit %>% 
  pull_workflow_fit() %>% 
  vip()

# 
# xgboost_res %>% 
#   collect_metrics() %>% 
#   filter(min_n == 11,
#          learn_rate == 0.1,
#          loss_reduction == xgboost_res %>% 
#            collect_metrics() %>% 
#            filter(min_n == 11,
#                   learn_rate == 0.1) %>% 
#            slice(7) %>% 
#            pull(loss_reduction))
# 
