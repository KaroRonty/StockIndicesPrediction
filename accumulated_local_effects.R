library(furrr)
library(signs)
library(scales)

if(exists("cl")){
  print("Starting ALE cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

model_recipe_arima_prepared <- model_recipe_arima %>% 
  prep()

model_recipe_elastic_prepared <- model_recipe_elastic %>% 
  prep()

model_recipe_prepared <- model_recipe %>% 
  prep()

# Join product names and models for PDP
to_ale <- to_model_mm %>% 
  filter(country %in% countries_to_predict)

# Get feature classes
feature_classes <- to_ale %>% 
  as_tibble() %>% 
  summarise_all(class) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(feature = 1, class = 2) %>% 
  mutate(row_number = row_number()) %>% 
  # Remove some extra columns in dataset from ALE 
  filter(!(feature %in% c("date",
                          "cagr_n_year",
                          "(Intercept)")),
         !str_detect(feature, "country"))

# Get categorical and continuous features separately for ALE
# categorical_features <- feature_classes %>% 
#   filter(class == "factor") %>% 
#   pull(row_number)

continuous_features <- feature_classes %>% 
  # filter(class != "factor") %>% 
  pull(row_number)

predict_stack(stack_model, 
              to_model_mm)

tic_continuous <- Sys.time()
ale_continuous <- future_map(
  continuous_features,
  ~safely(custom_ale)(X = as.data.frame(to_ale),
                      X.model = stack_model, 
                      pred.fun = predict_stack,
                      J = .x,
                      K = 100)$result %>% 
    as_tibble() %>% 
    mutate(feature = # FIXME 
             colnames(to_ale)[.x]) %>% 
    select(feature, everything()),
  .y = .x) %>% 
  reduce(bind_rows)
(toc_continuous <- Sys.time() - tic_continuous)

# FIXME
t4 <- custom_ale(X = as.data.frame(to_ale),
                 X.model = stack_model, 
                 pred.fun = predict_stack,
                 J = continuous_features[1],
                 K = 3)

# TODO
saveRDS(ale_continuous, 
        paste0("ale_continuous_xgb_", 
               Sys.Date(), 
               "_",
               round(runif(1, 0, 1000), 0), 
               ".RDS"))

ale_continuous_clean <- ale_continuous %>% 
  filter(case_when(
    feature == "cape" & (x.values > 0 & x.values < 100) ~ TRUE,
    feature == "cpi" & (x.values > 20 & x.values < 100) ~ TRUE,
    feature == "dividend_yield" & (x.values > 0 & x.values < 6) ~ TRUE,
    feature == "rate_10_year" & (x.values > -10 & x.values < 20) ~ TRUE,
    feature == "s_rate_10_year" & (x.values > -10 & x.values < 20) ~ TRUE,
    feature == "unemployment" & (x.values > 0 & x.values < 20) ~ TRUE)) %>%
  mutate(feature = case_when(
    feature == "cape" ~ "CAPE",
    feature == "cpi" ~ "CPI",
    feature == "dividend_yield" ~ "Dividend yield",
    feature == "rate_10_year" ~ "Long-term interest rate",
    feature == "s_rate_10_year" ~ "Short-term interest rate",
    feature == "unemployment" ~ "Unemployment"))

ale_continuous_clean %>% 
  ggplot(aes(x.values, f.values, color = feature)) +
  geom_line() +
  geom_rug(color = "black", alpha = 0.5, sides = "b") +
  geom_hline(yintercept = 0) +
  facet_wrap(~feature, scales = "free_x") +
  scale_y_continuous(labels = signs::signs_format(
    format = scales::percent_format(1),
    add_plusses = TRUE)) +
  labs(title = "Effect on the future 5-year CAGR",
       subtitle = "Accumulated local effects for XGBoost",
       x = "Value of feature",
       y = "Absolute change in future 5-year CAGR") +
  theme_minimal() +
  theme(legend.position = "none")
