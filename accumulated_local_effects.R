library(signs)
library(scales)

model_recipe_arima_prepared <- model_recipe_arima %>% 
  prep()

model_recipe_elastic_prepared <- model_recipe_elastic %>% 
  prep()

model_recipe_prepared <- model_recipe %>% 
  prep()

# Join product names and models for PDP
to_ale_training <- to_model_mm %>% 
  filter(country %in% countries_to_predict) %>% 
  filter(date < yearmonth(leakage_start_date))

# Get feature classes
feature_classes <- to_ale_training %>% 
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

continuous_features <- feature_classes %>% 
  pull(row_number)

tic_ale_training <- Sys.time()
ale_training <- map(
  continuous_features,
  ~safely(custom_ale)(X = as.data.frame(to_ale_training),
                      X.model = stack_model, 
                      pred.fun = predict_stack %>% 
                        suppressMessages(),
                      J = .x,
                      K = 100)$result %>% 
    as_tibble() %>% 
    mutate(feature = colnames(to_ale_training)[.x]) %>%
    select(feature, everything())) %>% 
  bind_rows() %>% 
  suppressMessages()
(toc_ale_training <- Sys.time() - tic_ale_training)

saveRDS(ale_training, 
        paste0("ale_continuous_stack_", 
               Sys.Date(), 
               "_",
               round(runif(1, 0, 1000), 0), 
               ".RDS"))

ale_continuous_clean <- ale_training %>% 
  # filter(case_when(
  #   feature == "cape" & (x.values > 0 & x.values < 40) ~ TRUE,
  #   feature == "cpi" & (x.values > 20 & x.values < 100) ~ TRUE,
  #   feature == "dividend_yield" & (x.values > 0 & x.values < 6) ~ TRUE,
  #   feature == "rate_10_year" & (x.values > -10 & x.values < 20) ~ TRUE,
  #   feature == "s_rate_10_year" & (x.values > -10 & x.values < 20) ~ TRUE,
  #   feature == "unemployment" & (x.values > 0 & x.values < 20) ~ TRUE)) %>%
  mutate(feature = case_when(
    feature == "cape" ~ "CAPE",
    feature == "cpi" ~ "CPI",
    feature == "dividend_yield" ~ "Dividend yield",
    feature == "rate_10_year" ~ "Long-term interest rate",
    feature == "s_rate_10_year" ~ "Short-term interest rate",
    feature == "unemployment" ~ "Unemployment"))

ale_stack_plot <- ale_continuous_clean %>% 
  ggplot(aes(x.values, f.values, color = feature)) +
  geom_line() +
  geom_rug(color = "black", alpha = 0.5, sides = "b") +
  geom_hline(yintercept = 0) +
  facet_wrap(~feature, scales = "free_x") +
  scale_y_continuous(labels = signs::signs_format(
    format = scales::percent_format(1),
    add_plusses = TRUE)) +
  labs(title = "Effect on the future 5-year CAGR",
       subtitle = "Accumulated local effects for stacked model",
       x = "Value of feature",
       y = "Absolute change in future 5-year CAGR") +
  theme_minimal() +
  theme(legend.position = "none")
