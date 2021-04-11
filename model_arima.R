to_model_arima_temp <- build.x(~ .,
                               data = to_model_mm[, -1:-2],
                               contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date) %>% 
  # TODO
  mutate_if(is.numeric, ~ifelse(.x == 1000, NA, .x))

model_data_arima_temp <- make_splits(split_indices, 
                                     to_model_arima_temp %>% 
                                       # select(-date) %>% 
                                       mutate(date = as.numeric(date)) %>%
                                       as.matrix())
model_training_arima_temp <- training(model_data_arima_temp)
model_test_arima_temp <- testing(model_data_arima_temp)

model_recipe_arima <- recipe(cagr_n_year ~ # FIXME
                               cape + 
                               dividend_yield + 
                               rate_10_year + # FIXME
                               unemployment +
                               s_rate_10_year +
                               cpi, 
                             data = model_training_arima_temp) %>% 
  step_knnimpute(cape,
                 dividend_yield,
                 rate_10_year, 
                 unemployment, 
                 s_rate_10_year,
                 cpi)

to_model_arima <- model_recipe_arima %>% 
  prep() %>% 
  bake(to_model_arima_temp) %>% 
  mutate(date = to_model_mm$date,
         country = to_model_mm$country,
         .before = 1)

model_arima <- function(selected_country){
  
  model_data_arima <- to_model_arima %>% 
    as_tibble() %>% 
    filter(country == selected_country) %>% 
    mutate(set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test")) %>% 
    ungroup()
  
  features_selected <- model_data_arima %>% 
    filter(set == "training") %>% 
    ungroup() %>% 
    summarise_all(~sum(!is.na(.x)) / 12) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(years_data = 1) %>% 
    rownames_to_column("feature") %>% 
    filter(years_data >= 7) %>% 
    pull(feature)
  
  model_training_arima <- model_data_arima %>% 
    filter(set == "training") %>% 
    select(country, !!features_selected) %>%
    # na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_leakage_arima <- model_data_arima %>% 
    filter(set == "leakage") %>%
    select(!!colnames(model_training_arima)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_test_arima <- model_data_arima %>% 
    filter(set == "test") %>%
    select(!!colnames(model_training_arima)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  list(model = model_training_arima %>% 
         model(arima = ARIMA(as.formula(
           paste("cagr_n_year", 
                 "~", 
                 paste0(!!predictors[predictors %in% features_selected],
                        collapse = " + "),
                 # include differencing
                 "+ PDQ(D = 1:5)")
                 # + PDQ(D = 1:4)"
                 ))),
       training = model_training_arima,
       leakage = model_leakage_arima,
       test = model_test_arima)
}

if(exists("cl")){
  print("Starting ARIMA cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

# 27 sec
tic_arima <- Sys.time()
arima_model <- future_map(countries_to_predict,
                          ~model_arima(.x))
print(toc_arima <- Sys.time() - tic_arima)

arima_fcast <- future_map(seq_len(length(arima_model)),
                          ~arima_model %>% 
                            pluck(.x) %>% 
                            pluck(1) %>% 
                            forecast(arima_model %>% 
                                       pluck(.x) %>% 
                                       pluck(4))) %>% 
  reduce(bind_rows)

arima_fitted <- future_map(seq_len(length(arima_model)),
                           ~arima_model %>% 
                             pluck(.x) %>% 
                             pluck(1) %>% 
                             pull(arima) %>%
                             pluck(1) %>%
                             .$fit %>%
                             .$est %>%
                             .$.fitted) %>%
  reduce(c)


arima_pred <- arima_fcast %>% 
  pull(.mean)

arima_actual <- future_map(seq_len(length(arima_model)),
                           ~arima_model %>% 
                             pluck(.x) %>% 
                             pluck(4) %>% 
                             pull(cagr_n_year)) %>% 
  reduce(c)

arima_training_to_plot <- future_map(
  seq_len(length(arima_model)),
  ~arima_model %>% 
    pluck(.x) %>% 
    pluck(2) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

arima_leakage_to_plot <- future_map(
  seq_len(length(arima_model)),
  ~arima_model %>% 
    pluck(.x) %>% 
    pluck(3) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

arima_actual_to_plot <- future_map(
  seq_len(length(arima_model)),
  ~arima_model %>% 
    pluck(.x) %>% 
    pluck(4) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

pred_plot_arima <- arima_fcast %>% 
  autoplot(color = "red") +
  autolayer(arima_training_to_plot, cagr_n_year, color = "black") +
  autolayer(arima_leakage_to_plot, cagr_n_year, color = "gray") +
  autolayer(arima_actual_to_plot, cagr_n_year, color = "black") +
  facet_wrap(~country) +
  coord_cartesian(ylim = c(0.75,1.5)) +
  labs(title = "ARIMA",
       x = "Date",
       y = cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

pred_vs_actual_arima <- arima_actual_to_plot %>% 
  as_tibble() %>% 
  rename(actual = cagr_n_year) %>% 
  inner_join(arima_fcast %>% 
               as_tibble() %>% 
               select(date, country, arima_pred = .mean)) %>% 
  suppressMessages()

suppressMessages(
  pred_vs_actual_arima %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(arima_mape = median(abs(((actual) - arima_pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_arima %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      arima_mape = median(abs(((actual) - arima_pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()


# VALIDATION -----

# differencing only applied to LM for AUSTRALIA 
map(1:8, ~arima_model[[.x]] %>% 
      .$model %>% 
      .$arima) %>% 
  reduce(c) %>% 
  tibble(models = .,
         country = map(1:8, ~arima_model[[.x]] %>% .$model %>% 
          .$country) %>%
          reduce(c))

# checking residuals of ARIMA models
# CANADA; USA; UK; NETHERLANDS; GERMANY; SPAIN; SWITZERLAND are not white-noise
# including mandatory d or D does not change the white-noise in the series


resid_data <- map_dfr(1:8, ~arima_model %>% 
  pluck(.x) %>% 
  .$model %>% 
  .$arima %>% 
  pluck(1) %>% 
  residuals() %>% 
  mutate(country = arima_model %>% 
           pluck(.x) %>% 
           .$model %>% 
           .$country) %>% 
  as_tibble())

resid_data %>% 
  ggplot(aes(date, .resid)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "All countries show non white-noise typical residuals") +
  theme_bw()

# train a single model

# get PACF statistics
acf_data <- map_dfr(1:8, ~arima_model %>% 
  pluck(.x) %>%
  pluck(4) %>% 
  feasts::PACF() %>% 
  as_tibble()) %>% 
  left_join(
    map_dfr(1:8, ~arima_model %>% 
              pluck(.x) %>%
              pluck(4) %>% 
              feasts::ACF() %>% 
              as_tibble())
  )
  
acf_data %>% 
  select(-pacf) %>% 
  ggplot(aes(lag, acf)) +
  geom_col(width = .1) +
  facet_wrap(~country) +
  labs(title = "ACF Analysis shows considerable autocorrelation left in the residuals") +
  theme_bw()

acf_data %>% 
  select(-acf) %>% 
  ggplot(aes(lag, pacf)) +
  geom_col(width = .1) +
  facet_wrap(~country) +
  labs(title = "PACF shows strong autocorrelation effect of previously predicted CAGR lag") +
  theme_bw()

# get ACF statistics
map(1:8, ~arima_model %>% 
      pluck(.x) %>%
      pluck(4) %>% 
      feasts::ACF() %>%
      autoplot() +
      labs(title = paste("ACF for", countries_to_predict[.x])))



future_map(seq_len(length(arima_model)),
           ~arima_model %>% 
             pluck(.x) %>% 
             pluck(1) %>% 
             pull(arima) %>%
             pluck(1) %>%
             .$fit %>%
             .$est %>%
             .$.fitted) %>%