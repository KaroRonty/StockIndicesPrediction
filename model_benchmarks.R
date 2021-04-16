model_naive <- function(selected_country){
  
  model_data_naive <- to_model_arima %>% 
    as_tibble() %>% 
    filter(country == selected_country) %>% 
    mutate(set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test")) %>% 
    ungroup()
  
  features_selected <- model_data_naive %>% 
    filter(set == "training") %>% 
    ungroup() %>% 
    summarise_all(~sum(!is.na(.x)) / 12) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(years_data = 1) %>% 
    rownames_to_column("feature") %>% 
    filter(years_data >= 7) %>% 
    pull(feature)
  
  model_training_naive <- model_data_naive %>% 
    filter(set == "training") %>% 
    select(country, !!features_selected) %>%
    # na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_leakage_naive <- model_data_naive %>% 
    filter(set == "leakage") %>%
    select(!!colnames(model_training_naive)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_test_naive <- model_data_naive %>% 
    filter(set == "test") %>%
    select(!!colnames(model_training_naive)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  # paste0("list(model = model_training_naive %>% 
  #        model(NAIVE = NAIVE(",cagr_n_year,"),
  #              MEAN = MEAN(",cagr_n_year,")),
  #      training =", model_training_naive,       
  #      "leakage =", model_leakage_naive,
  #      "test =", model_test_naive,")") %>% 
  #   parse(text = .) %>% 
  #   eval()
  
  paste0("list(model = model_training_naive %>% 
         model(NAIVE = NAIVE(cagr_n_year)),
         training = model_training_naive,       
         leakage = model_leakage_naive,
         test = model_test_naive)") %>% 
    parse(text = .) %>% 
    eval()
       }

if(exists("cl")){
  print("Starting naive cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

# 27 sec
tic_naive <- Sys.time()
naive_model <- future_map(countries_to_predict,
                          ~model_naive(.x))
print(toc_naive <- Sys.time() - tic_naive)

naive_fcast <- future_map(seq_len(length(naive_model)),
                          ~naive_model %>% 
                            pluck(.x) %>% 
                            pluck(1) %>% 
                            forecast(naive_model %>% 
                                       pluck(.x) %>% 
                                       pluck(4)) %>% 
                            as_tsibble(index = date)) %>% 
  reduce(bind_rows)

naive_fitted <- future_map(seq_len(length(naive_model)),
                           ~naive_model %>% 
                             pluck(.x) %>% 
                             pluck(1) %>% 
                             pull(NAIVE) %>%
                             pluck(1) %>%
                             .$fit %>%
                             .$.fitted) %>%
  reduce(c)


naive_pred <- naive_fcast %>% 
  pull(.mean)

naive_actual <- future_map(seq_len(length(naive_model)),
                           ~naive_model %>% 
                             pluck(.x) %>% 
                             pluck(4) %>% 
                             pull(cagr_n_year)) %>% 
  reduce(c)

naive_training_to_plot <- future_map(
  seq_len(length(naive_model)),
  ~naive_model %>% 
    pluck(.x) %>% 
    pluck(2) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

naive_leakage_to_plot <- future_map(
  seq_len(length(naive_model)),
  ~naive_model %>% 
    pluck(.x) %>% 
    pluck(3) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

naive_actual_to_plot <- future_map(
  seq_len(length(naive_model)),
  ~naive_model %>% 
    pluck(.x) %>% 
    pluck(4) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

pred_plot_naive <- naive_fcast %>% 
  autoplot(color = "red") +
  autolayer(naive_training_to_plot, cagr_n_year, color = "black") +
  autolayer(naive_leakage_to_plot, cagr_n_year, color = "gray") +
  autolayer(naive_actual_to_plot, cagr_n_year, color = "black") +
  facet_wrap(~country) +
  coord_cartesian(ylim = c(0.75,1.5)) +
  labs(title = "naive",
       x = "Date",
       y = cagr_name) +
  theme_minimal() +
  theme(legend.position = "none")

pred_vs_actual_naive <- naive_actual_to_plot %>% 
  as_tibble() %>% 
  rename(actual = cagr_n_year) %>% 
  inner_join(naive_fcast %>% 
               as_tibble() %>% 
               select(date, country, naive_pred = .mean)) %>% 
  suppressMessages()

preds_vs_actuals <- preds_vs_actuals %>% 
  left_join(pred_vs_actual_naive)


suppressMessages(
  pred_vs_actual_naive %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(naive_mape = median(abs(((actual) - naive_pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_naive %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      naive_mape = median(abs(((actual) - naive_pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()

