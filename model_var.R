to_model_var_temp <- build.x(~ .,
                             data = to_model_mm[, -1:-2],
                             contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date) %>% 
  # TODO
  mutate_if(is.numeric, ~ifelse(.x == 1000, NA, .x))

model_data_var_temp <- make_splits(split_indices, 
                                   to_model_var_temp %>% 
                                     # select(-date) %>% 
                                     mutate(date = as.numeric(date)) %>%
                                     as.matrix())

model_training_var_temp <- training(model_data_var_temp)
model_test_var_temp <- testing(model_data_var_temp)

model_recipe_var <- recipe(cagr_n_year ~ # FIXME
                             cape + 
                             dividend_yield + 
                             rate_10_year + # FIXME
                             unemployment +
                             s_rate_10_year +
                             cpi, 
                           data = model_training_var_temp) %>% 
  step_knnimpute(cape,
                 dividend_yield,
                 rate_10_year, 
                 unemployment, 
                 s_rate_10_year,
                 cpi)

to_model_var <- model_recipe_var %>% 
  prep() %>% 
  bake(to_model_var_temp) %>% 
  mutate(date = to_model_mm$date,
         country = to_model_mm$country,
         .before = 1)

# errors to big if all features are  differenced
to_model_var_diff <- to_model_var %>% 
  mutate(cagr_n_year = difference(cagr_n_year))

# manual set up to prevent name conflicts
predictors_var <- c("cagr_n_year", "cape", "dividend_yield", "rate_10_year", 
                    "s_rate_10_year", "cpi")

model_var <- function(selected_country){
  
  model_data_var <- to_model_var_diff %>% 
    as_tibble() %>% 
    filter(country == selected_country) %>% 
    mutate(set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test")) %>% 
    ungroup()
  
  features_selected <- model_data_var %>% 
    filter(set == "training") %>% 
    ungroup() %>% 
    summarise_all(~sum(!is.na(.x)) / 12) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(years_data = 1) %>% 
    rownames_to_column("feature") %>% 
    filter(years_data >= 7) %>% # FIXME longer period
    as_tibble() %>% 
    filter(!feature %in% c("set")) %>% 
    pull(feature)
  
  model_training_var <- model_data_var %>% 
    filter(set == "training") %>% 
    select(country, !!features_selected) %>%
    na.omit() %>%
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_leakage_var <- model_data_var %>% 
    filter(set == "leakage") %>%
    select(!!colnames(model_training_var)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  model_test_var <- model_data_var %>% 
    filter(set == "test") %>%
    select(!!colnames(model_training_var)) %>% 
    na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  paste0("list(model = model_training_var %>% 
         model(var = VAR(vars(",
         paste0(predictors_var[predictors_var %in% 
                                 features_selected], collapse = ", "),
         "))),
        training = model_training_var,
        leakage = model_leakage_var,
        test = model_test_var)") %>% 
    parse(text = .) %>% 
    eval()
}

if(exists("cl")){
  print("Starting VAR cluster...")
  stopCluster(cl)
  rm(cl)
}

cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

# 1 sec
tic_var <- Sys.time()
var_model <- future_map(countries_to_predict,
                        ~model_var(.x))
print(toc_var <- Sys.time() - tic_var)



# FIXME: something with length of names
var_fcast <- future_map(seq_len(length(var_model)),
                        ~var_model %>% 
                          pluck(.x) %>% 
                          pluck(1) %>% 
                          forecast(var_model %>% 
                                     pluck(.x) %>% 
                                     pluck(4) %>% 
                                     select(-cagr_n_year))) %>% 
  reduce(bind_rows)

backtransform_var <- function(bb){
  var_fcast %>% 
    filter(.model == "var",
           country == bb) %>% 
    pull(.mean_cagr_n_year) %>% 
    na.omit() %>% 
    as.vector() %>% 
    diffinv(lag = 1, 
            xi = to_model_var %>% 
              filter(date < yearmonth(leakage_start_date) | # FIXME 
                       date >= yearmonth(leakage_start_date)) %>% 
              filter(country == bb) %>% 
              select(cagr_n_year) %>% 
              na.omit() %>% 
              tail(n = 1)
    ) %>% 
    as_tibble() %>% 
    mutate(cagr_n_year = value,
           country = bb) %>% 
    select(-value) %>% 
    slice(-1)
}

var_fcast_bt <- map(countries_to_predict, 
                    ~backtransform_var(.x)) %>% 
  reduce(bind_rows)

var_fitted <- future_map(seq_len(length(var_model)),
                         ~var_model %>% 
                           pluck(.x) %>% 
                           pluck(1) %>% 
                           pull(var) %>%
                           pluck(1) %>%
                           .$fit %>%
                           .$est %>%
                           .$.fitted) %>%
  reduce(c)

var_pred <- var_fcast %>% 
  pull(.mean_cagr_n_year)

var_actual <- future_map(seq_len(length(var_model)),
                         ~var_model %>% 
                           pluck(.x) %>% 
                           pluck(4) %>% 
                           pull(cagr_n_year)) %>% 
  reduce(c)

var_training_to_plot <- future_map(
  seq_len(length(var_model)),
  ~var_model %>% 
    pluck(.x) %>% 
    pluck(2) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

var_leakage_to_plot <- future_map(
  seq_len(length(var_model)),
  ~var_model %>% 
    pluck(.x) %>% 
    pluck(3) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

var_actual_to_plot <- future_map(
  seq_len(length(var_model)),
  ~var_model %>% 
    pluck(.x) %>% 
    pluck(4) %>% 
    select(country, date, cagr_n_year)) %>% 
  reduce(bind_rows)

pred_plot_var <- var_fcast %>% 
  select(date, .distribution, .mean_cagr_n_year) %>% 
  ggplot(aes(date, .mean_cagr_n_year)) +
  geom_line() +
  autolayer(var_training_to_plot, cagr_n_year, color = "black") +
  autolayer(var_leakage_to_plot, cagr_n_year, color = "gray") +
  autolayer(var_actual_to_plot, cagr_n_year, color = "black") +
  facet_wrap(~country)

# autoplot(color = "red") +
# autolayer(var_training_to_plot, cagr_n_year, color = "black") +
# autolayer(var_leakage_to_plot, cagr_n_year, color = "gray") +
# autolayer(var_actual_to_plot, cagr_n_year, color = "black") +
# facet_wrap(~country) +
# coord_cartesian(ylim = c(0.75,1.5)) +
# labs(title = "var",
#      x = "Date",
#      y = cagr_name) +
# theme_minimal() +
# theme(legend.position = "none")

pred_vs_actual_var <- var_actual_to_plot %>% 
  as_tibble() %>% 
  rename(actual = cagr_n_year) %>% 
  inner_join(var_fcast %>% 
               as_tibble() %>% 
               select(date, country, var_pred = .mean_cagr_n_year)) %>% 
  suppressMessages()

suppressMessages(
  pred_vs_actual_var %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(var_mape = median(abs(((actual) - var_pred) / actual)),
              mean_mape = median(abs(((actual) - mean_prediction) / actual))))

suppressMessages(
  pred_vs_actual_var %>% 
    inner_join(mean_predictions) %>% 
    group_by(country) %>% 
    summarise(
      var_mape = median(abs(((actual) - var_pred) / actual)),
      mean_mape = median(abs(((actual) - mean_prediction) / actual))) %>%
    ungroup() %>% 
    summarise_if(is.numeric, median)) %>% 
  print()

preds_vs_actuals <- preds_vs_actuals %>% 
  left_join(pred_vs_actual_var)

# form of results for tables 
acc_single_var <- preds_vs_actuals %>% 
  inner_join(mean_predictions) %>% 
  group_by(country) %>% 
  summarise(
    MAPE = median(abs(((actual) - var_pred) / actual)),
    MAE = mean(abs(actual - var_pred)),
    RMSE = sqrt(sum((var_pred - actual)^2) / 121)) %>%  # FIXME
  mutate(model = "var_single") %>% 
  pivot_longer(cols = c(MAPE, MAE, RMSE),
               names_to = "errors",
               values_to = "value") %>% 
  suppressMessages()


# Validation --------------------------------------------------------------

# differencing only applied to LM for AUSTRALIA 
map(1:length(countries_to_predict), 
    ~var_model[[.x]] %>% 
      .$model %>% 
      .$var) %>% 
  reduce(c) %>% 
  tibble(models = .,
         country = map(1:length(countries_to_predict), 
                       ~var_model[[.x]] %>% 
                         .$model %>% 
                         .$country) %>%
           reduce(c))

# checking residuals of var models
# CANADA; USA; UK; NETHERLANDS; GERMANY; SPAIN; SWITZERLAND are not white-noise
# including mandatory d or D does not change the white-noise in the series

resid_data <- map_dfr(1:length(countries_to_predict), 
                      ~var_model %>% 
                        pluck(.x) %>% 
                        .$model %>% 
                        .$var %>% 
                        pluck(1) %>% 
                        residuals() %>% 
                        mutate(country = var_model %>% 
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

acf_data <- map_dfr(1:length(countries_to_predict), 
                    ~var_model %>% 
                      pluck(.x) %>%
                      pluck(2) %>% 
                      PACF() %>% 
                      as_tibble()) %>% 
  left_join(
    map_dfr(1:length(countries_to_predict), 
            ~var_model %>% 
              pluck(.x) %>%
              pluck(2) %>% 
              ACF() %>% 
              as_tibble())
  )

acf_data %>% 
  select(-pacf) %>% 
  ggplot(aes(lag, acf)) +
  geom_col(width = 0.1) +
  facet_wrap(~country) +
  labs(title = "ACF Analysis shows considerable autocorrelation left in the residuals") +
  theme_bw()

acf_data %>% 
  select(-acf) %>% 
  ggplot(aes(lag, pacf)) +
  geom_col(width = 0.1) +
  facet_wrap(~country) +
  labs(title = "PACF shows strong autocorrelation effect of previously predicted CAGR lag") +
  theme_bw()

# significance of params
sig_var_auto <- map_dfr(1:length(countries_to_predict), 
                        ~var_model[[.x]] %>% 
                          .$model %>% 
                          .$var %>% 
                          pluck(1) %>% 
                          .$fit %>% 
                          .$par %>% 
                          mutate(country = var_model[[.x]] %>% 
                                   .$model %>% 
                                   .$country))

sig_var_auto %>% 
  filter(term %in% c("ar1", "ar2", "sar1", "sar2", "ma1", "sma1")) %>% 
  ggplot(aes(country, p.value)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 0.05)) +
  facet_wrap(~term, scales = "free") +
  labs(title = "Significance Analysis of d = 1 forced auto.var models",
       subtitle = "Coefficients are less significant but also overall less prevalent because of the differencing") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
