# 16.01.2021
# RER10 ------------------------------------------------------------------
# filtered exchange rate and cpi data
cpi_long_filtered <- cpi_long %>% 
  filter(country %in% countries)

ex_long_filtered <- ex_long %>% 
  filter(country %in% countries)


dates_length <- length(seq(yearmonth("1950 Jan"), yearmonth("2020 Aug"), by = 1))
cc <- cpi_long_filtered %>% select(country) %>% distinct(country) %>% c() %>% unname() %>% rep(each = 7) %>% unlist()

# create RER10 df

# US CPI as comparison benchmark for RER10 calculation
cpi_us <- cpi_long_filtered %>% 
  filter(country == "USA") %>% 
  rename(cpi_us = cpi) %>% 
  select(cpi_us) %>% 
  reduce(c) %>% 
  rep(each = 7) 

rer10 <- tsibble(
  # date range
  date = rep(seq(yearmonth("1950 Jan"), yearmonth("2020 Aug"), by = 1), each = 7),
  # get countries 
  country = cpi_long_filtered %>% 
    select(country) %>% 
    distinct(country) %>% 
    c() %>% 
    unname() %>% 
    rep(each = dates_length) %>% 
    unlist(),
  # US CPI as benchmark
  cpi_us = cpi_us,
  key = country,
  index = date) %>%
  as_tibble() %>% 
  arrange(date)

# join with previous filtered sources
rer10 <- rer10 %>% 
  full_join(cpi_long_filtered) %>% 
  full_join(ex_long_filtered)

rer10 <- rer10 %>% 
  group_by(country) %>% 
  mutate(rer10_n = ((exchange_rate * cpi) / cpi_us), # numerator
         rer10_d = zoo::rollmean(rer10_n, 120, fill = NA, align = "right"), # denominator
         rer10 = rer10_n / rer10_d) %>% 
  select(date, country, rer10)

# set RER10 because of currency changes for SPAIN to NA to reduce bias from 1999 to 2009 
rer10 <- rer10 %>% 
  filter(country == "SPAIN") %>% 
  mutate(rer10 = case_when(date < yearmonth("1999 Jan") ~ as.numeric(rer10),
                           date >= yearmonth("1999 Jan") & 
                             date < yearmonth("2009 Jan") ~ NA_real_,
                           date >= yearmonth("2009 Jan") ~ as.numeric(rer10))) %>% 
  full_join(rer10)

# TODO should we consider the change to euro at all? --> how does radha do it?

rer10 %>% 
  ggplot(aes(x = date, y = rer10)) +
  geom_line() + 
  facet_wrap(~country)


# 17.11.2020  -----
# FS with Leakage as Test Set (despite leakage and correlation between leakage set and real test set)

feature_selection_leakage_as_test <- function(cagr, countries){
  # Get numeric value from CAGR name
  y <- suppressMessages(extract_numeric(cagr))
  
  # Test set is has a maximum length based on CAGR years
  leakage_end_date <- max(as.Date(max_data_date) - months(187), 
                          as.Date(max_data_date) -  years(10) -
                            years(y))
  
  leakage_start_date <- leakage_end_date - years(y)
  
  to_model <- to_model_exploration %>% 
    filter(country %in% countries) %>% # TODO
    select(date, country, !!cagrs, !!predictors) %>% 
    na.omit()
  
  # FIXME training set according to CAGR years
  # Split into different sets
  training <- to_model %>% 
    filter(date < yearmonth(leakage_start_date))
  
  leakage_set <- to_model %>%
    filter(date >= yearmonth(leakage_start_date),
           date < yearmonth(leakage_end_date))
  
  test <- to_model %>% 
    filter(date >= yearmonth(leakage_end_date))
  
  # Formulas for mean, naive and ARIMA
  # train 
  
  
  models_ts <- map(formulas_fs, ~training %>% 
                     model(ARIMA = ARIMA(as.formula(.x)),
                           VAR = VAR(as.formula(.x))))
  
  
  
  
  # Train mean and naive models
  models_naive_mean <- paste0("models_mean_cagr_",
                              y,
                              " <- training %>% model(MEAN = MEAN(",
                              cagrs[y],
                              "), NAIVE = NAIVE(", 
                              cagrs[y],
                              "))") %>% 
    parse(text = .) %>% 
    eval()
  
  
  
  # TRAINING VALIDATION
  
  fcast_no_leakage_training <- map(1:length(formulas_fs), ~models_ts[[.x]] %>% 
                                     forecast(training) %>% 
                                     filter(date <= yearmonth(leakage_end_date)) %>%
                                     mutate(.formula = paste(formulas_fs_simple[.x])) %>% 
                                     
                                     bind_rows(models_naive_mean %>%
                                                 forecast(training) %>% 
                                                 filter(date <= yearmonth(leakage_end_date))))
  
  
  # Calculate leakage-free accuracies
  acc_no_leakage_training <- map(1:length(formulas_fs), ~fcast_no_leakage_training[[.x]] %>% 
                                   accuracy(training) %>% 
                                   select(.model, country, .type, RMSE, MAE, MAPE) %>% 
                                   mutate(.formula = paste(formulas_fs_simple[.x]), 
                                          .predictors = (str_count(.formula, pattern = "\\+") + 1),
                                          .type = "Training") %>%
                                   filter(!is.na(MAE))) %>% 
    reduce(full_join)
  
  # TEST VALIDATION
  
  
  # Make forecasts and remove leakage
  # in order to validate accuracies, one needs already the winners 
  
  fcast_leakage_as_test <- map(1:length(formulas_fs), ~models_ts[[.x]] %>%
                                 forecast(leakage_set) %>%
                                 filter(date >= yearmonth(leakage_start_date),
                                        date < yearmonth(leakage_end_date)) %>%
                                 mutate(.formula = paste(formulas_fs_simple[.x])) %>%
                                 
                                 bind_rows(models_naive_mean %>%
                                             forecast(leakage_set) %>%
                                             filter(date >= yearmonth(leakage_start_date),
                                                    date < yearmonth(leakage_end_date))))
  
  
  # Calculate leakage-free accuracies
  acc_leakage_as_test <- map(1:length(formulas_fs), ~fcast_leakage_as_test[[.x]] %>%
                               accuracy(bind_rows(training, leakage_set)) %>%
                               select(.model, country, .type, RMSE, MAE, MAPE) %>%
                               mutate(.formula = paste(formulas_fs_simple[.x]),
                                      .predictors = (str_count(.formula, pattern = "\\+") + 1)) %>%
                               filter(!is.na(MAE))) %>%
    reduce(full_join) %>%
    bind_rows(acc_no_leakage_training)
  
}

# training and validation set comparison <-----
all_feature_accuracies_leakage_as_test <- future_map(cagrs[5], # fixed value
                                                     ~feature_selection_leakage_as_test(.x, countries),
                                                     .progress = TRUE) %>% 
  reduce(full_join)

# check best models per country (test and training comparison)
feature_accuracies_order_training_l <- all_feature_accuracies_leakage_as_test %>% 
  filter(.type == "Training") %>% 
  group_by(country,) %>% 
  arrange(country, MAPE) %>% 
  mutate(mape_order_training = order(MAPE),
         rmse_order_training = order(RMSE),
         mae_order_training = order(MAE)) %>% 
  select(.model, country, .formula, mape_order_training, rmse_order_training, mae_order_training)

# complete test and training MAPE performance comparison
feature_accuracies_order_full_l <- all_feature_accuracies_leakage_as_test %>% 
  filter(.type == "Test") %>% 
  group_by(country) %>% 
  arrange(country, MAPE) %>%
  mutate(mape_order_test = order(MAPE),
         rmse_order_test = order(RMSE),
         mae_order_test = order(MAE)) %>% 
  full_join(feature_accuracies_order_training_l, by = c(".formula" = ".formula",
                                                        "country" = "country",
                                                        ".model" = ".model")) %>% 
  arrange(country, mae_order_training) 
# slice(1:5) 




# 17.11.2020 -----
# Adding new variables

rate_3_month_long %>% 
  filter(country %in% countries) %>% 
  as_tsibble(key = country) %>% 
  ggplot(aes(x = date, y = rate_3_month, col = country)) + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~country)

rate_10_year_long %>% 
  filter(country %in% countries) %>% 
  as_tsibble(key = country) %>% 
  ggplot(aes(x = date, y = rate_10_year, col = country)) + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~country)

# 17.11.2020  -----
# TRAINING SET CALCULATION OUTSIDE THE FUNCITON (no incerasing window)

leakage_end_date <- max(as.Date(max_data_date) - months(187), 
                        as.Date(max_data_date) -  years(10) -
                          years(5))

leakage_start_date <- leakage_end_date - years(5)

to_model <- to_model_exploration %>% 
  filter(country %in% countries) %>% # TODO
  select(date, country, cagrs[5], !!predictors) %>% 
  # group_by(country) %>% # FIXME
  # mutate(lag_cagr = lag(cagr_5_year, 1)) %>%  #FIXME
  na.omit()

# FIXME training set according to CAGR years
# Split into different sets
training <- to_model %>% 
  filter(date < yearmonth(leakage_start_date))

leakage_set <- to_model %>% 
  filter(date >= yearmonth(leakage_start_date),
         date < yearmonth(leakage_end_date))

test <- to_model %>% 
  filter(date == yearmonth(leakage_end_date))

# just using correlation does not take the model dynamics into account and should not be used for pre selection
corr_country <- map(countries, ~training %>% 
                      as_tibble() %>%
                      filter(country == .x) %>% 
                      select(-date, -country) %>% 
                      cor())

names(corr_country) <- print(countries)

# 16.11.2020

# FIXME
# too many model combinations, leads to a session abort
feature_selection_window <- function(cagr, countries,s){
  # Get numeric value from CAGR name
  y <- suppressMessages(extract_numeric(cagr))
  
  # Test set is has a maximum length based on CAGR years
  leakage_end_date <- max(as.Date(max_data_date) - months(187), 
                          as.Date(max_data_date) -  years(10) -
                            years(y)) + months(s)
  
  leakage_start_date <- leakage_end_date - years(y)
  
  to_model <- to_model_exploration %>% 
    filter(country %in% countries) %>% # TODO
    select(date, country, !!cagr, !!predictors) %>% 
    # group_by(country) %>% # FIXME
    # mutate(lag_cagr = lag(cagr_5_year, 1)) %>%  #FIXME
    na.omit()
  
  # FIXME training set according to CAGR years
  # Split into different sets
  training <- to_model %>% 
    filter(date < yearmonth(leakage_start_date))
  
  leakage_set <- to_model %>% 
    filter(date >= yearmonth(leakage_start_date),
           date < yearmonth(leakage_end_date))
  
  test <- to_model %>% 
    filter(date == yearmonth(leakage_end_date))
  
  # Formulas for mean, naive and ARIMA
  # train 
  
  
  if(nrow(test) != 0){
    
    
    models_ts <- map(formulas_fs, ~training %>% 
                       model(ARIMA = ARIMA(as.formula(.x)),
                             VAR = VAR(as.formula(.x))))
    
    
    
    
    # Train mean and naive models
    models_naive_mean <- paste0("models_mean_cagr_",
                                y,
                                " <- training %>% model(MEAN = MEAN(",
                                cagrs[y],
                                "), NAIVE = NAIVE(", 
                                cagrs[y],
                                "))") %>% 
      parse(text = .) %>% 
      eval()
    
    
    
    fcast_no_leakage <- map(1:length(formulas_fs), ~models_ts[[.x]] %>% 
                              forecast(test) %>% 
                              filter(date == yearmonth(leakage_end_date)) %>% 
                              bind_rows(models_naive_mean %>% 
                                          forecast(test) %>% 
                                          filter(date == yearmonth(leakage_end_date))))
    
    acc_no_leakage_training <- map(1:length(formulas_fs), ~fcast_no_leakage[[.x]] %>%
                                     accuracy(bind_rows(training, test)) %>%
                                     select(.model, country, .type, RMSE, MAE, MAPE) %>%
                                     mutate(.formula = paste(formulas_fs_simple[.x]),
                                            .predictors = (str_count(.formula, pattern = "\\+") + 1)) %>%                                    filter(!is.na(MAE))) %>%
      reduce(full_join)
    
    
    # # FIXME
    # # TODO
    # list(training %>% mutate(set = "training"),
    #      test %>% mutate(set = "test"),
    #      fcast_no_leakage %>% mutate(set = "fcast"))
    
    # TRAINING VALIDATION -----
    
    # fcast_no_leakage_training <- map(1:length(formulas_fs), ~models_ts[[.x]] %>% 
    #                                    forecast(training) %>% 
    #                                    filter(date <= yearmonth(leakage_end_date)) %>%
    #                                    mutate(.formula = paste(formulas_fs_simple[.x])) %>% 
    #                                    
    #                                    bind_rows(models_naive_mean %>%
    #                                                forecast(training) %>% 
    #                                                filter(date <= yearmonth(leakage_end_date))))
    # 
    # 
    # # Calculate leakage-free accuracies
    # acc_no_leakage_training <- map(1:length(formulas_fs), ~fcast_no_leakage_training[[.x]] %>% 
    #                                  accuracy(training) %>% 
    #                                  select(.model, country, .type, RMSE, MAE, MAPE) %>% 
    #                                  mutate(.formula = paste(formulas_fs_simple[.x]), 
    #                                         .predictors = (str_count(.formula, pattern = "\\+") + 1),
    #                                         .type = "Training") %>%
    #                                  filter(!is.na(MAE))) %>% 
    #   reduce(full_join)
    
    # TEST VALIDATION -----
    
    
    # Make forecasts and remove leakage
    # in order to validate accuracies, one needs already the winners 
    
    # fcast_no_leakage_test <- map(1:length(formulas_fs), ~models_ts[[.x]] %>%
    #                                forecast(test) %>%
    #                                filter(date > yearmonth(leakage_end_date)) %>%
    #                                mutate(.formula = paste(formulas_fs_simple[.x])) %>%
    #                                
    #                                bind_rows(models_naive_mean %>%
    #                                            forecast(test) %>%
    #                                            filter(date > yearmonth(leakage_end_date))))
    # 
    # 
    # # Calculate leakage-free accuracies
    # acc_no_leakage_test <- map(1:length(formulas_fs), ~fcast_no_leakage_test[[.x]] %>%
    #                              accuracy(bind_rows(training, test)) %>%
    #                              select(.model, country, .type, RMSE, MAE, MAPE) %>%
    #                              mutate(.formula = paste(formulas_fs_simple[.x]),
    #                                     .predictors = (str_count(.formula, pattern = "\\+") + 1)) %>%
    #                              filter(!is.na(MAE))) %>%
    #   reduce(full_join) %>%
    #   bind_rows(acc_no_leakage_training)
  }
}

# train_test_fcast <- future_map(0:120,~feature_selection_window(cagrs[5],
#                                                                countries,
#                                                                .x))



# RANDOM-WALK ESTIMATED PRICES

# Calculate random-walk based price
rw <- prices_local_long %>% model(rw = RW(price ~ drift()))
prices_local_long <- prices_local_long %>% left_join(rw %>% fitted() %>% 
                                                       select(.fitted)) %>% 
  select(-.model)

rw_b <- prices_local_long %>% model(rw = RW(price ~ drift()))
prices_local_long <- prices_local_long %>% left_join(rw_b %>% fitted() %>% 
                                                       select(.fitted)) %>% 
  select(-.model)

prices_local_long <- prices_local_long %>% 
  mutate(diff_corr_price = difference(.fitted))
# 

prices_local_long %>%
  select(price, .fitted, country) %>% 
  filter(date > as.Date("1990-01-01"),
         country %in% countries) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = price), color = "red") +
  geom_line(aes(y = .fitted), color = "navy") +
  facet_wrap(~country)

to_model %>% filter(country == "AUSTRALIA") %>% 
  select(date, cagr_5_year, cape, dividend_yield, rate_10_year) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = cagr_5_year), col = "green") + 
  geom_line(aes(y = cape), col = "blue") + 
  geom_line(aes(y = rate_10_year), col = "black") + 
  geom_line(aes(y = dividend_yield), col = "orange") 


# MODEL FITTING FOR DIFFERENT CAGR COMBINATIONS (Mid-October)

full_training <- training %>% 
  full_join(prices_local_long) %>% 
  filter(date < as.Date(split_date)) %>% 
  na.omit()

full_test <- test %>% 
  full_join(prices_local_long) %>% 
  filter(date >= as.Date(split_date)) %>% 
  na.omit

country_training <- 
  full_training %>% 
  filter(country == "USA") %>% 
  na.omit()


train_cagr <- function(data, cc, cagr) {
  mod <- model(data %>% filter(country == cc),
               ARIMA = ARIMA(as.formula(paste(cagr, "~",
                                              formulas %>% 
                                                filter(country == cc) %>% 
                                                select(formula) %>% 
                                                as.character))))
  fcasts <- mod %>%
    forecast(full_test)
}

train_var_cagr <- function(data, cc, cagr) {
  mod <- model(data %>% filter(country == cc),
               VAR = VAR(as.formula(paste(cagr, "~",
                                          formulas %>% 
                                            filter(country == cc) %>% 
                                            select(formula) %>% 
                                            as.character))))
}

train_mean_cagr <- function(data, cc, cagr) {
  mod <- model(data %>% filter(country == cc),
               MEAN = MEAN(cagr))
  fcasts <- mod %>%
    forecast(full_test %>% filter(country == cc))
  acc <- fcasts %>% accuracy(bind_rows(full_training %>% filter(country == cc),
                                       full_test %>%  filter(country == cc))) %>% 
    select(.model, country, .type, RMSE, MAE, MAPE) %>% 
    filter(!is.na(MAE))
}

t3 <- purrr::pmap(expand.grid(unique(full_training$country),
                              colnames(prices_local_long)[4:13]),
                  ~train_cagr(full_training, .x, .y)) %>% 
  reduce(cbind)


t4 <- purrr::pmap(expand.grid(unique(full_training$country),
                              colnames(prices_local_long)[4:13]),
                  ~train_var_cagr(full_training, .x, .y)) %>% 
  reduce(cbind)

t5 <- purrr::pmap(expand.grid(unique(full_training$country),
                              colnames(prices_local_long)[4:13]),
                  ~train_mean_cagr(full_training, .x, .y)) %>% 
  reduce(cbind)

# include forecast pasrt
# include a accuracy part
# get data ready for plotting
# extract MAPE scores
