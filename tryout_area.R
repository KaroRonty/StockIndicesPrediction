# FEATURE SELECTION FOR CURRENT VARIABLES
# country --> most complicated model --> down by steps using AICc --> adjust formula (kick out predictor with least added value)
output_models <- function(cagr, countries){
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
  arima_f <- as.formula(paste(cagr, features_formula))
  
  # Formulas for VAR model training
  var_f_1 <- as.formula(paste(cagr, "~ cape")) 
  var_f_2 <- as.formula(paste(cagr, "~ cape + rate_10_year"))
  var_f_3 <- as.formula(paste(cagr, "~ cape + dividend_yield"))
  var_f_4 <- as.formula(paste(cagr, "~ cape + rate_10_year + dividend_yield"))
  # fvar_4 <- as.formula("cagr_10_year ~ cape + rate_10_year + dividend_yield + market_value")
  
  # Train different time series models
  models_ts <- training %>% 
    model(ARIMA = ARIMA(arima_f)) #,
  
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
  
  # Make forecasts and remove leakage
  fcast_no_leakage <- models_ts %>% 
    forecast(test) %>% 
    filter(date > yearmonth(leakage_end_date)) %>% 
    bind_rows(models_naive_mean %>% 
                forecast(test) %>% 
                filter(date > yearmonth(leakage_end_date)))
  
  # Calculate leakage-free accuracies
  acc_no_leakage <- fcast_no_leakage %>% 
    accuracy(bind_rows(training, test)) %>% 
    select(.model, country, .type, RMSE, MAE, MAPE) %>% 
    filter(!is.na(MAE))
  
  # acc_no_leakage %>% 
  #   pivot_wider(id = country, names_from = .model, values_from = MAPE) %>% 
  #   mutate(source = !!cagr) %>% 
  #   select(country, source, ARIMA, MEAN, NAIVE)
}

plan(multisession)
countries <- c("CANADA")
# countries <- c("CANADA", "USA", "UK", "NETHERLANDS", "GERMANY", "AUSTRALIA", "SPAIN")
features_formula <-  "~ cape + rate_10_year + dividend_yield"

all_cagr_accuracies <- future_map(cagrs[5],
                                  ~output_models(.x, countries),
                                  .progress = TRUE) %>% 
  reduce(full_join)


# ------

ad <- do.call("c", lapply(seq_along(xregs),function(i) combn(xregs, i, FUN = list)))


xregs <- c("cape", "rate_10_year", "dividend_yield")
rhs <- map_chr(seq_along(ad), ~ paste(gsub("[,]", " + ", 
                                           gsub("[^A-Za-z0-9,;._-]","", 
                                                gsub("[\\c]\\(", "", ad[.]))), collapse = " + "))
lhs <- as.character(cagrs[5])

formulas2 <- map(paste(lhs, rhs, sep = " ~ "), as.formula)

# MODEL FITTING FOR DIFFERENT CAGR COMBINATIONS

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
