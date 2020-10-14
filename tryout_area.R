
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
    forecast(full_test)
  acc <- fcasts %>% accuracy(full_test)
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
