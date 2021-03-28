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
