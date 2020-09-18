library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

library(fable)
library(tsibble)
library(feasts)

capes_wide <- read_excel("Data/cape.xls")

capes_long <- capes_wide %>% 
  pivot_longer(AUSTRALIA:USA, names_to = "country", values_to = "cape") %>% 
  mutate(cape = ifelse(cape == 0, NA, cape),
         date = yearmonth(date))

prices_local_wide <- read_excel("Data/loc.xls")

prices_local_long <- prices_local_wide %>%
  pivot_longer(AUSTRALIA:ZIMBABWE, names_to = "country", values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date),
         cagr_10_year = (lead(price, 12 * 10) / price)^(1 / 10))

to_model <- capes_long %>% 
  inner_join(prices_local_long) %>% 
  as_tsibble(key = "country", index = "date")

training <- to_model %>% 
  filter(date < yearmonth("1995-01-01"))

test <- to_model %>% 
  filter(date >= yearmonth("2005-01-01"))

models_ts <- training %>% 
  model(ARIMA = ARIMA(cagr_10_year ~ cape))

# Training set accuracy
models_ts %>% accuracy()

# Remove leakage
fcast_no_leakage <- fcast %>% 
  filter(date > yearmonth("2005-01-01"))


acc_no_leakage <- fcast_no_leakage %>% accuracy(bind_rows(training, test)) %>% 
  select(.model, country, .type, RMSE, MAE, MASE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE) %>% 
  print(n = 100)
