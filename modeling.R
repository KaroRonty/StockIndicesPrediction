library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(lubridate)

library(fable)
library(tsibble)
library(feasts)

# Define date to do the training/test set split
split_date <- "1995-01-01"
leakage_end_date <- as.Date(split_date) + years(10)

# CAPEs -------------------------------------------------------------------
capes_wide <- read_excel("Data/cape.xls")

capes_long <- capes_wide %>% 
  pivot_longer(AUSTRALIA:USA, names_to = "country", values_to = "cape") %>%  # FIXME
  mutate(cape = ifelse(cape == 0, NA, cape),
         date = yearmonth(date))

# Prices ------------------------------------------------------------------
prices_local_wide <- read_excel("Data/loc2.xlsx")

prices_local_long <- prices_local_wide %>%
  pivot_longer(AUSTRALIA:UNITED_ARAB_EMIRATES, # FIXME
               names_to = "country",
               values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date),
         cagr_10_year = (lead(price, 12 * 10) / price)^(1 / 10))

to_model <- capes_long %>% 
  inner_join(prices_local_long) %>% 
  as_tsibble(key = "country", index = "date")


# Macro -------------------------------------------------------------------
unemployment_wide <- read_excel("Data/macro_m.xlsx", sheet = "unr_sa") # unr_na

unemployment_long <- unemployment_wide %>% 
  pivot_longer(-date, names_to = "country", values_to = "unemployment") %>%  # FIXME
  mutate(date = yearmonth(date))

rate_10_year_wide <- read_excel("Data/macro_m.xlsx", sheet = "ltir")

rate_10_year_long <- rate_10_year_wide %>% 
  pivot_longer(-date, names_to = "country", values_to = "rate_10_year") %>%  # FIXME
  mutate(date = yearmonth(date))

to_model <- capes_long %>% 
  inner_join(prices_local_long) %>% 
  # inner_join(unemployment_long) %>%
  inner_join(rate_10_year_long)  %>% 
  na.omit() %>% # FIXME
  as_tsibble(key = "country", index = "date")

# Models ------------------------------------------------------------------
training <- to_model %>% 
  filter(date < yearmonth(split_date))

test <- to_model %>% 
  filter(date >= yearmonth(leakage_end_date))

models_ts <- training %>% 
  model(ARIMA = ARIMA(cagr_10_year ~ cape + rate_10_year),
        MEAN = MEAN(cagr_10_year))#,
  #       ETS = ETS(cagr_10_year),
  #       RW = RW(cagr_10_year),
  #       NAIVE = NAIVE(cagr_10_year),
  #       SNAIVE = SNAIVE(cagr_10_year),
  #       NNETAR = NNETAR(cagr_10_year),
  #       AR = AR(cagr_10_year),
  #       VAR = VAR(cagr_10_year)) %>% 
  # mutate(COMBINATION = (ARIMA + SNAIVE + VAR + NAIVE + RW) / 5,
  #        ARIMA_SNAIVE = (ARIMA + SNAIVE) / 2)

# # Training set accuracy
# models_ts %>% accuracy()
# 
# models_ts %>% print(n = 50)

fcast <- models_ts %>% 
  forecast(test)

# Remove leakage
fcast_no_leakage <- fcast %>% 
  filter(date > yearmonth(leakage_end_date))#,
         # .model == "ARIMA")

# mean_fcast_no_leakage <- fcast %>% 
#   filter(date > yearmonth("2005-01-01"),
#          .model == "MEAN(cagr_10_year)")


acc_no_leakage <- fcast_no_leakage %>% accuracy(bind_rows(training, test)) %>% 
  select(.model, country, .type, RMSE, MAE, MAPE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE) %>% 
  print(n = 100)

# TODO
# Accuracy of the model with additional features
acc_no_leakage %>% 
  filter(.model == "ARIMA") %>% 
  inner_join(acc_no_leakage_arima %>% select(country)) %>% 
  pull(MAE) %>% mean

# Accuracy of the comparable ARIMA models
acc_no_leakage_arima %>% 
  inner_join(acc_no_leakage %>% 
               filter(.model == "ARIMA") %>% 
               select(country)) %>% 
  pull(MAE) %>% 
  mean(na.rm = TRUE)

fcast_acc <- acc_no_leakage %>% arrange(MAE)

fcast %>%
  filter(.model == "ARIMA") %>% 
  # filter(country == "AUSTRALIA") %>% 
  autoplot(level = NULL) +
  autolayer(test, # %>% 
              # filter(country == "AUSTRALIA"), 
            cagr_10_year,
            color = "black") +
  facet_wrap(~ country) + 
  theme_minimal() +
  theme(legend.position = "bottom")

arima_acc <- fcast_acc %>% 
  filter(.model == "ARIMA")

non_arima_acc <- mean_fcast_acc %>% 
  filter(.model == "MEAN") %>% 
  mutate(MAE_mean = MAE,
         MAPE_mean = MAPE) %>% 
  select(country, MAE_mean, MAPE_mean)

arima_acc %>% 
  full_join(non_arima_acc) %>% 
  mutate(MAE_diff = MAE - MAE_mean,
         MAPE_diff = MAPE - MAPE_mean,
         MAPE_div = MAPE_mean / MAPE) %>% 
  arrange(-MAPE_div) %>% 
  print(n = 50) #%>%
   # pull(MAPE_div) %>% mean(na.rm = TRUE)

# Average accuracy of every model
acc_no_leakage %>% 
  group_by(.model) %>% 
  summarise(MAE = mean(MAE, na.rm = TRUE)) %>% 
  arrange(MAE)
