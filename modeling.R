library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)

library(fable)
library(tsibble)

# How many years ahead to calculate CAGRs for
lead_years <- 1:10

# Define date to do the training/test set split
split_date <- "1995-01-01"
leakage_end_date <- as.Date(split_date) + years(10)

# For plotting
fcast_start_date <- as.Date("1980-01-01")

# CAPEs -------------------------------------------------------------------
capes_wide <- read_excel("Data/cape.xls")

capes_long <- capes_wide %>% 
  pivot_longer(AUSTRALIA:USA, names_to = "country", values_to = "cape") %>%  # FIXME
  mutate(cape = ifelse(cape == 0, NA, cape),
         date = yearmonth(date))

# Prices ------------------------------------------------------------------
prices_local_wide <- read_excel("Data/loc2.xlsx")

# Function for making lagged columns for CAGR
add_lag_column <- function(df, lag){
  col_name <- paste0("cagr_", lag, "_year")
  
  df %>% 
    mutate(!!col_name := (lead(price, 12 * lag) / price)^(1 / lag))
}

prices_local_long <- prices_local_wide %>%
  pivot_longer(AUSTRALIA:UNITED_ARAB_EMIRATES, # FIXME
               names_to = "country",
               values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date))

# Computed separately due to lack of visibility inside a nested function
prices_local_long <- suppressMessages(map(lead_years,
                         ~add_lag_column(prices_local_long, .x)) %>% 
  reduce(inner_join))

# Macro -------------------------------------------------------------------
unemployment_wide <- read_excel("Data/macro_m.xlsx", sheet = "unr_sa") # unr_na

unemployment_long <- unemployment_wide %>% 
  pivot_longer(-date, names_to = "country", values_to = "unemployment") %>%  # FIXME
  mutate(date = yearmonth(date))

rate_10_year_wide <- read_excel("Data/macro_m.xlsx", sheet = "ltir")

rate_10_year_long <- rate_10_year_wide %>% 
  pivot_longer(-date, names_to = "country", values_to = "rate_10_year") %>%  # FIXME
  mutate(date = yearmonth(date))

# FIXME
to_model <- capes_long %>% 
  inner_join(prices_local_long) %>% 
  # inner_join(unemployment_long) %>%
  inner_join(rate_10_year_long) %>% 
  na.omit() %>% # FIXME
  as_tsibble(key = "country", index = "date")

# Models ------------------------------------------------------------------
training <- to_model %>% 
  filter(date < yearmonth(split_date))

leakage_set <- to_model %>% 
  filter(date >= yearmonth(split_date),
         date < yearmonth(leakage_end_date))

test <- to_model %>% 
  filter(date >= yearmonth(leakage_end_date))

models_ts <- training %>% 
  model(ARIMA = ARIMA(cagr_10_year ~ cape + rate_10_year),
        MEAN = MEAN(cagr_10_year),
        NAIVE = NAIVE(cagr_10_year))#,
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


acc_no_leakage <- fcast_no_leakage %>% 
  accuracy(bind_rows(training, test)) %>% 
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
  autoplot(color = "red", size = 1) +
  autolayer(bind_rows(training, leakage_set, test) %>% 
              filter(country %in% fcast$country), 
            cagr_10_year,
            color = "black",
            size = 1) +
  annotate("rect", fill = "gray", alpha = 0.25, 
           xmin = as.Date(split_date), xmax = as.Date(leakage_end_date),
           ymin = -Inf, ymax = Inf) +
  scale_x_yearmonth(labels = year(seq.Date(fcast_start_date,
                                                as.Date(max(test$date)),
                                                by = "5 years")),
                    breaks = yearmonth(seq.Date(fcast_start_date,
                                                as.Date(max(test$date)),
                                                by = "5 years"))) +
  facet_wrap(~ country) + 
  geom_vline(xintercept = as.Date(split_date),
             color = "gray", linetype = "dashed") +
  geom_vline(xintercept = as.Date(leakage_end_date),
             color = "gray", linetype = "dashed") +
  # scale_x_continuous() + 
  theme_minimal() +
  expand_limits(x = fcast_start_date) +
  theme(legend.position = c(0.95, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(angle = 45))

arima_acc <- fcast_acc %>% 
  filter(.model == "ARIMA")

mean_fcast_acc <- fcast_acc %>% 
  filter(.model == "MEAN")

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
  summarise(MAE = mean(MAE, na.rm = TRUE),
            MAPE = mean(MAPE, na.rm = TRUE)) %>% 
  arrange(MAE)
