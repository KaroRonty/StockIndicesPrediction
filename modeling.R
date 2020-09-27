library(dplyr)
library(tidyr)
library(purrr)
library(fable)
library(readxl)
library(ggplot2)
library(tsibble)
library(lubridate)

# How many years ahead to calculate CAGRs for
lead_years <- 1:10

# Define date to do the training/test set split
split_date <- "1995-01-01"
leakage_end_date <- as.Date(split_date) + years(10)

# For plotting
fcast_start_date <- as.Date("1980-01-01")

# Choose model to compare forecasts with
model_to_compare <- "NAIVE"

# Data frames and corresponding predictors to use in mapping
dfs <- c("capes_long", "prices_local_long", "rate_10_year_long", "unemployment_long")
predictors <- c("cape", "cagr_10_year", "rate_10_year", "unemployment")

# CAPEs -------------------------------------------------------------------
capes_wide <- read_excel("Data/cape.xls")

# Pivot into long format and replace missing values with NAs
capes_long <- capes_wide %>% 
  pivot_longer(AUSTRALIA:USA,
               names_to = "country",
               values_to = "cape") %>%
  mutate(cape = ifelse(cape == 0, NA, cape),
         date = yearmonth(date))

# Prices ------------------------------------------------------------------
prices_local_wide <- read_excel("Data/loc2.xlsx")

# Pivot into long format and replace missing values with NAs
prices_local_long <- prices_local_wide %>%
  pivot_longer(-date,
               names_to = "country",
               values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date))

# Function for making leaded columns for CAGR
add_cagr_columns <- function(df, lead){
  col_name <- paste0("cagr_", lead, "_year")
  
  df %>% 
    mutate(!!col_name := (lead(price, 12 * lead) / price)^(1 / lead))
}

# Computed separately due to lack of visibility inside a nested function
prices_local_long <- suppressMessages(
  map(lead_years,
      ~add_cagr_columns(prices_local_long, .x)) %>% 
    reduce(inner_join))

# Value -------------------------------------------------------------------
value_local_wide <- read_excel("Data/dm_vl.xlsx")

# Pivot into long format and replace missing values with NAs
value_local_long <- value_local_wide %>%
  pivot_longer(-date,
               names_to = "country",
               values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date))

# Function for making leaded columns for CAGR
# Computed separately due to lack of visibility inside a nested function
value_local_long <- suppressMessages(
  map(lead_years,
      ~add_cagr_columns(value_local_long, .x)) %>% 
    reduce(inner_join))


# Growth ------------------------------------------------------------------
growth_local_wide <- read_excel("Data/dm_gr.xlsx")

# Pivot into long format and replace missing values with NAs
growth_local_long <- growth_local_wide %>%
  pivot_longer(-date,
               names_to = "country",
               values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date))

# Function for making leaded columns for CAGR
# Computed separately due to lack of visibility inside a nested function
growth_local_long <- suppressMessages(
  map(lead_years,
      ~add_cagr_columns(growth_local_long, .x)) %>% 
    reduce(inner_join))

# Macro -------------------------------------------------------------------
unemployment_wide <- read_excel("Data/macro_m.xlsx", sheet = "unr_sa") # unr_na

unemployment_long <- unemployment_wide %>% 
  pivot_longer(-date, 
               names_to = "country", 
               values_to = "unemployment") %>% 
  mutate(date = yearmonth(date))

rate_10_year_wide <- read_excel("Data/macro_m.xlsx", sheet = "ltir")

rate_10_year_long <- rate_10_year_wide %>% 
  pivot_longer(-date, 
               names_to = "country", 
               values_to = "rate_10_year") %>% 
  mutate(date = yearmonth(date))

# Models ------------------------------------------------------------------
# Join variables
# FIXME
to_model_exploration <- map(dfs, ~get(.x)) %>% 
  reduce(full_join) %>% 
  select(date, country, cagr_10_year, cape, rate_10_year) %>% 
  as_tsibble(key = "country", index = "date")

to_model <- to_model_exploration %>% 
  na.omit()

# Split into different sets
training <- to_model %>% 
  filter(date < yearmonth(split_date))

leakage_set <- to_model %>% 
  filter(date >= yearmonth(split_date),
         date < yearmonth(leakage_end_date))

test <- to_model %>% 
  filter(date >= yearmonth(leakage_end_date))

# Train different time series models
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

# Make forecasts and remove leakage
fcast <- models_ts %>% 
  forecast(test)

fcast_no_leakage <- fcast %>% 
  filter(date > yearmonth(leakage_end_date))#,
# .model == "ARIMA")

# mean_fcast_no_leakage <- fcast %>% 
#   filter(date > yearmonth("2005-01-01"),
#          .model == "MEAN(cagr_10_year)")

# Calculate leakage-free accuracies
acc_no_leakage <- fcast_no_leakage %>% 
  accuracy(bind_rows(training, test)) %>% 
  select(.model, country, .type, RMSE, MAE, MAPE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE) %>% 
  print(n = 100)

# Plot all sets with forecasts
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
  theme_minimal() +
  expand_limits(x = fcast_start_date) +
  theme(legend.position = c(0.95, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(angle = 45))

# Calculate and compare accuracies of two different model types
arima_acc <- acc_no_leakage %>% 
  filter(.model == "ARIMA")

non_arima_acc <- acc_no_leakage %>% 
  filter(.model == model_to_compare) %>% 
  mutate(MAE_mean = MAE,
         MAPE_mean = MAPE) %>% 
  select(country, MAE_mean, MAPE_mean)

arima_acc %>% 
  full_join(non_arima_acc) %>% 
  mutate(MAE_diff = MAE - MAE_mean,
         MAPE_diff = MAPE - MAPE_mean,
         MAPE_div = MAPE_mean / MAPE) %>% 
  arrange(-MAPE_div) %>% 
  print(n = 50)

# Average accuracy of every model
acc_no_leakage %>% 
  group_by(.model) %>% 
  summarise(MAE = mean(MAE, na.rm = TRUE),
            MAPE = mean(MAPE, na.rm = TRUE)) %>% 
  arrange(MAE)
