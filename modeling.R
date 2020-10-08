library(dplyr)
library(tidyr)
library(purrr)
library(fable)
library(readxl)
library(ggplot2)
library(tsibble)
library(lubridate)
library(ggcorrplot)

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
dfs <- c("prices_local_long", "capes_long", "rate_10_year_long",
         "unemployment_long", "dividends_long")
predictors <- c("cagr_10_year", "cape", "rate_10_year",
                "unemployment", "dividend_yield")

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

# CAPEs -------------------------------------------------------------------
capes_wide <- read_excel("Data/cape.xls")

# Pivot into long format and replace missing values with NAs
capes_long <- capes_wide %>% 
  pivot_longer(-date,
               names_to = "country",
               values_to = "cape") %>%
  mutate(cape = ifelse(cape == 0, NA, cape),
         date = yearmonth(date))

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

# Dividends ---------------------------------------------------------------
# Get dividend yields from each sheet, rename according to country and combine
dividends_wide <- map(1:32,
                      ~read_excel("Data/sti.xlsx", sheet = .x) %>% 
                        select(date,
                               !!excel_sheets("Data/sti.xlsx")[.x] :=
                                 contains("dividend_yield"))) %>% 
  reduce(full_join)

dividends_long <- dividends_wide %>% 
  pivot_longer(-date, 
               names_to = "country", 
               values_to = "dividend_yield") %>% 
  mutate(date = yearmonth(date))

# Market capitalization ---------------------------------------------------
cap_wide <- map(1:32,
                  ~read_excel("Data/sti.xlsx", sheet = .x) %>% 
                        select(date,
                               !!excel_sheets("Data/sti.xlsx")[.x] :=
                                 contains("market_value"))) %>% 
  reduce(full_join)

cap_long <- cap_wide %>% 
  pivot_longer(-date, 
               names_to = "country", 
               values_to = "market_value") %>% 
  mutate(date = yearmonth(date))
  
# cap_availability <- cap_long %>% 
#   group_by(country) %>% 
#   summarise(non_na_count = sum(!is.na(market_value)) / 12) %>% 
#   arrange(desc(non_na_count))

repl_cap <- c("SWITZERLAND2" = "SWITZERLAND", "JAPAN1" = "JAPAN")

cap_long <- cap_long %>%
  as.data.frame() %>% 
  filter(country != "SWITZERLAND1",
         country != "JAPAN2") %>% 
  mutate(country = recode(country, !!!repl_cap)) %>% 
  as_tibble()

# Models ------------------------------------------------------------------
# Join variables
# FIXME
to_model_exploration <- map(dfs, ~get(.x)) %>% 
  reduce(full_join) %>% 
  select(date, country, !!predictors) %>% 
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

# Functions for VAR model training
fvar_1 <- as.formula("cagr_10_year ~ cape") 
fvar_2 <- as.formula("cagr_10_year ~ cape + rate_10_year")
fvar_3 <- as.formula("cagr_10_year ~ cape + dividend_yield")
fvar_4 <- as.formula("cagr_10_year ~ cape + rate_10_year + dividend_yield")
# fvar_4 <- as.formula("cagr_10_year ~ cape + rate_10_year + dividend_yield + market_value")

# Train different time series models
models_ts <- training %>% 
  model(ARIMA = ARIMA(cagr_10_year ~ cape + rate_10_year + dividend_yield),
        MEAN = MEAN(cagr_10_year),
        NAIVE = NAIVE(cagr_10_year),
        VAR = VAR(cagr_10_year),
        VAR_1 = VAR(fvar_1),
        VAR_2 = VAR(fvar_2),
        VAR_3 = VAR(fvar_3),
        VAR_4 = VAR(fvar_4))

# Make forecasts and remove leakage
fcast <- models_ts %>% 
  forecast(test)

fcast_no_leakage <- fcast %>% 
  filter(date > yearmonth(leakage_end_date))

# Calculate leakage-free accuracies
acc_no_leakage <- fcast_no_leakage %>% 
  accuracy(bind_rows(training, test)) %>% 
  select(.model, country, .type, RMSE, MAE, MAPE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE) %>% 
  print(n = 100)

# Plot all sets with forecasts
plot_forecasts <- function(model) {
  fcast %>%
    filter(.model == model) %>% 
    autoplot(color = "red", size = 1) +
    autolayer(bind_rows(training, leakage_set, test) %>% 
                filter(country %in% fcast$country), 
              cagr_10_year,
              color = "black",
              size = 1) +
    annotate("rect", fill = "gray", alpha = 0.25, 
             xmin = as.Date(split_date), xmax = as.Date(leakage_end_date),
             ymin = -Inf, ymax = Inf) +
    geom_hline(yintercept = 1) +
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
}

plot_forecasts("ARIMA")

# Calculate and compare accuracies of two different model types
naive_or_mean_acc <- acc_no_leakage %>% 
  filter(.model == model_to_compare) %>% 
  mutate(MAE_mean = MAE,
         MAPE_mean = MAPE) %>% 
  select(country, MAE_mean, MAPE_mean)

# Print accuracies with differences to naive or mean forecast
acc_no_leakage %>% 
  full_join(naive_or_mean_acc) %>% 
  mutate(MAE_diff = MAE - MAE_mean,
         MAPE_diff = MAPE - MAPE_mean,
         MAPE_div = MAPE_mean / MAPE) %>% 
  arrange(-MAPE_div) %>% 
  print(n = 200)

# Average accuracy of every model
acc_no_leakage %>% 
  group_by(.model) %>% 
  summarise(MAE = mean(MAE, na.rm = TRUE),
            MAPE = mean(MAPE, na.rm = TRUE)) %>% 
  arrange(MAE)

# VAR correlations
ggcorrplot(training %>% 
             as_tibble() %>% 
             select(-date, -country) %>% 
             cor(),
           hc.order = TRUE,
           lab = TRUE)

# Plot all VAR plots
map(c("VAR", "VAR_1", "VAR_2", "VAR_3", "VAR_4"), ~plot_forecasts(.x))
