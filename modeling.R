library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(fable)
library(readxl)
library(ggplot2)
library(tsibble)
library(lubridate)
library(ggbeeswarm)
library(ggcorrplot)

# How many years ahead to calculate CAGRs for
lead_years <- 1:10

# Define date to do the training/test set split
split_date <- "2005-01-01" # FIXME forecast plot to dynamic

# For plotting
fcast_start_date <- as.Date("1980-01-01")

# Choose model to compare forecasts with
model_to_compare <- "NAIVE"

# Data frames and corresponding predictors to use in mapping
dfs <- c("capes_long", "prices_local_long", "rate_10_year_long", "unemployment_long", "dividends_long")
cagrs <- paste0("cagr_", lead_years, "_year")
predictors <- c("cape", "cagr_10_year", "rate_10_year", "unemployment", "dividend_yield")

# Prices ------------------------------------------------------------------
prices_local_wide <- read_excel("Data/loc2.xlsx")

# Pivot into long format and replace missing values with NAs
prices_local_long <- prices_local_wide %>%
  pivot_longer(-date,
               names_to = "country",
               values_to = "price") %>% 
  group_by(country) %>% 
  mutate(price = ifelse(price == 0, NA, price),
         date = yearmonth(date)) %>% 
  as_tsibble(index = date, key = country)

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

# Unemployment -------------------------------------------------------------------
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

# CPI -------------------------------------------------------------------
cpi_wide <- read_excel("Data/macro_m.xlsx", sheet = "cpi") # cpi

cpi_long <- cpi_wide %>% 
  pivot_longer(-date, 
               names_to = "country", 
               values_to = "cpi") %>% 
  mutate(date = yearmonth(date))

# Exchange Rates -------------------------------------------------------------------
ex_wide <- read_excel("Data/macro_m.xlsx", sheet = "exr") # exchange rates

ex_long <- ex_wide %>% 
  pivot_longer(-date, 
               names_to = "country", 
               values_to = "exchange_rate") %>% 
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
  select(date, country, !!cagrs, !!predictors) %>% 
  as_tsibble(key = "country", index = "date")

# Get maximum date from the data
max_data_date <- to_model_exploration %>% 
  pull(date) %>% 
  max()

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
  
  acc_no_leakage %>% 
    pivot_wider(id = country, names_from = .model, values_from = MAPE) %>% 
    mutate(source = !!cagr) %>% 
    select(country, source, ARIMA, MEAN, NAIVE) %>% 
    arrange(ARIMA) # FIXME
}

plan(multisession)

countries <- c("CANADA", "USA", "UK", "NETHERLANDS", "GERMANY", "AUSTRALIA", "SPAIN")
features_formula <-  "~ cape + rate_10_year + dividend_yield"
# 28 sec
all_cagr_accuracies <- future_map(cagrs,
                                  ~output_models(.x, countries),
                                  .progress = TRUE) %>% 
  reduce(full_join)

# Plot mean accuracies of different models
all_cagr_accuracies %>% 
  group_by(source) %>% 
  pivot_longer(-country:-source) %>% 
  rename(Country = country,
         Model = name) %>% 
  group_by(Model) %>% 
  mutate(horizon = extract_numeric(source)) %>% 
  group_by(Model, horizon) %>% 
  mutate(mean = mean(value)) %>% 
  ggplot(aes(horizon, value, color = Model, group = Model)) +
  geom_beeswarm(aes(color = Model, group = Model), alpha = 0.15) +
  geom_line(aes(y = mean, color = Model, group = Model)) +
  coord_cartesian(ylim = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 30, 2.5), labels = seq(0, 30, 2.5)) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  scale_colour_manual(name = "Models",
                      values = c("ARIMA" = "#00BFC4",
                                 "MEAN" = "Red",
                                 "NAIVE" = "Black")) +
  ggtitle("Forecast period vs accuracy for all countries with data",
          subtitle = paste("Different test set periods for different CAGR",
                           "forecast periods")) +
  labs(caption = features_formula) +
  xlab("CAGR forecast period (years ahead)") +
  ylab("Average MAPE") +
  theme_minimal() +
  theme(legend.position = "bottom")

# FEATURE SELECTION FOR CURRENT VARIABLES ----

# list all predictor & formula combinations
predictor_lm <- c("cape", "dividend_yield", "rate_10_year", "unemployment")


create_formula <- function(pr) {
  
  
  # do.call for combination generation to prevent map_chr issues
  combinations <- do.call("c", map(seq_along(pr), 
                                   ~combn(pr, .x, FUN = list)))
  
  # define rhs of combinations and apply regex-cleaning
  rhs <- map_chr(seq_along(combinations), ~ paste(gsub("[,]", " + ", 
                                                       gsub("[^A-Za-z0-9,;._-]","", 
                                                            gsub("[\\c]\\(", "", combinations[.]))), collapse = " + "))
  # define cagr goal for lhs
  lhs <- as.character(cagrs[5])
  
  # paste lhs and rhs together
  formulas_fs <- map(paste(lhs, rhs, sep = " ~ "), as.character) %>% 
    flatten_chr() 
  
}

formulas_fs <- create_formula(predictor_lm)

# create short formula for readability
predictor_lm_simple <- substr(predictor_lm, start = 1, stop = 3)
formulas_fs_simple <- create_formula(predictor_lm_simple)

# recycled function from above adjusted for multiple feature combination runs

feature_selection <- function(cagr, countries){
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
                     model(ARIMA = ARIMA(as.formula(.x))))
  
  
  # Formulas for VAR model training
  var_f_1 <- as.formula(paste(cagr, "~ cape")) 
  var_f_2 <- as.formula(paste(cagr, "~ cape + rate_10_year"))
  var_f_3 <- as.formula(paste(cagr, "~ cape + dividend_yield"))
  var_f_4 <- as.formula(paste(cagr, "~ cape + rate_10_year + dividend_yield"))
  # fvar_4 <- as.formula("cagr_10_year ~ cape + rate_10_year + dividend_yield + market_value")
  
  # Train different time series models
  
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
  # in order to validate accuracies, one needs already the winners 
  fcast_no_leakage <- map(1:length(formulas_fs), ~models_ts[[.x]] %>% 
                            forecast(test) %>% 
                            filter(date > yearmonth(leakage_end_date)) %>%
                            mutate(.formula = paste(formulas_fs_simple[.x])) %>% 
                            bind_rows(models_naive_mean %>%
                                        forecast(test) %>% 
                                        filter(date > yearmonth(leakage_end_date))))
  
  # Calculate leakage-free accuracies
  acc_no_leakage <- map(1:length(formulas_fs), ~fcast_no_leakage[[.x]] %>% 
                          accuracy(bind_rows(training, test)) %>% 
                          select(.model, country, .type, RMSE, MAE, MAPE) %>% 
                          mutate(.formula = paste(formulas_fs_simple[.x]),
                                 .predictors = (str_count(.formula, pattern = "\\+") + 1)) %>% 
                          filter(!is.na(MAE))) %>% 
    reduce(full_join) 
  
  
}

# parallel
plan(multisession)
countries <- c("CANADA", "USA", "UK", "NETHERLANDS", "GERMANY", "AUSTRALIA", "SPAIN")

all_feature_accuracies <- future_map(cagrs[5], # fixed value
                                  ~feature_selection(.x, countries),
                                  .progress = TRUE) %>% 
  reduce(full_join)

# check best models per country
all_feature_accuracies %>% 
  group_by(country) %>% 
  arrange(country, MAPE) %>% 
  slice_min(1)

# check for complicated models
all_feature_accuracies %>% 
  filter(.model == "ARIMA",
         .predictors > 3) 

# FEATURE SELECTION on test set -----

# MAPE per country and per predictor combination
all_feature_accuracies %>% 
  arrange(country, .model, MAPE) %>% 
  filter(.model == "ARIMA") %>% 
  select(country, MAPE, .formula) %>% 
  ggplot(aes(x = .formula, y = MAPE, group = 1)) +
  geom_point() +
  geom_line() +
  facet_wrap(~country) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

# mean MAPE per predictor combination (ordered by # of predictors)
all_feature_accuracies %>% 
  filter(.model == "ARIMA") %>% 
  arrange(.predictors, .formula) %>% 
  mutate(.formula = factor(.formula, levels = formulas_fs_simple)) %>% 
  select(country, MAPE, .formula, .predictors) %>% 
  group_by(.formula) %>% 
  mutate(mean_MAPE = mean(MAPE)) %>% 
  ggplot() +
  geom_line(aes(x = .formula, y = mean_MAPE, group = 1), lwd = 0.5, col = "darkorange") +
  geom_point(aes(x = .formula, y = MAPE, col = country), size = 1) + 
  scale_color_brewer(palette="PuBu") +
  labs(title = "Ordered MAPE scores, validated on test set",
       subtitle = "High fluctuation between number of predictors utilized, no clear trend visible",
       x = "Formula",
       y = "Avrg. MAPE per Formula") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none")

# ----

# FIXME everything below this line

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
             xmin = as.Date(leakage_start_date), xmax = as.Date(leakage_end_date),
             ymin = -Inf, ymax = Inf) +
    geom_hline(yintercept = 1) +
    scale_x_yearmonth(labels = year(seq.Date(fcast_start_date,
                                             as.Date(max(test$date)),
                                             by = "5 years")),
                      breaks = yearmonth(seq.Date(fcast_start_date,
                                                  as.Date(max(test$date)),
                                                  by = "5 years"))) +
    facet_wrap(~ country) + 
    geom_vline(xintercept = as.Date(leakage_start_date),
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
# FIXME other models are missing and have to be manually added
naive_or_mean_acc <- acc_no_leakage %>% 
  filter(.model == model_to_compare) %>% 
  mutate(MAE_naive = MAE,
         MAPE_naive = MAPE) %>% 
  select(country, MAE_naive, MAPE_naive)

# Print mean accuracies of different models
acc_no_leakage %>% 
  full_join(naive_or_mean_acc) %>% 
  mutate(MAE_diff = MAE - MAE_naive,
         MAPE_diff = MAPE - MAPE_naive,
         MAPE_div = MAPE_naive / MAPE) %>% 
  group_by(.model) %>% 
  summarise_at(vars(RMSE, MAE, MAPE), mean) %>% 
  arrange(MAPE)

# Print accuracies with differences to naive or mean forecast
acc_no_leakage %>% 
  full_join(naive_or_mean_acc) %>% 
  mutate(MAE_diff = MAE - MAE_naive,
         MAPE_diff = MAPE - MAPE_naive,
         MAPE_div = MAPE_naive / MAPE) %>% 
  filter(.model == "ARIMA") %>% 
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
