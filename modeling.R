library(dplyr)
library(tidyr)
library(purrr)
library(fable)
library(readxl)
library(ggplot2)
library(tsibble)
library(lubridate)

library(corrplot)

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
  pivot_longer(AUSTRALIA:USA,
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

# Market Capitalization 

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
  
cap_availability <- 
  cap_long %>% 
  group_by(country) %>% 
  summarise(non_na_count = sum(!is.na(market_value))/12) %>% 
  arrange(desc(non_na_count))

repl_cap <- c("SWITZERLAND2" = "SWITZERLAND", "JAPAN1" = "JAPAN")

cap_long %>%
  as.data.frame() %>% 
  filter(country != "SWITZERLAND1",
         country != "JAPAN2") %>% 
  mutate(country = recode(country, !!!repl_cap)) %>% 
  as_tibble() -> cap_long

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

# functions for model training
fvar_1 <- as.formula("cagr_10_year ~ cape") 
fvar_2 <- as.formula("cagr_10_year ~ cape + rate_10_year")
fvar_3 <- as.formula("cagr_10_year ~ cape + dividend_yield")
fvar_4 <- as.formula("cagr_10_year ~ cape + rate_10_year + dividend_yield")
# fvar_4 <- as.formula("cagr_10_year ~ cape + rate_10_year + dividend_yield + market_value")



# Train different time series models
models_ts <- training %>% 
  model(ARIMA = ARIMA(cagr_10_year ~ cape + rate_10_year + dividend_yield),
        MEAN = MEAN(cagr_10_year),
        NAIVE = NAIVE(cagr_10_year),#,
#       ETS = ETS(cagr_10_year),
#       RW = RW(cagr_10_year),
#       NAIVE = NAIVE(cagr_10_year),
#       SNAIVE = SNAIVE(cagr_10_year),
#       NNETAR = NNETAR(cagr_10_year),
#       AR = AR(cagr_10_year),
        VAR = VAR(cagr_10_year),
        VAR1 = VAR(fvar_1),
        VAR2 = VAR(fvar_2),
        VAR3 = VAR(fvar_3),
        VAR4 = VAR(fvar_4))
        # VAR4 = VAR(fvar_4))
#       mutate(COMBINATION = (ARIMA + SNAIVE + VAR + NAIVE + RW) / 5,
#       ARIMA_SNAIVE = (ARIMA + SNAIVE) / 2)

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
  select(.model, country, .type, ME, RMSE, MAE, MAPE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE) %>% 
  print(n = 100)

# Plot all sets with forecasts
plot.fcst <- function(model) {
  fcast %>%
    filter(.model == model) %>% 
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

plot.fcst("ARIMA")


# Calculate and compare accuracies of two different model types
arima_acc <- acc_no_leakage %>% 
  filter(.model == "ARIMA")

var_acc <- acc_no_leakage %>% 
  filter(.model == "VAR")

var1_acc <- acc_no_leakage %>% 
  filter(.model == "VAR1")

var2_acc <- acc_no_leakage %>% 
  filter(.model == "VAR2")

var3_acc <- acc_no_leakage %>% 
  filter(.model == "VAR3")

var4_acc <- acc_no_leakage %>% 
  filter(.model == "VAR4")


non_arima_acc <- acc_no_leakage %>% 
  filter(.model == model_to_compare) %>% 
  mutate(MAE_mean = MAE,
         MAPE_mean = MAPE) %>% 
  select(country, MAE_mean, MAPE_mean)

# apply to one central list

acc.calc <- function(model) {
  
  model_acc <- get(model)
  
  model_acc %<>% 
    full_join(non_arima_acc) %>% 
    mutate(MAE_diff = MAE - MAE_mean,
           MAPE_diff = MAPE - MAPE_mean,
           MAPE_div = MAPE_mean / MAPE) %>% 
    arrange(-MAPE_div) %>% 
    print(n = 50)
}

res <- lapply(c("arima_acc", "var_acc", "var1_acc", "var2_acc", "var3_acc", "var4_acc"), acc.calc)
res2 <- do.call(rbind, res)

# Average accuracy of every model
acc_no_leakage %>% 
  group_by(.model) %>% 
  summarise(MAE = mean(MAE, na.rm = TRUE),
            MAPE = mean(MAPE, na.rm = TRUE),
            ME = mean(ME, na.rm = T)) %>% 
  arrange(MAE)

# ------ VAR Models

corrplot(cor(training[, -c(1:2)]), method = "square", order = "hclust")


plot.fcst("VAR")
plot.fcst("VAR1")
plot.fcst("VAR2")
plot.fcst("VAR3")
plot.fcst("VAR4")
# plot.fcst("VAR4")



# ---- XGB

cv <- trainControl(method = "timeslice", # used timeslice!
                   initialWindow = 60, # training observations
                   horizon = 30, # prediction ahead
                   skip = 60 + 30 - 1, # skip to avoid data leakage (Karo tested it - but hoow?)
                   fixedWindow = TRUE, # starts at the next observation
                   allowParallel = TRUE) # parallel backend usage

xgb.training <- function(c) {
  train(training %>% as_tibble() %>% filter(country == c) %>% select(-date, -cagr_10_year, -country) %>% as.matrix(),
                   training %>% filter(country == c) %>%  pull(cagr_10_year), 
                   method = "xgbTree",
                   trControl = cv)
  # difficulty is to find fitting parameters for every country (usually the error arises from too low values for horizon or skip)
  # assign(xgboost, paste0("xgboost_",c))
}

r1 <- lapply(unique(training$country), xgb.training)
do.call(rbind, r1)



xgboost <- train(training %>% as_tibble() %>% filter(country == "USA") %>% select(-date, -cagr_10_year, -country) %>% as.matrix(),
                 training %>% filter(country == "USA") %>%  pull(cagr_10_year), 
                 method = "xgbTree",
                 trControl = cv)

models <- tibble(name = c("xgboost"),
                 model = NA,
                 actual = NA,
                 pred = NA,
                 rsq_cv = NA,
                 mae_cv = NA,
                 date_train = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models)){
  models$model[i] <- get(models$name[i]) %>% list() # get urges to get the whole content 
  models$actual[i] <- training %>% filter(country == "USA") %>% pull(cagr_10_year) %>% list() # pull actual data
  models$pred[i] <- predict(get(models$name[i]), # get predictions based on actual training data
                            training %>%
                              as_tibble() %>% 
                              filter(country == "USA") %>% 
                              select(-date, -cagr_10_year, -country) %>%
                              as.matrix()) %>%
    as.vector() %>% 
    list()
  models$rsq_cv[i] <- get(models$name[i])$resample$Rsquared %>% mean(na.rm = TRUE)
  models$mae_cv[i] <- get(models$name[i])$resample$MAE %>% mean(na.rm = TRUE)
  models$date_train[i] <- training %>% filter(country == "USA") %>% pull(date) %>% list() # pull dates to make understandable again
}

# Make a tibble for storing test set results
models_test <- tibble(name = models$name,
                      actual_test = NA,
                      pred_test = NA,
                      rsq_test = NA,
                      mae_test = NA,
                      pred_future = NA,
                      date_test = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models_test)){
  models_test$actual_test[i] <- test %>% filter(country == "USA") %>%  pull(cagr_10_year) %>% list()
  models_test$pred_test[i] <- predict(get(models_test$name[i]),
                                      test %>% 
                                        as_tibble() %>% 
                                        filter(country == "USA") %>% 
                                        select(-date, -cagr_10_year, -country) %>%
                                        as.matrix()) %>%
    as.vector() %>% 
    list()
  models_test$rsq_test[i] <- cor(models_test$actual_test[[i]],
                                 models_test$pred_test[[i]])^2
  models_test$mae_test[i] <- mean(abs(models_test$pred_test[[i]] -
                                        models_test$actual_test[[i]]))
  models_test$date_test[i] <- test %>% filter(country == "USA") %>% pull(date) %>% list()
}
