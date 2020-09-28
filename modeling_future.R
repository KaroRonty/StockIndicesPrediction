# source("modeling.R")

scale <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Data frames and corresponding predictors to use in mapping
dfs <- c("capes_long", "growth_local_long", "rate_10_year_long", "unemployment_long")
predictors <- c("cape", "cagr_10_year", "rate_10_year", "unemployment")

# Models ------------------------------------------------------------------
# Join variables
# FIXME
to_model_exploration <- map(dfs, ~get(.x)) %>% 
  reduce(full_join) %>% 
  select(date, country, cagr_10_year, cape, rate_10_year) %>% 
  mutate_if(is.numeric, scale) %>% # FIXME scaling the target?
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
models_ts_future <- to_model %>% 
  model(ARIMA = ARIMA(cagr_10_year ~ cape + rate_10_year),
        MEAN = MEAN(cagr_10_year),
        NAIVE = NAIVE(cagr_10_year))

# Make forecasts and remove leakage
fcast <- models_ts_future %>% 
  forecast(to_model_exploration %>% 
             filter(date > yearmonth("2010-08-01")))

fcast_no_leakage <- fcast %>% 
  filter(date > yearmonth(leakage_end_date))

# Calculate leakage-free accuracies
acc_no_leakage <- fcast_no_leakage %>% 
  accuracy(bind_rows(training, test)) %>% 
  select(.model, country, .type, RMSE, MAE, MAPE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE) %>% 
  print(n = 100)

# Get countries with forecasts
fcast_countries <- fcast %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(sd = sd(.mean, na.rm = TRUE)) %>% 
  filter(sd != 0) %>% 
  pull(country)

# Plot all sets with forecasts
fcast %>%
  filter(.model == "ARIMA",
         country %in% fcast_countries) %>% 
  # filter(country == "AUSTRALIA") %>% 
  autoplot(color = "red", size = 1) +
  autolayer(bind_rows(training, leakage_set, test) %>% 
              filter(country %in% fcast_countries), 
            cagr_10_year,
            color = "black",
            size = 1) +
  scale_x_yearmonth(labels = year(seq.Date(fcast_start_date,
                                           as.Date(max(test$date)),
                                           by = "5 years")),
                    breaks = yearmonth(seq.Date(fcast_start_date,
                                                as.Date(max(test$date)),
                                                by = "5 years"))) +
  facet_wrap(~ country) + 
  ggtitle(dfs[2]) +
  theme_minimal() +
  expand_limits(x = fcast_start_date) +
  theme(legend.position = c(0.95, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(angle = 45))

# Countries with highest forecasts
fcast %>% 
  as_tibble() %>% 
  select(country, date, .mean, cape, rate_10_year) %>% 
  na.omit() %>% 
  group_by(country) %>% 
  slice(n()) %>% 
  arrange(-.mean)

coefs_future <- models_ts_future %>% 
  select(ARIMA) %>% 
  coef() %>% 
  arrange(abs(estimate)) %>% 
  print(n = 100)

coefs_future %>% 
  ggplot(aes(x = estimate, y = country, color = country)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  facet_wrap(~term) +
  ggtitle(dfs[2]) +
  theme_minimal() +
  theme(legend.position = "none")
