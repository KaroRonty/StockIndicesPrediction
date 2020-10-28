library(visdat)
library(glmnet)
library(tibble)
library(feasts)
library(ggrepel)
library(stringr)
library(patchwork)
library(ggbeeswarm)

# source("modeling.R")

# ACF and PACF plots
acf_p_1 <- models_ts %>% 
  residuals() %>% 
  ACF() %>% 
  autoplot() + 
  ggtitle("ACF of residuals",
          subtitle = "All residuals seem stationary") + 
  labs(caption = features_formula) +
  theme_minimal()

acf_p_2 <- models_ts %>% 
  residuals() %>% 
  PACF() %>% 
  autoplot() + 
  ggtitle("PACF of residuals",
          subtitle = "All residuals seem stationary") + 
  labs(caption = features_formula) +
  theme_minimal()

acf_p_1 + acf_p_2

# Custom scaling function that does not return a matrix
scale <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Data availability -------------------------------------------------------
# Function for getting the first date or amount of years for each country
get_first_date <- function(long_data, predictor, years_or_min){
  long_data <- get(long_data)
  
  # Years according to if using the leaded target or not
  n <- ifelse(grepl(predictor, "cagr"), 0, 10)
  
  long_data %>% 
    rename(predictor = 3) %>% 
    mutate(dates = ifelse(!is.na(predictor), date, NA)) %>% 
    group_by(country) %>% 
    summarise(min = as.Date(min(dates, na.rm = TRUE), origin = "1970-01-01"),
              max = as.Date(max(dates, na.rm = TRUE), origin = "1970-01-01") - years(n),
              # Without the leakage set
              years = interval(min, max) / years(1) - 10,
              min_yearmonth = yearmonth(min)) %>% 
    # Replace negative amount of years with zeros
    mutate(years = ifelse(years < 0, 0, years)) %>% 
    select(country, !!predictor := !!years_or_min)
}

# FIXME
# First date
availability_date <- map2(dfs, c(cagrs[10], predictors), ~get_first_date(.x, .y, "min")) %>% 
  reduce(full_join) %>% 
  mutate_if(is.Date, as.numeric) %>% 
  mutate(mean = rowMeans(across(-country), na.rm = TRUE)) %>% 
  mutate_if(is.numeric, ~as.Date(.x, origin = "1970-01-01")) %>% 
  arrange(mean)

# FIXME
# Amount of years
availability_years <- map2(dfs, c(cagrs[10], predictors), ~get_first_date(.x, .y, "years")) %>% 
  reduce(full_join) %>% 
  mutate(mean = rowMeans(across(-country), na.rm = TRUE)) %>% 
  replace(is.na(.), 0) %>% 
  arrange(-mean)

min_years_model <- 7

# Make formulas for each country based on feature availability
formulas <- availability_years %>% 
  mutate(cape = ifelse(cape >= min_years_model, "cape", NA),
         rate_10_year = ifelse(rate_10_year >= min_years_model, "rate_10_year", NA),
         dividend_yield = ifelse(dividend_yield >= min_years_model, "dividend_yield", NA),
         cpi = ifelse(cpi >= min_years_model, "cpi", NA),
         unemployment = ifelse(unemployment >= min_years_model, "unemployment", NA)) %>% 
  unite(formula, 
        cape, rate_10_year, dividend_yield, cpi, unemployment,
        sep = " + ",
        na.rm = TRUE) %>% 
  mutate(formula = ifelse(cagr_10_year <= min_years_model, "", formula)) %>% 
  select(-mean) %>% 
  filter(formula != "") %>% 
  mutate(n = str_count(formula, "\\+") + 1) %>% 
  arrange(-n)

# How many features for each model
formulas %>% 
  mutate(n = str_count(formula, "\\+") + 1) %>% 
  filter(formula != "") %>% 
  group_by(n) %>% 
  summarise(n = n())

output_availability <- availability_years %>% 
  full_join(formulas) %>% 
  arrange(country) %>% 
  select(-n)

# write.csv(output_availability, "output_availability.csv", row.names = FALSE)
# shell.exec("output_availability.csv")

# Get the feature names from the first model for the plot title
all_features_1st <- models_ts$ARIMA[[1]]$fit$par$term

features_plot_title <- all_features_1st[all_features_1st %in% predictors] %>% 
  paste0(collapse = " + ") %>% 
  c(models_ts$ARIMA[[1]]$response[[1]], .) %>% 
  paste0(collapse = " ~ ")

# Calculate accuracy of just the ARIMA model
arima_acc <- acc_no_leakage %>% 
  filter(.model == "ARIMA")

# Accuracy vs data availability plot
training %>%
  as_tibble() %>%
  group_by(country) %>%
  summarise(duration = (max(date) - min(date)) / 12) %>%
  inner_join(arima_acc) %>%
  arrange(MAE) %>%
  ggplot(aes(x = duration, y = RMSE, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", alpha = 0.2, size = 0, span = 0.5) +
  geom_text_repel(aes(label = country)) +
  scale_x_continuous(breaks = 1:13,
                     labels = 1:13) +
  ggtitle("Accuracy of each model vs years of observations in training set",
          subtitle = features_plot_title) +
  xlab("Years of observations") +
  theme_minimal() +
  theme(legend.position = "none")

# Coefficients and importances --------------------------------------------
# FIXME
to_elastic_model <- to_model_exploration %>% 
  select(-dividend_yield) %>% # FIXME make work with other variables 
  group_by(country) %>% 
  mutate_if(is.numeric, scale) %>% 
  na.omit()

to_elastic_model_training <- to_elastic_model %>% 
  group_by(country) %>% 
  do(train = model.matrix(cagr_10_year ~ cape + unemployment + rate_10_year,
                          data = .)[, -1])

to_elastic_model <- to_elastic_model %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(cagr_10_year = list(cagr_10_year)) %>% 
  inner_join(to_elastic_model_training)

models_elastic <- to_elastic_model %>% 
  group_by(country) %>% 
  do(cv = safely(cv.glmnet)(pluck(.$train, 1),
                            unlist(.$cagr_10_year),
                            alpha = 0.5,
                            nlambda = 30)$result)

models_elastic <- models_elastic %>% 
  # Extract lambda, 9 for min and 10 for 1se
  add_column(as.numeric(as.character(lapply(.$cv, `[[`, 10)))) %>%
  rename(l.1se = 3) %>% 
  inner_join(to_elastic_model, .)

# Make models
models_elastic <- models_elastic %>% 
  group_by(country) %>% 
  do(models = safely(glmnet)(pluck(.$train, 1),
                             unlist(.$cagr_10_year),
                             lambda = .$l.1se,
                             alpha = 0.5,
                             # might be faster when vars>obs
                             type.gaussian = "naive")$result) %>% 
  inner_join(models_elastic, .)

importances_elastic <- map(models_elastic$models,
                           ~ .x %>% 
                             coef() %>% 
                             as.matrix() %>% 
                             as.data.frame() %>% 
                             rownames_to_column()) %>% 
  reduce(bind_cols) %>% 
  select(predictor = rowname...1, contains("s0"))


coefs_to_plot <- importances_elastic %>% 
  pivot_longer(-predictor) %>%  
  group_by(predictor) %>% 
  mutate(mean = mean(value))

p_coef <- ggplot(coefs_to_plot,
                 aes(predictor, value, color = predictor)) +
  geom_quasirandom() +
  geom_point(data = coefs_to_plot %>%
               select(predictor, mean) %>%
               distinct(), aes(predictor, mean),
             shape = "\u2014", size = 20, color = "black", alpha = 0.4) +
  geom_hline(yintercept = 0) +
  ggtitle("Standardized coefficients of country-specific elastic net models",
          subtitle = "Mean coefficients as gray lines") +
  xlab(NULL) +
  ylab("Standardized coefficient") +
  theme_minimal() +
  theme(legend.position = "none")

importances_to_plot <- coefs_to_plot %>% 
  group_by(predictor) %>% 
  mutate(value = abs(value),
         mean = mean(abs(value)))

p_importance <- ggplot(importances_to_plot,
                       aes(predictor, value, color = predictor)) +
  geom_quasirandom() +
  geom_point(data = importances_to_plot %>%
               select(predictor, mean) %>%
               distinct(), aes(predictor, mean),
             shape = "\u2014", size = 20, color = "black", alpha = 0.4) +
  ggtitle("Importances of country-specific elastic net models",
          subtitle = "Mean importances as gray lines") +
  xlab(NULL) +
  ylab("Importance") +
  theme_minimal() +
  theme(legend.position = "none")

p_coef / p_importance

min_max_dates <- prices_local_long %>% 
  group_by(country) %>% 
  summarise(min_n = first(which(!is.na(cagr_10_year))),
            max_n = last(which(!is.na(cagr_10_year)))) %>% 
  mutate(min_d = prices_local_long$date[min_n],
         max_d = prices_local_long$date[max_n])

# Old code with first models and exploration ------------------------------

cagr_wide <- prices_local_wide %>% 
  mutate_if(is.numeric, function(x) (lead(x, 12 * 10) / x)^(1 / 10))

data <- read.csv("Data/wrds_indices.csv")

prices <- read_xlsx("Data/20200907_thesis_data_prices.xlsx",
                    sheet = 2, skip = 1, col_types = "numeric")
names_tickers <- read.csv2("Data/20200907_thesis_data_names.csv",
                           fileEncoding = "UTF-8-BOM")

prices_to_plot <- prices %>%
  mutate(Code = as.Date(Code, origin = "1899-12-30")) %>% 
  pivot_longer(AGSHRPRCF:ZMSHRPRCF) %>% 
  rename(Date = Code, Symbol = name) %>% 
  inner_join(names_tickers %>% select(Name, Market, Symbol)) %>% 
  mutate(value = as.numeric(value))

prices_to_plot %>% 
  ggplot(aes(x = Date, y = value, color = Market)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~Market, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")

data_in_the_90s <- prices_to_plot %>% 
  filter(Date == "1990-01-31") %>% 
  arrange(Market) %>% 
  mutate(has_data = !is.na(value)) %>% 
  select(Market, has_data, Name, Symbol) %>% 
  filter(has_data)

prices_30_years <- prices_to_plot %>% 
  filter(Market %in% data_in_the_90s$Market,
         Market != "Brazil")

# write.csv(data_in_the_90s, "countries_having_data_in_the_90s.csv")

prices_30_years %>% 
  ggplot(aes(x = Date, y = value, color = Market)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~Market, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")

cumulative <- prices_30_years %>% 
  group_by(Market) %>% 
  mutate(index = value / first(na.omit(value)))


cumulative %>% 
  ggplot(aes(x = Date, y = index, color = Market)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~Market, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")

cumulative_wider <- cumulative %>% 
  arrange(Market, Date) %>% 
  pivot_wider(id_cols = Date, names_from = Market)

# Check for missing data
vis_miss(cumulative_wider)

# FIXME impute
cagr <- cumulative %>% 
  mutate(CAGR_10y = (lead(index, 12 * 10) / index)^(1 / 10)) %>% 
  select(-index)

cagr_wider <- cagr %>% 
  pivot_wider(id_cols = Date, names_from = Market, values_from = CAGR_10y)

us <- cagr %>% 
  filter(Market == "United States") %>% 
  mutate(Date = yearmonth(Date))

training <- us %>% 
  filter(as.Date(Date) < quantile(as.numeric(as.Date(Date)), 0.8)) %>% 
  as_tsibble(key = Market, index = Date)

test <- us %>% 
  filter(as.Date(Date) >= quantile(as.numeric(Date), 0.8)) %>% 
  as_tsibble(key = Market, index = Date)

models <- training %>% 
  model(ARIMA = ARIMA(CAGR_10y))

fcasts <- models %>% 
  forecast(test)

acc <- fcasts %>% accuracy(test)
