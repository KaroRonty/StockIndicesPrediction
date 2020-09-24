library(dplyr)
library(tidyr)
library(fable)
library(purrr)
library(readxl)
library(tsibble)
library(ggplot2)
library(lubridate)

# Function for getting the first date or amount of years for each country
get_first_date <- function(long_data, predictor, years_or_min){
  long_data <- get(long_data)
  
  long_data %>% 
    rename(predictor = 3) %>% 
    mutate(dates = ifelse(!is.na(predictor), date, NA)) %>% 
    group_by(country) %>% 
    summarise(min = as.Date(min(dates, na.rm = TRUE), origin = "1970-01-01"),
              max = as.Date(max(dates, na.rm = TRUE), origin = "1970-01-01"),
              # Without the leakage set
              years = interval(min, max) / years(1) - 10,
              min_yearmonth = yearmonth(min)) %>% 
    select(country, !!predictor := !!years_or_min)
}

# Data frames and corresponding predictors to get data availability from
dfs <- c("capes_long", "prices_local_long", "rate_10_year_long", "unemployment_long")
predictors <- c("cape", "cagr_10_year", "rate_10_year", "unemployment")

# First date
availability_date <- map2(dfs, predictors, ~get_first_date(.x, .y, "min")) %>% 
  reduce(full_join) %>% 
  mutate_if(is.Date, as.numeric) %>% 
  mutate(mean = rowMeans(across(-country), na.rm = TRUE)) %>% 
  mutate_if(is.numeric, ~as.Date(.x, origin = "1970-01-01")) %>% 
  arrange(mean)

# Amount of years
availability_years <- map2(dfs, predictors, ~get_first_date(.x, .y, "years")) %>% 
  reduce(full_join) %>% 
  mutate(mean = rowMeans(across(-country), na.rm = TRUE)) %>% 
  arrange(-mean)

min_max_dates <- prices_local_long %>% 
  group_by(country) %>% 
  summarise(min_n = first(which(!is.na(cagr_10_year))),
            max_n = last(which(!is.na(cagr_10_year)))) %>% 
  mutate(min_d = prices_local_long$date[min_n],
         max_d = prices_local_long$date[max_n])

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

acc <- fcasts %>% accuracy(test_ts)
