library(dplyr)
library(tidyr)
library(fable)
library(readxl)
library(tsibble)
library(ggplot2)
library(lubridate)

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
