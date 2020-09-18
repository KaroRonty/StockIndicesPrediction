library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)

test %>% 
  pivot_wider(id = date, names_from = country, values_from = cagr_10_year) %>%
  View()

# Sheets where country data starts and ends
start_c <- 11
end_c <- 52

fundamental <- sapply(excel_sheets("Data/all_data.xlsx")[start_c:end_c],
                      function(x) read_xlsx("Data/all_data.xlsx",
                                            sheet = x,
                                            skip = 5,
                                            .name_repair = "unique"))


price <- sapply(excel_sheets("Data/MT1.xlsx"),
                function(x) clean_names(read_xlsx("Data/MT1.xlsx",
                                                  sheet = x,
                                                  skip = 1,
                                                  .name_repair = "unique")))

us <- price$Sheet3 %>%
  select(date:profit_margin_6)

t1 <- us %>% 
  select(date, last_price_2, x10_year_moving_average_p_e_4, price_earnings_ratio_5)

price_lags <- sapply(seq(4, 80, 4),
                     function(x) (lag(t1$last_price_2, x - 1) /
                                    t1$last_price_2)^(1 / (x / 4)))

t2 <- tibble(Correlation = sapply(1:ncol(price_lags), 
                                  function(x) cor(price_lags[, x], 
                                                  t1$price_earnings_ratio_5, 
                                                  use = "pairwise.complete.obs")),
             n = 1:ncol(price_lags))

ggplot(t2, aes(n, Correlation)) +
  geom_line(aes(color = Correlation), size = 2) +
  ggtitle("Correlation between the P/E ratio and CAGR returns of n years",
          subtitle = "For the S&P 500 index, quarterly data from 1954-03-31 until 2019-09-30") +
  scale_y_continuous(trans = "reverse", limits = c(0, -1), labels = scales::percent) +
  scale_x_continuous(breaks = 1:ncol(price_lags), minor_breaks = 1:ncol(price_lags)) +
  theme_minimal()



macro <- sapply(excel_sheets("Data/macro_clean.xlsx"),
                function(x) print(
                  paste(x, colnames(read_xlsx("Data/macro_clean.xlsx", sheet = x)))))

prices <- ""
