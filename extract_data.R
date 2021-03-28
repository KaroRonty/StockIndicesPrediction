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

# Function for reading data from Excel
read_data <- function(filename, extension, column,
                      remove_zero = FALSE, sheet = 1L){
  read_excel(paste0("Data/", filename, ".", extension), sheet = sheet) %>% 
    # Pivot into long format and replace missing values with NAs
    pivot_longer(-date,
                 names_to = "country",
                 values_to = column) %>%
    mutate(!!column := ifelse(remove_zero & get(column) == 0,
                              NA,
                              get(column)),
           date = yearmonth(date))
}

# Features ----------------------------------------------------------------
capes_long <- read_data("cape", "xls", "cape", TRUE, 1)
rate_10_year_long <- read_data("macro_m", "xlsx", "rate_10_year", FALSE, "ltir")
unemployment_long <- read_data("macro_m", "xlsx", "unemployment", FALSE, "unr_sa")
s_rate_10_year_long <- read_data("macro_m", "xlsx", "s_rate_10_year", FALSE, "stir")
cpi_long <- read_data("macro_m", "xlsx", "cpi", FALSE, "cpi")
ex_long <- read_data("macro_m", "xlsx", "exchange_rate", FALSE, "exr")

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

# Create sets
# Join variables
# FIXME
to_model_exploration <- map(dfs, ~get(.x)) %>% 
  reduce(full_join) %>% 
  full_join(prices_local_long) %>% 
  select(date, country, !!cagrs, !!predictors) %>% 
  as_tsibble(key = "country", index = "date")
