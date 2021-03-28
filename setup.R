# Which CAGR to use
selected_cagr <- 5
cagr_name <- paste0("cagr_", selected_cagr, "_year")

# How many years ahead to create CAGRs for sets
lead_years <- 1:10

# Data frames and corresponding predictors to use in mapping
dfs <- c("capes_long","rate_10_year_long",
         "unemployment_long", "dividends_long",
         "s_rate_10_year_long", "cpi_long")
cagrs <- paste0("cagr_", lead_years, "_year")
predictors <- c("cape", "dividend_yield",
                "rate_10_year", "dividend_yield", "unemployment", 
                "s_rate_10_year", "cpi") 

countries_to_predict <- c("AUSTRALIA", "CANADA", "USA", "UK", 
                          "NETHERLANDS", "GERMANY", "SPAIN", "SWITZERLAND")
