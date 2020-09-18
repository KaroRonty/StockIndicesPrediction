library(dplyr)
library(readxl)
library(purrr)
library(xlsx)
library(openxlsx) # easy multiple sheet connectivity
library(tsibble)
library(magrittr)
library(lubridate)

library(rio) # mass important of all xlsx sheets
library(fs)
library(janitor) # name formatting


# EMERING MARKETS MERGINGS

length_ts_dm <- length(ts(start = c(1969,12), end = c(2020,8), frequency = 12))
length_ts_em <- length(ts(start = c(1987,12), end = c(2020,8), frequency = 12))

read.excel <- function(path, name, l) { # files need to be separated by currency to work
  files <- list.files(path = paste0(getwd(),path), pattern = "*.xls", full.names = T)
  dat <- map_dfc(files, read_excel, skip = 6, n_max = l) # include cutoff point
  assign(paste(name), dat, envir = .GlobalEnv)
}



dat <- tsibble(
  mth = yearmonth("1950 Jan") + 0:length(ts(start = c(1950,1), end = c(2020,7), frequency = 12))
)

# 

read.excel("/07_DM/USD", "dm_usd", length_ts_dm)
read.excel("/07_DM/Local", "dm_loc", length_ts_dm)

read.excel("/08_EM/USD", "em_usd", length_ts_em) 
read.excel("/08_EM/USD", "em_loc", length_ts_em)

clean.data <- function(dr, name) {
  dr %>%  mutate(mth = Date...1) %>% # create copy of date column with new name 
    select(-contains("...")) %>% # delete date duplicates
    select(mth, everything()) %>% # change order
    mutate(mth = tsibble::yearmonth(mth)) %>% as_tsibble(index = mth) -> dr # transform date fo tsibble format
  
  names(dr) <- gsub(pattern = "*Standard.*", replacement = "", x = names(dr)) # delete name parts
  dr <- full_join(date, dr, by = c("mth" = "mth")) # join with full-scale date data
  
  assign(paste(name), dr, envir = .GlobalEnv) # create df
  # write.xlsx(paste(name), paste0(name,".xls")) # save as .xls
}

clean.data(dm_usd, "dm_usd")
clean.data(dm_loc, "dm_loc")
clean.data(em_usd, "em_usd")
clean.data(em_loc, "em_loc")

# create one file per currency
loc <- cbind(dm_loc, em_loc, fm_loc) %>%  
  select(-c(which(colnames(.) == "mth"))[2:3]) # delete doubled date columns
write.xlsx(loc, "loc.xls")


usd <- cbind(dm_usd, em_usd, fm_usd) %>% 
  select(-c(which(colnames(.) == "mth"))[2:3])
write.xlsx(usd, "usd.xls")

# CAPE adjustments

cape <- read.csv(paste0(getwd(),"/02_CAPE/cape.csv"), nrow = 465) %>% 
  mutate(mth = as.Date(cape$...date, origin = "1899-12-30")) %>% # create copy of date column with new name
  select(-contains("...")) %>% # delete date duplicates
  select(mth, everything()) %>% # change order
  mutate(mth = tsibble::yearmonth(mth)) %>% as_tsibble(index = mth)  # transform date fo tsibble format

cape <- full_join(date, cape, by = c("mth" = "mth")) # join with full-scale date data
write.xlsx(cape, "cape.xls")

# MACRO VARIABLES



path_ma <- "01_Macro Variables/macro_clean.xlsx"

mak <- path_ma %>% 
  excel_sheets() %>% 
  purrr::set_names() %>% 
  map(read_excel,
      path = path_ma, col_names = T)

# list2env(macro, .GlobalEnv)



# in case we need to important stock prices from DS as well

path_si <- "03_Stock Indices/stock_indices_clean.xlsx"

mad <- path_si %>%
  excel_sheets() %>%
  head(38) %>% 
  purrr::set_names() %>%
  map(read_excel,
      path = path_si, col_names = F)

sti <- sapply(mad, simplify = FALSE, USE.NAMES = TRUE, FUN = function(i) { 
  # USE.NAMES keeps the names of the nested df, simplifies the list structure (instead of simplifying)
  
  dt <- i %>% slice(-c(1:5,7)) %>%
    row_to_names(row_number = 1) %>% 
    clean_names() %>% 
    select(-contains("na")) %>% 
    mutate_if(is.character, as.numeric) %>% 
    mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
    mutate(date = tsibble::yearmonth(date)) %>% 
    as_tsibble(index = date)
  return(dt)
})



# OECD data transformation
macro_m <- list()
macro_q <- list()


# exchange rates
macro_m$exr <- read_excel("01_Macro Variables/macro.xlsx", sheet = "exrates", col_names = F) %>% 
  slice(-1) %>% 
  row_to_names(1) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(date = as.Date(Country, origin = "1900-01-01")) %>% 
  mutate(date = tsibble::yearmonth(date)) %>% 
  select(-Country) %>% 
  select(date, everything()) %>% 
  as_tsibble(index = date)

# cpi

macro_m$cpi <- read.csv("01_Macro Variables/cpi.csv") %>% 
  clean_names() %>% 
  tidyr::pivot_wider(., names_from = i_country, values_from = value) %>% 
  mutate(date = tsibble::yearmonth(time)) %>% 
  select(-time, -time_2) %>% 
  select(date, everything()) %>% 
  as_tsibble(index = date)

# unemployment rate (NS & SA)

f_unr <- c("ST", 
           "STSA")

n_unr <- c("unr_ns", "unr_sa")

unr <- lapply(f_unr, function(i) {
  dt <- read.csv("01_Macro Variables/unr.csv") %>% 
    clean_names() %>% 
    filter(measure == i) %>% 
    tidyr::pivot_wider(., names_from = i_country, values_from = value) %>% 
    mutate(date = tsibble::yearmonth(time)) %>% 
    select(-time, -time_2, -measure) %>% 
    select(date, everything()) %>% 
    as_tsibble(index = date)
  return(dt)
})
names(unr) <- n_unr

# GDP (NS & SA)

f_gdp <- c("CQR", 
           "CQRSA")

n_gdp <- c("gdp_ns", "gdp_sa")

gdp <- lapply(f_gdp, function(i) {
  dt <- read.csv("01_Macro Variables/gdp.csv") %>% 
    clean_names() %>% 
    filter(measure == i) %>% 
    tidyr::pivot_wider(., names_from = i_country, values_from = value) %>% 
    mutate(date = tsibble::yearquarter(time)) %>% 
    select(-time, -period, -measure) %>% 
    select(date, everything()) %>% 
    as_tsibble(index = date)
  return(dt)
})
names(gdp) <- n_gdp

# Indicators

f_sen <- c("Normalised (CLI)", 
           "OECD Standardised BCI, Amplitude adjusted (Long term average=100), sa", 
           "OECD Standardised CCI, Amplitude adjusted (Long term average=100), sa")

n_sen<- c("cli", "bci", "cci")

sen <- lapply(f_sen, function(i) {
  dt <- read.csv("01_Macro Variables/sen.csv") %>% 
    clean_names() %>% 
    filter(i_subject == i) %>% 
    tidyr::pivot_wider(., names_from = country, values_from = value) %>% 
    mutate(date = tsibble::yearmonth(time)) %>% 
    select(-time, -time_2, -i_subject) %>% 
    select(date, everything()) %>% 
    as_tsibble(index = date)
  return(dt)
})

names(sen) <- n_sen


# MFI (SA) # to be simplified

f_mfi <- c("Narrow Money (M1) Index, SA", 
           "Broad Money (M3) Index, SA", 
           "Long-term interest rates, Per cent per annum", 
           "Short-term interest rates, Per cent per annum", 
           "Immediate interest rates, Call Money, Interbank Rate, Per cent per annum")

n_mfi <- c("m1", "m3", "ltir", "stir", "prate")

mfi <- lapply(f_mfi, function(i) {
  dt <- read.csv("01_Macro Variables/mfi.csv") %>% 
    clean_names() %>% 
    filter(i_subject == i) %>% 
    tidyr::pivot_wider(., names_from = country, values_from = value) %>% 
    mutate(date = tsibble::yearmonth(time)) %>% 
    select(-time, -time_2, -i_subject) %>% 
    select(date, everything()) %>% 
    as_tsibble(index = date)
  return(dt)
})

names(mfi) <- n_mfi

macro_m <- do.call(c, list(macro_m, unr, mfi, sen))
macro_q <- do.call(c, list(gdp))


rm(mfi, sen, unr, gdp)


   
# date homologation

dat <- tsibble(
  mth = yearmonth("1950 Jan") + 0:length(ts(start = c(1950,1), end = c(2020,7), frequency = 12))
)

macro_m <- sapply(macro_m, simplify = FALSE, USE.NAMES = TRUE, FUN = function(i) { 
  # USE.NAMES keeps the names of the nested df, simplifies the list structure (instead of simplifying)
  cx <- i
  cx <- full_join(dat, i, by = c("mth" = "date")) %>% # join with full scale data
    filter_index(~ "2020 Aug")
  return(cx)
})

write.xlsx(macro_m, file = "macro_m.xlsx", keepNA = T) # write list of tsibbles to multiple-sheet excel
write.xlsx(macro_q, file = "macro_q.xlsx", keepNA = T) # same for quarterly data


