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
  
  assign(paste(name), dr, envir = .GlobalEnv) # create df
  # write.xlsx(paste(name), paste0(name,".xls")) # save as .xls
}

clean.data(dm_usd, "dm_usd") 
dm_usd <- full_join(dat, dm_usd, by = c("mth" = "mth"))

clean.data(dm_loc, "dm_loc")
dm_loc <- full_join(dat, dm_loc, by = c("mth" = "mth"))

clean.data(em_usd, "em_usd")
em_usd <- full_join(dat, em_usd, by = c("mth" = "mth"))

clean.data(em_loc, "em_loc")
em_loc <- full_join(dat, em_loc, by = c("mth" = "mth"))


# create one file per currency
loc <- cbind(dm_loc, em_loc) %>%  
  select(-c(which(colnames(.) == "mth"))[2]) # delete doubled date columns
write.xlsx(loc, "loc2.xls")


usd <- cbind(dm_usd, em_usd) %>% 
  select(-c(which(colnames(.) == "mth"))[2])
write.xlsx(usd, "usd2.xls")

# Value Indices
length_ts_vl <- length(ts(start = c(1974,12), end = c(2020,8), frequency = 12))

read.excel <- function(path, name, l) { # files need to be separated by currency to work
  files <- list.files(path = paste0(getwd(),path), pattern = "*.xls", full.names = T)
  dat <- map_dfc(files, read_excel, skip = 6, n_max = l) # include cutoff point
  assign(paste(name), dat, envir = .GlobalEnv)
}

read.excel("/11_Value", "dm_vl", length_ts_vl)

clean.data <- function(dr, name) {
  dr %>%  mutate(mth = Date...1) %>% # create copy of date column with new name 
    select(-contains("...")) %>% # delete date duplicates
    select(mth, everything()) %>% # change order
    mutate(mth = tsibble::yearmonth(mth)) %>% as_tsibble(index = mth) -> dr # transform date fo tsibble format
  
  names(dr) <- gsub(pattern = "* VALUE Standard.*", replacement = "", x = names(dr)) # delete name parts
  
  assign(paste(name), dr, envir = .GlobalEnv) # create df
  # write.xlsx(paste(name), paste0(name,".xls")) # save as .xls
}

clean.data(dm_vl, "dm_vl") 
dm_vl <- full_join(dat, dm_vl, by = c("mth" = "mth"))
write.xlsx(dm_vl, "dm_vl.xlsx")


# Growth Indices
length_ts_gr <- length(ts(start = c(1974,12), end = c(2020,8), frequency = 12))

read.excel <- function(path, name, l) { # files need to be separated by currency to work
  files <- list.files(path = paste0(getwd(),path), pattern = "*.xls", full.names = T)
  dat <- map_dfc(files, read_excel, skip = 6, n_max = l) # include cutoff point
  assign(paste(name), dat, envir = .GlobalEnv)
}

read.excel("/12_Growth", "dm_gr", length_ts_gr)

clean.data <- function(dr, name) {
  dr %>%  mutate(mth = Date...1) %>% # create copy of date column with new name 
    select(-contains("...")) %>% # delete date duplicates
    select(mth, everything()) %>% # change order
    mutate(mth = tsibble::yearmonth(mth)) %>% as_tsibble(index = mth) -> dr # transform date fo tsibble format
  
  names(dr) <- gsub(pattern = "* GROWTH Standard.*", replacement = "", x = names(dr)) # delete name parts
  
  assign(paste(name), dr, envir = .GlobalEnv) # create df
  # write.xlsx(paste(name), paste0(name,".xls")) # save as .xls
}

clean.data(dm_gr, "dm_gr") 
dm_gr <- full_join(dat, dm_gr, by = c("mth" = "mth"))
write.xlsx(dm_gr, "dm_gr.xlsx")


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
  head(32) %>% 
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
    as_tsibble(index = date) %>% 
    slice(-849)
  return(dt)
})


# replace names for stock indices with countries they belong to
ccc <- read_excel(path = path_si, sheet = 33) %>% # load the sheet with names
  clean_names() %>% 
  mutate(country = toupper(country)) %>% 
  mutate(country = str_replace_all(country, "\\s", "_"))
  
a <- names(sapply(sti, names)) %>% as.data.frame() # get names from sti
a <- left_join(a, ccc, by = c("." = "index_name")) # join
names(sti) <- a[,3] # rewrite names to countries

write.xlsx(sti, "Data/sti.xls", keepNA = T)


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


   
# date and zone homologation

dat <- tsibble(
  mth = yearmonth("1950 Jan") + 0:length(ts(start = c(1950,1), end = c(2020,7), frequency = 12)))

zones <- c("G7", "G20", "Euro area (19 countries)", "European Union â€“ 27 countries (from 01/02/2020)", 
           "OECD - Europe", "OECD - Total", "Major Five Asia", "Four Big European", "NAFTA", "OECD + Major Six NME")

macro_m <- sapply(macro_m, simplify = FALSE, USE.NAMES = TRUE, FUN = function(i) { 
  # USE.NAMES keeps the names of the nested df, simplifies the list structure (instead of simplifying)
  cx <- i
  cx <- full_join(dat, i, by = c("mth" = "date")) %>% # join with full scale data
    filter_index(~ "2020 Aug") %>% 
    select(-any_of(zones)) # cleaning unnecessary zones
  return(cx)
})

macro_q <- sapply(macro_q, simplify = FALSE, USE.NAMES = TRUE, FUN = function(i) { 
  # USE.NAMES keeps the names of the nested df, simplifies the list structure (instead of simplifying)
  cx <- i %>% filter_index(~ "2020 Aug") %>% 
    select(-any_of(zones)) # cleaning unnecessary zones
  return(cx)
}) 


write.xlsx(macro_m, file = "macro_m.xlsx", keepNA = T) # write list of tsibbles to multiple-sheet excel
write.xlsx(macro_q, file = "macro_q.xlsx", keepNA = T) # same for quarterly data

# graphs for analysis:

plot_list_q <- list()
plot_list_m <- list()

plot_list_q$gdp_ns <- macro_q$gdp_ns %>% # gdp na
  pivot_longer(Australia:`United Kingdom`)

plot_list_q$gdp_sa <- macro_q$gdp_sa %>% # gdp sa
  pivot_longer(Australia:Turkey)

plot_list_m$cpi <- macro_m$cpi %>% # cpi
  pivot_longer(Denmark:Austria) 

plot_list_m$unr_sa <- macro_m$unr_sa %>% # unr sa
  pivot_longer(Australia:Colombia) 

plot_list_m$unr_ns <- macro_m$unr_ns %>% # und ns
  pivot_longer(Australia:Colombia) 

plot_list_m$m1 <- macro_m$m1 %>% # m1
  pivot_longer(Australia:`Costa Rica`) 

plot_list_m$m3 <- macro_m$m3 %>% # m3
  pivot_longer(Australia:`Costa Rica`) 

plot_list_m$ltir <- macro_m$ltir %>% # long term interest rates / bond yields
  pivot_longer(Australia:Estonia) 

plot_list_m$stir <- macro_m$stir %>% # stir
  pivot_longer(Australia:India) 

plot_list_m$prate <- macro_m$prate %>% # policy rate
  pivot_longer(Australia:`Costa Rica`) 

plot_list_m$cli <- macro_m$cli %>% # cli
  pivot_longer(Australia:Iceland) 

plot_list_m$bci <- macro_m$bci %>% # bci
  pivot_longer(Australia:Israel) 

plot_list_m$cci <- macro_m$cci %>% # cci
  pivot_longer(Australia:Russia) 

plot.to.plots.q <- function(i){
  dt <- i %>% 
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line() +
    # ggtitle(print(i)) +
    scale_y_log10() +
    facet_wrap(~name, scales = "free") +
    theme_minimal() +
    theme(legend.position = "none")
  # return(dt)
}

plot.to.plots.m <- function(i) {
  dt <- i %>% 
    ggplot(aes(x = mth, y = value, color = name)) +
    geom_line() +
    # ggtitle(paste(print(i))) +
    scale_y_log10() +
    facet_wrap(~name, scales = "free") +
    theme_minimal() +
    theme(legend.position = "none")
}

lapply(plot_list_q, plot.to.plots.q)
lapply(plot_list_m, plot.to.plots.m)

