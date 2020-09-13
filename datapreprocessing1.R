library(dplyr)
library(readxl)
library(purrr)
library(xlsx)
library(tsibble)
library(magrittr)
library(lubridate)

library(rio) # mass important of all xlsx sheets


# EMERING MARKETS MERGINGS

length_ts_em <- length(ts(start = c(1987,12), end = c(2020,8), frequency = 12))

read.excel <- function(path, name) { # files need to be separated by currency to work
  files <- list.files(path = paste0(getwd(),path), pattern = "*.xls", full.names = T)
  dat <- map_dfc(files, read_excel, skip = 6, n_max = length_ts_em) # include cutoff point
  assign(paste(name), dat, envir = .GlobalEnv)
}

date <- tsibble(
  mth = yearmonth("1950 Jan") + 0:length(ts(start = c(1950,1), end = c(2020,7), frequency = 12))
)

# 

read.excel("/07_DM/USD", "dm_usd")
read.excel("/07_DM/Local", "dm_loc")

read.excel("/08_EM/USD", "em_usd") 
read.excel("/08_EM/USD", "em_loc")

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
  mutate(mth = as.Date(cape$ï..date, origin = "1899-12-30")) %>% # create copy of date column with new name
  select(-contains("...")) %>% # delete date duplicates
  select(mth, everything()) %>% # change order
  mutate(mth = tsibble::yearmonth(mth)) %>% as_tsibble(index = mth)  # transform date fo tsibble format

cape <- full_join(date, cape, by = c("mth" = "mth")) # join with full-scale date data
write.xlsx(cape, "cape.xls")

# MACRO VARIABLES
macro <- import_list("macro_clean.xlsx")
list2env(macro, .GlobalEnv)

# in case we need to important stock prices from DS as well

