library(readxl)
library(tsibble)
library(tidyverse)

source("setup.R")

read_from_extract <- FALSE

if(read_from_extract){
  to_model_exploration <- readRDS("to_model_exploration.RDS")
} else {
  invisible(source("extract_data.R"))
}

source("modeling_setup.R")
source("model_xgboost_pooled.R")
source("functions.R")