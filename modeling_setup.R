library(vip)
library(useful)
library(parallel)
library(lubridate)
library(tidymodels)
library(doParallel)

to_model_temp <- to_model_exploration %>% 
  # filter(country == selected_country) %>% # TODO
  select(date, country, !!cagr_name, !!predictors) %>% 
  rename(cagr_n_year := !!cagr_name) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.x), 1000, .x)) %>% 
  filter(date > yearmonth(ymd("1981-01-01")))
  
kept_countries <- to_model_temp %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(not_1000 = mean(cagr_n_year)) %>% 
  filter(not_1000 != 1000) %>% 
  pull(country)

to_model_countries <- to_model_temp %>% 
  filter(country %in% kept_countries)

# TODO only same countries as in training

to_model_mm <- to_model_countries %>% 
  as_tibble() %>% 
  group_by(country, date) %>% 
  mutate(no_data = ifelse(mean(c(cape,
                                 dividend_yield,
                                 rate_10_year,
                                 unemployment,
                                 s_rate_10_year,
                                 cpi)) == 1000 |
                            cagr_n_year == 1000,
                          1,
                          0)) %>% 
  filter(no_data != 1) %>% 
  select(-no_data) %>% 
  arrange(date)

to_model <- build.x(~ .,
                    data = to_model_mm[, -1],
                    contrasts = FALSE) %>% 
  as_tibble() %>% 
  add_column(date = to_model_mm$date)

max_data_date <- to_model %>% 
  pull(date) %>% 
  max()

leakage_end_date <- max(as.Date(max_data_date) - months(187), 
                        as.Date(max_data_date) -  years(10))

leakage_start_date <- leakage_end_date - years(selected_cagr)

split_indices_df <- to_model %>% 
  mutate(.row = row_number(),
         set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                         date >= yearmonth(leakage_start_date) & 
                           date < yearmonth(leakage_end_date) ~ "leakage",
                         date >= yearmonth(leakage_end_date) ~ "test"))

split_indices <- list(analysis = split_indices_df %>% 
                        filter(set == "training") %>% 
                        pull(.row) %>% 
                        as.integer(),
                      assessment = split_indices_df %>% 
                        filter(set == "test") %>% 
                        pull(.row) %>% 
                        as.integer())

model_data <- make_splits(split_indices, 
                          to_model %>% 
                            # select(-date) %>% 
                            mutate(date = as.numeric(date)) %>%
                            as.matrix())
model_training <- training(model_data)
model_test <- testing(model_data)

model_folds <- model_training %>% 
  rolling_origin(initial = 2700,
                 assess = 900,
                 skip = 300, # FIXME 0
                 lag = 1800)

model_recipe <- recipe(cagr_n_year ~ # FIXME
                         countryAUSTRALIA + 
                         countryAUSTRIA + 
                         countryBELGIUM + 
                         countryBRAZIL + 
                         countryCANADA + 
                         countryCHILE + 
                         countryCHINA + 
                         countryCOLOMBIA + 
                         countryCZECH_REPUBLIC + 
                         countryDENMARK + 
                         countryFINLAND + 
                         countryFRANCE + 
                         countryGERMANY + 
                         countryGREECE + 
                         countryHONG_KONG + 
                         countryHUNGARY + 
                         countryINDIA + 
                         countryINDONESIA + 
                         countryIRELAND + 
                         countryISRAEL + 
                         countryITALY + 
                         countryJAPAN + 
                         countryKOREA + 
                         countryMEXICO + 
                         countryNETHERLANDS + 
                         countryNEW_ZEALAND + 
                         countryNORWAY + 
                         countryPOLAND + 
                         countryPORTUGAL + 
                         countryRUSSIA + 
                         countrySINGAPORE + 
                         countrySOUTH_AFRICA + 
                         countrySPAIN + 
                         countrySWEDEN + 
                         countrySWITZERLAND + 
                         countryTAIWAN + 
                         countryTHAILAND + 
                         countryTURKEY + 
                         countryUK + 
                         countryUSA + 
                         cape + 
                         dividend_yield + 
                         rate_10_year + 
                         unemployment +
                         s_rate_10_year +
                         cpi, 
                       data = model_training)

training_temp <- model_training %>% 
  as_tibble()

mean_predictions <- tibble(date = to_model_mm %>% 
                             filter(date < yearmonth(leakage_start_date)) %>% 
                             pull(date), 
                           country = to_model_mm %>% 
                             filter(date < yearmonth(leakage_start_date)) %>% 
                             pull(country),
                           actual = training_temp$cagr_n_year) %>% 
  filter(country %in% countries_to_predict) %>% 
  group_by(country) %>% 
  summarise(mean_prediction = mean(actual, na.rm = TRUE))
