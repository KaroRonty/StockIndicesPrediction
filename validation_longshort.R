# financial performance

library(quantmod)
library(PerformanceAnalytics)

# GENERAL TODOS -------


# TODO: across all models
# TODO: include all financial metrics

# TODO: money must be reallocated accordingly to profits made

init <- 100
models <- c("actual", "xgboost_pred", "rf_pred", "elastic_pred", "stack_pred", "ensemble_mean_pred", "mean_prediction")


# REBALANCING ONCE -------


reb_1 <- seq(yearmonth("2005 Aug"), yearmonth("2010 Aug"), by = 60)
ber_1 <- c(yearmonth("2010 Aug") -1 , yearmonth("2020 Aug") -1)

cros_1 <- crossing(reb_1, countries_to_predict, models)
sorc_1 <- crossing(ber_1, countries_to_predict)


# initial predictions for first rebalance period
init_reb_1 <- map(1:nrow(cros_1), ~preds_vs_actuals %>% 
                    filter(date == yearmonth(as.Date(as.integer(cros_1[.x, 1]), origin = "1970-01-01")),
                           country == as.character(cros_1[.x, 2])) %>% 
                    select(date, country, !!as.character(cros_1[.x, 3])) %>%
                    arrange(date) %>% 
                    group_by(date) %>% 
                    pivot_longer(cols = 3, names_to = "model", values_to = "prediction")) %>% 
  reduce(bind_rows) %>% 
  arrange(date)  

# effective crossing only start of period and models
reb_1_cros_models <- crossing(reb_1, models)

# calculate shares based on predictions, across countries and models
reb_1_shares <- map(1:nrow(reb_1_cros_models), ~preds_vs_actuals %>% 
                       arrange(date) %>% 
                       filter(country %in% countries_to_predict, 
                              date == yearmonth(as.Date(as.integer(reb_1_cros_models[.x, 1])))) %>% 
                       select(date, country, actual, !!as.character(reb_1_cros_models[.x, 2])) %>% # FIXME: across all models 
                       group_by(date) %>%
                       mutate(model = as.character(reb_1_cros_models[.x, 2]),
                              order = row_number(!!as.name(as.character(reb_1_cros_models[.x, 2]))),
                              share = case_when(order <= 5 ~ 0, 
                                                order <= 8 ~ 1/3),
                              init_val = init * share) %>% 
                       select(date, country, model, order, share, init_val)) %>%
  reduce(bind_rows) %>% 
  right_join(init_reb_1) %>% 
  arrange(model) %>% 
  as_tsibble(index = date, key = c(country, model)) %>% 
  ungroup()

# start and end time of rebalance period + countries
reb_1_eff_loop <- tibble(reb_1, ber_1) %>% 
  crossing(countries_to_predict, models)

# first rebalancing period: calculate returns 
run_1_reb_1 <- map(1:(length(countries_to_predict) * length(models)), # FIXME no hard 8, but otherwise name conflict in bind_rows because of empty tibbles
                   ~prices_local_long %>% 
                     filter(country == as.character(reb_1_eff_loop[.x, 3]),
                            date >= yearmonth(as.Date(as.integer(reb_1_eff_loop[.x, 1]), origin = "1970-01-01")) & 
                              date <= yearmonth(as.Date(as.integer(reb_1_eff_loop[.x, 2]), origin = "1970-01-01"))) %>% 
                     select(date, country, return_1_month) %>% 
                     arrange(date) %>% 
                     left_join(reb_1_shares %>% filter(model == as.character(reb_1_eff_loop[.x, 4]))) %>% 
                     mutate(return_monthly = init_val * return_1_month,
                            model = as.character(reb_1_eff_loop[.x, 4])) %>% 
                     group_by(country) %>% 
                     mutate(order = row_number(),
                            return_cum = case_when(share == 0 ~ 0, # FIXME
                                                   TRUE ~ Reduce(function(x, y) x * y, .$return_1_month, 
                                                                 init = .$return_monthly[1], accumulate = T) 
                                                   %>% na.omit() 
                                                   %>%  .[-61])) %>%  # FIXME: lengnth of datediff between rebalance windows + 1
                     as_tsibble(index = date, key = c(country, model))) %>% 
  reduce(bind_rows) %>% 
  as_tibble() %>% 
  group_by(country, model) %>% 
  mutate(share = replace(share, is.na(share), mean(share, na.rm = T))) %>% 
  ungroup() %>%
  group_by(date, model) %>%
  mutate(sum_return_cum = sum(return_cum),
         share_return_cum = share * sum_return_cum,
         return_monthly = case_when(order == 60 ~ sum_return_cum * share, # FIXME must be the lead of share_mean
                              T ~ as.numeric(return_monthly)))

# second rebalancing period: calculate returns based on previous cumulative results 
run_2_reb_1 <- map((length(countries_to_predict) * length(models) + 1):nrow(reb_1_eff_loop), 
                   ~run_1_reb_1 %>% 
                     filter(country == as.character(reb_1_eff_loop[.x, 3]),
                            model == as.character(reb_1_eff_loop[.x, 4]),
                            date < yearmonth(as.Date(as.integer(reb_1_eff_loop[.x, 1]), origin = "1970-01-01"))) %>% 
                     bind_rows(prices_local_long %>% 
                                 filter(country == as.character(reb_1_eff_loop[.x, 3]),
                                        date >= yearmonth(as.Date(as.integer(reb_1_eff_loop[.x, 1]), origin = "1970-01-01")) & 
                                          date <= yearmonth(as.Date(as.integer(reb_1_eff_loop[.x, 2]), origin = "1970-01-01"))) %>% # FIXME why not until 2020?
                                 select(date, country, return_1_month) %>% 
                                 arrange(date) %>% 
                                 left_join(reb_1_shares %>% filter(model == as.character(reb_1_eff_loop[.x, 4]) &
                                                                     country == as.character(reb_1_eff_loop[.x, 3]) & 
                                                                     date == yearmonth(as.Date(as.integer(reb_1_eff_loop[.x, 1]))))) %>%
                                 mutate(return_monthly = init_val * return_1_month,
                                        model = as.character(reb_1_eff_loop[.x, 4]))) %>% 
                     ungroup() %>% 
                     mutate(order = row_number(),
                            return_monthly = case_when(order == 61 ~ lag(sum_return_cum) * share * return_1_month,
                                                 TRUE ~ as.numeric(return_monthly)),
                            return_cum = case_when(share == 0 ~ 0, # FIXME
                                                   # FIXME: it should interrupt the sequence and start a new one
                                                   # FIXME: should it pull out all the money or stay in there, if it was invested beforehand
                                                   order >= 61 ~ Reduce(function(x, y) x * y, .$return_1_month, 
                                                                        init = .$return_monthly[61], accumulate = T) %>% 
                                                     na.omit() %>% 
                                                     .[-61],
                                                   T ~ as.numeric(return_cum)))) %>% 
  reduce(bind_rows) %>% 
  as_tibble() %>% 
  group_by(country, model) %>% 
  mutate(share = replace(share, is.na(share), mean(share, na.rm = T))) %>% 
  ungroup() %>%
  group_by(date, model) %>%
  mutate(sum_return_cum = sum(return_cum),
         share_return_cum = share * sum_return_cum,
         return_monthly = case_when(order == 60 ~ sum_return_cum * share, # FIXME must be the lead of share_mean
                                    T ~ as.numeric(return_monthly)))

# graph
run_2_reb_1 %>% 
  ggplot(aes(date, sum_return_cum, colour = model)) +
  geom_line() +
  labs(title = "Rebalancing in 2010 Aug") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))


# NO REBALANCING ----

# TODO: check if we beat the actual?? 

no_reb <- c(yearmonth("2005 Aug"))
no_reb_cros <- crossing(no_reb, countries_to_predict, models)


# across all models 

init_no_reb <- map(1:nrow(no_reb_cros), ~preds_vs_actuals %>% 
                     filter(date == yearmonth(as.Date(as.integer(no_reb_cros[.x, 1]), origin = "1970-01-01")),
                            country == as.character(no_reb_cros[.x, 2])) %>% 
                     select(date, country, !!as.character(no_reb_cros[.x, 3])) %>% 
                     arrange(date) %>% 
                     group_by(date) %>% 
                   pivot_longer(cols = 3, names_to = "model", values_to = "prediction")) %>% # FIXME: dynamic cols
  reduce(bind_rows) %>% 
  arrange(date)

no_reb_cros_models <- crossing(no_reb, models)

no_reb_shares <- map(1:nrow(no_reb_cros_models), ~preds_vs_actuals %>% 
                       arrange(date) %>% 
                       filter(country %in% countries_to_predict, 
                              date == yearmonth(as.Date(as.integer(no_reb_cros_models[.x, 1])))) %>% 
                       select(date, country, actual, !!as.character(no_reb_cros_models[.x, 2])) %>% # FIXME: across all models 
                       group_by(date) %>%
                       mutate(model = as.character(no_reb_cros_models[.x, 2]),
                              order = row_number(!!as.name(as.character(no_reb_cros_models[.x, 2]))),
                              share = case_when(order <= 5 ~ 0, 
                                                order <= 8 ~ 1/3),
                              init_val = init * share) %>% 
                       select(date, country, model, order, share, init_val)) %>%
  reduce(bind_rows) %>% 
  right_join(init_no_reb) %>% 
  arrange(model) %>% 
  as_tsibble(index = date, key = c(country, model))

run_no_reb <- map(1:nrow(no_reb_cros), ~prices_local_long %>% 
               filter(country == as.character(no_reb_cros[.x, 2]),
                      date >= yearmonth(as.Date(as.integer(no_reb_cros[.x, 1]), origin = "1970-01-01"))) %>% 
               select(date, country, return_1_month) %>% 
               arrange(date) %>% 
               left_join(no_reb_shares %>% filter(model == as.character(no_reb_cros[.x, 3]))) %>% 
               mutate(return_monthly = init_val * return_1_month,
                      model = as.character(no_reb_cros[.x, 3])) %>% 
               group_by(country) %>% 
               mutate(order = row_number(),
                      return_cum = case_when(share == 0 ~ 0, # FIXME
                                             TRUE ~ Reduce(function(x, y) x * y, .$return_1_month, 
                                                           init = .$return_monthly[1], accumulate = T) # FIXME, not dynamic
                                             %>% na.omit() 
                                             %>%  .[-182])) %>% 
               as_tsibble(index = date, key = c(country, model))) %>% # FIXME: lengnth of datediff between rebalance windows + 1
  reduce(bind_rows) %>% 
  as_tibble() %>% 
  group_by(country, model) %>% 
  mutate(share = replace(share, is.na(share), mean(share, na.rm = T))) %>% 
  ungroup() %>%
  group_by(date, model) %>%
  mutate(sum_return_cum = sum(return_cum),
         share_return_cum = share * sum_return_cum)

run_no_reb %>% 
  ggplot(aes(date, sum_return_cum, colour = model)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))



# TRIAL ZONE ------

# REBALANCE 3 times ------

reb_3 <- seq(yearmonth("2005 Aug"), yearmonth("2012 Aug"), by = 30)
ber_3 <- reb_3 + 29
# TODO: add rebalancing 

cros_3 <- crossing(reb, countries_to_predict)
sorc_3 <- crossing(ber, countries_to_predict)

tt_3 <- map(1:nrow(cros_3), ~preds_vs_actuals %>% 
              filter(date == yearmonth(as.Date(as.integer(cros_3[.x, 1]), origin = "1970-01-01")),
                     country == as.character(cros_3[.x, 2])) %>% 
              select(date, country, mean_prediction, actual) %>% 
              arrange(date, -mean_prediction) %>% 
              group_by(date)) %>% 
  reduce(bind_rows) %>% 
  arrange(date)

reb_shares_3 <- map(1:length(reb_3), ~preds_vs_actuals %>% 
                      arrange(date) %>% 
                      filter(date == yearmonth(reb_3[.x])) %>% 
                      select(date, country, mean_prediction, actual) %>% 
                      group_by(date) %>% 
                      mutate(order = row_number(mean_prediction),
                             share_mean = case_when(order <= 2 ~ 0, 
                                                    order <= 6 ~ 0.125,
                                                    order <= 8 ~ 0.25),
                             init_val = init * share_mean) %>% 
                      select(date, country, order, share_mean, init_val)) %>%
  reduce(bind_rows) %>% 
  right_join(tt_3) %>% 
  arrange(date) %>% 
  as_tsibble(index = date, key = country)



aaa <- tibble(reb_3, 
              ber_3, 
              period = rep(0:2)) %>% 
  crossing(countries_to_predict)


test1 <- map(1:8, # FIXME no hard 8, but otherwise name conflict in bind_rows because of empty tibbles
             # if initial phase (no rebalancing yet):
             # ~ifelse
             # yearmonth(as.Date(as.integer(aaa[.x, 1]), origin = "1970-01-01")) == yearmonth("2005 Aug"),
             ~prices_local_long %>% 
               filter(country == as.character(aaa[.x, 4]),
                      
                      date >= yearmonth(as.Date(as.integer(aaa[.x, 1]), origin = "1970-01-01")) & 
                        date <= yearmonth(as.Date(as.integer(aaa[.x, 2]), origin = "1970-01-01"))) %>% 
               select(date, country, return_1_month) %>% 
               arrange(date) %>% 
               left_join(reb_shares) %>% 
               mutate(ret_mean = init_val) %>% 
               group_by(country) %>% 
               mutate(order = row_number(),
                      return_cum = case_when(share_mean == 0 ~ 0, # FIXME
                                             TRUE ~ Reduce(function(x, y) x * y, .$return_1_month, 
                                                           init = .$ret_mean[1], accumulate = T) 
                                             %>% na.omit() 
                                             %>%  .[-31]))) %>%  # FIXME: lengnth of datediff between rebalance windows + 1
  # FIXME: can be fixed with dynamic "by"
  # paste0(""))) %>% 
  reduce(bind_rows) %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  mutate(share_mean = replace(share_mean, is.na(share_mean), mean(share_mean, na.rm = T))) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(sum_return_cum = sum(return_cum),
         share_return_cum = share_mean * sum_return_cum,
         ret_mean = case_when(order == 30 ~ sum_return_cum * share_mean, # FIXME must be the lead of share_mean
                              T ~ as.numeric(ret_mean)))

# FIXME: does not work for >2 rebalancing periods
# need to combine all previous periods
test2 <- map(9:nrow(aaa), 
             # ~ifelse(
             # if rebalancing phase ():
             # yearmonth(as.Date(as.integer(aaa[.x, 1]), origin = "1970-01-01")) > yearmonth("2005 Aug"),
             ~test1 %>% 
               filter(country == as.character(aaa[.x, 4]),
                      date < yearmonth(as.Date(as.integer(aaa[.x, 2]), origin = "1970-01-01"))) %>% 
               bind_rows(prices_local_long %>% 
                           filter(country == as.character(aaa[.x, 4]),
                                  date >= yearmonth(as.Date(as.integer(aaa[.x, 1]), origin = "1970-01-01")) & 
                                    date <= yearmonth(as.Date(as.integer(aaa[.x, 2]), origin = "1970-01-01"))) %>% 
                           select(date, country, return_1_month) %>% 
                           arrange(date) %>% 
                           left_join(reb_shares) %>% 
                           mutate(ret_mean = init_val * return_1_month)) %>% 
               ungroup() %>% 
               mutate(order = row_number(),
                      ret_mean = case_when(order == as.integer(aaa[.x, 3] * 30 + 1) ~ lag(sum_return_cum) * share_mean * return_1_month,
                                           TRUE ~ as.numeric(ret_mean)),
                      return_cum = case_when(share_mean == 0 ~ 0, # FIXME
                                             # FIXME: it should interrupt the sequence and start a new one
                                             # FIXME: should it pull out all the money or stay in there, if it was invested beforehand
                                             order >= (as.integer(aaa[.x, 3] * 30 + 1)) ~ Reduce(function(x, y) x * y, .$return_1_month, 
                                                                                                 init = .$ret_mean[as.integer(aaa[.x, 3] * 30 + 1)], accumulate = T) %>% 
                                               na.omit() %>% 
                                               .[-(as.integer(aaa[.x, 3] * 30 + 1))],
                                             T ~ as.numeric(return_cum)))) %>% 
  reduce(bind_rows) %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  mutate(share_mean = replace(share_mean, is.na(share_mean), mean(share_mean, na.rm = T))) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(sum_return_cum = sum(return_cum),
         share_return_cum = share_mean * sum_return_cum)
