library(tsbox)
library(kableExtra)
library(PerformanceAnalytics)

n_countries_to_invest_in <- 3
n_rebalances <- c(1, 2, 3)
plot_max_date <- "2020 Aug"

preds_vs_actuals <- preds_vs_actuals %>% 
  filter(country %in% countries_to_predict, 
         country != "SPAIN")

evaluate_financial_performance <- function(n_rebalance){
  
  investing_dates <- seq.Date(as_date(yearmonth("2005 Aug")),
                              as_date(yearmonth("2020 Aug")), 
                              length.out = n_rebalance + 1) %>% 
    yearmonth()
  
  years_in_data <- investing_dates %>% 
    tibble(date = .) %>% 
    filter(date <= yearmonth(plot_max_date)) %>% 
    summarise(years_in_data = (last(date) - first(date)) / 12) %>% 
    pull(years_in_data)
  
  next_month_returns <- prices_local_long %>% 
    as_tibble() %>% 
    select(date, country, return_next_1_month) %>%
    na.omit() %>% 
    filter(country %in% countries_to_predict,
           date >= min(investing_dates),
           date <= yearmonth(plot_max_date)) %>%
    group_by_all() %>% 
    mutate(rebalancing_group = 
             which(date <= yearmonth(investing_dates)[-1])[1]) %>% 
    ungroup()
  
  best_countries_by_model <- preds_vs_actuals %>% 
    na.omit() %>% 
    filter(date %in% investing_dates,
           date <= yearmonth(plot_max_date)) %>% 
    group_by(date) %>% 
    mutate(benchmark = mean(actual)) %>% 
    ungroup() %>% 
    pivot_longer(-c(date, country, actual, benchmark), 
                 names_to = "model",
                 values_to = "prediction") %>% 
    group_by(date, model) %>% 
    arrange(-prediction) %>% 
    slice(1:n_countries_to_invest_in) %>% 
    group_by_all() %>% 
    mutate(rebalancing_group = 
             which(date < yearmonth(investing_dates)[-1])[1]) %>% 
    ungroup()
  
  monthly_returns <- next_month_returns %>% 
    inner_join(best_countries_by_model %>% 
                 select(country, model, rebalancing_group) %>% 
                 distinct()) %>% 
    suppressMessages()
  
  to_sharpe <- monthly_returns %>%
    select(-rebalancing_group) %>%
    # TODO
    # left_join(s_rate_10_year_long) %>%
    full_join(next_month_returns %>%
                group_by(date) %>%
                summarise(return_next_1_month = mean(return_next_1_month)) %>%
                mutate(model = "benchmark")) %>%
    mutate(R = return_next_1_month - 1#,
           # Rf = s_rate_10_year / 100
    ) %>%
    group_by(model, date) %>%
    summarise(R_mean = mean(R)) %>%
    ungroup() %>%
    rename(time = date) %>%
    as_tsibble(key = model) %>%
    suppressMessages()
  
  sharpes <- to_sharpe %>%
    ts_xts() %>%
    SharpeRatio.annualized() %>%
    as_tibble() %>%
    pivot_longer(everything(),
                 names_to = "model",
                 values_to = "Sharpe") %>%
    mutate(rebalance_freq = n_rebalance) %>%
    arrange(-Sharpe) %>% 
    suppressMessages()
  
  equal_weight_monthly_return <- next_month_returns %>%
    group_by(date) %>%
    summarise(equal_weight_return_1_month = mean(return_next_1_month))
  
  equal_weight_monthly_return_cum <- equal_weight_monthly_return %>%
    arrange(date) %>%
    mutate(equal_weight_return_cum = cumprod(equal_weight_return_1_month))
  
  monthly_returns_strategy <- monthly_returns %>%
    group_by(model, date) %>%
    summarise(strategy_return_1_month = mean(return_next_1_month)) %>%
    ungroup() %>%
    suppressMessages()
  
  monthly_returns_strategy_cum <- monthly_returns_strategy %>%
    group_by(model) %>%
    arrange(date) %>%
    mutate(strategy_return_cum = cumprod(strategy_return_1_month))
  
  cumulative_return_comparison <- monthly_returns_strategy_cum %>%
    inner_join(equal_weight_monthly_return_cum) %>%
    select(model, date, strategy_return_cum, equal_weight_return_cum) %>%
    group_by(model) %>%
    filter(date == max(date)) %>%
    select(-date) %>%
    arrange(-strategy_return_cum) %>%
    select(model,
           benchmark = equal_weight_return_cum,
           strategy = strategy_return_cum) %>%
    suppressMessages()
  
  cagr_return_comparison <- cumulative_return_comparison %>%
    mutate_all(~.x^(1 / years_in_data)) %>%
    suppressMessages()
  
  monthly_returns_strategy_cum %>%
    inner_join(cagr_return_comparison) %>%
    # filter(!(model %in% c("mean_prediction", "naive_prediction"))) %>%
    filter(date <= yearmonth(plot_max_date)) %>%
    arrange(date) %>%
    mutate(outperformed = last(strategy) > last(benchmark),
           strategy_return_cum = strategy_return_cum * 100) %>%
    mutate(rebalance_freq = n_rebalance) %>% 
    full_join(sharpes) %>% 
    left_join(equal_weight_monthly_return_cum %>% 
                select(date, benchmark_cum = equal_weight_return_1_month) %>% 
                mutate(benchmark_cum = cumprod(benchmark_cum) * 100)) %>% 
    suppressMessages()
}

performances <- map(n_rebalances,
                    evaluate_financial_performance) %>% 
  bind_rows()



performance_to_plot <- performances %>% 
  mutate(model_and_cagr = model %>% 
           factor(
             levels = performances %>% 
               # FIXME
               filter(date == yearmonth(plot_max_date)) %>% 
               group_by(model) %>% 
               summarise(avg_cum_return = mean(strategy_return_cum)) %>% 
               arrange(-avg_cum_return) %>% 
               pull(model)) %>% 
           fct_relabel(~.x %>% 
                         str_remove("_prediction|_pred") %>%
                         str_replace("_", " ") %>%
                         toupper() %>%
                         str_replace("SINGLE", "single") %>%
                         str_replace("RF", "Random Forest") %>%
                         str_replace("XGBOOST", "XGBoost") %>%
                         str_replace("ELASTIC", "Elastic Net") %>%
                         
                         str_replace("ARIMA single" , "ARIMA") %>% 
                         str_replace("ENSEMBLE MEDIAN", "Median-Stacking") %>%
                         str_replace("XGBoost single", "XGB_s") %>% 
                         str_replace("Random Forest single", "RF_s") %>%
                         str_replace("ENSEMBLE MEAN", "Mean-Stacking") %>% 
                         str_replace("STACK", "Elastic-Net-Stacking") %>%
                         str_replace("Elastic Net", "EN_p") %>% 
                         str_replace("XGBoost", "XGB_p") %>% 
                         str_replace("Random Forest", "RF_p")))
         


sharpes_to_plot <- performance_to_plot %>% 
  ungroup() %>% 
  group_by(model, rebalance_freq) %>% 
  filter(date == max(date)) %>%
  ungroup() %>% 
  select(model,
         Sharpe,
         rebalance_freq) %>% 
  full_join(performance_to_plot %>% 
              filter(model == "benchmark") %>% 
              select(model, rebalance_freq, Sharpe))


sharpes_to_table <- sharpes_to_plot %>% 
  pivot_wider(names_from = rebalance_freq, 
              values_from = Sharpe)


years_per_rebalancing_period <- performances %>%
  na.omit() %>% 
  group_by(rebalance_freq) %>%
  summarise(years = (max(date) - min(date)) / 12)

returns_per_rebalancing <- performances %>% 
  group_by(model, rebalance_freq) %>% 
  filter(date == max(date)) %>% 
  select(model, rebalance_freq, strategy_return_cum) %>% 
  full_join(years_per_rebalancing_period) %>%
  mutate(strategy_cagr = (strategy_return_cum / 100)^(1 / years)) %>% 
  select(-strategy_return_cum, -years) %>% 
  pivot_wider(names_from = rebalance_freq,
              values_from = strategy_cagr) %>% 
  # Join benchmark
  bind_rows(performances %>% 
              group_by(model, rebalance_freq) %>% 
              filter(date == max(date)) %>% 
              ungroup() %>% 
              select(rebalance_freq, benchmark_cum) %>% 
              distinct() %>% 
              full_join(years_per_rebalancing_period) %>% 
              mutate(benchmark_cagr = (benchmark_cum / 100)^(1 / years)) %>% 
              select(-benchmark_cum, -years) %>% 
              pivot_wider(names_from = rebalance_freq,
                          values_from = benchmark_cagr) %>% 
              mutate(model = "benchmark", .before = 1))

to_table <- preds_vs_actuals %>% 
  na.omit() %>% 
  pivot_longer(-c("date", "country", "actual"), 
               names_to = "Model",
               values_to = "pred") %>% 
  group_by(country, Model) %>% 
  summarise(MAPE = mean(abs(((actual) - pred) / actual))) %>% 
  ungroup() %>% 
  group_by(Model) %>% 
  summarise(`Median MAPE` = median(MAPE)) %>% 
  full_join(returns_per_rebalancing %>%
              rename_with(~paste("CAGR", .x, "rebalancing")) %>% 
              rename(Model = 1)) %>% 
  full_join(sharpes_to_table %>% 
              rename_with(~paste("Sharpe", .x, "rebalancing")) %>% 
              rename(Model = 1)) %>% 
  mutate(Model = Model %>%
           str_remove("_prediction|_pred") %>%
           str_replace("_", "") %>%
           toupper() %>% 
           str_replace("SINGLE", "_s") %>% 
           str_replace("RF$", "RF_p") %>% 
           str_replace("ELASTIC", "EN_p") %>% 
           str_replace("XGBOOST$", "XGB_p") %>%
           str_replace("XGBOOST", "XGB") %>%
           str_replace("ENSEMBLEMEDIAN", "Median_ens") %>% 
           str_replace("ENSEMBLEMEAN", "Mean_ens") %>% 
           str_replace("MEAN", "Mean") %>% 
           str_replace("NAIVE",  "Naive") %>% 
           str_replace("STACK", "Stacking") %>% 
           str_replace("BENCHMARK", "Benchmark")) %>% 
             
  mutate(`Median MAPE` = scales::number(`Median MAPE` * 1, 0.001) %>% 
           replace_na("")) %>% 
  mutate_at(vars(starts_with("CAGR")),
            ~scales::percent(.x - 1, 0.001, suffix = "")) %>% 
  mutate_at(vars(starts_with("Sharpe")),
            ~scales::number(.x, 0.001)) %>% 
  slice(1, 2, 7, 8, 10, 11,
        3, 4, 9,
        5, 6, 12) 

# sharpes_per_rebalancing <- performances %>% 
# sharpes_per_rebalancing <- performances %>% 
to_table %>% 
  rename_at(vars(contains("Sharpe")), 
            ~str_replace(.x, "rebalancing", "rebalancing ")) %>% 
  rename_with(~str_remove(.x, "CAGR |Sharpe ")) %>% 
  kable(
    caption = 
      "Financial and non-financial measures by rebalancing period", digits = 3) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  add_header_above(c(" " = 1,
                     "Predictive Perf." = 1,
                     "CAGR" = 3,
                     "Sharpe" = 3)) %>% 
  row_spec(c(10:12), color = "black", background = "#DCDCDC") %>% 
  column_spec(2, border_right = T) %>% 
  column_spec(5, border_right = T) %>% 
  row_spec(
    6,
    extra_css = "border-bottom: 1px solid; border-bottom-color: black") %>%
  row_spec(
    9,
    extra_css = "border-bottom: 1px solid; border-bottom-color: black") %>%
  row_spec(
    11,
    extra_css = "border-bottom: 1px solid; border-bottom-color: black") %>%
  print()

write.xlsx(to_table, file = "03_financial_performance.xlsx",
           sheetName="01", append=TRUE)


p_cum <- performance_to_plot %>%
  filter(!(model_and_cagr %in% c("NAIVE", "MEAN"))) %>% 
  filter(!is.na(model_and_cagr)) %>%
  ggplot(aes(date, 
             strategy_return_cum, 
             color = factor(rebalance_freq), 
             group = factor(rebalance_freq)
             )) +
  geom_line() +
  geom_line(aes(date, benchmark_cum),
            inherit.aes = FALSE,
            data = performance_to_plot %>% 
              ungroup() %>% 
              select(date, benchmark_cum) %>% 
              distinct(), linetype = "dashed") +
  facet_wrap(~model_and_cagr, ncol = 3) + 
  # geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  scale_x_yearmonth(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = seq(50, 300, 50),
                     labels = seq(50, 300, 50)) +
  scale_color_discrete(name = "Rebalance Frequence") +
  # scale_color_discrete(name = "Outperformed benchmark",
  #                      breaks = c(TRUE, FALSE),
  #                      labels = c("Yes", "No")) +
  labs(
    title = 
      "Returns of strategies based on models (colored) vs benchmark (black)",
    # subtitle = paste0("Investing in ",
    #                   n_countries_to_invest_in,
    #                   " out of 8 countries"),
    caption = "Ordered from best to worst average performance by model",
    x = NULL,
    y = "Cumulative Return (over the whole test period)") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_cum)

ggsave("Results_Financial Performance and Rebalancing.png", path = "Plots", 
       width = 10, height = 10, dpi = 300)



# SIGNIFICANCE OF SHARPE ----
# account for significance
# our sharpe is annualized, rebalancing does not affect to number of 


library(SharpeR)

# model and rebalancing variations
variations_fin_results <- performances %>% 
  distinct(model, rebalance_freq) %>% 
  filter(model != "benchmark")

# risk free rate USA
rf_usa <- s_rate_10_year_long %>%
  filter(country == "USA", date > yearmonth("2005 Jul")) %>%
  rename(time = date) %>%
  select(time, s_rate_10_year) %>%
  imputeTS::na_ma(k = 4, weighting = "simple") %>%
  # transfer from annualized to yearly yield
  mutate(s_rate_10_year = (1 + (s_rate_10_year / 100))^(1 / 12) - 1)


sharpe_significance <- function(x) {
  a1 <- as.sr(x = performances %>% 
                filter(model == paste0(variations_fin_results[x,1]), 
                       rebalance_freq == as.integer(variations_fin_results[x,2])) %>%
                rename(time = date) %>% 
                select(time, strategy_return_1_month) %>% 
                mutate(strategy_return_1_month = strategy_return_1_month - 1) %>%
                ts_xts(),
              # c0 = (1 + 0.016)^(1 / 12) - 1,
              c0 = 0, # FIXME
              ope = 12, 
              na.rm = T) %>% 
    suppressMessages()
  
  a2 <- confint(a1, 
          level = 0.95,
          type = "exact")
  
    tibble(model = as.character(variations_fin_results[x,1]),
           rebalancing = as.integer(variations_fin_results[x,2]),
           upper = a2[2],
           bottom = a2[1],
           sr = a1$sr[1])
  
}

sharpe_full <- 
  map(1:nrow(variations_fin_results), ~sharpe_significance(.x)) %>% 
  reduce(bind_rows) 
  
write.xlsx(sharpe_full, file = "05_new_sharpes.xlsx",
             sheetName="01", append=TRUE)


# SHARPE RF BASED RESULTS -----

sharpe_significance_rf <- function(x) {
  a1 <- as.sr(x = performances %>% 
                filter(model == paste0(variations_fin_results[x,1]), 
                       rebalance_freq == as.integer(variations_fin_results[x,2])) %>%
                rename(time = date) %>% 
                select(time, strategy_return_1_month) %>% 
                mutate(strategy_return_1_month = strategy_return_1_month - 1) %>%
                # add risk free rate
                left_join(rf_usa) %>% 
                mutate(strategy_return_1_month  = strategy_return_1_month  - s_rate_10_year) %>% 
                select(-model, -s_rate_10_year) %>% 
                ts_xts(),
                c0 = 0, # added in x 
              ope = 12, 
              na.rm = T) %>% 
    suppressMessages()
  
  a2 <- confint(a1, 
                level = 0.95,
                type = "exact")
  
  tibble(model = as.character(variations_fin_results[x,1]),
         rebalancing = as.integer(variations_fin_results[x,2]),
         upper = a2[2],
         bottom = a2[1],
         sr = a1$sr[1])
  
}

sharpe_full_rf <- 
  map(1:nrow(variations_fin_results), ~sharpe_significance_rf(.x)) %>% 
  reduce(bind_rows) 

write.xlsx(sharpe_full_rf, file = "05_new_sharpes_rf.xlsx",
           sheetName="01", append=TRUE)

# SR Equal-Weight Benchmark 
as.sr(x = equal_weight_monthly_return %>% 
        rename(time = date) %>% 
        mutate(equal_weight_return_1_month = equal_weight_return_1_month - 1) %>% 
        left_join(rf_usa) %>% 
        mutate(equal_weight_return_1_month  = equal_weight_return_1_month  - s_rate_10_year) %>% 
        select(-s_rate_10_year) %>% 
        ts_xts(),
      c0 = 0, # added in x 
      ope = 12, 
      na.rm = T) %>% 
  suppressMessages()


# SHARPE EQUALITY TESTS NO RF-FREE RATE -----


robust_sharpes <- function(x) {
  r1 <- performances %>% 
    filter(model == paste0(variations_fin_results[x,1]), 
           rebalance_freq == as.integer(variations_fin_results[x,2])) %>%
    rename(time = date) %>% 
    select(time, strategy_return_1_month) %>% 
    mutate(strategy_return_1_month = strategy_return_1_month - 1) %>%
    left_join(equal_weight_monthly_return %>% 
                rename(time = date) %>% 
                mutate(equal_weight_return_1_month = equal_weight_return_1_month - 1)) %>% 
    ungroup() %>% 
    select(-model, -time) %>%
    as.matrix()
  
  
  
  r2 <- sr_equality_test(r1, type = "chisq", alternative = "two.sided", vcov.func = vcov)
  
  tibble(
    model = as.character(variations_fin_results[x,1]),
    rebalancing = as.integer(variations_fin_results[x,2]),
    p.value = r2$p.value[[1]],
    sr1 = r2$SR[[1]],
    sr2 = r2$SR[[2]])
  
}

# TODO: why are SHARPE ratios so much smaller?
sharpe_equality_tests <- 
  map(1:nrow(variations_fin_results), ~robust_sharpes(.x)) %>% 
  reduce(bind_rows)

write.xlsx(sharpe_equality_tests, file = "06_significance_sharpes.xlsx",
           sheetName="01", append=TRUE)


# TEST WITH RF = US ST RATES


robust_sharpes_rf <- function(x) {
  r1 <- performances %>% 
    filter(model == paste0(variations_fin_results[x,1]), 
           rebalance_freq == as.integer(variations_fin_results[x,2])) %>%
    rename(time = date) %>% 
    select(time, strategy_return_1_month) %>% 
    mutate(strategy_return_1_month = strategy_return_1_month - 1) %>%
    left_join(equal_weight_monthly_return %>% 
                rename(time = date) %>% 
                mutate(equal_weight_return_1_month = equal_weight_return_1_month - 1)) %>% 
    ungroup() %>% 
    select(-model) %>%
    left_join(rf_usa) %>% 
    mutate(strategy_return_1_month  = strategy_return_1_month  - s_rate_10_year,
           equal_weight_return_1_month  = equal_weight_return_1_month  - s_rate_10_year) %>% 
    select(-time, -s_rate_10_year) %>% 
    as.matrix()
  
  r2 <- sr_equality_test(r1, type = "chisq", alternative = "two.sided", vcov.func = vcov)
  
  tibble(
    model = as.character(variations_fin_results[x,1]),
    rebalancing = as.integer(variations_fin_results[x,2]),
    p.value = r2$p.value[[1]],
    sr1 = r2$SR[[1]],
    sr2 = r2$SR[[2]])
  
}

# TODO: why are SHARPE ratios so much smaller?
sharpe_equality_tests_rf <- 
  map(1:nrow(variations_fin_results), ~robust_sharpes_rf(.x)) %>% 
  reduce(bind_rows)

write.xlsx(sharpe_equality_tests_rf, file = "06_significance_sharpes_rf.xlsx",
           sheetName="01", append=TRUE)




# TEST AREA SHARPER Package
sr_test(rnorm(1000,mean=0.5,sd=0.1),zeta=2,ope=1,alternative="greater")

power.sr_test(253,1,0.05,NULL,ope=253)
power.sr_test(n=NULL,zeta=0.6,sig.level=0.05,power=0.5,ope=253)

power.sropt_test(8,4*253,1,0.05,NULL,ope=253)

predint(rnorm(1000,mean=0.5,sd=0.1),oosdf=127,ope=1)

