library(tsbox)
library(kableExtra)
library(PerformanceAnalytics)

n_countries_to_invest_in <- 3
n_rebalances <- c(1, 2, 3)
plot_max_date <- "2020 Aug"

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
           fct_relabel(~str_remove(.x, "_prediction|_pred")))

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
  pivot_longer(-c(date, country, actual), 
               names_to = "Model",
               values_to = "pred") %>% 
  group_by(country, Model) %>% 
  summarise(MAPE = mean(abs(((actual) - pred) / actual))) %>% 
  ungroup() %>% 
  group_by(Model) %>% 
  summarise(`Median MAPE` = median(MAPE)) %>% 
  full_join(returns_per_rebalancing %>%
              rename_with(~paste("CAGR", .x, "rebalances")) %>% 
              rename(Model = 1)) %>% 
  full_join(sharpes_to_table %>% 
              rename_with(~paste("Sharpe", .x, "rebalances")) %>% 
              rename(Model = 1)) %>% 
  mutate(Model = Model %>% 
           str_remove("_prediction|_pred") %>% 
           str_replace("_", " ") %>% 
           toupper() %>% 
           str_replace("SINGLE", "single") %>% 
           str_replace("RF", "Random Forest") %>% 
           str_replace("XGBOOST", "XGBoost") %>% 
           str_replace("ELASTIC", "Elastic Net")) %>% 
  mutate(`Median MAPE` = scales::number(`Median MAPE` * 100, 0.001) %>% 
           replace_na("")) %>% 
  mutate_at(vars(starts_with("CAGR")),
            ~scales::percent(.x - 1, 0.001, suffix = "")) %>% 
  mutate_at(vars(starts_with("Sharpe")),
            ~scales::number(.x, 0.001))

# sharpes_per_rebalancing <- performances %>% 
to_table %>% 
  rename_at(vars(contains("Sharpe")), 
            ~str_replace(.x, "balances", "balances ")) %>% 
  rename_with(~str_remove(.x, "CAGR |Sharpe ")) %>% 
  kable(
    caption = 
      "Financial and non-financial measures by rebalancing period") %>% 
  kable_classic_2() %>% 
  add_header_above(c(" " = 2, 
                     "CAGR" = 3,
                     "Sharpe" = 3))

p_cum <- performance_to_plot %>%
  filter(!(model_and_cagr %in% c("naive", "mean"))) %>% 
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
              distinct()) +
  facet_wrap(~model_and_cagr) + 
  # geom_hline(yintercept = 100, linetype = "dashed", color = "gray") +
  scale_x_yearmonth(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = seq(50, 300, 50),
                     labels = seq(50, 300, 50)) +
  scale_color_discrete(name = "Number of rebalances") +
  # scale_color_discrete(name = "Outperformed benchmark",
  #                      breaks = c(TRUE, FALSE),
  #                      labels = c("Yes", "No")) +
  labs(
    title = 
      "Returns of strategies based on models (colored) vs benchmark (black)",
    subtitle = paste0("Investing in ",
                      n_countries_to_invest_in,
                      " out of 8 countries"),
    caption = "Ordered from best to worst average performance by model",
    x = NULL,
    y = "Cumulative return over the whole test period") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_cum)
