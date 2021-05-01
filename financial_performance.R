rebalancing_frequency <- "1 year"
n_countries_to_invest_in <- 4

investing_dates <- seq.Date(as_date(yearmonth("2005 Aug")),
                            as_date(yearmonth("2020 Aug")),
                            rebalancing_frequency) %>% 
  yearmonth()

years_in_data <- (last(investing_dates) - first(investing_dates)) / 12

next_month_returns <- prices_local_long %>% 
  as_tibble() %>% 
  select(date, country, return_next_1_month) %>%
  na.omit() %>% 
  filter(country %in% countries_to_predict,
         date >= min(investing_dates)) %>%
  group_by_all() %>% 
  mutate(rebalancing_group = 
           which(date <= yearmonth(investing_dates)[-1])[1]) %>% 
  ungroup()

best_countries_by_model <- preds_vs_actuals %>% 
  na.omit() %>% 
  filter(date %in% investing_dates) %>% 
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

cumulative_return_comparison %>% color(6)

cagr_return_comparison <- cumulative_return_comparison %>% 
  mutate_all(~.x^(1 / years_in_data)) %>% 
  suppressMessages()

cagr_return_comparison %>% color(6)

monthly_returns_strategy_cum_to_plot <- monthly_returns_strategy_cum %>% 
  inner_join(cagr_return_comparison) %>% 
  mutate(outperformed = last(strategy) > last(benchmark)) %>% 
  mutate(model_and_cagr = model %>% 
           factor(levels = cagr_return_comparison %>% 
                    pull(model)) %>% 
           fct_relabel(~str_remove(.x, "_prediction|_pred"))) %>% 
  suppressMessages()

p_cum <- monthly_returns_strategy_cum_to_plot %>% 
  ggplot(aes(date, strategy_return_cum, color = outperformed)) +
  geom_line() +
  geom_line(aes(date, equal_weight_return_cum),
            inherit.aes = FALSE,
            data = equal_weight_monthly_return_cum %>% 
              filter(date <= monthly_returns_strategy_cum_to_plot %>% 
                       pull(date) %>% 
                       max())) +
  facet_wrap(~model_and_cagr) + 
  scale_x_yearmonth(guide = guide_axis(n.dodge = 2)) +
  scale_color_discrete(name = "Outperformed benchmark",
                       breaks = c(TRUE, FALSE),
                       labels = c("Yes", "No")) +
  labs(title = "Returns of strategies based on models (colored) vs benchmark (black)",
       subtitle = paste0("Rebalancing frequency of ",
                         rebalancing_frequency),
       caption = "Ordered from best to worst performance",
       x = NULL,
       y = "Cumulative return over the whole test period") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_cum)
