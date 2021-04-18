library(corrr)
library(dplyr)
library(multDM)
library(tsibble)
library(ggplot2)
library(ggforce)

# GRAPH: TIME-SERIES TRAINING PROCESS

plot_segmentation <- arima_fcast %>% 
  filter(country == "UK") %>% 
  mutate(cagr_5_predict = .mean) %>% 
  ggplot(aes(date, cagr_5_predict)) +
  # draw forecast
  geom_line(aes(color = "Forecast")) +
  # draw training set
  geom_line(data = arima_training_to_plot %>% 
              filter(country == "UK") %>% 
              mutate(cagr_5_predict = cagr_n_year),
              aes(color = "Training")) +
  # draw leakage set
  geom_line(data = arima_leakage_to_plot %>% 
              filter(country == "UK") %>% 
              mutate(cagr_5_predict = cagr_n_year),
            aes(color = "Leakage"), 
            linetype = "dashed") +
  # draw actual values
  geom_line(data = arima_actual_to_plot %>% 
              filter(country == "UK") %>% 
              mutate(cagr_5_predict = cagr_n_year),
            aes(color = "Actual")) +
  
  
  labs(title = "Data Segmentation Process for U.K.",
       subtitle = "Using CAGR as a target requires the implementation of a 5-year leakage set",
       x = "Year",
       y = "5-year CAGR",
       colour = "Data / Forecast") +
  coord_cartesian(ylim = c(0.85, 1.3)) +
  scale_color_manual(values = c("Forecast" = "#00BFC4", 
                                "Training" = "black",
                                "Leakage" = "gray", 
                                "Actual" = "black")) +
  
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))
  


# GRAPH: DETAILLED FITTING PROCESS


plot_segmentation +
  # plotting important initial CAGR predictions
  geom_point(data = arima_training_to_plot %>% 
               filter(country == "UK") %>% 
               mutate(cagr_5_predict = cagr_n_year) %>% 
               filter(date == yearmonth("2000 Jul")),
             aes(fill = "Lag"),
             shape = 21,
             size = 3,
             stroke = 1) +
  # plot initial prediction
  geom_point(data = arima_fcast %>% 
               filter(country == "UK") %>% 
               mutate(cagr_5_predict = .mean) %>% 
               filter(date == yearmonth("2005 Aug")),
             aes(fill = "Prediction"),
             shape = 21,
             size = 3,
             stroke = 1) +
  # plot initial prediciton in plot
  geom_point(data = arima_fcast %>% 
               filter(country == "UK") %>% 
               mutate(cagr_5_predict = .mean) %>% 
               filter(date == yearmonth("2008 Aug")),
             aes(fill = "Lag"),
             shape = 21,
             size = 3,
             stroke = 1) +
  # plot current prediction
  geom_point(data = arima_fcast %>% 
               filter(country == "UK") %>% 
               mutate(cagr_5_predict = .mean) %>% 
               filter(date == yearmonth("2008 Sep")),
             aes(fill = "Prediction"),
             shape = 21,
             size = 3,
             stroke = 1) +
  
  # add geom label for initial (1)
  geom_label(label = "(1)",
             size = 3,
             x = as.Date(yearmonth("2002 Aug")),
             y = 1.03,
             alpha = .2,
             show.legend = F,
             aes(colour = "Forecast")) +
  
  # add geom label for next step (2)
  geom_label(label = "(2)",
             size = 3,
             x = as.Date(yearmonth("2007 Oct")),
             y = 1.12,
             alpha = .2,
             show.legend = F,
             aes(colour = "Forecast")) +
  
  # TODO: mb explain subtitle in the caption (and in cursive)
  labs(title = "Time Series Regression Model Training Process",
       subtitle = "Initial prediction is based on last leakage-free observation (1). \nNext lags use the predicted CAGR from previous steps (2)",
       x = "Year",
       y = "5-year CAGR",
       colour = "Data / Forecast",
       fill = "Fitting") +
  
  # TODO: mb in 1 row (the whole Legend)
  scale_color_manual(values = c("Forecast" = "#00BFC4", 
                                "Training" = "black",
                                "Leakage" = "gray", 
                                "Actual" = "black")) +
  scale_fill_manual(values = c("Prediction" = "#00BFC4",
                               "Lag" = "#F8766D")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(fill = guide_legend(nrow = 2),
         color = guide_legend(nrow = 2))

# GRAPH: EXPLAINABILITY OF CAGR BY INCREASING CAGR PRIOD
  
to_model_exploration %>% 
  filter(country %in% countries_to_predict) %>% 
  as_tibble() %>% 
  select(-tail(names(.),5), -date, -country) %>% 
  corrr::correlate(quiet = T) %>% 
  select(term, cape) %>% 
  slice(1:10) %>% 
  mutate(year = c(1:10),
         cape = -1*cape) %>% 
  ggplot(aes(year, cape, colour = cape)) + 
  geom_line(size = 1.4) +
  labs(x = "Number of CAGR Leads",
       y = "Correlation",
       title = "Correlation of Forward CAGR with CAPE",
       subtitle = "Correlation with CAGR and CAPE increases with increasing lead years",
       colour = "Correlation") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))

# GRAPH: CORRELATION ACROSS ALL CAGR LEADS AND CAGR / CAPE COMBINATIONS

to_model_exploration %>% 
  filter(country %in% countries_to_predict) %>% 
  as_tibble() %>% 
  select(-tail(names(.),5), -date) %>% 
  pivot_longer(cols = starts_with("cagr"), names_to = "cagr_lead", values_to = "cagr") %>% 
  mutate(cagr_lead = rep(1:10, times = nrow(.)/10)) %>% 
  filter(country == "UK") %>% 
  ggplot(aes(cagr, cape, colour = cagr_lead)) +
  geom_jitter(alpha = .1) +
  geom_smooth() +
  facet_wrap(~cagr_lead) +
  coord_cartesian(xlim = c(0.6,1.6)) +
  labs(x = "CAGR",
       y = "CAPE",
       title = "Relationship between CAGR and CAPE with increasing CAGR leads",
       subtitle = "With increasing leads, relationship becomes stronger, while the CAGR variance decreases",
       colour = "Number of CAGR Leads") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))

# EDA HYPOTHESIS

# 1.) more normal disribution the longer the return horizon
# 2.) better properties of cagr vs. normal returns (based on mean, variance, distribution)



# EDA: DISTRIBUTION OF CAGR VS. DISTRIBUTION OF RETURNS


# create returns for different leads
add_return <- function(df, lead){
  col_name <- paste0("return_", lead, "_year")
  
  df %>% 
    mutate(!!col_name := (lead(price, 12 * lead) / price))
}

return_years <- seq(1:5)

prices_local_long <- suppressMessages(
  map(return_years,
      ~add_return(prices_local_long, .x)) %>% 
    reduce(inner_join))

plot_cagr_distribution <-  prices_local_long %>% 
  filter(date < leakage_start_date, 
         country %in% countries_to_predict) %>% 
  select(date, country, cagr_5_year, cagr_1_year) %>% 
  pivot_longer(cols = 3:4,
               names_to = "trans",
               values_to = "value") %>% 
  ggplot(aes(value, fill = trans)) +
  geom_histogram(bins = 60, alpha = .6, position = "identity") +
  facet_wrap(~country, nrow = 2) +
  labs(x = "CAGR",
       y = "Density",
       title = "CAGR Distribution Across Countries and Leads") +
  theme_bw() +
  theme(legend.position = "none")

plot_return_distribution <- prices_local_long %>% 
  filter(date < leakage_start_date, 
         country %in% countries_to_predict) %>% 
  select(date, country, return_5_year, return_1_year) %>% 
  pivot_longer(cols = 3:4,
               names_to = "trans",
               values_to = "value") %>% 
  ggplot(aes(value, fill = trans)) +
  geom_histogram(bins = 60, alpha = .6, position = "identity") +
  facet_wrap(~country, nrow = 2) +
  labs(x = "Return",
       y = "Density",
       title = "Return Distribution Across Countries and Leads") +
  scale_fill_manual(values = trans,
                    labels = c("1year", "5years")) + #FIXME to 1 and 5 years in legend
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal")

plot_cagr_distribution / plot_return_distribution


# -----

prices_local_long %>% 
  filter(date < leakage_start_date, 
         country %in% countries_to_predict) %>% 
  select(country, cagr_5_year) %>% 
  ggplot(aes(cagr_5_year)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "gray60") +
  geom_density(size = 0.8) +
  facet_wrap(~country) +
  labs(x = "CAGR",
       y = "Density",
       title = "CAGR Distribution",
       subtitle = "..") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))



  
  

# GRAPH CONTAINING ALL BASE FORECASTS PER COUNTRY 

preds_vs_actuals %>% 
  filter(country %in% countries_to_predict) %>% 
  pivot_longer(cols = c("actual", "xgboost_pred", "rf_pred", "elastic_pred", "arima_pred"),
               names_to = "models",
               values_to = "cagr") %>% 
  ggplot(aes(date, cagr, colour = models)) +
  geom_line() +
  geom_hline(yintercept = 1, colour = "red", linetype = "dashed") + 
  facet_wrap(~country) +
  labs(title = "Prediction Comparison among base-models",
       subtitle = "Machine Learning models using pooling show the best fit, while ARIMA tends to over- or underestimate",
       colour = "Base Models",
       x = "Year",
       y = "5-Year CAGR") +
    # RColorBrewer::brewer.pal(n = 8, name = 'Dark2')
  scale_color_manual(values = c("actual" = "black", 
                                "xgboost_pred" = "#1B9E77", # dark turq
                                "rf_pred" = "#D95F02", # orange brown
                                "elastic_pred" = "#E7298A", # magenta
                                "arima_pred" = "#7570B3")) + # dark violett
    theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(colour = guide_legend(nrow = 2))

# GRAPH CORRELATION BETWEEN BASE FORECASTS
preds_vs_actuals %>% 
  filter(country %in% countries_to_predict) %>% 
  GGally::ggpairs(columns = 3:7)


# STACKED AND ENSEMBLE FORECASTS
preds_vs_actuals_ensemble %>% 
  left_join(preds_vs_actuals_stack %>% select(date, country, stack_pred)) %>% 
  select(date, country, actual, ensemble_mean_pred, ensemble_median_pred, stack_pred) %>% 
  pivot_longer(cols = c("actual", "ensemble_mean_pred", "ensemble_median_pred", "stack_pred"),
               names_to = "model",
               values_to = "pred") %>% 
  ggplot(aes(date, pred, colour = model)) +
  geom_line() +
  facet_wrap(~country) +
  scale_x_yearquarter() +
  scale_color_manual(values = c("actual" = "black", 
                                "ensemble_mean_pred" = "#1B9E77", # dark turq
                                "ensemble_median_pred" = "#E7298A", # magenta
                                "stack_pred" = "#D95F02")) + # dark violett
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(colour = guide_legend(nrow = 1))


# GRAPH CORRELATION BETWEEN ENSEMBLE AND STACKED FORECASTS 
preds_vs_actuals_ensemble %>% 
  left_join(preds_vs_actuals_stack %>% select(date, country, stack_pred)) %>% 
  select(date, country, actual, ensemble_mean_pred, ensemble_median_pred, stack_pred) %>%
  filter(country %in% countries_to_predict) %>% 
  GGally::ggpairs(columns = 3:6)

# SIGNAL-TO-NOISE RATIO ANALYSIS

# statistically significant difference between models in terms of forecast accuracy

# vector for model combinations
base_models <- c("xgboost_pred", "rf_pred", "elastic_pred", "arima_pred", "mean_pred")
base_models2 <- c("xgboost_pred", "rf_pred", "elastic_pred", "arima_pred", "mean_pred")
horizons <- c(1,6,12,24,36,48,60,90,120)

mc <- crossing(countries_to_predict, base_models, base_models2, horizons)

# FIXME: strange, even same models with same forecasts are not significantly different from each other
dm_test <- 
map(1:nrow(mc), ~preds_vs_actuals %>%
      filter(country %in% countries_to_predict) %>% 
      filter(country == paste0(mc[.x,1])) %>%
      mutate(period = row_number()) %>% # period number necessary for DM.test function
      select(period, actual, xgboost_pred, rf_pred, elastic_pred, arima_pred, mean_pred) %>% 
      select(period, actual, paste(mc[.x,2]), paste(mc[.x,3])) %>%
      as.matrix() %>%  
      DM.test(pluck(3), 
              pluck(4), 
              pluck(2), 
              # ASE for absolute scaled error 
              loss.type = "SE", h = mc[.x, 4] %>% as.integer(), H1 = "more") %>% pluck(4) %>% 
      tibble(p.value = .,
             Country = paste(mc[.x, 1]),
             Model = paste(mc[.x, 2]),
             CompModel = paste(mc[.x, 3]),
             h = mc[.x,4] %>% as.integer())) %>% 
      reduce(bind_rows)


# I assume that the signal to noise ratio, e.g., measured by 
# 1 minus the variance of forecast errors divided by the variance of the returns, 
# is very small, whichever model you use.


signal_to_noise <- preds_vs_actuals %>% 
  filter(country %in% countries_to_predict) %>% 
  group_by(country) %>% 
  summarise(stn_xgboost = 1 - (var(actual - xgboost_pred) / var(actual)),
            stn_rf = 1 - (var(actual - rf_pred) / var(actual)),
            stn_en = 1 - (var(actual - elastic_pred) / var(actual)),
            stn_arima = 1 - (var(actual - arima_pred) / var(actual)),
            stn_mean = 1 - (var(actual - mean_pred) / var(actual))) 
