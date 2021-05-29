library(corrr)
library(dplyr)
library(multDM)
library(tsibble)
library(ggplot2)
library(ggforce)

# GRAPH: TIME-SERIES TRAINING PROCESS -------

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
  
  
  labs(title = "Data Segmentation Process for U.K. using DR with ARIMA errors",
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

ggsave("Data_Showing Data Structures.png", path = "Plots", 
       width = 10, height = 6, dpi = 300)

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

# GRAPH: EXPLAINABILITY OF CAGR BY INCREASING CAGR PRIOD  -------
  
corr_cagr_cape <- to_model_exploration %>% 
  filter(country %in% countries_to_predict,
         country != "SPAIN") %>% 
  as_tibble() %>% 
  select(-tail(names(.),5), -date, -country) %>% 
  corrr::correlate(quiet = T) %>% 
  select(term, cape) %>% 
  slice(1:10) %>% 
  mutate(year = as.integer(c(1:10)),
         cape = -1*cape) %>% 
  ggplot(aes(year, cape, colour = cape)) + 
  geom_line(size = 1.4) +
  labs(x = "Number of CAGR Leads",
       y = "Correlation",
       # title = "Correlation of Forward CAGR with CAPE",
       # subtitle = "Correlation with CAGR and CAPE increases with increasing lead years",
       colour = "Correlation") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))
  
  ggsave("EDA_CAGR CAPE Correlation.png", path = "Plots", 
       width = 10, height = 6, dpi = 300)

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

# EDA HYPOTHESIS  -------

# 1.) more normal disribution the longer the return horizon
# 2.) better properties of cagr vs. normal returns (based on mean, variance, distribution)


# EDA: DISTRIBUTION OF CAGR VS. DISTRIBUTION OF RETURNS


# create returns for different leads
add_return <- function(df, lead){
  col_name <- paste0("return_", lead, "_month")
  
  df %>% 
    mutate(!!col_name := (lead(price, 1 * lead) / price))
}

return_months <- c(1,12,24,36,48,60)

prices_local_long <- suppressMessages(
  map(return_months,
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



  
  

# GRAPH CONTAINING ALL BASE FORECASTS PER COUNTRY   -------

preds_vs_actuals %>% 
  filter(country %in% countries_to_predict,
         country != "SPAIN") %>% 
  pivot_longer(cols = c("actual", "xgboost_pred", "rf_pred", "elastic_pred", 
                        "xgboost_single_pred","rf_single_pred", "arima_single_pred"),
               names_to = "models",
               values_to = "cagr") %>% 
  mutate(models = models %>%
           str_replace("arima_single_pred", "ARIMA") %>% 
           str_replace("elastic_pred", "EN_p") %>% 
           str_replace("rf_pred", "RF_p") %>%
           str_replace("rf_single_pred", "RF_s") %>%
           str_replace("xgboost_pred", "XGB_p") %>%
           str_replace("xgboost_single_pred", "XGB_s") %>% 
           str_replace("actual", "Actual")) %>%
  ggplot(aes(date, cagr, colour = models, linetype = models, size = models)) +
  geom_line() +
  facet_wrap(~country, nrow = 4) +
  labs(title = "Prediction Comparison among base-models",
       color = "Base-Models",
       linetype = "Base-Models",
       size = "Base-Models",
       x = "Year",
       y = "5-Year CAGR") +
    # RColorBrewer::brewer.pal(n = 8, name = 'Dark2')
  scale_color_manual(values = c("Actual" = "black", 
                                "XGB_p" = "#1B9E77", # dark turq
                                "RF_p" = "#D95F02", # orange brown
                                "EN_p" = "#E7298A", # magenta
                                "XGB_s" = "#1B9E77", # dark turq
                                "RF_s" = "#D95F02", # orange brown
                                "ARIMA" = "#7570B3")) + # dark violett
  scale_linetype_manual(values = c("Actual" = "solid", 
                                   "XGB_p" = "solid", # dark turq
                                   "RF_p" = "solid", # orange brown
                                   "EN_p" = "solid", # magenta
                                   "XGB_s" = "dashed",
                                   "RF_s" = "dashed",
                                   "ARIMA" = "dashed")) +
  scale_size_manual(values = c("Actual" = 0.7, 
                               "XGB_p" = 0.5, # dark turq
                               "RF_p" = 0.5, # orange brown
                               "EN_p" = 0.5, # magenta
                               "XGB_s" = 0.5,
                               "RF_s" = 0.5,
                               "ARIMA" = 0.5)) +
  scale_x_yearmonth(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 2))

ggsave("Results_Base Model and Predictive Performance.png", path = "Plots", 
       width = 10, height = 12, dpi = 300)

# GRAPH CORRELATION BETWEEN BASE FORECASTS
preds_vs_actuals %>% 
  filter(country %in% countries_to_predict) %>% 
  GGally::ggpairs(columns = 3:9)


# STACKED AND ENSEMBLE FORECASTS -----
preds_vs_actuals %>% 
  filter(country %in% countries_to_predict,
         country != "SPAIN") %>% 
  pivot_longer(cols = c("actual", "ensemble_mean_pred", "ensemble_median_pred", "stack_pred"),
               names_to = "model",
               values_to = "pred") %>% 
  mutate(model = model %>%
           str_replace("ensemble_mean_pred", "Mean-Stacking") %>% 
           str_replace("ensemble_median_pred", "Median-Stacking") %>%
           str_replace("stack_pred", "Elastic-Net-Stacking") %>% 
           str_replace("actual", "Actual")) %>% 
  ggplot(aes(date, pred, colour = model)) +
  geom_line() +
  facet_wrap(~country, nrow = 4) +
  labs(title = "Prediction Comparison among Stacking Models",
       colour = "Stacking / Meta-Models",
       x = "Year",
       y = "5-Year CAGR") +
  scale_x_yearquarter() +
    scale_color_manual(values = c("Actual" = "black", 
                                "Mean-Stacking" = "#1B9E77", # dark turq
                                "Median-Stacking" = "#E7298A", # magenta
                                "Elastic-Net-Stacking" = "#D95F02")) + # dark violett
  scale_x_yearmonth(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))

ggsave("Results_Ensembles and Predictive Performance.png", path = "Plots", 
       width = 10, height = 12, dpi = 300)


# GRAPH CORRELATION BETWEEN ENSEMBLE AND STACKED FORECASTS 
preds_vs_actuals %>% 
  select(date, country, actual, ensemble_mean_pred, ensemble_median_pred, stack_pred) %>%
  filter(country %in% countries_to_predict) %>% 
  GGally::ggpairs(columns = 3:6)