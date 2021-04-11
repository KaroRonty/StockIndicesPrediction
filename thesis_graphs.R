library(corrr)
library(dplyr)
library(tsibble)
library(ggplot2)
library(ggforce)

# PRESELECT FORECAST FOR PLOTTING (from modelling.R, fcasts)

plot_country <- "USA"
cols <- c("Predicted" = "gray10",
          "Original" = "gray50")

to_fcast_graph <- fcasts %>% 
  filter(country == plot_country,
         source == "cagr_5_year",
         .model == "ARIMA") %>% 
  select(date, .mean) %>% 
  mutate(cagr_5_year = .mean) %>% 
  select(-.mean)

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
                                "Training" = "#F8766D",
                                "Leakage" = "gray", 
                                "Actual" = "#F8766D")) +
  
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
                                "Training" = "#F8766D",
                                "Leakage" = "gray", 
                                "Actual" = "#F8766D")) +
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


