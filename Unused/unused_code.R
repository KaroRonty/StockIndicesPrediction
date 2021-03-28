library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)


# Training/test/fcast split from function ---------------------------------

test_to_plot <- future_map(1:length(train_test_fcast %>%  
                                      pluck(10)),
                           ~train_test_fcast %>%  
                             pluck(10) %>% 
                             pluck(.x) %>% 
                             pluck(2)) %>% 
  reduce(bind_rows)

training_test <- bind_rows(train_test_fcast %>% 
                             pluck(10) %>% 
                             pluck(length(train_test_fcast)) %>% 
                             pluck(1),
                           test_to_plot)

fcast_to_plot <- future_map(1:length(train_test_fcast %>%  
                                       pluck(10)),
                            ~train_test_fcast %>%  
                              pluck(10) %>% 
                              pluck(.x) %>% 
                              pluck(3)) %>% 
  reduce(bind_rows)

fcast_to_plot %>%
  filter(.model == "ARIMA") %>% # FIXME
  autoplot(color = "red", size = 1) +
  autolayer(training_test %>% 
              filter(country %in% fcast_to_plot$country), 
            cagr_10_year,
            color = "black",
            size = 1) +
  annotate("rect", fill = "gray", alpha = 0.25, 
           xmin = as.Date(leakage_start_date), xmax = as.Date(leakage_end_date),
           ymin = -Inf, ymax = Inf) +
  geom_hline(yintercept = 1) +
  scale_x_yearmonth(labels = year(seq.Date(fcast_start_date,
                                           as.Date(max(test$date)),
                                           by = "5 years")),
                    breaks = yearmonth(seq.Date(fcast_start_date,
                                                as.Date(max(test$date)),
                                                by = "5 years"))) +
  facet_wrap(~ country) + 
  geom_vline(xintercept = as.Date(leakage_start_date),
             color = "gray", linetype = "dashed") +
  geom_vline(xintercept = as.Date(leakage_end_date),
             color = "gray", linetype = "dashed") +
  theme_minimal() +
  expand_limits(x = fcast_start_date) +
  theme(legend.position = c(0.95, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(angle = 45))

# Accuracy of different slices --------------------------------------------

slice_acc %>% 
  reduce(bind_rows) %>% 
  mutate(diff = ARIMA - MEAN) %>% 
  group_by(slice, start, end) %>% 
  summarise(mean_diff = mean(diff)) %>% 
  print(n = 100)

# Unemployment correlation example ----------------------------------------

t4 <- map(c("prices_local_long", "unemployment_long"), ~get(.x)) %>% 
  reduce(full_join) %>% 
  select(date, country, !!cagrs, cagr_10_year, unemployment) %>% 
  as_tsibble(key = "country", index = "date") %>% 
  select(date, country, cagr_10_year, unemployment) %>% 
  filter(date >= yearmonth(leakage_end_date))

t3 %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  do(cor = cor(.$cagr_10_year, .$unemployment, use = "pairwise.complete.obs")) %>% 
  mutate(cor = pluck(cor, 1)) %>% View

# Spain unemployment vs CAGR ----------------------------------------------

t2 <- training %>% inner_join(unemployment_long) %>% 
  filter(country == "SPAIN")

t2 %>% 
  as_tibble() %>% 
  pivot_longer(c(cagr_10_year, unemployment)) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free", nrow = 2) +
  ggtitle("10-year CAGR vs unemployment rate in Spain") +
  theme_minimal()

# ---- XGB

cv <- trainControl(method = "timeslice", # used timeslice!
                   initialWindow = 120, # training observations
                   horizon = 120, # prediction ahead
                   skip = 120 + 120 - 1, # skip to avoid data leakage (Karo tested it - but hoow?)
                   fixedWindow = TRUE, # starts at the next observation
                   allowParallel = TRUE) # parallel backend usage

xgb.training <- function(c) {
  train(training %>% as_tibble() %>% filter(country == c) %>% select(-date, -cagr_10_year, -country) %>% as.matrix(),
        training %>% filter(country == c) %>%  pull(cagr_10_year), 
        method = "xgbTree",
        trControl = cv)
  # difficulty is to find fitting parameters for every country (usually the error arises from too low values for horizon or skip)
  # assign(xgboost, paste0("xgboost_",c))
}

r1 <- lapply(unique(training$country), xgb.training)
do.call(rbind, r1)


c = "GERMANY"

xgboost <- train(training %>% as_tibble() %>% filter(country == c) %>% select(-date, -cagr_10_year, -country) %>% as.matrix(),
                 training %>% filter(country == c) %>%  pull(cagr_10_year), 
                 method = "xgbTree",
                 trControl = cv)

models <- tibble(name = c("xgboost"),
                 model = NA,
                 actual = NA,
                 pred = NA,
                 rsq_cv = NA,
                 mae_cv = NA,
                 date_train = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models)){
  models$model[i] <- get(models$name[i]) %>% list() # get urges to get the whole content 
  models$actual[i] <- training %>% filter(country == c) %>% pull(cagr_10_year) %>% list() # pull actual data
  models$pred[i] <- predict(get(models$name[i]), # get predictions based on actual training data
                            training %>%
                              as_tibble() %>% 
                              filter(country == c) %>% 
                              select(-date, -cagr_10_year, -country) %>%
                              as.matrix()) %>%
    as.vector() %>% 
    list()
  models$rsq_cv[i] <- get(models$name[i])$resample$Rsquared %>% mean(na.rm = TRUE)
  models$mae_cv[i] <- get(models$name[i])$resample$MAE %>% mean(na.rm = TRUE)
  models$date_train[i] <- training %>% filter(country == c) %>% pull(date) %>% list() # pull dates to make understandable again
}

# Make a tibble for storing test set results
models_test <- tibble(name = models$name,
                      actual_test = NA,
                      pred_test = NA,
                      rsq_test = NA,
                      mae_test = NA,
                      pred_future = NA,
                      date_test = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models_test)){
  models_test$actual_test[i] <- test %>% filter(country == c) %>%  pull(cagr_10_year) %>% list()
  models_test$pred_test[i] <- predict(get(models_test$name[i]),
                                      test %>% 
                                        as_tibble() %>% 
                                        filter(country == c) %>% 
                                        select(-date, -cagr_10_year, -country) %>%
                                        as.matrix()) %>%
    as.vector() %>% 
    list()
  models_test$rsq_test[i] <- cor(models_test$actual_test[[i]],
                                 models_test$pred_test[[i]])^2
  models_test$mae_test[i] <- mean(abs(models_test$pred_test[[i]] -
                                        models_test$actual_test[[i]]))
  models_test$date_test[i] <- test %>% filter(country == c) %>% pull(date) %>% list()
}



# -------------------------------------------------------------------------

# Amount of obs
(training %>% 
    filter(country == "USA") %>% 
    pull(date) %>% 
    max() -
    training %>% 
    filter(country == "USA") %>% 
    pull(date) %>% 
    min()) / 12 +
  (test %>% 
     filter(country == "USA") %>% 
     pull(date) %>% 
     max() -
     test %>% 
     filter(country == "USA") %>% 
     pull(date) %>% 
     min()) / 12

test %>% 
  pivot_wider(id = date, names_from = country, values_from = cagr_10_year) %>%
  View()

# Sheets where country data starts and ends
start_c <- 11
end_c <- 52

fundamental <- sapply(excel_sheets("Data/all_data.xlsx")[start_c:end_c],
                      function(x) read_xlsx("Data/all_data.xlsx",
                                            sheet = x,
                                            skip = 5,
                                            .name_repair = "unique"))


price <- sapply(excel_sheets("Data/MT1.xlsx"),
                function(x) clean_names(read_xlsx("Data/MT1.xlsx",
                                                  sheet = x,
                                                  skip = 1,
                                                  .name_repair = "unique")))

us <- price$Sheet3 %>%
  select(date:profit_margin_6)

t1 <- us %>% 
  select(date, last_price_2, x10_year_moving_average_p_e_4, price_earnings_ratio_5)

price_lags <- sapply(seq(4, 80, 4),
                     function(x) (lag(t1$last_price_2, x - 1) /
                                    t1$last_price_2)^(1 / (x / 4)))

t2 <- tibble(Correlation = sapply(1:ncol(price_lags), 
                                  function(x) cor(price_lags[, x], 
                                                  t1$price_earnings_ratio_5, 
                                                  use = "pairwise.complete.obs")),
             n = 1:ncol(price_lags))

ggplot(t2, aes(n, Correlation)) +
  geom_line(aes(color = Correlation), size = 2) +
  ggtitle("Correlation between the P/E ratio and CAGR returns of n years",
          subtitle = "For the S&P 500 index, quarterly data from 1954-03-31 until 2019-09-30") +
  scale_y_continuous(trans = "reverse", limits = c(0, -1), labels = scales::percent) +
  scale_x_continuous(breaks = 1:ncol(price_lags), minor_breaks = 1:ncol(price_lags)) +
  theme_minimal()



macro <- sapply(excel_sheets("Data/macro_clean.xlsx"),
                function(x) print(
                  paste(x, colnames(read_xlsx("Data/macro_clean.xlsx", sheet = x)))))

prices <- ""


to_model %>% 
  as_tibble() %>% 
  ungroup() %>% 
  group_by(country) %>% 
  na.omit() %>% 
  summarise(n = n()) %>% 
  arrange(-n)

training %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(duration = (max(date) - min(date)) / 12)

training %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(duration = (max(date) - min(date)) / 12) %>% 
  inner_join(arima_acc) %>% 
  arrange(MAE) %>% 
  mutate(Duration = ifelse(duration >= 13,
                           "13 years of data", 
                           "Less than 13 years of data")) %>% 
  group_by(Duration) %>% 
  summarise(Count = n(),
            RMSE = mean(RMSE),
            MAE = mean(MAE),
            MAPE = mean(MAPE))

library(ggrepel)

training %>% 
  as_tibble() %>% 
  group_by(country) %>% 
  summarise(duration = (max(date) - min(date)) / 12) %>% 
  inner_join(arima_acc) %>% 
  arrange(MAE) %>% 
  ggplot(aes(x = duration, y = RMSE, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", alpha = 0.2, size = 0, span = 0.5) +
  geom_text_repel(aes(label = country)) +
  scale_x_continuous(breaks = 1:13,
                     labels = 1:13) +
  ggtitle("Accuracy of each model vs years of observations") +
  xlab("Years of observations") +
  theme_minimal() +
  theme(legend.position = "none")


# Double-checking accuracy calculation
fcast_no_leakage %>% 
  accuracy(bind_rows(training, test)) %>% 
  select(.model, country, .type, RMSE, MAE, MAPE) %>% 
  filter(!is.na(MAE)) %>% 
  arrange(MAE)

fcast_no_leakage %>% 
  as_tibble() %>% 
  filter(.model == "ARIMA") %>% 
  select(.model, country, date, .mean) %>% 
  inner_join(test %>% 
               select(country, date, cagr_10_year)) %>% 
  group_by(country) %>% 
  mutate(resid = cagr_10_year - .mean) %>% 
  summarise(MAE = MAE(resid))