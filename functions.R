get_final_single_rf_model <- function(.x){
  selected_country <- countries_to_predict[.x]
  
  split_indices_df_rf_single <- to_model_rf_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_rf_single <- list(
    analysis = split_indices_df_rf_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_rf_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  rf_single_data <- make_splits(
    split_indices_rf_single, 
    to_model_rf_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  rf_single_training <- training(rf_single_data)
  
  model_recipe_rf_single <- recipe(
    cagr_n_year ~
      cape + 
      dividend_yield + 
      rate_10_year +
      unemployment +
      s_rate_10_year +
      cpi, 
    data = rf_single_training) %>% 
    step_nzv(cape,
             dividend_yield,
             rate_10_year,
             unemployment,
             s_rate_10_year,
             cpi)
  
  rf_single_workflow <- workflow() %>%
    add_recipe(model_recipe_rf_single) %>% 
    add_model(rf_single_specification)
  
  set.seed(1)
  rf_single_workflow %>% 
    finalize_workflow(rf_tuning_results_single[[.x]] %>% 
                        select_best(metric = "mape")) %>% 
    fit(rf_single_training)
}

get_final_single_rf_training_predictions <- function(.x){
  selected_country <- countries_to_predict[.x]
  
  split_indices_df_rf_single <- to_model_rf_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_rf_single <- list(
    analysis = split_indices_df_rf_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_rf_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  rf_single_data <- make_splits(
    split_indices_rf_single, 
    to_model_rf_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  rf_single_training <- training(rf_single_data)
  
  rf_model_single_temp <- rf_models_single[[.x]]
  
  tibble(date = rf_single_training %>% 
           as_tibble() %>% 
           pull(date) %>% 
           yearmonth(),
         country = selected_country,
         rf_single_pred = rf_model_single_temp %>% 
           predict(rf_single_training) %>% 
           pull(.pred))
}

get_final_single_rf_predictions <- function(.x){
  selected_country <- countries_to_predict[.x]
  
  split_indices_df_rf_single <- to_model_rf_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_rf_single <- list(
    analysis = split_indices_df_rf_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_rf_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  rf_single_data <- make_splits(
    split_indices_rf_single, 
    to_model_rf_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  rf_single_test <- testing(rf_single_data)
  
  rf_model_single_temp <- rf_models_single[[.x]]
  
  tibble(date = rf_single_test %>% 
           as_tibble() %>% 
           pull(date) %>% 
           yearmonth(),
         country = selected_country,
         rf_single_pred = rf_model_single_temp %>% 
           predict(rf_single_test) %>% 
           pull(.pred))
}

get_final_single_xgboost_model <- function(.x){
  selected_country <- countries_to_predict[.x]
  
  split_indices_df_xgboost_single <- to_model_xgboost_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_xgboost_single <- list(
    analysis = split_indices_df_xgboost_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_xgboost_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  xgboost_single_data <- make_splits(
    split_indices_xgboost_single, 
    to_model_xgboost_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  xgboost_single_training <- training(xgboost_single_data)
  
  model_recipe_xgboost_single <- recipe(
    cagr_n_year ~
      cape + 
      dividend_yield + 
      rate_10_year +
      unemployment +
      s_rate_10_year +
      cpi, 
    data = xgboost_single_training) %>% 
    step_nzv(cape,
             dividend_yield,
             rate_10_year,
             unemployment,
             s_rate_10_year,
             cpi)
  
  xgboost_single_workflow <- workflow() %>%
    add_recipe(model_recipe_xgboost_single) %>% 
    add_model(xgboost_single_specification)
  
  set.seed(1)
  xgboost_single_workflow %>% 
    finalize_workflow(xgboost_tuning_results_single[[.x]] %>% 
                        select_best(metric = "mape")) %>% 
    fit(xgboost_single_training)
}

get_final_single_xgboost_training_predictions <- function(.x){
  selected_country <- countries_to_predict[.x]
  
  split_indices_df_xgboost_single <- to_model_xgboost_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_xgboost_single <- list(
    analysis = split_indices_df_xgboost_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_xgboost_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  xgboost_single_data <- make_splits(
    split_indices_xgboost_single, 
    to_model_xgboost_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  xgboost_single_training <- training(xgboost_single_data)
  
  xgboost_model_single_temp <- xgboost_models_single[[.x]]
  
  tibble(date = xgboost_single_training %>% 
           as_tibble() %>% 
           pull(date) %>% 
           yearmonth(),
         country = selected_country,
         xgboost_single_pred = xgboost_model_single_temp %>% 
           predict(xgboost_single_training) %>% 
           pull(.pred))
}

get_final_single_xgboost_predictions <- function(.x){
  selected_country <- countries_to_predict[.x]
  
  split_indices_df_xgboost_single <- to_model_xgboost_single_temp %>%  
    filter(country == selected_country) %>% 
    mutate(.row = row_number(),
           set = case_when(date < yearmonth(leakage_start_date) ~ "training",
                           date >= yearmonth(leakage_start_date) & 
                             date < yearmonth(leakage_end_date) ~ "leakage",
                           date >= yearmonth(leakage_end_date) ~ "test"))
  
  split_indices_xgboost_single <- list(
    analysis = split_indices_df_xgboost_single %>% 
      filter(set == "training") %>% 
      pull(.row) %>% 
      as.integer(),
    assessment = split_indices_df_xgboost_single %>% 
      filter(set == "test") %>% 
      pull(.row) %>% 
      as.integer())
  
  xgboost_single_data <- make_splits(
    split_indices_xgboost_single, 
    to_model_xgboost_single_temp %>% 
      mutate(date = as.numeric(date)) %>%
      filter(country == selected_country) %>% 
      select(-country) %>% 
      as.matrix())
  
  xgboost_single_test <- testing(xgboost_single_data)
  
  xgboost_model_single_temp <- xgboost_models_single[[.x]]
  
  tibble(date = xgboost_single_test %>% 
           as_tibble() %>% 
           pull(date) %>% 
           yearmonth(),
         country = selected_country,
         xgboost_single_pred = xgboost_model_single_temp %>% 
           predict(xgboost_single_test) %>% 
           pull(.pred))
}

predict_xgboost <- function(newdata){
  to_model_xgboost_ale <- build.x(~ .,
                                  data = newdata[, -1],
                                  contrasts = FALSE) %>% 
    as_tibble() %>% 
    add_column(date = newdata$date) %>% 
    full_join(to_model %>%
                select(starts_with("country")) %>% 
                slice(0)) %>% 
    mutate_at(vars(starts_with("country")),
              ~replace_na(0))
  
  ale_data_xgboost <- model_recipe_prepared %>% 
    bake(to_model_xgboost_ale)
  
  xgboost_model %>% 
    predict(to_model_xgboost_ale %>% 
              add_column(country = newdata$country) %>% 
              filter(country %in% countries_to_predict)) %>% 
    mutate(country = newdata %>% 
             filter(country %in% countries_to_predict) %>% 
             pull(country),
           date = newdata %>% 
             filter(country %in% countries_to_predict) %>% 
             pull(date)) %>% 
    select(date, country, xgboost_pred = .pred)
}

predict_rf <- function(newdata){
  to_model_rf_ale <- build.x(~ .,
                             data = newdata[, -1],
                             contrasts = FALSE) %>% 
    as_tibble() %>% 
    add_column(date = newdata$date) %>% 
    full_join(to_model %>%
                select(starts_with("country")) %>% 
                slice(0)) %>% 
    mutate_at(vars(starts_with("country")),
              ~replace_na(0))
  
  ale_data_rf <- model_recipe_prepared %>% 
    bake(to_model_rf_ale)
  
  rf_model %>% 
    predict(to_model_rf_ale %>% 
              add_column(country = newdata$country) %>% 
              filter(country %in% countries_to_predict)) %>% 
    mutate(country = newdata %>% 
             filter(country %in% countries_to_predict) %>% 
             pull(country),
           date = newdata %>% 
             filter(country %in% countries_to_predict) %>% 
             pull(date)) %>% 
    select(date, country, rf_pred = .pred)
}

predict_elastic <- function(newdata){
  to_model_elastic_ale <- build.x(~ .,
                                  data = newdata[, -1],
                                  contrasts = FALSE) %>% 
    as_tibble() %>% 
    full_join(to_model %>%
                select(starts_with("country")) %>% 
                slice(0)) %>% 
    mutate_at(vars(starts_with("country")),
              ~replace_na(0)) %>% 
    mutate_if(is.numeric, ~ifelse(.x == 1000, NA, .x)) %>% 
    add_column(date = newdata$date) 
  
  ale_data_elastic <- model_recipe_elastic_prepared %>% 
    bake(to_model_elastic_ale)
  
  elastic_model %>% 
    predict(to_model_elastic_ale %>% 
              add_column(country = newdata$country) %>% 
              filter(country %in% countries_to_predict)) %>% 
    mutate(country = newdata %>% 
             filter(country %in% countries_to_predict) %>% 
             pull(country),
           date = newdata %>% 
             filter(country %in% countries_to_predict) %>% 
             pull(date)) %>% 
    select(date, country, elastic_pred = .pred)
}

predict_arima <- function(country_n, newdata){
  
  newdata_temp <- newdata %>% 
    filter(country == countries_to_predict[country_n])
  
  to_model_arima_ale <- build.x(~ .,
                                data = newdata_temp[, -1:-2],
                                contrasts = FALSE) %>% 
    as_tibble() %>% 
    mutate_if(is.numeric, ~ifelse(.x == 1000, NA, .x))
  
  ale_data_arima <- model_recipe_arima_prepared %>% 
    bake(to_model_arima_ale %>% 
           mutate_all(as.numeric)) %>% 
    mutate(country = newdata_temp %>% 
             pull(country),
           date = newdata_temp %>% 
             pull(date)) %>% 
    # na.omit() %>% 
    as_tsibble(key = "country") %>% 
    suppressMessages()
  
  arima_model[[country_n]][[1]] %>% 
    forecast(ale_data_arima) %>% 
    as_tibble() %>% 
    select(date, country, arima_single_pred = .mean)
}

predict_xgboost_single <- function(country_n, newdata){
  
  newdata_temp <- newdata %>% 
    filter(country == countries_to_predict[country_n])
  
  to_model_xgboost_single_ale <- build.x(~ .,
                                         data = newdata_temp[, -1:-2],
                                         contrasts = FALSE) %>% 
    as_tibble() %>% 
    add_column(date = as.numeric(newdata_temp$date),
               country = countries_to_predict[country_n])
  
  model_recipe_xgboost_single_ale <- recipe(
    cagr_n_year ~
      cape + 
      dividend_yield + 
      rate_10_year +
      unemployment +
      s_rate_10_year +
      cpi, 
    data = to_model_xgboost_single_ale)
  
  ale_data_xgboost_single <- model_recipe_xgboost_single_ale %>% 
    prep() %>% 
    juice()
  
  predict(xgboost_models_single[[country_n]], ale_data_xgboost_single) %>% 
    mutate(country = countries_to_predict[country_n],
           date = newdata_temp$date) %>% 
    select(date, country, xgboost_single_pred = .pred)
}

predict_rf_single <- function(country_n, newdata){
  
  newdata_temp <- newdata %>% 
    filter(country == countries_to_predict[country_n])
  
  to_model_rf_single_ale <- build.x(~ .,
                                    data = newdata_temp[, -1:-2],
                                    contrasts = FALSE) %>% 
    as_tibble() %>% 
    add_column(date = as.numeric(newdata_temp$date),
               country = countries_to_predict[country_n])
  
  model_recipe_rf_single_ale <- recipe(
    cagr_n_year ~
      cape + 
      dividend_yield + 
      rate_10_year +
      unemployment +
      s_rate_10_year +
      cpi, 
    data = to_model_rf_single_ale)
  
  ale_data_rf_single <- model_recipe_rf_single_ale %>% 
    prep() %>% 
    juice()
  
  predict(rf_models_single[[country_n]], ale_data_rf_single) %>% 
    mutate(country = countries_to_predict[country_n],
           date = newdata_temp$date) %>% 
    select(date, country, rf_single_pred = .pred)
}

predict_stack <- function(X.model, newdata){
  xgboost_pred <- predict_xgboost(newdata)
  
  rf_pred <- predict_rf(newdata)
  
  elastic_pred <- predict_elastic(newdata)
  
  arima_pred <- map(
    seq_along(countries_to_predict),
    ~safely(predict_arima)(
      .x, 
      newdata)$result) %>% 
    bind_rows()
  
  xgboost_single_pred <- map(
    seq_along(countries_to_predict),
    ~safely(predict_xgboost_single)(
      .x, 
      newdata)$result) %>% 
    bind_rows()
  
  rf_single_pred <- map(
    seq_along(countries_to_predict),
    ~safely(predict_rf_single)(
      .x, 
      newdata)$result) %>% 
    bind_rows()
  
  to_stack_ale <- map(c("xgboost_pred",
                        "rf_pred",
                        "elastic_pred",
                        "arima_pred",
                        "xgboost_single_pred",
                        "rf_single_pred"),
                      ~get(.x)) %>% 
    reduce(full_join) %>% 
    rename_with(~str_remove(.x, "_pred"))
  
  X.model %>% 
    predict(to_stack_ale) %>% 
    pull(.pred)
}

custom_pdp <- function (X, X.model, pred.fun, J, K){
  N = dim(X)[1]
  d = dim(X)[2]
  if (length(J) == 1) {
    if (class(X[, J]) == "numeric" | class(X[, J]) == "integer") {
      fJ = numeric(K)
      fJ = numeric(K)
      xmin = min(X[, J][X[, J] < 1000])
      xmax = max(X[, J][X[, J] < 1000])
      x = seq(xmin, xmax, length.out = K)
      for (k in 1:K) {
        X.predict = X
        X.predict[, J] = x[k]
        y.hat = pred.fun(X.model = X.model, newdata = X.predict)
        fJ[k] = mean(y.hat)
      }
      a <- cut(X[, J],
               breaks = c(xmin - (x[2] - x[1]),
                          x),
               include.lowest = TRUE)
      b <- as.numeric(table(a))
      fJ = fJ - sum(fJ * b) / sum(b)
    }
    else if (class(X[, J]) == "factor") {
      X[, J] <- droplevels(X[, J])
      x.count <- as.numeric(table(X[, J]))
      x.prob <- x.count / sum(x.count)
      K <- nlevels(X[, J])
      x <- levels(X[, J])
      fJ = numeric(K)
      for (k in 1:K) {
        X.predict = X
        X.predict[, J] = x[k]
        y.hat = pred.fun(X.model = X.model, newdata = X.predict)
        fJ[k] = mean(y.hat)
      }
      fJ = fJ - sum(fJ * x.prob)
      barplot(
        fJ,
        names = x,
        xlab = paste("x_", J, " (",
                     names(X)[J], ")", sep = ""),
        ylab = paste("f_",
                     J, "(x_", J, ")", sep = ""),
        las = 3
      )
    }
    else
      print("error:  class(X[,J]) must be either factor or numeric or integer")
  }
  else if (length(J) == 2) {
    if (class(X[, J[2]]) != "numeric" & class(X[, J[2]]) !=
        "integer") {
      print("error: X[,J[2]] must be numeric or integer. Only X[,J[1]] can be a factor")
    }
    if (class(X[, J[1]]) == "factor") {
      X[, J[1]] <- droplevels(X[, J[1]])
      K1 <- nlevels(X[, J[1]])
      fJ = matrix(0, K1, K)
      x1.char <- levels(X[, J[1]])
      x1.num <- 1:K1
      xmin2 = min(X[, J[2]])
      xmax2 = max(X[, J[2]])
      x2 = seq(xmin2, xmax2, length.out = K)
      for (k1 in 1:K1) {
        for (k2 in 1:K) {
          X.predict = X
          X.predict[, J[1]] = x1.char[k1]
          X.predict[, J[2]] = x2[k2]
          y.hat = pred.fun(X.model = X.model, newdata = X.predict)
          fJ[k1, k2] = mean(y.hat)
        }
      }
      b1 = as.numeric(table(X[, J[1]]))
      a2 = cut(X[, J[2]],
               breaks = c(xmin2 - (x2[2] -
                                     x2[1]), x2),
               include.lowest = TRUE)
      b2 = as.numeric(table(a2))
      b = as.matrix(table(X[, J[1]], a2))
      fJ1 = apply(t(fJ) * b2, 2, sum) / sum(b2)
      fJ2 = apply(fJ * b1, 2, sum) / sum(b1)
      fJ = fJ - outer(fJ1, rep(1, K)) - outer(rep(1, K1),
                                              fJ2)
      fJ0 = sum(fJ * b) / sum(b)
      fJ = fJ - fJ0
      x <- list(x1.char, x2)
      K <- c(K1, K)
      image(
        x1.num,
        x2,
        fJ,
        xlab = paste("x_", J[1], " (",
                     names(X)[J[1]], ")", sep = ""),
        ylab = paste("x_",
                     J[2], " (", names(X)[J[2]], ")", sep = ""),
        ylim = range(x2),
        yaxs = "i"
      )
      contour(x1.num, x2, fJ, add = TRUE, drawlabels = TRUE)
      axis(
        side = 1,
        labels = x1.char,
        at = 1:K1,
        las = 3,
        padj = 1.2
      )
    }
    else if (class(X[, J[1]]) == "numeric" | class(X[, J[1]]) ==
             "integer") {
      fJ = matrix(0, K, K)
      xmin1 = min(X[, J[1]])
      xmax1 = max(X[, J[1]])
      xmin2 = min(X[, J[2]])
      xmax2 = max(X[, J[2]])
      x1 = seq(xmin1, xmax1, length.out = K)
      x2 = seq(xmin2, xmax2, length.out = K)
      for (k1 in 1:K) {
        for (k2 in 1:K) {
          X.predict = X
          X.predict[, J[1]] = x1[k1]
          X.predict[, J[2]] = x2[k2]
          y.hat = pred.fun(X.model = X.model, newdata = X.predict)
          fJ[k1, k2] = mean(y.hat)
        }
      }
      a1 = cut(X[, J[1]],
               breaks = c(xmin1 - (x1[2] -
                                     x1[1]), x1),
               include.lowest = TRUE)
      a2 = cut(X[, J[2]],
               breaks = c(xmin2 - (x2[2] -
                                     x2[1]), x2),
               include.lowest = TRUE)
      b1 = as.numeric(table(a1))
      b2 = as.numeric(table(a2))
      b = as.matrix(table(a1, a2))
      fJ1 = apply(t(fJ) * b2, 2, sum) / sum(b2)
      fJ2 = apply(fJ * b1, 2, sum) / sum(b1)
      fJ = fJ - outer(fJ1, rep(1, K)) - outer(rep(1, K),
                                              fJ2)
      fJ0 = sum(fJ * b) / sum(b)
      fJ = fJ - fJ0
      x <- list(x1, x2)
      K <- c(K, K)
      image(
        x1,
        x2,
        fJ,
        xlab = paste("x_", J[1], " (",
                     names(X)[J[1]], ")", sep = ""),
        ylab = paste("x_",
                     J[2], " (", names(X)[J[2]], ")", sep = ""),
        xlim = range(x1),
        ylim = range(x2),
        xaxs = "i",
        yaxs = "i"
      )
      contour(x1, x2, fJ, add = TRUE, drawlabels = TRUE)
    }
    else
      print("error:  class(X[,J[1]]) must be either factor or numeric/integer")
  }
  else
    print("error:  J must be a vector of length one or two")
  list(x.values = x, f.values = fJ)
}

custom_ale <- function(X, X.model, pred.fun, J, K = 40, NA.plot = TRUE){
  X <- X[X[, J] != 1000, ]
  N = dim(X)[1]
  d = dim(X)[2]
  if (length(J) == 1) {
    if (class(X[, J]) == "factor") {
      X[, J] <- droplevels(X[, J])
      x.count <- as.numeric(table(X[, J]))
      x.prob <- x.count / sum(x.count)
      K <- nlevels(X[, J])
      D.cum <- matrix(0, K, K)
      D <- matrix(0, K, K)
      for (j in setdiff(1:d, J)) {
        if (class(X[, j]) == "factor") {
          A = table(X[, J], X[, j])
          A = A / x.count
          for (i in 1:(K - 1)) {
            for (k in (i + 1):K) {
              D[i, k] = sum(abs(A[i,] - A[k,])) / 2
              D[k, i] = D[i, k]
            }
          }
          D.cum <- D.cum + D
        }
        else {
          q.x.all <- quantile(
            X[, j],
            probs = seq(0,
                        1, length.out = 100),
            na.rm = TRUE,
            names = FALSE
          )
          x.ecdf = tapply(X[, j], X[, J], ecdf)
          for (i in 1:(K - 1)) {
            for (k in (i + 1):K) {
              D[i, k] = max(abs(x.ecdf[[i]](q.x.all) -
                                  x.ecdf[[k]](q.x.all)))
              D[k, i] = D[i, k]
            }
          }
          D.cum <- D.cum + D
        }
      }
      D1D <- cmdscale(D.cum, k = 1)
      ind.ord <- sort(D1D, index.return = T)$ix
      ord.ind <- sort(ind.ord, index.return = T)$ix
      levs.orig <- levels(X[, J])
      levs.ord <- levs.orig[ind.ord]
      x.ord <- ord.ind[as.numeric(X[, J])]
      row.ind.plus <- (1:N)[x.ord < K]
      row.ind.neg <- (1:N)[x.ord > 1]
      X.plus <- X
      X.neg <- X
      X.plus[row.ind.plus, J] <- levs.ord[x.ord[row.ind.plus] +
                                            1]
      X.neg[row.ind.neg, J] <- levs.ord[x.ord[row.ind.neg] -
                                          1]
      y.hat <- pred.fun(X.model = X.model, newdata = X)
      y.hat.plus <-
        pred.fun(X.model = X.model, newdata = X.plus[row.ind.plus,])
      y.hat.neg <-
        pred.fun(X.model = X.model, newdata = X.neg[row.ind.neg,])
      Delta.plus <- y.hat.plus - y.hat[row.ind.plus]
      Delta.neg <- y.hat[row.ind.neg] - y.hat.neg
      Delta <- as.numeric(tapply(c(Delta.plus, Delta.neg),
                                 c(x.ord[row.ind.plus], x.ord[row.ind.neg] -
                                     1), mean))
      fJ <- c(0, cumsum(Delta))
      fJ = fJ - sum(fJ * x.prob[ind.ord])
      x <- levs.ord
      barplot(
        fJ,
        names = x,
        xlab = paste("x_", J, " (",
                     names(X)[J], ")", sep = ""),
        ylab = paste("f_",
                     J, "(x_", J, ")", sep = ""),
        las = 3
      )
    }
    else if (class(X[, J]) == "numeric" | class(X[, J]) ==
             "integer") {
      z = c(min(X[, J]), as.numeric(quantile(X[, J][X[, J] < 1000], seq(
        1 / K,
        1, length.out = K
      ), type = 1)))
      z = unique(z)
      K = length(z) - 1
      fJ = numeric(K)
      a1 = as.numeric(cut(X[, J], breaks = z, include.lowest = TRUE))
      X1 = X
      X2 = X
      X1[, J] = z[a1]
      X2[, J] = z[a1 + 1]
      y.hat1 = pred.fun(X.model = X.model, newdata = X1)
      y.hat2 = pred.fun(X.model = X.model, newdata = X2)
      Delta = y.hat2 - y.hat1
      Delta = as.numeric(tapply(Delta, a1, mean))
      fJ = c(0, cumsum(Delta))
      b1 <- as.numeric(table(a1))
      fJ = fJ - sum((fJ[1:K] + fJ[2:(K + 1)]) / 2 * b1) / sum(b1)
      x <- z
    }
    else
      print("error:  class(X[,J]) must be either factor or numeric or integer")
  }
  else if (length(J) == 2) {
    if (class(X[, J[2]]) != "numeric" & class(X[, J[2]]) !=
        "integer") {
      print("error: X[,J[2]] must be numeric or integer. Only X[,J[1]] can be a factor")
    }
    if (class(X[, J[1]]) == "factor") {
      X[, J[1]] <- droplevels(X[, J[1]])
      x.count <- as.numeric(table(X[, J[1]]))
      x.prob <- x.count / sum(x.count)
      K1 <- nlevels(X[, J[1]])
      D.cum <- matrix(0, K1, K1)
      D <- matrix(0, K1, K1)
      for (j in setdiff(1:d, J[1])) {
        if (class(X[, j]) == "factor") {
          A = table(X[, J[1]], X[, j])
          A = A / x.count
          for (i in 1:(K1 - 1)) {
            for (k in (i + 1):K1) {
              D[i, k] = sum(abs(A[i,] - A[k,])) / 2
              D[k, i] = D[i, k]
            }
          }
          D.cum <- D.cum + D
        }
        else {
          q.x.all <- quantile(
            X[, j],
            probs = seq(0,
                        1, length.out = 100),
            na.rm = TRUE,
            names = FALSE
          )
          x.ecdf = tapply(X[, j], X[, J[1]], ecdf)
          for (i in 1:(K1 - 1)) {
            for (k in (i + 1):K1) {
              D[i, k] = max(abs(x.ecdf[[i]](q.x.all) -
                                  x.ecdf[[k]](q.x.all)))
              D[k, i] = D[i, k]
            }
          }
          D.cum <- D.cum + D
        }
      }
      D1D <- cmdscale(D.cum, k = 1)
      ind.ord <- sort(D1D, index.return = T)$ix
      ord.ind <- sort(ind.ord, index.return = T)$ix
      levs.orig <- levels(X[, J[1]])
      levs.ord <- levs.orig[ind.ord]
      x.ord <- ord.ind[as.numeric(X[, J[1]])]
      z2 = c(min(X[, J[2]]), as.numeric(quantile(X[, J[2]],
                                                 seq(
                                                   1 / K, 1, length.out = K
                                                 ), type = 1)))
      z2 = unique(z2)
      K2 = length(z2) - 1
      a2 = as.numeric(cut(X[, J[2]], breaks = z2, include.lowest = TRUE))
      row.ind.plus <- (1:N)[x.ord < K1]
      X11 = X
      X12 = X
      X21 = X
      X22 = X
      X11[row.ind.plus, J[2]] = z2[a2][row.ind.plus]
      X12[row.ind.plus, J[2]] = z2[a2 + 1][row.ind.plus]
      X21[row.ind.plus, J[1]] = levs.ord[x.ord[row.ind.plus] +
                                           1]
      X22[row.ind.plus, J[1]] = levs.ord[x.ord[row.ind.plus] +
                                           1]
      X21[row.ind.plus, J[2]] = z2[a2][row.ind.plus]
      X22[row.ind.plus, J[2]] = z2[a2 + 1][row.ind.plus]
      y.hat11 = pred.fun(X.model = X.model, newdata = X11[row.ind.plus,])
      y.hat12 = pred.fun(X.model = X.model, newdata = X12[row.ind.plus,])
      y.hat21 = pred.fun(X.model = X.model, newdata = X21[row.ind.plus,])
      y.hat22 = pred.fun(X.model = X.model, newdata = X22[row.ind.plus,])
      Delta.plus = (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
      row.ind.neg <- (1:N)[x.ord > 1]
      X11 = X
      X12 = X
      X21 = X
      X22 = X
      X11[row.ind.neg, J[1]] = levs.ord[x.ord[row.ind.neg] -
                                          1]
      X12[row.ind.neg, J[1]] = levs.ord[x.ord[row.ind.neg] -
                                          1]
      X11[row.ind.neg, J[2]] = z2[a2][row.ind.neg]
      X12[row.ind.neg, J[2]] = z2[a2 + 1][row.ind.neg]
      X21[row.ind.neg, J[2]] = z2[a2][row.ind.neg]
      X22[row.ind.neg, J[2]] = z2[a2 + 1][row.ind.neg]
      y.hat11 = pred.fun(X.model = X.model, newdata = X11[row.ind.neg,])
      y.hat12 = pred.fun(X.model = X.model, newdata = X12[row.ind.neg,])
      y.hat21 = pred.fun(X.model = X.model, newdata = X21[row.ind.neg,])
      y.hat22 = pred.fun(X.model = X.model, newdata = X22[row.ind.neg,])
      Delta.neg = (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
      Delta = as.matrix(tapply(c(Delta.plus, Delta.neg),
                               list(c(
                                 x.ord[row.ind.plus], x.ord[row.ind.neg] -
                                   1
                               ), a2[c(row.ind.plus, row.ind.neg)]), mean))
      NA.Delta = is.na(Delta)
      NA.ind = which(NA.Delta, arr.ind = T, useNames = F)
      if (nrow(NA.ind) > 0) {
        notNA.ind = which(!NA.Delta, arr.ind = T, useNames = F)
        range1 = K1 - 1
        range2 = max(z2[z2 < 1000]) - min(z2)
        Z.NA = cbind(NA.ind[, 1] / range1, (z2[NA.ind[,
                                                      2]] + z2[NA.ind[, 2] + 1]) /
                       2 / range2)
        Z.notNA = cbind(notNA.ind[, 1] / range1, (z2[notNA.ind[,
                                                               2]] + z2[notNA.ind[, 2] + 1]) /
                          2 / range2)
        nbrs <-
          ann(Z.notNA, Z.NA, k = 1, verbose = F)$knnIndexDist[,
                                                              1]
        Delta[NA.ind] = Delta[matrix(notNA.ind[nbrs,], ncol = 2)]
      }
      fJ = matrix(0, K1 - 1, K2)
      fJ = apply(t(apply(Delta, 1, cumsum)), 2, cumsum)
      fJ = rbind(rep(0, K2), fJ)
      fJ = cbind(rep(0, K1), fJ)
      b = as.matrix(table(x.ord, a2))
      b2 = apply(b, 2, sum)
      Delta = fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
      b.Delta = b * Delta
      Delta.Ave = apply(b.Delta, 2, sum) / b2
      fJ2 = c(0, cumsum(Delta.Ave))
      b.ave = matrix((b[1:(K1 - 1),] + b[2:K1,]) / 2,
                     K1 - 1, K2)
      b1 = apply(b.ave, 1, sum)
      Delta = matrix(fJ[2:K1,] - fJ[1:(K1 - 1),], K1 -
                       1, K2 + 1)
      b.Delta = matrix(b.ave * (Delta[, 1:K2] + Delta[,
                                                      2:(K2 + 1)]) / 2, K1 - 1, K2)
      Delta.Ave = apply(b.Delta, 1, sum) / b1
      fJ1 = c(0, cumsum(Delta.Ave))
      fJ = fJ - outer(fJ1, rep(1, K2 + 1)) - outer(rep(1,
                                                       K1), fJ2)
      fJ0 = sum(b * (fJ[, 1:K2] + fJ[, 2:(K2 + 1)]) / 2) / sum(b)
      fJ = fJ - fJ0
      x <- list(levs.ord, z2)
      K <- c(K1, K2)
      image(
        1:K1,
        x[[2]],
        fJ,
        xlab = paste("x_", J[1],
                     " (", names(X)[J[1]], ")", sep = ""),
        ylab = paste("x_",
                     J[2], " (", names(X)[J[2]], ")", sep = ""),
        ylim = range(z2),
        yaxs = "i"
      )
      contour(1:K1, x[[2]], fJ, add = TRUE, drawlabels = TRUE)
      axis(
        side = 1,
        labels = x[[1]],
        at = 1:K1,
        las = 3,
        padj = 1.2
      )
      if (NA.plot == FALSE) {
        if (nrow(NA.ind) > 0) {
          NA.ind = which(b == 0, arr.ind = T, useNames = F)
          rect(
            xleft = NA.ind[, 1] - 0.5,
            ybottom = z2[NA.ind[,
                                2]],
            xright = NA.ind[, 1] + 0.5,
            ytop = z2[NA.ind[,
                             2] + 1],
            col = "black"
          )
        }
      }
    }
    else if (class(X[, J[1]]) == "numeric" | class(X[, J[1]]) ==
             "integer") {
      z1 = c(min(X[, J[1]]), as.numeric(quantile(X[, J[1]],
                                                 seq(
                                                   1 / K, 1, length.out = K
                                                 ), type = 1)))
      z1 = unique(z1)
      K1 = length(z1) - 1
      a1 = as.numeric(cut(X[, J[1]], breaks = z1, include.lowest = TRUE))
      z2 = c(min(X[, J[2]]), as.numeric(quantile(X[, J[2]],
                                                 seq(
                                                   1 / K, 1, length.out = K
                                                 ), type = 1)))
      z2 = unique(z2)
      K2 = length(z2) - 1
      fJ = matrix(0, K1, K2)
      a2 = as.numeric(cut(X[, J[2]], breaks = z2, include.lowest = TRUE))
      X11 = X
      X12 = X
      X21 = X
      X22 = X
      X11[, J] = cbind(z1[a1], z2[a2])
      X12[, J] = cbind(z1[a1], z2[a2 + 1])
      X21[, J] = cbind(z1[a1 + 1], z2[a2])
      X22[, J] = cbind(z1[a1 + 1], z2[a2 + 1])
      y.hat11 = pred.fun(X.model = X.model, newdata = X11)
      y.hat12 = pred.fun(X.model = X.model, newdata = X12)
      y.hat21 = pred.fun(X.model = X.model, newdata = X21)
      y.hat22 = pred.fun(X.model = X.model, newdata = X22)
      Delta = (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
      Delta = as.matrix(tapply(Delta, list(a1, a2), mean))
      NA.Delta = is.na(Delta)
      NA.ind = which(NA.Delta, arr.ind = T, useNames = F)
      if (nrow(NA.ind) > 0) {
        notNA.ind = which(!NA.Delta, arr.ind = T, useNames = F)
        range1 = max(z1[z1 < 1000]) - min(z1)
        range2 = max(z2[z2 < 1000]) - min(z2)
        Z.NA = cbind((z1[NA.ind[, 1]] + z1[NA.ind[,
                                                  1] + 1]) / 2 / range1, (z2[NA.ind[, 2]] + z2[NA.ind[,
                                                                                                      2] + 1]) /
                       2 / range2)
        Z.notNA = cbind((z1[notNA.ind[, 1]] + z1[notNA.ind[,
                                                           1] + 1]) / 2 /
                          range1, (z2[notNA.ind[, 2]] + z2[notNA.ind[,
                                                                     2] + 1]) /
                          2 / range2)
        nbrs <-
          ann(Z.notNA, Z.NA, k = 1, verbose = F)$knnIndexDist[,
                                                              1]
        Delta[NA.ind] = Delta[matrix(notNA.ind[nbrs,], ncol = 2)]
      }
      fJ = apply(t(apply(Delta, 1, cumsum)), 2, cumsum)
      fJ = rbind(rep(0, K2), fJ)
      fJ = cbind(rep(0, K1 + 1), fJ)
      b = as.matrix(table(a1, a2))
      b1 = apply(b, 1, sum)
      b2 = apply(b, 2, sum)
      Delta = fJ[2:(K1 + 1),] - fJ[1:K1,]
      b.Delta = b * (Delta[, 1:K2] + Delta[, 2:(K2 + 1)]) / 2
      Delta.Ave = apply(b.Delta, 1, sum) / b1
      fJ1 = c(0, cumsum(Delta.Ave))
      Delta = fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
      b.Delta = b * (Delta[1:K1,] + Delta[2:(K1 + 1),]) / 2
      Delta.Ave = apply(b.Delta, 2, sum) / b2
      fJ2 = c(0, cumsum(Delta.Ave))
      fJ = fJ - outer(fJ1, rep(1, K2 + 1)) - outer(rep(1,
                                                       K1 + 1), fJ2)
      fJ0 = sum(b * (fJ[1:K1, 1:K2] + fJ[1:K1, 2:(K2 +
                                                    1)] + fJ[2:(K1 + 1), 1:K2] + fJ[2:(K1 + 1),
                                                                                    2:(K2 + 1)]) /
                  4) / sum(b)
      fJ = fJ - fJ0
      x <- list(z1, z2)
      K <- c(K1, K2)
      image(
        x[[1]],
        x[[2]],
        fJ,
        xlab = paste("x_", J[1],
                     " (", names(X)[J[1]], ")", sep = ""),
        ylab = paste("x_",
                     J[2], " (", names(X)[J[2]], ")", sep = ""),
        xlim = range(z1),
        ylim = range(z2),
        xaxs = "i",
        yaxs = "i"
      )
      contour(x[[1]], x[[2]], fJ, add = TRUE, drawlabels = TRUE)
      if (NA.plot == FALSE) {
        if (nrow(NA.ind) > 0) {
          rect(
            xleft = z1[NA.ind[, 1]],
            ybottom = z2[NA.ind[,
                                2]],
            xright = z1[NA.ind[, 1] + 1],
            ytop = z2[NA.ind[,
                             2] + 1],
            col = "black"
          )
        }
      }
    }
    else
      print("error:  class(X[,J[1]]) must be either factor or numeric/integer")
  }
  else
    print("error:  J must be a vector of length one or two")
  list(K = K,
       x.values = x,
       f.values = fJ)
}
