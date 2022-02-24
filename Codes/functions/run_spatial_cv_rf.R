# /*===========================================================
#' # Run spatial CV
# /*===========================================================

run_sptial_cv_rf <- function(split_data, mtry, min.node.size, sample.fraction, x_vars) {
  cv_results <-
    split_data %>%
    rowwise() %>%
    mutate(train_data = list(
      analysis(splits)
    )) %>%
    mutate(test_data = list(
      assessment(splits)
    )) %>%
    mutate(trained_model = list(
      regression_forest(
        X = train_data[, ..x_vars],
        Y = train_data[, yield],
        mtry = mtry,
        min.node.size = min.node.size,
        sample.fraction = sample.fraction,
        num.threads = 1,
        num.trees = 1000
      )
    )) %>%
    mutate(test_data = list(
      mutate(test_data, y_hat = predict(trained_model, newdata = test_data[, ..x_vars])$predictions)
    )) %>%
    mutate(perf_measures = list(
      data.table(
        rmse_cv = test_data[, (yield - y_hat)^2 %>% mean() %>% sqrt()],
        r2 = lm(yield ~ y_hat, data = test_data) %>% summary() %>% .$r.squared
      )
    )) %>%
    pull(perf_measures) %>%
    rbindlist() %>%
    .[, .(rmse_cv = mean(rmse_cv), r2 = mean(r2))]

  return(cv_results)
}