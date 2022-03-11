# /*===========================================================
#' # Run spatial CV
# /*===========================================================

# id <- 1
# x <- 1
# split_data <- RF_sp_cv$split_data[[id]]
# tune_pars <- RF_sp_cv$tune_pars[[id]]
# mtry <- tune_pars[x, mtry]
# min.node.size <- tune_pars[x, min.node.size]
# sample.fraction <- tune_pars[x, sample.fraction]
# x_vars <- c("nrate", tune_pars[x, vars_set][[1]])

run_sptial_cv_rf <- function(split_data, formula, mtry, min.node.size, sample.fraction) {

  # trained_model <- cv_results$trained_model[[1]]
  # test_data <- cv_results$test_data[[1]]
  # temp <- predict(trained_model, data = test_data)$predictions

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
      ranger(
        formula = formula,
        data = train_data,
        mtry = mtry,
        min.node.size = min.node.size,
        sample.fraction = sample.fraction,
        num.threads = 1,
        num.trees = 500
      )
    )) %>%
    mutate(test_data = list(
      mutate(
        test_data,
        y_hat = predict(
          trained_model,
          data = test_data
        )$predictions
      )
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