# /*===========================================================
#' # Run spatial CV
# /*===========================================================

# train_data <- cv_results$train_data[[1]]
# gam_res <- gam(
#   formula(gam_formula),
#   data = train_data,
#   method = "REML"
# )
# predict(gam_res, newdata = train_data)

run_sptial_cv_gam <- function(split_data, gam_formula) {

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
      gam(
        formula(gam_formula),
        data = train_data,
        method = "REML"
      )
    )) %>%
    mutate(test_data = list(
      mutate(
        test_data,
        y_hat = predict(trained_model, newdata = test_data)
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