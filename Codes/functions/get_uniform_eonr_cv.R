# /*===========================================================
#' # Get uniform EONR for each of the 10 folds
# /*===========================================================

get_uniform_eonr_cv <- function(split_data, x_vars, eonr_data_rf, eonr_data_gam, id_field) {
  print(paste0("working on field ", id_field))

  uniform_eonr <-
    split_data %>%
    rowwise() %>%
    mutate(test_data = list(
      assessment(splits)
    )) %>%
    # mutate(reg_formula = list(
    #   paste0(
    #     "yield ~ s(nrate, bs = 'cv') +",
    #     paste0("s(", x_vars, ", k = 3)", collapse = "+")
    #   ) %>%
    #     formula()
    # )) %>%
    # mutate(reg_formula = list(
    #   paste0(
    #     "yield ~ s(nrate, k = 3) +",
    #     paste0("s(", x_vars, ", k = 3)", collapse = "+")
    #   ) %>%
    #     formula()
    # )) %>%
    mutate(reg_formula = list(
      paste0(
        "yield ~ nrate + I(nrate^2) + ",
        paste0(x_vars, collapse = "+")
      ) %>%
        formula()
    )) %>%
    mutate(model_trained = list(
      # scam::scam(reg_formula, data = test_data)
      # gam(reg_formula, data = test_data)
      lm(reg_formula, data = test_data)
    )) %>%
    mutate(n_seq = list(
      test_data[, seq(min(nrate), max(nrate), by = 3)]
    )) %>%
    mutate(
      u_eonr =
        copy(test_data) %>%
          .[, nrate := NULL] %>%
          expand_grid_df(., data.table(nrate = n_seq)) %>%
          .[, y_hat := predict(model_trained, newdata = .)] %>%
          .[, pi_hat := Pw * y_hat - Pn * nrate] %>%
          .[, .(mean_pi_hat = mean(pi_hat)), by = nrate] %>%
          .[, .SD[which.max(mean_pi_hat), ]] %>%
          .[, nrate]
    ) %>%
    dplyr::select(-model_trained) %>%
    mutate(eonr_rf = list(
      eonr_data_rf[test_data, on = "obs_id"] %>%
        .[, .(obs_id, nrate)] %>%
        setnames("nrate", "eonr_rf")
    )) %>%
    mutate(eonr_gam = list(
      eonr_data_gam[test_data, on = "obs_id"] %>%
        .[, .(obs_id, nrate)] %>%
        setnames("nrate", "eonr_gam")
    )) %>%
    mutate(eonr_rf_gam = list(
      eonr_gam[eonr_rf, on = "obs_id"]
    )) %>%
    dplyr::select(id, u_eonr, eonr_rf_gam) %>%
    unnest(eonr_rf_gam) %>%
    data.table()

  return(uniform_eonr)
}