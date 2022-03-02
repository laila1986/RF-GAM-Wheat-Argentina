scale_data <- function(data) {
  scaled_data <-
    lapply(
      names(data),
      function(x) {
        min_x <- data[, ..x] %>% min()
        max_x <- data[, ..x] %>% max()
      }
    ) %>%
    reduce(cbind)

  return(scaled_data)
}