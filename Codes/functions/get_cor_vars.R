
find_vars_to_keep <- function(data) {
  tmp <- cor(data)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0

  data_new <- data[, apply(tmp, 2, function(x) all(abs(x) < 0.8, na.rm = TRUE))]

  keep_vars <- names(data_new)[which(data_new)]

  return(keep_vars)
}