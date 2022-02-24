
get_unique_permutations <- function(x_vars_to_keep) {
  max_k <- length(x_vars_to_keep)
  num_comb_ls <- c(seq(3, max_k, by = 3), max_k - 1) %>% unique()
  var_permutations <-
    lapply(
      num_comb_ls,
      function(x) get_permutations_indiv(x_vars_to_keep, num_comb = x)
    ) %>%
    rbindlist()
  return(var_permutations)
}

get_permutations_indiv <- function(x_vars_to_keep, num_comb) {
  all_permutations <-
    permutations(
      n = length(x_vars_to_keep),
      r = num_comb,
      v = x_vars_to_keep
    )

  unique_permutations <-
    lapply(
      1:nrow(all_permutations),
      function(x) {
        tibble(
          x_vars = list(all_permutations[x, ]),
          x_comb = paste0(all_permutations[x, ] %>% .[order(.)], collapse = "+")
        )
      }
    ) %>%
    rbindlist() %>%
    unique(by = "x_comb") %>%
    .[, .(x_vars)]

  return(unique_permutations)
}


