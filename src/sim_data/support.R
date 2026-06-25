simulate_true_data <- function(date_params, nsims) {
  sim1 <- function() {
    chronofix::chronofix_simulate_true_data(
      date_params$n_per_group,
      date_params$group_names,
      date_params$delay_info,
      date_params$date_range
    )
  }
  
  replicate(nsims, sim1(), simplify = FALSE)
}
