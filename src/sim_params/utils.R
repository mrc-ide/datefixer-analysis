# Function to create sim_params for all scenarios

build_sim_params <- function(
    scenario_id,
    n_per_group = rep(100, 4),
    mu_scale = 1,
    cv_scale = 1,
    delay_distribution = "gamma",
    prop_missing_data = 0.2,
    zeta = 0.05,
    error_model = "typo"
) {
  
  n_groups <- 4
  n_dates <- c(2, 3, 4, 4)
  
  mu_baseline <- list(5, c(6, 7), c(8, 9, 10), c(11, 12, 13))
  cv_baseline <- list(0.5, c(0.5, 0.5), c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5))
  
  mu_scaled <- lapply(mu_baseline, function(x) x * mu_scale)
  cv_scaled <- lapply(cv_baseline, function(x) x * cv_scale)
  
  theta <- list(
    prop_missing_data = prop_missing_data,
    zeta = zeta,
    mu = mu_scaled,
    CV = cv_scaled
  )
  
  range_dates <- date_to_int(
    c(
      as.Date("01/01/2014", "%d/%m/%Y"),
      as.Date("01/01/2015", "%d/%m/%Y")
    )
  )
  
  index_dates <- list(
    matrix(c(1, 2), nrow = 2),
    cbind(c(1, 2), c(1, 3)),
    cbind(c(1, 2), c(2, 3),c(1, 4)),
    cbind(c(1, 2), c(2, 3), c(1, 4))
  )
  
  index_dates_order <- list(
    matrix(c(1, 2), nrow = 2),
    cbind(c(1, 2), c(1, 3)),
    cbind(c(1, 2), c(2, 3), c(1, 3), c(1, 4)),
    cbind(c(1, 2), c(2, 3), c(1, 3), c(1, 4))
  )
  
  list(
    scenario_id = scenario_id,
    n_per_group = n_per_group,
    delay_distribution = delay_distribution,
    error_model = error_model,
    theta = theta,
    range_dates = range_dates,
    index_dates = index_dates,
    index_dates_order = index_dates_order
  )
}
