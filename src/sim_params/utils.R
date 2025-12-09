# Function to create sim_params for all scenarios

build_sim_params <- function(
    scenario_id,
    n_per_group = rep(100, 4),
    mean_scale = 1,
    cv_scale = 1,
    delay_dist = "gamma",
    prop_missing_data = 0.2,
    prob_error = 0.05,
    error_model = "naive"
) {
  
  error_params <- list(prop_missing_data = prop_missing_data,
                       prob_error = prob_error,
                       error_model = error_model)
  
  date_range <- as.integer(as.Date(c("2014-01-01", "2015-01-01")))
  
  delay_map <- data.frame(
    from = c("onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = I(list(1:4, 2, 3, 3, 4, 4))
  )
  
  delay_params <- data.frame(
    group = c(1:4, 2, 3, 3, 4, 4),
    from = c("onset", "onset", "onset", "onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "report", "report", "report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    delay_dist = delay_dist,
    delay_mean = c(5, 6, 7, 8, 9, 10, 11, 12, 13),
    delay_cv = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  )
  
  delay_params$delay_mean <- delay_params$delay_mean * mean_scale
  delay_params$delay_cv <- delay_params$delay_cv * cv_scale
  
  list(
    scenario_id = scenario_id,
    n_per_group = n_per_group,
    delay_map = delay_map,
    delay_params = delay_params,
    error_params = error_params,
    date_range = date_range
  )
}
