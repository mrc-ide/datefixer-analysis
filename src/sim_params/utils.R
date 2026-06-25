# Function to create sim_params for all scenarios

build_date_params <- function(
    date_model,
    n_per_group = rep(100, 4),
    group_names = c("community-alive", "community-dead",
                    "hospitalised-alive", "hospitalised-dead"),
    mean_scale = 1,
    cv_scale = 1,
    delay_distribution = "gamma"
) {
  
  date_range <- as.integer(as.Date(c("2014-01-01", "2015-01-01")))
  
  delay_info <- data.frame(
    from = c("onset", "onset", "onset", "onset", "onset", "onset",
             "hospitalisation", "onset", "hospitalisation"),
    to = c("report", "report", "report", "report", "death", "hospitalisation",
           "discharge", "hospitalisation", "death"),
    group = c("community-alive", "community-dead", "hospitalised-alive",
              "hospitalised-dead", "community-dead", "hospitalised-alive",
              "hospitalised-alive", "hospitalised-dead", "hospitalised-dead"),
    distribution = delay_distribution,
    mean = c(5, 6, 8, 11, 7, 9, 10, 12, 13),
    cv = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  )
  
  delay_info$mean <- delay_info$mean * mean_scale
  delay_info$cv <- delay_info$cv * cv_scale
  
  list(
    date_model = date_model,
    n_per_group = n_per_group,
    group_names = group_names,
    delay_info = delay_info,
    date_range = date_range
  )
}
