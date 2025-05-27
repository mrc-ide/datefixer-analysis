library(tidyverse)

extract_est_df <- function(MCMCres, theta_true = NULL) {
  iterations <- seq_along(MCMCres$theta_chain)
  df_list <- list()
  
  for (group_idx in seq_along(MCMCres$theta_chain[[1]]$mu)) {
    n_dates <- length(MCMCres$theta_chain[[1]]$mu[[group_idx]])
    
    for (j in 1:n_dates) {
      mu_vals <- sapply(MCMCres$theta_chain, function(tc) tc$mu[[group_idx]][j])
      cv_vals <- sapply(MCMCres$theta_chain, function(tc) tc$CV[[group_idx]][j])
      df_tmp <- data.frame(
        iteration = iterations,
        mu = mu_vals,
        cv = cv_vals,
        group = paste0("group_", group_idx),
        date_index = j
        )
      df_list[[length(df_list) + 1]] <- df_tmp
    }
  }
  
  est_df <- bind_rows(df_list)
  
  # Add true values for reference
  if (!is.null(theta_true)) {
    true_df <- lapply(seq_along(theta_true$mu), function(group_idx) {
      n_dates <- length(theta_true$mu[[group_idx]])
      data.frame(
        group = paste0("group_", group_idx),
        date_index = 1:n_dates,
        true_mu = theta_true$mu[[group_idx]],
        true_cv = theta_true$CV[[group_idx]]
        )
      }) %>%
      bind_rows()
    
    est_df <- left_join(est_df, true_df, by = c("group", "date_index"))
    }
  }
