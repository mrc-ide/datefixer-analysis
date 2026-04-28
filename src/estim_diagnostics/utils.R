library(tidyverse)

extract_est_df <- function(MCMCres,
                           theta_true = NULL,
                           true_dat = NULL,
                           index_dates = NULL) {
  
  iterations <- seq_along(MCMCres$theta_chain)
  draws_list <- list()
  
  for (group_idx in seq_along(MCMCres$theta_chain[[1]]$mu)) {
    n_delays <- length(MCMCres$theta_chain[[1]]$mu[[group_idx]])
    
    for (j in seq_len(n_delays)) {
      mu_vals <- sapply(MCMCres$theta_chain, function(tc) tc$mu[[group_idx]][j])
      cv_vals <- sapply(MCMCres$theta_chain, function(tc) tc$CV[[group_idx]][j])
      
      draws_list[[length(draws_list) + 1]] <-
        tibble(iteration   = iterations,
               mu          = mu_vals,
               cv          = cv_vals,
               group       = paste0("group_", group_idx),
               delay_index = j)
    }
  }
  
  est_draws <- bind_rows(draws_list)
  
  # Posterior summaries
  est_quant <- est_draws %>%
    group_by(group, delay_index) %>%
    summarise(
      mu_mean = mean(mu),
      cv_mean = mean(cv),
      
      mu_median = median(mu),
      cv_median = median(cv),
      
      mu_lower95 = quantile(mu, 0.025),
      mu_upper95 = quantile(mu, 0.975),
      cv_lower95 = quantile(cv, 0.025),
      cv_upper95 = quantile(cv, 0.975),
      
      mu_lower50 = quantile(mu, 0.25),
      mu_upper50 = quantile(mu, 0.75),
      cv_lower50 = quantile(cv, 0.25),
      cv_upper50 = quantile(cv, 0.75),
      
      .groups = "drop"
    )
  
  # Compare to theta_true (true parameters) ----------------------------------
  if (!is.null(theta_true)) {
    true_df <- imap(theta_true$mu, \(mu_vec, g) {
      tibble(
        group       = paste0("group_", g),
        delay_index = seq_along(mu_vec),
        true_mu     = mu_vec,
        true_cv     = theta_true$CV[[g]]
      )
    }) |> list_rbind()
    
    est_quant <- est_quant %>%
      left_join(true_df, by = c("group", "delay_index")) %>%
      mutate(
        mu_bias = mu_mean - true_mu,
        cv_bias = cv_mean - true_cv,
        mu_cov95 = between(true_mu, mu_lower95, mu_upper95),
        cv_cov95 = between(true_cv, cv_lower95, cv_upper95),
        mu_cov50 = between(true_mu, mu_lower50, mu_upper50),
        cv_cov50 = between(true_cv, cv_lower50, cv_upper50),
        mu_width95 = mu_upper95 - mu_lower95,
        mu_width50 = mu_upper50 - mu_lower50,
        cv_width95 = cv_upper95 - cv_lower95,
        cv_width50 = cv_upper50 - cv_lower50
      )
  }
  
  # Compare to simulated empirical delays (true_dat) -------------------------
  if (!is.null(true_dat) && !is.null(index_dates)) {
    emp_df <- map_dfr(seq_along(true_dat), function(g) {
      delays_mat <- true_dat[[g]]
      idx_mat    <- index_dates[[g]]
      n_delays   <- ncol(idx_mat)
      
      map_dfr(seq_len(n_delays), function(d) {
        idx_start <- idx_mat[1, d]
        idx_end   <- idx_mat[2, d]
        delays <- delays_mat[, idx_end] - delays_mat[, idx_start]
        
        tibble(
          group = paste0("group_", g),
          delay_index = d,
          mu_empirical = mean(delays),
          cv_empirical = sd(delays) / mean(delays)
        )
      })
    })
    
    est_quant <- est_quant %>%
      left_join(emp_df, by = c("group", "delay_index")) %>%
      mutate(
        mu_bias_emp = mu_mean - mu_empirical,
        cv_bias_emp = cv_mean - cv_empirical,
        mu_cov95_emp = between(mu_empirical, mu_lower95, mu_upper95),
        cv_cov95_emp = between(cv_empirical, cv_lower95, cv_upper95),
        mu_cov50_emp = between(mu_empirical, mu_lower50, mu_upper50),
        cv_cov50_emp = between(cv_empirical, cv_lower50, cv_upper50)
      )
  }
    
  list(
    draws   = est_draws,
    summary = est_quant
  )
}


## Extract zeta

extract_zeta_df <- function(MCMCres, zeta_true = NULL) {
  
  iterations <- seq_along(MCMCres$theta_chain)
  
  # one draw per iteration
  zeta_vals  <- vapply(MCMCres$theta_chain,
                       function(tc) tc$zeta,
                       numeric(1))
  
  est_draws  <- tibble(
    iteration = iterations,
    zeta      = zeta_vals
  )
  
  est_quant <- est_draws %>%
    summarise(
      zeta_mean     = mean(zeta),
      zeta_median   = median(zeta),
      zeta_lower95  = quantile(zeta, 0.025),
      zeta_upper95  = quantile(zeta, 0.975),
      zeta_lower50  = quantile(zeta, 0.25),
      zeta_upper50  = quantile(zeta, 0.75)
    )
  
  if (!is.null(zeta_true)) {
    est_quant <- est_quant %>%
      mutate(
        true_zeta  = zeta_true,
        zeta_cov95 = between(true_zeta, zeta_lower95, zeta_upper95),
        zeta_cov50 = between(true_zeta, zeta_lower50, zeta_upper50)
      )
  }
  
  list(
    draws    = est_draws,
    quantile = est_quant
  )
}
