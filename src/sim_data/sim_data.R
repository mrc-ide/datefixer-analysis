# Simulate data for each simulation scenario

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

library(orderly2)
library(MixDiff)

## Number of data sets to simulate for each scenario
pars <- orderly_parameters(nsims = NULL)

orderly_dependency("sim_params", "latest", files = "sim_params.rds")

orderly_artefact(description = "Simulated Data", files = "sim_data.rds")

# Load all simulation parameters
all_params <- readRDS("sim_params.rds")

# Taken DiscrGamma function from MixDiff/R/LikelihoodPrior.R
DiscrGamma <- function (k, mu, CV = NULL, sigma = mu*CV, log=TRUE) 
{
  if (!is.null(CV)) {
    if(CV < 0)
      stop("CV must be >=0.")
  }
  if (sigma < 0) {
    stop("sigma must be >=0.")
  }
  shape <- (mu/sigma)^2
  rate <- mu/(sigma^2)
  
  res <- (k + 1) * pgamma(k + 1, shape, rate) +
    (k - 1) * pgamma(k - 1, shape, rate) -
    2 * k * pgamma(k, shape, rate)
  
  res <- res +
    (shape / rate) * (
      2 * pgamma(k, shape + 1, rate) -
        pgamma(k - 1, shape + 1, rate) -
        pgamma(k + 1, shape + 1, rate)
      )
  
  res <- pmax(0, res)
  
  return(if(log) log(res) else res)
}

# Edit original simul_true_data to change discretisation
simul_true_data_alt <- function(
    theta,
    n_per_group,
    range_dates,
    index_dates,
    simul_error = FALSE,
    remove_allNA_indiv = FALSE,
    discretise_method = c("round", "discr_gamma")
    ) {
  
  discretise_method <- match.arg(discretise_method)
  D <- list()
  
  for (g in seq_along(theta$mu)) {
    
    # Simulate 20% more per group than needed in case of NA rows
    extra_rows <- n_per_group[g] * 1.2
    
    D[[g]] <- matrix(NA, extra_rows, length(theta$mu[[g]]) + 1)
    D[[g]][, 1] <- sample(seq(range_dates[1], range_dates[2], 1), extra_rows, replace = TRUE)
    
    for (j in seq_len(ncol(index_dates[[g]]))) {
      mu <- theta$mu[[g]][j]
      CV <- theta$CV[[g]][j]
      
      if (discretise_method == "round") {
        params <- find_params_gamma(mu, CV)
        delay <- round(rgamma(extra_rows, shape = params[1], scale = params[2]))
        
      } else if (discretise_method == "discr_gamma") {
        delay <- discr_gamma_sample(extra_rows, mu, CV)
      }
      
      D[[g]][, index_dates[[g]][2, j]] <- D[[g]][, index_dates[[g]][1, j]] + delay
    }
  }
  
  if (simul_error) {
    # Add remove_allNA_indiv = TRUE here to remove rows with only NAs:
    observed_D <- simul_obs_dat_alt(D, theta, range_dates, remove_allNA_indiv = TRUE, n_group = n_per_group)
    return(list(true_dat = observed_D$true_dat, obs_dat = observed_D$obs_dat, E = observed_D$E))
  } else {
    return(list(true_dat = D, obs_dat = NULL, E = NULL))
  }
  
}

# Add function discr_gamma_sample to simulate n delay values
discr_gamma_sample <- function(n, mu, CV) {
  # convert to gamma params:
  shape <- (mu / (mu * CV))^2
  rate <- mu / (mu * CV)^2
  # range of possible ks (99.9th percentile)
  k_max <- ceiling(qgamma(0.999, shape = shape, rate = rate))
  ks <- 0:k_max
  # compute probabilities using function taken from LikelihoodPrior (above):
  probs <- pmax(0, exp(DiscrGamma(ks, mu, CV, log = TRUE)))
  probs <- probs / sum(probs)
  # sample ks using probabilities:
  sample(ks, size = n, replace = TRUE, prob = probs)
}

# Edit simul_obs_dat (copied over from simulfunctions.R mixdiff) to select sims 
simul_obs_dat_alt <- function(D, theta, range_dates, remove_allNA_indiv = FALSE, n_group)
{
  E <- D
  obs_dat <- D
  for(g in seq_len(length(D)) )
  {
    for(j in seq_len(ncol(D[[g]])) )
    {
      E[[g]][,j] <- sample(
        c(-1, 1, 0), nrow(D[[g]]), replace = TRUE,
        prob = c(
          theta$prop_missing_data,
          (1 - theta$prop_missing_data) * theta$zeta,
          (1 - theta$prop_missing_data) * (1 - theta$zeta))
        )
      obs_dat[[g]][E[[g]][,j] == -1, j]  <- NA
      obs_dat[[g]][E[[g]][,j] == 0, j]  <- D[[g]][E[[g]][,j] == 0,j]
      obs_dat[[g]][E[[g]][,j] == 1, j]  <- sample(seq(range_dates[1], range_dates[2], 1), sum(E[[g]][,j] == 1), replace = TRUE) # need to update if change error model
    }
    if(remove_allNA_indiv)
    {
      exclude <- which(rowSums(is.na(obs_dat[[g]])) == ncol(obs_dat[[g]]))
      if(length(exclude) > 0)
      {
        # remove corresponding true_dat for excluded rows
        obs_dat[[g]] <- obs_dat[[g]][-exclude,]
        E[[g]] <- E[[g]][-exclude,]
        D[[g]] <- D[[g]][-exclude,]
      }
    }
    # remove extra rows: select nrow == n_per_group
    if(nrow(obs_dat[[g]]) > n_group[g]) {
      obs_dat[[g]] <- obs_dat[[g]][1:n_group[g],]
      E[[g]] <- E[[g]][1:n_group[g],]
      D[[g]] <- D[[g]][1:n_group[g],]
    }
  }
  return(list(true_dat = D, obs_dat = obs_dat, E = E))
}

# Simulate
simulate_scenario <- function(sim_params, nsims) {
  replicate(nsims, {
    simul_true_data_alt(
      sim_params$theta,
      sim_params$n_per_group,
      sim_params$range_dates,
      sim_params$index_dates,
      simul_error = TRUE,
      remove_allNA_indiv = TRUE,
      discretise_method = "discr_gamma"
    )
  }, simplify = FALSE)
}

# Named list of simulated data
sim_data_all <- lapply(all_params, simulate_scenario, nsims = nsims)

saveRDS(sim_data_all, "sim_data.rds")

