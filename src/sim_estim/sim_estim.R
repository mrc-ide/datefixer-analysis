library(orderly)
library(chronofix)

orderly_shared_resource("util.R")
source("util.R")

version_check("chronofix", "0.0.5")

pars <- orderly_parameters(scenario = "baseline",
                           dataset = 1)

scenario <- pars$scenario
dataset <- pars$dataset

n_steps <- 10000
burnin <- 5000
thinning_factor <- 20

orderly_dependency("sim_params", "latest", 
                   c("date_params.rds",
                     "scenarios.rds"))
data_filename <- paste0("outputs/sim_data_", scenario, "_", dataset, ".rds")
orderly_dependency("sim_data", "latest", 
                   c("sim_data.rds" = data_filename))

orderly_artefact(description = "MCMC outputs for simulation scenarios",
                 files = "sim_estim.rds")



# Read in dependencies --------------------------------------------------------

date_params <- readRDS("date_params.rds")
scenarios <- readRDS("scenarios.rds")
sim_data <- readRDS("sim_data.rds")

# MCMC settings ---------------------------------------------------------------

control <- chronofix_mcmc_control(n_steps = n_steps,
                                  burnin = burnin,
                                  thinning_factor = thinning_factor,
                                  n_chains = 4,
                                  parallel = TRUE,
                                  n_workers = 4,
                                  earliest_possible_date = "2014-01-01",
                                  latest_possible_date = "2015-01-01",
                                  cascade_sampling = TRUE,
                                  prob_update_estimated_dates = 0.1,
                                  prob_update_error_indicators = 0.1)
sampler <- chronofix_sampler(control)
hyperparameters <- chronofix_hyperparameters(
  gamma_shape_prior_shape = 1,
  gamma_shape_prior_rate = 0.1,
  gamma_mean_prior_shape = 2,
  gamma_mean_prior_scale = 10
)

# Run MCMC -------------------------------------------------------------------

date_model <- scenarios[[scenario]]$date_model
delay_info <- date_params[[date_model]]$delay_info

model <- chronofix_model(sim_data$observed_data, delay_info,
                         hyperparameters, control)
res <- chronofix_mcmc_run(model, sampler, control = control)

nms <- rownames(res$initial)

if (scenario == "lognormal_delays") {
  i_meanlog <- which(endsWith(nms, "meanlog"))
  i_precisionlog <- which(endsWith(nms, "precisionlog"))
  
  initial_mean <- 
    exp(res$initial[i_meanlog, ] + 1 / (2 *res$initial[i_precisionlog, ]))
  initial_cv <- sqrt(exp(1 / res$initial[i_precisionlog, ]) - 1)
  
  res$initial[i_meanlog, ] <- initial_mean
  res$initial[i_precisionlog, ] <- initial_cv
  
  pars_mean <- 
    exp(res$pars[i_meanlog, , ] + 1 / (2 *res$pars[i_precisionlog, , ]))
  pars_cv <- sqrt(exp(1 / res$pars[i_precisionlog, , ]) - 1)
  
  res$pars[i_meanlog, , ] <- pars_mean
  res$pars[i_precisionlog, , ] <- pars_cv
  
  res$full_chains$initial[i_meanlog, ] <- initial_mean
  res$full_chains$initial[i_precisionlog, ] <- initial_cv
  
  full_pars_mean <- 
    exp(res$full_chains$pars[i_meanlog, , ] + 
          1 / (2 *res$full_chains$pars[i_precisionlog, , ]))
  full_pars_cv <- sqrt(exp(1 / res$full_chains$pars[i_precisionlog, , ]) - 1)
  
  res$full_chains$pars[i_meanlog, , ] <- pars_mean
  res$full_chains$pars[i_precisionlog, , ] <- pars_cv
  
  nms[i_meanlog] <- paste0("delay_mean", seq_along(i_meanlog))
  nms[i_precisionlog] <- paste0("delay_cv", seq_along(i_precisionlog))
} else {
  i_shape <- which(endsWith(nms, "shape"))
  i_mean <- which(endsWith(nms, "mean"))
  
  res$initial[i_shape, ] <- 1 / sqrt(res$initial[i_shape, ])
  res$pars[i_shape, , ] <- 1 / sqrt(res$pars[i_shape, , ])
  
  res$full_chains$initial[i_shape, ] <- 1 / sqrt(res$full_chains$initial[i_shape, ])
  res$full_chains$pars[i_shape, , ] <- 1 / sqrt(res$full_chains$pars[i_shape, , ])
  
  nms[i_shape] <- paste0("delay_cv", seq_along(i_shape))
  nms[i_mean] <- paste0("delay_mean", seq_along(i_shape))
}


rownames(res$initial) <- nms
rownames(res$pars) <- nms
rownames(res$full_chains$initial) <- nms
rownames(res$full_chains$pars) <- nms

saveRDS(res, "sim_estim.rds")
