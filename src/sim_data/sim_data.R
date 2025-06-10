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

simulate_scenario <- function(sim_params, nsims) {
  replicate(nsims, {
    simul_true_data(
      sim_params$theta,
      sim_params$n_per_group,
      sim_params$range_dates,
      sim_params$index_dates,
      simul_error = TRUE,
      remove_allNA_indiv = TRUE
    )
  }, simplify = FALSE)
}

# Named list of simulated data
sim_data_all <- lapply(all_params, simulate_scenario, nsims = nsims)

saveRDS(sim_data_all, "sim_data.rds")

