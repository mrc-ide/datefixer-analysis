# Simulate data for each simulation scenario

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

library(orderly)
library(chronofix)

## Number of data sets to simulate for each scenario
pars <- orderly_parameters(nsims = NULL)
orderly_dependency("sim_params", "latest", 
                   files = c("date_params.rds",
                             "error_params.rds",
                             "scenarios.rds"))

orderly_resource("support.R")
source("support.R")

dir.create("outputs")

# Load all simulation parameters
date_params <- readRDS("date_params.rds")
error_params <- readRDS("error_params.rds")
scenarios <- readRDS("scenarios.rds")

set.seed(1)

# Simulate true data

true_data <- lapply(date_params, simulate_true_data, nsims = pars$nsims)

for (nm_scenario in names(scenarios)) {
  
  for (i in seq_len(pars$nsims)) {
    filename <- paste0("outputs/sim_data", "_", nm_scenario, "_", i, ".rds")
    orderly_artefact(description = "Simulated Data", 
                     files = filename)
    
    scenario <- scenarios[[nm_scenario]]
    
    res <- chronofix_simulate_observation_errors(
      true_data[[scenario$date_model]][[i]],
      error_params[[scenario$error_model]],
      date_params[[scenario$date_model]]$date_range
    )
    saveRDS(res, filename)
  }
}