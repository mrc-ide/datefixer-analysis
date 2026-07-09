# Simulate data for each simulation scenario

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

library(orderly)
library(datefixer)
library(parallel)

## Number of data sets to simulate for each scenario
pars <- orderly_parameters(nsims = NULL)
orderly_dependency("sim_params", "latest", files = "sim_params.rds")
orderly_artefact(description = "Simulated Data", files = "sim_data.rds")

# Load all simulation parameters
all_params <- readRDS("sim_params.rds")

# Simulate
simulate_scenario <- function(sim_params, nsims) {
  
  cl <- parallel::getDefaultCluster()
  parallel::clusterExport(cl, 
                          varlist = c("sim_params"),
                          envir = environment())
  parallel::clusterEvalQ(cl, library(datefixer))
  
  parallel::parLapply(cl, seq_len(nsims), function(i) {
    simulate_data(
      sim_params$n_per_group,
      sim_params$group_names,
      sim_params$delay_info,
      sim_params$error_params,
      sim_params$date_range
    )
  })
}

# Named list of simulated data
sim_data_all <- lapply(all_params, simulate_scenario, nsims = nsims)

saveRDS(sim_data_all, "sim_data.rds")
