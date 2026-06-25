# Simulate data for each simulation scenario

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

library(orderly)
library(chronofix)

## Number of data sets to simulate for each scenario
pars <- orderly_parameters(nsims = NULL)
orderly_dependency("sim_params", "latest", files = "sim_params.rds")

dir.create("outputs")

# Load all simulation parameters
all_params <- readRDS("sim_params.rds")

set.seed(1)

# Simulate

for (scenario in names(all_params)) {
  
  sim_params <- all_params[[scenario]]
  
  for (i in seq_len(pars$nsims)) {
    filename <- paste0("outputs/sim_data", "_", scenario, "_", i, ".rds")
    
    orderly_artefact(description = "Simulated Data", 
                     files = filename)
    
    res <- chronofix_simulate_data(
      sim_params$n_per_group,
      sim_params$group_names,
      sim_params$delay_info,
      sim_params$error_params,
      sim_params$date_range
    )
    saveRDS(res, filename)
  }
}
