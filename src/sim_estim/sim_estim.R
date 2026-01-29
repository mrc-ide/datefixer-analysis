library(orderly)
library(datefixer)
library(parallel)

pars <- orderly_parameters(scenario = NULL)

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

orderly_artefact(description = "MCMC outputs for simulation scenarios",
                 files = "sim_estim.rds")

selected_scenario <- scenario

# Read in dependencies --------------------------------------------------------

sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")

scenario_names <- names(sim_data)

# Check for scenario name errors
if (is.null(selected_scenario) || identical(selected_scenario, "all")) {
  scenario_list <- scenario_names
} else if (all(selected_scenario %in% scenario_names)) {
  scenario_list <- selected_scenario
} else {
  invalid <- setdiff(selected_scenario, scenario_names)
  stop(sprintf(
    "Invalid scenario(s): %s. Must be 'all' or subset of: %s",
    paste(invalid, collapse = ", "),
    paste(scenario_names, collapse = ", ")
  ))
}

# MCMC settings ---------------------------------------------------------------

control <- mcmc_control(earliest_possible_date = "2014-01-01",
                        latest_possible_date = "2015-01-01")
sampler <- datefixer_sampler(control)
hyperparameters <- datefixer_hyperparameters()

# Run MCMC -------------------------------------------------------------------
mcmc_all <- list()

for (scenario in scenario_list) {
  message("Running MCMC for scenario: ", scenario)

  sim_data_list <- sim_data[[scenario]]
  sim_param <- sim_params[[scenario]]
  delay_map <- sim_param$delay_map
  
  # Get the cluster that hipercow created
  cl <- parallel::getDefaultCluster()
  
  parallel::clusterExport(cl, 
                          varlist = c("sim_data_list", "delay_map", 
                                      "hyperparameters", "control", "sampler"),
                          envir = environment())
  
  # Load datefixer package on each worker
  parallel::clusterEvalQ(cl, library(datefixer))
  
mcmc_samples <- parLapply(cl, seq_along(sim_data_list), function(sim) {
  
  x <- sim_data_list[[sim]]
  #message("Processing sim ", sim, " for scenario: ", scenario)

  model <- datefixer_model(x$observed_data, delay_map, hyperparameters, control)
  mcmc_run(model, sampler)

})

mcmc_all[[scenario]] <- mcmc_samples
}

saveRDS(mcmc_all, "sim_estim.rds")
