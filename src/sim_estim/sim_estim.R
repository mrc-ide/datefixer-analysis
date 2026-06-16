library(orderly)
library(chronofix)
library(parallel)

pars <- orderly_parameters(scenario = NULL, n_steps = NULL, burnin = NULL,
                           thinning_factor = NULL, cascade_sampling = NULL)

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

orderly_artefact(description = "MCMC outputs for simulation scenarios",
                 files = "sim_estim.rds")

selected_scenario <- scenario
iterations <- n_steps
burn <- burnin
thin <- thinning_factor
cascade_sampling <- cascade_sampling

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

control <- chronofix_mcmc_control(n_steps = iterations,
                                  burnin = burn,
                                  thinning_factor = thin,
                                  n_chains = 4,
                                  earliest_possible_date = "2014-01-01",
                                  latest_possible_date = "2015-01-01",
                                  cascade_sampling = cascade_sampling)
sampler <- chronofix_sampler(control)
hyperparameters <- chronofix_hyperparameters()

# Run MCMC -------------------------------------------------------------------
mcmc_all <- list()

for (scenario in scenario_list) {
  message("Running MCMC for scenario: ", scenario)

  sim_data_list <- sim_data[[scenario]]
  sim_param <- sim_params[[scenario]]
  delay_info <- sim_param$delay_info
  
  out_dir <- file.path(tempdir(), paste0("mcmc_", scenario))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Get the cluster that hipercow created
  cl <- parallel::getDefaultCluster()
  
  parallel::clusterExport(cl, 
                          varlist = c("delay_info", "hyperparameters",
                                      "control", "sampler", "out_dir"),
                          envir = environment())
  
  # Load chronofix package on each worker
  parallel::clusterEvalQ(cl, library(chronofix))
  
  # Fix memory issues - each worker saves their result and returns a path
  paths <- parLapply(cl, sim_data_list, function(sim) {
    model <- chronofix_model(sim$observed_data, delay_info, hyperparameters, control)
    res <- chronofix_mcmc_run(model, sampler, control = control)
    
    nms <- rownames(res$initial)
    i_shape <- which(endsWith(nms, "shape"))
    i_mean <- which(endsWith(nms, "mean"))
    
    res$initial[i_shape, ] <- 1 / sqrt(res$initial[i_shape, ])
    res$pars[i_shape, , ] <- 1 / sqrt(res$pars[i_shape, , ])
    
    res$full_chains$initial[i_shape, ] <- 1 / sqrt(res$full_chains$initial[i_shape, ])
    res$full_chains$pars[i_shape, , ] <- 1 / sqrt(res$full_chains$pars[i_shape, , ])
    
    nms[i_shape] <- paste0("delay_cv", seq_along(i_shape))
    nms[i_mean] <- paste0("delay_mean", seq_along(i_shape))
    
    rownames(res$initial) <- nms
    rownames(res$pars) <- nms
    rownames(res$full_chains$initial) <- nms
    rownames(res$full_chains$par) <- nms
    
    p <- tempfile(pattern = "mcmc_", tmpdir = out_dir, fileext = ".rds")
    saveRDS(res, p)
    p
  })
  paths <- unlist(paths)
  
  saveRDS(1, "one.rds")
  
  mcmc_samples <- lapply(paths, readRDS)
  names(mcmc_samples) <- names(sim_data_list)
  mcmc_all[[scenario]] <- mcmc_samples
}

saveRDS(mcmc_all, "sim_estim.rds")
