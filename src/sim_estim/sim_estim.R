library(orderly)
library(chronofix)
library(parallel)

pars <- orderly_parameters(scenario = "baseline", dataset = 1,
                           n_steps = 20000, burnin = 10000,
                           thinning_factor = 10, mean_sdlog = 0.3,
                           cv_sdlog = 0.1, cascade_sampling = TRUE)

scenario <- pars$scenario
dataset <- pars$dataset
iterations <- pars$n_steps
burn <- pars$burnin
thin <- pars$thinning_factor
mean_sdlog <- pars$mean_sdlog
cv_sdlog <- pars$cv_sdlog
cascade_sampling <- pars$cascade_sampling

orderly_dependency("sim_params", "latest", "sim_params.rds")
data_filename <- paste0("outputs/sim_data_", scenario, "_", dataset, ".rds")
orderly_dependency("sim_data", "latest", 
                   c("sim_data.rds" = data_filename))

orderly_artefact(description = "MCMC outputs for simulation scenarios",
                 files = "sim_estim.rds")



# Read in dependencies --------------------------------------------------------

sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")

# MCMC settings ---------------------------------------------------------------

control <- chronofix_mcmc_control(n_steps = iterations,
                                  burnin = burn,
                                  thinning_factor = thin,
                                  n_chains = 4,
                                  parallel = TRUE,
                                  n_workers = 4,
                                  earliest_possible_date = "2014-01-01",
                                  latest_possible_date = "2015-01-01",
                                  mean_sdlog = mean_sdlog,
                                  cv_sdlog = cv_sdlog,
                                  cascade_sampling = cascade_sampling)
sampler <- chronofix_sampler(control)
hyperparameters <- chronofix_hyperparameters()

# Run MCMC -------------------------------------------------------------------

sim_param <- sim_params[[scenario]]
delay_info <- sim_param$delay_info

model <- chronofix_model(sim_data$observed_data, delay_info,
                         hyperparameters, control)
res <- chronofix_mcmc_run(model, sampler, control = control)

saveRDS(res, "sim_estim.rds")
