library(orderly)
library(hipercow)
#renv::install("mrc-ide/datefixer@export_functions")
#renv::install("mrc-ide/monty@mrc-6769")

resources <- hipercow_resources(cores = 4)

# Create a named list containing the simulation parameters for all scenarios
orderly_run("sim_params")

# Simulate data for all scenarios
orderly_run("sim_data", parameters = list(nsims = 100))

# Smaller number of sims for de-bugging
#orderly_run("sim_data", parameters = list(nsims = 10))

# MCMC output -----------------------------------------------------------------
baseline <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "baseline")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline)
task_info(baseline)
task_result(baseline)

orderly_run("sim_estim", parameters = list(scenario = c("baseline")))

# sanity checks
orderly_run("sim_estim", parameters = list(scenario = c("no_missing")))
orderly_run("sim_estim", parameters = list(scenario = c("no_error")))
orderly_run("sim_estim", parameters = list(scenario = c("no_error_no_missing")))

# running in scenario batches
scenarios <- c("very_small_sample",
               "small_sample",
               "moderate_sample",
               "very_large_sample")

for (s in scenarios) {
  orderly_run("sim_estim", parameters = list(scenario = s))
}
