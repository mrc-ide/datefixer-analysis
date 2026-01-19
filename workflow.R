# orderly_location_fetch_metadata()
# orderly_location_pull(task_id, location = "outbreak_analysis_network")

library(orderly)
library(hipercow)
#renv::install("mrc-ide/datefixer")
#renv::install("mrc-ide/monty@mrc-6769")

orderly_location_fetch_metadata("outbreak_analysis_network")

hipercow_provision() # set up packages using renv
resources <- hipercow_resources(cores = 32)

# Create a named list containing the simulation parameters for all scenarios
orderly_run("sim_params")

# Simulate data for all scenarios
sim100 <- task_create_expr(
  orderly::orderly_run("sim_data", parameters = list(nsims = 100)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

# Smaller number of sims for de-bugging
# sim10 <- task_create_expr(
#   orderly::orderly_run("sim_data", parameters = list(nsims = 10))
# )

task_status(sim100)
task_info(sim100)
task_result(sim100)
#task_cancel(sim100)

# MCMC output -----------------------------------------------------------------
baseline <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "baseline")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline)
task_info(baseline)
task_result(baseline)

# sanity checks
no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_error")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_error_no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)


orderly_run("estim_diagnostics_sanity")
