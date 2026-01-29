#pak::pkg_install("mrc-ide/datefixer@update-simulation-groups")
#pak::pkg_install("mrc-ide/monty@mrc-6769")

library(orderly)
library(hipercow)

# orderly_location_fetch_metadata()
# orderly_location_pull(task_id, location = "outbreak_analysis_network")

orderly_location_fetch_metadata("outbreak_analysis_network")

hipercow_provision(method = "pkgdepends")
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
task_info(sim100) # 2f431feee37c8bd40af60f1e83058cd2
task_result(sim100)
#task_cancel(sim100)

# MCMC output -----------------------------------------------------------------
baseline <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "baseline")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline)
task_info(baseline) # 0ea3e7d879fafebee604e2975a8f9df0
task_result(baseline) # "20260105-101404-d0f1a4d9"

# sanity checks
no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing) # f5dc536c929516316f4de0fc58167704
task_result(no_missing) # "20260105-101522-a821c5cb"

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_error")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error) # 741085fc391ee442181375d5a89cbd8c
task_result(no_error) # "20260105-101551-ebf2f18a"

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_error_no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing) # 97e1229eef6e472f237a2d24dc916c23
task_result(no_error_no_missing) # "20260105-101600-0a839f54"

