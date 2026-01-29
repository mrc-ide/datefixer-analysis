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
orderly_run("sim_params") # "20260129-154325-a6f8e3c5"

# Simulate data for all scenarios
sim100 <- task_create_expr(
  orderly::orderly_run("sim_data", parameters = list(nsims = 100)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(sim100)
task_info(sim100) # 81525dfadd8b16f915923e88e70be77c
task_result(sim100) # "20260129-154351-0fb3bc07"

# MCMC output -----------------------------------------------------------------
baseline <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "baseline")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline)
task_info(baseline) # 590bc6a10b5d77f51277aafc320df14c
task_result(baseline) # "20260129-155259-b8fa7c26"

# sanity checks
no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing) # d9af3648bc132ccd582139479b44f142
task_result(no_missing) # "20260129-155617-b47e172d"

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_error")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error) # a4cb101177d0e83ea6424ced636ac41e
task_result(no_error) # "20260129-155627-9f57deb1"

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim", parameters = list(scenario = "no_error_no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing) # 763cbea5a9f06f12d2e79c7b70c9ab76
task_result(no_error_no_missing) # "20260129-155636-ea8ce86d"


orderly_run("estim_diagnostics_sanity")
