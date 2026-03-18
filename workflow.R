#pak::pkg_install("mrc-ide/datefixer")
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

task_status(sim100)
task_info(sim100)
task_result(sim100)

# MCMC output -----------------------------------------------------------------
baseline <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline) # c3360b8748af86ad22340359f7f441d1
task_info(baseline)
task_result(baseline) # "20260317-185618-e09a9af3"

# sanity checks
no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing) # 7ccc5e0d16fc28e3152b252da17d0be8
task_result(no_missing) # "20260317-185635-bb30f6cd"

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error) # 23d34cc00ccc50f3fbc6db4fc7df4491
task_result(no_error) # "20260317-185646-2042fe19"

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing) # acbd0586a7307210419fed9ac55e9e8d
task_result(no_error_no_missing) # "20260317-185717-555c9355"

# Summarise ------------------------------------------------------------------

sanity <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity")
)

sanity <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity"),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(sanity)
task_result(sanity)
