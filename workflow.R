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

task_status(baseline) # ad40517e513b317299d5dd69826c023f
task_info(baseline)
task_result(baseline) # "20260218-164721-7f26e3f4"

# sanity checks
no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing) # f87d714634f1642b522386eece443586
task_result(no_missing) # "20260218-164724-ef4728a0"

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error) # dcb8550bfd492c1f2630b21d91bfb906
task_result(no_error) # "20260218-164728-c3e81533"

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing")),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing) # 99c5d01a46ca81e75131e3ca5d16a080
task_result(no_error_no_missing) # "20260218-164732-ca32fa3c"

# Summarise ------------------------------------------------------------------

sanity <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity")
)

task_info(sanity)
task_result(sanity) # 5000 iter: "20260218-220419-976bc80a"
