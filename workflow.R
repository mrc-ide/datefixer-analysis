#pak::pkg_install("mrc-ide/datefixer@new-update-erroneous-date")
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
task_result(sim100) #"20260422-150253-f5b3e775"

# MCMC output -----------------------------------------------------------------

### quick 1000 iteration version...

baseline_1000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 1000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline_1000) # ade29245e87225c687f6765b3ab3fef4
task_info(baseline_1000)
task_result(baseline_1000) # "20260423-093006-50bca22a"

# sanity checks
no_missing_1000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 1000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing_1000) # fc40572d150bb92b1567de5db0d8fef3
task_result(no_missing_1000) # "20260423-093511-dc55a2e2"

no_error_1000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 1000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_1000) # d6029b41c34d6dfe9dbe9c186f5b8110
task_result(no_error_1000) # "20260423-093023-71295562"

no_error_no_missing_1000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 1000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing_1000) # 29f76126d63ee45ccffbc9bf5308b3fe
task_result(no_error_no_missing_1000) # "20260423-093033-a3c82e39"

### 5000 iterations, no burnin

baseline_5000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 5000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_status(baseline_5000) # bdfbb4a1445dbda5706412c21ce56acd
task_info(baseline_5000)
task_result(baseline_5000) # 

# sanity checks
no_missing_5000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 5000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing_5000) # b5f9defd4d3827b3a73ba64032ed5f06
task_result(no_missing_5000) # 

no_error_5000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 5000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_5000) # 042abdc6929bdf5d1fab41db7fdf8882
task_result(no_error_5000) # 

no_error_no_missing_5000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 5000,
                                         burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing_5000) # 44b25bf8752123b89d92885d3202932c
task_result(no_error_no_missing_5000) # 


### 10000 iterations, 5000 burnin

baseline_10000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 10000,
                                         burnin = 5000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(baseline_10000) # 312b90d0b0b81fb0236c0dbb8b813742
task_result(baseline_10000) # "20260423-090615-b81fca04"

# sanity checks
no_missing_10000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 10000,
                                         burnin = 5000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing_10000) # 9bedf9c5aca1b759f1649cbb585dc527
task_result(no_missing_10000) # "20260423-090621-a3b328e2"

no_error_10000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 10000,
                                         burnin = 5000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_10000) # ecb0a9e5a0ec73880a299589aa351833
task_result(no_error_10000) # "20260423-090624-8a5d8b0b"

no_error_no_missing_10000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 10000,
                                         burnin = 5000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing_10000) # 318796fcf0020591d743e648053730a0
task_result(no_error_no_missing_10000) # "20260423-090628-c22b527b"

### 20,000 iterations, 10,000 burnin

baseline_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 20000,
                                         burnin = 10000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(baseline_20000) # bcb1fb6b5927014e99230342de9bf646
task_result(baseline_20000) # 

no_missing_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 20000,
                                         burnin = 10000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing_20000) # 04a6a183fb156e4740438fadb09cbfdd
task_result(no_missing_20000) # 

no_error_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 20000,
                                         burnin = 10000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_20000) # a7ecedf45ee5d4dce93fc39f58800be6
task_result(no_error_20000) # 

no_error_no_missing_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 20000,
                                         burnin = 10000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing_20000) # 97c4b219413d96b3c8a2f1fcf6a8f912
task_result(no_error_no_missing_20000) # 


# Summarise ------------------------------------------------------------------

sanity <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity")
)

# small run: 1000 iterations

sanity_1000 <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity",
                       parameters = list(n_steps = 1000, burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(sanity_1000) # f0b64e6692902d5a51533b07d34a8f38
task_result(sanity_1000)

# medium: 5000 iterations

sanity_5000 <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity",
                       parameters = list(n_steps = 5000, burnin = 0)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(sanity_5000) # 636caa80afadd319977787074681c272
task_result(sanity_5000)

# large: 10,000 iterations, 5000 burnin

sanity_10000 <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity",
                       parameters = list(n_steps = 10000, burnin = 5000)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(sanity_10000) # 0fad6de7944002a053982d36eae64f53 // a39b4f06a1b3c5e7ee4ebfc10b73ce01
task_result(sanity_10000) # "20260424-140834-dae3cceb"
