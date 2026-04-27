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

### 20,000 iterations, 10,000 burnin -----------------------------------------

# "baseline" x
# "low_missingness" x
# "no_missing" x
# "no_error" x
# "no_error_no_missing" x
# "low_error" x
# "high_error" x
# "very_small_sample" x
# "small_sample" x
# "moderate_sample" x
# "very_large_sample" x
# "long_delays" x
# "short_delays" x
# "high_variability"
# "low_variability"
# "lognormal_delays"

baseline_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(baseline_20000) # 8e44ac8858dc07175b474c338647f528
task_result(baseline_20000) # "20260426-121041-06eb4727"

no_missing_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing_20000) # 246d2e891435f5b436a6a24f653a86e9
task_result(no_missing_20000) # "20260426-122009-d5f085b0"

no_error_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_20000) # a4717c2593f30e9f02faeb5ffd11d443
task_result(no_error_20000) # "20260426-122313-50011f3b"

no_error_no_missing_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing_20000) # 0241aabcce4ae19ec8ad4b29bd1caacd
task_result(no_error_no_missing_20000) # "20260426-123522-02d3d4d2"

low_missingness_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_missingness",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_missingness_20000) # 85d147572307ea16fe9ee1ba7ddf327a
task_result(low_missingness_20000) # 

low_error_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_error_20000) # 2f7f615191cfd9302bfd97761e12eede
task_result(low_error_20000) # 

high_error_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "high_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(high_error_20000) # 2c7f0924c0903ef39d14aef6774a5354
task_result(high_error_20000) # 

## sample sizes

very_small_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "very_small_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(very_small_sample_20000) # 51579511835c791dae098b6694565a3e
task_result(very_small_sample_20000) # 

small_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "small_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(small_sample_20000) # 2d3036e7e0443d91c1659f7dd16d5e14
task_result(small_sample_20000) # 

moderate_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "moderate_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(moderate_sample_20000) # 01c41528f2156c92afb8daf2d4dc87a3
task_result(moderate_sample_20000) # 

very_large_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "very_large_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(very_large_sample_20000) # 97202b489b42f7e55ac8f6116168d7d4
task_result(very_large_sample_20000) # 

# "long_delays"
# "short_delays"

long_delays_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "long_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(long_delays_20000) # 0c53a49690cd4bc929b6e5a5857d70b9
task_result(long_delays_20000) # 

short_delays_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "short_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(short_delays_20000) # 9f29decd1bf33835ba1aad304c3ef304
task_result(short_delays_20000) # 

# "high_variability"
# "low_variability"
# "lognormal_delays"

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
task_result(sanity_10000) # "20260424-152842-6d290172"

# v. large: 20,000 iterations, 10,000 burnin, 10 thinning (due to memory issues)
resources <- hipercow_resources(cores = 1)
sanity_20000 <- task_create_expr(
  orderly::orderly_run("estim_diagnostics_sanity",
                       parameters = list(n_steps = 20000, burnin = 10000)),
  resources = resources
)

task_info(sanity_20000) # 9ad0addd53892170f4999c26df07e31f
task_result(sanity_20000) # "20260427-124822-35ac60c2"
