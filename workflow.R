#setwd("/Volumes/outbreak_analysis/rnash/datefixer-analysis")
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

## all simulation scenarios
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

task_info(low_missingness_20000)
#low_missingness_20000 <- "85d147572307ea16fe9ee1ba7ddf327a"
task_result(low_missingness_20000) # "20260427-135452-4ffd7c3e"

low_error_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_error_20000)
#low_error_20000 <- "2f7f615191cfd9302bfd97761e12eede"
task_result(low_error_20000) # "20260427-135503-21fcbfc3"

high_error_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "high_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(high_error_20000)
#high_error_20000 <- "2c7f0924c0903ef39d14aef6774a5354"
task_result(high_error_20000) # "20260427-135517-87bedaf4"

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

task_info(very_small_sample_20000)
#very_small_sample_20000 <- "51579511835c791dae098b6694565a3e"
task_result(very_small_sample_20000) # "20260427-135527-8194d072"

small_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "small_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(small_sample_20000)
#small_sample_20000 <- "2d3036e7e0443d91c1659f7dd16d5e14"
task_result(small_sample_20000) # "20260427-135558-96648c33"

moderate_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "moderate_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(moderate_sample_20000)
#moderate_sample_20000 <- "01c41528f2156c92afb8daf2d4dc87a3"
task_result(moderate_sample_20000) # "20260427-135543-1a353f28"

very_large_sample_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "very_large_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(very_large_sample_20000)
#very_large_sample_20000 <- "97202b489b42f7e55ac8f6116168d7d4"
task_result(very_large_sample_20000) # 

long_delays_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "long_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(long_delays_20000)
#long_delays_20000 <- "0c53a49690cd4bc929b6e5a5857d70b9"
task_result(long_delays_20000) # "20260427-135830-2ff689cc"

short_delays_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "short_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(short_delays_20000)
#short_delays_20000 <- "9f29decd1bf33835ba1aad304c3ef304"
task_result(short_delays_20000) # "20260427-135811-9e4802e6"

high_variability_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "high_variability",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(high_variability_20000)
#high_variability_20000 <- 
task_result(short_delays_20000) #

low_variability_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_variability",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_variability_20000)
#low_variability_20000 <- 
task_result(low_variability_20000) #

lognormal_delays_20000 <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "lognormal_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(lognormal_delays_20000)
#lognormal_delays_20000 <- 
task_result(lognormal_delays_20000) #


# Summarise ------------------------------------------------------------------

## sanity check diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
sanity_20000 <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,no_error,no_missing,no_error_no_missing")),
  resources = resources
)

task_info(sanity_20000)
#sanity_20000 <- "9ad0addd53892170f4999c26df07e31f"
task_result(sanity_20000) # "20260427-124822-35ac60c2"

## variable error diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
variable_error <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,low_error,high_error")),
  resources = resources
)

task_info(variable_error)
#variable_error <- "12850f364289f719e0509e76e28961bb"
task_result(variable_error) # 

## variable sample size -----------------------

resources <- hipercow_resources(cores = 1)
variable_sample <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,very_small_sample,small_sample,moderate_sample,very_large_sample")),
  resources = resources
)

task_info(variable_sample)
#variable_sample <-
task_result(variable_sample) # 


## variable delay diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
variable_delays <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,long_delays,short_delays")),
  resources = resources
)

task_info(variable_delays)
#variable_delays <- bde2d5af9866b184b5dde76e8c8e2a0d
task_result(variable_delays) # 

## variable cv -----------------------

resources <- hipercow_resources(cores = 1)
variable_cv <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = c("baseline,high_variability,low_variability"))),
  resources = resources
)

task_info(variable_cv)
#variable_cv <-
task_result(variable_cv) # 

