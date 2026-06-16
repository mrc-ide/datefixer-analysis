#setwd("/Volumes/outbreak_analysis/rnash/datefixer-analysis")
#pak::pkg_install("mrc-ide/chronofix@generate_linelist")
#pak::pkg_install("mrc-ide/chronofix")
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
task_info(sim100) # "f1aff5b2416902a7751d1506d9fffed9"
task_result(sim100) # "20260518-141548-ad43a0aa"

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
# "high_variability" x
# "low_variability" x
# "lognormal_delays" x

baseline <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 10000,
                                         burnin = 5000,
                                         thinning_factor = 10,
                                         cascade_sampling = TRUE)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(baseline) # baseline <- "806c9a12e707dd3da85898e2aa756bd1"
task_result(baseline)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190312-00afea49"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-082901-7d875780"
# sdlog 0.5: "20260519-155144-42cba06b"
# new branch: "20260505-114129-4551912f"

no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10,
                                         mean_sdlog = 0.1,
                                         cv_sdlog = 0.3,
                                         cascade_sampling = TRUE)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing) # no_missing <- "4036b170678e7a61fcd6934694dfc01e"
task_result(no_missing)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190320-d8bbe36f"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-083231-4a34e4db"
# sdlog 0.5: "20260519-155421-68f7db09"
# new branch: "20260505-114202-082f82d3"

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10,
                                         mean_sdlog = 0.1,
                                         cv_sdlog = 0.3,
                                         cascade_sampling = TRUE)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error) # no_error <- "294c4a35557ac045f4690a8cbf75a357"
task_result(no_error)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190330-bf6af230"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-083537-61ac6dea"
# sdlog 0.5: "20260519-155727-e407f51e"
# new branch: "20260505-114158-3c1befb2"

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10,
                                         mean_sdlog = 0.1,
                                         cv_sdlog = 0.3,
                                         cascade_sampling = TRUE)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing) # no_error_no_missing <- "b761fdeb25a7e25d1b5edbaa521a1e84"
task_result(no_error_no_missing)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190345-9545b9ab"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-083627-51f1e6ee"
# sdlog 0.5: "20260519-155843-78fea145"
# new branch: "20260505-114212-a3abdcfa"

low_missingness <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_missingness",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_missingness) #low_missingness <- "fa76343314f1e0dd01f151eba43a4cee"
task_result(low_missingness)
# new branch: "20260505-114223-97d2e641"

low_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_error) #low_error <- "ebb67455bebe9527186782ab23103fa3"
task_result(low_error) # "20260505-114237-f42708b1"

high_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "high_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(high_error) #high_error <- "8082920eb1e84f51a0bb4fd95c572320"
task_result(high_error) # "20260505-114251-30e28beb"

## sample sizes

very_small_sample <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "very_small_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(very_small_sample) #very_small_sample <- "b7fec47abdbf00f68e6aefe1eabeaf7b"
task_result(very_small_sample) # "20260505-114302-58690ada"

small_sample <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "small_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(small_sample) #small_sample <- "bb25a562d95f8a6192e0ed7f74b0bc2e"
task_result(small_sample) # "20260505-114313-96017c02"

moderate_sample <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "moderate_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(moderate_sample) #moderate_sample <- "5029df308b012684bd0fcc9a5e8c4e50"
task_result(moderate_sample) # "20260505-114326-62ab173d"

very_large_sample <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "very_large_sample",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(very_large_sample) #very_large_sample <- ""
task_result(very_large_sample) # 

long_delays <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "long_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(long_delays) #long_delays <- "e96b1eb2ce937014218692d008c3d7b1"
task_result(long_delays) # "20260505-114357-42f89ae9"

short_delays <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "short_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(short_delays) #short_delays <- "c1e24a93d9cdbc80c6037e894b3de96a"
task_result(short_delays) # "20260505-114407-670d7f67"

high_variability <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "high_variability",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(high_variability) #high_variability <- "51d9282416599410b4543073f904d6df"
task_result(high_variability) # 

low_variability <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_variability",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_variability) #low_variability <- "9548bcabde91dba4820624b0d96fa330"
task_result(low_variability) # "20260505-114416-ca96249b"

lognormal_delays <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "lognormal_delays",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(lognormal_delays) #lognormal_delays <- "b723db9288eb6d75bdeb4acd283804a4"
task_result(lognormal_delays) # "20260505-114444-de3b6e3f"


# Summarise ------------------------------------------------------------------

resources <- hipercow_resources(cores = 32)

## Baseline
baseline_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "baseline")),
  resources = resources
)

task_result(baseline_summarise)
# mean sdlog 0.1, cv sdlog 0.3: "20260521-084659-6be8c61b"
# sd_log mean 0.2 cv 0.3: "20260520-183159-a6b232e3"
# sd_log 0.5: "20260520-072928-e86d64cf"
# new branch: "20260515-085054-08ec515f"

## No error - 9a81466afce1f6336e9109eb2973c501
no_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "no_error")),
  resources = resources
)

task_result(no_error_summarise)
# mean sdlog 0.1, cv sdlog 0.3: "20260521-084709-41f5a726"
# sd_log mean 0.2 cv 0.3: "20260520-173336-be22d86d"
# sd_log 0.5: "20260520-072957-7da0801b"
# new branch: "20260515-085109-4080c97a"

## No missing
no_missing_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "no_missing")),
  resources = resources
)

task_result(no_missing_summarise)
# mean sdlog 0.1, cv sdlog 0.3: "20260521-084719-a3a8dc6e"
# sd_log mean 0.2 cv 0.3: "20260520-181615-3ddda5e7"
# sd_log 0.5: "20260520-073257-f200ca16"
# new branch: "20260515-085159-c93c0cea"

## No error and no missing - d6bc0f83f04ee2821591b4ad6d7ddda9
no_error_no_missing_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "no_error_no_missing")),
  resources = resources
)

task_result(no_error_no_missing_summarise)
# mean sdlog 0.1, cv sdlog 0.3: "20260521-084727-57cc48a8"
# sd_log mean 0.2 cv 0.3: "20260520-173357-7d804cfd"
# sd_log 0.5: "20260520-073334-9216c7a9"
# new branch: "20260515-085210-69637a94"

## Low error
low_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "low_error")),
  resources = resources
)

task_result(low_error_summarise)
# new branch: "20260515-085224-461ef692"

## High error
high_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "high_error")),
  resources = resources
)

task_result(high_error_summarise)
# new branch: "20260515-085458-2e4dd981"

# "20260515-085244-6a3a867f"
low_missingness_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "low_missingness")),
  resources = resources
)

# "20260515-085253-49d1730b"
very_small_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "very_small_sample")),
  resources = resources
)

# "20260515-085547-4ed20fb5"
small_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "small_sample")),
  resources = resources
)

# "20260515-085608-35c36139"
moderate_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "moderate_sample")),
  resources = resources
)

# new: "62c91ec1475779b210fbb080a5f91823"
very_large_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "very_large_sample")),
  resources = resources
)

# "20260515-085332-027e37c7"
long_delays_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "long_delays")),
  resources = resources
)

# "20260515-085340-37bd2879"
short_delays_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "short_delays")),
  resources = resources
)

# "20260515-085350-89089090"
high_variability_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "high_variability")),
  resources = resources
)

# "20260515-085359-9d10b3b1"
low_variability_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "low_variability")),
  resources = resources
)

# "20260515-085414-4e5cd65a"
lognormal_delays_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "lognormal_delays")),
  resources = resources
)


# Visualisations -------------------------------------------------------------

## sanity check diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
sanity <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,no_error,no_missing,no_error_no_missing")),
  resources = resources
)

task_info(sanity) #sanity <- "944eab79f10298048a2e86b0a7653a5c"
task_result(sanity)
# sd_log mean 0.1 cv 0.3: "20260521-095203-14533576"
# sd_log mean 0.2 cv 0.3: "20260520-185138-28d1e46c"
# sd_log 0.5: "20260520-074739-ae4b2d61"
# new branch:"20260515-152045-3eb1bcae"

## variable error diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
variable_error <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,low_error,high_error")),
  resources = resources
)

task_info(variable_error) #variable_error <- "34373e21f34b21d0a347f2a7246375c0"
task_result(variable_error) # "20260515-153212-f1f3f5ad"

## variable group sample size -----------------------

resources <- hipercow_resources(cores = 1)
variable_sample <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,very_small_sample,small_sample,moderate_sample")),#,very_large_sample")),
  resources = resources
)

task_info(variable_sample) #variable_sample <- "96e7b8def165b0da3df35cd3b6e4294a"
task_result(variable_sample) # "20260515-153222-7edbe14e"


## variable delay diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
variable_delays <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = "baseline,long_delays,short_delays")),
  resources = resources
)

task_info(variable_delays) #variable_delays <- "506e022e2717252260c65bcc09b034e5"
task_result(variable_delays) # "20260515-153237-a94657b1"

## variable cv -----------------------

resources <- hipercow_resources(cores = 1)
variable_cv <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = c("baseline,high_variability,low_variability"))),
  resources = resources
)

task_info(variable_cv) #variable_cv <- "24d32af0b3b72c6595bc1688742be87e"
task_result(variable_cv) # "20260515-153245-6c5b2e6d"


## variable delay type -----------------------

resources <- hipercow_resources(cores = 1)
variable_distr <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenarios = c("baseline,lognormal_delays"))),
  resources = resources
)

task_info(variable_distr) #variable_distr <- "4791f3c209ed0f14cd7f927f47ad8406"
task_result(variable_distr) # "20260515-153252-8222aa59"
