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

baseline <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "baseline",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(baseline)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190312-00afea49"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-082901-7d875780"
# sdlog 0.5: "20260519-155144-42cba06b"
# new branch: "20260505-114129-4551912f"

no_missing <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "no_missing",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(no_missing)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190320-d8bbe36f"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-083231-4a34e4db"
# sdlog 0.5: "20260519-155421-68f7db09"
# new branch: "20260505-114202-082f82d3"

no_error <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "no_error",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(no_error)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190330-bf6af230"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-083537-61ac6dea"
# sdlog 0.5: "20260519-155727-e407f51e"
# new branch: "20260505-114158-3c1befb2"

no_error_no_missing <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "no_error_no_missing",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(no_error_no_missing)
# generate linelist: 
# mean sdlog 0.1, cv sdlog 0.3: "20260520-190345-9545b9ab"
# mean sdlog 0.2, cv sdlog 0.3: "20260520-083627-51f1e6ee"
# sdlog 0.5: "20260519-155843-78fea145"
# new branch: "20260505-114212-a3abdcfa"

low_missingness <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "low_missingness",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(low_missingness)
# new branch: "20260505-114223-97d2e641"

low_error <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "low_error",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(low_error)

high_error <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "high_error",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(high_error)

## sample sizes

very_small_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "very_small_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(very_small_sample)


small_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "small_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(small_sample)


moderate_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "moderate_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(moderate_sample)


very_large_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "very_large_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(very_large_sample)


long_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "long_delays",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(long_delays)


short_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "short_delays",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(short_delays)


high_variability <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "high_variability",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(high_variability)


low_variability <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "low_variability",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(low_variability)


lognormal_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "lognormal_delays",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(lognormal_delays)


# Summarise ------------------------------------------------------------------

resources <- hipercow_resources(cores = 32)

## Baseline
baseline_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "baseline")),
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
    parameters = list(scenario = "no_error")),
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
    parameters = list(scenario = "no_missing")),
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
    parameters = list(scenario = "no_error_no_missing")),
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
    parameters = list(scenario = "low_error")),
  resources = resources
)

task_result(low_error_summarise)
# new branch: "20260515-085224-461ef692"

## High error
high_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "high_error")),
  resources = resources
)

task_result(high_error_summarise)
# new branch: "20260515-085458-2e4dd981"

# "20260515-085244-6a3a867f"
low_missingness_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "low_missingness")),
  resources = resources
)

task_result(low_missingness_summarise)

# "20260515-085253-49d1730b"
very_small_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "very_small_sample")),
  resources = resources
)

task_result(very_small_sample_summarise)

# "20260515-085547-4ed20fb5"
small_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "small_sample")),
  resources = resources
)

# "20260515-085608-35c36139"
moderate_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "moderate_sample")),
  resources = resources
)

# new: "62c91ec1475779b210fbb080a5f91823"
very_large_sample_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "very_large_sample")),
  resources = resources
)

# "20260515-085332-027e37c7"
long_delays_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "long_delays")),
  resources = resources
)

# "20260515-085340-37bd2879"
short_delays_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "short_delays")),
  resources = resources
)

# "20260515-085350-89089090"
high_variability_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "high_variability")),
  resources = resources
)

# "20260515-085359-9d10b3b1"
low_variability_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "low_variability")),
  resources = resources
)

# "20260515-085414-4e5cd65a"
lognormal_delays_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(scenario = "lognormal_delays")),
  resources = resources
)


# Visualisations -------------------------------------------------------------

## sanity check diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
sanity <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(scenarios = "baseline,no_error,no_missing,no_error_no_missing")),
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
    parameters = list(scenarios = "baseline,low_error,high_error")),
  resources = resources
)

task_info(variable_error) #variable_error <- "34373e21f34b21d0a347f2a7246375c0"
task_result(variable_error) # "20260515-153212-f1f3f5ad"

## variable group sample size -----------------------

resources <- hipercow_resources(cores = 1)
variable_sample <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(scenarios = "baseline,very_small_sample,small_sample,moderate_sample,very_large_sample")),
  resources = resources
)

task_info(variable_sample) #variable_sample <- "96e7b8def165b0da3df35cd3b6e4294a"
task_result(variable_sample) # "20260515-153222-7edbe14e"


## variable delay diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
variable_delays <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(scenarios = "baseline,long_delays,short_delays")),
  resources = resources
)

task_info(variable_delays) #variable_delays <- "506e022e2717252260c65bcc09b034e5"
task_result(variable_delays) # "20260515-153237-a94657b1"

## variable cv -----------------------

resources <- hipercow_resources(cores = 1)
variable_cv <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(scenarios = c("baseline,high_variability,low_variability"))),
  resources = resources
)

task_info(variable_cv) #variable_cv <- "24d32af0b3b72c6595bc1688742be87e"
task_result(variable_cv) # "20260515-153245-6c5b2e6d"


## variable delay type -----------------------

resources <- hipercow_resources(cores = 1)
variable_distr <- task_create_expr(
  orderly::orderly_run(
    "estim_diagnostics",
    parameters = list(scenarios = c("baseline,lognormal_delays"))),
  resources = resources
)

task_info(variable_distr) #variable_distr <- "4791f3c209ed0f14cd7f927f47ad8406"
task_result(variable_distr) # "20260515-153252-8222aa59"
