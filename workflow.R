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
# "high_variability" x
# "low_variability" x
# "lognormal_delays" x

baseline <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "baseline",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(baseline) # 2176b2a02d99c69c1f34ac3356b52e9c
task_result(baseline) # "20260505-114129-4551912f"

no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_missing",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_missing) # 6431b30fad173484f484cac7ce390259
task_result(no_missing) # "20260505-114202-082f82d3"

no_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error) # no_error <- "c2e13485c99b8ef227293e2c95a0c37f"
task_result(no_error) # "20260505-114158-3c1befb2"

no_error_no_missing <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "no_error_no_missing",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(no_error_no_missing) # no_error_no_missing <- "96102fb77ace169f83428dc768521562"
task_result(no_error_no_missing) # "20260505-114212-a3abdcfa"

low_missingness <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_missingness",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_missingness)
#low_missingness <- "fa76343314f1e0dd01f151eba43a4cee"
task_result(low_missingness) # "20260505-114223-97d2e641"

low_error <- task_create_expr(
  orderly::orderly_run("sim_estim",
                       parameters = list(scenario = "low_error",
                                         n_steps = 20000,
                                         burnin = 10000,
                                         thinning_factor = 10)),
  parallel = hipercow_parallel("parallel"),
  resources = resources
)

task_info(low_error)
#low_error <- "ebb67455bebe9527186782ab23103fa3"
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

task_info(high_error)
#high_error <- "8082920eb1e84f51a0bb4fd95c572320"
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

task_info(very_small_sample)
#very_small_sample <- "b7fec47abdbf00f68e6aefe1eabeaf7b"
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

task_info(small_sample)
#small_sample <- "bb25a562d95f8a6192e0ed7f74b0bc2e"
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

task_info(moderate_sample)
#moderate_sample <- "5029df308b012684bd0fcc9a5e8c4e50"
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

task_info(very_large_sample)
#very_large_sample <- "77c6ca92efeacd225093ddf060f85f4b"
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

task_info(long_delays)
#long_delays <- "e96b1eb2ce937014218692d008c3d7b1"
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

task_info(short_delays)
#short_delays <- "c1e24a93d9cdbc80c6037e894b3de96a"
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

task_info(high_variability)
#high_variability <- "51d9282416599410b4543073f904d6df"
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

task_info(low_variability)
#low_variability <- "9548bcabde91dba4820624b0d96fa330"
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

task_info(lognormal_delays)
#lognormal_delays <- "b723db9288eb6d75bdeb4acd283804a4"
task_result(lognormal_delays) # "20260505-114444-de3b6e3f"


# Summarise ------------------------------------------------------------------

resources <- hipercow_resources(cores = 32)

# "20260515-085054-08ec515f"
baseline_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "baseline")),
  resources = resources
)

# "20260515-085109-4080c97a"
no_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "no_error")),
  resources = resources
)

# "20260515-085159-c93c0cea"
no_missing_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "no_missing")),
  resources = resources
)

# "20260515-085210-69637a94"
no_error_no_missing_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "no_error_no_missing")),
  resources = resources
)

# "20260515-085224-461ef692"
low_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "low_error")),
  resources = resources
)

# "20260515-085458-2e4dd981"
high_error_summarise <- task_create_expr(
  orderly::orderly_run(
    "estim_summary",
    parameters = list(n_steps = 20000, burnin = 10000, thinning_factor = 10,
                      scenario = "high_error")),
  resources = resources
)

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

task_info(sanity)
#sanity <- "bbc3194aa9fec80dc9af116098c277ac"
task_result(sanity) # "20260515-152045-3eb1bcae"

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
#variable_error <- "34373e21f34b21d0a347f2a7246375c0"
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

task_info(variable_sample)
#variable_sample <- "96e7b8def165b0da3df35cd3b6e4294a"
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

task_info(variable_delays)
#variable_delays <- "506e022e2717252260c65bcc09b034e5"
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

task_info(variable_cv)
#variable_cv <- "24d32af0b3b72c6595bc1688742be87e"
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

task_info(variable_distr)
#variable_distr <- "4791f3c209ed0f14cd7f927f47ad8406"
task_result(variable_distr) # "20260515-153252-8222aa59"
