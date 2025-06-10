# Simulation parameters

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

orderly2::orderly_artefact(description = "Simulation parameters for all scenarios",
                           files = "sim_params.rds")

library(MixDiff)
library(tibble)
library(dplyr)
library(purrr)

source("utils.R") # build_sim_params()

# Baseline scenario
baseline <- tibble(
  scenario_id = "baseline",
  group_size = 100,
  mu_scale = 1,
  cv_scale = 1,
  delay_dist = "gamma",
  prop_missing = 0.2,
  zeta = 0.05, # error rate
  error_model = "typo"
)

# Create other simulation scenarios by modifying the baseline
scenarios <- bind_rows(
  baseline,
  baseline %>% mutate(
    scenario_id = "low_missingness",
    prop_missing = 0.05
  ),
  baseline %>% mutate(
    scenario_id = "low_error",
    zeta = 0.02
  ),
  baseline %>% mutate(
    scenario_id = "high_error",
    zeta = 0.2
  ),
  # baseline %>% mutate(
  #   scenario_id = "lognormal_delays",
  #   delay_dist = "lognormal"
  # ),
  # baseline %>% mutate(
  #   scenario_id = "weibull_delays",
  #   delay_dist = "weibull"
  # ),
  baseline %>% mutate(
    scenario_id = "very_small_sample",
    group_size = 10
  ),
  baseline %>% mutate(
    scenario_id = "small_sample",
    group_size = 20
  ),
  baseline %>% mutate(
    scenario_id = "moderate_sample",
    group_size = 50
  ),
  baseline %>% mutate(
    scenario_id = "very_large_sample",
    group_size = 500
  ),
  baseline %>% mutate(
    scenario_id = "long_delays",
    mu_scale = 2
  ),
  baseline %>% mutate(
    scenario_id = "short_delays",
    mu_scale = 0.5
  ),
  baseline %>% mutate(
    scenario_id = "high_variability",
    cv_scale = 2
  ),
  baseline %>% mutate(
    scenario_id = "low_variability",
    cv_scale = 0.5
  )
)

params_list <- map(
  scenarios$scenario_id,
  function(sid) {
    row <- scenarios %>% filter(scenario_id == sid)
    
    build_sim_params(
      scenario_id  = row$scenario_id,
      n_per_group  = rep(row$group_size, 4),
      mu_scale     = row$mu_scale,
      cv_scale     = row$cv_scale,
      delay_dist   = row$delay_dist,
      prop_missing = row$prop_missing,
      zeta         = row$zeta,
      error_model  = row$error_model
    )
  }
)

names(params_list) <- scenarios$scenario_id

saveRDS(params_list, "sim_params.rds")
