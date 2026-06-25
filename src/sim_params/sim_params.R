# Simulation parameters

# TO DO: set up lognormal_delays, weibull_delays and other error model scenarios

orderly_artefact(description = "Simulation parameters for all scenarios",
                 files = c("date_params.rds",
                           "error_params.rds",
                           "scenarios.rds"))

library(tibble)
library(dplyr)
library(purrr)

source("utils.R") # build_sim_params()

# 1. setup date params
## baseline date params
baseline_date_params <- tibble(
  date_model = "baseline",
  group_size = 100,
  mean_scale = 1,
  cv_scale = 1,
  delay_distribution = "gamma"
)

## Create other date params by modifying the baseline
date_params <- list(
  baseline = baseline_date_params,
  lognormal_delays = 
    baseline_date_params %>% mutate(delay_distribution = "log-normal"),
  very_small_sample = 
    baseline_date_params %>% mutate(group_size = 10),
  small_sample = 
    baseline_date_params %>% mutate(group_size = 20),
  moderate_sample =
    baseline_date_params %>% mutate(group_size = 50),
  very_large_sample = 
    baseline_date_params %>% mutate(group_size = 250),
  long_delays =
    baseline_date_params %>% mutate(mean_scale = 2),
  short_delays = 
    baseline_date_params %>% mutate(mean_scale = 0.5),
  high_variability = 
    baseline_date_params %>% mutate(cv_scale = 2),
  low_variability = 
    baseline_date_params %>% mutate(cv_scale = 0.5)
)

date_params <- lapply(
  date_params,
  function(x) {
    build_date_params(
      date_model  = x$date_model,
      n_per_group  = rep(x$group_size, 4),
      mean_scale   = x$mean_scale,
      cv_scale     = x$cv_scale,
      delay_distribution = x$delay_distribution,
    )
  }
)


# 2. setup error params
## baseline error params
baseline_error_params <- tibble(
  prop_missing_data = 0.2,
  prob_error = 0.05
)

# Create other error params by modifying the baseline
error_params <- list(
  baseline = baseline_error_params,
  low_missingness = 
    baseline_error_params %>% mutate(prop_missing_data = 0.05),
  no_missing = 
    baseline_error_params %>% mutate(prop_missing_data = 0), # sanity check
  no_error = 
    baseline_error_params %>% mutate(prob_error = 0), # sanity check
  no_error_no_missing = 
    baseline_error_params %>% mutate(prob_error = 0, 
                                     prop_missing_data = 0), # sanity check
  low_error = 
    baseline_error_params %>% mutate(prob_error = 0.02),
  high_error = 
    baseline_error_params %>% mutate(prob_error = 0.2)
)


# 3. setup scenarios
## baseline error params
baseline_scenario <- tibble(
  date_model = "baseline",
  error_model = "baseline"
)

# Create other simulation scenarios by modifying the baseline
scenarios <- list(
  baseline = baseline_scenario,
  low_missingness = 
    baseline_scenario %>% mutate(error_model = "low_missingness"),
  no_missing = 
    baseline_scenario %>% mutate(error_model = "no_missing"),
  no_error = 
    baseline_scenario %>% mutate(error_model = "no_error"),
  no_error_no_missing = 
    baseline_scenario %>% mutate(error_model = "no_error_no_missing"),
  low_error = 
    baseline_scenario %>% mutate(error_model = "low_error"),
  high_error = 
    baseline_scenario %>% mutate(error_model = "high_error"),
  lognormal_delays = 
    baseline_scenario %>% mutate(date_model = "lognormal_delays"),
  very_small_sample = 
    baseline_scenario %>% mutate(date_model = "very_small_sample"),
  small_sample = 
    baseline_scenario %>% mutate(date_model = "small_sample"),
  moderate_sample =
    baseline_scenario %>% mutate(date_model = "moderate_sample"),
  very_large_sample = 
    baseline_scenario %>% mutate(date_model = "very_large_sample"),
  long_delays = 
    baseline_scenario %>% mutate(date_model = "long_delays"),
  short_delays = 
    baseline_scenario %>% mutate(date_model = "short_delays"),
  high_variability =
    baseline_scenario %>% mutate(date_model = "high_variability"),
  low_variability = 
    baseline_scenario %>% mutate(date_model = "low_variability")
)

saveRDS(date_params, "date_params.rds")
saveRDS(error_params, "error_params.rds")
saveRDS(scenarios, "scenarios.rds")
