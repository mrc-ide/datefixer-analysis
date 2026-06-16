library(orderly)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(posterior)
library(stringr)
library(furrr)
library(future)
library(abind)
library(monty)

options(future.globals.maxSize = Inf)

pars <- orderly_parameters(scenario = NULL, n_steps = NULL, burnin = NULL,
                           thinning_factor = NULL, cascade_sampling = TRUE)

scenario <- scenario
n_steps <- n_steps
burnin <- burnin
thinning_factor <- thinning_factor
cascade_sampling <- cascade_sampling

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

orderly_dependency("sim_estim",
                   "latest(parameter:scenario == environment:scenario && 
                   parameter:n_steps == environment:n_steps &&
                   parameter:burnin == environment:burnin &&
                   parameter:thinning_factor == environment:thinning_factor &&
                   parameter:cascade_sampling == environment:cascade_sampling)",
                   c("sim_estim.rds" = "sim_estim.rds"))

orderly_artefact(files = c("results/true_params.rds",
                           "results/all_draws.rds",
                           "results/sim_summaries.rds",
                           "results/agg_summaries.rds",
                           "results/observed_patterns.rds",
                           "results/indiv_obs_summary.rds",
                           "results/obs_pattern_summary.rds",
                           "results/indiv_event_status.rds",
                           "results/event_match_summary.rds",
                           "results/event_confusion.rds",
                           "results/pattern_confusion.rds",
                           "results/convergence_issues.rds",
                           "results/scenario_convergence.rds",
                           "results/chain_diagnostics.rds",
                           "results/acceptance_rates.rds",
                           "results/convergence_issues_by_individual.rds",
                           "results/indiv_performance_summary.rds"),
                 description = "MCMC Summary outputs for a single scenario")

dir.create("results", showWarnings = FALSE)

scenario_labels <- c(
  "low_missingness" = "Low missingness (0.05)",
  "very_small_sample" = "Very small groups (n = 10)",
  "small_sample" = "Small groups (n = 20)",
  "moderate_sample" = "Moderate groups (n = 50)",
  "high_error" = "High error (0.2)",
  "short_delays" = "Short delays (0.5x baseline)",
  "low_variability" = "Low variability (0.5x baseline cv)",
  "baseline" = "Baseline", 
  "low_error" = "Low error (0.02)",
  "no_error" = "Missing dates only (0.2)",
  "no_missing" = "Errors only (0.05)",
  "no_error_no_missing" = "No errors or missing dates",
  "very_large_sample" = "Very large groups (n = 500)",
  "long_delays" = "Long delays (2x baseline)",
  "high_variability" = "High variability (2x baseline cv)",
  "lognormal_delays" = "Lognormal delays"
)

scenario_labels <- scenario_labels[intersect(names(scenario_labels), scenario)]
scenario_labels <- factor(scenario_labels, levels = scenario_labels)

cl <- parallel::getDefaultCluster()

if (!is.null(cl)) {
  plan(cluster, workers = cl)
} else {
  num_cores <- max(1, parallel::detectCores() - 1)
  plan(multisession, workers = num_cores)
}

# Read in dependencies -------------------------------------------------------
sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")

# Delay mapping --------------------------------------------------------------
delay_mapping <- tribble(
  ~param_idx, ~delay_from,       ~delay_to,           ~group,
  1,          "onset",           "report",            "community-alive",
  2,          "onset",           "report",            "community-dead",
  3,          "onset",           "report",            "hospitalised-alive",
  4,          "onset",           "report",            "hospitalised-dead",
  5,          "onset",           "death",             "community-dead",
  6,          "onset",           "hospitalisation",   "hospitalised-alive",
  7,          "hospitalisation", "discharge",         "hospitalised-alive",
  8,          "onset",           "hospitalisation",   "hospitalised-dead",
  9,          "hospitalisation", "death",             "hospitalised-dead"
) %>%
  mutate(
    param_label = as.character(glue("{delay_from} to {delay_to}")),
    group = as.character(group)
  )

apply_factor_levels <- function(df) {
  df %>% mutate(
    param_label = as.character(param_label),
    param_label = ifelse(param_idx == 0, "probability of error", param_label),
    group = factor(group, levels = c("community-alive", "community-dead", 
                                     "hospitalised-alive", "hospitalised-dead")),
    param_label = factor(param_label, levels = c(
      "probability of error",
      "onset to report", 
      "onset to death", 
      "onset to hospitalisation", 
      "hospitalisation to discharge", 
      "hospitalisation to death"
    )),
    scenario = factor(recode(scenario, !!!scenario_labels),
                      levels = unname(scenario_labels))
  )
}

apply_scenario_labels <- function(df) {
  df %>%
    mutate(
      scenario = factor(
        recode(scenario, !!!scenario_labels),
        levels = unname(scenario_labels)
      ),
      group = factor(group, levels = c(
        "community-alive", "community-dead",
        "hospitalised-alive", "hospitalised-dead"
      ))
    )
}

# Extract true parameter values for this scenario ----------------------------
params <- sim_params[[scenario]]
true_params <- tibble(
  param_idx = 1:nrow(params$delay_info),
  true_mean = params$delay_info$mean,
  true_cv   = params$delay_info$cv
) %>% 
  bind_rows(tibble(
    param_idx = 0,
    true_mean = params$error_params$prob_error,
    true_cv   = NA
  )) %>% 
  mutate(scenario = scenario) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels()

# Helper functions -----------------------------------------------------------
extract_draws_and_summary <- function(pars_array, scenario_name, sim_idx) {
  pars_transposed <- aperm(pars_array, c(2, 3, 1))
  draws_df <- as_draws_df(pars_transposed)
  
  summary <- summarise_draws(
    draws_df,
    "mean",
    ~quantile(.x, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE),
    "rhat", "ess_bulk", "ess_tail"
  ) %>%
    rename(
      overall_mean = mean,
      overall_q025 = `2.5%`,
      overall_q25 = `25%`,
      overall_q75 = `75%`,
      overall_q975 = `97.5%`
    ) %>%
    filter(grepl("^(prob_error|delay_mean|delay_cv)", variable)) %>%
    mutate(
      scenario = scenario_name,
      simulation = sim_idx,
      param_idx = case_when(
        variable == "prob_error" ~ 0,
        grepl("delay_mean|delay_cv", variable) ~
          as.numeric(str_extract(variable, "\\d+")),
        TRUE ~ NA
      ),
      type = case_when(
        grepl("^delay_cv", variable) ~ "cv",
        TRUE ~ "mean"
      )
    ) %>%
    select(-variable)
  
  tidy <- draws_df %>%
    as_tibble() %>%
    select(.chain, .iteration, matches("^(prob_error|delay_mean|delay_cv)")) %>%
    pivot_longer(
      cols = matches("^(prob_error|delay_mean|delay_cv)"),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      scenario = scenario_name,
      simulation = sim_idx,
      chain = .chain,
      iteration = .iteration,
      param_idx = case_when(
        variable == "prob_error" ~ 0,
        grepl("delay_mean|delay_cv", variable) ~
          as.numeric(str_extract(variable, "\\d+")),
        TRUE ~ NA
      ),
      type = ifelse(grepl("^delay_cv", variable), "cv", "mean")
    ) %>%
    select(-variable, -.chain, -.iteration)
  
  list(summary = summary, tidy_draws = tidy)
}

calc_acceptance_rate <- function(samples) {
  initial <- samples$full_chains$initial
  initial <- array(initial, c(dim(initial)[1], 1, dim(initial)[2]))
  pars <- abind::abind(initial, samples$full_chains$pars, along = 2)
  
  n_accept <- apply(apply(pars, c(1, 3), diff) != 0, c(2, 3), sum)
  n_steps <- dim(pars)[2] - 1
  
  n_accept / n_steps
}

# Process single Scenario ----------------------------------------------------
estim_list <- readRDS("sim_estim.rds")[[1]]
current_sim_data <- sim_data[[scenario]]

all_results <- future_map(seq_along(estim_list), function(sim_idx) {
  
  estim_obj <- estim_list[[sim_idx]]
  sim_obj <- current_sim_data[[sim_idx]]
  
  draws_result <- extract_draws_and_summary(estim_obj$pars, scenario, sim_idx)
  
  err_ind <- sim_obj$error_indicators %>% select(-id, -group)
  total_errors <- sum(err_ind == TRUE, na.rm = TRUE)
  total_possible <- sum(!is.na(err_ind))
  
  emp_error_row <- tibble(
    param_idx = 0,
    empirical_mean = total_errors / total_possible,
    empirical_cv = NA,
    group = NA,
    param_label = "probability of error"
  )
  
  delay_stats <- sim_obj$true_data %>%
    mutate(
      "onset to report" = as.numeric(report - onset),
      "onset to death" = as.numeric(death - onset),
      "onset to hospitalisation" = as.numeric(hospitalisation - onset),
      "hospitalisation to discharge" = as.numeric(discharge - hospitalisation),
      "hospitalisation to death" = as.numeric(death - hospitalisation)
    ) %>%
    pivot_longer(cols = contains(" to "),
                 names_to = "param_label",
                 values_to = "val") %>%
    inner_join(delay_mapping, by = c("group", "param_label")) %>%
    filter(!is.na(val)) %>%
    group_by(param_idx, group, param_label) %>%
    summarise(
      empirical_mean = mean(val),
      empirical_cv = sd(val) / mean(val),
      .groups = "drop"
    )
  
  empirical <- bind_rows(emp_error_row, delay_stats) %>%
    mutate(scenario = scenario, simulation = sim_idx)
  
  obs <- sim_obj$observed_data %>%
    mutate(scenario = scenario, simulation = sim_idx)
  
  err <- sim_obj$error_indicators %>% select(-id, -group)
  
  observed_pat <- obs %>%
    mutate(
      onset = case_when(
        is.na(onset) ~ "missing",
        err$onset == TRUE ~ "error",
        TRUE ~ "correct"
      ),
      hospitalisation = case_when(
        group %in% c("community-alive", "community-dead") ~ NA,
        is.na(hospitalisation) ~ "missing",
        err$hospitalisation == TRUE ~ "error",
        TRUE ~ "correct"
      ),
      report = case_when(
        is.na(report) ~ "missing",
        err$report == TRUE ~ "error",
        TRUE ~ "correct"
      ),
      death = case_when(
        group %in% c("community-alive", "hospitalised-alive") ~ NA,
        is.na(death) ~ "missing",
        err$death == TRUE ~ "error",
        TRUE ~ "correct"
      ),
      discharge = case_when(
        group != "hospitalised-alive" ~ NA,
        is.na(discharge) ~ "missing",
        err$discharge == TRUE ~ "error",
        TRUE ~ "correct"
      )
    ) %>%
    select(id, group, scenario, simulation, onset, hospitalisation,
           report, death, discharge)
  
  true_errors <- sim_obj$error_indicators
  obs_data <- sim_obj$observed_data
  est_error_array <- estim_obj$data$error_indicators
  post_prob_error <- apply(est_error_array, c(1, 2), mean, na.rm = TRUE)
  
  get_event_status <- function(thresh) {
    est_is_error <- post_prob_error > thresh
    
    tibble(
      scenario = scenario,
      simulation = sim_idx,
      threshold = paste0(thresh * 100, "% Threshold"),
      id = true_errors$id,
      group = true_errors$group
    ) %>%
      mutate(
        true_onset = case_when(
          is.na(obs_data$onset) ~ "missing",
          true_errors$onset == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_onset = ifelse(est_is_error[, 1], "error", "correct"),
        match_onset = ifelse(true_onset == "missing", NA, true_onset == estimated_onset),
        
        true_hospitalisation = case_when(
          is.na(true_errors$hospitalisation) ~ NA,
          is.na(obs_data$hospitalisation) ~ "missing",
          true_errors$hospitalisation == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_hospitalisation = case_when(
          is.na(true_errors$hospitalisation) ~ NA,
          est_is_error[, 2] == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        match_hospitalisation = ifelse(
          true_hospitalisation == "missing" | is.na(true_hospitalisation), NA,
          true_hospitalisation == estimated_hospitalisation
        ),
        
        true_report = case_when(
          is.na(obs_data$report) ~ "missing",
          true_errors$report == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_report = ifelse(est_is_error[, 3], "error", "correct"),
        match_report = ifelse(true_report == "missing", NA, true_report == estimated_report),
        
        true_death = case_when(
          is.na(true_errors$death) ~ NA,
          is.na(obs_data$death) ~ "missing",
          true_errors$death == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_death = case_when(
          is.na(true_errors$death) ~ NA,
          est_is_error[, 4] == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        match_death = ifelse(
          true_death == "missing" | is.na(true_death), NA,
          true_death == estimated_death
        ),
        
        true_discharge = case_when(
          is.na(true_errors$discharge) ~ NA ,
          is.na(obs_data$discharge) ~ "missing",
          true_errors$discharge == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_discharge = case_when(
          is.na(true_errors$discharge) ~ NA,
          est_is_error[, 5] == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        match_discharge = ifelse(
          true_discharge == "missing" | is.na(true_discharge), NA,
          true_discharge == estimated_discharge
        )
      )
  }
  
  event_status <- bind_rows(get_event_status(0.5), get_event_status(0.9))
  
  acc_matrix <- calc_acceptance_rate(estim_obj)
  acc_df <- as.data.frame(acc_matrix) %>%
    mutate(param_idx = row_number(),
           scenario = scenario,
           simulation = sim_idx) %>%
    pivot_longer(cols = -c(param_idx, scenario, simulation),
                 names_to = "chain",
                 values_to = "acceptance_rate")
  
  samples_df_full <- posterior::as_draws_df(estim_obj$full_chains$pars)
  diag_full <- posterior::summarise_draws(samples_df_full) %>%
    mutate(burnin_applied = FALSE, scenario = scenario, simulation = sim_idx)
  
  full_chains_no_burnin <- monty::monty_samples_thin(estim_obj$full_chains, burnin = burnin)
  samples_df_thin <- posterior::as_draws_df(full_chains_no_burnin$pars)
  diag_thin <- posterior::summarise_draws(samples_df_thin) %>%
    mutate(burnin_applied = TRUE, scenario = scenario, simulation = sim_idx)
  
  chain_diag_df <- bind_rows(diag_full, diag_thin)
  
  list(
    summary      = draws_result$summary,
    tidy_draws   = draws_result$tidy_draws,
    empirical    = empirical,
    observed_pat = observed_pat,
    event_status = event_status,
    acceptance   = acc_df,
    diagnostics  = chain_diag_df
  )
}, .options = furrr_options(
  seed = TRUE, 
  packages = c("dplyr", "tidyr", "posterior", "stringr", "monty", "abind") 
))

rm(estim_list)

# Extract outputs from the list
sim_summaries_raw <- bind_rows(lapply(all_results, `[[`, "summary"))
all_draws_raw <- bind_rows(lapply(all_results, `[[`, "tidy_draws"))
empirical_params <- bind_rows(lapply(all_results, `[[`, "empirical")) %>%
  apply_factor_levels()
observed_patterns <- bind_rows(lapply(all_results, `[[`, "observed_pat")) %>%
  apply_scenario_labels()
indiv_event_status <- bind_rows(lapply(all_results, `[[`, "event_status")) %>%
  apply_scenario_labels()
acceptance_rates_df <- bind_rows(lapply(all_results, `[[`, "acceptance"))
chain_diagnostics_df <- bind_rows(lapply(all_results, `[[`, "diagnostics"))

rm(all_results)

# Post-process ----------------------------------------------------------------
sim_summaries_mean <- sim_summaries_raw %>%
  filter(type == "mean") %>%
  select(-type) %>%
  rename(
    overall_mean_est = overall_mean,
    overall_q025_est = overall_q025,
    overall_q25_est = overall_q25,
    overall_q75_est = overall_q75,
    overall_q975_est = overall_q975,
    rhat_est = rhat,
    ess_bulk_est = ess_bulk,
    ess_tail_est = ess_tail
  )

sim_summaries_cv <- sim_summaries_raw %>%
  filter(type == "cv") %>%
  select(
    scenario, simulation, param_idx,
    overall_mean_cv = overall_mean,
    overall_q025_cv = overall_q025,
    overall_q975_cv = overall_q975,
    rhat_cv = rhat,
    ess_bulk_cv = ess_bulk,
    ess_tail_cv = ess_tail
  )

sim_summaries <- sim_summaries_mean %>%
  left_join(sim_summaries_cv,
            by = c("scenario", "simulation", "param_idx")) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels() %>%
  left_join(
    select(true_params, scenario, param_idx, true_mean, true_cv),
    by = c("scenario", "param_idx")
  ) %>%
  left_join(
    select(empirical_params, scenario, simulation, param_idx,
           empirical_mean, empirical_cv),
    by = c("scenario", "simulation", "param_idx")
  ) %>%
  mutate(
    bias_gt = overall_mean_est - true_mean,
    bias_emp = overall_mean_est - empirical_mean,
    cv_bias_gt = overall_mean_cv - true_cv,
    cv_bias_emp = overall_mean_cv - empirical_cv,
    cov95_gt = true_mean >= overall_q025_est & true_mean <= overall_q975_est,
    cov95_emp = empirical_mean >= overall_q025_est & empirical_mean <= overall_q975_est,
    cov50_gt = true_mean >= overall_q25_est & true_mean <= overall_q75_est,
    cov50_emp = empirical_mean >= overall_q25_est & empirical_mean <= overall_q75_est,
    cv_cov95_gt = true_cv >= overall_q025_cv & true_cv <= overall_q975_cv,
    width95 = overall_q975_est - overall_q025_est,
    cv_width95 = overall_q975_cv - overall_q025_cv
  )

all_draws <- all_draws_raw %>%
  pivot_wider(names_from = type, values_from = value) %>%
  rename(post_mean = mean, post_cv = cv) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels()

rm(all_draws_raw, sim_summaries_raw)
gc()

# Aggregate across simulations and assess convergence -------------------------
rhat_threshold <- 1.05
ess_threshold <- 200

agg_summaries <- sim_summaries %>%
  group_by(scenario, param_label, group) %>%
  summarise(
    n_sims = n(),
    avg_rhat = mean(rhat_est, na.rm = TRUE),
    min_ess = min(ess_bulk_est, na.rm = TRUE),
    across(c(bias_gt, bias_emp, cv_bias_gt, cv_bias_emp),
           list(avg = ~median(., na.rm = TRUE), 
                sd = ~sd(., na.rm = TRUE),
                rmse = ~sqrt(mean(.^2, na.rm = TRUE))),
           .names = "{.col}_{.fn}"),
    across(c(cov95_gt, cov95_emp, cov50_gt, cov50_emp),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_pct"),
    across(c(width95, cv_width95),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_avg"),
    .groups = "drop"
  ) %>%
  mutate(
    rhat_ok = avg_rhat <= rhat_threshold,
    ess_ok = min_ess >= ess_threshold,
    converged = rhat_ok & ess_ok
  )

convergence_issues <- agg_summaries %>%
  filter(!converged) %>%
  select(scenario, param_label, group, avg_rhat, min_ess, rhat_ok, ess_ok) %>%
  arrange(desc(avg_rhat))

scenario_convergence <- agg_summaries %>%
  group_by(scenario) %>%
  summarise(
    n_params = n(),
    n_converged = sum(converged),
    pct_converged = n_converged / n_params * 100,
    worst_rhat = max(avg_rhat),
    worst_ess = min(min_ess),
    .groups = "drop"
  )

indiv_obs_summary <- observed_patterns %>%
  pivot_longer(cols = c(onset, hospitalisation, report, death, discharge), 
               names_to = "event", 
               values_to = "status") %>%
  filter(!is.na(status)) %>%
  group_by(scenario, simulation, id, group) %>%
  summarise(
    n_events = n(),
    n_correct = sum(status == "correct"),
    n_errors = sum(status == "error"),
    n_missing = sum(status == "missing"),
    all_correct = all(status == "correct"),
    any_error = any(status == "error"),
    any_missing = any(status == "missing"),
    .groups = "drop"
  ) %>%
  mutate(
    pattern = case_when(
      n_correct == n_events ~ "All correct",
      n_errors == n_events ~ "All errors",
      n_missing == n_events ~ "All missing",
      TRUE ~ paste0(
        ifelse(n_correct > 0, paste0(n_correct, " correct"), ""),
        ifelse(n_correct > 0 & (n_errors > 0 | n_missing > 0), " + ", ""),
        ifelse(n_errors > 0, paste0(n_errors, " error",
                                    ifelse(n_errors > 1, "s", "")), ""),
        ifelse(n_errors > 0 & n_missing > 0, " + ", ""),
        ifelse(n_missing > 0, paste0(n_missing, " missing"), "")
      )
    )
  )

obs_pattern_summary <- indiv_obs_summary %>%
  group_by(scenario, group, pattern) %>%
  summarise(n_individuals = n(), .groups = "drop") %>%
  group_by(scenario, group) %>%
  mutate(
    total = sum(n_individuals),
    pct = n_individuals / total * 100
  ) %>%
  ungroup() %>%
  arrange(scenario, group, desc(n_individuals))

indiv_event_status_with_pattern <- indiv_event_status %>%
  left_join(
    select(indiv_obs_summary, scenario, simulation, id, pattern),
    by = c("scenario", "simulation", "id")
  )

event_match_summary <- indiv_event_status %>%
  pivot_longer(cols = starts_with("match_"), names_to = "event",
               values_to = "match") %>%
  mutate(event = gsub("match_", "", event)) %>%
  filter(!is.na(match)) %>%
  group_by(scenario, group, event) %>%
  summarise(
    n_total = n(),
    n_match = sum(match, na.rm = TRUE),
    pct_match = n_match / n_total * 100,
    .groups = "drop"
  )

event_confusion <- indiv_event_status %>%
  pivot_longer(
    cols = c(starts_with("true_"), starts_with("estimated_")),
    names_to = c(".value", "event"),
    names_pattern = "(true|estimated)_(.*)"
  ) %>%
  filter(!is.na(true), true != "missing") %>%
  group_by(scenario, group, threshold, event, true) %>%
  summarise(
    pred_correct = sum(estimated == "correct", na.rm = TRUE),
    pred_error = sum(estimated == "error", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_actual = pred_correct + pred_error,
    pct_accuracy = case_when(
      true == "correct" & total_actual > 0 ~ (pred_correct / total_actual) * 100,
      true == "error" & total_actual > 0 ~ (pred_error / total_actual) * 100,
      TRUE ~ NA_real_
    ),
    metric_type = case_when(
      true == "correct" ~ "Specificity",
      true == "error" ~ "Sensitivity"
    )
  ) %>%
  select(scenario, group, threshold, event, true_status = true,
         pred_correct, pred_error, total_actual, pct_accuracy, metric_type) %>%
  arrange(scenario, group, threshold, event, desc(true_status))


pattern_group_confusion <- indiv_event_status %>%
  left_join(
    select(indiv_obs_summary, scenario, simulation, id, pattern),
    by = c("scenario", "simulation", "id")
  ) %>%
  pivot_longer(
    cols = c(starts_with("true_"), starts_with("estimated_")),
    names_to = c(".value", "event"),
    names_pattern = "(true|estimated)_(.*)"
  ) %>%
  filter(!is.na(true), true != "missing") %>%
  group_by(scenario, group, pattern, true) %>%
  summarise(
    pred_correct = sum(estimated == "correct", na.rm = TRUE),
    pred_error   = sum(estimated == "error", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_actual = pred_correct + pred_error,
    pct_accuracy = case_when(
      true == "correct" & total_actual > 0 ~ (pred_correct / total_actual) * 100,
      true == "error"   & total_actual > 0 ~ (pred_error / total_actual) * 100,
      TRUE ~ NA
    )
  )

indiv_performance <- indiv_event_status %>%
  pivot_longer(
    cols = c(starts_with("true_"), starts_with("estimated_")),
    names_to = c(".value", "event"),
    names_pattern = "(true|estimated)_(.*)"
  ) %>%
  filter(!is.na(true), true != "missing") %>%
  group_by(scenario, simulation, threshold, id, group) %>%
  summarise(
    n_dates = n(),
    true_has_error = any(true == "error"),
    est_has_error = any(estimated == "error"),
    .groups = "drop"
  ) %>%
  filter(n_dates >= 2) %>%
  group_by(scenario, simulation, group, threshold) %>%
  summarise(
    total_truly_correct = sum(!true_has_error),
    correctly_pred_correct = sum(!true_has_error & !est_has_error),
    specificity = ifelse(total_truly_correct > 0, 
                         (correctly_pred_correct / total_truly_correct), NA),
    total_truly_error = sum(true_has_error),
    correctly_pred_error = sum(true_has_error & est_has_error),
    sensitivity = ifelse(total_truly_error > 0, 
                         (correctly_pred_error / total_truly_error), NA),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(specificity, sensitivity), 
               names_to = "metric_type", 
               values_to = "accuracy") %>%
  mutate(metric_type = stringr::str_to_title(metric_type))

low_ess_threshold <- 200
problem_sims <- sim_summaries %>%
  filter(ess_bulk_est < low_ess_threshold | rhat_est > rhat_threshold) %>%
  distinct(scenario, param_label, group, simulation,
           ess_bulk_est, rhat_est, true_mean, overall_mean_est) %>%
  rename(est_mean = overall_mean_est) %>%
  mutate(rhat_ok = rhat_est < rhat_threshold,
         ess_ok  = ess_bulk_est > low_ess_threshold)

# Save outputs -----------------------------------------------------------
saveRDS(true_params, "results/true_params.rds")
saveRDS(all_draws, "results/all_draws.rds")
saveRDS(sim_summaries, "results/sim_summaries.rds")
saveRDS(agg_summaries, "results/agg_summaries.rds")
saveRDS(convergence_issues, "results/convergence_issues.rds")
saveRDS(scenario_convergence, "results/scenario_convergence.rds")
saveRDS(acceptance_rates_df, "results/acceptance_rates.rds")
saveRDS(chain_diagnostics_df, "results/chain_diagnostics.rds")
saveRDS(observed_patterns, "results/observed_patterns.rds")
saveRDS(indiv_obs_summary, "results/indiv_obs_summary.rds")
saveRDS(obs_pattern_summary, "results/obs_pattern_summary.rds")
saveRDS(indiv_event_status, "results/indiv_event_status.rds")
saveRDS(event_match_summary, "results/event_match_summary.rds")
saveRDS(event_confusion, "results/event_confusion.rds")
saveRDS(pattern_group_confusion, "results/pattern_confusion.rds")
saveRDS(indiv_performance, "results/indiv_performance_summary.rds")
saveRDS(problem_sims, "results/convergence_issues_by_individual.rds")

if (is.null(cl)) plan(sequential)