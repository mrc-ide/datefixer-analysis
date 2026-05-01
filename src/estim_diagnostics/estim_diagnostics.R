library(orderly)
library(dplyr)
library(tidyr)
library(patchwork)
library(purrr)
library(ggplot2)
library(glue)
library(posterior)
library(stringr)
library(ggrastr)
library(furrr)
library(future)

options(future.globals.maxSize = Inf)

pars <- orderly_parameters(n_steps = NULL, burnin = NULL, thinning_factor = NULL,
                           scenarios = NULL)

n_steps <- n_steps
burnin <- burnin
thinning_factor <- thinning_factor
scenarios <- strsplit(scenarios, ",")[[1]]

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

# Scenario name mapping for better plot labels
scenario_labels <- c(
  "low_missingness" = "Low missingness (0.05)",
  "very_small_sample" = "Very small groups (n = 10)",
  "small_sample" = "Small groups (n = 20)",
  "moderate_sample" = "Moderate groups (n = 50)",
  "high_error" = "High error (0.2)",
  "short_delays" = "Short delays (0.5x baseline)",
  "low_variability" = "Low variability (0.5x baseline cv)",
  "baseline" = "Baseline", # error = 0.05, missing = 0.2
  "low_error" = "Low error (0.02)",
  "no_error" = "Missing dates only (0.2)",
  "no_missing" = "Errors only (0.05)",
  "no_error_no_missing" = "No errors or missing dates",
  "very_large_sample" = "Very large groups (n = 500)",
  "long_delays" = "Long delays (2x baseline)",
  "high_variability" = "High variability (2x baseline cv)",
  "lognormal_delays" = "Lognormal delays"
)

scenario_labels <- scenario_labels[intersect(names(scenario_labels), scenarios)]
scenario_labels <- factor(scenario_labels, levels = scenario_labels)

for (s in scenarios) {
  orderly_dependency("sim_estim",
                     "latest(parameter:scenario == environment:s && 
                     parameter:n_steps == environment:n_steps &&
                     parameter:burnin == environment:burnin &&
                     parameter:thinning_factor == environment:thinning_factor)",
                     c("sim_estim_${s}.rds" = "sim_estim.rds"))
}

orderly_artefact(files = c("results/figures/trace_error.pdf",
                           "results/figures/trace_delays_10.pdf",
                           "results/figures/trace_delays_all.pdf",
                           "results/figures/bias_plot_delays_gt.pdf",
                           "results/figures/bias_plot_error_gt.pdf",
                           "results/figures/bias_plot_cv_gt.pdf",
                           "results/figures/coverage_plot.pdf",
                           "results/sim_summaries.rds",
                           "results/agg_summaries.rds",
                           "results/figures/posterior_delay_mean.pdf",
                           "results/figures/posterior_cv.pdf",
                           "results/figures/posterior_prob_error.pdf",
                           "results/figures/observed_patterns.pdf",
                           "results/observed_patterns.rds",
                           "results/indiv_obs_summary.rds",
                           "results/obs_pattern_summary.rds",
                           "results/indiv_event_status.rds",
                           "results/event_match_summary.rds",
                           "results/event_confusion.rds",
                           "results/pattern_confusion.rds",
                           "results/convergence_issues.rds",
                           "results/scenario_convergence.rds",
                           "results/figures/ess_plot.pdf",
                           "results/convergence_issues_by_individual.rds",
                           "results/figures/problem_traces",
                           "results/figures/rhat_vs_ess.pdf",
                           "results/figures/width_vs_ess.pdf"),
                 description = "Analysis outputs")

dir.create("results", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE)

# Read in dependencies -------------------------------------------------------
sim_params <- readRDS("sim_params.rds")[scenarios]
sim_data <- readRDS("sim_data.rds")[scenarios]

# Delay mapping (defines which param_idx corresponds to which delay) ---------
delay_mapping <- tribble(
  ~param_idx, ~delay_from,       ~delay_to,           ~group,
  1,          "onset",             "report",            "community-alive",
  2,          "onset",             "report",            "community-dead",
  3,          "onset",             "report",            "hospitalised-alive",
  4,          "onset",             "report",            "hospitalised-dead",
  5,          "onset",             "death",             "community-dead",
  6,          "onset",             "hospitalisation",   "hospitalised-alive",
  7,          "hospitalisation",   "discharge",         "hospitalised-alive",
  8,          "onset",             "hospitalisation",   "hospitalised-dead",
  9,          "hospitalisation",   "death",             "hospitalised-dead"
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

# Extract true parameter values ----------------------------------------------
true_params <- map_dfr(scenarios, function(scenario_name) {
  params <- sim_params[[scenario_name]]
  # Delays
  tibble(
    param_idx = 1:nrow(params$delay_info),
    true_mean = params$delay_info$mean,
    true_cv   = params$delay_info$cv
  ) %>% 
    # Error
    bind_rows(tibble(
      param_idx = 0,
      true_mean = params$error_params$prob_error,
      true_cv   = NA
    )) %>% 
    mutate(scenario = scenario_name)
}) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels()

# Helper function to extract, tidy and summarise draws (move to utils) -------
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

# Create grid of simulations --------------------------------------------------
sim_grid <- expand.grid(
  scenario_name = scenarios,
  sim_idx = seq_len(length(sim_data[[1]])),
  stringsAsFactors = FALSE
)

all_results <- map(scenarios, function(s) {
  
  message(glue("Processing scenario: {s}"))
  
  estim_list <- readRDS(paste0("sim_estim_", s, ".rds"))[[1]]
  current_sim_data <- sim_data[[s]]
  
  # Loop through simulations in this scenario
  res <- map(seq_along(estim_list), function(sim_idx) {
    
    estim_obj <- estim_list[[sim_idx]]
    sim_obj <- current_sim_data[[sim_idx]]
    
    # extract draws and summary of estimates
    draws_result <- extract_draws_and_summary(estim_obj$pars, s, sim_idx)
    
    # extract empirical params from simulated data
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
      mutate(scenario = s, simulation = sim_idx)
    
    # observed patterns
    obs <- sim_obj$observed_data %>%
      mutate(scenario = s, simulation = sim_idx)
    
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
    
    # True vs estimated error status (excluding missing)
    true_errors <- sim_obj$error_indicators
    obs_data <- sim_obj$observed_data
    est_error_array <- estim_obj$data$error_indicators
    post_prob_error <- apply(est_error_array, c(1, 2), mean, na.rm = TRUE)
    est_is_error <- post_prob_error > 0.9
    
    event_status <- tibble(
      scenario = s,
      simulation = sim_idx,
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
        match_onset = ifelse(true_onset == "missing", NA,
                             true_onset == estimated_onset),
        
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
        match_report = ifelse(true_report == "missing", NA,
                              true_report == estimated_report),
        
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
    
    list(
      summary      = draws_result$summary,
      tidy_draws   = draws_result$tidy_draws,
      empirical    = empirical,
      observed_pat = observed_pat,
      event_status = event_status
    )
  })
  rm(estim_list)
  gc()
  return(res)
}) %>% list_flatten()

sim_summaries_raw <- bind_rows(lapply(all_results, `[[`, "summary"))
all_draws_raw <- bind_rows(lapply(all_results, `[[`, "tidy_draws"))
empirical_params <- bind_rows(lapply(all_results, `[[`, "empirical")) %>%
  apply_factor_levels()
observed_patterns <- bind_rows(lapply(all_results, `[[`, "observed_pat")) %>%
  apply_scenario_labels()
indiv_event_status <- bind_rows(lapply(all_results, `[[`, "event_status")) %>%
  apply_scenario_labels()

rm(all_results)
gc()

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
  # convergence
  mutate(
    rhat_ok = avg_rhat <= rhat_threshold,
    ess_ok = min_ess >= ess_threshold,
    converged = rhat_ok & ess_ok
  )

# problematic parameters
convergence_issues <- agg_summaries %>%
  filter(!converged) %>%
  select(scenario, param_label, group, avg_rhat, min_ess, rhat_ok, ess_ok) %>%
  arrange(desc(avg_rhat))

# overall convergence by scenario
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

# Save outputs ---------------------------------------------------------------
saveRDS(sim_summaries, "results/sim_summaries.rds")
saveRDS(agg_summaries, "results/agg_summaries.rds")
saveRDS(convergence_issues, "results/convergence_issues.rds")
saveRDS(scenario_convergence, "results/scenario_convergence.rds")

# Observed patterns summaries ------------------------------------------------

# by individual
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

# across individuals
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

# for each scenario, group and event - % of error indicators that match truth
# (excluding missing dates)
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

# separate accuracy into sensitivity (ability to identify true errors) and
# specificity (ability to identify truly correct dates)
event_confusion <- indiv_event_status %>%
  pivot_longer(
    cols = c(starts_with("true_"), starts_with("estimated_")),
    names_to = c(".value", "event"),
    names_pattern = "(true|estimated)_(.*)"
  ) %>%
  filter(!is.na(true), true != "missing") %>%
  group_by(scenario, group, event, true, estimated) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = estimated, values_from = n, values_fill = 0,
              names_prefix = "pred_") %>%
  mutate(
    total_actual = pred_correct + pred_error,
    pct_accuracy = case_when(
      true == "correct" ~ (pred_correct / total_actual) * 100,
      true == "error" ~ (pred_error / total_actual) * 100
    ),
    metric_type = case_when(
      true == "correct" ~ "Specificity",
      true == "error" ~ "Sensitivity"
    )
  ) %>%
  select(scenario, group, event, true_status = true, pred_correct, pred_error,
         total_actual, pct_accuracy, metric_type) %>%
  arrange(scenario, group, event, desc(true_status))

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
  group_by(scenario, group, pattern, true, estimated) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = estimated, values_from = count, values_fill = 0,
              names_prefix = "pred_") %>%
  mutate(
    total_actual = pred_correct + pred_error,
    pct_accuracy = case_when(
      true == "correct" ~ (pred_correct / total_actual) * 100,
      true == "error" ~ (pred_error / total_actual) * 100
    )
  )

saveRDS(observed_patterns, "results/observed_patterns.rds")
saveRDS(indiv_obs_summary, "results/indiv_obs_summary.rds")
saveRDS(obs_pattern_summary, "results/obs_pattern_summary.rds")
saveRDS(indiv_event_status, "results/indiv_event_status.rds")
saveRDS(event_match_summary, "results/event_match_summary.rds")
saveRDS(event_confusion, "results/event_confusion.rds")
saveRDS(pattern_group_confusion, "results/pattern_confusion.rds")


# Plots ----------------------------------------------------------------

# Trace plot: Prob error
trace_prob_error <- all_draws %>%
  filter(param_idx == 0 & iteration > 100) %>%
  left_join(select(true_params, scenario, param_idx, true_mean),
            by = c("scenario", "param_idx")) %>%
  mutate(sim_chain = paste(simulation, chain, sep = "_")) %>%
  ggplot(aes(x = iteration, y = post_mean,
             colour = factor(chain), group = sim_chain)) +
  rasterise(geom_line(alpha = 0.3), dpi = 300) +
  geom_hline(aes(yintercept = true_mean),
             linetype = "dashed", linewidth = 0.8, colour = "black") +
  facet_grid(cols = vars(scenario), scales = "free_y") +
  labs(y = "Probability of Error", x = "Iteration",
       title = "Trace plots for probability of error (100 simulations)",
       subtitle = glue("MCMC: {n_steps} steps, {burnin} burn-in"),
       colour = "Chain") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("results/figures/trace_error.pdf", trace_prob_error, width = 14, height = 4)

# Trace plot: Delays
make_trace_plot <- function(data, y_var, true_var, title, y_label,
                            add_symbols = FALSE) {
  p <- ggplot(data, aes(x = iteration, y = {{y_var}})) +
    rasterise(geom_line(alpha = 0.3,
                        aes(colour = param_label,
                            group = interaction(param_label, simulation))),
              dpi = 300) +
    geom_hline(aes(yintercept = {{true_var}}, colour = param_label),
               linetype = "dashed", linewidth = 0.8) +
    facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
    labs(y = y_label, x = "Iteration", colour = "Delay", title = title) +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))
  
  if (add_symbols) {
    true_points <- data %>%
      distinct(scenario, group, param_label, {{true_var}}) %>%
      crossing(iteration = c(min(data$iteration), max(data$iteration)))
    
    p <- p + geom_point(data = true_points,
                        aes(x = iteration, y = {{true_var}}, fill = param_label),
                        shape = 23, size = 3, colour = "black",
                        inherit.aes = FALSE, show.legend = FALSE)
  }
  p
}

trace_data_base <- all_draws %>%
  filter(param_idx > 0) %>%
  left_join(select(true_params, scenario, param_idx, group, true_mean),
            by = c("scenario", "param_idx", "group"))

# if burnin is 0 then first 100 iterations removed from trace plots
if (burnin == 0) trace_data_base <- trace_data_base %>% filter(iteration > 100)

trace_10 <- make_trace_plot(
  trace_data_base %>% filter(simulation <= 10),
  post_mean, true_mean,
  "Trace plots (first 10 simulations)", "Mean Delay",
  add_symbols = TRUE
)

trace_all <- make_trace_plot(
  trace_data_base, #%>% filter(iteration %% 5 == 0),
  post_mean, true_mean,
  "Trace plots (all simulations)", "Mean Delay",
  add_symbols = TRUE
)

ggsave("results/figures/trace_delays_10.pdf", trace_10, width = 14, height = 10)
ggsave("results/figures/trace_delays_all.pdf", trace_all, width = 14, height = 10)

# Bias plots
make_bias_plot <- function(data, bias_avg_col, bias_sd_col, title, subtitle) {
  ggplot(data, aes(x = param_label, y = {{bias_avg_col}}, colour = param_label)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    geom_errorbar(aes(ymin = {{bias_avg_col}} - {{bias_sd_col}}, 
                      ymax = {{bias_avg_col}} + {{bias_sd_col}}), 
                  width = 0.3) +
    facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
    labs(title = title,
         subtitle = subtitle,
         y = "Median Bias",
         x = "",
         colour = "Parameter") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))
}

make_error_bias_plot <- function(data, bias_col, sd_col, title, subtitle) {
  ggplot(data, aes(x = scenario, y = {{bias_col}})) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(ymin = {{bias_col}} - {{sd_col}}, 
                      ymax = {{bias_col}} + {{sd_col}}), 
                  width = 0.15, colour = "midnightblue", alpha = 0.5) +
    geom_point(size = 2.5, colour = "midnightblue") +
    labs(title = title, subtitle = subtitle, y = "Median Bias", x = "") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))
}

# Compare to true mean delay (ground truth)
ggsave("results/figures/bias_plot_delays_gt.pdf",
       agg_summaries %>% filter(!param_label %in% "probability of error") %>%
         make_bias_plot(bias_gt_avg,  bias_gt_sd,
                        "Median Bias of Delay Parameters (+/- SD)",
                        "Compared to Ground Truth"),
       width = 10, height = 8)

# # Compare to mean delay of simulated true data (sample truth)
# ggsave("results/figures/bias_plot_delays_emp.pdf",
#        agg_summaries %>% filter(!param_label %in% "probability of error") %>%
#          make_bias_plot(bias_emp_avg, bias_emp_sd,
#                         "Median Bias of Delay Parameters (+/- SD)",
#                         "Compared to Sample Truth"),
#        width = 10, height = 8)

# Compare error probability to ground truth
ggsave("results/figures/bias_plot_error_gt.pdf",
       agg_summaries %>% filter(param_label %in% "probability of error") %>%
         make_error_bias_plot(bias_gt_avg,  bias_gt_sd,
                              "Median Bias: Probability of Error",
                              "Compared to Ground Truth"),
       width = 14, height = 4)

# # Compare error probability to sample truth
# ggsave("results/figures/bias_plot_error_emp.pdf",
#        agg_summaries %>% filter(param_label %in% "probability of error") %>%
#          make_error_bias_plot(bias_emp_avg, bias_emp_sd,
#                               "Median Bias: Probability of Error",
#                               "Compared to Sample Truth"),
#        width = 8, height = 4)

# CV bias plots
ggsave("results/figures/bias_plot_cv_gt.pdf",
       agg_summaries %>% filter(!param_label %in% "probability of error") %>%
         make_bias_plot(cv_bias_gt_avg,  cv_bias_gt_sd,
                        "Median Bias of CV Parameters (+/- SD)",
                        "Compared to Ground Truth"),
       width = 10, height = 8)

# ggsave("results/figures/bias_plot_cv_emp.pdf",
#        agg_summaries %>% filter(!param_label %in% "probability of error") %>%
#          make_bias_plot(cv_bias_emp_avg, cv_bias_emp_sd,
#                         "Median Bias of CV Parameters (+/- SD)",
#                         "Compared to Sample Truth"),
#        width = 10, height = 8)

# Coverage plots
make_coverage_plot <- function(data, cov95_col, cov50_col, subtitle) {
  coverage_data <- data %>%
    select(scenario, group, param_label, n_sims, 
           cov95 = {{cov95_col}}, cov50 = {{cov50_col}}) %>%
    pivot_longer(cols = c(cov95, cov50),
                 names_to = "metric",
                 values_to = "coverage") %>%
    mutate(interval = ifelse(metric == "cov95", "95% CrI", "50% CrI"),
           n_success = round(coverage * n_sims)) %>%
    rowwise() %>%
    mutate(
      binom_ci = list(binom.test(n_success, n_sims, conf.level = 0.95)$conf.int),
      ci_lower = binom_ci[1],
      ci_upper = binom_ci[2]
    ) %>%
    ungroup() %>%
    select(-binom_ci, -n_success, -metric)
  
  ggplot(coverage_data, 
         aes(x = param_label, y = coverage, 
             colour = param_label, shape = interval)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.3, alpha = 0.6, 
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0.95, linetype = "dashed",
               colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed",
               colour = "lightseagreen", alpha = 0.8) +
    facet_grid(rows = vars(group), cols = vars(scenario)) +
    labs(title = "Coverage of Credible Intervals",
         subtitle = subtitle,
         y = "Coverage Probability",
         x = "",
         shape = "Interval") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    theme_minimal() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          panel.border = element_rect(colour = "darkgrey", fill = NA,
                                      linewidth = 1)) +
    guides(colour = "none")
}

# Ground truth coverage
ggsave(
  "results/figures/coverage_plot.pdf",
  agg_summaries %>% filter(!param_label %in% "probability of error") %>%
    make_coverage_plot(
      cov95_gt_pct, cov50_gt_pct,
      "True parameters (ground truth). Error bars: 95% binomial confidence intervals"
      ),
  width = 10, height = 8)

# # Empirical coverage
# ggsave("results/figures/coverage_plot_emp.pdf",
#        agg_summaries %>% filter(!param_label %in% "probability of error") %>%
#          make_coverage_plot(
#            cov95_emp_pct, cov50_emp_pct,
#            "Empirical values (sample truth). Error bars: 95% binomial confidence intervals"
#            ),
#        width = 10, height = 8)

# Posterior density plots ------------------------------------------------------

# Aggregate draws across simulations for cleaner visualisation
posterior_data <- all_draws %>%
  filter(param_label != "probability of error")

# Handle long tails (1% and 99% boundaries)
posterior_data_trimmed <- posterior_data %>%
  group_by(param_label, group) %>%
  mutate(lower_bound_delay = quantile(post_mean, 0.01, na.rm = TRUE),
         upper_bound_delay = quantile(post_mean, 0.99, na.rm = TRUE),
         lower_bound_cv = quantile(post_cv, 0.01, na.rm = TRUE),
         upper_bound_cv = quantile(post_cv, 0.99, na.rm = TRUE)) %>%
  filter(post_mean >= lower_bound_delay & post_mean <= upper_bound_delay &
           post_cv >= lower_bound_cv & post_cv <= upper_bound_cv) %>%
  ungroup()

ref_lines <- true_params %>%
  filter(param_idx > 0) %>%
  group_by(param_label, group) %>%
  mutate(mean_shared = n_distinct(true_mean) == 1,
         cv_shared = n_distinct(true_cv) == 1) %>%
  ungroup()

# Mean delay posteriors
ggsave("results/figures/posterior_delay_mean.pdf",
       ggplot(posterior_data_trimmed,
              aes(x = post_mean, colour = scenario)) +
         geom_density() +
         geom_vline(data = ref_lines %>% filter(mean_shared) %>% distinct(param_label, group, true_mean),
                    aes(xintercept = true_mean),
                    colour = "black", linetype = "dashed", linewidth = 0.8) +
         geom_vline(data = ref_lines %>% filter(!mean_shared),
                    aes(xintercept = true_mean, colour = scenario),
                    linetype = "dashed", linewidth = 0.8) +
         facet_grid(rows = vars(group), cols = vars(param_label), scales = "free") +
         labs(title = "Posterior Distributions: Mean Delay",
              subtitle = "Dashed line = true value. Densities across all simulations.",
              x = "Mean Delay (days)", y = "Density", colour = "Scenario") +
         theme_minimal() +
         theme(strip.text = element_text(size = 7, face = "bold"),
               axis.text.x = element_text(angle = 45, hjust = 1),
               panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)),
       width = 14, height = 10)

# CV delay posteriors
ggsave("results/figures/posterior_cv.pdf",
       ggplot(posterior_data_trimmed,
              aes(x = post_cv, colour = scenario)) +
         geom_density() +
         geom_vline(data = ref_lines %>% filter(cv_shared) %>% distinct(param_label, group, true_cv),
                    aes(xintercept = true_cv),
                    colour = "black", linetype = "dashed", linewidth = 0.8) +
         geom_vline(data = ref_lines %>% filter(!cv_shared),
                    aes(xintercept = true_cv, colour = scenario),
                    linetype = "dashed", linewidth = 0.8) +
         facet_grid(rows = vars(group), cols = vars(param_label), scales = "free") +
         labs(title    = "Posterior Distributions: CV",
              subtitle = "Dashed line = true value. Densities across all simulations.",
              x = "Coefficient of Variation", y = "Density", colour = "Scenario") +
         theme_minimal() +
         theme(strip.text  = element_text(size = 7, face = "bold"),
               axis.text.x = element_text(angle = 45, hjust = 1),
               panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)),
       width = 14, height = 10)


# Error probability posteriors
ggsave("results/figures/posterior_prob_error.pdf",
       all_draws %>%
         filter(param_label %in% "probability of error") %>%
         left_join(true_params %>%
                     filter(param_idx == 0) %>%
                     select(scenario, true_mean), by = "scenario") %>%
         ggplot(aes(x = post_mean, fill = scenario, colour = scenario)) +
         geom_density(alpha = 0.3) +
         geom_vline(aes(xintercept = true_mean), linetype = "dashed", linewidth = 0.8) +
         facet_wrap(~scenario, scales = "free_y", nrow = 1) +
         scale_x_continuous(expand = c(0.005, 0), limits = c(0, NA)) +
         scale_y_continuous(expand = c(0, 0.05)) +
         labs(title = "Posterior Distributions: Probability of Error",
              subtitle = "Dashed line = true value. Densities across all simulations.",
              x = "Probability of Error", y = "Density",
              fill = "Scenario", colour = "Scenario") +
         theme_minimal() +
         theme(strip.text = element_text(size = 10, face = "bold"),
               legend.position = "none",
               panel.border = element_rect(
                 colour = "darkgrey", fill = NA, linewidth = 1
                 )),
       width = 14, height = 4)


# Observed patterns plot
ggsave("results/figures/observed_patterns.pdf",
       obs_pattern_summary %>%
         ggplot(aes(x = reorder(pattern, n_individuals), y = pct, fill = pattern)) +
         geom_col() +
         geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 2.5) +
         coord_flip() +
         facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
         labs(title = "Check simulated error/missingness patterns",
              x = "", y = "Percentage of Individuals (%)", fill = "Pattern") +
         theme_minimal() +
         theme(strip.text      = element_text(size = 9, face = "bold"),
               legend.position = "none",
               panel.border    = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)),
       width = 14, height = 7)

# ESS plot
ggsave("results/figures/ess_plot.pdf",
       sim_summaries %>%
         ggplot(aes(x = scenario, y = ess_bulk_est, fill = scenario)) +
         geom_violin(alpha = 0.3, scale = "width") +
         geom_jitter(aes(colour = scenario), width = 0.2, alpha = 0.5, size = 1) +
         geom_hline(yintercept = 200, linetype = "dashed", colour = "black", linewidth = 0.8) +
         facet_wrap(~param_label, scales = "free_y") +
         labs(
           title = "Distribution of effective sample size across simulations",
           subtitle = "Red line = threshold of 200",
           y = "ESS",
           x = "") +
         theme_bw() +
         theme(
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "none",
           strip.text = element_text(face = "bold")
           ),
       width = 14, height = 7)

# problematic runs
low_ess_threshold <- 200
rhat_threshold <- 1.05
problem_sims <- sim_summaries %>%
  filter(ess_bulk_est < low_ess_threshold | rhat_est > rhat_threshold) %>%
  distinct(scenario, param_label, group, simulation,
           ess_bulk_est, rhat_est, true_mean, overall_mean_est) %>%
  rename(est_mean = overall_mean_est) %>%
  mutate(rhat_ok = rhat_est < rhat_threshold,
         ess_ok  = ess_bulk_est > low_ess_threshold)

saveRDS(problem_sims, "results/convergence_issues_by_individual.rds")

# problematic trace plots
problem_dir <- "results/figures/problem_traces"
dir.create(problem_dir, recursive = TRUE, showWarnings = FALSE)

  # unique scenario/simulations that had issues
problem_combos <- problem_sims %>%
  distinct(scenario, simulation)

true_params_plot <- true_params %>%
  select(scenario, param_label, group, true_mean) %>%
  mutate(group = forcats::fct_na_value_to_level(group, "Global"))

if (nrow(problem_combos) > 0) {
  
  for(i in 1:nrow(problem_combos)) {
    scen <- as.character(problem_combos$scenario[i])
    sim <- problem_combos$simulation[i]
    
    plot_data <- all_draws %>%
      filter(scenario == scen, simulation == sim) %>%
      mutate(group = forcats::fct_na_value_to_level(group, "Global"))
    
    failed_params <- problem_sims %>%
      filter(scenario == scen, simulation == sim) %>%
      pull(param_label) %>%
      unique() %>%
      paste(collapse = ", ")
    
    p <- ggplot(plot_data, aes(x = iteration, y = post_mean, colour = factor(chain))) +
      rasterise(geom_line(alpha = 0.6, linewidth = 0.5), dpi = 300) +
      geom_hline(data = true_params_plot %>% filter(scenario == scen),
                 aes(yintercept = true_mean), linetype = "dashed", colour = "black", linewidth = 0.8) +
      facet_grid(rows = vars(group), cols = vars(param_label), scales = "free_y") +
      labs(title = glue("Diagnostic Trace: {scen} (Sim {sim})"),
           subtitle = glue("Failed params: {failed_params}\nDashed line = True Value"),
           x = "Iteration", y = "Estimate", colour = "Chain") +
      theme_bw() +
      theme(strip.text = element_text(size = 7, face = "bold"),
            legend.position = "bottom")
    
    clean_scen <- str_replace_all(scen, "[^[:alnum:]]", "_")
    file_name <- file.path(problem_dir, glue("trace_{clean_scen}_sim{sim}.pdf"))
    
    ggsave(file_name, plot = p, width = 16, height = 10)
  }
}

# see if low ess correlates with poor rhat
ggsave("results/figures/rhat_vs_ess.pdf",
       sim_summaries %>%
         ggplot(aes(x = ess_bulk_est, y = rhat_est)) +
         geom_point(aes(colour = scenario), alpha = 0.4) +
         geom_vline(xintercept = 200, linetype = "dotted") +
         geom_hline(yintercept = 1.05, linetype = "dotted") +
         facet_grid(rows = vars(scenario), cols = vars(param_label),
                    scales = "free_y") +
         labs(title = "Rhat vs ESS",
              subtitle = "Top-left quadrant = above rhat and below bulk ess thresholds",
              y = "Rhat", x = "Bulk ESS") +
         theme_bw() +
         theme(strip.text = element_text(size = 8, face = "bold"),
               legend.position = "none"),
       width = 12, height = 8)

# see if low ESS correlates with wider crIs
ggsave("results/figures/width_vs_ess.pdf",
       sim_summaries %>%
         mutate(is_low_ess = ess_bulk_est < low_ess_threshold) %>%
         ggplot(aes(x = is_low_ess, y = width95, colour = is_low_ess)) +
         facet_grid(rows = vars(scenario), cols = vars(param_label),
                    scales = "free_y") +
         geom_jitter(width = 0.2, alpha = 0.6, size = 0.8) +
         labs(title = "Low ESS vs credible intervals width",
              x = "ESS < 200", y = "Width of 95% CrI") +
         theme_bw() +
         theme(strip.text = element_text(size = 8, face = "bold"),
               legend.position = "none"),
       width = 12, height = 8)

