# SANITY CHECK

library(orderly)
library(dplyr)
library(tidyr)
library(patchwork)
library(purrr)
library(ggplot2)
library(glue)
library(posterior)

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

scenarios <- c("baseline", "no_error", "no_missing", "no_error_no_missing")

# Scenario name mapping for better plot labels
scenario_labels <- c(
  "baseline" = "Baseline",
  "no_error" = "Missing dates only",
  "no_missing" = "Errors only",
  "no_error_no_missing" = "No errors or missing dates"
)

for (s in scenarios) {
  orderly_dependency("sim_estim",
                     "latest(parameter:scenario == environment:s)",
                     c("sim_estim_${s}.rds" = "sim_estim.rds"))
}

orderly_artefact(files = c("results/figures/trace_error.pdf",
                           "results/figures/trace_delays_10.pdf",
                           "results/figures/trace_delays_all.pdf",
                           "results/figures/bias_plot_delays_gt.pdf",
                           "results/figures/bias_plot_delays_emp.pdf",
                           "results/figures/bias_plot_error_gt.pdf",
                           "results/figures/bias_plot_error_emp.pdf",
                           "results/figures/bias_plot_cv_gt.pdf",
                           "results/figures/bias_plot_cv_emp.pdf",
                           "results/figures/coverage_plot.pdf",
                           "results/figures/coverage_plot_emp.pdf",
                           "results/sim_summaries.rds",
                           "results/agg_summaries.rds",
                           "results/figures/posterior_mean_delay.pdf",
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
                           "results/scenario_convergence.rds"),
                 description = "Analysis outputs")

dir.create("results", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE)

# Read in dependencies -------------------------------------------------------
sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")
sim_estim <- setNames(
  lapply(scenarios, function(s) readRDS(paste0("sim_estim_", s, ".rds"))[[1]]),
  scenarios
)

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

# Extract true parameter values ----------------------------------------------
true_params <- map_dfr(scenarios, function(scenario_name) {
  params <- sim_params[[scenario_name]]
  # Delays
  tibble(
    param_idx = 1:nrow(params$delay_params),
    true_mean = params$delay_params$delay_mean,
    true_cv   = params$delay_params$delay_cv
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

# Extract empirical params from simulated data --------------------------------
empirical_params <- map_dfr(scenarios, function(scenario_name) {
  map_dfr(seq_along(sim_data[[scenario_name]]), function(sim_idx) {
    
    sim_obj <- sim_data[[scenario_name]][[sim_idx]]
    
    err_ind <- sim_obj$error_indicators %>% select(-id, -group)
    total_errors <- sum(err_ind == TRUE, na.rm = TRUE)
    total_possible_dates <- sum(!is.na(err_ind))
    
    emp_error_row <- tibble(
      param_idx = 0,
      empirical_mean = total_errors / total_possible_dates,
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
      pivot_longer(cols = contains(" to "), names_to = "param_label", values_to = "val") %>%
      inner_join(delay_mapping, by = c("group", "param_label")) %>%
      filter(!is.na(val)) %>%
      group_by(param_idx, group, param_label) %>%
      summarise(empirical_mean = mean(val),
                empirical_cv = sd(val) / mean(val), .groups = "drop")
    
    bind_rows(emp_error_row, delay_stats) %>%
      mutate(scenario = scenario_name, simulation = sim_idx)
  })
}) %>% 
  apply_factor_levels()

# MCMC Extraction ------------------------------------------------------------
sim_summaries <- map_dfr(names(sim_estim), function(scenario_name) {
  map_dfr(seq_along(sim_estim[[scenario_name]]), function(sim_idx) {
    
    # pars array [parameters, iterations, chains]
    pars_array <- sim_estim[[scenario_name]][[sim_idx]]$pars
    
    # transpose to [iterations, chains, parameters] for posterior package
    pars_transposed <- aperm(pars_array, c(2, 3, 1))

    # Convert to draws format
    draws_df <- as_draws_df(pars_transposed)
    
    summarise_draws(
      draws_df, 
      "mean", 
      ~quantile(.x, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE),
      "rhat", "ess_bulk", "ess_tail"
    ) %>%
      rename(
        overall_mean = mean,
        overall_q025 = `2.5%`,
        overall_q25  = `25%`,
        overall_q75  = `75%`,
        overall_q975 = `97.5%`
      ) %>%
      filter(grepl("^(prob_error|mean_delay|cv_delay)", variable)) %>%
      mutate(
        scenario = scenario_name,
        simulation = sim_idx,
        param_idx = case_when(
          variable == "prob_error" ~ 0,
          grepl("mean_delay|cv_delay", variable) ~ 
            as.numeric(str_extract(variable, "\\d+")),
          TRUE ~ NA
        ),
        type = case_when(
          grepl("^cv_delay", variable) ~ "cv",
          TRUE ~ "mean"
        )
      ) %>%
      select(-variable)
  })
})

# Separate mean and cv, then join
sim_summaries_mean <- sim_summaries %>%
  filter(type == "mean") %>%
  select(-type) %>%
  rename(
    overall_mean_mean = overall_mean,
    overall_q025_mean = overall_q025,
    overall_q25_mean = overall_q25,
    overall_q75_mean = overall_q75,
    overall_q975_mean = overall_q975,
    rhat_mean = rhat,
    ess_bulk_mean = ess_bulk,
    ess_tail_mean = ess_tail
  )

sim_summaries_cv <- sim_summaries %>%
  filter(type == "cv") %>%
  select(scenario, simulation, param_idx, 
         overall_mean_cv = overall_mean,
         overall_q025_cv = overall_q025,
         overall_q975_cv = overall_q975,
         rhat_cv = rhat,
         ess_bulk_cv = ess_bulk,
         ess_tail_cv = ess_tail)

sim_summaries <- sim_summaries_mean %>%
  left_join(sim_summaries_cv, by = c("scenario", "simulation", "param_idx")) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels() %>%
  left_join(select(true_params, scenario, param_idx, true_mean, true_cv), 
            by = c("scenario", "param_idx")) %>%
  left_join(select(empirical_params, scenario, simulation, param_idx, 
                   empirical_mean, empirical_cv),
            by = c("scenario", "simulation", "param_idx")) %>%
  mutate(
    # Bias
    bias_gt = overall_mean_mean - true_mean,
    bias_emp = overall_mean_mean - empirical_mean,
    cv_bias_gt = overall_mean_cv - true_cv,
    cv_bias_emp = overall_mean_cv - empirical_cv,
    # Coverage
    cov95_gt = true_mean >= overall_q025_mean & true_mean <= overall_q975_mean,
    cov95_emp = empirical_mean >= overall_q025_mean & empirical_mean <= overall_q975_mean,
    cov50_gt = true_mean >= overall_q25_mean & true_mean <= overall_q75_mean,
    cov50_emp = empirical_mean >= overall_q25_mean & empirical_mean <= overall_q75_mean,
    cv_cov95_gt = true_cv >= overall_q025_cv & true_cv <= overall_q975_cv,
    # Width
    width95 = overall_q975_mean - overall_q025_mean,
    cv_width95 = overall_q975_cv - overall_q025_cv
  )

# Aggregate across simulations -----------------------------------------------
agg_summaries <- sim_summaries %>%
  group_by(scenario, param_label, group) %>%
  summarise(
    n_sims = n(),
    avg_rhat = mean(rhat_mean, na.rm = TRUE),
    min_ess = min(ess_bulk_mean, na.rm = TRUE),
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
  )

# Convergence diagnostics ------------------------------------------------------

rhat_threshold <- 1.05
ess_threshold <- 400

agg_summaries <- agg_summaries %>%
  mutate(
    rhat_ok = avg_rhat <= rhat_threshold,
    ess_ok = min_ess >= ess_threshold,
    converged = rhat_ok & ess_ok
  )

# problematic parameters
convergence_issues <- agg_summaries %>%
  filter(!converged) %>%
  select(scenario, param_label, group, avg_rhat, min_ess, 
         rhat_ok, ess_ok) %>%
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

# Trace plots ----------------------------------------------------------------
all_draws <- map_dfr(names(sim_estim), function(scenario_name) {
  map_dfr(seq_along(sim_estim[[scenario_name]]), function(sim_idx) {
    
    # transpose pars array
    pars_array <- sim_estim[[scenario_name]][[sim_idx]]$pars
    pars_transposed <- aperm(pars_array, c(2, 3, 1))
    
    as_draws_df(pars_transposed) %>%
      as_tibble() %>%
      select(.chain, .iteration, 
             matches("^(prob_error|mean_delay|cv_delay)")) %>%
      pivot_longer(cols = matches("^(prob_error|mean_delay|cv_delay)"),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(
        scenario = scenario_name,
        simulation = sim_idx,
        chain = .chain,
        iteration = .iteration,
        param_idx = case_when(
          variable == "prob_error" ~ 0,
          grepl("mean_delay|cv_delay", variable) ~ 
            as.numeric(str_extract(variable, "\\d+")),
          TRUE ~ NA
        ),
        type = ifelse(grepl("^cv_delay", variable), "cv", "mean")
      ) %>%
      select(-variable, -.chain, -.iteration)
  })
}) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  rename(post_mean = mean, post_cv = cv) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels()

# Prob error trace
trace_prob_error <- all_draws %>%
  filter(param_idx == 0, iteration > 100) %>%
  left_join(
    true_params %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(scenario, param_idx, true_mean), 
    by = c("scenario", "param_idx")
  ) %>%
  mutate(sim_chain = paste(simulation, chain, sep = "_")) %>%
  ggplot(aes(x = iteration, y = post_mean,
             colour = factor(chain), group = sim_chain)) +
  geom_line(alpha = 0.3) +
  geom_hline(aes(yintercept = true_mean),
             linetype = "dashed", linewidth = 0.8, colour = "black") +
  facet_grid(cols = vars(scenario), scales = "free_y") +
  labs(y = "Probability of Error", x = "Iteration",
       title = "Trace plots for probability of error (100 simulations)",
       colour = "Chain") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("results/figures/trace_error.pdf", trace_prob_error, width = 14, height = 4)

# Delays
make_trace_plot <- function(data, y_var, true_var, title, y_label, add_symbols = FALSE) {
  p <- ggplot(data, aes(x = iteration, y = {{y_var}})) +
    geom_line(alpha = 0.3, aes(colour = param_label, 
                               group = interaction(param_label, simulation))) +
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
  filter(param_idx > 0, iteration > 100) %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join(
    true_params %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(scenario, param_idx, group, true_mean),
    by = c("scenario", "param_idx", "group")
  )

trace_10 <- make_trace_plot(
  filter(trace_data_base, simulation <= 10),
  post_mean,
  true_mean,
  "Trace plots (first 10 simulations)", "Mean Delay",
  add_symbols = TRUE
)

trace_all <- make_trace_plot(
  trace_data_base,
  post_mean,
  true_mean,
  "Trace plots (all simulations)", "Mean Delay",
  add_symbols = TRUE
)

ggsave("results/figures/trace_delays_10.pdf", trace_10, width = 14, height = 10)
ggsave("results/figures/trace_delays_all.pdf", trace_all, width = 14, height = 10)

# Bias plots -----------------------------------------------------------------
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
    theme(
      panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
    )
}

# Compare to true mean delay (ground truth)
bias_plot_delays_gt <- agg_summaries %>%
  filter(!param_label %in% "probability of error") %>%
  make_bias_plot(bias_gt_avg, bias_gt_sd,
                 "Median Bias of Delay Parameters (+/- SD)",
                 "Compared to Ground Truth"
                 )

# Compare to mean delay of simulated true data (sample truth)
bias_plot_delays_emp <- agg_summaries %>%
  filter(!param_label %in% "probability of error") %>%
  make_bias_plot(bias_emp_avg, bias_emp_sd,
                 "Median Bias of Delay Parameters (+/- SD)",
                 "Compared to Sample Truth"
                 )

ggsave("results/figures/bias_plot_delays_gt.pdf", bias_plot_delays_gt, width = 10, height = 8)
ggsave("results/figures/bias_plot_delays_emp.pdf", bias_plot_delays_emp, width = 10, height = 8)


# Compare error probability to ground truth
bias_plot_error_gt <- agg_summaries %>%
  filter(param_label %in% "probability of error") %>%
  make_error_bias_plot(bias_gt_avg, bias_gt_sd, 
                       "Median Bias: Probability of Error",
                       "Compared to Ground Truth")

# Compare error probability to sample truth
bias_plot_error_emp <- agg_summaries %>%
  filter(param_label %in% "probability of error") %>%
  make_error_bias_plot(bias_emp_avg, bias_emp_sd, 
                       "Median Bias: Probability of Error",
                       "Compared to Sample Truth")

ggsave("results/figures/bias_plot_error_gt.pdf", bias_plot_error_gt, width = 8, height = 4)
ggsave("results/figures/bias_plot_error_emp.pdf", bias_plot_error_emp, width = 8, height = 4)


# CV bias plots
bias_plot_cv_gt <- agg_summaries %>%
  filter(!param_label %in% "probability of error") %>%
  make_bias_plot(
    cv_bias_gt_avg, cv_bias_gt_sd,
    "Median Bias of CV Parameters (+/- SD)",
    "Compared to Ground Truth"
  )

bias_plot_cv_emp <- agg_summaries %>%
  filter(!param_label %in% "probability of error") %>%
  make_bias_plot(
    cv_bias_emp_avg, cv_bias_emp_sd,
    "Median Bias of CV Parameters (+/- SD)",
    "Compared to Sample Truth"
  )

ggsave("results/figures/bias_plot_cv_gt.pdf", bias_plot_cv_gt, width = 10, height = 8)
ggsave("results/figures/bias_plot_cv_emp.pdf", bias_plot_cv_emp, width = 10, height = 8)

# Coverage plots --------------------------------------------------------------
make_coverage_plot <- function(data, cov95_col, cov50_col, subtitle) {
  coverage_data <- data %>%
    select(scenario, group, param_label, n_sims, 
           cov95 = {{cov95_col}}, cov50 = {{cov50_col}}) %>%
    pivot_longer(cols = c(cov95, cov50),
                 names_to = "metric",
                 values_to = "coverage") %>%
    mutate(
      interval = ifelse(metric == "cov95", "95% CrI", "50% CrI"),
      n_success = round(coverage * n_sims)
    ) %>%
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
gt_coverage <- agg_summaries %>%
  filter(!param_label %in% "probability of error") %>%
  make_coverage_plot(
    cov95_gt_pct, 
    cov50_gt_pct,
    "True parameters (ground truth). Error bars: 95% binomial confidence intervals"
  )

# Empirical coverage
emp_coverage <- agg_summaries %>%
  filter(!param_label %in% "probability of error") %>%
  make_coverage_plot(
    cov95_emp_pct, 
    cov50_emp_pct,
    "Empirical values (sample truth). Error bars: 95% binomial confidence intervals"
    )

ggsave("results/figures/coverage_plot.pdf", gt_coverage, width = 10, height = 8)
ggsave("results/figures/coverage_plot_emp.pdf", emp_coverage, width = 10, height = 8)

# Posterior density plots ----------------------------------------------------

# Aggregate draws across simulations for cleaner visualisation
posterior_data <- all_draws %>%
  filter(param_label != "probability of error", iteration > 100) %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join(
    true_params %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(scenario, param_idx, group, true_mean, true_cv),
    by = c("scenario", "param_idx", "group")
  )

# Mean delay posteriors
mean_posterior_plot <- ggplot(posterior_data, 
                              aes(x = post_mean, fill = scenario, colour = scenario)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = true_mean), linetype = "dashed", linewidth = 0.8) +
  facet_grid(rows = vars(group), cols = vars(param_label), scales = "free") +
  labs(title = "Posterior Distributions: Mean Delay",
       subtitle = "Dashed line = true value. Densities across all simulations.",
       x = "Mean Delay (days)",
       y = "Density",
       fill = "Scenario",
       colour = "Scenario") +
  theme_minimal() +
  theme(strip.text = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

# CV delay posteriors
cv_posterior_plot <- ggplot(posterior_data, 
                            aes(x = post_cv, fill = scenario, colour = scenario)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = true_cv), linetype = "dashed", linewidth = 0.8) +
  facet_grid(rows = vars(group), cols = vars(param_label), scales = "free") +
  labs(title = "Posterior Distributions: CV",
       subtitle = "Dashed line = true value. Densities across all simulations.",
       x = "Coefficient of Variation",
       y = "Density",
       fill = "Scenario",
       colour = "Scenario") +
  theme_minimal() +
  theme(strip.text = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))


ggsave("results/figures/posterior_mean_delay.pdf", mean_posterior_plot, width = 14, height = 10)
ggsave("results/figures/posterior_cv.pdf", cv_posterior_plot, width = 14, height = 10)


# Error probability posteriors
prob_error_posterior_plot <- all_draws %>%
  filter(param_label %in% "probability of error", iteration > 100) %>%
  left_join(
    true_params %>% 
      mutate(scenario = as.character(scenario)) %>% 
      filter(param_idx == 0) %>% 
      select(scenario, true_mean),
    by = "scenario"
  ) %>%
  ggplot(aes(x = post_mean, fill = scenario, colour = scenario)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = true_mean), linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ scenario, scales = "free_y", nrow = 1) +
  scale_x_continuous(expand = c(0.005, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0.05)) +
  labs(title = "Posterior Distributions: Probability of Error",
       subtitle = "Dashed line = true value. Densities across all simulations.",
       x = "Probability of Error",
       y = "Density",
       fill = "Scenario",
       colour = "Scenario") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none",
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("results/figures/posterior_prob_error.pdf",
       prob_error_posterior_plot, width = 14, height = 4)


# -----------------------------------------------------------------------------
# Look at individuals:
# how often are different events correctly identified for different groups
# how often are errors correctly identified
# how many indiv. have all correctly observed dates
# how many indiv. have different combinations of errors and missing dates

apply_scenario_labels <- function(df) {
  df %>% 
    mutate(
      scenario = factor(recode(scenario, !!!scenario_labels),
                        levels = unname(scenario_labels)),
      group = factor(group, levels = c("community-alive", "community-dead", 
                                       "hospitalised-alive", "hospitalised-dead"))
    )
}

observed_patterns <- map_dfr(scenarios, function(scenario_name) {
  map_dfr(seq_along(sim_data[[scenario_name]]), function(sim_idx) {
    
    sim_obj <- sim_data[[scenario_name]][[sim_idx]]
    
    obs <- sim_obj$observed_data %>%
      mutate(scenario = scenario_name, simulation = sim_idx)
    
    err <- sim_obj$error_indicators %>%
      select(-id, -group)
    
    # Classify each date based on group
    obs %>%
      mutate(
        # Onset should exist for all groups
        onset = case_when(
          is.na(onset) ~ "missing",
          err$onset == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        # Hospitalisation only for hospitalised groups
        hospitalisation = case_when(
          group %in% c("community-alive", "community-dead") ~ NA,
          is.na(hospitalisation) ~ "missing",
          err$hospitalisation == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        # Report should exist for all groups
        report = case_when(
          is.na(report) ~ "missing",
          err$report == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        # Death only for dead groups
        death = case_when(
          group %in% c("community-alive", "hospitalised-alive") ~ NA,
          is.na(death) ~ "missing",
          err$death == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        # Discharge only for hospitalised-alive
        discharge = case_when(
          group != "hospitalised-alive" ~ NA,
          is.na(discharge) ~ "missing",
          err$discharge == TRUE ~ "error",
          TRUE ~ "correct"
        )
      ) %>%
      select(id, group, scenario, simulation, onset,
             hospitalisation, report, death, discharge)
  })
}) %>%
  apply_scenario_labels()

# Summarise per individual
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

# Patterns across individuals
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

  # rough visualisation
p_obs_patterns <- obs_pattern_summary %>%
  ggplot(aes(x = reorder(pattern, n_individuals), y = pct, fill = pattern)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 2.5) +
  coord_flip() +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
  labs(
    title = "Check simulated error/missingness patterns",
    x = "", y = "Percentage of Individuals (%)", fill = "Pattern"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "none",
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

ggsave("results/figures/observed_patterns.pdf",
       p_obs_patterns, width = 14, height = 7)

# True vs estimated error status (excluding missing)
event_names <- c("onset", "hospitalisation", "report", "death", "discharge")

indiv_event_status <- map_dfr(scenarios, function(scenario_name) {
  map_dfr(seq_along(sim_estim[[scenario_name]]), function(sim_idx) {
    
    true_errors <- sim_data[[scenario_name]][[sim_idx]]$error_indicators
    obs_data <- sim_data[[scenario_name]][[sim_idx]]$observed_data
    
    # estimated error indicators [individuals, events, iterations, chains]
    est_error_array <- sim_estim[[scenario_name]][[sim_idx]]$data$error_indicators

    # posterior probability of error (mean across iterations and chains)
    post_prob_error <- apply(est_error_array, c(1, 2), mean, na.rm = TRUE)
    
    # classify as error based on posterior probability
    #est_is_error <- post_prob_error > 0.45
    est_is_error <- post_prob_error > 0.9
    
    tibble(
      scenario = scenario_name,
      simulation = sim_idx,
      id = true_errors$id,
      group = true_errors$group
    ) %>%
      mutate(
        # Onset
        true_onset = case_when(
          is.na(obs_data$onset) ~ "missing",
          true_errors$onset == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_onset = case_when(
          est_is_error[, 1] == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        match_onset = ifelse(
          true_onset == "missing", NA,
          true_onset == estimated_onset
          ),
        
        # Hospitalisation
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
        
        # Report
        true_report = case_when(
          is.na(obs_data$report) ~ "missing",
          true_errors$report == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        estimated_report = case_when(
          est_is_error[, 3] == TRUE ~ "error",
          TRUE ~ "correct"
        ),
        match_report = ifelse(
          true_report == "missing", NA,
          true_report == estimated_report
        ),
        
        # Death
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
        
        # Discharge
        true_discharge = case_when(
          is.na(true_errors$discharge) ~ NA,
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
  })
}) %>%
  apply_scenario_labels()

indiv_event_status_with_pattern <- indiv_event_status %>%
  left_join(
    indiv_obs_summary %>% 
      select(scenario, simulation, id, pattern), 
    by = c("scenario", "simulation", "id")
  )

# % of true and estimated error classifications that match for each event
event_match_summary <- indiv_event_status %>%
  pivot_longer(cols = starts_with("match_"),
               names_to = "event",
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

# Confusion matrix for each event
event_confusion <- indiv_event_status %>%
  pivot_longer(cols = c(starts_with("true_"), starts_with("estimated_")),
               names_to = c(".value", "event"),
               names_pattern = "(true|estimated)_(.*)") %>%
  filter(!is.na(true), true != "missing") %>%
  group_by(scenario, group, event, true, estimated) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = estimated, 
              values_from = n, 
              values_fill = 0,
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

# Confusion matrix with pattern
pattern_group_confusion <- indiv_event_status %>%
  left_join(
    indiv_obs_summary %>% 
      select(scenario, simulation, id, pattern), 
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
  pivot_wider(
    names_from = estimated, 
    values_from = count, 
    values_fill = 0,
    names_prefix = "pred_"
  ) %>%
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
