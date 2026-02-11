# SANITY CHECK

library(orderly)
library(dplyr)
library(tidyr)
library(patchwork)
library(purrr)
library(ggplot2)
library(glue)

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

orderly_artefact(files = c("figures/trace_error.pdf",
                           "figures/trace_delays_10.pdf",
                           "figures/trace_delays_all.pdf",
                           "figures/bias_plot_delays_gt.pdf",
                           "figures/bias_plot_delays_emp.pdf",
                           "figures/bias_plot_error_gt.pdf",
                           "figures/bias_plot_error_emp.pdf",
                           "figures/bias_plot_cv_gt.pdf",
                           "figures/bias_plot_cv_emp.pdf",
                           "figures/coverage_plot.pdf",
                           "figures/coverage_plot_emp.pdf",
                           "figures/sim_summaries.rds",
                           "figures/agg_summaries.rds",
                           "figures/posterior_mean_delay.pdf",
                           "figures/posterior_cv.pdf",
                           "figures/posterior_prob_error.pdf"),
                 description = "Analysis outputs")

dir.create("figures", showWarnings = FALSE)

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
true_params <- map_dfr(scenarios, function(sn) {
  params <- sim_params[[sn]]
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
    mutate(scenario = sn)
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
extract_draws <- function(mcmc_result, scenario_name, sim_idx) {
  pars <- mcmc_result$pars
  n_iter <- dim(pars)[2]
  n_chains <- dim(pars)[3]
  
  iter_chain <- expand_grid(
    iteration = 1:n_iter,
    chain = 1:n_chains
  )
  
  map_dfr(0:9, function(i) {
    p_name <- if(i == 0) "prob_error" else paste0("mean_delay", i)
    cv_name <- if(i == 0) NULL else paste0("cv_delay", i)
    
    iter_chain %>%
      mutate(
        scenario = scenario_name,
        simulation = sim_idx,
        param_idx = i,
        post_mean = as.vector(pars[p_name, , ]),
        post_cv   = if(!is.null(cv_name)) as.vector(pars[cv_name, , ]) else NA
      )
  })
}

# Extract all and apply factors
all_draws <- map_dfr(names(sim_estim), function(sn) {
  map_dfr(seq_along(sim_estim[[sn]]), 
          ~extract_draws(sim_estim[[sn]][[.x]], sn, .x))
}) %>%
  left_join(delay_mapping, by = "param_idx") %>%
  apply_factor_levels()


# Calculate posterior summaries per simulation -------------------------------
sim_summaries <- all_draws %>%
  group_by(scenario, simulation, param_idx, param_label, group) %>%
  summarise(
    across(post_mean, 
           list(overall_mean = ~mean(., na.rm = TRUE),
                overall_q025 = ~quantile(., 0.025, na.rm = TRUE),
                overall_q975 = ~quantile(., 0.975, na.rm = TRUE),
                overall_q25  = ~quantile(., 0.25, na.rm = TRUE),
                overall_q75  = ~quantile(., 0.75, na.rm = TRUE)),
           .names = "{.fn}"),
    across(post_cv,
           list(overall_mean = ~mean(., na.rm = TRUE),
                overall_q025 = ~quantile(., 0.025, na.rm = TRUE),
                overall_q975 = ~quantile(., 0.975, na.rm = TRUE)),
           .names = "{.fn}_cv"),
    
    .groups = "drop"
  ) %>%
  left_join(select(true_params, scenario, param_idx, true_mean, true_cv), 
            by = c("scenario", "param_idx")) %>%
  left_join(select(empirical_params, scenario, simulation, param_idx, 
                   empirical_mean, empirical_cv),
            by = c("scenario", "simulation", "param_idx")) %>%
  mutate(
    # bias
    bias_gt = overall_mean - true_mean,
    bias_emp = overall_mean - empirical_mean,
    cv_bias_gt = overall_mean_cv - true_cv,
    cv_bias_emp = overall_mean_cv - empirical_cv,
    # 95% coverage
    cov95_gt = true_mean >= overall_q025 & true_mean <= overall_q975,
    cov95_emp = empirical_mean >= overall_q025 & empirical_mean <= overall_q975,
    # 50% coverage
    cov50_gt = true_mean >= overall_q25 & true_mean <= overall_q75,
    cov50_emp = empirical_mean >= overall_q25 & empirical_mean <= overall_q75,
    # CrI width
    width95 = overall_q975 - overall_q025,
    cv_width95 = overall_q975_cv - overall_q025_cv
  )

# Aggregate across simulations -----------------------------------------------
agg_summaries <- sim_summaries %>%
  group_by(scenario, param_label, group) %>%
  summarise(
    n_sims = n(),
    # Bias metrics: median, SD and RMSE for both ground truth and empirical bias
    across(c(bias_gt, bias_emp, cv_bias_gt, cv_bias_emp),
           list(avg = ~median(., na.rm = TRUE), 
                sd  = ~sd(., na.rm = TRUE),
                rmse = ~sqrt(mean(.^2, na.rm = TRUE))),
           .names = "{.col}_{.fn}"),
    
    # Coverage: proportion of simulations where CI contains the truth
    across(c(cov95_gt, cov95_emp, cov50_gt, cov50_emp),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_pct"),
    
    # Width: average CI width across simulations
    across(c(width95, cv_width95),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_avg"),
    
    .groups = "drop"
  )

# Save outputs ---------------------------------------------------------------
saveRDS(sim_summaries, "figures/sim_summaries.rds")
saveRDS(agg_summaries, "figures/agg_summaries.rds")

# Trace plots ----------------------------------------------------------------

# Prob error trace
trace_prob_error <- all_draws %>%
  filter(param_idx == 0, iteration > 100) %>%
  left_join(
    true_params %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(scenario, param_idx, true_mean), 
    by = c("scenario", "param_idx")
  ) %>%
  ggplot(aes(x = iteration, y = post_mean)) +
  geom_line(colour = "dodgerblue", alpha = 0.3) +
  geom_hline(aes(yintercept = true_mean),
             linetype = "dashed", linewidth = 0.8) +
  facet_grid(cols = vars(scenario), scales = "free_y") +
  labs(y = "Probability of Error", x = "Iteration",
       title = "Trace plots for probability of error") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("figures/trace_error.pdf", trace_prob_error, width = 14, height = 4)

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
      tidyr::crossing(iteration = c(min(data$iteration), max(data$iteration)))
    
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

ggsave("figures/trace_delays_10.pdf", trace_10, width = 14, height = 10)
ggsave("figures/trace_delays_all.pdf", trace_all, width = 14, height = 10)

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

ggsave("figures/bias_plot_delays_gt.pdf", bias_plot_delays_gt, width = 10, height = 8)
ggsave("figures/bias_plot_delays_emp.pdf", bias_plot_delays_emp, width = 10, height = 8)


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

ggsave("figures/bias_plot_error_gt.pdf", bias_plot_error_gt, width = 8, height = 4)
ggsave("figures/bias_plot_error_emp.pdf", bias_plot_error_emp, width = 8, height = 4)


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

ggsave("figures/bias_plot_cv_gt.pdf", bias_plot_cv_gt, width = 10, height = 8)
ggsave("figures/bias_plot_cv_emp.pdf", bias_plot_cv_emp, width = 10, height = 8)

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
    geom_hline(yintercept = 0.95, linetype = "dashed", colour = "seagreen", alpha = 0.8) +
    geom_hline(yintercept = 0.50, linetype = "dashed", colour = "lightseagreen", alpha = 0.8) +
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
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)) +
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

ggsave("figures/coverage_plot.pdf", gt_coverage, width = 10, height = 8)
ggsave("figures/coverage_plot_emp.pdf", emp_coverage, width = 10, height = 8)

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


ggsave("figures/posterior_mean_delay.pdf", mean_posterior_plot, width = 14, height = 10)
ggsave("figures/posterior_cv.pdf", cv_posterior_plot, width = 14, height = 10)


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

ggsave("figures/posterior_prob_error.pdf", prob_error_posterior_plot, width = 14, height = 4)
