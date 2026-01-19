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
                           "figures/bias_plot.pdf",
                           "figures/bias_plot_empirical.pdf",
                           "figures/coverage_plot.pdf",
                           "figures/coverage_plot_empirical.pdf",
                           "figures/sim_summaries.rds",
                           "figures/agg_summaries.rds"),
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
  ~param_idx, ~delay_from,         ~delay_to,            ~group,
  1,          "onset",             "report",             "Community-alive",
  2,          "onset",             "report",             "Community-dead",
  3,          "onset",             "report",             "Hospital-alive",
  4,          "onset",             "report",             "Hospital-dead",
  5,          "onset",             "death",              "Community-dead",
  6,          "onset",             "hospitalisation",    "Hospital-alive",
  7,          "hospitalisation",   "discharge",          "Hospital-alive",
  8,          "onset",             "hospitalisation",    "Hospital-dead",
  9,          "hospitalisation",   "death",              "Hospital-dead"
) %>%
  mutate(delay_label = glue("{delay_from} to {delay_to}"),
         group = factor(group, levels = c("Community-alive", "Community-dead", 
                                          "Hospital-alive", "Hospital-dead")),
         delay_group_label = glue("{delay_from} to {delay_to} ({group})"))

# Extract true parameter values ----------------------------------------------
true_params <- map_dfr(names(sim_params), ~tibble(
  scenario = .x,
  param_idx = 1:nrow(sim_params[[.x]]$delay_params),
  true_mean = sim_params[[.x]]$delay_params$delay_mean,
  true_cv = sim_params[[.x]]$delay_params$delay_cv
)) %>%
  mutate(
    scenario = recode(scenario, !!!scenario_labels),
    scenario = factor(scenario, levels = unname(scenario_labels))
  ) %>%
  left_join(delay_mapping, by = "param_idx")

true_prob_error <- map_dfr(names(sim_params), ~tibble(
  scenario = .x,
  true_prob_error = sim_params[[.x]]$error_params$prob_error
)) %>%
  mutate(
    scenario = recode(scenario, !!!scenario_labels),
    scenario = factor(scenario, levels = unname(scenario_labels))
  )

# Extract empirical mean/CV from simulated data -----------------------------
delay_specs <- list(
  list(col = "onset_report", groups = 1:4, param_idx_fn = function(g) g),
  list(col = "onset_death", groups = 2, param_idx_fn = function(g) 5),
  list(col = "onset_hosp", groups = 3, param_idx_fn = function(g) 6),
  list(col = "hosp_discharge", groups = 3, param_idx_fn = function(g) 7),
  list(col = "onset_hosp", groups = 4, param_idx_fn = function(g) 8),
  list(col = "hosp_death", groups = 4, param_idx_fn = function(g) 9)
)

empirical_params <- map_dfr(names(sim_data), function(scenario_name) {
  map_dfr(seq_along(sim_data[[scenario_name]]), function(sim_idx) {
    
    delays <- sim_data[[scenario_name]][[sim_idx]]$true_data %>%
      mutate(
        onset_report = as.numeric(report - onset),
        onset_death = as.numeric(death - onset),
        onset_hosp = as.numeric(hospitalisation - onset),
        hosp_discharge = as.numeric(discharge - hospitalisation),
        hosp_death = as.numeric(death - hospitalisation)
      )
    
    # Calculate empirical stats for all delay types
    map_dfr(delay_specs, function(spec) {
      delays %>%
        filter(group %in% spec$groups) %>%
        group_by(group) %>%
        summarise(
          empirical_mean = mean(.data[[spec$col]], na.rm = TRUE),
          empirical_cv = sd(.data[[spec$col]], na.rm = TRUE) / empirical_mean,
          .groups = "drop"
        ) %>%
        mutate(
          param_idx = as.integer(spec$param_idx_fn(group)),
          scenario = scenario_name,
          simulation = sim_idx
        ) %>%
        select(-group)
    })
  })
}) %>%
  left_join(delay_mapping, by = c("param_idx")) %>%
  mutate(
    scenario = recode(scenario, !!!scenario_labels),
    scenario = factor(scenario, levels = unname(scenario_labels))
  )

# Extract MCMC draws ---------------------------------------------------------
extract_draws <- function(mcmc_result, scenario_name, sim_idx) {
  pars <- mcmc_result$pars
  n_iter <- dim(pars)[2]
  n_chains <- dim(pars)[3]
  iter_chain <- expand_grid(
    iteration = 1:n_iter,
    chain = 1:n_chains
  )
  
  bind_rows(
    # Prob error
    iter_chain %>%
      mutate(
        scenario = scenario_name,
        simulation = sim_idx,
        param_idx = 0L,
        prob_error = as.vector(pars["prob_error", , ]),
        mean_delay = NA,
        cv_delay = NA
      ),
    # Delay parameters
    map_dfr(1:max(delay_mapping$param_idx), function(i) {
      iter_chain %>%
        mutate(
          scenario = scenario_name,
          simulation = sim_idx,
          param_idx = i,
          prob_error = NA,
          mean_delay = as.vector(pars[paste0("mean_delay", i), , ]),
          cv_delay = as.vector(pars[paste0("cv_delay", i), , ])
        )
    })
  )
}

# Extract draws
all_draws <- map_dfr(names(sim_estim), function(scenario_name) {
  cat(sprintf("  %s\n", scenario_name))
  map_dfr(seq_along(sim_estim[[scenario_name]]),
          ~extract_draws(sim_estim[[scenario_name]][[.x]], scenario_name, .x))
}) %>%
  mutate(
    scenario = recode(scenario, !!!scenario_labels),
    scenario = factor(scenario, levels = unname(scenario_labels))
    ) %>%
  left_join(delay_mapping, by = "param_idx")


# Calculate posterior summaries per simulation -------------------------------

prob_error_summaries <- all_draws %>%
  filter(param_idx == 0) %>%
  group_by(scenario, simulation) %>%
  summarise(
    across(prob_error,
           list(mean = ~mean(., na.rm = TRUE),
                q025 = ~quantile(., 0.025, na.rm = TRUE),
                q975 = ~quantile(., 0.975, na.rm = TRUE),
                q25 = ~quantile(., 0.25, na.rm = TRUE),
                q75 = ~quantile(., 0.75, na.rm = TRUE)),
           .names = "post_{.fn}_{.col}"),
    .groups = "drop"
  )

sim_summaries <- all_draws %>%
  filter(param_idx > 0) %>%
  group_by(scenario, simulation, param_idx, delay_label, delay_from, delay_to, group) %>%
  summarise(
    across(c(mean_delay, cv_delay), 
           list(mean = ~mean(., na.rm = TRUE),
                q025 = ~quantile(., 0.025, na.rm = TRUE),
                q975 = ~quantile(., 0.975, na.rm = TRUE),
                q25 = ~quantile(., 0.25, na.rm = TRUE),
                q75 = ~quantile(., 0.75, na.rm = TRUE)),
           .names = "post_{.fn}_{.col}"),
    .groups = "drop"
  ) %>%
  # Join all reference data
  left_join(select(true_params, scenario, param_idx, group, true_mean, true_cv),
            by = c("scenario", "param_idx", "group")) %>%
  left_join(select(empirical_params, scenario, simulation, param_idx, group,
                   empirical_mean, empirical_cv),
            by = c("scenario", "simulation", "param_idx", "group")) %>%
  left_join(prob_error_summaries, by = c("scenario", "simulation")) %>%
  left_join(true_prob_error, by = "scenario") %>%
  # Calculate all metrics
  mutate(
    # Bias - ground truth
    delay_bias = post_mean_mean_delay - true_mean,
    delay_cv_bias = post_mean_cv_delay - true_cv,
    prob_error_bias = post_mean_prob_error - true_prob_error,
    
    # Bias - empirical
    delay_bias_emp = post_mean_mean_delay - empirical_mean,
    delay_cv_bias_emp = post_mean_cv_delay - empirical_cv,
    
    # Coverage - ground truth
    delay_cov95 = true_mean >= post_q025_mean_delay & true_mean <= post_q975_mean_delay,
    delay_cov50 = true_mean >= post_q25_mean_delay & true_mean <= post_q75_mean_delay,
    delay_cv_cov95 = true_cv >= post_q025_cv_delay & true_cv <= post_q975_cv_delay,
    delay_cv_cov50 = true_cv >= post_q25_cv_delay & true_cv <= post_q75_cv_delay,
    prob_error_cov95 = true_prob_error >= post_q025_prob_error & true_prob_error <= post_q975_prob_error,
    prob_error_cov50 = true_prob_error >= post_q25_prob_error & true_prob_error <= post_q75_prob_error,
    
    # Coverage - empirical
    delay_cov95_emp = empirical_mean >= post_q025_mean_delay & empirical_mean <= post_q975_mean_delay,
    delay_cov50_emp = empirical_mean >= post_q25_mean_delay & empirical_mean <= post_q75_mean_delay,
    delay_cv_cov95_emp = empirical_cv >= post_q025_cv_delay & empirical_cv <= post_q975_cv_delay,
    delay_cv_cov50_emp = empirical_cv >= post_q25_cv_delay & empirical_cv <= post_q75_cv_delay,
    
    # Credible interval width
    delay_width95 = post_q975_mean_delay - post_q025_mean_delay,
    delay_width50 = post_q75_mean_delay - post_q25_mean_delay,
    delay_cv_width95 = post_q975_cv_delay - post_q025_cv_delay,
    delay_cv_width50 = post_q75_cv_delay - post_q25_cv_delay,
    prob_error_width95 = post_q975_prob_error - post_q025_prob_error,
    prob_error_width50 = post_q75_prob_error - post_q025_prob_error
  )

# Aggregate across simulations -----------------------------------------------
agg_summaries <- sim_summaries %>%
  mutate(delay_label = factor(delay_label, levels = c("onset to report",
                                                      "onset to death",
                                                      "onset to hospitalisation",
                                                      "hospitalisation to discharge",
                                                      "hospitalisation to death"))
         ) %>%
  group_by(scenario, delay_label, group) %>%
  summarise(
    n_sims = n(),
    # Bias metrics: average, SD, and RMSE across simulations
    across(ends_with("_bias") | ends_with("_bias_emp"),
           list(avg = ~mean(., na.rm = TRUE), 
                sd = ~sd(., na.rm = TRUE),
                rmse = ~sqrt(mean(.^2, na.rm = TRUE))),
           .names = "{.col}_{.fn}"),
    
    # Coverage: proportion of simulations where CI contains true value
    across(ends_with("_cov95") | ends_with("_cov50") | 
             ends_with("_cov95_emp") | ends_with("_cov50_emp"),
           ~mean(., na.rm = TRUE),
           .names = "{.col}_pct"),
    
    # Width: average CI width across simulations
    across(ends_with("_width95") | ends_with("_width50"),
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
  left_join(true_prob_error, by = "scenario") %>%
  ggplot(aes(x = iteration, y = prob_error)) +
  geom_line(colour = "dodgerblue", alpha = 0.3) +
  geom_hline(aes(yintercept = true_prob_error), 
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
    geom_line(alpha = 0.3, aes(colour = delay_label, 
                               group = interaction(delay_label, simulation))) +
    geom_hline(aes(yintercept = {{true_var}}, colour = delay_label),
               linetype = "dashed", linewidth = 0.8) +
    facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
    labs(y = y_label, x = "Iteration", colour = "Delay", title = title) +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))
  
  if (add_symbols) {
    true_points <- data %>%
      distinct(scenario, group, delay_label, {{true_var}}) %>%
      crossing(iteration = c(min(data$iteration), max(data$iteration)))
    
    p <- p + geom_point(data = true_points,
                        aes(x = iteration, y = {{true_var}}, fill = delay_label),
                        shape = 23, size = 3, colour = "black",
                        inherit.aes = FALSE, show.legend = FALSE)
  }
  p
}

trace_data_base <- all_draws %>%
  filter(param_idx > 0, iteration > 100) %>%
  left_join(select(true_params, scenario, param_idx, group, true_mean),
            by = c("scenario", "param_idx", "group"))

trace_10 <- make_trace_plot(
  filter(trace_data_base, simulation <= 10),
  mean_delay, true_mean,
  "Trace plots (first 10 simulations)", "Mean Delay",
  add_symbols = TRUE
)

trace_all <- make_trace_plot(
  trace_data_base,
  mean_delay, true_mean,
  "Trace plots (all simulations)", "Mean Delay",
  add_symbols = TRUE
)

ggsave("figures/trace_delays_10.pdf", trace_10, width = 14, height = 10)
ggsave("figures/trace_delays_all.pdf", trace_all, width = 14, height = 10)

# Bias plots -----------------------------------------------------------------

make_bias_plot <- function(data, bias_avg_col, bias_sd_col, title, subtitle) {
  ggplot(data, aes(x = delay_label, y = {{bias_avg_col}}, colour = delay_label)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    geom_errorbar(aes(ymin = {{bias_avg_col}} - {{bias_sd_col}}, 
                      ymax = {{bias_avg_col}} + {{bias_sd_col}}), 
                  width = 0.3) +
    facet_grid(rows = vars(group), cols = vars(scenario)) +
    labs(title = title,
         subtitle = subtitle,
         y = "Mean Bias",
         x = "Delay") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))
}

# Compare to true mean delay (ground truth)
bias_plot_gt <- make_bias_plot(agg_summaries,
                               delay_bias_avg, delay_bias_sd,
                               "Mean Bias of Delay Parameters (+/- SD)",
                               "Compared to ground truth")

ggsave("figures/bias_plot.pdf", bias_plot_gt, width = 10, height = 8)

# Compare to mean delay of simulated true data (sample truth)
bias_plot_emp <- make_bias_plot(agg_summaries,
                                delay_bias_emp_avg, delay_bias_emp_sd,
                                "Mean Bias of Delay Parameters (+/- SD)",
                                "Compared to sample truth")

ggsave("figures/bias_plot_empirical.pdf", bias_plot_emp, width = 10, height = 8)

# CV bias plots
bias_plot_cv <- make_bias_plot(agg_summaries,
                               delay_cv_bias_avg, delay_cv_bias_sd,
                               "Mean Bias of CV Parameters (+/- SD)",
                               "Compared to ground truth")

ggsave("figures/cv_bias_plot.pdf", bias_plot_cv, width = 10, height = 8)

# Coverage plots --------------------------------------------------------------

make_coverage_plot <- function(data, cov95_col, cov50_col, subtitle) {
  coverage_data <- data %>%
    select(scenario, group, delay_label, n_sims, 
           cov95 = {{cov95_col}}, cov50 = {{cov50_col}}) %>%
    pivot_longer(cols = c(cov95, cov50),
                 names_to = "metric",
                 values_to = "coverage") %>%
    mutate(
      interval = ifelse(metric == "cov95", "95% CrI", "50% CrI"),
      n_success = round(coverage * n_sims),
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
              aes(x = delay_label, y = coverage, 
                  colour = delay_label, shape = interval)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.3, alpha = 0.6) +
    geom_hline(yintercept = 0.95, linetype = "dashed", colour = "black") +
    geom_hline(yintercept = 0.50, linetype = "dashed", colour = "black") +
    facet_grid(rows = vars(group), cols = vars(scenario)) +
    labs(title = "Coverage of Credible Intervals",
         subtitle = subtitle,
         y = "Coverage Probability",
         x = "Delay",
         shape = "") +
    ylim(0, 1) +
    theme_minimal() +
    theme(strip.text = element_text(size = 9, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          panel.border = element_rect(colour = "darkgrey",
                                      fill = NA, linewidth = 1)
          ) +
    guides(colour = "none")

}

# Ground truth coverage
gt_coverage <- make_coverage_plot(
  agg_summaries, 
  delay_cov95_pct, 
  delay_cov50_pct,
  "True parameters (ground truth). Error bars: 95% binomial confidence intervals"
)

ggsave("figures/coverage_plot.pdf", gt_coverage, width = 10, height = 8)

# Empirical coverage
emp_coverage <- make_coverage_plot(
  agg_summaries, 
  delay_cov95_emp_pct, 
  delay_cov50_emp_pct,
  "Empirical values (sample truth). Error bars: 95% binomial confidence intervals"
)

ggsave("figures/coverage_plot_empirical.pdf", emp_coverage, width = 10, height = 8)

# Posterior density plots ----------------------------------------------------

# Aggregate draws across simulations for cleaner visualisation
posterior_data <- all_draws %>%
  mutate(delay_label = factor(delay_label, levels = c("onset to report",
                                                      "onset to death",
                                                      "onset to hospitalisation",
                                                      "hospitalisation to discharge",
                                                      "hospitalisation to death"))
  ) %>%
  filter(param_idx > 0, iteration > 100) %>%
  left_join(select(true_params, scenario, param_idx, group, true_mean, true_cv),
            by = c("scenario", "param_idx", "group"))

# Mean delay posteriors
mean_posterior_plot <- ggplot(posterior_data, 
                              aes(x = mean_delay, fill = scenario, colour = scenario)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = true_mean), linetype = "dashed", linewidth = 0.8) +
  facet_grid(rows = vars(group), cols = vars(delay_label), scales = "free") +
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

ggsave("figures/posterior_mean_delay.pdf", mean_posterior_plot, width = 14, height = 10)

# CV delay posteriors
cv_posterior_plot <- ggplot(posterior_data, 
                            aes(x = cv_delay, fill = scenario, colour = scenario)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = true_cv), linetype = "dashed", linewidth = 0.8) +
  facet_grid(rows = vars(group), cols = vars(delay_label), scales = "free") +
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

ggsave("figures/posterior_cv.pdf", cv_posterior_plot, width = 14, height = 10)


# Error probability posteriors
prob_error_posterior_data <- all_draws %>%
  filter(param_idx == 0, iteration > 100) %>%
  left_join(true_prob_error, by = "scenario")

prob_error_posterior_plot <- ggplot(prob_error_posterior_data,
                                    aes(x = prob_error, fill = scenario, colour = scenario)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = true_prob_error), linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ scenario, scales = "free_y", nrow = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.001, NA)) +
  scale_y_continuous(expand = c(0, 1)) +
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
