library(orderly2)
library(dplyr)
library(patchwork)
library(purrr)
library(ggplot2)

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")
orderly_dependency("sim_estim", "latest", "sim_estim.rds")

orderly_artefact(
  "trace_plots", "figures/trace_plots.pdf"
)

dir.create("figures")
source("utils.R")

# Read in dependencies
sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")
sim_estim <- readRDS("sim_estim.rds")

## Plan for across simulation summaries --------------------------------------
# Mean bias (posterior mean - true value)
# Coverage (prop of simulations where true value falls within 50% and 95% CrIs)
# RMSE (sqrt((mean-true)^2))
# Average CrI width


# Define keys for group, delay and date --------------------------------------

group_key <- tibble(
  group_code  = paste0("group_", 1:4),
  group_label = c("Community-alive",
                  "Community-dead",
                  "Hospitalised-alive",
                  "Hospitalised-dead")
)

delay_key <- tribble(
  ~group_label,           ~delay_index, ~delay_label,
  "Community-alive",      1,           "Onset-to-report",
  "Community-dead",       1,           "Onset-to-report",
  "Community-dead",       2,           "Onset-to-death",
  "Hospitalised-alive",   1,           "Onset-to-hospitalisation",
  "Hospitalised-alive",   2,           "Hospitalisation-to-discharge",
  "Hospitalised-alive",   3,           "Onset-to-report",
  "Hospitalised-dead",    1,           "Onset-to-hospitalisation",
  "Hospitalised-dead",    2,           "Hospitalisation-to-death",
  "Hospitalised-dead",    3,           "Onset-to-report"
)

date_key <- tribble(
  ~group_label,           ~date_index, ~date_label,
  "Community-alive",      1,           "Onset",
  "Community-alive",      2,           "Report",
  "Community-dead",       1,           "Onset",
  "Community-dead",       2,           "Report",
  "Community-dead",       3,           "Death",
  "Hospitalised-alive",   1,           "Onset",
  "Hospitalised-alive",   2,           "Hospitalisation",
  "Hospitalised-alive",   3,           "Discharged",
  "Hospitalised-alive",   4,           "Report",
  "Hospitalised-dead",    1,           "Onset",
  "Hospitalised-dead",    2,           "Hospitalisation",
  "Hospitalised-dead",    3,           "Death",
  "Hospitalised-dead",    4,           "Report"
)

# Extract mu, cv and zeta ----------------------------------------------------
scenario_names <- names(sim_estim)

all_res <- map(scenario_names, function(scenario) {
  message("Extracting results for scenario: ", scenario)
  
  scenario_runs <- sim_estim[[scenario]]
  sim_theta <- sim_params[[scenario]]$theta
  
  map(scenario_runs, extract_est_df, theta_true = sim_theta)
}) %>%
  set_names(scenario_names)

  # all_draws contains each iteration of each simulation
  # (9 delays x 100 sims x 45 iter x n scenarios)

all_draws <- imap(all_res, function(sim_list, scenario) {
  imap(sim_list, function(res, sim_idx) {
    res$draws %>%
      mutate(
        scenario = scenario,
        simulation = sim_idx
      )
  }) %>% list_rbind()
}) %>% list_rbind()


# Add labels using keys 
draws_with_labels <- all_draws %>%
  left_join(group_key,  by = c("group" = "group_code")) %>%
  left_join(delay_key,  by = c("group_label", "delay_index")) %>%
  select(
    scenario,
    group_label,
    delay_label,
    simulation,
    everything()
  ) %>%
  select(-group, -delay_index) %>%
  rename(
    group = group_label,
    delay = delay_label
  )

# Per-simulation summary -----------------------------------------------------

  # For each of the 100 sims get the mean, quantiles, coverage, bias, rmse and
  # CrI width for each delay (9 delays x 100 sims x n scenarios)

sims_summary <- imap(all_res, function(sim_list, scenario) {
  imap(sim_list, function(res, sim_idx) {
    res$summary %>%
      mutate(
        scenario = scenario,
        simulation = sim_idx
      )
  }) %>% list_rbind()
}) %>% list_rbind()

summary_with_labels <- sims_summary %>%
  left_join(group_key,  by = c("group" = "group_code")) %>%
  left_join(delay_key,  by = c("group_label", "delay_index")) %>%
  select(
    scenario,
    group_label,
    delay_label,
    simulation,
    everything()
  ) %>%
  select(-group, -delay_index) %>%
  rename(
    group = group_label,
    delay = delay_label
  )

# Across-simulations summary -------------------------------------------------  
agg_sims_summary <- summary_with_labels %>%
  group_by(scenario, group, delay) %>%
  summarise(
    mu_mean_bias = mean(mu_bias, na.rm = TRUE),
    mu_sd_bias = sd(mu_bias, na.rm = TRUE),
    cv_mean_bias = mean(cv_bias, na.rm = TRUE),
    cv_sd_bias = sd(cv_bias, na.rm = TRUE),
    mu_rmse = sqrt(mean(mu_bias^2, na.rm = TRUE)),
    cv_rmse = sqrt(mean(cv_bias^2, na.rm = TRUE)),
    mu_95cov = mean(mu_cov95, na.rm = TRUE),
    cv_95cov = mean(cv_cov95, na.rm = TRUE),
    mu_50cov = mean(mu_cov50, na.rm = TRUE),
    cv_50cov = mean(cv_cov50, na.rm = TRUE),
    mu_width95 = mean(mu_width95, na.rm = TRUE),
    mu_width50 = mean(mu_width50, na.rm = TRUE),
    cv_width95 = mean(cv_width95, na.rm = TRUE),
    cv_width50 = mean(cv_width50, na.rm = TRUE),
    .groups  = "drop"
  )

# Visualise -------------------------------------------------------------------

scenario_group_size <- tibble::tibble(
  scenario = c("very_small_sample", "small_sample", "moderate_sample",
               "baseline", "very_large_sample"),
  group_size = c(10, 20, 50, 100, 500)
)

sample_size_scenarios <- agg_sims_summary %>%
  filter(scenario %in% scenario_group_size$scenario) %>%
  left_join(scenario_group_size, by = "scenario") %>%
  mutate(group_size = factor(group_size, levels = c(10, 20, 50, 100, 500)))

# Coverage (with 95% binomial confidence intervals)
ggplot(sample_size_scenarios,
       aes(x = group_size, y = mu_95cov, group = group, color = delay)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_point(aes(y = mu_50cov, group = group, color = delay, shape = "50 CrI")) +
  facet_wrap(~ group, ncol = 4, scales = "free_x") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(title = "Coverage for μ by Delay",
       x = "Sample size",
       y = "Coverage",
       color = "Delay") +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )


# Bias (mean +/- SD) for mu
ggplot(summary_with_labels, aes(x = delay_label, y = mu_mean_bias, color = group_label)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ group_label, ncol = 4, scales = "free_x") +
  geom_errorbar(aes(ymin = mu_mean_bias - mu_sd_bias, ymax = mu_mean_bias + mu_sd_bias), width = 0.2) +
  labs(title = "Mean Bias of mu (± SD)", y = "Mean Bias", x = "Delay") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  ) +
  ylim(-2, 2)

# Bias (mean +/- SD) for cv
ggplot(summary_with_labels, aes(x = delay_label, y = cv_mean_bias, color = group_label)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ group_label, ncol = 4, scales = "free_x") +
  geom_errorbar(aes(ymin = cv_mean_bias - cv_sd_bias, ymax = cv_mean_bias + cv_sd_bias), width = 0.2) +
  labs(title = "Mean Bias of CV (± SD)", y = "Mean Bias", x = "Delay") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  ) +
  ylim(-0.2, 0.2)
