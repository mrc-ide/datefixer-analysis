# sanity check

# Summary of sample scenarios

library(orderly)
library(dplyr)
library(patchwork)
library(purrr)
library(ggplot2)

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

# need to add very small and small sample when I've worked out the issue
# orderly_dependency("sim_estim",
#                    "latest(parameter:scenario == 'baseline')",
#                    c("sim_estim_baseline.rds" = "sim_estim.rds"))


orderly_dependency("sim_estim",
                   "latest(parameter:scenario == 'baseline')",
                   c("sim_estim_baseline.rds" = "sim_estim.rds"))

orderly_dependency("sim_estim",
                   "latest(parameter:scenario == 'no_error')",
                   c("sim_estim_no_error.rds" = "sim_estim.rds"))
orderly_dependency("sim_estim",
                   "latest(parameter:scenario == 'no_missing')",
                   c("sim_estim_no_missing.rds" = "sim_estim.rds"))
orderly_dependency("sim_estim",
                   "latest(parameter:scenario == 'no_error_no_missing')",
                   c("sim_estim_no_error_no_missing.rds" = "sim_estim.rds")) #"20250717-062514-ae410bfc"

orderly_artefact(
  "trace_plots", "figures/trace_plots.pdf"
)

dir.create("figures")
source("utils.R")

# Read in dependencies
sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")

sim_estim_no_error   <- readRDS("sim_estim_no_error.rds")
sim_estim_no_error_no_miss <- readRDS("sim_estim_no_error_no_missing.rds")
sim_estim_no_miss   <- readRDS("sim_estim_no_missing.rds")
sim_estim_baseline   <- readRDS("sim_estim_baseline.rds")

sim_estim <- c(sim_estim_no_error, sim_estim_no_error_no_miss, sim_estim_no_miss, sim_estim_baseline)

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
  
  scenario_sims <- sim_estim[[scenario]]
  sim_theta <- sim_params[[scenario]]$theta
  true_data <- sim_data[[scenario]] |> map("true_dat")
  index_dates <- sim_params[[scenario]]$index_dates
  
  map2(scenario_sims, true_data, function(sim, sim_dat) {
    extract_est_df(
      MCMCres    = sim,
      theta_true = sim_theta,
      true_dat   = sim_dat,
      index_dates = index_dates
    )
  })
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

test <- draws_with_labels %>%
  group_by(group, scenario, delay, iteration) %>%
  summarise(mean_mu = mean(mu, na.rm = TRUE))

mu_true_df <- imap_dfr(sim_params, function(params, scenario_name) {
  mu_list <- params$theta$mu
  n_groups <- length(mu_list)
  
  map_dfr(seq_len(n_groups), function(g) {
    n_delays <- length(mu_list[[g]])
    
    tibble(
      scenario = scenario_name,
      group = group_key$group_label[g],
      delay_index = seq_len(n_delays),
      mu_true = mu_list[[g]]
    ) %>%
      left_join(delay_key, by = c("group" = "group_label", "delay_index")) %>%
      rename(delay = delay_label)
  })
})


# Compare to true mean delay (ground truth) ----------------------------------
test_with_true <- test %>%
  left_join(mu_true_df, by = c("scenario", "group", "delay"))

trace_plot_sanity <- ggplot(
  test_with_true, aes(x = iteration, y = mean_mu, colour = as.factor(delay))
  ) +
  geom_line() +
  geom_hline(aes(yintercept = mu_true, linetype = "True value", colour = as.factor(delay)), lty = 2) +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
  ylab("Mean Mu") +
  labs(colour = "Delay")

ggsave("figures/trace_plot_sanity.pdf", trace_plot_sanity, width = 11, height = 7)


# Compare to mean delay of simulated true data (sample truth) ----------------
mu_empirical_df <- imap_dfr(all_res, function(sim_list, scenario_name) {
  map_dfr(sim_list, function(res) {
    res$summary %>%
      select(group, delay_index, mu_empirical) %>%
      distinct()
  }) %>%
    group_by(group, delay_index) %>%
    summarise(mu_empirical = mean(mu_empirical, na.rm = TRUE), .groups = "drop") %>%
    mutate(scenario = scenario_name) %>%
    left_join(group_key, by = c("group" = "group_code")) %>%
    left_join(delay_key, by = c("group_label", "delay_index")) %>%
    transmute(
      scenario,
      group = group_label,
      delay = delay_label,
      mu_empirical
    )
})

test_with_emp <- test %>%
  left_join(mu_empirical_df, by = c("scenario", "group", "delay"))

trace_plot_empirical <- ggplot(test_with_emp,
                               aes(x = iteration, y = mean_mu,
                                   colour = as.factor(delay))) +
  geom_line() +
  geom_hline(aes(yintercept = mu_empirical, linetype = "Empirical value",
                 colour = as.factor(delay)), lty = 2) +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
  ylab("Mean Mu") +
  labs(colour = "Delay")

trace_plot_empirical

ggsave("figures/trace_plot_empirical.pdf", trace_plot_empirical, width = 11, height = 7)

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
  mutate(
    scenario = factor(scenario, levels =
                        c("baseline", "no_error", "no_missing", "no_error_no_missing"))
  ) %>%
  group_by(scenario, group, delay) %>%
  summarise(
    # bias to true params
    mu_mean_bias = mean(mu_bias, na.rm = TRUE),
    mu_sd_bias = sd(mu_bias, na.rm = TRUE),
    cv_mean_bias = mean(cv_bias, na.rm = TRUE),
    cv_sd_bias = sd(cv_bias, na.rm = TRUE),
    # bias to simulated delays
    mu_emp_bias  = mean(mu_bias_emp, na.rm = TRUE),
    mu_emp_sd    = sd(mu_bias_emp, na.rm = TRUE),
    cv_emp_bias  = mean(cv_bias_emp, na.rm = TRUE),
    cv_emp_sd    = sd(cv_bias_emp, na.rm = TRUE),
    # others
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

# scenario_group_size <- tibble::tibble(
#   scenario = c("baseline", "no_error", "no_missing", "no_error_no_missing"),
#   group_size = c(100, 100, 100, 100)
# )
# 
# sample_size_scenarios <- agg_sims_summary %>%
#   filter(scenario %in% scenario_group_size$scenario) %>%
#   left_join(scenario_group_size, by = "scenario") #%>%
#   #mutate(group_size = factor(group_size, levels = c(10, 20, 50, 100, 500)))

# # Coverage (with 95% binomial confidence intervals)
# ggplot(sample_size_scenarios,
#        aes(x = group_size, y = mu_95cov, group = group, colour = delay)) +
#   #geom_point(position = position_dodge(width = 0.6), size = 2) +
#   geom_point(size = 2) +
#   geom_point(aes(y = mu_50cov, group = group, colour = delay)) +
#   facet_wrap(~ group, ncol = 4, scales = "free_x") +
#   geom_hline(yintercept = 0.95, linetype = "dashed", colour = "black") +
#   geom_hline(yintercept = 0.5, linetype = "dashed", colour = "black") +
#   labs(title = "Coverage for μ by Delay",
#        x = "Sample size",
#        y = "Coverage",
#        colour = "Delay") +
#   ylim(0, 1) +
#   theme_minimal() +
#   theme(
#     axis.title.y = element_text(vjust = +4),
#     axis.title.x = element_text(vjust = -2),
#     panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
#   )

# Bias (mean +/- SD) for mu
bias <- ggplot(agg_sims_summary, aes(x = delay, y = mu_mean_bias, colour = delay)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  #facet_wrap(~ group + scenario, ncol = 4, scales = "free_x") +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free") +
  geom_errorbar(aes(ymin = mu_mean_bias - mu_sd_bias, ymax = mu_mean_bias + mu_sd_bias), width = 0.2) +
  labs(title = "Mean Bias of mu (+/- SD) compared to ground truth", y = "Mean Bias", x = "Delay") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    axis.text.x = element_text(angle = 25, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    legend.position = "none"
  ) +
ylim(-2.6, 2.6)

ggsave("figures/bias_sanity_plot.pdf", bias, width = 7, height = 8)



bias_mu_emp <- ggplot(agg_sims_summary, aes(x = delay, y = mu_emp_bias, colour = delay)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free") +
  geom_errorbar(aes(ymin = mu_emp_bias - mu_emp_sd, ymax = mu_emp_bias + mu_emp_sd), width = 0.2) +
  labs(title = "Mean Bias of mu (+/- SD) compared to sample truth", y = "Mean Bias", x = "Delay") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    axis.text.x = element_text(angle = 25, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    legend.position = "none"
  )

ggsave("figures/bias_sanity_plot_empirical.pdf", bias_mu_emp,
       width = 7, height = 8)

# Bias (mean +/- SD) for cv
cv_bias <- ggplot(agg_sims_summary, aes(x = delay, y = cv_mean_bias, colour = delay)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  #facet_wrap(~ group + scenario, ncol = 3, scales = "free_x") +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
  geom_errorbar(aes(ymin = cv_mean_bias - cv_sd_bias, ymax = cv_mean_bias + cv_sd_bias), width = 0.2) +
  labs(title = "Mean Bias of CV (+/- SD)", y = "Mean Bias", x = "Delay") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    legend.position = "none"
  ) #+
#ylim(-0.2, 0.2)

ggsave("figures/cv_bias_sanity_plot.pdf", cv_bias, width = 7, height = 8)


bias_cv_emp <- ggplot(agg_sims_summary, aes(x = delay, y = cv_emp_bias, colour = delay)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free") +
  geom_errorbar(aes(ymin = cv_emp_bias - cv_emp_sd, ymax = cv_emp_bias + cv_emp_sd), width = 0.2) +
  labs(title = "Mean Bias of CV (+/- SD)", y = "Mean Bias", x = "Delay") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(vjust = +4),
    axis.title.x = element_text(vjust = -2),
    axis.text.x = element_text(angle = 25, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    legend.position = "none"
  )

ggsave("figures/cv_bias_sanity_plot_empirical.pdf", bias_cv_emp, width = 7, height = 8)


## Pick a sim

one_sim <- draws_with_labels %>%
  filter(simulation %in% 1 & scenario %in% c("baseline", "no_error_no_missing")) %>%
  group_by(group, scenario, delay, iteration) %>%
  summarise(mean_mu = mean(mu, na.rm = TRUE))

baseline_data <- sim_data$baseline[[1]]
correct_data <- sim_data$no_error_no_missing[[1]]

identical(baseline_data$true_dat, baseline_data$obs_dat) # should be false
identical(correct_data$true_dat, correct_data$obs_dat) # should be true
