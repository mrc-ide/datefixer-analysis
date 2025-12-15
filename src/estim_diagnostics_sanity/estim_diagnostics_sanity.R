# sanity check

# Summary of sample scenarios

library(orderly)
library(dplyr)
library(patchwork)
library(purrr)
library(ggplot2)

orderly_dependency("sim_params", "latest", "sim_params.rds")
orderly_dependency("sim_data", "latest", "sim_data.rds")

scenarios <- c("baseline", "no_error", "no_missing", "no_error_no_missing")

for (s in scenarios) {
orderly_dependency("sim_estim",
                   "latest(parameter:scenario == environment:s)",
                   c("sim_estim_${s}.rds" = "sim_estim.rds"))
}

orderly_artefact(files = c("figures/trace_plot.pdf",
                           "figures/bias_plot.pdf",
                           "figures/bias_plot_empirical.pdf",
                           "figures/coverage_plot.pdf",
                           "figures/sim_summaries.rds",
                           "figures/agg_summaries.rds"),
                 description = "Analysis outputs")

dir.create("figures")
#source("utils.R")

# Read in dependencies
sim_params <- readRDS("sim_params.rds")
sim_data <- readRDS("sim_data.rds")

sim_estim <- setNames(
  lapply(scenarios, function(s) {
    filename <- paste0("sim_estim_", s, ".rds")
    readRDS(filename)[[1]]
  }),
  scenarios
)

## Plan for across simulation summaries --------------------------------------
# Mean bias (posterior mean - true value)
# Coverage (prop of simulations where true value falls within 50% and 95% CrIs)
# RMSE (sqrt((mean-true)^2))
# Average CrI width


# Map the estimated delays ---------------------------------------------------
delay_mapping <- tribble(
  ~param_idx, ~delay_from,         ~delay_to,            ~groups,
  1,          "onset",             "report",             "1,2,3,4",
  2,          "onset",             "death",              "2",
  3,          "onset",             "hospitalisation",    "3",
  4,          "hospitalisation",   "discharge",          "3",
  5,          "onset",             "hospitalisation",    "4",
  6,          "hospitalisation",   "death",              "4"
) %>%
  mutate(group_list = strsplit(groups, ",")) %>%
  tidyr::unnest(group_list) %>%
  mutate(group = paste0("Group ", trimws(group_list))) %>%
  select(-group_list, -groups)

# Extract true parameter values ----------------------------------------------
true_params <- map_dfr(names(sim_params), function(scenario_name) {
  params <- sim_params[[scenario_name]]
  
  # Match delay_params rows to the estimated delays
  delay_params <- params$delay_params

  # For onset to report can take the first value (now they're all the same)
  true_vals <- tibble(
    scenario = scenario_name,
    param_idx = c(1, 1, 1, 1, 2, 3, 4, 5, 6),
    group = c("Group 1", "Group 2", "Group 3", "Group 4",
              "Group 2", "Group 3", "Group 3", "Group 4", "Group 4"),
    true_mean = delay_params$delay_mean,
    true_cv =  delay_params$delay_cv,
  ) %>%
    left_join(delay_mapping, by = c("param_idx", "group"))
  
  true_vals
})

# Extract empirical mean/CV from simulated true data ------------------------

empirical_params <- map_dfr(names(sim_data), function(scenario_name) {
  scenario_sims <- sim_data[[scenario_name]]
  
  # For each simulation, calculate empirical delays
  map_dfr(seq_along(scenario_sims), function(sim_idx) {
    true_data <- scenario_sims[[sim_idx]]$true_data
    
    # Calculate delays for each type
    delays <- true_data %>%
      mutate(
        onset_report = report - onset,
        onset_death = death - onset,
        onset_hosp = hospitalisation - onset,
        hosp_discharge = discharge - hospitalisation,
        hosp_death = death - hospitalisation
      )
    
    # Empirical means and CVs for each delay type
    tibble(
      scenario = scenario_name,
      simulation = sim_idx,
      param_idx = c(1, 1, 1, 1, 2, 3, 4, 5, 6),
      group = c("Group 1", "Group 2", "Group 3", "Group 4",
                "Group 2", "Group 3", "Group 3", "Group 4", "Group 4"),
      empirical_mean = c(
        mean(delays$onset_report[delays$group == 1]),
        mean(delays$onset_report[delays$group == 2]),
        mean(delays$onset_report[delays$group == 3]),
        mean(delays$onset_report[delays$group == 4]),
        mean(delays$onset_death[delays$group == 2]),
        mean(delays$onset_hosp[delays$group == 3]),
        mean(delays$hosp_discharge[delays$group == 3]),
        mean(delays$onset_hosp[delays$group == 4]),
        mean(delays$hosp_death[delays$group == 4])
      ),
      empirical_cv = c(
        sd(delays$onset_report[delays$group == 1]) / mean(delays$onset_report[delays$group == 1]),
        sd(delays$onset_report[delays$group == 2]) / mean(delays$onset_report[delays$group == 2]),
        sd(delays$onset_report[delays$group == 3]) / mean(delays$onset_report[delays$group == 3]),
        sd(delays$onset_report[delays$group == 4]) / mean(delays$onset_report[delays$group == 4]),
        sd(delays$onset_death[delays$group == 2]) / mean(delays$onset_death[delays$group == 2]),
        sd(delays$onset_hosp[delays$group == 3]) / mean(delays$onset_hosp[delays$group == 3]),
        sd(delays$hosp_discharge[delays$group == 3]) / mean(delays$hosp_discharge[delays$group == 3]),
        sd(delays$onset_hosp[delays$group == 4]) / mean(delays$onset_hosp[delays$group == 4]),
        sd(delays$hosp_death[delays$group == 4]) / mean(delays$hosp_death[delays$group == 4])
      )
    )
  })
}) %>%
  left_join(delay_mapping, by = c("param_idx", "group"))


# Extract MCMC draws ---------------------------------------------------------

extract_draws <- function(mcmc_result, scenario_name, sim_idx) {
  # mcmc_result is a monty_samples object
  pars <- mcmc_result$pars  # [n_params, n_iterations, n_chains]

  n_iter <- dim(pars)[2]
  n_chains <- dim(pars)[3]
  
  # Extract mean and CV parameters
  draws_list <- list()
  
  for (i in 1:6) {
    mean_name <- paste0("mean_delay", i)
    cv_name <- paste0("cv_delay", i)
    
    mean_draws <- as.vector(pars[mean_name, , ])
    cv_draws <- as.vector(pars[cv_name, , ])
    
    draws_list[[i]] <- tibble(
      scenario = scenario_name,
      simulation = sim_idx,
      param_idx = i,
      iteration = rep(1:n_iter, n_chains),
      chain = rep(1:n_chains, each = n_iter),
      mean_delay = mean_draws,
      cv_delay = cv_draws
    )
  }
  
  bind_rows(draws_list)
}

# Extract all draws
all_draws <- map_dfr(names(sim_estim), function(scenario_name) {
  scenario_sims <- sim_estim[[scenario_name]]
  
  map_dfr(seq_along(scenario_sims), function(sim_idx) {
    extract_draws(scenario_sims[[sim_idx]], scenario_name, sim_idx)
  })
})

# Add delay labels
all_draws <- all_draws %>%
  left_join(delay_mapping, by = c("param_idx")) %>%
  mutate(delay_label = glue::glue("{delay_from} to {delay_to}"))

# Calculate posterior summaries per simulation -------------------------------

sim_summaries <- all_draws %>%
  group_by(scenario, simulation, param_idx, delay_label, delay_from, delay_to, group) %>%
  summarise(
    post_mean_mean = mean(mean_delay),
    post_mean_cv = mean(cv_delay),
    post_q025_mean = quantile(mean_delay, 0.025),
    post_q975_mean = quantile(mean_delay, 0.975),
    post_q25_mean = quantile(mean_delay, 0.25),
    post_q75_mean = quantile(mean_delay, 0.75),
    post_q025_cv = quantile(cv_delay, 0.025),
    post_q975_cv = quantile(cv_delay, 0.975),
    post_q25_cv = quantile(cv_delay, 0.25),
    post_q75_cv = quantile(cv_delay, 0.75),
    .groups = "drop"
  )

# Join with true values
sim_summaries <- sim_summaries %>%
  left_join(true_params,
            by = c("scenario", "param_idx", "group",
                   "delay_from", "delay_to"))

# Join with empirical values
sim_summaries <- sim_summaries %>%
  left_join(empirical_params,
            by = c("scenario", "simulation", "param_idx", "group",
                   "delay_from", "delay_to"))

# Calculate metrics
sim_summaries <- sim_summaries %>%
  mutate(
    # Bias - ground truth
    bias_mean = post_mean_mean - true_mean,
    bias_cv = post_mean_cv - true_cv,
    # Bias - empirical
    bias_mean_emp = post_mean_mean - empirical_mean,
    bias_cv_emp = post_mean_cv - empirical_cv,
    # Coverage
    cov95_mean = true_mean >= post_q025_mean & true_mean <= post_q975_mean,
    cov50_mean = true_mean >= post_q25_mean & true_mean <= post_q75_mean,
    cov95_cv = true_cv >= post_q025_cv & true_cv <= post_q975_cv,
    cov50_cv = true_cv >= post_q25_cv & true_cv <= post_q75_cv,
    # CrI width
    width95_mean = post_q975_mean - post_q025_mean,
    width50_mean = post_q75_mean - post_q25_mean,
    width95_cv = post_q975_cv - post_q025_cv,
    width50_cv = post_q75_cv - post_q25_cv
  )

# Aggregate across simulations -----------------------------------------------

agg_summaries <- sim_summaries %>%
  mutate(scenario = factor(
    scenario, levels = c("baseline", "no_error",
                         "no_missing", "no_error_no_missing")
    )) %>%
  group_by(scenario, param_idx, delay_label, delay_from, delay_to, group) %>%
  summarise(
    # Bias
    mean_bias = mean(bias_mean, na.rm = TRUE),
    sd_bias = sd(bias_mean, na.rm = TRUE),
    cv_mean_bias = mean(bias_cv, na.rm = TRUE),
    cv_sd_bias = sd(bias_cv, na.rm = TRUE),
    # Empirical bias
    mean_emp_bias = mean(bias_mean_emp, na.rm = TRUE),
    mean_emp_sd = sd(bias_mean_emp, na.rm = TRUE),
    cv_emp_bias = mean(bias_cv_emp, na.rm = TRUE),
    cv_emp_sd = sd(bias_cv_emp, na.rm = TRUE),
    # RMSE
    mean_rmse = sqrt(mean(bias_mean^2, na.rm = TRUE)),
    cv_rmse = sqrt(mean(bias_cv^2, na.rm = TRUE)),
    # Coverage
    mean_cov95 = mean(cov95_mean, na.rm = TRUE),
    mean_cov50 = mean(cov50_mean, na.rm = TRUE),
    cv_cov95 = mean(cov95_cv, na.rm = TRUE),
    cv_cov50 = mean(cov50_cv, na.rm = TRUE),
    # Width
    mean_width95 = mean(width95_mean, na.rm = TRUE),
    mean_width50 = mean(width50_mean, na.rm = TRUE),
    cv_width95 = mean(width95_cv, na.rm = TRUE),
    cv_width50 = mean(width50_cv, na.rm = TRUE),
    .groups = "drop"
  )

# Trace plots ----------------------------------------------------------------

# Average across simulations
trace_data <- all_draws %>%
  group_by(scenario, param_idx, delay_label, iteration, group) %>%
  summarise(mean_delay = mean(mean_delay, na.rm = TRUE), .groups = "drop") %>%
  left_join(true_params %>% select(scenario, param_idx, group, true_mean),
            by = c("scenario", "param_idx", "group"))

trace_plot_sanity <- ggplot(trace_data, 
                            aes(x = iteration, y = mean_delay,
                                colour = delay_label)) +
  geom_line(alpha = 0.7) +
  geom_hline(aes(yintercept = true_mean, colour = delay_label), 
             linetype = "dashed", linewidth = 0.8) +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
  labs(y = "Mean Delay",
       x = "Iteration",
       colour = "Delay") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("figures/trace_plot.pdf", trace_plot_sanity, width = 12, height = 10)

# Bias plots -----------------------------------------------------------------

# Compare to true mean delay (ground truth)
bias_plot <- ggplot(agg_summaries, 
                    aes(x = delay_label, y = mean_bias, colour = delay_label)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(ymin = mean_bias - sd_bias, ymax = mean_bias + sd_bias), 
                width = 0.3) +
  facet_grid(rows = vars(group), cols = vars(scenario)) +
  labs(title = "Mean Bias of Delay Parameters (+/- SD)",
       subtitle = "Compared to ground truth",
       y = "Mean Bias",
       x = "Delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("figures/bias_plot.pdf", bias_plot, width = 10, height = 8)

# Compare to mean delay of simulated true data (sample truth)
bias_emp_plot <- ggplot(agg_summaries, 
                        aes(x = delay_label, y = mean_emp_bias, colour = delay_label)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(ymin = mean_emp_bias - mean_emp_sd, 
                    ymax = mean_emp_bias + mean_emp_sd), 
                width = 0.3) +
  facet_grid(rows = vars(group), cols = vars(scenario)) +
  labs(title = "Mean Bias of Delay Parameters (+/- SD)",
       subtitle = "Compared to sample truth",
       y = "Mean Bias",
       x = "Delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("figures/bias_plot_empirical.pdf", bias_emp_plot, width = 10, height = 8)

# CV bias plots
cv_bias_plot <- ggplot(agg_summaries, 
                       aes(x = delay_label, y = cv_mean_bias, colour = delay_label)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(ymin = cv_mean_bias - cv_sd_bias, 
                    ymax = cv_mean_bias + cv_sd_bias), 
                width = 0.3) +
  facet_grid(rows = vars(group), cols = vars(scenario)) +
  labs(title = "Mean Bias of CV Parameters (+/- SD)",
       subtitle = "Compared to ground truth",
       y = "Mean Bias",
       x = "Delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1))

ggsave("figures/cv_bias_plot.pdf", cv_bias_plot, width = 10, height = 8)

# Coverage plot
coverage_data <- agg_summaries %>%
  select(scenario, group, delay_label, mean_cov95, mean_cov50) %>%
  tidyr::pivot_longer(cols = c(mean_cov95, mean_cov50),
                      names_to = "interval",
                      values_to = "coverage") %>%
  mutate(interval = ifelse(interval == "mean_cov95", "95% CrI", "50% CrI"))

coverage_plot <- ggplot(coverage_data, 
                        aes(x = delay_label, y = coverage, 
                            colour = delay_label, shape = interval)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "gray40") +
  geom_hline(yintercept = 0.50, linetype = "dashed", colour = "gray40") +
  facet_grid(rows = vars(group), cols = vars(scenario)) +
  labs(title = "Coverage of Credible Intervals",
       y = "Coverage Probability",
       x = "Delay",
       shape = "") +
  ylim(0, 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)) +
  guides(colour = "none")

ggsave("figures/coverage_plot.pdf", coverage_plot, width = 10, height = 8)

# Save summary tables
saveRDS(sim_summaries, "figures/sim_summaries.rds")
saveRDS(agg_summaries, "figures/agg_summaries.rds")
