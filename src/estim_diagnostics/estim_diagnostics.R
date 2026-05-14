library(orderly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(glue)
library(stringr)
library(ggrastr)
library(forcats)

pars <- orderly_parameters(n_steps = NULL, burnin = NULL,
                           thinning_factor = NULL, scenarios = NULL)

n_steps <- n_steps
burnin <- burnin
thinning_factor <- thinning_factor
scenarios <- strsplit(scenarios, ",")[[1]]

# Loop through scenarios and fetch the individual summaries
for (s in scenarios) {
  remote_files <- c(
    "results/true_params.rds",
    "results/all_draws.rds",
    "results/sim_summaries.rds",
    "results/agg_summaries.rds",
    "results/observed_patterns.rds",
    "results/indiv_obs_summary.rds",
    "results/obs_pattern_summary.rds",
    "results/indiv_event_status.rds",
    "results/event_confusion.rds",
    "results/indiv_performance_summary.rds",
    "results/convergence_issues_by_individual.rds"
  )
  
  local_names <- glue("{str_remove(basename(remote_files), '\\\\.rds')}_{s}.rds")
  deps_mapping <- setNames(remote_files, local_names)
  
  orderly_dependency(
    "estim_summary", 
    "latest(parameter:scenario == environment:s &&
    parameter:n_steps == environment:n_steps &&
    parameter:burnin == environment:burnin &&
    parameter:thinning_factor == environment:thinning_factor)",
    deps_mapping
  )
}

orderly_artefact(files = c("results/figures/trace_error.pdf",
                           "results/figures/trace_delays_10.pdf",
                           "results/figures/trace_delays_all.pdf",
                           "results/figures/bias_plot_delays_gt.pdf",
                           "results/figures/bias_plot_error_gt.pdf",
                           "results/figures/bias_plot_cv_gt.pdf",
                           "results/figures/coverage_plot.pdf",
                           "results/figures/posterior_delay_mean.pdf",
                           "results/figures/posterior_cv.pdf",
                           "results/figures/posterior_prob_error.pdf",
                           "results/figures/observed_patterns.pdf",
                           "results/figures/ess_plot.pdf",
                           "results/figures/problem_traces",
                           "results/figures/rhat_vs_ess.pdf",
                           "results/figures/width_vs_ess.pdf",
                           "results/figures/sensitivity_specificity_events.pdf",
                           "results/figures/sensitivity_specificity_individuals.pdf",
                           "results/figures/problem_shared_patterns.pdf"),
                 description = "Diagnostic figures")

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

# Bind all the individual scenarios together -----------------------------------
true_params <- map_dfr(scenarios, ~readRDS(glue("true_params_{.x}.rds")))
all_draws <- map_dfr(scenarios, ~readRDS(glue("all_draws_{.x}.rds")))
sim_summaries <- map_dfr(scenarios, ~readRDS(glue("sim_summaries_{.x}.rds")))
agg_summaries <- map_dfr(scenarios, ~readRDS(glue("agg_summaries_{.x}.rds")))
observed_patterns <- map_dfr(scenarios, ~readRDS(glue("observed_patterns_{.x}.rds")))
indiv_obs_summary <- map_dfr(scenarios, ~readRDS(glue("indiv_obs_summary_{.x}.rds")))
obs_pattern_summary <- map_dfr(scenarios, ~readRDS(glue("obs_pattern_summary_{.x}.rds")))
indiv_event_status <- map_dfr(scenarios, ~readRDS(glue("indiv_event_status_{.x}.rds")))
event_confusion <- map_dfr(scenarios, ~readRDS(glue("event_confusion_{.x}.rds")))
indiv_performance <- map_dfr(scenarios, ~readRDS(glue("indiv_performance_summary_{.x}.rds")))
problem_sims <- map_dfr(scenarios, ~readRDS(glue("convergence_issues_by_individual_{.x}.rds")))

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

scenario_labels <- scenario_labels[intersect(names(scenario_labels), scenarios)]
scenario_labels <- factor(scenario_labels, levels = scenario_labels)

# Re-apply correct factor orderings after the bind_rows ----------------------
apply_factor_levels <- function(df) {
  if("scenario" %in% names(df)) {
    df$scenario <- factor(df$scenario, levels = unname(scenario_labels))
  }
  if("group" %in% names(df)) {
    df$group <- factor(df$group, levels = c("community-alive", "community-dead", 
                                            "hospitalised-alive", "hospitalised-dead"))
  }
  if("param_label" %in% names(df)) {
    df$param_label <- factor(df$param_label, levels = c(
      "probability of error", "onset to report", "onset to death", 
      "onset to hospitalisation", "hospitalisation to discharge", "hospitalisation to death"
    ))
  }
  df
}

true_params <- apply_factor_levels(true_params)
all_draws <- apply_factor_levels(all_draws)
sim_summaries <- apply_factor_levels(sim_summaries)
agg_summaries <- apply_factor_levels(agg_summaries)
observed_patterns <- apply_factor_levels(observed_patterns)
indiv_obs_summary <- apply_factor_levels(indiv_obs_summary)
obs_pattern_summary <- apply_factor_levels(obs_pattern_summary)
indiv_event_status <- apply_factor_levels(indiv_event_status)
event_confusion <- apply_factor_levels(event_confusion)
indiv_performance <- apply_factor_levels(indiv_performance)
problem_sims <- apply_factor_levels(problem_sims)


# Plots ----------------------------------------------------------------------

# Event level sensitivity and specificity
plot_event_perf <- event_confusion %>%
  ggplot(aes(x = event, y = pct_accuracy, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(threshold ~ scenario + metric_type) + 
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Accuracy in identifying individual erroneous vs correct dates",
       x = "Event Type",
       y = "Accuracy (%)",
       fill = "Group") +
  theme_bw() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "darkgrey",
                                    fill = NA, linewidth = 1))

ggsave("results/figures/sensitivity_specificity_events.pdf", plot_event_perf,
       width = 14, height = 10)

# Individual level sensitivity and specificity
plot_indiv_perf <- indiv_performance %>%
  ggplot(aes(x = group, y = pct_accuracy, fill = metric_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(threshold ~ scenario) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Individual-level Performance",
       subtitle = "Restricted to individuals with >= 2 recorded dates",
       x = "Group",
       y = "Accuracy (%)",
       fill = "Metric") +
  theme_bw() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "darkgrey",
                                    fill = NA, linewidth = 1),
        legend.position = "bottom")

ggsave("results/figures/sensitivity_specificity_individuals.pdf",
       plot_indiv_perf, width = 14, height = 10)

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
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

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
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
  
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

if (burnin == 0) trace_data_base <- trace_data_base %>% filter(iteration > 100)

trace_10 <- make_trace_plot(
  trace_data_base %>% filter(simulation <= 10),
  post_mean, true_mean,
  "Trace plots (first 10 simulations)", "Mean Delay",
  add_symbols = TRUE
)

trace_all <- make_trace_plot(
  trace_data_base,
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
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
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
          panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)))
}

ggsave("results/figures/bias_plot_delays_gt.pdf",
       agg_summaries %>% filter(!param_label %in% "probability of error") %>%
         make_bias_plot(bias_gt_avg,  bias_gt_sd,
                        "Median Bias of Delay Parameters (+/- SD)",
                        "Compared to Ground Truth"),
       width = 10, height = 8)

ggsave("results/figures/bias_plot_error_gt.pdf",
       agg_summaries %>% filter(param_label %in% "probability of error") %>%
         make_error_bias_plot(bias_gt_avg,  bias_gt_sd,
                              "Median Bias: Probability of Error",
                              "Compared to Ground Truth"),
       width = 14, height = 4)

ggsave("results/figures/bias_plot_cv_gt.pdf",
       agg_summaries %>% filter(!param_label %in% "probability of error") %>%
         make_bias_plot(cv_bias_gt_avg,  cv_bias_gt_sd,
                        "Median Bias of CV Parameters (+/- SD)",
                        "Compared to Ground Truth"),
       width = 10, height = 8)

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
                                      linewidth = 1),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10))) +
    guides(colour = "none")
}

ggsave(
  "results/figures/coverage_plot.pdf",
  agg_summaries %>% filter(!param_label %in% "probability of error") %>%
    make_coverage_plot(
      cov95_gt_pct, cov50_gt_pct,
      "True parameters (ground truth). Error bars: 95% binomial confidence intervals"
    ),
  width = 10, height = 8)

# Posterior density plots
posterior_data <- all_draws %>%
  filter(param_label != "probability of error")

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
               panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))),
       width = 14, height = 10)

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
               panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))),
       width = 14, height = 10)

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
               panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))),
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
         theme(strip.text = element_text(size = 9, face = "bold"),
               legend.position = "none",
               panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))),
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
           strip.text = element_text(face = "bold"),
           axis.title.x = element_text(margin = margin(t = 10)),
           axis.title.y = element_text(margin = margin(r = 10))
         ),
       width = 14, height = 7)

# Problem traces
dir.create("results/figures/problem_traces", recursive = TRUE, showWarnings = FALSE)

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
            legend.position = "bottom",
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)))
    
    clean_scen <- str_replace_all(scen, "[^[:alnum:]]", "_")
    file_name <- file.path("results/figures/problem_traces", glue("trace_{clean_scen}_sim{sim}.pdf"))
    
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
               legend.position = "none",
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))),
       width = 12, height = 8)

# see if low ESS correlates with wider crIs
low_ess_threshold <- 200
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
               legend.position = "none",
               axis.title.x = element_text(margin = margin(t = 10)),
               axis.title.y = element_text(margin = margin(r = 10))),
       width = 12, height = 8)

# Compare features
pattern_comparison <- indiv_obs_summary %>%
  left_join(
    problem_combos %>% mutate(is_problem = TRUE),
    by = c("scenario", "simulation")
  ) %>%
  mutate(is_problem = replace_na(is_problem, FALSE)) %>%
  group_by(is_problem, scenario, group, pattern) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(is_problem, scenario, group) %>%
  mutate(pct = n / sum(n) * 100)

plot_pattern_comparison <- ggplot(pattern_comparison, 
                                  aes(x = reorder(pattern, n),
                                      y = pct,
                                      fill = is_problem)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("FALSE" = "lightgrey", "TRUE" = "firebrick"),
                    labels = c("Good", "Problematic")) +
  coord_flip() +
  facet_grid(rows = vars(group), cols = vars(scenario), scales = "free_y") +
  labs(title = "Are specific error patterns driving non-convergence?",
       subtitle = "Comparing pattern frequencies in problematic vs good runs",
       x = "Simulated Pattern",
       y = "Percentage of Individuals (%)",
       fill = "Run") +
  theme_minimal() +
  theme(strip.text = element_text(size = 9, face = "bold"),
        panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
        legend.position = "top")

ggsave("results/figures/problem_shared_patterns.pdf", plot_pattern_comparison,
       width = 16, height = 8)
