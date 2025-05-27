library(orderly2)
library(dplyr)

orderly_dependency(
  "sim_params", "latest",
  c(sim_params.rds = "sim_params.rds")
)

orderly_dependency(
  "sim_data_baseline", "latest",
  c(sim_data_baseline.rds = "sim_data_baseline.rds")
)

orderly_dependency(
  "sim_estim_baseline", "latest",
  c(sim_estim_baseline.rds = "sim_estim_baseline.rds")
)

source("utils.R")

# Read in dependencies
sim_params <- readRDS("sim_params.rds")
sim_data_baseline <- readRDS("sim_data_baseline.rds")
sim_estim_baseline <- readRDS("sim_estim_baseline.rds")

# # Create new versions of these plots:
# plot_parameter_chains(MCMCres = sim_estim_baseline[[1]], theta_true = sim_params$theta_baseline)
# dev.off()
# 
# plot_aug_dat_chains(MCMCres = sim_estim_baseline[[1]], aug_dat_true = sim_data_baseline)
# dev.off()

# Extract estimates for mu and cv
est_df <- extract_est_df(MCMCres = sim_estim_baseline[[1]],
                         theta_true = sim_params$theta_baseline)

# Add group and delay labels
est_df$group <- factor(
  est_df$group,
  levels = c("group_1", "group_2", "group_3", "group_4"),
  labels = c("Community-alive", "Community-dead", "Hospitalised-alive", "Hospitalised-dead")
)

est_df <- est_df %>% mutate(
  date_index = case_when(
    group %in% "Community-alive" & date_index == 1 ~ "Onset-to-report",
    group %in% "Community-dead" & date_index == 1 ~ "Onset-to-report",
    group %in% "Community-dead" & date_index == 2 ~ "Onset-to-death",
    group %in% "Hospitalised-alive" & date_index == 1 ~ "Onset-to-hospitalisation",
    group %in% "Hospitalised-alive" & date_index == 2 ~ "Hospitalisation-to-discharge",
    group %in% "Hospitalised-alive" & date_index == 3 ~ "Onset-to-report",
    group %in% "Hospitalised-dead" & date_index == 1 ~ "Onset-to-hospitalisation",
    group %in% "Hospitalised-dead" & date_index == 2 ~ "Hospitalisation-to-death",
    group %in% "Hospitalised-dead" & date_index == 3 ~ "Onset-to-report"
  )
)

# Plots
my_colour_scale <- c(
  "#ffb14e", "#75a8fa", "#ea5f94", "#009f47", "#2b0089"
)

ggplot(est_df, aes(x = iteration, y = mu, color = factor(date_index))) +
  geom_line() +
  facet_wrap(~ group, ncol = 4) +
  geom_hline(
    aes(yintercept = true_mu, color = factor(date_index)),
    linetype = "dashed", na.rm = TRUE
    ) +
  scale_colour_manual(values = my_colour_scale) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  ) +
  labs(y = "Mean Delay", color = "Delay")

ggplot(est_df, aes(x = iteration, y = cv, color = factor(date_index))) +
  geom_line() +
  facet_wrap(~ group, ncol = 4) +
  geom_hline(
    aes(yintercept = true_cv),
    colour = "black", linetype = "dashed", na.rm = TRUE
    ) +
  scale_colour_manual(values = my_colour_scale) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  ) +
  ylim(0, 1) +
  labs(y = "Mean CV", color = "Delay")


