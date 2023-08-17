orderly2::orderly_artefact("Simulated and Augmented data", c("sim_data_baseline.rds", "aug_data_baseline.rds"))
orderly2::orderly_dependency(
  "sim_params", "latest", c(sim_params.rds = "sim_params.rds")
)
library(MixDiff)
sim_params <- readRDS("sim_params.rds")
## sim_params is a list with the following structure
## list(
##   theta_baseline = theta_baseline,
##   range_dates = range_dates,
##   index_dates = index_dates,
##   index_dates_order = index_dates_order
## )

truth <- simul_true_data(
  sim_params$theta_baseline, sim_params$n_per_group,
  sim_params$range_dates, sim_params$index_dates
)

observed <- simul_obs_dat(
  truth$true_dat, sim_params$theta_baseline,
  sim_params$range_dates
)

augmented <- list(
  D = truth$true_dat, E = observed$E
)
saveRDS(observed$obs_dat, "sim_data_baseline.rds")
saveRDS(augmented, "aug_data_baseline.rds")




