library(orderly2)
orderly_dependency(
  "sim_params", "latest", c(sim_params.rds = "sim_params.rds")
  )
orderly_dependency(
  "sim_data_baseline", "latest",
  c(sim_data_baseline.rds = "sim_data_baseline.rds")
)
library(MixDiff)
sim_params <- readRDS("sim_params.rds")
index_dates <- sim_params$index_dates
sim_data_baseline <- readRDS("sim_data_baseline.rds")
mcmc_settings <- list(
  moves_switch = list(
    D_on = TRUE, E_on = TRUE,
    swapE_on = TRUE, mu_on = TRUE,
    CV_on = TRUE, zeta_on = TRUE
  ),
  moves_options = list(
    fraction_Di_to_update = 1/10, move_D_by_groups_of_size = 1, fraction_Ei_to_update = 1/10,
    sdlog_mu = list(
      0.05,
      c(0.15, 0.15),
      c(0.15, 0.15, 0.15),
      c(0.25, 0.25, 0.25)
    ),
    sdlog_CV = list(0.25, c(0.25, 0.25), c(0.25, 0.25, 0.25), c(0.25, 0.25, 0.25))),
  init_options = list(mindelay=0, maxdelay=100),
  chain_properties=list(n_iter = 5000, burnin = 500, record_every=10)
)

hyperparameters <- list(
  shape1_prob_error=3,
  shape2_prob_error=12,
  mean_mean_delay=100,
  mean_CV_delay=100)

mcmc_out <- RunMCMC(
  sim_data_baseline,
  mcmc_settings,
  hyperparameters,
  index_dates
)