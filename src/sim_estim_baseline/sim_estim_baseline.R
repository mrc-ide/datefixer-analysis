library(orderly2)

orderly_dependency(
  "sim_params", "latest",
  c(sim_params.rds = "sim_params.rds")
  )

orderly_dependency(
  "sim_data_baseline", "latest",
  c(sim_data_baseline.rds = "sim_data_baseline.rds")
  )

orderly_artefact(
  description = "MCMC output for baseline sim",
  files = "sim_estim_baseline.rds"
  )

library(MixDiff)


################################################################################
# Read in dependencies

sim_params <- readRDS("sim_params.rds")
sim_data_baseline <- readRDS("sim_data_baseline.rds")


################################################################################
# MCMC settings

mcmc_settings <- list(
  # moves_switch: List of booleans stating whether each parameter/augmented data
  # should be moved in the procedure or not.
  moves_switch = list(
    D_on = TRUE, # augmented dates (latent true dates)
    E_on = TRUE, # error indicators (-1, 0, 1)
    swapE_on = TRUE, # swaps error indicators - explore alternative errors
    mu_on = TRUE, # mean of each delay distribution
    CV_on = TRUE, # cv of each delay
    zeta_on = TRUE # probability of error
  ),
  # moves_options: 
  moves_options = list(
    # Fraction of augmented dates to be updated at each iteration of the MCMC.
    fraction_Di_to_update = 1 / 10,
    # Number of augmented dates to be updated simultaneously in each group.
    move_D_by_groups_of_size = 1,
    # Fraction of indicators of whether observed dates are erroneous to be
    # updated at each iteration of the MCMC.
    fraction_Ei_to_update = 1 / 10,
    # List of standard deviations to be used for proposing moves of the mean
    # delays of length n_groups. Each element in the list should be a vector
    # with length given by the numbers of delays to be considered in this group.
    sdlog_mu = list(
      0.05,
      c(0.15, 0.15),
      c(0.15, 0.15, 0.15),
      c(0.25, 0.25, 0.25)
    ),
    # Same as above but for proposing moves of the CV of delays.
    sdlog_CV = list(
      0.25, c(0.25, 0.25), c(0.25, 0.25, 0.25), c(0.25, 0.25, 0.25))
    ),
  # minimum and maximum delays, below/above which dates are considered
  # incompatible with one another at the initialisation stage of the MCMC.
  init_options = list(mindelay = 0, maxdelay = 100),
  # total number of iterations, initial burnin and then after burnin how many
  # iterations should be recorded (thinning). (500 - 50) / 10 = 45 samples from
  # the posterior for each dataset.
  chain_properties = list(n_iter = 500, burnin = 50, record_every = 10)
)


################################################################################
# Hyperparameters

hyperparameters <- list(
  # scalars giving the first and second shape parameters for the beta prior
  # used for zeta
  shape1_prob_error = 3,
  shape2_prob_error = 12,
  # scalars giving the mean of the exponential prior used for mu and CV
  mean_mean_delay = 100,
  mean_CV_delay = 100
  )


################################################################################
# Run MCMC

# Number of simulations
nsims <- length(sim_data_baseline)

# Which delays to use to simulate subsequent dates from the first, in each group
index_dates <- sim_params$index_dates

mcmc_out <- lapply(seq_len(nsims), function(idx) {
  x <- sim_data_baseline[[idx]]
  message("Processing ", idx)
  # Remove rows with only NAs
  x$obs_dat <- lapply(
    x$obs_dat, function(y) {
      all_nas <- apply(y, 1, function(row) all(is.na(row)))
      y[!all_nas, ]
    }
  )
  RunMCMC(
    x$obs_dat,
    mcmc_settings,
    hyperparameters,
    index_dates
  )
})

saveRDS(mcmc_out, "sim_estim_baseline.rds")
