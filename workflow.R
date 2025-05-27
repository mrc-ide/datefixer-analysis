library(orderly2)

# Simulation parameters
orderly_run("sim_params")

# Simulate baseline data
orderly_run("sim_data_baseline", parameters = list(nsims = 1))

# MCMC output for baseline estim
orderly_run("sim_estim_baseline")
