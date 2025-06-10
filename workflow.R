library(orderly2)

# Create a named list containing the simulation parameters for all scenarios
orderly_run("sim_params")

# Simulate data for all scenarios
orderly_run("sim_data", parameters = list(nsims = 100))

# MCMC output -----------------------------------------------------------------
orderly_run("sim_estim", parameters = list(scenario = c("baseline"))) # done

orderly_run("sim_estim", parameters = list(scenario = c("low_missingness"))) # done

orderly_run("sim_estim", parameters = list(scenario = c("very_small_sample"))) # ISSUE - just stops for no reason
orderly_run("sim_estim", parameters = list(scenario = c("small_sample")))
orderly_run("sim_estim", parameters = list(scenario = c("moderate_sample"))) # done
orderly_run("sim_estim", parameters = list(scenario = c("very_large_sample"))) # done - ran very slowly (10 hours!)

orderly_run("sim_estim", parameters = list(scenario = c("low_error"))) # done
orderly_run("sim_estim", parameters = list(scenario = c("high_error"))) # done

orderly_run("sim_estim", parameters = list(scenario = c("long_delays"))) # done
orderly_run("sim_estim", parameters = list(scenario = c("short_delays")))


# running in scenario batches
scenarios <- c("very_small_sample",
               "small_sample",
               "moderate_sample",
               "very_large_sample")

for (s in scenarios) {
  orderly_run("sim_estim", parameters = list(scenario = s))
}

