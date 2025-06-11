library(orderly2)

# Create a named list containing the simulation parameters for all scenarios
orderly_run("sim_params") # 20250606-125859-77841c4b

# Simulate data for all scenarios
orderly_run("sim_data", parameters = list(nsims = 100)) # 20250606-160753-98a0b168

# MCMC output -----------------------------------------------------------------
orderly_run("sim_estim", parameters = list(scenario = c("baseline"))) # done - 20250608-094618-e1df9d80

orderly_run("sim_estim", parameters = list(scenario = c("low_missingness"))) # done - 20250609-071748-5e8c60f2

orderly_run("sim_estim", parameters = list(scenario = c("very_small_sample"))) # ISSUE - just stops for no reason
orderly_run("sim_estim", parameters = list(scenario = c("small_sample")))
orderly_run("sim_estim", parameters = list(scenario = c("moderate_sample"))) # done - 20250608-183314-5a94d6f0
orderly_run("sim_estim", parameters = list(scenario = c("very_large_sample"))) # done - 20250608-200937-b66ddee7. ran very slowly (10 hours!)

orderly_run("sim_estim", parameters = list(scenario = c("low_error"))) # done - 20250609-124217-eb8f433f
orderly_run("sim_estim", parameters = list(scenario = c("high_error"))) # done - 20250609-153641-a4c6f89f

orderly_run("sim_estim", parameters = list(scenario = c("long_delays"))) # done - 20250609-212221-141df17c
orderly_run("sim_estim", parameters = list(scenario = c("short_delays")))

# Push outputs to onedrive
orderly2::orderly_location_push("20250606-125859-77841c4b", "personal_onedrive") # sim_params
orderly2::orderly_location_push("20250606-160753-98a0b168", "personal_onedrive") # sim_data
orderly2::orderly_location_push("20250608-094618-e1df9d80", "personal_onedrive") # sim_estim: baseline
orderly2::orderly_location_push(c("20250609-071748-5e8c60f2",
                                  "20250608-183314-5a94d6f0",
                                  "20250608-200937-b66ddee7",
                                  "20250609-124217-eb8f433f",
                                  "20250609-153641-a4c6f89f",
                                  "20250609-212221-141df17c"), "personal_onedrive")
orderly2::orderly_location_push("20250611-094732-dc3580f6", "personal_onedrive")
# on other device:
# orderly_location_fetch_metadata("personal_onedrive)
# orderly_location_pull("<id>") or orderly_location_pull(expr = NULL)

# running in scenario batches
scenarios <- c("very_small_sample",
               "small_sample",
               "moderate_sample",
               "very_large_sample")

for (s in scenarios) {
  orderly_run("sim_estim", parameters = list(scenario = s))
}

