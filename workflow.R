library(orderly)
#renv::install("mrc-ide/datefixer")

# Create a named list containing the simulation parameters for all scenarios
orderly_run("sim_params") # 20250606-125859-77841c4b

# Simulate data for all scenarios
orderly_run("sim_data", parameters = list(nsims = 100))

# Smaller number of sims for de-bugging
#orderly_run("sim_data", parameters = list(nsims = 10))

# This was for the sims where errors are only one date:
# orderly_run("sim_data_error", parameters = list(nsims = 10, missing_type = "all"))
# orderly_run("sim_data_error", parameters = list(nsims = 10, missing_type = "onset"))
# orderly_run("sim_data_error", parameters = list(nsims = 10, missing_type = "report"))
# orderly_run("sim_data_error", parameters = list(nsims = 10, missing_type = "hospitalisation"))
# orderly_run("sim_data_error", parameters = list(nsims = 10, missing_type = "discharge"))
# orderly_run("sim_data_error", parameters = list(nsims = 10, missing_type = "dead"))

# MCMC output -----------------------------------------------------------------
orderly_run("sim_estim", parameters = list(scenario = c("baseline")))

# rest of scenarios for sanity check
orderly_run("sim_estim", parameters = list(scenario = c("no_missing")))
orderly_run("sim_estim", parameters = list(scenario = c("no_error")))
orderly_run("sim_estim", parameters = list(scenario = c("no_error_no_missing")))

# # targetted error checks
# orderly_run("sim_estim_error", parameters = list(scenario = c("baseline"), missing_type = c("all")))
# 
# orderly_run("sim_estim_error", parameters = list(scenario = c("baseline"), missing_type = c("onset"))) # done
# orderly_run("sim_estim_error", parameters = list(scenario = c("baseline"), missing_type = c("report"))) # done
# orderly_run("sim_estim_error", parameters = list(scenario = c("baseline"), missing_type = c("hospitalisation"))) # all running
# orderly_run("sim_estim_error", parameters = list(scenario = c("baseline"), missing_type = c("discharge")))
# orderly_run("sim_estim_error", parameters = list(scenario = c("baseline"), missing_type = c("dead")))
# 
# orderly_run("sim_estim_error", parameters = list(scenario = c("no_error"), missing_type = c("onset"))) # done
# orderly_run("sim_estim_error", parameters = list(scenario = c("no_error"), missing_type = c("report"))) # done
# orderly_run("sim_estim_error", parameters = list(scenario = c("no_error"), missing_type = c("hospitalisation"))) # all running
# orderly_run("sim_estim_error", parameters = list(scenario = c("no_error"), missing_type = c("discharge")))
# orderly_run("sim_estim_error", parameters = list(scenario = c("no_error"), missing_type = c("dead")))


orderly_run("sim_estim", parameters = list(scenario = c("low_missingness"))) # done - 20250609-071748-5e8c60f2

# check for NAs
orderly_run("sim_estim", parameters = list(scenario = c("very_small_sample"))) # ISSUE - just stops for no reason
orderly_run("sim_estim", parameters = list(scenario = c("small_sample")))
orderly_run("sim_estim", parameters = list(scenario = c("moderate_sample")))
orderly_run("sim_estim", parameters = list(scenario = c("very_large_sample"))) # done - 20250608-200937-b66ddee7. ran very slowly (10 hours!)

orderly_run("sim_estim", parameters = list(scenario = c("low_error"))) # done - 20250609-124217-eb8f433f
orderly_run("sim_estim", parameters = list(scenario = c("high_error"))) # done - 20250609-153641-a4c6f89f

orderly_run("sim_estim", parameters = list(scenario = c("long_delays"))) # done - 20250609-212221-141df17c
orderly_run("sim_estim", parameters = list(scenario = c("short_delays")))

# Push outputs to onedrive
orderly_location_push("20250606-125859-77841c4b", "personal_onedrive") # sim_params
#orderly_location_push("20250606-160753-98a0b168", "personal_onedrive") # sim_data
orderly_location_push("20250704-145813-8ad87966", "personal_onedrive") # sim_data - changed discretisation
orderly_location_push("20250608-094618-e1df9d80", "personal_onedrive") # sim_estim: baseline
orderly_location_push(c("20250609-071748-5e8c60f2",
                                  "20250608-183314-5a94d6f0",
                                  "20250608-200937-b66ddee7",
                                  "20250609-124217-eb8f433f",
                                  "20250609-153641-a4c6f89f",
                                  "20250609-212221-141df17c"), "personal_onedrive")
orderly_location_push("20250611-094732-dc3580f6", "personal_onedrive")
orderly_location_push("20250704-145909-b81d711a", "personal_onedrive") # baseline using new sim_data
orderly_location_push("20250704-172828-0d743aed", "personal_onedrive") # moderate sample using new sim_data
# on other device:
# orderly_location_fetch_metadata("personal_onedrive")
# orderly_location_pull("<id>") or orderly_location_pull(expr = NULL)

# running in scenario batches
scenarios <- c("very_small_sample",
               "small_sample",
               "moderate_sample",
               "very_large_sample")

for (s in scenarios) {
  orderly_run("sim_estim", parameters = list(scenario = s))
}

