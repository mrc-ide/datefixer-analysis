library(orderly2)
orderly_artefact("Simulated Data", c("sim_data_baseline.rds"))
orderly_dependency(
  "sim_params", "latest", c(sim_params.rds = "sim_params.rds")
)
## Number of data sets to simulate
orderly_parameters(nsims = 1)
library(MixDiff)
sim_params <- readRDS("sim_params.rds")
## sim_params is a list with the following structure
## list(
##   theta_baseline = theta_baseline,
##   range_dates = range_dates,
##   index_dates = index_dates,
##   index_dates_order = index_dates_order
## )
out <- vector(
  mode = "list", length = nsims
)
for (idx in seq_len(nsims)) {
  out[[idx]] <- simul_true_data(
    sim_params$theta_baseline, sim_params$n_per_group,
    sim_params$range_dates, sim_params$index_dates,
    simul_error = TRUE,
    remove_allNA_indiv = TRUE
  )
}

saveRDS(out, "sim_data_baseline.rds")





