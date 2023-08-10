orderly2::orderly_artefact("Simulation parameters", "sim_params.rds")
library(MixDiff)
## Baseline
n_groups <- 4
n_dates <- c(2, 3, 4, 4)
theta_baseline <- list(
  prop_missing_date = 0.05,
  zeta = 0.05,
  mu = list(
    5, c(6, 7), c(8, 9, 10), c(11, 12, 13)
  ),
  CV = list(
    0.5, c(0.5, 0.5), c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5)
  )
)

n_per_group <- rep(100, n_groups)
range_dates <- date_to_int(
  c(
    as.Date("01/01/2014", "%d/%m/%Y"),
    as.Date("01/01/2015", "%d/%m/%Y")
  )
)
index_dates <- list(
  matrix(c(1, 2), nrow = 2),
  cbind(c(1, 2), c(1, 3)),
  cbind(
    c(1, 2), c(2, 3),c(1, 4)
  ),
  cbind(
    c(1, 2), c(2, 3), c(1, 4)
  )
)
index_dates_order <- list(
  matrix(c(1, 2), nrow=2),
  cbind(c(1, 2), c(1, 3)),
  cbind(c(1, 2), c(2, 3), c(1, 3), c(1, 4)),
  cbind(c(1, 2), c(2, 3), c(1, 3), c(1, 4))
)

out <- list(
  theta_baseline = theta_baseline,
  range_dates = range_dates,
  index_dates = index_dates,
  index_dates_order = index_dates_order
)

saveRDS(out, "sim_params.rds")
