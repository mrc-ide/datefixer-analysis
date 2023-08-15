library(orderly2)
orderly_artefact("Figures", c("aug_data_baseline.png"))
orderly_dependency(
  "sim_data_baseline",
  "latest",
  c(aug_data_baseline.rds = "aug_data_baseline.rds")
)
library(MixDiff)
library(ggplot2)
library(purrr)
library(tidyr)
augmented <- readRDS("aug_data_baseline.rds")
delays <- lapply(
  augmented$D, function(x) {
    cols <- seq(from = 2, to = ncol(x))
    out <- matrix(
      data = NA, nrow = nrow(x), ncol = length(cols)
    )
    for (col in cols) {
      out[, col - 1] <- int_to_date(x[ , col]) - int_to_date(x[ , col - 1])
    }
    out
  }
)

## To plot
delays_df <- imap_dfr(
  delays, function(x, index) {
    x <- as.data.frame(x)
    x$group <- paste0("G", index)
    pivot_longer(x, starts_with("V"))
  }
)

p <- ggplot(
  delays_df, aes(value)
) + geom_bar() +
  facet_grid(group~name)

ggsave("aug_data_baseline.png", p)



