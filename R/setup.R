###############################################################################
# setup.R
#
# Shared setup, sourced by 01_prepare_data.R and 02_estimate_models.R:
# packages, analysis window and project paths.
###############################################################################

required_packages <- c("tidyverse", "here", "fixest")

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg)
}
invisible(lapply(required_packages, library, character.only = TRUE))

options(scipen = 999, digits = 4)

# ---------------------------------------------------------------------------
# Analysis window
# Coefficients and baseline profiles are estimated on this window only.
#
# 2025-11-27 is deliberately EXCLUDED: on this last export day all 494 tariff
# households carry only high_tariff/low_tariff for the full 24 h and not a
# single project_tariff row -- an edge artefact of how signal_state was built.
# Through the any()-window logic in 02_estimate_models.R this one day would
# otherwise create HT windows in ALL 24 hours and spurious estimates for the
# night hours (e.g. hour 0: +0.094 kWh/15min, p = 0.003 -- from this day alone).
# ---------------------------------------------------------------------------
window_from <- as.Date("2025-08-09")
window_to   <- as.Date("2025-11-26")

restrict_window <- function(d) {
  d <- dplyr::filter(d, as.Date(date) >= window_from, as.Date(date) <= window_to)
  stopifnot(nrow(d) > 0)
  d
}

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
paths <- list(
  raw_data_nb2  = here::here("data", "raw", "data_simulator_nb2.rds"),
  raw_data_nb1 = here::here("data", "raw", "data_simulator_nb1.rds"),
  data_nb2      = here::here("data", "processed", "data_nb2.rds"),
  data_nb1     = here::here("data", "processed", "data_nb1.rds"),
  models       = here::here("data")
)

dir.create(here::here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(paths$models, showWarnings = FALSE, recursive = TRUE)
