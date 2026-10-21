###############################################################################
# 01_prepare_data.R
#
# Loads the raw field-trial data from data/raw/, drops flagged rows, translates
# the German survey categories and writes the analysis-ready datasets to
# data/processed/. Run before 02_estimate_models.R.
###############################################################################

message("Running 01_prepare_data.R ...")
source(here::here("R", "setup.R"))

data_nb2  <- readRDS(paths$raw_data_nb2)  |> filter(drop != 1)
data_nb1 <- readRDS(paths$raw_data_nb1) |> filter(drop != 1)

stopifnot(nrow(data_nb2)  > 0)
stopifnot(nrow(data_nb1) > 0)

translate_categories <- function(df) {
  df |>
    mutate(
      household_size = as.integer(readr::parse_number(as.character(household_size_raw))),
      has_pv_raw = dplyr::recode(has_pv_raw,
        "Ja, mit Speicher"  = "PV + Storage",
        "Ja, ohne Speicher" = "PV only",
        "Nein"              = "No PV"
      ),
      heating_system_type = dplyr::recode(heating_system_type,
        "Biomassekessel"      = "Biomass boiler",
        "Elektrische Heizung" = "Electric heating",
        "Fern-/Nahwärme"      = "District heating",
        "Gasheizung/Heizöl"   = "Gas/oil heating",
        "Wärmepumpe"          = "Heat pump",
        "Sonstiges"           = "Other"
      ),
      across(
        c(has_sauna_raw, has_dryer_raw, has_ac_raw, has_pool_raw, has_e_motorcycle),
        ~ as.integer(. == "Ja")
      )
    )
}

data_nb2  <- translate_categories(data_nb2)
data_nb1 <- translate_categories(data_nb1)

saveRDS(data_nb2,  paths$data_nb2)
saveRDS(data_nb1, paths$data_nb1)

message("01_prepare_data.R completed.")
