###############################################################################
# 02_estimate_models.R
#
# Estimate hour x signal interaction models by subgroup (EV, PV type, HP).
# Output: tidy coefficient tables + baseline load profiles -> data/
# These are the only inputs the Shiny simulator needs.
# Requires data/processed/ from 01_prepare_data.R.
###############################################################################

message("Running 02_estimate_models.R ...")
source(here::here("R", "setup.R"))

data_nb2  <- readRDS(paths$data_nb2)
data_nb1 <- readRDS(paths$data_nb1)

# ---------------------------------------------------------------------------
# Restrict to the analysis window (window_from..window_to, defined in
# setup.R). Applied to the source frames so analysis datasets AND baseline
# inherit it.
# Per default: 9. August to 26. November 2025. This is the window where households
# in NB 1 received signals at the same time.
# ---------------------------------------------------------------------------
data_nb2  <- restrict_window(data_nb2)
data_nb1 <- restrict_window(data_nb1)

message("Analysis window ", window_from, " to ", window_to,
        " | NB 1 rows: ", nrow(data_nb1), " | NB 2 rows: ", nrow(data_nb2))

dir_models <- paths$models


# ---------------------------------------------------------------------------
# 1. Coefficient extractor
#    Pulls hour-level estimates for one dummy variable out of a fixest model.
#    Coefficient names from i(hour, dummy) look like "hour::11:af_treat".
# ---------------------------------------------------------------------------

extract_hour_coefs <- function(model, dummy_var, signal_label,
                               subgroup_dim, subgroup_val) {
  ct    <- coeftable(model)
  keep  <- grepl(dummy_var, rownames(ct), fixed = TRUE)
  if (!any(keep)) return(NULL)
  ct    <- as.data.frame(ct[keep, , drop = FALSE])
  ct$hour <- as.integer(sub("^.*::(\\d+):.*$", "\\1", rownames(ct)))
  ct |>
    transmute(
      subgroup_dim = subgroup_dim,
      subgroup_val = subgroup_val,
      signal_type  = signal_label,
      hour         = hour,
      estimate     = Estimate,
      std_error    = `Std. Error`,
      t_value      = `t value`,
      p_value      = `Pr(>|t|)`,
      conf_low     = Estimate - 1.96 * `Std. Error`,
      conf_high    = Estimate + 1.96 * `Std. Error`
    )
}

# ---------------------------------------------------------------------------
# 2. Hour x signal models (DiD against the control group)
#
#     Spec:  cons ~ i(hour, sig_treat) + i(hour, sig_win) | survey_key + date
#       sig_win   = timestamp lies in a signal window (common to treated AND
#                   control) -> absorbs the endogenous demand component (high
#                    demand in evening hours).
#       sig_treat = the household's OWN signal at that timestamp -> DiD effect
#                   per hour. NOT treated x sig_win: the windows are staggered
#                   across households (see build_did_data), so at a given
#                   timestamp only a fraction of the tariff group is actually
#                   signalled. Broadcasting the window onto all of them would
#                   label unsignalled households as treated and attenuate the
#                   estimate towards zero -- by hour, and unevenly, so the
#                   hourly profile itself would be distorted.
# ---------------------------------------------------------------------------

message("Estimating hour x signal models ...")

# Signal windows are NOT a uniform broadcast: they are staggered across
# households by design (so the tariff group does not all react at once and
# build a new peak). Example 2025-09-17, per full hour: 174 / 373 / 294 / 223
# of 494 tariff households in NT from 11:00 to 14:45 -- constant within the
# hour, changing between hours.
#
# ht_win/nt_win therefore mean "a window existed at this timestamp", NOT "every
# tariff household was in it". They are collapsed to one row per timestamp and
# mapped onto ALL households (treated + control), because the model needs a
# window main effect that is common to both groups to absorb the endogenous
# demand; controls carry no signal_state, so it must come from the treated
# calendar. The per-household treatment is taken from signal_state instead.
build_did_data <- function(d, win) {
  win_cols <- setdiff(names(win), "time")
  d |>
    # Treated rows with no_signal = unknown treatment state. 
    # Drop them so treated only contributes rows with a known signal. 
    # Controls (treated==0, always no_signal) are KEPT as the
    # counterfactual. 
    filter(post == 1, !(treated == 1 & signal_state == "no_signal")) |>
    left_join(win, by = "time") |>
    mutate(
      across(all_of(win_cols), ~ coalesce(.x, 0L)),
      hp = as.integer(heating_system_type == "Heat pump")
    ) |>
    # Keep only model + subgroup columns; otherwise blows up memory.
    # signal_state is kept because the treatment dummies are read from it.
    select(survey_key, date, hour, cons_kwh_15m, treated, signal_state,
           has_ev, has_pv_raw, hp, all_of(win_cols))
}

win_nb2 <- data_nb2 |>
  filter(treated == 1, signal_state != "no_signal") |>
  group_by(time) |>
  summarise(af_win = as.integer(any(signal_state == "exception_window")),
            .groups = "drop")

data_nb2_did <- build_did_data(data_nb2, win_nb2) |>
  # Own signal, not treated x window (see section header). Controls are always
  # "no_signal" and therefore 0, so the DiD structure is unchanged.
  mutate(af_treat = as.integer(signal_state == "exception_window"))

win_nb1 <- data_nb1 |>
  filter(treated == 1, signal_state != "no_signal") |>
  group_by(time) |>
  summarise(ht_win = as.integer(any(signal_state == "high_tariff")),
            nt_win = as.integer(any(signal_state == "low_tariff")),
            .groups = "drop")

data_nb1_did <- build_did_data(data_nb1, win_nb1) |>
  # Own signal, not treated x window (see section header). Controls are always
  # "no_signal" and therefore 0, so the DiD structure is unchanged.
  mutate(ht_treat = as.integer(signal_state == "high_tariff"),
         nt_treat = as.integer(signal_state == "low_tariff"))

run_nb2_did <- function(data, subgroup_dim, subgroup_val) {
  m <- feols(
    cons_kwh_15m ~ i(hour, af_treat) + i(hour, af_win) | survey_key + date,
    data = data, cluster = ~survey_key, warn = TRUE, notes = TRUE
  )
  extract_hour_coefs(m, "af_treat", "exception_window", subgroup_dim, subgroup_val)
}

run_nb1_did <- function(data, subgroup_dim, subgroup_val) {
  m <- feols(
    cons_kwh_15m ~ i(hour, ht_treat) + i(hour, nt_treat) +
                   i(hour, ht_win)   + i(hour, nt_win) | survey_key + date,
    data = data, cluster = ~survey_key, warn = TRUE, notes = TRUE
  )
  bind_rows(
    extract_hour_coefs(m, "ht_treat", "high_tariff", subgroup_dim, subgroup_val),
    extract_hour_coefs(m, "nt_treat", "low_tariff",  subgroup_dim, subgroup_val)
  )
}

coefs_nb2_did <- bind_rows(
  run_nb2_did(data_nb2_did,                                       "all", "All"),
  run_nb2_did(filter(data_nb2_did, has_ev == 1),                  "ev",  "EV"),
  run_nb2_did(filter(data_nb2_did, has_ev == 0),                  "ev",  "No EV"),
  run_nb2_did(filter(data_nb2_did, has_pv_raw == "No PV"),        "pv",  "No PV"),
  run_nb2_did(filter(data_nb2_did, has_pv_raw == "PV only"),      "pv",  "PV only"),
  run_nb2_did(filter(data_nb2_did, has_pv_raw == "PV + Storage"), "pv",  "PV + Storage"),
  run_nb2_did(filter(data_nb2_did, hp == 1),                      "hp",  "Heat Pump"),
  run_nb2_did(filter(data_nb2_did, hp == 0),                      "hp",  "No Heat Pump")
)
saveRDS(coefs_nb2_did, file.path(dir_models, "coefs_nb2_did.rds"))

coefs_nb1_did <- bind_rows(
  run_nb1_did(data_nb1_did,                                       "all", "All"),
  run_nb1_did(filter(data_nb1_did, has_ev == 1),                  "ev",  "EV"),
  run_nb1_did(filter(data_nb1_did, has_ev == 0),                  "ev",  "No EV"),
  run_nb1_did(filter(data_nb1_did, has_pv_raw == "No PV"),        "pv",  "No PV"),
  run_nb1_did(filter(data_nb1_did, has_pv_raw == "PV only"),      "pv",  "PV only"),
  run_nb1_did(filter(data_nb1_did, has_pv_raw == "PV + Storage"), "pv",  "PV + Storage"),
  run_nb1_did(filter(data_nb1_did, hp == 1),                      "hp",  "Heat Pump"),
  run_nb1_did(filter(data_nb1_did, hp == 0),                      "hp",  "No Heat Pump")
)
saveRDS(coefs_nb1_did, file.path(dir_models, "coefs_nb1_did.rds"))

# ---------------------------------------------------------------------------
# 3. Baseline load profiles
#    Source: control group, post-period -> counterfactual "without tariff"
#    Aggregated to mean kWh/15min by hour for each subgroup dimension.
# ---------------------------------------------------------------------------

message("Computing baseline load profiles ...")

baseline_by_group <- function(data, region) {
  base <- data |>
    filter(treated == 0, post == 1) |>
    mutate(
      hp         = as.integer(heating_system_type == "Heat pump"),
      is_weekend = lubridate::wday(time, week_start = 1) >= 6
    )

  agg_one_day <- function(d, day_type_val) {
    bind_rows(
      d |>
        group_by(hour) |>
        summarise(mean_kwh = mean(cons_kwh_15m, na.rm = TRUE), .groups = "drop") |>
        mutate(subgroup_dim = "all", subgroup_val = "All"),

      d |>
        group_by(hour, has_ev) |>
        summarise(mean_kwh = mean(cons_kwh_15m, na.rm = TRUE), .groups = "drop") |>
        mutate(subgroup_dim = "ev",
               subgroup_val = ifelse(has_ev == 1, "EV", "No EV")) |>
        select(-has_ev),

      d |>
        group_by(hour, has_pv_raw) |>
        summarise(mean_kwh = mean(cons_kwh_15m, na.rm = TRUE), .groups = "drop") |>
        rename(subgroup_val = has_pv_raw) |>
        mutate(subgroup_dim = "pv"),

      d |>
        group_by(hour, hp) |>
        summarise(mean_kwh = mean(cons_kwh_15m, na.rm = TRUE), .groups = "drop") |>
        mutate(subgroup_dim = "hp",
               subgroup_val = ifelse(hp == 1, "Heat Pump", "No Heat Pump")) |>
        select(-hp)
    ) |>
      mutate(region = region, day_type = day_type_val)
  }

  bind_rows(
    agg_one_day(base,                      "Gesamt"),
    agg_one_day(filter(base, !is_weekend), "Werktag"),
    agg_one_day(filter(base,  is_weekend), "Wochenende")
  )
}

baseline <- bind_rows(
  baseline_by_group(data_nb2,  "NB2"),
  baseline_by_group(data_nb1, "NB1")
)

saveRDS(baseline, file.path(dir_models, "baseline_profiles.rds"))
message("  Baseline done: ", nrow(baseline), " rows")

# ---------------------------------------------------------------------------
# 3b. Baseline load-duration data (15-min resolution)
#    Same source as the baseline profiles (control group, post period) but NOT
#    collapsed to the average day: the mean load across the subgroup's
#    households at each 15-min timestamp. The Shiny app adds the hourly tariff
#    effect to the slots in active hours and sorts descending -> a true load
#    duration curve whose peak reflects the actual peak timestamps instead of a
#    daily mean. Kept compact (no `time` column) since only the load values and
#    their hour are needed; the curve is re-sorted in the app anyway.
# ---------------------------------------------------------------------------

message("Computing baseline load-duration data (15-min) ...")

duration_by_group <- function(data, region) {
  base <- data |>
    filter(treated == 0, post == 1) |>
    mutate(
      hp         = as.integer(heating_system_type == "Heat pump"),
      is_weekend = lubridate::wday(time, week_start = 1) >= 6
    )

  # Mean load across the subgroup's households at each 15-min timestamp.
  # `hour` is a function of `time`, so grouping by both keeps the hour column
  # without changing the grouping.
  agg <- function(d, group_var = NULL) {
    g <- if (is.null(group_var)) c("time", "hour") else c("time", "hour", group_var)
    d |>
      group_by(across(all_of(g))) |>
      summarise(load_kwh = mean(cons_kwh_15m, na.rm = TRUE), .groups = "drop")
  }

  agg_one_day <- function(d, day_type_val) {
    bind_rows(
      agg(d) |>
        mutate(subgroup_dim = "all", subgroup_val = "All"),

      agg(d, "has_ev") |>
        mutate(subgroup_dim = "ev",
               subgroup_val = ifelse(has_ev == 1, "EV", "No EV")) |>
        select(-has_ev),

      agg(d, "has_pv_raw") |>
        rename(subgroup_val = has_pv_raw) |>
        mutate(subgroup_dim = "pv"),

      agg(d, "hp") |>
        mutate(subgroup_dim = "hp",
               subgroup_val = ifelse(hp == 1, "Heat Pump", "No Heat Pump")) |>
        select(-hp)
    ) |>
      mutate(region = region, day_type = day_type_val) |>
      select(region, subgroup_dim, subgroup_val, day_type, hour, load_kwh)
  }

  bind_rows(
    agg_one_day(base,                      "Gesamt"),
    agg_one_day(filter(base, !is_weekend), "Werktag"),
    agg_one_day(filter(base,  is_weekend), "Wochenende")
  )
}

baseline_duration <- bind_rows(
  duration_by_group(data_nb2,  "NB2"),
  duration_by_group(data_nb1, "NB1")
) |>
  # Sorted by value within each hour: the app only uses the distribution of
  # values per hour (Dauerlinie, peak, mean), so dropping the calendar order
  # changes no result but keeps the file from being readable as a time series.
  arrange(region, subgroup_dim, subgroup_val, day_type, hour, load_kwh)

saveRDS(baseline_duration, file.path(dir_models, "baseline_duration.rds"))
message("  Duration data done: ", nrow(baseline_duration), " rows")

# ---------------------------------------------------------------------------
# 3c. Dashboard metadata
#    Small descriptor so the Shiny app can state, in sync with the pipeline,
#    which data and which period the curves are based on (dates formatted here
#    to avoid locale surprises in the app). Household counts are over the
#    analysis window, per region.
#
#    n_by_group: subgroup labels used by the app for legend label mapping.
# ---------------------------------------------------------------------------

counts_by_group <- function(data, region) {
  base <- data |>
    filter(treated == 0, post == 1) |>
    mutate(hp = as.integer(heating_system_type == "Heat pump"))

  bind_rows(
    tibble(subgroup_dim = "all", subgroup_val = "All"),

    base |>
      distinct(has_ev) |>
      mutate(subgroup_dim = "ev",
             subgroup_val = ifelse(has_ev == 1, "EV", "No EV")) |>
      select(-has_ev),

    base |>
      distinct(has_pv_raw) |>
      rename(subgroup_val = has_pv_raw) |>
      mutate(subgroup_dim = "pv"),

    base |>
      distinct(hp) |>
      mutate(subgroup_dim = "hp",
             subgroup_val = ifelse(hp == 1, "Heat Pump", "No Heat Pump")) |>
      select(-hp)
  ) |>
    mutate(region = region) |>
    select(region, subgroup_dim, subgroup_val)
}

# Signal coverage per hour: on how many distinct days did this signal actually
# occur at this hour, and how many households were signalled on a typical such
# day? The windows are staggered (see section 2), so an hour can carry an
# estimate that rests on very few nights -- e.g. NB1 low_tariff at 01:00-03:00
# comes from 7 days only, yet yields the largest effect in the whole tool
# (+0.36 kW/household, driven by EV charging). The app uses this to disable
# hours whose estimate is too thinly identified to put in front of a user.
coverage_one <- function(data, region, state_val, label) {
  data |>
    filter(post == 1, treated == 1, signal_state == state_val) |>
    group_by(hour, date) |>
    summarise(n_hh = n_distinct(survey_key), .groups = "drop") |>
    group_by(hour) |>
    summarise(n_days    = n_distinct(date),
              median_hh = as.integer(median(n_hh)),
              .groups   = "drop") |>
    mutate(region = region, signal_type = label) |>
    select(region, signal_type, hour, n_days, median_hh)
}

signal_coverage <- bind_rows(
  coverage_one(data_nb2,  "NB2",  "exception_window",   "exception_window"),
  coverage_one(data_nb1, "NB1", "high_tariff", "high_tariff"),
  coverage_one(data_nb1, "NB1", "low_tariff",  "low_tariff")
)

message("Signal coverage per hour:")
print(as.data.frame(signal_coverage), row.names = FALSE)

sim_meta <- list(
  period_from = format(window_from, "%d.%m.%Y"),
  period_to   = format(window_to,   "%d.%m.%Y"),
  coverage    = signal_coverage,
  n_households = c(
    NB2  = dplyr::n_distinct(data_nb2$survey_key),
    NB1 = dplyr::n_distinct(data_nb1$survey_key)
  ),
  n_by_group = bind_rows(
    counts_by_group(data_nb2,  "NB2"),
    counts_by_group(data_nb1, "NB1")
  )
)
saveRDS(sim_meta, file.path(dir_models, "sim_meta.rds"))

message("02_estimate_models.R completed successfully.")
message("Outputs: ", dir_models)


