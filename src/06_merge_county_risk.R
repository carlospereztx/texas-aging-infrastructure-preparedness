# src/06_merge_county_risk.R
# Merge county supply with ACS DP05 demographics (TX counties).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidycensus)
})

in_supply <- here("data", "processed", "tx_county_supply.rds")
if (!file.exists(in_supply)) stop("Missing input: ", in_supply, "\nRun src/05_rollup_county_supply.R first.")

out_rds <- here("data", "processed", "tx_county_supply_risk_merged.rds")
out_csv <- here("data", "processed", "tx_county_supply_risk_merged.csv")

out_missing <- here("reports", "tables", "qc_counties_missing_acs.csv")
out_qc_cov  <- here("reports", "tables", "qc_merge_coverage_summary.csv")

tx_county_supply <- readRDS(in_supply)
if (!("county_geoid" %in% names(tx_county_supply))) stop("tx_county_supply missing required column: county_geoid")

tx_counties <- tidycensus::fips_codes %>%
  filter(state_code == "48") %>%
  transmute(
    county_geoid = paste0(state_code, county_code),
    county_name = county
  ) %>%
  distinct()

acs_year <- 2023
vars <- c("DP05_0001E", "DP05_0024E")
acs_cache_rds <- here("data", "processed", sprintf("acs_dp05_tx_county_%s_acs5_wide.rds", acs_year))

has_key <- nzchar(Sys.getenv("CENSUS_API_KEY")) ||
  (tryCatch(tidycensus::has_census_key(), error = function(e) FALSE))

if (!file.exists(acs_cache_rds)) {
  if (!has_key) {
    message("No Census API key detected; tidycensus may be rate-limited.")
  }

  acs_tx_raw <- tidycensus::get_acs(
    geography = "county",
    variables = vars,
    state = "TX",
    year = acs_year,
    survey = "acs5",
    output = "wide"
  )
  saveRDS(acs_tx_raw, acs_cache_rds)
} else {
  acs_tx_raw <- readRDS(acs_cache_rds)
}

nm <- names(acs_tx_raw)
has_raw_codes <- all(c("DP05_0001E", "DP05_0024E") %in% nm)
has_aliases <- all(c("total_popE", "pop_65pE") %in% nm)

acs_tx <- if (has_raw_codes) {
  acs_tx_raw %>%
    transmute(
      county_geoid = GEOID,
      total_pop = DP05_0001E,
      pop_65p   = DP05_0024E
    )
} else if (has_aliases) {
  acs_tx_raw %>%
    transmute(
      county_geoid = GEOID,
      total_pop = total_popE,
      pop_65p   = pop_65pE
    )
} else {
  stop("Unexpected ACS wide output columns:\n- ", paste(nm, collapse = "\n- "))
}

acs_tx <- acs_tx %>%
  mutate(
    total_pop = suppressWarnings(as.numeric(total_pop)),
    pop_65p   = suppressWarnings(as.numeric(pop_65p))
  )

tx_county_supply_risk_merged <- tx_counties %>%
  left_join(acs_tx, by = "county_geoid") %>%
  left_join(tx_county_supply, by = "county_geoid") %>%
  mutate(
    hha_n = coalesce(hha_n, 0L),
    snf_n = coalesce(snf_n, 0L),
    providers_total = coalesce(providers_total, 0L),
    providers_per_10k_65p = if_else(pop_65p > 0, providers_total / pop_65p * 10000, NA_real_),
    hha_per_10k_65p       = if_else(pop_65p > 0, hha_n / pop_65p * 10000, NA_real_),
    snf_per_10k_65p       = if_else(pop_65p > 0, snf_n / pop_65p * 10000, NA_real_)
  )

missing_acs <- tx_county_supply_risk_merged %>%
  filter(is.na(total_pop) | is.na(pop_65p)) %>%
  select(county_geoid, county_name, total_pop, pop_65p, hha_n, snf_n, providers_total)

qc_coverage <- tibble(
  rows = nrow(tx_county_supply_risk_merged),
  expected_rows_tx_counties = 254,
  counties_with_any_providers = sum(tx_county_supply_risk_merged$providers_total > 0, na.rm = TRUE),
  counties_with_zero_providers = sum(tx_county_supply_risk_merged$providers_total == 0, na.rm = TRUE),
  counties_missing_acs = nrow(missing_acs)
)

write_csv(missing_acs, out_missing, na = "")
write_csv(qc_coverage, out_qc_cov, na = "")

saveRDS(tx_county_supply_risk_merged, out_rds)
write_csv(tx_county_supply_risk_merged, out_csv, na = "")

message("Supply + ACS merged: ", basename(out_rds), " / ", basename(out_csv))
message("QC tables: qc_counties_missing_acs.csv, qc_merge_coverage_summary.csv")