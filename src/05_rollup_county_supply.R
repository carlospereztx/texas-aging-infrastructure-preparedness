# src/05_rollup_county_supply.R
# Roll up HHA + SNF supply to Texas county level.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(tibble)
})

hha_path <- here("data", "processed", "tx_hha_agency_geocoded.rds")
snf_path <- here("data", "processed", "tx_snf_facility_geocoded.rds")

if (!file.exists(hha_path)) stop("Missing input: ", hha_path, "\nRun src/04_assign_geoid_hha.R first.")
if (!file.exists(snf_path)) stop("Missing input: ", snf_path, "\nRun src/03_assign_geoid_snf.R first.")

hha <- readRDS(hha_path)
snf <- readRDS(snf_path)

if (!("county_geoid" %in% names(hha))) stop("HHA input missing required column: county_geoid")

if (!("county_geoid" %in% names(snf))) {
  if ("geoid" %in% names(snf)) {
    snf <- snf %>% mutate(county_geoid = as.character(geoid))
  } else {
    stop("SNF input missing county GEOID column (expected county_geoid or geoid).")
  }
}

hha_missing_geoid <- sum(is.na(hha$county_geoid) | hha$county_geoid == "")
snf_missing_geoid <- sum(is.na(snf$county_geoid) | snf$county_geoid == "")

hha_ok <- hha %>% filter(!is.na(county_geoid) & county_geoid != "")
snf_ok <- snf %>% filter(!is.na(county_geoid) & county_geoid != "")

hha_county <- hha_ok %>%
  group_by(county_geoid) %>%
  summarize(hha_n = n_distinct(ccn, na.rm = TRUE), .groups = "drop")

snf_county <- snf_ok %>%
  group_by(county_geoid) %>%
  summarize(snf_n = n_distinct(ccn, na.rm = TRUE), .groups = "drop")

tx_county_supply <- full_join(hha_county, snf_county, by = "county_geoid") %>%
  mutate(
    hha_n = coalesce(hha_n, 0L),
    snf_n = coalesce(snf_n, 0L),
    providers_total = hha_n + snf_n
  ) %>%
  arrange(desc(providers_total), county_geoid)

qc_summary <- tibble(
  hha_rows_in = nrow(hha),
  snf_rows_in = nrow(snf),
  hha_missing_county_geoid_rows = hha_missing_geoid,
  snf_missing_county_geoid_rows = snf_missing_geoid,
  counties_with_any_provider = nrow(tx_county_supply),
  total_hha_counted = sum(tx_county_supply$hha_n, na.rm = TRUE),
  total_snf_counted = sum(tx_county_supply$snf_n, na.rm = TRUE),
  total_providers_counted = sum(tx_county_supply$providers_total, na.rm = TRUE)
)

qc_top10 <- tx_county_supply %>%
  select(county_geoid, hha_n, snf_n, providers_total) %>%
  slice_head(n = 10)

write_csv(qc_summary, here("reports", "tables", "qc_county_supply_rollup_summary.csv"), na = "")
write_csv(qc_top10, here("reports", "tables", "qc_county_supply_top10.csv"), na = "")

out_rds <- here("data", "processed", "tx_county_supply.rds")
out_csv <- here("data", "processed", "tx_county_supply.csv")
saveRDS(tx_county_supply, out_rds)
write_csv(tx_county_supply, out_csv, na = "")

message("County supply saved: ", basename(out_rds), " / ", basename(out_csv))
message("QC tables: qc_county_supply_rollup_summary.csv, qc_county_supply_top10.csv")