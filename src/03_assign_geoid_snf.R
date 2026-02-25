# src/03_assign_geoid_snf.R
# Assign Texas county GEOID to SNF facilities via county name normalization + crosswalk.

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(here)
  library(fs)
  library(tigris)
  library(sf)
  library(readr)
})

snf_in <- here("data", "processed", "tx_snf_facility_clean.rds")
stopifnot(file_exists(snf_in))
snf <- readRDS(snf_in)

normalize_county <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("\\bcounty\\b", "") %>%
    str_replace_all("\\bcnty\\b", "") %>%
    str_squish() %>%
    str_to_title() %>%
    na_if("")
}

county_fix_map <- tibble::tibble(
  from = c("Mc Lennan", "De Witt", "Mc Culloch"),
  to   = c("McLennan", "DeWitt", "McCulloch")
) %>%
  mutate(
    from = normalize_county(from),
    to   = normalize_county(to)
  ) %>%
  filter(!is.na(from), !is.na(to))

tx_counties <- tigris::counties(state = "TX", cb = TRUE, year = 2022) %>%
  st_drop_geometry() %>%
  transmute(
    geoid = GEOID,
    county_name_official = NAME,
    county_name = normalize_county(NAME)
  )

crosswalk_rds <- here("data", "processed", "tx_county_crosswalk.rds")
crosswalk_csv <- here("data", "processed", "tx_county_crosswalk.csv")
saveRDS(tx_counties, crosswalk_rds)
write_csv(tx_counties, crosswalk_csv, na = "")

snf2 <- snf %>%
  mutate(county_name = normalize_county(county_name)) %>%
  left_join(county_fix_map, by = c("county_name" = "from")) %>%
  mutate(county_name = coalesce(to, county_name)) %>%
  select(-to)

snf_geocoded <- snf2 %>%
  left_join(
    tx_counties %>% select(geoid, county_name_official, county_name),
    by = "county_name"
  )

qc_unmatched <- snf_geocoded %>%
  filter(is.na(geoid) | geoid == "") %>%
  count(county_name, sort = TRUE)

qc_summary <- snf_geocoded %>%
  summarise(
    rows = n(),
    facilities_distinct_ccn = n_distinct(ccn, na.rm = TRUE),
    matched_geoid_rows = sum(!is.na(geoid) & geoid != ""),
    matched_geoid_pct = round(100 * matched_geoid_rows / n(), 2),
    unmatched_rows = sum(is.na(geoid) | geoid == ""),
    .groups = "drop"
  )

write_csv(qc_unmatched, here("reports", "tables", "qc_snf_unmatched_county_names.csv"), na = "")
write_csv(qc_summary, here("reports", "tables", "qc_snf_geoid_match_summary.csv"), na = "")

out_rds <- here("data", "processed", "tx_snf_facility_geocoded.rds")
out_csv <- here("data", "processed", "tx_snf_facility_geocoded.csv")
saveRDS(snf_geocoded, out_rds)
write_csv(snf_geocoded, out_csv, na = "")

message("SNF GEOID assigned: ", basename(out_rds), " / ", basename(out_csv))
message("QC tables: reports/tables/qc_snf_unmatched_county_names.csv, qc_snf_geoid_match_summary.csv")
message("Crosswalk: ", basename(crosswalk_rds), " / ", basename(crosswalk_csv))