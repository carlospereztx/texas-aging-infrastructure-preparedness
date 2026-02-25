# src/01_ingest_snf.R
# Ingest and standardize CMS SNF file; filter to TX; deduplicate by CCN.

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(stringr)
  library(here)
  library(readr)
  library(fs)
})

snf_file <- here("data", "raw", "CMS_SNF.csv")
stopifnot(file_exists(snf_file))

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit)) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit]])
}

snf_raw <- read_csv(
  snf_file,
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A", "NULL")
)

snf <- snf_raw %>%
  clean_names() %>%
  mutate(across(everything(), ~ str_squish(.x)))

snf_std <- snf %>%
  transmute(
    ccn = first_existing_col(
      snf,
      c(
        "cms_certification_number_ccn",
        "cms_certification_number",
        "cms_certification_number__ccn",
        "provider_ccn",
        "provider_id",
        "ccn"
      )
    ) %>%
      str_remove_all("\\D") %>%
      na_if(""),

    provider_name = first_existing_col(snf, c("provider_name", "facility_name", "name")),

    state = first_existing_col(snf, c("state", "provider_state", "state_code")) %>%
      str_to_upper(),

    county_raw = first_existing_col(snf, c("county_parish", "county", "county_name")),

    city = first_existing_col(snf, c("city", "city_town", "provider_city")),

    zip = first_existing_col(snf, c("zip_code", "zip", "provider_zip_code")) %>%
      str_extract("\\d{5}"),

    latitude  = first_existing_col(snf, c("latitude", "lat")),
    longitude = first_existing_col(snf, c("longitude", "lon", "lng"))
  ) %>%
  mutate(
    county_name = county_raw %>%
      str_to_lower() %>%
      str_replace_all("\\.", "") %>%
      str_replace_all("\\bcounty\\b", "") %>%
      str_replace_all("\\bcnty\\b", "") %>%
      str_squish() %>%
      str_to_title()
  )

snf_tx <- snf_std %>% filter(state == "TX")

qc_summary <- snf_tx %>%
  summarise(
    rows = n(),
    facilities_distinct_ccn = n_distinct(ccn, na.rm = TRUE),
    missing_ccn = sum(is.na(ccn) | ccn == ""),
    missing_county = sum(is.na(county_name) | county_name == ""),
    missing_zip = sum(is.na(zip) | zip == ""),
    .groups = "drop"
  )

qc_top_counties <- snf_tx %>%
  mutate(county_name = na_if(county_name, "")) %>%
  count(county_name, sort = TRUE) %>%
  slice_head(n = 25)

write_csv(qc_summary, here("reports", "tables", "qc_snf_ingest_summary.csv"), na = "")
write_csv(qc_top_counties, here("reports", "tables", "qc_snf_top_counties.csv"), na = "")

snf_tx_dedup <- snf_tx %>%
  arrange(ccn, provider_name) %>%
  distinct(ccn, .keep_all = TRUE)

out_rds <- here("data", "processed", "tx_snf_facility_clean.rds")
out_csv <- here("data", "processed", "tx_snf_facility_clean.csv")

saveRDS(snf_tx_dedup, out_rds)
write_csv(snf_tx_dedup, out_csv, na = "")

message("SNF cleaned: ", basename(out_rds), " / ", basename(out_csv))
message("QC tables: reports/tables/qc_snf_ingest_summary.csv, qc_snf_top_counties.csv")