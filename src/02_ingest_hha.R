# src/02_ingest_hha.R
# Ingest and standardize CMS HHA provider file; filter to TX; deduplicate by CCN.

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(stringr)
  library(readr)
  library(here)
  library(fs)
})

hha_file <- here("data", "raw", "CMS_HH_Provider.csv")
stopifnot(file_exists(hha_file))

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit)) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit]])
}

hha_raw <- read_csv(
  hha_file,
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A", "NULL")
)

hha <- hha_raw %>%
  clean_names() %>%
  mutate(across(everything(), ~ str_squish(.x)))

hha_std <- hha %>%
  transmute(
    ccn = first_existing_col(
      hha,
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

    provider_name = first_existing_col(hha, c("provider_name", "facility_name", "name")),

    state = first_existing_col(hha, c("state", "provider_state", "state_code")) %>%
      str_to_upper(),

    address = first_existing_col(
      hha,
      c("address", "street_address", "provider_address", "address_line_1")
    ),

    city = first_existing_col(hha, c("city", "city_town", "provider_city", "city_name")),

    zip = first_existing_col(hha, c("zip_code", "zip", "provider_zip_code", "zip5")) %>%
      str_extract("\\d{5}"),

    phone = first_existing_col(
      hha,
      c("phone_number", "phone", "provider_phone", "telephone_number")
    ) %>%
      str_remove_all("[^0-9]")
  )

hha_tx <- hha_std %>% filter(state == "TX")

qc_summary <- hha_tx %>%
  summarise(
    rows = n(),
    agencies_distinct_ccn = n_distinct(ccn, na.rm = TRUE),
    missing_ccn = sum(is.na(ccn) | ccn == ""),
    missing_zip = sum(is.na(zip) | zip == ""),
    missing_city = sum(is.na(city) | city == ""),
    .groups = "drop"
  )

qc_top_zips <- hha_tx %>%
  mutate(zip = na_if(zip, "")) %>%
  count(zip, sort = TRUE) %>%
  slice_head(n = 25)

write_csv(qc_summary, here("reports", "tables", "qc_hha_ingest_summary.csv"), na = "")
write_csv(qc_top_zips, here("reports", "tables", "qc_hha_top_zips.csv"), na = "")

hha_tx_dedup <- hha_tx %>%
  arrange(ccn, provider_name) %>%
  distinct(ccn, .keep_all = TRUE)

out_rds <- here("data", "processed", "tx_hha_agency_clean.rds")
out_csv <- here("data", "processed", "tx_hha_agency_clean.csv")

saveRDS(hha_tx_dedup, out_rds)
write_csv(hha_tx_dedup, out_csv, na = "")

message("HHA cleaned: ", basename(out_rds), " / ", basename(out_csv))
message("QC tables: reports/tables/qc_hha_ingest_summary.csv, qc_hha_top_zips.csv")