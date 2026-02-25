# src/04_assign_geoid_hha.R
# Assign Texas county GEOIDs to HHAs using ZCTA→county (primary) with HUD USPS fallback.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(httr)
  library(purrr)
  library(fs)
})

fmt_zip5 <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[^0-9]", "")
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- ifelse(!is.na(x) & nchar(x) > 5, substr(x, 1, 5), x)
  x <- ifelse(!is.na(x), str_pad(x, 5, "left", "0"), NA_character_)
  x
}

hud_get_zip_county_for_zips <- function(zips, api_key, year = NULL, quarter = NULL) {
  zips <- unique(fmt_zip5(zips))
  zips <- zips[!is.na(zips)]
  if (length(zips) == 0) return(tibble())

  base_url <- "https://www.huduser.gov/hudapi/public/usps"

  fetch_one <- function(zip) {
    q <- list(type = 2, query = zip)
    if (!is.null(year)) q$year <- year
    if (!is.null(quarter)) q$quarter <- quarter

    resp <- httr::RETRY(
      verb = "GET",
      url = base_url,
      query = q,
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      times = 3,
      pause_base = 1,
      pause_cap = 8
    )

    if (httr::http_error(resp)) return(tibble())

    out <- httr::content(resp, as = "parsed", type = "application/json")
    results <- out$data$results
    if (is.null(results)) return(tibble())

    df <- if (is.data.frame(results)) {
      tibble::as_tibble(results, .name_repair = "unique")
    } else if (is.list(results)) {
      purrr::map_dfr(results, ~ tibble::as_tibble(.x, .name_repair = "unique"))
    } else {
      return(tibble())
    }

    nm <- names(df)
    zip_field <- intersect(nm, c("zip", "ZIP", "zipcode", "zip_code"))[1]
    geoid_field <- intersect(nm, c("geoid", "GEOID"))[1]
    if (is.na(zip_field) || is.na(geoid_field)) return(tibble())

    safe_num <- function(col) {
      if (col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else NA_real_
    }

    df %>%
      transmute(
        zip5 = fmt_zip5(.data[[zip_field]]),
        county_geoid = as.character(.data[[geoid_field]]),
        res_ratio = safe_num("res_ratio"),
        bus_ratio = safe_num("bus_ratio"),
        oth_ratio = safe_num("oth_ratio"),
        tot_ratio = safe_num("tot_ratio")
      ) %>%
      filter(!is.na(zip5), !is.na(county_geoid)) %>%
      filter(str_starts(county_geoid, "48"))
  }

  purrr::map_dfr(zips, fetch_one)
}

in_path <- here("data", "processed", "tx_hha_agency_clean.rds")
stopifnot(file_exists(in_path))

out_rds <- here("data", "processed", "tx_hha_agency_geocoded.rds")
out_csv <- here("data", "processed", "tx_hha_agency_geocoded.csv")

out_unmatched  <- here("reports", "tables", "qc_hha_unmatched_zips.csv")
out_qc_summary <- here("reports", "tables", "qc_hha_geoid_match_summary.csv")
out_qc_methods <- here("reports", "tables", "qc_hha_match_method_breakdown.csv")

out_xwalk_rds <- here("data", "processed", "tx_zcta_to_county_crosswalk_tx_primary.rds")
out_xwalk_csv <- here("data", "processed", "tx_zcta_to_county_crosswalk_tx_primary.csv")

out_hud_cache <- here("data", "processed", "tx_hud_zip_to_county_lookup_used.csv")

hha <- readRDS(in_path)

zip_col <- intersect(names(hha), c("zip", "zipcode", "zip_code", "postal_code", "ZIP", "Zip"))
if (length(zip_col) == 0) stop("No ZIP column found.")
zip_col <- zip_col[1]

hha <- hha %>%
  mutate(
    zip_raw = .data[[zip_col]],
    zip5 = fmt_zip5(.data[[zip_col]])
  )

if (all(c("state", "city", "address") %in% names(hha))) {
  hha <- hha %>%
    mutate(
      zip5 = case_when(
        state == "TX" &
          str_to_upper(city) == "TEXARKANA" &
          str_detect(str_to_upper(address), "3505\\s+SUMMERHILL") &
          zip5 == "75502" ~ "75503",
        TRUE ~ zip5
      )
    )
}

census_rel_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt"

zcta_county <- read_delim(
  file = census_rel_url,
  delim = "|",
  col_types = cols(
    GEOID_ZCTA5_20  = col_character(),
    GEOID_COUNTY_20 = col_character(),
    AREALAND_PART   = col_double(),
    AREAWATER_PART  = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    zcta5 = GEOID_ZCTA5_20,
    county_geoid = GEOID_COUNTY_20,
    arealand_part = AREALAND_PART,
    areawater_part = AREAWATER_PART
  ) %>%
  filter(str_starts(county_geoid, "48"))

zcta_land_totals <- zcta_county %>%
  group_by(zcta5) %>%
  summarize(arealand_total_tx = sum(arealand_part, na.rm = TRUE), .groups = "drop")

zcta_primary_county <- zcta_county %>%
  left_join(zcta_land_totals, by = "zcta5") %>%
  group_by(zcta5) %>%
  arrange(desc(arealand_part), desc(areawater_part), county_geoid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    land_share_tx = if_else(arealand_total_tx > 0, arealand_part / arealand_total_tx, NA_real_)
  ) %>%
  select(zcta5, county_geoid, land_share_tx)

zcta_multi <- zcta_county %>%
  count(zcta5, name = "n_counties_tx") %>%
  mutate(is_multicounty_tx = n_counties_tx > 1)

saveRDS(zcta_primary_county, out_xwalk_rds)
write_csv(zcta_primary_county, out_xwalk_csv, na = "")

hha_geoid <- hha %>%
  left_join(zcta_primary_county, by = c("zip5" = "zcta5")) %>%
  left_join(zcta_multi, by = c("zip5" = "zcta5")) %>%
  mutate(
    is_multicounty_tx = if_else(is.na(is_multicounty_tx), FALSE, is_multicounty_tx),
    match_method = if_else(!is.na(county_geoid), "census_zcta", NA_character_)
  )

hud_key <- Sys.getenv("HUD_USER_API_KEY", unset = NA_character_)

still_unmatched_zips <- hha_geoid %>%
  filter(!is.na(zip5) & is.na(county_geoid)) %>%
  distinct(zip5) %>%
  pull(zip5)

if (length(still_unmatched_zips) > 0 && file_exists(out_hud_cache)) {
  hud_cache <- read_csv(out_hud_cache, col_types = cols(.default = col_character())) %>%
    transmute(
      zip5 = fmt_zip5(zip5),
      county_geoid_hud = as.character(county_geoid)
    )

  hha_geoid <- hha_geoid %>%
    left_join(hud_cache, by = "zip5") %>%
    mutate(
      county_geoid = if_else(is.na(county_geoid) & !is.na(county_geoid_hud), county_geoid_hud, county_geoid),
      match_method = if_else(is.na(match_method) & !is.na(county_geoid_hud), "hud_usps_cached", match_method)
    ) %>%
    select(-county_geoid_hud)

  still_unmatched_zips <- hha_geoid %>%
    filter(!is.na(zip5) & is.na(county_geoid)) %>%
    distinct(zip5) %>%
    pull(zip5)
}

if (length(still_unmatched_zips) > 0 && !is.na(hud_key) && nzchar(hud_key)) {
  hud_rows <- hud_get_zip_county_for_zips(still_unmatched_zips, api_key = hud_key)

  if (nrow(hud_rows) > 0) {
    hud_primary_county <- hud_rows %>%
      group_by(zip5) %>%
      arrange(desc(tot_ratio), desc(res_ratio), county_geoid) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(
        zip5,
        county_geoid,
        hud_tot_ratio = as.character(tot_ratio),
        match_source = "hud_usps_api",
        retrieval_date = as.character(Sys.Date())
      )

    write_csv(hud_primary_county, out_hud_cache, na = "")

    hha_geoid <- hha_geoid %>%
      left_join(hud_primary_county %>% select(zip5, county_geoid_hud = county_geoid), by = "zip5") %>%
      mutate(
        county_geoid = if_else(is.na(county_geoid) & !is.na(county_geoid_hud), county_geoid_hud, county_geoid),
        match_method = if_else(is.na(match_method) & !is.na(county_geoid_hud), "hud_usps", match_method)
      ) %>%
      select(-county_geoid_hud)
  }
}

has_zip <- sum(!is.na(hha_geoid$zip5))
matched <- sum(!is.na(hha_geoid$county_geoid))
unmatched <- sum(!is.na(hha_geoid$zip5) & is.na(hha_geoid$county_geoid))
missing_zip <- sum(is.na(hha_geoid$zip5))

qc_summary <- tibble(
  total_rows = nrow(hha_geoid),
  rows_with_zip5 = has_zip,
  matched_to_county_geoid = matched,
  unmatched_zip_rows = unmatched,
  missing_or_invalid_zip5_rows = missing_zip,
  match_rate_among_rows_with_zip5 = ifelse(has_zip > 0, round(100 * matched / has_zip, 2), NA_real_)
)

method_counts <- hha_geoid %>%
  mutate(match_method = if_else(is.na(match_method) & is.na(county_geoid), "unmatched", match_method)) %>%
  count(match_method, sort = TRUE)

unmatched_zips <- hha_geoid %>%
  filter(!is.na(zip5) & is.na(county_geoid)) %>%
  count(zip5, sort = TRUE, name = "n_agencies") %>%
  rename(zip = zip5)

write_csv(qc_summary, out_qc_summary, na = "")
write_csv(method_counts, out_qc_methods, na = "")
write_csv(unmatched_zips, out_unmatched, na = "")

saveRDS(hha_geoid, out_rds)
write_csv(hha_geoid, out_csv, na = "")

message("HHA GEOID assigned: ", basename(out_rds), " / ", basename(out_csv))
message("QC tables: qc_hha_unmatched_zips.csv, qc_hha_geoid_match_summary.csv, qc_hha_match_method_breakdown.csv")
message("Crosswalk: ", basename(out_xwalk_rds), " / ", basename(out_xwalk_csv))
if (file_exists(out_hud_cache)) message("HUD cache: ", basename(out_hud_cache))