# src/07_prepare_preparedness_metrics.R
# Create preparedness metrics and generate final figures/tables.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(here)
  library(ggplot2)
  library(sf)
  library(tigris)
  library(scales)
})

options(tigris_use_cache = TRUE)

in_path <- here("data", "processed", "tx_county_supply_risk_merged.rds")
stopifnot(file.exists(in_path))

out_rds <- here("data", "processed", "tx_county_preparedness_metrics.rds")
out_csv <- here("data", "processed", "tx_county_preparedness_metrics.csv")

out_map4  <- here("reports", "figures", "fig_tx_preparedness_4cat_map.png")
out_scatt <- here("reports", "figures", "fig_supply_vs_aging_scatter.png")
out_table <- here("reports", "tables",  "table_top_bottom_counties.csv")

out_qc_thr     <- here("reports", "tables", "qc_preparedness_thresholds.csv")
out_qc_counts  <- here("reports", "tables", "qc_preparedness_counts.csv")
out_quartiles  <- here("reports", "tables", "table_preparedness_quartiles.csv")
out_zero_table <- here("reports", "tables", "table_zero_provider_eligible_counties.csv")

df <- readRDS(in_path) %>%
  mutate(
    county_geoid = str_pad(as.character(county_geoid), 5, side = "left", pad = "0"),
    total_pop = suppressWarnings(as.numeric(total_pop)),
    pop_65p = suppressWarnings(as.numeric(pop_65p)),
    providers_total = suppressWarnings(as.numeric(providers_total)),
    providers_per_10k_65p = suppressWarnings(as.numeric(providers_per_10k_65p))
  )

min_65p <- 1000

df <- df %>%
  mutate(
    aging_share = if_else(total_pop > 0, pop_65p / total_pop, NA_real_),
    eligible_for_rate = !is.na(pop_65p) & pop_65p >= min_65p,
    zero_provider_eligible = eligible_for_rate & (providers_total == 0)
  )

q <- quantile(
  df$providers_per_10k_65p[df$eligible_for_rate],
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

df <- df %>%
  mutate(
    provider_density_quartile = case_when(
      !eligible_for_rate ~ NA_character_,
      providers_per_10k_65p <= q[[1]] ~ "Q1 (Lowest)",
      providers_per_10k_65p <= q[[2]] ~ "Q2",
      providers_per_10k_65p <= q[[3]] ~ "Q3",
      TRUE ~ "Q4 (Highest)"
    ),
    preparedness_gap_q1 = case_when(
      provider_density_quartile == "Q1 (Lowest)" ~ TRUE,
      is.na(provider_density_quartile) ~ NA,
      TRUE ~ FALSE
    ),
    preparedness_4cat = case_when(
      zero_provider_eligible ~ paste0("Zero providers (\u2265", format(min_65p, big.mark = ","), " residents age 65+)"),
      eligible_for_rate & preparedness_gap_q1 & providers_total > 0 ~ "Low density (lowest quartile, >0 providers)",
      eligible_for_rate & !preparedness_gap_q1 ~ "Higher capacity (upper 75%)",
      TRUE ~ paste0("Not ranked (<", format(min_65p, big.mark = ","), " residents age 65+)")
    )
  ) %>%
  mutate(
    preparedness_4cat = factor(
      preparedness_4cat,
      levels = c(
        paste0("Zero providers (\u2265", format(min_65p, big.mark = ","), " residents age 65+)"),
        "Low density (lowest quartile, >0 providers)",
        "Higher capacity (upper 75%)",
        paste0("Not ranked (<", format(min_65p, big.mark = ","), " residents age 65+)")
      )
    )
  )

saveRDS(df, out_rds)
write_csv(df, out_csv, na = "")

qc_thresholds <- tibble(
  denom_threshold_min_pop_65p = min_65p,
  q1_cut = as.numeric(q[[1]]),
  q2_cut = as.numeric(q[[2]]),
  q3_cut = as.numeric(q[[3]])
)

qc_counts <- df %>%
  summarise(
    counties_total = n(),
    counties_eligible = sum(eligible_for_rate, na.rm = TRUE),
    counties_not_ranked = sum(!eligible_for_rate, na.rm = TRUE),
    zero_provider_eligible_counties = sum(zero_provider_eligible, na.rm = TRUE),
    lowest_quartile_counties = sum(preparedness_gap_q1 == TRUE, na.rm = TRUE),
    upper_75pct_counties = sum(preparedness_gap_q1 == FALSE, na.rm = TRUE),
    .groups = "drop"
  )

table_quartiles <- df %>%
  filter(eligible_for_rate) %>%
  count(provider_density_quartile, name = "n_counties") %>%
  mutate(pct = round(100 * n_counties / sum(n_counties), 1))

table_zero_provider <- df %>%
  filter(zero_provider_eligible) %>%
  arrange(county_name) %>%
  select(county_geoid, county_name, total_pop, pop_65p, providers_total, providers_per_10k_65p)

write_csv(qc_thresholds, out_qc_thr, na = "")
write_csv(qc_counts, out_qc_counts, na = "")
write_csv(table_quartiles, out_quartiles, na = "")
write_csv(table_zero_provider, out_zero_table, na = "")

tx_counties_sf <- tigris::counties(state = "TX", cb = TRUE, year = 2023, class = "sf") %>%
  transmute(county_geoid = GEOID, geometry)

map_df <- tx_counties_sf %>%
  left_join(df, by = "county_geoid")

# Compute label points in a projected CRS, then transform back for plotting.
zero_label_sf <- map_df %>%
  filter(zero_provider_eligible == TRUE) %>%
  st_transform(3857) %>%
  mutate(label_pt = st_point_on_surface(geometry)) %>%
  st_as_sf() %>%
  st_set_geometry("label_pt") %>%
  st_transform(4326)

lvl <- levels(df$preparedness_4cat)
fill_colors_4cat <- setNames(
  c("#C65D57", "#E3A84A", "#5B7DAA", "#D9D9D9"),
  lvl
)

p_map4 <- ggplot(map_df) +
  geom_sf(aes(fill = preparedness_4cat), color = "white", linewidth = 0.15) +
  geom_sf_text(
    data = zero_label_sf,
    aes(label = county_name),
    size = 3.0,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = fill_colors_4cat, name = "Preparedness category", drop = FALSE) +
  labs(
    title = "Preparedness Gaps in Long-Term Care Infrastructure (Texas)",
    subtitle = paste0(
      "County facility presence for HHAs + SNFs. Eligibility threshold: \u2265",
      format(min_65p, big.mark = ","), " residents age 65+."
    ),
    caption = "Provider counts reflect facility presence, not bed capacity, utilization, or service radius."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "right",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 11),
    plot.caption     = element_text(size = 9)
  )

ggsave(out_map4, p_map4, width = 10, height = 7, dpi = 300, bg = "white")

scatter_df <- df %>%
  mutate(
    point_group = case_when(
      zero_provider_eligible ~ "Zero providers (eligible)",
      eligible_for_rate & preparedness_gap_q1 ~ "Low density (Q1)",
      eligible_for_rate & !preparedness_gap_q1 ~ "Higher capacity (Q2–Q4)",
      TRUE ~ "Not ranked"
    ),
    point_group = factor(
      point_group,
      levels = c("Zero providers (eligible)", "Low density (Q1)", "Higher capacity (Q2–Q4)", "Not ranked")
    )
  ) %>%
  filter(is.finite(aging_share), is.finite(providers_per_10k_65p))

x_lo <- quantile(scatter_df$aging_share, 0.02, na.rm = TRUE)
x_hi <- quantile(scatter_df$aging_share, 0.98, na.rm = TRUE)
y_hi <- quantile(scatter_df$providers_per_10k_65p, 0.95, na.rm = TRUE)

# Make axis trimming explicit so ggplot doesn't warn about removed rows.
scatter_df_plot <- scatter_df %>%
  filter(
    aging_share >= max(0, x_lo),
    aging_share <= min(1, x_hi),
    providers_per_10k_65p >= 0,
    providers_per_10k_65p <= y_hi
  )

trend_df <- scatter_df_plot %>% filter(eligible_for_rate)
fit <- lm(providers_per_10k_65p ~ aging_share, data = trend_df)
fit_sum <- summary(fit)
slope <- unname(coef(fit)[["aging_share"]])
r2 <- fit_sum$r.squared

pt_colors <- c(
  "Zero providers (eligible)" = "#C65D57",
  "Low density (Q1)"          = "#E3A84A",
  "Higher capacity (Q2–Q4)"   = "#5B7DAA",
  "Not ranked"                = "#BDBDBD"
)

p_scatter <- ggplot(scatter_df_plot, aes(x = aging_share, y = providers_per_10k_65p)) +
  geom_point(aes(color = point_group), alpha = 0.75, size = 2.3) +
  geom_smooth(data = trend_df, method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  scale_color_manual(values = pt_colors, name = "County category") +
  scale_x_continuous(
    limits = c(max(0, x_lo), min(1, x_hi)),
    labels = percent_format(accuracy = 1)
  ) +
  coord_cartesian(ylim = c(0, y_hi)) +
  labs(
    title = "Provider Density vs Aging Intensity (Texas Counties)",
    subtitle = sprintf(
      "Eligible counties trend: slope = %.2f providers per 10k 65+ per +1.0 in aging share; R\u00b2 = %.3f",
      slope, r2
    ),
    x = "Share of population age 65+ (ACS)",
    y = "Providers per 10,000 residents age 65+",
    caption = paste0(
      "Y-axis capped at 95th percentile (", round(y_hi, 1), "). ",
      "Not ranked: < ", format(min_65p, big.mark = ","), " residents age 65+."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 11),
    plot.caption     = element_text(size = 9),
    legend.position  = "bottom"
  )

ggsave(out_scatt, p_scatter, width = 9, height = 6, dpi = 300, bg = "white")

eligible_df <- df %>% filter(eligible_for_rate)

bottom5 <- eligible_df %>%
  arrange(providers_per_10k_65p, providers_total, pop_65p) %>%
  slice_head(n = 5) %>%
  mutate(rank_group = "Bottom 5 (eligible, lowest density)")

top5 <- eligible_df %>%
  arrange(desc(providers_per_10k_65p), desc(providers_total), desc(pop_65p)) %>%
  slice_head(n = 5) %>%
  mutate(rank_group = "Top 5 (eligible, highest density)")

top_bottom_table <- bind_rows(bottom5, top5) %>%
  select(rank_group, county_name, county_geoid, pop_65p, providers_total, providers_per_10k_65p) %>%
  mutate(providers_per_10k_65p = round(providers_per_10k_65p, 2))

write_csv(top_bottom_table, out_table, na = "")

message("Preparedness dataset: ", basename(out_csv))
message("Figures: ", basename(out_map4), ", ", basename(out_scatt))
message("Tables/QC: qc_preparedness_thresholds.csv, qc_preparedness_counts.csv, table_preparedness_quartiles.csv, table_zero_provider_eligible_counties.csv, table_top_bottom_counties.csv")