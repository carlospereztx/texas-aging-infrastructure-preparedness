# ------------------------------------------------------------
# 08_make_summary_figures.R
# Generates executive summary figures from final QC tables
# ------------------------------------------------------------

library(tidyverse)

cat("Generating summary figures...\n")

dir.create("reports/figures", showWarnings = FALSE, recursive = TRUE)


# ------------------------------------------------------------
# 1) Eligibility Overview (stacked: eligible vs not eligible)
#   Source: reports/tables/qc_preparedness_counts.csv
#   Columns: counties_total, counties_eligible, zero_provider_eligible_counties
# ------------------------------------------------------------

counts <- readr::read_csv("reports/tables/qc_preparedness_counts.csv", show_col_types = FALSE)

total_counties <- counts$counties_total[[1]]
eligible_counties <- counts$counties_eligible[[1]]
not_eligible_counties <- total_counties - eligible_counties
zero_provider_eligible <- counts$zero_provider_eligible_counties[[1]]

elig_df <- tibble::tibble(
  group = "All Texas counties",
  status = factor(c("Eligible (ranked)", "Not eligible / not ranked"),
                  levels = c("Eligible (ranked)", "Not eligible / not ranked")),
  n = c(eligible_counties, not_eligible_counties)
)

p1 <- ggplot2::ggplot(elig_df, ggplot2::aes(x = group, y = n, fill = status)) +
  ggplot2::geom_col(width = 0.6) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Eligibility Overview (Texas Counties)",
    subtitle = paste0("Zero-provider eligible counties: ", zero_provider_eligible),
    x = NULL,
    y = "Number of Counties",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  "reports/figures/fig_eligibility_overview_stacked.png",
  p1, width = 8, height = 3.8, dpi = 300
)

# ------------------------------------------------------------
# 2) Zero-Provider Eligible Counties
# ------------------------------------------------------------

zero <- readr::read_csv(
  "reports/tables/table_zero_provider_eligible_counties.csv",
  show_col_types = FALSE
)

if (nrow(zero) > 0) {
  
  p2 <- zero %>%
    dplyr::arrange(dplyr::desc(pop_65p)) %>%
    dplyr::slice_head(n = 15) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(county_name, pop_65p),
        y = pop_65p
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Top Zero-Provider Eligible Counties by 65+ Population",
      x = NULL,
      y = "Population Age 65+"
    ) +
    ggplot2::theme_minimal(base_size = 13)
  
  ggplot2::ggsave(
    "reports/figures/fig_zero_provider_top_counties.png",
    p2, width = 8, height = 6, dpi = 300
  )
  
} else {
  cat("No zero-provider eligible counties found; skipping figure.\n")
}

# ------------------------------------------------------------
# 3) ZIP → County Match Method Breakdown
# ------------------------------------------------------------

match_method <- read_csv("reports/tables/qc_hha_match_method_breakdown.csv",
                         show_col_types = FALSE)

p3 <- ggplot(match_method,
             aes(x = match_method, y = n, fill = match_method)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "ZIP to County Match Method Breakdown",
    x = "Match Method",
    y = "Number of Records"
  ) +
  theme_minimal(base_size = 13)

ggsave("reports/figures/fig_zip_match_method_breakdown.png",
       p3, width = 8, height = 5, dpi = 300)

cat("Summary figures saved.\n")