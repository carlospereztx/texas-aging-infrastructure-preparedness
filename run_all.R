# run_all.R
# Run the full pipeline from the project root.

options(stringsAsFactors = FALSE)
options(warn = 1)

cat("Running pipeline...\n")

root_marker <- file.path("src", "00_setup_packages.R")
if (!file.exists(root_marker)) {
  stop("Project root not detected. Expected: ", root_marker)
}

scripts <- c(
  "src/00_setup_packages.R",
  "src/01_ingest_snf.R",
  "src/02_ingest_hha.R",
  "src/03_assign_geoid_snf.R",
  "src/04_assign_geoid_hha.R",
  "src/05_rollup_county_supply.R",
  "src/06_merge_county_risk.R",
  "src/07_prepare_preparedness_metrics.R",
  "src/08_make_summary_figures.R"
)

for (s in scripts) {
  if (!file.exists(s)) stop("Missing script: ", s)
  cat("\n==> ", s, "\n", sep = "")
  source(s, local = new.env())
}

cat("\nDone.\n")