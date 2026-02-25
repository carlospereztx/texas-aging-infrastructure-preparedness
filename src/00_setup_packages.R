# src/00_setup_packages.R
# Environment setup: packages + standard project folders.

suppressPackageStartupMessages({
  library(fs)
  library(here)
})

# ---- folders ----
dir_create(here("data", "raw"))
dir_create(here("data", "processed"))
dir_create(here("reports", "figures"))
dir_create(here("reports", "tables"))

# ---- packages ----
required_packages <- c(
  "tidyverse",
  "janitor",
  "stringr",
  "readr",
  "here",
  "fs",
  "tigris",
  "sf",
  "tidycensus"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

AUTO_INSTALL <- FALSE

if (length(missing) > 0) {
  if (AUTO_INSTALL) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  } else {
    stop(
      "Missing required packages: ", paste(missing, collapse = ", "),
      "\nInstall with:\n",
      "install.packages(c(", paste0('"', missing, '"', collapse = ", "), "), dependencies = TRUE)\n",
      call. = FALSE
    )
  }
}

suppressPackageStartupMessages(
  invisible(lapply(required_packages, require, character.only = TRUE))
)

message("Setup complete.")