# Texas County Aging Infrastructure Preparedness
**R-Based ETL + Public Health Infrastructure Analytics**

* **Author:** Carlos Perez
* **Project Type:** County-Level Structural Capacity Modeling
* **Environment:** R (`tidyverse`, `tidycensus`, `sf`), CMS Public Use Files
* **Status:** Fully reproducible end-to-end pipeline

---

## Executive Summary
This project builds a production-style ETL and analytics pipeline to evaluate county-level aging infrastructure preparedness across Texas. 

Using CMS Skilled Nursing Facility (SNF) and Home Health Agency (HHA) public use files combined with American Community Survey (ACS) aging indicators, the pipeline:

* Standardizes geographic identifiers across heterogeneous public datasets.
* Crosswalks ZIP codes to counties using Census primary logic with HUD fallback.
* Caches ACS demographic indicators locally for deterministic reproducibility.
* Aggregates provider supply at the county level.
* Merges structural supply with demographic aging demand.
* Constructs a preparedness tier framework.
* Generates automated QC tables and executive-ready figures.

The result is a county-level structural assessment of aging service capacity relative to demographic pressure.

**This repository demonstrates:**
* Modular ETL design
* Geographic reconciliation logic
* Public data integration
* County-level risk modeling
* Built-in data validation discipline

> **Note:** This is structural capacity modeling — not descriptive reporting.

---

## Pipeline Overview

```mermaid
flowchart TD
    A[CMS SNF + HHA (raw PUFs)] --> B[Ingest + Clean]
    B --> C[Assign County GEOID (ZIP --> County)]
    C --> D[Aggregate County-Level Supply]
    D --> E[Merge Aging Risk (ACS 5-year, cached locally)]
    E --> F[Preparedness Metrics + Tiering]
    F --> G[Executive Figures + QC Tables]
```

---

## Business Framing: Why This Matters
Texas is aging rapidly, with significant rural–urban disparities in provider distribution. 

Misalignment between elderly population concentration, dual-eligible exposure, skilled nursing facility presence, and home health availability creates access, financial, and network adequacy risk.

**This framework mirrors real-world use cases in:**
* Medicare Advantage network adequacy modeling
* Medicaid LTSS planning
* Rural health infrastructure strategy
* Population health capacity alignment
* State-level aging policy analytics

The tiering logic provides a prioritization framework rather than a descriptive summary.

---

## Architecture
Repository structure:
```
run_all.R
README.md
renv.lock
LICENSE
.gitignore

src/
├── 00_setup_packages.R
├── 01_ingest_snf.R
├── 02_ingest_hha.R
├── 03_assign_geoid_snf.R
├── 04_assign_geoid_hha.R
├── 05_rollup_county_supply.R
├── 06_merge_county_risk.R
├── 07_prepare_preparedness_metrics.R
└── 08_make_summary_figures.R

data/
├── raw/            # CMS public use files (not versioned)
└── processed/      # Final datasets + cached ACS pull

reports/
├── figures/        # Executive visual outputs (PNG)
└── tables/         # QC + supporting tables
```

### Design Principles
* Modular script segmentation
* Deterministic outputs
* Local caching of API pulls
* Explicit fallback logic for ZIP reconciliation
* Automated QC generation
* Single-command reproducibility

---

## Outputs to View First
Navigate to `/reports/figures/` for executive artifacts. These simulate executive-facing infrastructure dashboards:

1. **Preparedness 4-Category Map:** County-level classification of aging infrastructure preparedness.
2. **Supply vs Aging Scatter:** Provider density relative to elderly population concentration.
3. **Eligibility Overview (Stacked):** Constructs a preparedness tier framework.
4. **Zero-Provider Counties:** Counties with no SNF or HHA presence.
5. **ZIP Match Breakdown:** Crosswalk performance and unmatched record tracking.

---

## Data Integrity & QA Framework
Healthcare public datasets rarely reconcile cleanly. This pipeline explicitly measures reconciliation quality.

**Built-in validation:**
* ZIP → County match rate reporting
* Fallback tracking (Census primary vs unmatched)
* Zero-provider detection
* Missing GEOID flagging
* ACS merge coverage checks
* Row count validation
* Output snapshot consistency

*QC tables are written automatically to `/reports/tables/`.*

**Reconciliation priority order:**
1. Census ZIP crosswalk
2. HUD fallback
3. Explicit unmatched logging

---

## Reproducibility

### Requirements
* R ≥ 4.2
* `renv` (included)
* Census API key

### 1. Restore Environment
```R
renv::restore()
```

### 2. Set Census API Key
```R
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

### 3. Run Full Pipeline
```R
source("run_all.R")
```

Outputs regenerate in:
* `/data/processed/`
* `/reports/figures/`
* `/reports/tables/`

*Note: ACS demographic pulls are cached locally after first retrieval to ensure deterministic reproducibility and avoid API rate limitations.*

---

## Data Sources
All sources are publicly available and reproducible:
* CMS Skilled Nursing Facility Public Use Files
* CMS Home Health Agency Public Use Files
* U.S. Census Bureau – American Community Survey (5-Year Estimates)
* Census ZIP to County Crosswalk
* HUD ZIP–County Crosswalk

---

## Limitations
* Provider presence reflects facility count, not bed capacity or staffing intensity.
* ACS 5-year estimates introduce temporal lag.
* ZIP-to-county mapping may introduce minor geographic distortion.
* Preparedness tiers are analytic constructs, not regulatory classifications.
* Financial viability, reimbursement rates, and quality metrics are not modeled.

*This project focuses on structural supply alignment rather than operational performance.*

