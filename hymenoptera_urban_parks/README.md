# Hymenoptera assemblages in urban forest parks — research data and code

**Data creator:** Bárbara Ibelli Victorino  
**Institution:** Federal University of São Carlos (UFSCar), Brazil  
**Study region:** Six urban forest parks in São Carlos, São Paulo State, southeastern Brazil (Cerrado–Atlantic Forest transition).  
**Sampling:** Two seasonal campaigns, 2023–2024 and 2024–2025.

## Dataset overview

This folder provides a curated subset of field observations and park-level landscape measurements from a study of Hymenoptera assemblages in urban forest parks. It is designed for ongoing research use and is **not tied to any journal or publication venue**. The dataset has **156 repeated temporal sampling observations**, **14 superfamilies**, and **62,147 individuals**; spatial analyses use **six independent parks**.

These are a selection of the original research data. Other measurements collected during the doctoral project are not included in this dataset.

## Files

- `data/hymenoptera_samples.csv` — sample-level abundances: 156 records, six parks, two campaigns, 13 sampling intervals per park and campaign, effective Malaise trapping days and counts for 14 Hymenoptera superfamilies.
- `data/landscape_predictors.csv` — six park-level records, with fragment area, Euclidean nearest-neighbor distance (ENN), NDVI, natural vegetation cover, and urbanized area in 250-, 500-, and 1,000-m buffers (11 spatial predictors).
- `data/data_dictionary.csv` — descriptions of fields, data types, units, and derivation.
- `scripts/00_run_checks.R` — entry point for validation and selected ecological computations.
- `scripts/01_validate_describe.R` — sample totals, sample and park diversity metrics (Shannon, Simpson and Pielou), park capture rates.
- `scripts/02_exact_landscape_correlations.R` — 33 Spearman tests at the park level (all 720 label permutations; Benjamini–Hochberg adjustments within each group of 11 tests).
- `scripts/03_rarefaction_inputs.R` — campaign richness and extrapolation calculations when package `iNEXT` is installed.
- `scripts/04_community_inputs.R` — square-root-transformed abundance input and Bray–Curtis dissimilarities when package `vegan` is installed.

## R usage

Start R in the **root of the repository** (`urban-hymenoptera-ecology`) and run:

```r
source('hymenoptera_urban_parks/scripts/00_run_checks.R', encoding = 'UTF-8')
```

Required for optional analysis modules:

```r
install.packages(c('iNEXT', 'vegan'))
```

The entry script expects both CSV files in `hymenoptera_urban_parks/data/`, then sets the working directory to `hymenoptera_urban_parks` to run the checks. It writes derived files to the ignored local `outputs/` directory.

**Status of scripts:** checked for referenced file paths and data structure, but **not executed in an R runtime** during package preparation. These scripts validate observations and reproduce selected descriptors and spatial correlations; they do **not** provide full re-fitting or verification of the negative-binomial, COM-Poisson and beta models, final NMDS, restricted-permutation PERMANOVA, or PERMDISP. Do not assume that all published numerical results are reproduced by this code alone.

## Variable interpretation

`park_id` ranges from P1 to P6. Campaigns are `2023-2024` and `2024-2025`. `sampling_interval` is the order from 1 to 13 within each park and campaign; `sample_id` is an anonymized/stable public record key rather than the original laboratory identifier. Superfamily columns are counts of individuals, and zeros denote no specimens recorded.

`trapping_days` is **effective Malaise trap operation time** for each sample, used as the exposure in abundance calculations. The P6 records for sampling interval 6 in each campaign contain 10 effective days, while the source collection date endpoints span 11 calendar days. This discrepancy should be cross-checked against field records if analyzing effort-sensitive models. Collection start/end dates are not included in this dataset.

At park level, `fragment_area_ha` is area in hectares; `enn_m` is edge-to-edge structural isolation distance in metres (not functional connectivity). NDVI is dimensionless. `natural_vegetation_pct_*` and `urbanized_area_pct_*` are percentages ranging from 0 to 100. Natural vegetation aggregates MapBiomas classes 3 (Forest Formation), 4 (Savanna Formation), and 11 (Wetland); the individual classes are not distributed here. Raster and complete land-use data are not included.

**Statistical unit:** biological CSV records are repeated temporal observations from six parks. The spatial correlations must use one observation per park (n = 6), not 156 independent landscape replicates. Join the two tables by `park_id` only when relevant to a particular analysis.

## Data provenance, access and citation

The counts were filtered from the data creator’s reviewed study matrix without changing any superfamily count. Landscape variables were selected from the study’s park-level landscape measurements. Public release deliberately excludes meteorological, microclimatic, individual land-cover-class, and other unreported research variables.

If reusing these data, credit **Bárbara Ibelli Victorino (Federal University of São Carlos)** and cite the GitHub repository URL with the date accessed or a specific commit. For code and repository licensing see the top-level `LICENSE` file; data reuse terms should be reviewed by the dataset creator before public distribution. A future archived dataset identifier can be added without renaming this research dataset.

**Public-access notice:** files uploaded to a *public* GitHub repository can be copied and redistributed. Later deletion or a change to private visibility does not undo previously downloaded copies.
