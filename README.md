# SPM-Benefits

This project explores how changes in SNAP and Medicaid benefit eligibility, specifically expanded work requirements, could affect the Supplemental Poverty Measure (SPM) in Maryland. The analysis is based on microdata from the American Community Survey (ACS) Public Use Microdata Sample (PUMS) and uses survey-weighted methods to estimate the impact on child and youth poverty at the PUMA (Public Use Microdata Area) level.

## Methodology

The analysis uses the following approach:

1. **Data Source**: ACS PUMS data with SPM and demographic variables, downloaded from [IPUMS USA](https://usa.ipums.org/usa/). Users wishing to replicate the results must obtain a similar file with variables for SNAP receipt, Medicaid coverage, hours worked, age, disability status, and household composition.

2. **Identifying Affected Households**:
   - **SNAP and Medicaid Recipients**: Individuals are flagged as SNAP or Medicaid beneficiaries based on reported program participation.
   - **Work Requirement Exemptions**: Adults aged 18–64 are considered subject to new work requirements unless they have a disability, have a child aged 6 or younger, or usually work at least 20 hours per week.
   - **Household Impact**: Households are flagged as affected if any member is subject to the new requirements and would lose benefits.

3. **Estimating Financial Impact**:
   - **SNAP**: The value of SNAP benefits is subtracted from SPM resources for affected households.
   - **Medicaid**: The financial impact of losing Medicaid is estimated by adding the difference in median medical expenses between Medicaid and non-Medicaid households ($3,421) to affected households’ expenses. This approach assumes that losing Medicaid increases out-of-pocket medical costs by this amount.

4. **Poverty Calculation**:
   - SPM poverty status is recalculated for each scenario: baseline, loss of SNAP, loss of Medicaid, and loss of both.
   - Survey-weighted means and totals are computed for children (<18) and youth (<25) at the PUMA level, including margins of error.

5. **Outputs**:
   - `outputs/child_and_youth_poverty_effects_by_puma.csv`: CSV with poverty rates and counts by PUMA under each scenario.
   - `outputs/pumas.geojson`: GeoJSON file for mapping results, using Maryland PUMA boundaries.

## Replication

To replicate these results, you will need to:
- Download ACS PUMS data with SPM and relevant demographic variables from [IPUMS USA](https://usa.ipums.org/usa/).
- Download 2020 PUMA shapefiles from IPUMS or the Census Bureau.
- Install the required R packages: `data.table`, `sf`, `dplyr`, `survey`, `stringr`, `ggplot2`, `scales`, `Hmisc`, `ipumsr`.
- Run `code/process_data.R` in R.

## Citation

Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Grace Cooper, Julia A. Rivera Drew, Stephanie Richards, Renae Rodgers, Jonathan Schroeder, and Kari C.W. Williams. IPUMS USA: Version 16.0 [dataset]. Minneapolis, MN: IPUMS, 2025. https://doi.org/10.18128/D010.V16.0
