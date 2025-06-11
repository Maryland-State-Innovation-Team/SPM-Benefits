# How Will the 2025 Reconciliation Bill Affect the Uninsured Rate in Each State? Allocating CBO’s Estimates of Coverage Loss
# https://www.kff.org/affordable-care-act/issue-brief/how-will-the-2025-reconciliation-bill-affect-the-uninsured-rate-in-each-state-allocating-cbos-estimates-of-coverage-loss/

# Estimated number of SNAP participants in households at risk from two provisions of Johnson proposal
# https://www.cbpp.org/sites/default/files/Congressional-district-data.pdf

# Expanded Work Requirements in House Republican Bill Would Take Away Food Assistance From Millions: State and Congressional District Estimates
# https://www.cbpp.org/research/food-assistance/expanded-work-requirements-in-house-republican-bill-would-take-away-food

list.of.packages <- c(
  "data.table", "sf", "dplyr", "survey", "stringr",
  "ggplot2", "scales", "Hmisc", "ipumsr"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

setwd("C:/git/SPM-Benefits/")

# From ACS PUMS
# - SNAP recipients
# - Medicaid and public health insurance coverage
# - Usual hours worked per week
# - Head of household age (18 to 64)
# - Head of household disability status
# - Children age (6 or younger)

# Poverty Formulas:
#   SPM_Resources = SPM_Totval +
#  (SPM_SnapSub + SPM_CapHouseSub + SPM_SchLunch + SPM_EngVal + SPM_WICval) –
#  (SPM_FICA + SPM_FedTax + SPM_StTax) – SPM_CapWkCCXpns – SPM_MedXpns

# SPM_Poor
# = 0 if SPM_Resources >= SPM_PovThreshold
# = 1 if SPM_Resources < SPM_PovThreshold

ddi = read_ipums_ddi("large_inputs/usa_00008.xml")
acs = read_ipums_micro(ddi)
acs = subset(acs, SPMFAMUNIT != 9999999999) # Not in universe
acs$PUMA_GEOID = paste0(acs$STATEFIP, str_pad(acs$PUMA, 5, pad="0"))

# How many current SNAP and Medicaid beneficiaries do not meet the future work requirements?
acs$MEDICAID_BENEFICIARY = acs$HINSCAID == 2
acs$SNAP_RECIPIENT = acs$SPMSNAP > 0
acs$ADULT = acs$AGE > 17 & acs$AGE < 65
acs$DIFF = (
  acs$DIFFREM == 2 |
    acs$DIFFPHYS == 2 |
    acs$DIFFMOB == 2 |
    acs$DIFFCARE == 2 |
    acs$DIFFSENS == 2 |
    acs$DIFFEYE == 2 |
    acs$DIFFHEAR == 2
)
acs = acs %>%
  group_by(SERIAL) %>%
  mutate(CHILD6 = any(AGE <= 6) & NCHILD > 0) %>% # Person has their own child in the household, and the household has a child 6 and under
  ungroup()
acs$WORK20 = acs$UHRSWORK >= 20

acs$SNAP_AFFECTED =
  acs$SNAP_RECIPIENT & # Snap recipients
  acs$ADULT & # Aged 18 to 64
  !acs$DIFF & # Without disabilities
  !acs$CHILD6 & # Without children 6 and younger
  !acs$WORK20 # That don't usually work at least 20 hours per week

acs$MEDICAID_AFFECTED =
  acs$MEDICAID_BENEFICIARY & # Medicaid beneficiaries
  acs$ADULT & # Aged 18 to 64
  !acs$DIFF & # Without disabilities
  !acs$CHILD6 & # Without children 6 and younger
  !acs$WORK20 # That don't usually work at least 20 hours per week

acs = acs %>%
  group_by(SERIAL) %>%
  mutate(
    HH_SNAP_AFFECTED = any(SNAP_AFFECTED),
    HH_MEDICAID_AFFECTED = any(MEDICAID_AFFECTED)
  ) %>%
  ungroup()

acs$POTENTIAL_LOST_SNAP_BENEFITS = acs$SPMSNAP * (acs$HH_SNAP_AFFECTED * 1)
acs$SPMTOTRES_LESS_SNAP = acs$SPMTOTRES - acs$POTENTIAL_LOST_SNAP_BENEFITS
acs$SPMPOV_LESS_SNAP = (acs$SPMTOTRES_LESS_SNAP < acs$SPMTHRESH) * 1

acs$ADDITIONAL_MEDICAL_EXPENSES = 3421 * (acs$HH_MEDICAID_AFFECTED * 1) # Difference between median expenses in medicare and non-medicare households
acs$SPMTOTRES_LESS_MED = acs$SPMTOTRES - acs$ADDITIONAL_MEDICAL_EXPENSES
acs$SPMPOV_LESS_MED = (acs$SPMTOTRES_LESS_MED < acs$SPMTHRESH) * 1

acs$SPMTOTRES_LESS_SNAP_LESS_MED = acs$SPMTOTRES - (acs$POTENTIAL_LOST_SNAP_BENEFITS + acs$ADDITIONAL_MEDICAL_EXPENSES)
acs$SPMPOV_LESS_SNAP_LESS_MED = (acs$SPMTOTRES_LESS_SNAP_LESS_MED < acs$SPMTHRESH) * 1

svy = svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = acs,
  nest = TRUE,
  check.strata = FALSE
)
# medicaid_svy = subset(svy, MEDICAID_BENEFICIARY)
# non_medicaid_svy = subset(svy, !MEDICAID_BENEFICIARY)
# svyquantile(~SPMMEDXPNS, medicaid_svy, quantiles=0.5, ci=F)
# 1579
# svyquantile(~SPMMEDXPNS, non_medicaid_svy, quantiles=0.5, ci=F)
# 5000
# 5000 - 1579
# [1] 3421


# Check overall significance
svymean(~SPMPOV, svy)
svymean(~SPMPOV_LESS_SNAP, svy)
svyttest(I(SPMPOV - SPMPOV_LESS_SNAP) ~ 0, design = svy)

svymean(~SPMPOV, svy)
svymean(~SPMPOV_LESS_MED, svy)
svyttest(I(SPMPOV - SPMPOV_LESS_MED) ~ 0, design = svy)

svymean(~SPMPOV, svy)
svymean(~SPMPOV_LESS_SNAP_LESS_MED, svy)
svyttest(I(SPMPOV - SPMPOV_LESS_SNAP_LESS_MED) ~ 0, design = svy)

# Child poverty
child_svy = subset(svy, AGE < 18)

svymean(~SPMPOV, child_svy)
svymean(~SPMPOV_LESS_SNAP, child_svy)
svyttest(I(SPMPOV - SPMPOV_LESS_SNAP) ~ 0, design = child_svy)
svytotal(~I(SPMPOV_LESS_SNAP-SPMPOV), child_svy)

svymean(~SPMPOV, child_svy)
svymean(~SPMPOV_LESS_MED, child_svy)
svyttest(I(SPMPOV - SPMPOV_LESS_MED) ~ 0, design = child_svy)
svytotal(~I(SPMPOV_LESS_MED-SPMPOV), child_svy)

svymean(~SPMPOV, child_svy)
svymean(~SPMPOV_LESS_SNAP_LESS_MED, child_svy)
svyttest(I(SPMPOV - SPMPOV_LESS_SNAP_LESS_MED) ~ 0, design = child_svy)
svytotal(~I(SPMPOV_LESS_SNAP_LESS_MED-SPMPOV), child_svy)
confint(
  svytotal(~I(SPMPOV_LESS_SNAP_LESS_MED-SPMPOV), child_svy)
)

# Youth definition
youth_svy = subset(svy, AGE < 25)
svymean(~SPMPOV, youth_svy)
svymean(~SPMPOV_LESS_SNAP, youth_svy)
svyttest(I(SPMPOV - SPMPOV_LESS_SNAP) ~ 0, design = youth_svy)
svytotal(~I(SPMPOV_LESS_SNAP-SPMPOV), youth_svy)

svymean(~SPMPOV, youth_svy)
svymean(~SPMPOV_LESS_MED, youth_svy)
svyttest(I(SPMPOV - SPMPOV_LESS_MED) ~ 0, design = youth_svy)
svytotal(~I(SPMPOV_LESS_MED-SPMPOV), youth_svy)

svymean(~SPMPOV, youth_svy)
svymean(~SPMPOV_LESS_SNAP_LESS_MED, youth_svy)
svyttest(I(SPMPOV - SPMPOV_LESS_SNAP_LESS_MED) ~ 0, design = youth_svy)
svytotal(~I(SPMPOV_LESS_SNAP_LESS_MED-SPMPOV), youth_svy)
confint(
  svytotal(~I(SPMPOV_LESS_SNAP_LESS_MED-SPMPOV), youth_svy)
)

# By PUMA
merge_list = list()
merge_index = 1
## Baseline child
puma_child_poverty_counts = svyby(
  ~SPMPOV,
  FUN=svytotal,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_counts$se =
  puma_child_poverty_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_counts) = c(
  "GEOID", "child_poverty_baseline_count", "child_poverty_baseline_count_moe"
)

puma_child_poverty_percents = svyby(
  ~SPMPOV,
  FUN=svymean,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_percents$se =
  puma_child_poverty_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_percents) = c(
  "GEOID", "child_poverty_baseline_percent", "child_poverty_baseline_percent_moe"
)
merge_list[[merge_index]] = puma_child_poverty_percents
merge_index = merge_index + 1

## With SNAP work requirements child
puma_child_poverty_snap_counts = svyby(
  ~SPMPOV_LESS_SNAP,
  FUN=svytotal,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_snap_counts$se =
  puma_child_poverty_snap_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_snap_counts) = c(
  "GEOID", "child_poverty_snap_count", "child_poverty_snap_count_moe"
)
merge_list[[merge_index]] = puma_child_poverty_snap_counts
merge_index = merge_index + 1

puma_child_poverty_snap_percents = svyby(
  ~SPMPOV_LESS_SNAP,
  FUN=svymean,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_snap_percents$se =
  puma_child_poverty_snap_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_snap_percents) = c(
  "GEOID", "child_poverty_snap_percent", "child_poverty_snap_percent_moe"
)
merge_list[[merge_index]] = puma_child_poverty_snap_percents
merge_index = merge_index + 1

## With Medicaid work requirements child
puma_child_poverty_med_counts = svyby(
  ~SPMPOV_LESS_MED,
  FUN=svytotal,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_med_counts$se =
  puma_child_poverty_med_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_med_counts) = c(
  "GEOID", "child_poverty_med_count", "child_poverty_med_count_moe"
)
merge_list[[merge_index]] = puma_child_poverty_med_counts
merge_index = merge_index + 1

puma_child_poverty_med_percents = svyby(
  ~SPMPOV_LESS_MED,
  FUN=svymean,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_med_percents$se =
  puma_child_poverty_med_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_med_percents) = c(
  "GEOID", "child_poverty_med_percent", "child_poverty_med_percent_moe"
)
merge_list[[merge_index]] = puma_child_poverty_med_percents
merge_index = merge_index + 1

## With both SNAP and Medicaid work requirements child
puma_child_poverty_snap_med_counts = svyby(
  ~SPMPOV_LESS_SNAP_LESS_MED,
  FUN=svytotal,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_snap_med_counts$se =
  puma_child_poverty_snap_med_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_snap_med_counts) = c(
  "GEOID", "child_poverty_snap_med_count", "child_poverty_snap_med_count_moe"
)
merge_list[[merge_index]] = puma_child_poverty_snap_med_counts
merge_index = merge_index + 1

puma_child_poverty_snap_med_percents = svyby(
  ~SPMPOV_LESS_SNAP_LESS_MED,
  FUN=svymean,
  by=~PUMA_GEOID,
  child_svy,
)
puma_child_poverty_snap_med_percents$se =
  puma_child_poverty_snap_med_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_child_poverty_snap_med_percents) = c(
  "GEOID", "child_poverty_snap_med_percent", "child_poverty_snap_med_percent_moe"
)
merge_list[[merge_index]] = puma_child_poverty_snap_med_percents
merge_index = merge_index + 1

## Baseline youth
puma_youth_poverty_counts = svyby(
  ~SPMPOV,
  FUN=svytotal,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_counts$se =
  puma_youth_poverty_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_counts) = c(
  "GEOID", "youth_poverty_baseline_count", "youth_poverty_baseline_count_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_counts
merge_index = merge_index + 1

puma_youth_poverty_percents = svyby(
  ~SPMPOV,
  FUN=svymean,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_percents$se =
  puma_youth_poverty_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_percents) = c(
  "GEOID", "youth_poverty_baseline_percent", "youth_poverty_baseline_percent_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_percents
merge_index = merge_index + 1

## With SNAP work requirements youth
puma_youth_poverty_snap_counts = svyby(
  ~SPMPOV_LESS_SNAP,
  FUN=svytotal,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_snap_counts$se =
  puma_youth_poverty_snap_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_snap_counts) = c(
  "GEOID", "youth_poverty_snap_count", "youth_poverty_snap_count_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_snap_counts
merge_index = merge_index + 1

puma_youth_poverty_snap_percents = svyby(
  ~SPMPOV_LESS_SNAP,
  FUN=svymean,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_snap_percents$se =
  puma_youth_poverty_snap_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_snap_percents) = c(
  "GEOID", "youth_poverty_snap_percent", "youth_poverty_snap_percent_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_snap_percents
merge_index = merge_index + 1

## With Medicaid work requirements youth
puma_youth_poverty_med_counts = svyby(
  ~SPMPOV_LESS_MED,
  FUN=svytotal,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_med_counts$se =
  puma_youth_poverty_med_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_med_counts) = c(
  "GEOID", "youth_poverty_med_count", "youth_poverty_med_count_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_med_counts
merge_index = merge_index + 1

puma_youth_poverty_med_percents = svyby(
  ~SPMPOV_LESS_MED,
  FUN=svymean,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_med_percents$se =
  puma_youth_poverty_med_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_med_percents) = c(
  "GEOID", "youth_poverty_med_percent", "youth_poverty_med_percent_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_med_percents
merge_index = merge_index + 1

## With both SNAP and Medicaid work requirements youth
puma_youth_poverty_snap_med_counts = svyby(
  ~SPMPOV_LESS_SNAP_LESS_MED,
  FUN=svytotal,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_snap_med_counts$se =
  puma_youth_poverty_snap_med_counts$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_snap_med_counts) = c(
  "GEOID", "youth_poverty_snap_med_count", "youth_poverty_snap_med_count_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_snap_med_counts
merge_index = merge_index + 1

puma_youth_poverty_snap_med_percents = svyby(
  ~SPMPOV_LESS_SNAP_LESS_MED,
  FUN=svymean,
  by=~PUMA_GEOID,
  youth_svy,
)
puma_youth_poverty_snap_med_percents$se =
  puma_youth_poverty_snap_med_percents$se * qnorm(0.975) # SE to 95% MOE
names(puma_youth_poverty_snap_med_percents) = c(
  "GEOID", "youth_poverty_snap_med_percent", "youth_poverty_snap_med_percent_moe"
)
merge_list[[merge_index]] = puma_youth_poverty_snap_med_percents
merge_index = merge_index + 1

# Merge
puma_dat = puma_child_poverty_counts
for(i in 1:length(merge_list)){
  puma_dat = merge(puma_dat, merge_list[[i]], by="GEOID")
}
fwrite(puma_dat, "outputs/child_and_youth_poverty_effects_by_puma.csv")


# IPUMS PUMAs shapefile https://usa.ipums.org/usa/volii/boundaries.shtml
pumas = st_read("large_inputs/ipums_2020_pumas/ipums_puma_2020.shp")
pumas = subset(pumas, State=="Maryland")
pumas = st_transform(pumas, crs=4326)

pumas = merge(pumas, puma_dat)
simple_pumas = st_simplify(pumas, preserveTopology = T, dTolerance = 10)
st_write(simple_pumas, "outputs/pumas.geojson", delete_dsn = T)
