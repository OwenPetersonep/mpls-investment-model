# ============================================================
# 02_scoring_model.R
# Minneapolis Neighborhood Investment Model
# Data Quality Fixes + Annualized CAGR Scoring
# ============================================================
#
# METHODOLOGY NOTES
# ============================================================
#
# Date of analysis: May 2026
# Data vintage:
#   - Price data:   Zillow ZHVI, March 2024
#   - Permit data:  Minneapolis CCS Permits, 2018-2024
#   - Census data:  ACS 5-year 2023 (2019-2023 survey period)
#
# PRICE METRIC CONSTRUCTION — ANNUALIZED CAGR:
#   All price changes converted to annualized rates using CAGR:
#   CAGR = ((1 + total_return/100)^(1/years) - 1) * 100
#   1yr:  no conversion needed (already annual)
#   3yr:  CAGR over 3 years
#   5yr:  CAGR over 5 years
#
#   Blended CAGR = 5yr_ann(25%) + 3yr_ann(40%) + 1yr_ann(35%)
#   Rationale: all three components now in same units (%/year)
#   5yr avoids COVID-peak distortion as baseline
#   3yr captures medium trend through recovery
#   1yr captures current momentum
#   All components winsorized at 95th percentile before blending
#
# PERMIT PERIOD COMPARISON:
#   Baseline: 2018-2019 (clean pre-COVID)
#   Recent:   2022-2024 (post-COVID settlement)
#   2020-2021 excluded as COVID distortion period
#
# CENSUS GROWTH WINDOW:
#   2018 ACS → 2023 ACS
#   Effective survey midpoints: ~2020 → ~2021
#
# SCORING WEIGHTS (regression-validated):
#   Renter share (reversed): 30% — strongest predictor (R²=0.541)
#   Blended CAGR:            25% — annualized appreciation momentum
#   Income growth:           15% — demographic fundamental
#   Permit volume:           10% — market activity context
#   Permit growth:           10% — construction momentum
#   Rent level:                5% — rental income potential
#   Population change:         5% — demand indicator (lagging)
#
# DATA QUALITY FLAGS:
#   - Bedroom prices flagged: price_3bed > 2.25x median → unreliable
#   - Population artifact:    pop < 100 → NA
#   - Rent imputed:           4 ZIPs missing ZORI → 0.5% rule
#   - Permit baseline:        excludes 2020 COVID year
#
# LIMITATIONS:
#   1. ZIP geography masks intra-neighborhood variation
#   2. Census ACS 2023 effective midpoint ~2021 (3yr lag vs price)
#   3. Permit data covers Minneapolis city limits only
#   4. Regression n≈19 limits statistical power
#   5. Renter share reflects 2022-2024 rate environment
#   6. 3yr CAGR window starts near COVID peak (March 2021)
#      — partially offset by blending with 5yr component
# ============================================================

library(tidyverse)
library(janitor)
library(scales)
library(broom)

setwd("/Users/owenpeterson/Downloads/mpls_investment_model")

# ============================================================
# LOAD DATA
# ============================================================

master <- read_csv("data/processed/master_dataset.csv") %>%
  mutate(zip_code = as.character(zip_code)) %>%
  rename(price_2024 = current_price) # <--- RENAMED HERE

cat("Loaded:", nrow(master), "ZIP codes,", ncol(master), "columns\n")

# Verify 2023 Census columns are present
cat("Census columns found:\n")
print(names(master)[grep("population|renter|income_2", names(master))])

# ============================================================
# FIX 1 — BEDROOM PRICE ANOMALY FLAG
# Flag ZIPs where 3-bed price > 2.25x median (data artifact)
# ============================================================

master <- master %>%
  mutate(
    bedroom_data_reliable = case_when(
      is.na(price_3bed)                     ~ FALSE,
      is.na(price_2024)                     ~ FALSE, # <--- UPDATED HERE
      price_3bed > price_2024 * 2.25        ~ FALSE, # <--- UPDATED HERE
      TRUE                                  ~ TRUE
    ),
    price_3bed_clean = ifelse(bedroom_data_reliable, price_3bed, NA),
    price_4bed_clean = ifelse(bedroom_data_reliable, price_4bed, NA),
    price_5bed_clean = ifelse(bedroom_data_reliable, price_5bed, NA)
  )

cat("=== FIX 1 AUDIT: Bedroom Data ===\n")
cat("Reliable:", sum(master$bedroom_data_reliable, na.rm=TRUE), "ZIPs\n")
cat("Flagged: ", sum(!master$bedroom_data_reliable, na.rm=TRUE), "ZIPs\n\n")

master %>%
  filter(!bedroom_data_reliable) %>%
  select(zip_code, neighborhoods, price_2024, price_3bed) %>% # <--- UPDATED HERE
  mutate(ratio = round(price_3bed / price_2024, 2)) %>% # <--- UPDATED HERE
  print()

# ============================================================
# FIX 2 — POPULATION ARTIFACT FLAG
# ZIPs with population < 100 are Census artifacts → set to NA
# ============================================================

master <- master %>%
  mutate(
    low_pop_flag = case_when(
      is.na(population_2023)   ~ TRUE,
      population_2023 < 100    ~ TRUE,   # catches North Loop artifact (27.9)
      population_2023 < 500    ~ TRUE,   # sparse tract threshold
      TRUE                     ~ FALSE
    ),
    population_2023_clean = ifelse(population_2023 < 100, NA, population_2023),
    pop_change_pct_clean  = ifelse(population_2023 < 100, NA, pop_change_pct)
  )

cat("=== FIX 2 AUDIT: Population ===\n")
cat("Flagged:", sum(master$low_pop_flag, na.rm=TRUE), "ZIPs\n\n")

master %>%
  filter(low_pop_flag) %>%
  select(zip_code, neighborhoods, population_2023, pop_change_pct) %>%
  print()

# ============================================================
# FIX 3 — RENT ESTIMATE FALLBACK
# 4 ZIPs missing ZORI → impute at 0.5% of home value
# ============================================================

master <- master %>%
  mutate(
    rent_imputed = is.na(monthly_rent_estimate),
    monthly_rent_final = case_when(
      !is.na(monthly_rent_estimate) ~ monthly_rent_estimate,
      !is.na(price_2024)            ~ price_2024 * 0.005, # <--- UPDATED HERE
      TRUE                          ~ NA_real_
    )
  )

cat("=== FIX 3 AUDIT: Rent ===\n")
cat("Observed:", sum(!master$rent_imputed), "ZIPs\n")
cat("Imputed: ", sum(master$rent_imputed),  "ZIPs\n\n")

master %>%
  filter(rent_imputed) %>%
  select(zip_code, neighborhoods, price_2024, monthly_rent_final) %>% # <--- UPDATED HERE
  mutate(method = "0.5% rule") %>%
  print()

# ---- FINAL PRE-SCORING AUDIT ----
cat("=== COMPLETE DATA QUALITY AUDIT ===\n")
cat("Total ZIPs:             ", nrow(master), "\n")
cat("Bedroom reliable:      ", sum(master$bedroom_data_reliable, na.rm=TRUE), "\n")
cat("Bedroom flagged:       ", sum(!master$bedroom_data_reliable, na.rm=TRUE), "\n")
cat("Population flagged:    ", sum(master$low_pop_flag, na.rm=TRUE), "\n")
cat("Rent observed:         ", sum(!master$rent_imputed), "\n")
cat("Rent imputed:          ", sum(master$rent_imputed), "\n")
cat("ZIPs ready (≥65%):     ", sum(master$data_completeness >= 65), "\n")
cat("ZIPs full core data:   ", sum(master$data_completeness == 100), "\n")

write_csv(master, "data/processed/master_dataset_clean.csv")
cat("Clean master saved.\n\n")

# ============================================================
# ANNUALIZED PRICE CHANGES (CAGR)
# Convert cumulative returns to annual rates before blending
# CAGR = ((1 + total_return/100)^(1/years) - 1) * 100
# ============================================================

master <- master %>%
  mutate(
    # 1yr is already annualized — no conversion needed
    pct_change_1yr_ann = pct_change_1yr,
    
    # 3yr CAGR: compound annual growth rate over 3 years
    pct_change_3yr_ann = ((1 + pct_change_3yr / 100)^(1/3) - 1) * 100,
    
    # 5yr CAGR: compound annual growth rate over 5 years
    pct_change_5yr_ann = ((1 + pct_change_5yr / 100)^(1/5) - 1) * 100
  )

# Audit annualized values
cat("=== ANNUALIZED PRICE CHANGE AUDIT ===\n")
master %>%
  select(zip_code, 
         pct_change_1yr, pct_change_1yr_ann,
         pct_change_3yr, pct_change_3yr_ann,
         pct_change_5yr, pct_change_5yr_ann) %>%
  filter(!is.na(pct_change_3yr_ann)) %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(width = Inf)

cat("\nAnnualized 3yr range:", 
    round(min(master$pct_change_3yr_ann, na.rm=TRUE), 2), "to",
    round(max(master$pct_change_3yr_ann, na.rm=TRUE), 2), "%/yr\n")
cat("Annualized 5yr range:", 
    round(min(master$pct_change_5yr_ann, na.rm=TRUE), 2), "to",
    round(max(master$pct_change_5yr_ann, na.rm=TRUE), 2), "%/yr\n")

# ============================================================
# WINSORIZATION
# Cap extreme outliers at 95th percentile before normalization
# Prevents one outlier (55405: 148% permit growth) from
# compressing all other ZIPs into a narrow normalized range
# ============================================================

winsorize_95 <- function(x) {
  p95 <- quantile(x, 0.95, na.rm = TRUE)
  p05 <- quantile(x, 0.05, na.rm = TRUE)
  pmax(pmin(x, p95), p05)
}

master <- master %>%
  mutate(
    pct_change_1yr_w    = winsorize_95(pct_change_1yr_ann),
    pct_change_3yr_w    = winsorize_95(pct_change_3yr_ann),
    pct_change_5yr_w    = winsorize_95(pct_change_5yr_ann),
    permit_growth_pct_w = winsorize_95(permit_growth_pct),
    income_growth_pct_w = winsorize_95(income_growth_pct),
    pop_change_pct_w    = winsorize_95(pop_change_pct_clean)
  )

cat("\n=== WINSORIZATION AUDIT ===\n")
cat("Permit growth — original:", 
    round(min(master$permit_growth_pct, na.rm=TRUE), 1), "to",
    round(max(master$permit_growth_pct, na.rm=TRUE), 1), "\n")
cat("Permit growth — winsorized:", 
    round(min(master$permit_growth_pct_w, na.rm=TRUE), 1), "to",
    round(max(master$permit_growth_pct_w, na.rm=TRUE), 1), "\n")

# ============================================================
# NORMALIZATION
# Min-max scale each variable to 0-100
# reverse=TRUE flips scale (used for renter share:
# lower renter share = higher score)
# ============================================================

normalize_0_100 <- function(x, reverse = FALSE) {
  x_clean <- x[!is.na(x)]
  min_val  <- min(x_clean, na.rm = TRUE)
  max_val  <- max(x_clean, na.rm = TRUE)
  if (max_val == min_val) return(rep(50, length(x)))
  normalized <- (x - min_val) / (max_val - min_val) * 100
  if (reverse) normalized <- 100 - normalized
  return(normalized)
}

master_normalized <- master %>%
  mutate(
    
    # ---- BLENDED ANNUALIZED PRICE SCORE ----
    # All components now in same units (%/year) before blending
    # 5yr CAGR (25%): pre-COVID baseline, lowest distortion
    # 3yr CAGR (40%): medium trend through COVID recovery
    # 1yr CAGR (35%): current momentum signal
    price_blend_raw = (
      coalesce(pct_change_5yr_w, 0) * 0.25 +
        coalesce(pct_change_3yr_w, 0) * 0.40 +
        coalesce(pct_change_1yr_w, 0) * 0.35
    ),
    
    score_price_blend   = normalize_0_100(price_blend_raw),
    
    # Individual annualized scores (kept for reference)
    score_price_1yr_ann = normalize_0_100(pct_change_1yr_w),
    score_price_3yr_ann = normalize_0_100(pct_change_3yr_w),
    score_price_5yr_ann = normalize_0_100(pct_change_5yr_w),
    
    # ---- OTHER SCORING INPUTS ----
    score_income_growth = normalize_0_100(income_growth_pct_w),
    score_pop_change    = normalize_0_100(pop_change_pct_w),
    score_permit_growth = normalize_0_100(permit_growth_pct_w),
    score_permit_volume = normalize_0_100(recent_permit_count),
    score_rent_level    = normalize_0_100(monthly_rent_final),
    
    # Renter share REVERSED: lower renter share = higher score
    # Justified by regression: R²=0.541, p=0.0003
    score_pct_renter    = normalize_0_100(pct_renter_2023,
                                          reverse = TRUE)
  )

cat("=== NORMALIZED VARIABLE DISTRIBUTIONS ===\n")
master_normalized %>%
  select(starts_with("score_")) %>%
  summary() %>%
  print()

cat("\n=== SCORE COVERAGE (non-NA count) ===\n")
master_normalized %>%
  select(starts_with("score_")) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  print()

# ============================================================
# COMPOSITE INVESTMENT SCORE
# Weighted sum of normalized components
# Weights reflect regression-validated predictive power
# ZIPs below 65% data completeness excluded from scoring
# ============================================================

master_scored <- master_normalized %>%
  mutate(
    
    investment_score_raw = (
      coalesce(score_pct_renter,    0) * 0.40 +  # 30% → 40% (strongest predictor)
        coalesce(score_income_growth, 0) * 0.20 +  # 15% → 20%
        coalesce(score_permit_volume, 0) * 0.15 +  # 10% → 15%
        coalesce(score_permit_growth, 0) * 0.15 +  # 10% → 15%
        coalesce(score_rent_level,    0) * 0.05 +  # 5%  → 5%
        coalesce(score_pop_change,    0) * 0.05    # 5%  → 5%
    ),
    
    # Only score ZIPs with sufficient data
    investment_score = ifelse(
      data_completeness >= 65,
      investment_score_raw,
      NA_real_
    ),
    
    rank = ifelse(
      !is.na(investment_score),
      rank(-investment_score, ties.method = "min"),
      NA_integer_
    ),
    
    score_tier = case_when(
      investment_score >= 65 ~ "High Opportunity",
      investment_score >= 50 ~ "Moderate Opportunity",
      investment_score >= 35 ~ "Watch List",
      !is.na(investment_score) ~ "Lower Priority",
      TRUE ~ "Insufficient Data"
    ),
    
    is_mpls_city = zip_code %in% c(
      "55401","55402","55403","55404","55405","55406","55407","55408",
      "55409","55410","55411","55412","55413","55414","55415","55416",
      "55417","55418","55419"
    )
    
  ) %>%
  arrange(rank)

# ============================================================
# RESULTS AUDIT
# ============================================================

cat("=== SCORING RESULTS ===\n")
cat("Total scored:", sum(!is.na(master_scored$investment_score)), "\n\n")

cat("By tier:\n")
master_scored %>% count(score_tier) %>% print()

cat("\n=== SCORE DISTRIBUTION ===\n")
master_scored %>%
  filter(!is.na(investment_score)) %>%
  summarise(
    min    = round(min(investment_score), 1),
    p25    = round(quantile(investment_score, 0.25), 1),
    median = round(median(investment_score), 1),
    mean   = round(mean(investment_score), 1),
    p75    = round(quantile(investment_score, 0.75), 1),
    max    = round(max(investment_score), 1),
    n      = n()
  ) %>%
  print()

cat("\n=== TOP 10 NEIGHBORHOODS (ALL ZIPs) ===\n")
master_scored %>%
  filter(!is.na(investment_score)) %>%
  select(rank, zip_code, neighborhoods, investment_score, score_tier,
         price_2024, price_blend_raw, # <--- UPDATED HERE
         pct_change_1yr_ann, pct_change_3yr_ann, pct_change_5yr_ann) %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(width = Inf)

cat("\n=== TOP 10 MINNEAPOLIS CITY ZIPs ===\n")
master_scored %>%
  filter(!is.na(investment_score), is_mpls_city) %>%
  select(rank, zip_code, neighborhoods, investment_score, score_tier,
         price_2024, price_blend_raw, pct_change_1yr_ann) %>% # <--- UPDATED HERE
  head(10) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(width = Inf)

cat("\n=== BOTTOM 5 ===\n")
master_scored %>%
  filter(!is.na(investment_score)) %>%
  select(rank, zip_code, neighborhoods, investment_score,
         score_tier, price_2024) %>% # <--- UPDATED HERE
  tail(5) %>%
  print(width = Inf)

# ============================================================
# SAVE FINAL OUTPUT
# ============================================================

write_csv(master_scored, "data/processed/master_scored.csv")
cat("\nSaved master_scored.csv —", nrow(master_scored), "rows,",
    ncol(master_scored), "columns\n")
cat("Scored ZIPs:", sum(!is.na(master_scored$investment_score)), "\n")
cat("City ZIPs scored:", 
    sum(!is.na(master_scored$investment_score) & master_scored$is_mpls_city), "\n")

