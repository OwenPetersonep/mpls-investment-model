# ============================================================
# 01_data_cleaning.R
# Minneapolis Neighborhood Investment Model
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# Set working directory (Update if needed)
# setwd("/users/owenpeterson/downloads/mpls_investment_model")

# Minneapolis metro ZIP codes
mpls_zips <- c(
  "55401", "55402", "55403", "55404", "55405", "55406", "55407", "55408",
  "55409", "55410", "55411", "55412", "55413", "55414", "55415", "55416",
  "55417", "55418", "55419", "55420", "55421", "55422", "55423", "55424",
  "55425", "55426", "55427", "55428", "55429", "55430", "55431", "55432",
  "55433", "55434", "55435", "55436", "55437", "55438", "55439", "55440",
  "55441", "55442", "55443", "55444", "55445", "55446", "55447", "55448"
)

# ---- SECTION 1: ZILLOW MEDIAN SALE PRICE ----

# Load raw data
zillow_raw <- read_csv("data/raw/zillow_median_sale_price.csv")

# Filter to Minneapolis ZIPs and convert from wide to long format
zillow_long <- zillow_raw %>%
  clean_names() %>%
  filter(region_name %in% mpls_zips) %>%
  select(region_id, region_name, state_name, city, size_rank,
         starts_with("x")) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "date_raw",
    values_to = "median_sale_price"
  ) %>%
  mutate(
    date = date_raw %>%
      str_remove("^x") %>%
      str_replace_all("_", "-") %>%
      as.Date(),
    zip_code = region_name
  ) %>%
  select(-date_raw) %>%
  rename(zip_name = region_name) %>%
  filter(!is.na(median_sale_price))

# FIX: Cap the "most recent date" at March 2024 to get the 2024 price
most_recent_date <- max(zillow_long$date[zillow_long$date <= as.Date("2024-03-31")], na.rm = TRUE)
cat("Using as current price date:", as.character(most_recent_date), "\n")

# Pivot back wide for easier period comparison based on the capped date
zillow_metrics <- zillow_long %>%
  filter(date %in% c(
    most_recent_date,
    most_recent_date - years(1),
    most_recent_date - years(3),
    most_recent_date - years(5)
  )) %>%
  mutate(period = case_when(
    date == most_recent_date            ~ "current",
    date == most_recent_date - years(1) ~ "yr1_ago",
    date == most_recent_date - years(3) ~ "yr3_ago",
    date == most_recent_date - years(5) ~ "yr5_ago",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  select(zip_code, period, median_sale_price) %>%
  pivot_wider(names_from = period, values_from = median_sale_price) %>%
  mutate(
    pct_change_1yr  = (current - yr1_ago) / yr1_ago * 100,
    pct_change_3yr  = (current - yr3_ago) / yr3_ago * 100,
    pct_change_5yr  = (current - yr5_ago) / yr5_ago * 100,
    current_price   = current
  ) %>%
  select(zip_code, current_price, pct_change_1yr, pct_change_3yr, pct_change_5yr)

cat("ZIPs with complete price data:", nrow(zillow_metrics), "\n")
write_csv(zillow_metrics, "data/processed/zillow_metrics.csv")

# ---- SECTION 1B: ZILLOW RENT INDEX (ZORI) ----

zori_raw <- read_csv("data/raw/zillow_rent_index.csv")

zori_current <- zori_raw %>%
  clean_names() %>%
  filter(region_name %in% mpls_zips) %>%
  select(zip_code = region_name, starts_with("x")) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "date_raw",
    values_to = "monthly_rent"
  ) %>%
  mutate(
    date = date_raw %>%
      str_remove("^x") %>%
      str_replace_all("_", "-") %>%
      as.Date()
  ) %>%
  filter(!is.na(monthly_rent)) %>%
  # Optionally cap rent date to 2024 as well: 
  # filter(date <= as.Date("2024-03-31")) %>%
  group_by(zip_code) %>%
  slice_max(date, n = 1) %>%
  ungroup() %>%
  select(zip_code, monthly_rent_estimate = monthly_rent, rent_data_date = date)

cat("ZIPs with rent data:", nrow(zori_current), "\n")
write_csv(zori_current, "data/processed/zori_rent.csv")


# ---- SECTION 1C: ZHVI BY BEDROOM COUNT ----

get_current_zhvi <- function(filepath, bed_label) {
  read_csv(filepath, show_col_types = FALSE) %>%
    clean_names() %>%
    filter(region_name %in% mpls_zips) %>%
    select(zip_code = region_name, starts_with("x")) %>%
    pivot_longer(
      cols = starts_with("x"),
      names_to = "date_raw",
      values_to = "value"
    ) %>%
    mutate(
      date = date_raw %>%
        str_remove("^x") %>%
        str_replace_all("_", "-") %>%
        as.Date()
    ) %>%
    filter(!is.na(value)) %>%
    # Optionally cap bedroom date to 2024:
    # filter(date <= as.Date("2024-03-31")) %>%
    group_by(zip_code) %>%
    slice_max(date, n = 1) %>%
    ungroup() %>%
    select(zip_code, !!bed_label := value)
}

zhvi_1bed <- get_current_zhvi("data/raw/zillow_zhvi_1bed.csv", "price_1bed")
zhvi_2bed <- get_current_zhvi("data/raw/zillow_zhvi_2bed.csv", "price_2bed")
zhvi_3bed <- get_current_zhvi("data/raw/zillow_zhvi_3bed.csv", "price_3bed")
zhvi_4bed <- get_current_zhvi("data/raw/zillow_zhvi_4bed.csv", "price_4bed")
zhvi_5bed <- get_current_zhvi("data/raw/zillow_zhvi_5bed.csv", "price_5bed")

zhvi_by_bedroom <- zhvi_1bed %>%
  full_join(zhvi_2bed, by = "zip_code") %>%
  full_join(zhvi_3bed, by = "zip_code") %>%
  full_join(zhvi_4bed, by = "zip_code") %>%
  full_join(zhvi_5bed, by = "zip_code") %>%
  mutate(
    zip_code = as.character(zip_code),
    bed_price_spread   = coalesce(price_5bed, price_4bed, price_3bed) - price_1bed,
    price_2bed_premium = (price_2bed - price_1bed) / price_1bed * 100,
    price_3bed_premium = (price_3bed - price_2bed) / price_2bed * 100
  )

cat("ZIPs with bedroom data:", nrow(zhvi_by_bedroom), "\n")
write_csv(zhvi_by_bedroom, "data/processed/zhvi_by_bedroom.csv")


# ---- SECTION 2: CENSUS ACS DATA ----

acs_vars <- c(
  median_income     = "B19013_001",
  total_population  = "B01003_001",
  median_home_value = "B25077_001",
  unemployed        = "B23025_005",
  labor_force       = "B23025_002",
  median_rooms      = "B25018_001",
  renter_occupied   = "B25003_003",
  total_occupied    = "B25003_001"
)

acs_2023 <- get_acs(
  geography = "tract", variables = acs_vars,
  state = "MN", county = "Hennepin",
  year = 2023, survey = "acs5", geometry = FALSE
)

acs_2018 <- get_acs(
  geography = "tract", variables = acs_vars,
  state = "MN", county = "Hennepin",
  year = 2018, survey = "acs5", geometry = FALSE
)

tidy_acs <- function(df, year_label) {
  df %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    clean_names() %>%
    mutate(
      year              = year_label,
      unemployment_rate = unemployed / labor_force * 100,
      pct_renter        = renter_occupied / total_occupied * 100
    ) %>%
    select(-unemployed, -labor_force, -renter_occupied, -total_occupied)
}

acs_2023_wide <- tidy_acs(acs_2023, 2023)
acs_2018_wide <- tidy_acs(acs_2018, 2018)

acs_joined <- acs_2023_wide %>%
  left_join(acs_2018_wide, by = "geoid", suffix = c("_23", "_18"))

acs_growth <- acs_joined %>%
  mutate(
    income_growth_pct = (median_income_23 - median_income_18) / median_income_18 * 100,
    pop_change_pct    = (total_population_23 - total_population_18) / total_population_18 * 100,
    income_2023       = median_income_23,
    population_2023   = total_population_23,
    low_pop_flag      = total_population_23 < 500,
    pct_renter_2023   = pct_renter_23,
    median_rooms_2023 = median_rooms_23
  ) %>%
  select(geoid, name = name_23, income_2023, population_2023,
         income_growth_pct, pop_change_pct, low_pop_flag,
         pct_renter_2023, median_rooms_2023)

write_csv(acs_growth, "data/processed/census_metrics.csv")

# HUD USPS Crosswalk
crosswalk_raw <- read_excel("data/raw/ZIP_TRACT_122022.xlsx")

crosswalk <- crosswalk_raw %>%
  clean_names() %>%
  filter(str_sub(zip, 1, 3) %in% c("554")) %>%
  select(zip, tract, res_ratio) %>%
  mutate(geoid = str_pad(tract, 11, "left", "0"))

census_by_zip <- acs_growth %>%
  left_join(crosswalk, by = "geoid") %>%
  filter(!is.na(zip)) %>%
  group_by(zip) %>%
  summarise(
    income_2023       = sum(income_2023 * res_ratio, na.rm=TRUE) / sum(res_ratio, na.rm=TRUE),
    income_growth_pct = sum(income_growth_pct * res_ratio, na.rm=TRUE) / sum(res_ratio, na.rm=TRUE),
    pop_change_pct    = sum(pop_change_pct * res_ratio, na.rm=TRUE) / sum(res_ratio, na.rm=TRUE),
    population_2023   = sum(population_2023 * res_ratio, na.rm=TRUE),
    pct_renter_2023   = sum(pct_renter_2023 * res_ratio, na.rm=TRUE) / sum(res_ratio, na.rm=TRUE),
    median_rooms_2023 = sum(median_rooms_2023 * res_ratio, na.rm=TRUE) / sum(res_ratio, na.rm=TRUE)
  ) %>%
  rename(zip_code = zip)

write_csv(census_by_zip, "data/processed/census_by_zip.csv")

# ---- SECTION 3: BUILDING PERMITS ----

permits_raw <- read_csv("data/raw/building_permits.csv")

permits_clean <- permits_raw %>% clean_names()

permits_filtered <- permits_clean %>%
  mutate(
    issue_date = ymd_hms(issue_date),
    issue_year = year(issue_date)
  ) %>%
  filter(issue_year >= 2018, issue_year <= 2024) %>%
  filter(!str_detect(tolower(permit_type), "demo|demolish|wreck")) %>%
  filter(value >= 5000 | is.na(value)) %>%
  filter(!is.na(neighborhoods_desc))

permits_by_neighborhood <- permits_filtered %>%
  group_by(neighborhood = neighborhoods_desc, issue_year) %>%
  summarise(
    permit_count = n(),
    total_value  = sum(value, na.rm = TRUE),
    avg_value    = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

permit_trend <- permits_by_neighborhood %>%
  mutate(period = case_when(
    issue_year %in% 2018:2019 ~ "early",
    issue_year %in% 2022:2024 ~ "recent",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(neighborhood, period) %>%
  summarise(
    avg_permits = mean(permit_count),
    avg_value   = mean(total_value),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = period, values_from = c(avg_permits, avg_value)) %>%
  mutate(
    permit_growth_pct = (avg_permits_recent - avg_permits_early) / avg_permits_early * 100,
    value_growth_pct  = (avg_value_recent - avg_value_early) / avg_value_early * 100,
    recent_permit_count = avg_permits_recent
  ) %>%
  select(neighborhood, permit_growth_pct, value_growth_pct, recent_permit_count)

write_csv(permit_trend, "data/processed/permit_metrics.csv")

# ---- SECTION 4: NEIGHBORHOOD-TO-ZIP CROSSWALK ----

neighborhoods_sf <- st_read("data/raw/minneapolis_neighborhoods.geojson")

mpls_zips_sf <- zctas(year = 2020) %>%
  filter(GEOID20 %in% mpls_zips)

neighborhoods_sf <- st_transform(neighborhoods_sf, 4326)
mpls_zips_sf     <- st_transform(mpls_zips_sf, 4326)

neighborhood_centroids <- st_centroid(neighborhoods_sf)

nbhd_zip_crosswalk <- st_join(
  neighborhood_centroids,
  mpls_zips_sf %>% select(zip_code = GEOID20),
  join = st_within
) %>%
  st_drop_geometry() %>%
  select(neighborhood = BDNAME, zip_code) %>%
  filter(!is.na(zip_code))

write_csv(nbhd_zip_crosswalk, "data/processed/nbhd_zip_crosswalk.csv")


# ---- SECTION 5: MASTER DATASET JOIN ----

# Join permits to ZIP codes via neighborhood crosswalk
permits_by_zip <- permit_trend %>%
  left_join(nbhd_zip_crosswalk, by = "neighborhood") %>%
  filter(!is.na(zip_code)) %>%
  group_by(zip_code) %>%
  summarise(
    permit_growth_pct   = mean(permit_growth_pct, na.rm = TRUE),
    value_growth_pct    = mean(value_growth_pct, na.rm = TRUE),
    recent_permit_count = sum(recent_permit_count, na.rm = TRUE)
  )

# Standardize all zip_code columns to character
zillow_metrics  <- zillow_metrics  %>% mutate(zip_code = as.character(zip_code))
census_by_zip   <- census_by_zip   %>% mutate(zip_code = as.character(zip_code))
permits_by_zip  <- permits_by_zip  %>% mutate(zip_code = as.character(zip_code))
zori_current    <- zori_current    %>% mutate(zip_code = as.character(zip_code))
zhvi_by_bedroom <- zhvi_by_bedroom %>% mutate(zip_code = as.character(zip_code))
nbhd_crosswalk  <- nbhd_zip_crosswalk %>% mutate(zip_code = as.character(zip_code))

# Master join — all five sources
master_raw <- zillow_metrics %>%
  left_join(census_by_zip,   by = "zip_code") %>%
  left_join(permits_by_zip,  by = "zip_code") %>%
  left_join(zori_current,    by = "zip_code") %>%
  left_join(zhvi_by_bedroom, by = "zip_code")

# Completeness flags
master_raw <- master_raw %>%
  mutate(
    has_price_data   = !is.na(current_price),
    has_census_data  = !is.na(income_2023),
    has_permit_data  = !is.na(permit_growth_pct),
    has_rent_data    = !is.na(monthly_rent_estimate),
    has_bedroom_data = !is.na(price_2bed),
    data_completeness = (has_price_data + has_census_data + has_permit_data) / 3 * 100
  )

# Add neighborhood labels
master_with_labels <- master_raw %>%
  left_join(
    nbhd_crosswalk %>%
      group_by(zip_code) %>%
      summarise(neighborhoods = paste(neighborhood, collapse = " / ")),
    by = "zip_code"
  )

# Final audit
cat("=== FINAL MASTER AUDIT ===\n")
cat("Total ZIPs:", nrow(master_with_labels), "\n")
cat("Columns:", ncol(master_with_labels), "\n")
cat("ZIPs with price data:", sum(!is.na(master_with_labels$current_price)), "\n")
cat("ZIPs with census data:", sum(!is.na(master_with_labels$income_2023)), "\n")
cat("ZIPs with permit data:", sum(!is.na(master_with_labels$permit_growth_pct)), "\n")
cat("ZIPs with rent data:", sum(!is.na(master_with_labels$monthly_rent_estimate)), "\n")
cat("ZIPs with bedroom data:", sum(!is.na(master_with_labels$price_2bed)), "\n")
cat("ZIPs with full core data:", sum(master_with_labels$data_completeness == 100), "\n")

write_csv(master_with_labels, "data/processed/master_dataset.csv")
cat("Master dataset saved.\n")

