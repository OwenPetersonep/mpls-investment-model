# ============================================================
# 01_data_cleaning.R
# Minneapolis Neighborhood Investment Model
# Author: [Your Name]
# Last updated: [Today's date]
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)

# ---- SECTION 1: ZILLOW MEDIAN SALE PRICE ----

# Load raw data
zillow_raw <- read_csv("data/raw/zillow_median_sale_price.csv")
getwd()
setwd("/users/owenpeterson/downloads/mpls_investment_model")
# Inspect structure
glimpse(zillow_raw)
names(zillow_raw)[1:10]  # First 10 column names

# Minneapolis metro ZIP codes
# These are Hennepin County + adjacent ZIPs covering Minneapolis proper
# Source: USPS ZIP code lookup + manual verification
mpls_zips <- c(
  "55401", "55402", "55403", "55404", "55405", "55406", "55407", "55408",
  "55409", "55410", "55411", "55412", "55413", "55414", "55415", "55416",
  "55417", "55418", "55419", "55420", "55421", "55422", "55423", "55424",
  "55425", "55426", "55427", "55428", "55429", "55430", "55431", "55432",
  "55433", "55434", "55435", "55436", "55437", "55438", "55439", "55440",
  "55441", "55442", "55443", "55444", "55445", "55446", "55447", "55448"
)

# Filter to Minneapolis ZIPs and convert from wide to long format
# The Zillow file has one column per date — we pivot to tidy format
zillow_long <- zillow_raw %>%
  clean_names() %>%                        # NOW all columns are snake_case
  filter(region_name %in% mpls_zips) %>%   # use snake_case from here down
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
    zip_code = region_name                 # copy before dropping
  ) %>%
  select(-date_raw) %>%                    # only drop date_raw, keep region_name for now
  rename(zip_name = region_name) %>%       # NOW rename works because column still exists
  filter(!is.na(median_sale_price))
# METHODOLOGY NOTE: Removing NA price months.
# Some ZIP codes have sparse data pre-2015.
# We'll only use 2019-2024 for trend calculation to get consistent coverage.

# Calculate price appreciation metrics
# We want: % change over trailing 1-year, 3-year, and 5-year periods
# Using the most recent available month as "current"

most_recent_date <- max(zillow_long$date)
cat("Most recent Zillow data:", as.character(most_recent_date), "\n")

# Pivot back wide for easier period comparison
zillow_metrics <- zillow_long %>%
  filter(date %in% c(
    most_recent_date,
    most_recent_date - years(1),
    most_recent_date - years(3),
    most_recent_date - years(5)
  )) %>%
  mutate(period = case_when(
    date == most_recent_date          ~ "current",
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

# IMPORTANT: Check how many ZIPs survived
cat("ZIPs with complete price data:", nrow(zillow_metrics), "\n")
cat("Expected: 20-40. If fewer than 15, check your mpls_zips vector.\n")

# Save intermediate output
write_csv(zillow_metrics, "data/processed/zillow_metrics.csv")
