# ============================================================
# 04_validation.R
# Out-of-sample validation test
# Question: Did higher-scored ZIPs appreciate more 
# from March 2024 → most recent Zillow data?
# ============================================================

library(tidyverse)
library(broom)
library(ggplot2)

setwd("/Users/owenpeterson/Downloads/mpls_investment_model")

# ============================================================
# LOAD DATA
# ============================================================

# Your scored model results (built on March 2024 data)
scored <- read_csv("data/processed/master_scored.csv") %>%
  mutate(zip_code = as.character(zip_code) %>% str_pad(5, pad="0"))

# Fresh Zillow download with 2025/2026 data
zillow_new <- read_csv("data/raw/zillow_zhvi_updated.csv")

cat("Scored ZIPs in model:", nrow(scored), "\n")
cat("Zillow columns:", ncol(zillow_new), "\n")

# ============================================================
# FIND DATE COLUMNS
# ============================================================

date_cols <- names(zillow_new)[grepl("^\\d{4}-\\d{2}-\\d{2}", names(zillow_new))]
cat("Date range:", head(date_cols, 1), "to", tail(date_cols, 1), "\n")

# Your model baseline
baseline_col <- "2024-03-31"  # adjust if your file uses different format

# Most recent available date
latest_col <- tail(date_cols, 1)
cat("Baseline:", baseline_col, "\n")
cat("Latest:  ", latest_col, "\n")

# Calculate months elapsed
months_elapsed <- as.numeric(
  as.Date(latest_col) - as.Date(baseline_col)
) / 30.44
cat("Months elapsed:", round(months_elapsed, 1), "\n\n")

# ============================================================
# PREP ZILLOW LATEST PRICES
# ============================================================

# Only pull the latest price so we don't collide with your model's 2024 price
zillow_validation <- zillow_new %>%
  filter(RegionType == "zip") %>%
  mutate(
    zip_code = str_pad(as.character(RegionName), 5, pad="0")
  ) %>%
  select(
    zip_code,
    price_latest = all_of(latest_col)
  ) %>%
  filter(!is.na(price_latest))

# ============================================================
# JOIN & CALCULATE POST-MODEL APPRECIATION
# ============================================================

validation_data <- scored %>%
  filter(!is.na(investment_score)) %>%
  inner_join(zillow_validation, by = "zip_code") %>%
  # Calculate appreciation using the price_2024 from your model!
  mutate(
    # Raw cumulative appreciation since model baseline
    pct_change_post_model = (price_latest - price_2024) / price_2024 * 100,
    
    # Annualized CAGR for fair comparison
    cagr_post_model = ((1 + pct_change_post_model/100)^(12/months_elapsed) - 1) * 100
  ) %>%
  filter(!is.na(pct_change_post_model))

cat("ZIPs in validation sample:", nrow(validation_data), "\n\n")

# Preview
validation_data %>%
  select(zip_code, neighborhoods, investment_score, score_tier,
         price_2024, price_latest, 
         pct_change_post_model, cagr_post_model) %>%
  arrange(desc(investment_score)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(width = Inf)

# ============================================================
# VALIDATION REGRESSION
# Key question: does investment_score predict post-model appreciation?
# ============================================================

val_model <- lm(pct_change_post_model ~ investment_score, 
                data = validation_data)
val_fit   <- glance(val_model)
val_coef  <- tidy(val_model)

cat("=== VALIDATION REGRESSION ===\n")
cat("Dependent: % appreciation March 2024 →", latest_col, "\n")
cat("Independent: Investment score (built on March 2024 data)\n\n")
cat("R²:        ", round(val_fit$r.squared, 3), "\n")
cat("p-value:  ", round(val_fit$p.value, 4), "\n")
cat("n =", nrow(validation_data), "ZIP codes\n\n")

slope <- val_coef$estimate[2]
cat("Slope:", round(slope, 3), "\n")
cat("Direction:", ifelse(slope > 0, "POSITIVE ✅", "NEGATIVE ❌"), "\n")
cat("Meaning: Each 10-point score increase predicts",
    round(slope * 10, 2), "% more appreciation\n\n")

# Spearman rank correlation (more robust with small n)
rank_cor <- cor(
  validation_data$investment_score,
  validation_data$pct_change_post_model,
  method = "spearman"
)
cat("Spearman rank correlation:", round(rank_cor, 3), "\n")
cat("(Positive = higher scores → more appreciation ✅)\n\n")

# ============================================================
# TIER PERFORMANCE SUMMARY
# Did High Opportunity ZIPs outperform Watch List?
# ============================================================

cat("=== PERFORMANCE BY TIER ===\n")
tier_summary <- validation_data %>%
  group_by(score_tier) %>%
  summarise(
    n              = n(),
    avg_score      = round(mean(investment_score), 1),
    avg_return_pct = round(mean(pct_change_post_model), 2),
    median_return  = round(median(pct_change_post_model), 2),
    best_zip       = zip_code[which.max(pct_change_post_model)],
    worst_zip      = zip_code[which.min(pct_change_post_model)],
    pct_positive   = paste0(round(mean(pct_change_post_model > 0)*100), "%")
  ) %>%
  arrange(desc(avg_score))

print(tier_summary, width = Inf)

# ============================================================
# MINNEAPOLIS CITY ONLY
# ============================================================

cat("\n=== MINNEAPOLIS CITY ZIPs ONLY ===\n")
validation_data %>%
  filter(is_mpls_city) %>%
  select(rank, zip_code, neighborhoods, investment_score,
         score_tier, pct_change_post_model) %>%
  arrange(desc(pct_change_post_model)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(width = Inf)

# Did model rank correlate with actual performance?
city_data <- validation_data %>% filter(is_mpls_city)
city_cor <- cor(
  city_data$investment_score,
  city_data$pct_change_post_model,
  method = "spearman"
)
cat("\nCity-only Spearman correlation:", round(city_cor, 3), "\n")

# ============================================================
# VISUALIZATION
# ============================================================

p <- ggplot(validation_data,
            aes(x     = investment_score,
                y     = pct_change_post_model,
                color = score_tier,
                label = zip_code)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#E84855", fill = "#E84855",
              alpha = 0.15) +
  geom_text(vjust = -0.8, size = 2.8, color = "gray40") +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "gray60") +
  scale_color_manual(values = c(
    "High Opportunity"     = "#27AE60",
    "Moderate Opportunity" = "#F39C12",
    "Watch List"           = "#E67E22",
    "Lower Priority"       = "#E74C3C",
    "Insufficient Data"    = "#BDC3C7"
  )) +
  labs(
    title    = "Model Validation: Investment Score vs Actual Appreciation",
    subtitle = paste0(
      "Minneapolis ZIP Codes | ",
      "Score built: March 2024 | ",
      "Validation period: March 2024 → ", latest_col, " (",
      round(months_elapsed, 0), " months) | ",
      "R² = ", round(val_fit$r.squared, 3),
      ", p = ", round(val_fit$p.value, 4)
    ),
    x       = "Investment Score (0-100) — built March 2024",
    y       = paste0("Actual Price Appreciation (%)\n",
                     "March 2024 → ", latest_col),
    color   = "Score Tier",
    caption = paste0(
      "n = ", nrow(validation_data), " ZIP codes | ",
      "Spearman r = ", round(rank_cor, 3)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50", size = 9),
    plot.caption  = element_text(color = "gray60", size = 8)
  )

ggsave("data/outputs/model_validation.png",
       plot = p, width = 10, height = 6, dpi = 150)
cat("\nValidation chart saved to data/outputs/model_validation.png\n")

# ============================================================
# SAVE VALIDATION RESULTS
# ============================================================

write_csv(
  validation_data %>%
    select(zip_code, neighborhoods, investment_score, score_tier,
           rank, is_mpls_city, price_2024, price_latest,
           pct_change_post_model, cagr_post_model),
  "data/outputs/validation_results.csv"
)

write_csv(tier_summary, "data/outputs/validation_by_tier.csv")

cat("Validation results saved to data/outputs/\n\n")

# ============================================================
# FINAL SUMMARY FOR METHODOLOGY NOTES
# ============================================================

cat("=== COPY THIS INTO YOUR METHODOLOGY NOTES ===\n\n")
cat("OUT-OF-SAMPLE VALIDATION\n")
cat("Validation period:", baseline_col, "→", latest_col, "\n")
cat("Months elapsed:", round(months_elapsed, 1), "\n")
cat("n =", nrow(validation_data), "ZIP codes\n")
cat("Validation R²:", round(val_fit$r.squared, 3), "\n")
cat("p-value:", round(val_fit$p.value, 4), "\n")
cat("Spearman rank correlation:", round(rank_cor, 3), "\n")
cat("High Opportunity avg return:",
    round(mean(validation_data$pct_change_post_model[
      validation_data$score_tier == "High Opportunity"
    ], na.rm=TRUE), 2), "%\n")
cat("Watch List avg return:",
    round(mean(validation_data$pct_change_post_model[
      validation_data$score_tier == "Watch List"
    ], na.rm=TRUE), 2), "%\n")

