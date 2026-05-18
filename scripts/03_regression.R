# ============================================================
# 03_regression.R
# Minneapolis Neighborhood Investment Model
# Regression Analysis — Annualized CAGR as Dependent Variable
# ============================================================
#
# DEPENDENT VARIABLE: price_blend_raw
#   Weighted average of annualized (CAGR) price appreciation:
#   = 5yr_CAGR(25%) + 3yr_CAGR(40%) + 1yr(35%)
#   All components in same units (%/year) before blending
#   Units: percentage points per year (%/yr)
#
# INDEPENDENT VARIABLES:
#   x1 = income_growth_pct  — Census 2018→2023
#   x2 = pop_change_pct     — Census 2018→2023
#   x3 = permit_growth_pct  — pre-COVID baseline vs recent
#   x4 = monthly_rent_final — ZORI observed or imputed
#   x5 = pct_renter_2023    — Census 2023 ACS
#   x6 = price_2024      — Zillow ZHVI March 2024
#
# SAMPLE: n≈19 Minneapolis ZIP codes with complete data
# ============================================================

library(tidyverse)
library(broom)
library(corrplot)
library(ggplot2)

setwd("/Users/owenpeterson/Downloads/mpls_investment_model")

scored <- read_csv("data/processed/master_scored.csv") %>%
  mutate(zip_code = as.character(zip_code))

cat("Loaded:", nrow(scored), "rows\n")
cat("price_blend_raw available:", sum(!is.na(scored$price_blend_raw)), "ZIPs\n\n")

dir.create("data/outputs", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# CORRELATION MATRIX
# ============================================================

cor_data <- scored %>%
  select(
    "Blended CAGR (%/yr)" = price_blend_raw,
    "1yr CAGR"            = pct_change_1yr_ann,
    "3yr CAGR"            = pct_change_3yr_ann,
    "Income Growth"        = income_growth_pct,
    "Pop Change"           = pop_change_pct,
    "Permit Growth"        = permit_growth_pct,
    "Rent Level"           = monthly_rent_final,
    "Renter Share"         = pct_renter_2023,
    "2024 Price"        = price_2024
  ) %>%
  filter(complete.cases(.))

cat("Observations for correlation:", nrow(cor_data), "\n")

cor_matrix <- cor(cor_data)
print(round(cor_matrix, 2))

png("data/outputs/correlation_matrix.png", width=900, height=900)
corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  addCoef.col = "black",
  number.cex  = 0.75,
  tl.cex      = 0.85,
  title       = "Minneapolis Investment Variables — Correlation Matrix",
  mar         = c(0, 0, 2, 0)
)
dev.off()
cat("Correlation matrix saved.\n\n")

# ============================================================
# REGRESSION DATA PREPARATION
# ============================================================

reg_data <- scored %>%
  select(
    zip_code,
    y  = price_blend_raw,     # blended annualized CAGR (%/yr)
    x1 = income_growth_pct,
    x2 = pop_change_pct,
    x3 = permit_growth_pct,
    x4 = monthly_rent_final,
    x5 = pct_renter_2023,
    x6 = price_2024
  ) %>%
  filter(complete.cases(.))

cat("=== REGRESSION SAMPLE ===\n")
cat("n =", nrow(reg_data), "complete ZIP codes\n")
cat("Dependent variable: Blended annualized CAGR (%/yr)\n")
cat("Range:", round(min(reg_data$y), 2), "to", round(max(reg_data$y), 2), "%/yr\n\n")

if (nrow(reg_data) < 15) {
  cat("WARNING: n <15 — results are directional only.\n\n")
}

# ============================================================
# FULL REGRESSION MODEL
# ============================================================

model_full <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = reg_data)

model_results <- tidy(model_full) %>%
  mutate(
    variable = case_when(
      term == "(Intercept)" ~ "Intercept (baseline)",
      term == "x1"          ~ "Income Growth %",
      term == "x2"          ~ "Population Change %",
      term == "x3"          ~ "Permit Growth %",
      term == "x4"          ~ "Monthly Rent ($)",
      term == "x5"          ~ "Renter Share %",
      term == "x6"          ~ "2024 Price ($)",
      TRUE                  ~ term
    ),
    sig_05pct = p.value < 0.05,
    sig_10pct = p.value < 0.10
  ) %>%
  select(variable, estimate, std.error, p.value, sig_05pct, sig_10pct)

cat("=== FULL MODEL RESULTS ===\n")
print(model_results, digits = 3)

model_fit <- glance(model_full)
cat("\n=== MODEL FIT ===\n")
cat("R-squared:          ", round(model_fit$r.squared, 3), "\n")
cat("Adjusted R-squared: ", round(model_fit$adj.r.squared, 3), "\n")
cat("F-statistic p-value:", round(model_fit$p.value, 4), "\n")
cat("\nInterpretation: The six variables explain",
    round(model_fit$r.squared * 100, 1),
    "% of variation in blended annualized price appreciation.\n\n")

# ============================================================
# SINGLE VARIABLE MODELS
# ============================================================

single_models <- list(
  "Income Growth"     = lm(y ~ x1, data = reg_data),
  "Population Change" = lm(y ~ x2, data = reg_data),
  "Permit Growth"     = lm(y ~ x3, data = reg_data),
  "Rent Level"        = lm(y ~ x4, data = reg_data),
  "Renter Share"      = lm(y ~ x5, data = reg_data),
  "2024 Price"     = lm(y ~ x6, data = reg_data)
)

cat("=== SINGLE VARIABLE R-SQUARED ===\n")
cat("Predicting: blended annualized CAGR (%/yr)\n\n")

single_results <- map_df(names(single_models), function(name) {
  m <- single_models[[name]]
  tibble(
    variable  = name,
    r_squared = round(glance(m)$r.squared, 3),
    p_value   = round(tidy(m)$p.value[2], 4),
    direction = ifelse(tidy(m)$estimate[2] > 0, "positive", "negative")
  )
}) %>%
  arrange(desc(r_squared))

print(single_results)

cat("\nTop predictor:", single_results$variable[1], "\n")
cat("R² =",           single_results$r_squared[1], "\n")
cat("p-value =",      single_results$p_value[1], "\n")
cat("Direction:",     single_results$direction[1], "\n\n")

# ============================================================
# SCATTER PLOT — TOP PREDICTOR vs BLENDED CAGR
# ============================================================

plot_data <- reg_data %>%
  left_join(scored %>% select(zip_code, neighborhoods), by = "zip_code")

p <- ggplot(plot_data, aes(x = x5, y = y)) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#E84855", fill = "#E84855", alpha = 0.15) +
  geom_text(aes(label = zip_code), vjust = -0.8,
            size = 2.8, color = "gray40") +
  annotate("text",
           x     = max(plot_data$x5, na.rm=TRUE) * 0.92,
           y     = min(plot_data$y,  na.rm=TRUE) * 1.05,
           label = "55403: Loring Park\n(anomalous decline,\nsee methodology)",
           size  = 2.5, color = "gray40", hjust = 1) +
  labs(
    title    = "Renter Share % vs. Blended Annualized Price Appreciation",
    subtitle = paste("Minneapolis ZIP Codes | R² =",
                     single_results$r_squared[1],
                     "| n =", nrow(reg_data), "ZIPs"),
    x        = "Renter Share % (Census 2023)",
    y        = "Blended Annualized CAGR (%/yr)",
    caption  = paste(
      "Data: Zillow Research (March 2024), US Census Bureau ACS 2023 |",
      "n =", nrow(reg_data), "Minneapolis ZIP codes |",
      "Dependent variable: weighted avg of 1yr, 3yr CAGR, 5yr CAGR"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50"),
    plot.caption  = element_text(color = "gray60", size = 8)
  )

ggsave("data/outputs/top_predictor_scatter.png",
       plot = p, width = 9, height = 6, dpi = 150)
cat("Scatter plot saved.\n\n")

# ============================================================
# SAVE ALL REGRESSION OUTPUTS
# ============================================================

write_csv(model_results,  "data/outputs/regression_results.csv")
write_csv(model_fit,      "data/outputs/regression_fit.csv")
write_csv(single_results, "data/outputs/single_variable_results.csv")

# Save regression_results.csv for Tableau
regression_tableau <- tibble(
  Variable    = single_results$variable,
  R_Squared   = single_results$r_squared,
  Significant = ifelse(single_results$p_value < 0.05, "Yes", "No"),
  Direction   = single_results$direction,
  P_Value     = single_results$p_value
)

write_csv(regression_tableau, "data/outputs/regression_tableau.csv")

cat("=== ALL FILES SAVED ===\n")
cat("data/outputs/regression_results.csv\n")
cat("data/outputs/regression_fit.csv\n")
cat("data/outputs/single_variable_results.csv\n")
cat("data/outputs/regression_tableau.csv\n")
cat("data/outputs/top_predictor_scatter.png\n")
cat("data/outputs/correlation_matrix.png\n\n")

cat("=== FINAL SUMMARY ===\n")
cat("Dependent variable: Blended annualized CAGR (%/yr)\n")
cat("Full model R²:", round(model_fit$r.squared, 3), "\n")
cat("Full model adj R²:", round(model_fit$adj.r.squared, 3), "\n")
cat("Top predictor:", single_results$variable[1],
    "— R²:", single_results$r_squared[1],
    "— p:", single_results$p_value[1], "\n")
cat("n =", nrow(reg_data), "complete ZIP codes\n")

