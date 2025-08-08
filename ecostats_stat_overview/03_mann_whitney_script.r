# Mann-Whitney-Wilcoxon Test Analysis: Slimy Sculpin Fish Length Comparison
# Author: Bill Perry
# Purpose: Compare total length of slimy sculpin fish between two lakes using non-parametric test
# Note: Mann-Whitney-Wilcoxon test is a non-parametric alternative to t-test

# ============================================================================
# LOAD LIBRARIES
# ============================================================================

library(tidyverse)   # For data manipulation and visualization
library(car)         # For Levene's test
library(coin)        # For permutation tests and exact Mann-Whitney test
library(skimr)       # For data summary statistics
library(rcompanion)  # For plotNormalHistogram

# ============================================================================
# LOAD AND PREVIEW DATA
# ============================================================================

# Load the sculpin dataset
sculpin_df <- read_csv("data/t_test_sculpin_s07_ne14.csv")

# Preview the first few rows
head(sculpin_df)

# ============================================================================
# DATA OVERVIEW AND SUMMARY STATISTICS
# ============================================================================

# Generate summary statistics grouped by lake
sculpin_df %>% 
  group_by(lake) %>% 
  skim()

# ============================================================================
# DATA VISUALIZATION
# ============================================================================

# Create boxplot with individual data points
ggplot(sculpin_df, aes(x = lake, y = length_mm, fill = lake)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 2) +
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# Create mean and standard error plot with individual data points
sculpin_df %>% 
  ggplot(aes(x = lake, y = length_mm, color = lake)) +
  # Add individual data points in the background
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 1.5) +
  # Add mean and standard error
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    color = "Lake"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# ============================================================================
# ASSUMPTION TESTING: NORMALITY (for comparison with parametric tests)
# ============================================================================

# Create histograms by lake to assess normality
sculpin_df %>% 
  ggplot(aes(length_mm, fill = lake)) +
  geom_histogram() +
  facet_wrap(~lake)

# Create Q-Q plots to assess normality
sculpin_df %>%  
  ggplot(aes(sample = length_mm, color = lake)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~lake, scales = "free") +
  labs(title = "Normal Q-Q Plots by Lake",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Shapiro-Wilk test for normality - Lake S 07
sculpin_df %>%
  filter(lake == "S 07") %>%
  pull(length_mm) %>%
  shapiro.test()

# Shapiro-Wilk test for normality - Lake NE 14
sculpin_df %>%
  filter(lake == "NE 14") %>%
  pull(length_mm) %>%
  shapiro.test()

# Alternative approach: Run Shapiro-Wilk tests for both lakes with formatted output
sculpin_df %>%
  group_by(lake) %>%
  group_walk(~ {
    cat("Shapiro-Wilk test for Lake", .y$lake, ":\n")
    test_result <- shapiro.test(.x$length_mm)
    print(test_result)
    cat("\n")
  })

# ============================================================================
# MANN-WHITNEY-WILCOXON TEST (BASE R)
# ============================================================================

# Perform the Mann-Whitney-Wilcoxon test using base R
wilcox_test <- wilcox.test(length_mm ~ lake, 
                          data = sculpin_df,
                          exact = FALSE,  # Use approximate method for larger samples
                          correct = TRUE)  # Apply continuity correction

# Display the results
wilcox_test

# Store the p-value for later use
p_value <- wilcox_test$p.value

# ============================================================================
# MANN-WHITNEY-WILCOXON TEST (COIN PACKAGE - MORE PRECISE)
# ============================================================================

# Convert lake to factor (required for the coin package)
sculpin_df$lake_factor <- factor(sculpin_df$lake)

# Perform the Mann-Whitney test using the coin package with approximate method
coin_wilcox <- coin::wilcox_test(
  length_mm ~ lake_factor,
  data = sculpin_df,
  distribution = "approximate"
)

# Display results
coin_wilcox

# Extract the p-value from coin package results
pvalue_coin <- pvalue(coin_wilcox)

# ============================================================================
# EFFECT SIZE CALCULATION
# ============================================================================

# Calculate standardized effect size using rank-biserial correlation
# (equivalent to r = Z / sqrt(N))
z_score <- qnorm(p_value/2)  # Convert p-value to Z-score
N <- nrow(sculpin_df)
r <- abs(z_score) / sqrt(N)  # Rank-biserial correlation

# Interpret effect size according to Cohen's conventions
effect_size <- r
if(effect_size < 0.1) {
  effect_interpretation <- "negligible effect"
} else if(effect_size < 0.3) {
  effect_interpretation <- "small effect"
} else if(effect_size < 0.5) {
  effect_interpretation <- "moderate effect"
} else if(effect_size < 0.7) {
  effect_interpretation <- "large effect"
} else {
  effect_interpretation <- "very large effect"
}

# Display effect size results
cat("Effect size (rank-biserial correlation):", round(r, 3), "\n")
cat("This represents a:", effect_interpretation, "\n")

# ============================================================================
# RESULTS VISUALIZATION (MEDIAN AND IQR FOCUS)
# ============================================================================

# Create publication-ready plot emphasizing medians (appropriate for Mann-Whitney test)
ggplot() +
  # Add individual data points in the background
  geom_point(data = sculpin_df, 
             aes(x = lake, y = length_mm, color = lake),
             position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 1.5) +
  # Add boxplot without outliers (shows median and IQR)
  geom_boxplot(data = sculpin_df,
               aes(x = lake, y = length_mm, fill = lake),
               alpha = 0.7, outlier.shape = NA, width = 0.5) +
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake",
    color = "Lake"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# ============================================================================
# SUMMARY STATISTICS FOR REPORTING (MEDIANS AND IQR)
# ============================================================================

# Calculate medians and IQR by lake (appropriate for Mann-Whitney test)
median_iqr_by_lake <- sculpin_df %>%
  group_by(lake) %>%
  summarize(
    n = n(),
    median = median(length_mm),
    q1 = quantile(length_mm, 0.25),
    q3 = quantile(length_mm, 0.75),
    iqr = IQR(length_mm)
  )

# Display summary statistics
print(median_iqr_by_lake)

# Calculate median difference between lakes
median_diff <- abs(diff(median_iqr_by_lake$median))

# Calculate percent difference based on medians
percent_diff <- median_diff / min(median_iqr_by_lake$median) * 100

# Display differences
cat("Median difference:", round(median_diff, 2), "mm\n")
cat("Percent difference:", round(percent_diff, 1), "%\n")

# ============================================================================
# END OF SCRIPT
# ============================================================================

# NOTES:
# - Mann-Whitney-Wilcoxon test is non-parametric (doesn't assume normality)
# - Tests whether one distribution is stochastically greater than another
# - More robust to outliers than t-test
# - Uses ranks instead of actual values
# - Appropriate for ordinal or continuous data
# - Effect size calculated using rank-biserial correlation
# - Results should be reported with medians and IQR rather than means and SD