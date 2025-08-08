# Welch's Two-Sample t-Test Analysis: Slimy Sculpin Fish Length Comparison
# Author: Bill Perry
# Purpose: Compare total length of slimy sculpin fish between two lakes using Welch's t-test
# Note: Welch's t-test does not assume equal variances between groups

# ============================================================================
# LOAD LIBRARIES
# ============================================================================

library(car)         # For Levene's test
library(coin)        # For permutation tests
library(rcompanion)  # For plotNormalHistogram
library(patchwork)   # For combining plots
library(skimr)       # For data summary statistics
library(tidyverse)   # For data manipulation and visualization

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
# ASSUMPTION TESTING: NORMALITY
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
# ASSUMPTION TESTING: HOMOGENEITY OF VARIANCES
# ============================================================================
# Note: Welch's t-test does not assume equal variances, but we test this
# to understand our data and justify using Welch's t-test

# Levene's test for homogeneity of variances
leveneTest(length_mm ~ lake, data = sculpin_df)

# ============================================================================
# WELCH'S T-TEST
# ============================================================================

# Perform Welch's t-test (does not assume equal variances)
welch_t_test <- t.test(
  length_mm ~ lake,
  data = sculpin_df,
  var.equal = FALSE  # This specifies Welch's t-test
)

# Display results
welch_t_test

# ============================================================================
# RESULTS VISUALIZATION
# ============================================================================

# Create publication-ready boxplot with results
sculpin_df %>% 
  ggplot(aes(x = lake, y = length_mm, fill = lake)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 2) +
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

# ============================================================================
# SUMMARY STATISTICS FOR REPORTING
# ============================================================================

# Calculate means, standard deviations, and standard errors by lake
mean_se_by_lake <- sculpin_df %>%
  group_by(lake) %>%
  summarize(
    n = n(),
    mean = mean(length_mm),
    sd = sd(length_mm),
    se = sd / sqrt(n)
  )

# Display summary statistics
print(mean_se_by_lake)

# Calculate percent difference between lakes
percent_diff <- abs(diff(mean_se_by_lake$mean)) / min(mean_se_by_lake$mean) * 100

# Display percent difference
percent_diff

# ============================================================================
# END OF SCRIPT
# ============================================================================

# NOTES:
# - Welch's t-test is more robust than standard t-test when variances are unequal
# - It uses the Welch-Satterthwaite equation to calculate degrees of freedom
# - Results in non-integer degrees of freedom
# - Recommended as default approach for comparing two independent groups
# - Minimal loss of power when variances are equal, but protects against
#   inflated Type I error when variances are unequal