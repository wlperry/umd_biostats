# Two-Sample t-Test Analysis: Slimy Sculpin Fish Length Comparison
# Author: Bill Perry
# Purpose: Compare total length of slimy sculpin fish between two lakes

# ============================================================================
# LOAD LIBRARIES
# ============================================================================

library(car)         # For Levene's test
library(coin)        # For permutation tests
library(rcompanion)  # For plotNormalHistogram
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
sculpin_df %>%  
  ggplot(aes(x = lake, y = length_mm, fill = lake)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 2) +
  labs(
    title = "Total Length of Slimy Sculpin Fish by Lake",
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
    title = "Mean Total Length (± SE) of Slimy Sculpin Fish by Lake",
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

# Levene's test for homogeneity of variances
leveneTest(length_mm ~ lake, data = sculpin_df)

# ============================================================================
# TWO-SAMPLE T-TESTS
# ============================================================================

# Perform standard t-test assuming equal variances (pooled variance)
t_test_equal_var <- t.test(
  length_mm ~ lake,
  data = sculpin_df,
  var.equal = TRUE  # Use pooled variance
)

# Display results
t_test_equal_var

# Perform Welch's t-test (does not assume equal variances)
t_test_welch <- t.test(
  length_mm ~ lake,
  data = sculpin_df,
  var.equal = FALSE  # Use Welch's correction
)

# Display results
t_test_welch

# ============================================================================
# RESULTS VISUALIZATION
# ============================================================================

# Create publication-ready boxplot
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
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Set2")

# Create mean and SE plot for publication
sculpin_df %>% 
  ggplot(aes(x = lake, y = length_mm, color = lake, shape = lake, fill = lake)) +
  stat_summary(fun = mean, geom = "point", alpha = 0.7, size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake",
    color = "Lake",
    shape = "Lake"
  ) +
  coord_cartesian(ylim = c(0, 60)) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
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
mean_se_by_lake

# Calculate percent difference between lakes
percent_diff <- abs(diff(mean_se_by_lake$mean)) / min(mean_se_by_lake$mean) * 100
percent_diff

# ============================================================================
# END OF SCRIPT
# ============================================================================