# One-Way Analysis of Variance (ANOVA) - M&M Mass Analysis
# Author: Bill Perry
# Purpose: Analyze differences in M&M mass among different center types

# ==============================================================================
# SETUP: Load required libraries and data
# ==============================================================================

# Load required libraries
library(skimr)      # For summary statistics
library(car)        # For Levene's test and Type III ANOVA
library(emmeans)    # For estimated marginal means and post-hoc tests
library(tidyverse)  # For data manipulation and visualization

# Load the M&M data
mms_df <- read_csv("data/mms.csv")

# Preview the data structure
head(mms_df)

# ==============================================================================
# DATA EXPLORATION: Summary statistics and visualization
# ==============================================================================

# Get summary statistics by center type
mms_df %>% 
  group_by(center) %>% 
  skim() %>%
  select(-complete_rate, -n_missing)

# Create boxplot to visualize mass distribution by center type
exploratory_plot <- mms_df %>%
  ggplot(aes(x = center, y = mass, fill = center)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), 
             alpha = 0.4, size = 1) +
  labs(
    title = "M&M Mass Distribution by Center Type",
    x = "Center Type",
    y = "Mass (grams)",
    fill = "Center Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

exploratory_plot

# ==============================================================================
# ANOVA MODEL: Fit the one-way ANOVA model
# ==============================================================================

# Fit the one-way ANOVA model using lm()
mms_model <- lm(mass ~ center, data = mms_df)

# Display model summary
summary(mms_model)

# Display the ANOVA table using Type III sums of squares
Anova(mms_model, type = "III")

# ==============================================================================
# ASSUMPTION TESTING: Check ANOVA assumptions
# ==============================================================================

# Create diagnostic plots to assess assumptions
par(mfrow = c(2, 2))
plot(mms_model)

# Test for equal variances using Levene's test
leveneTest(mass ~ center, data = mms_df)

# Test normality of residuals using Shapiro-Wilk test
shapiro.test(residuals(mms_model))

# ==============================================================================
# POST-HOC TESTING: Determine which specific groups differ
# ==============================================================================

# Method 1: Tukey's Honestly Significant Difference (HSD)
# Note: TukeyHSD requires aov object, so we convert our lm model
mms_aov <- aov(mass ~ center, data = mms_df)
tukey_results <- TukeyHSD(mms_aov)
tukey_results

# Method 2: Estimated Marginal Means with Sidak Correction
# Calculate estimated marginal means
mms_emmeans <- emmeans(mms_model, ~ center)
mms_emmeans

# Pairwise comparisons with Sidak correction
emmeans_sidak <- pairs(mms_emmeans, adjust = "sidak")
emmeans_sidak

# Visualize post-hoc results with confidence intervals
emmeans_plot <- plot(mms_emmeans, comparisons = TRUE) +
  labs(
    title = "Estimated Marginal Means with 95% Confidence Intervals",
    x = "Estimated Marginal Mean (grams)",
    y = "Center Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

emmeans_plot

# ==============================================================================
# RESULTS SUMMARY: Calculate effect size and group statistics
# ==============================================================================

# Calculate eta-squared (effect size)
ss_center <- sum((fitted(mms_model) - mean(mms_df$mass))^2)
ss_total <- sum((mms_df$mass - mean(mms_df$mass))^2)
eta_squared <- ss_center / ss_total

# Display group means and standard deviations
group_stats <- mms_df %>%
  group_by(center) %>%
  summarise(
    n = n(),
    mean = mean(mass),
    sd = sd(mass),
    .groups = 'drop'
  )

# Print results
group_stats
eta_squared

# ==============================================================================
# PUBLICATION QUALITY FIGURE: Create final visualization
# ==============================================================================

# Create publication-quality plot with means and error bars
publication_plot <- mms_df %>%
  ggplot(aes(x = center, y = mass, fill = center)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_point(position = position_jitter(width = 0.15), 
             alpha = 0.3, size = 0.8) +
  stat_summary(fun = mean, geom = "point", 
               size = 3, shape = 18, color = "black") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", width = 0.2, 
               color = "black", linewidth = 0.8) +
  labs(
    x = "Center Type",
    y = "Mass (g)"
  ) +
  scale_fill_manual(values = c("peanut" = "#8B4513", "peanut butter" = "#DEB887", 
                              "plain" = "#87CEEB")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "right",
  )

publication_plot

# ==============================================================================
# END OF SCRIPT
# ==============================================================================

# This script performs a complete one-way ANOVA analysis including:
# 1. Data loading and exploration
# 2. Model fitting and ANOVA table generation
# 3. Assumption testing (normality and homogeneity of variance)
# 4. Post-hoc testing using multiple methods
# 5. Effect size calculation
# 6. Publication-quality visualization

