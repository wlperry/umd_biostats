# =============================================================================
# Correlation Analysis: M&M Diameter and Mass Relationship
# =============================================================================
# This script performs a complete correlation analysis including data exploration,
# assumption testing, correlation testing, and visualization

# =============================================================================
# 1. SETUP AND DATA LOADING
# =============================================================================

# Load required libraries
library(skimr)      # For summary statistics
library(car)        # For diagnostic tests
library(tidyverse)  # For data manipulation and plotting

# Load the M&M peanut data
peanut_df <- read_csv("mms_peanut.csv")

# Preview the data structure
head(peanut_df)

# =============================================================================
# 2. DATA EXPLORATION
# =============================================================================

# Get summary statistics for diameter and mass
peanut_df %>% 
  select(diameter, mass) %>%
  skim() %>%
  select(-complete_rate, -n_missing)

# Create scatter plot with correlation ellipse
correlation_plot <- peanut_df %>%
  ggplot(aes(x = diameter, y = mass)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") +
  stat_ellipse(color = "red", linewidth = 1.2) +
  labs(
    title = "Relationship Between M&M Diameter and Mass",
    x = "Diameter (mm)",
    y = "Mass (grams)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Display the plot
correlation_plot

# =============================================================================
# 3. ASSUMPTION TESTING
# =============================================================================

# Test normality assumptions with Q-Q plots
par(mfrow = c(1, 2))

# Q-Q plot for diameter
qqnorm(peanut_df$diameter, main = "Q-Q Plot: Diameter")
qqline(peanut_df$diameter)

# Q-Q plot for mass
qqnorm(peanut_df$mass, main = "Q-Q Plot: Mass")
qqline(peanut_df$mass)

# Reset plotting parameters
par(mfrow = c(1, 1))

# Formal tests for normality using Shapiro-Wilk test
# Test normality of diameter
shapiro.test(peanut_df$diameter)

# Test normality of mass
shapiro.test(peanut_df$mass)

# Test linearity assumption with smooth trend line
linearity_plot <- peanut_df %>%
  ggplot(aes(x = diameter, y = mass)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Assessment of Linearity",
    subtitle = "Dashed line shows smooth trend",
    x = "Diameter (mm)",
    y = "Mass (grams)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Display linearity plot
linearity_plot

# =============================================================================
# 4. CORRELATION ANALYSIS
# =============================================================================

# Perform Pearson correlation test
correlation_result <- cor.test(peanut_df$diameter, peanut_df$mass)
correlation_result

# Extract correlation coefficient and calculate effect size measures
r_value <- correlation_result$estimate
r_squared <- r_value^2

# Display effect size results
cat("Correlation coefficient (r):", round(r_value, 3), "\n")
cat("Coefficient of determination (r²):", round(r_squared, 3), "\n")
cat("Percentage of variance explained:", round(r_squared * 100, 1), "%")

# =============================================================================
# 5. ALTERNATIVE NON-PARAMETRIC TEST
# =============================================================================

# Spearman's rank correlation (non-parametric alternative)
spearman_result <- cor.test(peanut_df$diameter, peanut_df$mass, method = "spearman")
spearman_result

# =============================================================================
# 6. PUBLICATION QUALITY VISUALIZATION
# =============================================================================

# Create publication-quality correlation plot
final_plot <- peanut_df %>%
  ggplot(aes(x = diameter, y = mass)) +
  geom_point(alpha = 0.7, size = 2.5, color = "#2E86AB") +
  stat_ellipse(color = "#A23B72", linewidth = 1.5, linetype = "solid") +
  labs(
    x = "Diameter (mm)",
    y = "Mass (g)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
  ) +
  annotate("text", 
           x = max(peanut_df$diameter) * 0.85, 
           y = min(peanut_df$mass) * 1.1,
           label = paste("r =", round(correlation_result$estimate, 3)),
           size = 4, fontface = "bold")

# Display final plot
final_plot

# =============================================================================
# 7. SUMMARY STATISTICS FOR REPORTING
# =============================================================================

# Extract key statistics for reporting
correlation_coefficient <- round(correlation_result$estimate, 3)
confidence_interval <- round(correlation_result$conf.int, 3)
p_value <- correlation_result$p.value
r_squared_value <- round(r_squared, 3)
variance_explained <- round(r_squared * 100, 1)

# Display summary for results section
cat("\n=== SUMMARY FOR RESULTS SECTION ===\n")
cat("Correlation coefficient (r):", correlation_coefficient, "\n")
cat("95% CI: [", confidence_interval[1], ", ", confidence_interval[2], "]\n")
cat("P-value:", p_value, "\n")
cat("R-squared:", r_squared_value, "\n")
cat("Variance explained:", variance_explained, "%\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================