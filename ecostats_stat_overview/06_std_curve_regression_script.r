# Linear Regression as Standard Curve: Paperweight Calibration for Leaf Area Estimation
# Author: Bill Perry
# Purpose: Develop calibration model to estimate leaf areas from paper trace masses
# Method: Paperweight technique using known areas to establish mass-area relationship

# ============================================================================
# LOAD LIBRARIES
# ============================================================================

library(lmtest)     # For Breusch-Pagan test
library(patchwork)  # For combining plots
library(car)        # For regression diagnostics
library(skimr)      # For data summary
library(tidyverse)  # For data manipulation and visualization
library(readxl)     # For reading Excel files

# ============================================================================
# LOAD AND PREVIEW DATA
# ============================================================================

# Load the calibration data (paperweights with known areas)
paperweight_df <- read_excel("data/paperweights.xlsx")

# Load the leaf trace data for applying the calibration
leaf_trace_df <- read_excel("data/leaf_trace_masses.xlsx")

# Preview the calibration data
head(paperweight_df)

# Preview the leaf trace data
head(leaf_trace_df)

# ============================================================================
# DATA OVERVIEW AND SUMMARY STATISTICS
# ============================================================================

# Generate comprehensive summary statistics for calibration data
paperweight_df %>% 
  skim()

# ============================================================================
# DATA VISUALIZATION (CALIBRATION DATA)
# ============================================================================

# Create exploratory scatterplot of mass vs known area
paperweight_df %>%  
  ggplot(aes(x = mass_g, y = area_cm2)) +
  geom_point(alpha = 0.7, size = 3, color = "darkblue") +
  labs(
    x = "Mass (g)",
    y = "Area (cm²)"
  ) +
  theme_minimal()

# Create scatterplot with regression line and confidence interval
paperweight_df %>% 
  ggplot(aes(x = mass_g, y = area_cm2)) +
  geom_point(alpha = 0.7, size = 3, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    x = "Mass (g)",
    y = "Area (cm²)"
  ) +
  theme_minimal()

# ============================================================================
# CALIBRATION MODEL DEVELOPMENT
# ============================================================================

# Fit the linear regression model for calibration
calibration_model <- lm(area_cm2 ~ mass_g, data = paperweight_df)

# Display the comprehensive model summary
summary(calibration_model)

# Get ANOVA table for the regression
anova(calibration_model)

# ============================================================================
# CALIBRATION MODEL ASSUMPTION TESTING
# ============================================================================

# Create diagnostic plots to check assumptions
par(mfrow = c(2, 2))
plot(calibration_model)
par(mfrow = c(1, 1))

# Test for normality of residuals using Shapiro-Wilk test
shapiro.test(residuals(calibration_model))

# Test for homoscedasticity using Breusch-Pagan test
bptest(calibration_model)

# ============================================================================
# CALIBRATION MODEL INTERPRETATION
# ============================================================================

# Extract model coefficients
coef(calibration_model)

# Create the calibration equation
intercept <- coef(calibration_model)[1]
slope <- coef(calibration_model)[2]

# Display the calibration equation
paste("Area (cm²) =", round(intercept, 4), "+", round(slope, 2), "× Mass (g)")

# Calculate R-squared for model quality assessment
r_squared <- summary(calibration_model)$r.squared
cat("R-squared:", round(r_squared, 4), "\n")
cat("This indicates", round(r_squared * 100, 1), "% of area variation is explained by mass\n")

# ============================================================================
# APPLICATION: ESTIMATING LEAF AREAS FROM TRACES
# ============================================================================

# Clean up the leaf_type variable (remove extra spaces and standardize)
leaf_trace_df <- leaf_trace_df %>%
  mutate(leaf_type = str_trim(tolower(leaf_type)))

# Apply the calibration model to estimate leaf areas from trace masses
leaf_trace_df <- leaf_trace_df %>%
  mutate(
    estimated_leaf_area_cm2 = predict(calibration_model, newdata = data.frame(mass_g = trace_mass_g))
  )

# Preview the results
head(leaf_trace_df)

# ============================================================================
# SUMMARY STATISTICS FOR ESTIMATED LEAF AREAS
# ============================================================================

# Calculate summary statistics of estimated leaf areas by species and leaf type
leaf_area_summary <- leaf_trace_df %>%
  group_by(species, leaf_type) %>% 
  summarise(
    n = n(),
    mean_area = mean(estimated_leaf_area_cm2),
    sd_area = sd(estimated_leaf_area_cm2),
    se_area = sd_area / sqrt(n),
    min_area = min(estimated_leaf_area_cm2),
    max_area = max(estimated_leaf_area_cm2),
    .groups = "drop"
  )

# Display summary results
print(leaf_area_summary)

# ============================================================================
# VISUALIZATION OF ESTIMATED LEAF AREAS
# ============================================================================

# Create boxplot of estimated leaf areas by species and leaf type
leaf_trace_df %>%
  ggplot(aes(x = species, y = estimated_leaf_area_cm2, fill = leaf_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2),
             alpha = 0.5, size = 1) +
  labs(
    x = "Species",
    y = "Estimated Leaf Area (cm²)",
    fill = "Leaf Type"
  ) +
  theme_minimal()

# ============================================================================
# PUBLICATION-QUALITY COMBINED FIGURE
# ============================================================================

# Create calibration plot
calib_plot <- paperweight_df %>% 
  ggplot(aes(x = mass_g, y = area_cm2)) +
  geom_point(alpha = 0.7, size = 2, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", 
              fill = "lightgray", alpha = 0.3, linewidth = 1) +
  labs(
    x = "Paper Mass (g)",
    y = "Known Area (cm²)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    plot.title = element_text(size = 11, face = "bold")
  )

# Create application plot (mean ± SE by species and leaf type)
app_plot <- leaf_trace_df %>%
  ggplot(aes(x = species, y = estimated_leaf_area_cm2, fill = leaf_type)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    x = "Species",
    y = "Estimated Leaf Area (cm²)",
    fill = "Leaf Type"
  ) +
  theme_light() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 10, face = "bold")
  )

# Combine plots using patchwork
combined_plot <- calib_plot + app_plot + plot_layout(ncol = 2)
combined_plot

# ============================================================================
# RESIDUAL ANALYSIS AND MODEL DIAGNOSTICS
# ============================================================================

# Check for missing values in the calibration dataset
sum(is.na(paperweight_df$mass_g))
sum(is.na(paperweight_df$area_cm2))

# Verify number of observations used in the model
cat("Observations in calibration model:", nobs(calibration_model), "\n")
cat("Observations in calibration dataset:", nrow(paperweight_df), "\n")

# Create augmented dataset with model diagnostics
augmented_data <- paperweight_df
augmented_data[names(fitted(calibration_model)), c("fitted_values", "residuals", "std_residuals", "student_residuals", "leverage", "cooks_d")] <- 
  data.frame(
    fitted_values = fitted(calibration_model),
    residuals = residuals(calibration_model), 
    std_residuals = rstandard(calibration_model),
    student_residuals = rstudent(calibration_model),
    leverage = hatvalues(calibration_model),
    cooks_d = cooks.distance(calibration_model)
  )

# Preview the augmented calibration dataset
head(augmented_data)

# Identify potential outliers and influential points
cat("Potential outliers (|standardized residuals| > 2):\n")
outliers <- which(abs(augmented_data$std_residuals) > 2, arr.ind = TRUE)
if(length(outliers) > 0) {
  print(augmented_data[outliers, ])
} else {
  cat("No outliers detected\n")
}

# Check for highly influential points (Cook's distance > 4/n)
n <- nobs(calibration_model)
high_influence <- which(augmented_data$cooks_d > 4/n, arr.ind = TRUE)
cat("Highly influential points (Cook's d > 4/n =", round(4/n, 3), "):\n")
if(length(high_influence) > 0) {
  print(augmented_data[high_influence, ])
} else {
  cat("No highly influential points detected\n")
}

# ============================================================================
# SAVE RESULTS FOR FUTURE ANALYSIS
# ============================================================================

# Save the augmented calibration dataset with diagnostic measures
write_csv(augmented_data, "paperweight_calibration_with_diagnostics.csv")

# Save the leaf area estimates
write_csv(leaf_trace_df, "estimated_leaf_areas.csv")

# Save the summary statistics
write_csv(leaf_area_summary, "leaf_area_summary_statistics.csv")

# Create and save calibration model summary
calibration_summary <- data.frame(
  parameter = c("intercept", "slope", "r_squared", "adj_r_squared", "residual_se", "f_statistic", "p_value"),
  value = c(
    coef(calibration_model)[1],
    coef(calibration_model)[2], 
    summary(calibration_model)$r.squared,
    summary(calibration_model)$adj.r.squared,
    summary(calibration_model)$sigma,
    summary(calibration_model)$fstatistic[1],
    pf(summary(calibration_model)$fstatistic[1], 
       summary(calibration_model)$fstatistic[2], 
       summary(calibration_model)$fstatistic[3], 
       lower.tail = FALSE)
  )
)

# Save calibration model summary to CSV
write_csv(calibration_summary, "calibration_model_summary.csv")

# ============================================================================
# END OF SCRIPT
# ============================================================================

# NOTES FOR CALIBRATION CURVES:
# - Expected relationship: Strong positive linear (higher mass = higher area)
# - R-squared should be > 0.98 for good calibration
# - Intercept should be close to 0 (no mass = no area)
# - Slope represents specific area (cm²/g) of the paper
# - Residuals should be small and randomly distributed
# - No outliers or influential points in calibration data

# BIOLOGICAL APPLICATIONS:
# - Compare sun vs. shade leaves (morphological adaptation)
# - Species differences in leaf size
# - Individual variation within treatments
# - Cost-effective alternative to expensive leaf area meters

# QUALITY CONTROL CHECKS:
# - Calibration R² > 0.98
# - Intercept not significantly different from 0
# - No systematic patterns in residuals
# - No influential outliers affecting calibration