# Linear Regression Analysis: DEET Concentration and Mosquito Bites
# Author: Bill Perry
# Purpose: Examine the relationship between DEET concentration and mosquito bites
# Data from: Golenda et al. 1999, American Journal of Tropical Medicine and Hygiene

# ============================================================================
# LOAD LIBRARIES
# ============================================================================

library(lmtest)     # For Breusch-Pagan test
library(patchwork)  # For combining plots
library(car)        # For regression diagnostics
library(skimr)      # For data summary
library(tidyverse)  # For data manipulation and visualization

# ============================================================================
# LOAD AND PREVIEW DATA
# ============================================================================

# Load the DEET mosquito bite data
deet_df <- read_csv("data/chap17q30DEETMosquiteBites.csv")

# Preview the first few rows
head(deet_df)

# ============================================================================
# DATA OVERVIEW AND SUMMARY STATISTICS
# ============================================================================

# Generate comprehensive summary statistics
deet_df %>% 
  skim()

# ============================================================================
# DATA VISUALIZATION
# ============================================================================

# Create exploratory scatterplot
deet_df %>%  
  ggplot(aes(x = dose, y = bites)) +
  geom_point(alpha = 0.7, size = 3, color = "darkred") +
  labs(
    x = "DEET Concentration",
    y = "Number of Mosquito Bites"
  ) +
  theme_minimal()

# Create scatterplot with regression line and confidence interval
deet_df %>% 
  ggplot(aes(x = dose, y = bites)) +
  geom_point(alpha = 0.7, size = 3, color = "darkred") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
  labs(
    x = "DEET Concentration",
    y = "Number of Mosquito Bites"
  ) +
  theme_minimal()

# ============================================================================
# LINEAR REGRESSION ANALYSIS
# ============================================================================

# Fit the linear regression model
deet_model <- lm(bites ~ dose, data = deet_df)

# Display the comprehensive model summary
summary(deet_model)

# Get ANOVA table for the regression
anova(deet_model)

# ============================================================================
# REGRESSION ASSUMPTION TESTING
# ============================================================================

# Create diagnostic plots to check assumptions
par(mfrow = c(2, 2))
plot(deet_model)
par(mfrow = c(1, 1))

# Test for normality of residuals using Shapiro-Wilk test
shapiro.test(residuals(deet_model))

# Test for homoscedasticity using Breusch-Pagan test
bptest(deet_model)

# ============================================================================
# MODEL INTERPRETATION AND COEFFICIENTS
# ============================================================================

# Extract model coefficients
coef(deet_model)

# Create the regression equation
intercept <- coef(deet_model)[1]
slope <- coef(deet_model)[2]

# Display the regression equation
paste("Number of Bites =", round(intercept, 3), "+", round(slope, 3), "× DEET Concentration")

# ============================================================================
# MODEL PERFORMANCE METRICS
# ============================================================================

# Calculate R-squared and correlation coefficient
r_squared <- summary(deet_model)$r.squared
correlation <- cor(deet_df$dose, deet_df$bites)

# Display model performance
cat("R-squared:", round(r_squared, 3), "\n")
cat("Correlation coefficient:", round(correlation, 3), "\n")

# ============================================================================
# MAKING PREDICTIONS
# ============================================================================

# Create new data for predictions
new_doses <- data.frame(dose = c(2.0, 3.5, 5.0))

# Make point predictions
predicted_bites <- predict(deet_model, new_doses)
predicted_bites

# Predictions with confidence intervals (for the mean response)
predict(deet_model, new_doses, interval = "confidence")

# Predictions with prediction intervals (for individual observations)
predict(deet_model, new_doses, interval = "prediction")

# ============================================================================
# PUBLICATION-QUALITY FIGURE
# ============================================================================

# Create publication-ready scatterplot with regression line
publication_plot <- deet_df %>% 
  ggplot(aes(x = dose, y = bites)) +
  geom_point(alpha = 0.6, size = 2.5, color = "darkred") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", 
              fill = "lightgray", alpha = 0.3, linewidth = 1.2) +
  labs(
    x = "DEET Concentration",
    y = "Number of Mosquito Bites"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.caption.position = "plot"
  ) +
  coord_cartesian(expand = FALSE)

# Display the publication plot
publication_plot

# ============================================================================
# RESIDUAL ANALYSIS AND MODEL DIAGNOSTICS
# ============================================================================

# Check for missing values in the original dataset
sum(is.na(deet_df$dose))
sum(is.na(deet_df$bites))

# Verify number of observations used in the model
cat("Observations in model:", nobs(deet_model), "\n")
cat("Observations in dataset:", nrow(deet_df), "\n")

# Create augmented dataset with model diagnostics
augmented_data <- deet_df
augmented_data[names(fitted(deet_model)), c("fitted_values", "residuals", "std_residuals", "student_residuals", "leverage", "cooks_d")] <- 
  data.frame(
    fitted_values = fitted(deet_model),
    residuals = residuals(deet_model), 
    std_residuals = rstandard(deet_model),
    student_residuals = rstudent(deet_model),
    leverage = hatvalues(deet_model),
    cooks_d = cooks.distance(deet_model)
  )

# Preview the augmented dataset
head(augmented_data)

# ============================================================================
# SAVE RESULTS FOR FUTURE ANALYSIS
# ============================================================================

# Save the augmented dataset with diagnostic measures
write_csv(augmented_data, "deet_regression_with_diagnostics.csv")

# Create and save model summary statistics
model_summary <- data.frame(
  parameter = c("intercept", "slope", "r_squared", "adj_r_squared", "residual_se", "f_statistic", "p_value"),
  value = c(
    coef(deet_model)[1],
    coef(deet_model)[2], 
    summary(deet_model)$r.squared,
    summary(deet_model)$adj.r.squared,
    summary(deet_model)$sigma,
    summary(deet_model)$fstatistic[1],
    pf(summary(deet_model)$fstatistic[1], 
       summary(deet_model)$fstatistic[2], 
       summary(deet_model)$fstatistic[3], 
       lower.tail = FALSE)
  )
)

# Save model summary to CSV
write_csv(model_summary, "deet_model_summary.csv")

# ============================================================================
# END OF SCRIPT
# ============================================================================

# NOTES FOR PUBLICATION:
# - Expected relationship: Negative (higher DEET = fewer bites)
# - Key assumptions: Linearity, independence, homoscedasticity, normality of residuals
# - Diagnostic tools: Residual plots, Shapiro-Wilk test, Breusch-Pagan test
# - Effect interpretation: Slope represents change in bites per unit DEET increase
# - R-squared: Proportion of variance in bites explained by DEET concentration
# - Confidence intervals: Uncertainty in mean response at given DEET levels
# - Prediction intervals: Uncertainty for individual observations

# BIOLOGICAL INTERPRETATION:
# - Intercept: Expected bites when DEET concentration = 0
# - Slope: Effectiveness of DEET (negative slope confirms repellent effect)
# - R-squared: How well DEET concentration explains bite variation
# - Residuals: Individual variation not explained by DEET alone