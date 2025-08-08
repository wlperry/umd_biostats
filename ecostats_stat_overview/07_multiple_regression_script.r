# Multiple Linear Regression Analysis: Ant Species Density
# Author: Your Name  
# Purpose: Examine how ant species density varies with latitude and elevation
# Data from: Gotelli & Ellison 2002, Ecology and Oikos

# ============================================================================
# LOAD LIBRARIES
# ============================================================================

library(lmtest)     # For Breusch-Pagan test
library(patchwork)  # For combining plots
library(car)        # For regression diagnostics and VIF
library(skimr)      # For data summary
library(tidyverse)  # For data manipulation and visualization

# ============================================================================
# LOAD AND PREVIEW DATA
# ============================================================================

# Load the ant species density data
ant_df <- read_csv("data/AntSpeciesDensity.csv")

# Preview the first few rows
head(ant_df)

# ============================================================================
# DATA OVERVIEW AND SUMMARY STATISTICS
# ============================================================================

# Generate comprehensive summary statistics
ant_df %>% 
  skim()

# ============================================================================
# DATA VISUALIZATION
# ============================================================================

# Create individual scatterplots for each predictor variable
p1 <- ant_df %>%  
  ggplot(aes(x = Latitude, y = AntSpeciesDensity)) +
  geom_point(alpha = 0.7, size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = "Latitude (degrees N)",
    y = "Ant Species Density"
  ) +
  theme_minimal()

p2 <- ant_df %>%  
  ggplot(aes(x = Elevation, y = AntSpeciesDensity)) +
  geom_point(alpha = 0.7, size = 3, color = "brown") +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
  labs(
    x = "Elevation (m)",
    y = "Ant Species Density"
  ) +
  theme_minimal()

# Combine plots to show both relationships
p1 + p2

# ============================================================================
# CORRELATION ANALYSIS
# ============================================================================

# Check correlations between all variables
cor(ant_df)

# Create geographic visualization with species density
ant_df %>%
  ggplot(aes(x = Latitude, y = Elevation, color = AntSpeciesDensity, size = AntSpeciesDensity)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "Species\nDensity") +
  scale_size_continuous(name = "Species\nDensity", range = c(2, 6)) +
  labs(
    x = "Latitude (degrees N)",
    y = "Elevation (m)",
    title = "Ant Species Density in Geographic Space"
  ) +
  theme_minimal()

# ============================================================================
# MULTIPLE LINEAR REGRESSION ANALYSIS
# ============================================================================

# Fit the multiple linear regression model
ant_model <- lm(AntSpeciesDensity ~ Latitude + Elevation, data = ant_df)

# Display the comprehensive model summary
summary(ant_model)

# Get ANOVA table for the multiple regression
anova(ant_model)

# ============================================================================
# ASSUMPTION TESTING
# ============================================================================

# Check sample size relative to number of predictors
n_obs <- nrow(ant_df)
n_predictors <- 2
obs_per_predictor <- n_obs / n_predictors

cat("Number of observations:", n_obs, "\n")
cat("Number of predictors:", n_predictors, "\n") 
cat("Observations per predictor:", obs_per_predictor, "\n")

# Calculate Variance Inflation Factors (VIF) to check for multicollinearity
vif(ant_model)

# Create standard diagnostic plots for multiple regression
par(mfrow = c(2, 2))
plot(ant_model)
par(mfrow = c(1, 1))

# Test for normality of residuals using Shapiro-Wilk test
shapiro.test(residuals(ant_model))

# Test for homoscedasticity using Breusch-Pagan test
bptest(ant_model)

# Create added-variable plots (partial regression plots)
par(mfrow = c(1, 2))
avPlots(ant_model)
par(mfrow = c(1, 1))

# ============================================================================
# MODEL INTERPRETATION AND COEFFICIENTS
# ============================================================================

# Extract model coefficients
coef(ant_model)

# Create the multiple regression equation
intercept <- coef(ant_model)[1]
lat_coef <- coef(ant_model)[2]  
elev_coef <- coef(ant_model)[3]

# Display the regression equation
paste("Ant Species Density =", round(intercept, 2), "+", 
      round(lat_coef, 3), "× Latitude", "+", 
      round(elev_coef, 4), "× Elevation")

# ============================================================================
# MODEL COMPARISON AND VARIABLE IMPORTANCE
# ============================================================================

# Fit reduced models to test variable importance
model_lat_only <- lm(AntSpeciesDensity ~ Latitude, data = ant_df)
model_elev_only <- lm(AntSpeciesDensity ~ Elevation, data = ant_df)

# Compare R-squared values across models
cat("R-squared values:\n")
cat("Latitude only:", round(summary(model_lat_only)$r.squared, 3), "\n")
cat("Elevation only:", round(summary(model_elev_only)$r.squared, 3), "\n")
cat("Full model:", round(summary(ant_model)$r.squared, 3), "\n")

# Test significance of adding elevation to latitude-only model
cat("\nTesting significance of adding elevation to latitude model:\n")
anova(model_lat_only, ant_model)

# Test significance of adding latitude to elevation-only model
cat("\nTesting significance of adding latitude to elevation model:\n")
anova(model_elev_only, ant_model)

# ============================================================================
# PUBLICATION-QUALITY FIGURE
# ============================================================================

# Panel A: Latitude relationship (partial effect)
p1 <- ant_df %>%
  ggplot(aes(x = Latitude, y = AntSpeciesDensity)) +
  geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = "Latitude (°N)",
    y = "Ant Species Density",
    title = "A) Latitude Effect"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold")
  )

# Panel B: Elevation relationship (partial effect)  
p2 <- ant_df %>%
  ggplot(aes(x = Elevation, y = AntSpeciesDensity)) +
  geom_point(size = 3, alpha = 0.7, color = "brown") +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
  labs(
    x = "Elevation (m)",
    y = "Ant Species Density", 
    title = "B) Elevation Effect"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold")
  )

# Panel C: Model fit (observed vs predicted)
ant_df$predicted <- fitted(ant_model)
p3 <- ant_df %>%
  ggplot(aes(x = predicted, y = AntSpeciesDensity)) +
  geom_point(size = 3, alpha = 0.7, color = "purple") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Predicted Species Density",
    y = "Observed Species Density",
    title = "C) Model Fit"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold")
  )

# Panel D: Geographic distribution
p4 <- ant_df %>%
  ggplot(aes(x = Latitude, y = Elevation, color = AntSpeciesDensity, size = AntSpeciesDensity)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_c(name = "Species\nDensity") +
  scale_size_continuous(name = "Species\nDensity", range = c(2, 6)) +
  labs(
    x = "Latitude (°N)",
    y = "Elevation (m)",
    title = "D) Geographic Distribution"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold")
  )

# Combine all panels into a comprehensive figure
combined_plot <- (p1 + p2) / (p3 + p4)
combined_plot

# ============================================================================
# MODEL PERFORMANCE METRICS
# ============================================================================

# Extract key model performance metrics
model_summary <- summary(ant_model)

cat("Model Performance Summary:\n")
cat("Multiple R-squared:", round(model_summary$r.squared, 3), "\n")
cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 3), "\n")
cat("Residual standard error:", round(model_summary$sigma, 3), "\n")
cat("F-statistic:", round(model_summary$fstatistic[1], 2), "\n")
cat("Model p-value:", format.pval(pf(model_summary$fstatistic[1], 
                                     model_summary$fstatistic[2], 
                                     model_summary$fstatistic[3], 
                                     lower.tail = FALSE)), "\n")

# ============================================================================
# END OF SCRIPT
# ============================================================================

# NOTES FOR MULTIPLE LINEAR REGRESSION:
# - Partial slopes: Effect of each predictor holding others constant
# - VIF < 5: Acceptable multicollinearity between predictors
# - Adjusted R²: More conservative than regular R² for multiple predictors
# - Added-variable plots: Show partial relationships after controlling for other variables
# - Model comparison: Use ANOVA to test significance of additional predictors

# BIOLOGICAL INTERPRETATION:
# - Latitude coefficient: Change in species density per degree latitude (holding elevation constant)
# - Elevation coefficient: Change in species density per meter elevation (holding latitude constant)
# - Intercept: Expected species density at 0° latitude and 0m elevation (often not meaningful)
# - Geographic patterns: Consider latitudinal and elevational diversity gradients

# ASSUMPTIONS TO CHECK:
# - Linearity: Added-variable plots and residual patterns
# - Independence: Study design consideration
# - Homoscedasticity: Breusch-Pagan test and residual plots
# - Normality: Shapiro-Wilk test and Q-Q plots
# - Multicollinearity: VIF values < 5-10
# - Adequate sample size: >10 observations per predictor