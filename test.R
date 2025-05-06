# Load required packages
library(tidyverse)
library(car)
library(smatr)  # For Model II regression
library(lmodel2) # Alternative package for Model II regression
library(broom)
library(patchwork) # For combining plots

# Set seed for reproducibility
set.seed(123)

# Create some example data where both X and Y have error
n <- 100
x <- rnorm(n, mean = 10, sd = 2)
y <- 2 + 3 * x + rnorm(n, mean = 0, sd = 5)
data_ols_m2 <- tibble(x = x, y = y)

# MODEL I: OLS Regression
model_I <- lm(y ~ x, data = data_ols_m2)
summary(model_I)

# Extract coefficients for plotting
model_I_intercept <- coef(model_I)[1]
model_I_slope <- coef(model_I)[2]

# MODEL II: Using smatr package
# Note: smatr uses "SMA" for Standardized Major Axis
# and "MA" for Major Axis regression

# Standardized Major Axis (SMA) regression
model_II_sma <- sma(y ~ x, data = data_ols_m2, method = "SMA")
summary(model_II_sma)

# Major Axis (MA) regression
model_II_ma <- sma(y ~ x, data = data_ols_m2, method = "MA")
summary(model_II_ma)

# For Reduced Major Axis (RMA), we'll use the lmodel2 package
model_II_lmodel2 <- lmodel2(y ~ x, data = data_ols_m2, 
                            range.y = "relative", 
                            range.x = "relative", 
                            nperm = 99)
print(model_II_lmodel2)

# Extract RMA coefficients from lmodel2 results
rma_results <- model_II_lmodel2$regression.results
rma_intercept <- rma_results$Intercept[rma_results$Method == "RMA"]
rma_slope <- rma_results$Slope[rma_results$Method == "RMA"]

# Create a plot comparing the different regression lines
ggplot(data_ols_m2, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = model_I_intercept, 
              slope = model_I_slope, 
              color = "blue", 
              size = 1,
              linetype = "solid") +
  geom_abline(intercept = coef(model_II_ma)[1], 
              slope = coef(model_II_ma)[2], 
              color = "red", 
              size = 1,
              linetype = "dashed") +
  geom_abline(intercept = coef(model_II_sma)[1], 
              slope = coef(model_II_sma)[2], 
              color = "green", 
              size = 1,
              linetype = "dotted") +
  geom_abline(intercept = rma_intercept, 
              slope = rma_slope, 
              color = "purple", 
              size = 1,
              linetype = "dotdash") +
  labs(title = "MI vs MII Regressions",
       subtitle = "Blue = OLS, Red = MA, Green = SMA, Purple = RMA",
       x = "X Variable",
       y = "Y Variable") +
  theme_minimal()

# Visualize the differences in how the models minimize distances
# Create a subset of points to show minimization lines
subset_points <- sample(1:n, 10)
subset_data_ols_m2 <- data_ols_m2[subset_points, ]

# Function to calculate perpendicular distances
perp_dist <- function(x, y, a, b) {
  abs(-b*x + y - a) / sqrt(b^2 + 1)
}

# Calculate vertical and perpendicular distances for these points
subset_data_ols_m2 <- subset_data_ols_m2 %>%
  mutate(
    # For Model I (OLS)
    pred_y_ols = model_I_intercept + model_I_slope * x,
    vert_dist = abs(y - pred_y_ols),
    
    # For Model II (RMA) - perpendicular distances
    perp_dist_rma = perp_dist(x, y, rma_intercept, rma_slope)
  )

# Create a plot to show the different distance minimization approaches
p1 <- ggplot(data_ols_m2, aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = model_I_intercept, 
              slope = model_I_slope, 
              color = "blue", 
              size = 1) +
  geom_segment(data = subset_data_ols_m2,
               aes(x = x, y = y, xend = x, yend = pred_y_ols),
               color = "blue", linetype = "dashed") +
  labs(title = "MI: Min Vert Dist",
       x = "X Variable",
       y = "Y Variable") +
  theme_minimal()

# Calculate endpoints for perpendicular lines to RMA
calc_perp_endpoints <- function(x0, y0, a, b) {
  # Slope of the regression line
  m1 <- b
  # Slope of the perpendicular line
  m2 <- -1/m1
  # Y-intercept of the perpendicular line
  c2 <- y0 - m2 * x0
  # Find intersection point
  x_int <- (c2 - a) / (m1 - m2)
  y_int <- m1 * x_int + a
  return(c(x_int, y_int))
}

# Calculate perpendicular line endpoints
perp_endpoints <- t(sapply(1:nrow(subset_data_ols_m2), function(i) {
  calc_perp_endpoints(subset_data_ols_m2$x[i], subset_data_ols_m2$y[i], 
                      rma_intercept, rma_slope)
}))

subset_data_ols_m2$x_perp_end <- perp_endpoints[,1]
subset_data_ols_m2$y_perp_end <- perp_endpoints[,2]

p2 <- ggplot(data_ols_m2, aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = rma_intercept, 
              slope = rma_slope, 
              color = "purple", 
              size = 1) +
  geom_segment(data = subset_data_ols_m2,
               aes(x = x, y = y, xend = x_perp_end, yend = y_perp_end),
               color = "purple", linetype = "dashed") +
  labs(title = "MII RMA: Min Perpendicular Dist",
       x = "X Variable",
       y = "Y Variable") +
  theme_minimal()

# Display both plots side by side using patchwork
p1 + 
  p2 + theme(axis.title.y = element_blank(),
             axis.text.y = element_blank())

# Compare confidence intervals
confint(model_I)
summary(model_II_sma)
summary(model_II_ma)

