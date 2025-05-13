
# Power analysis example
power.t.test(
  n = 24,           # Sample size per group
  delta = 5,        # Effect size (difference in means)
  sd = 10,          # Standard deviation
  sig.level = 0.05, # Type I error rate
  type = "two.sample"
)



# Create a visual showing how sample size affects power
sample_sizes <- seq(5, 50, by = 5)
power_values <- sapply(sample_sizes, function(n) {
  power.t.test(
    n = n, 
    delta = 5, 
    sd = 10,
    sig.level = 0.05,
    type = "two.sample"
  )$power
})

power_df <- data.frame(sample_size = sample_sizes, power = power_values)

ggplot(power_df, aes(x = sample_size, y = power)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  labs(title = "Power vs. Sample Size",
       subtitle = "Effect size = 5, SD = 10, α = 0.05",
       x = "Sample Size per Group", 
       y = "Power (1-β)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1))

# Create a visual representation of Type I and Type II errors
alpha_beta_plot <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                geom = "line", aes(color = "Ho True")) +
  stat_function(fun = dnorm, args = list(mean = 2, sd = 1), 
                geom = "line", aes(color = "H1 True")) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1),
            fill = "red", alpha = 0.3, xlim = c(1.96, 4)) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 2, sd = 1),
            fill = "blue", alpha = 0.3, xlim = c(-4, 1.96)) +
  scale_color_manual(values = c("H₀ True" = "black", "H₁ True" = "blue"),
                     name = "Distribution") +
  labs(title = "Type I and Type II Errors",
       x = "Test Statistic Value", 
       y = "Probability Density") +
  theme_minimal() +
  annotate("text", x = 2.5, y = 0.09, label = "Type I Error (α)", color = "red") +
  annotate("text", x = 0, y = 0.05, label = "Type II Error (β)", color = "blue")
alpha_beta_plot


p1 <- ggplot(pine_data, aes(x = len_mm)) +
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram", x = "Length (mm)", y = "Count") +
  theme_minimal()

p2 <- ggplot(pine_data, aes(x = len_mm)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(title = "Density Plot", x = "Length (mm)", y = "Density") +
  theme_minimal()

p3 <- ggplot(pine_data, aes(x = factor(1), y = len_mm)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot", x = "", y = "Length (mm)") +
  theme_minimal()

p4 <- ggplot(pine_data, aes(sample = len_mm)) +
  geom_qq() + 
  geom_qq_line() +
  labs(title = "QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combine plots
(p1 + p2) / (p3 + p4)


ggplot(pine_data, aes(x = len_mm)) +
  geom_histogram(aes(y = ..density..), 
                 fill = "lightblue", 
                 color = "black", bins = 10) + 
  geom_density(alpha = 0.5, 
               fill = "steelblue") + 
  labs(title = "Pine Needle Length Distribution", 
       x = "Length (mm)", 
       y = "Density") + theme_minimal()



