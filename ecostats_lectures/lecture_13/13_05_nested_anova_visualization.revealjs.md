---
title: "Lecture 13 - NESTED ANOVA Visualization"
author: "Bill Perry"
prefer-html: true
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "13_05_nested_anova_visual_html.html"
  revealjs:
    output-file: "13_05_nested_anovat_visual_slides.html"
---

::: {.cell}

```{.r .cell-code}
# Load necessary libraries
library(tidyverse)
library(patchwork)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(knitr)

library(viridis)

# Custom theme for consistent plotting
my_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8)
  )
```
:::




# Introduction

This document provides a comparison between one-way ANOVA and nested ANOVA using the `urchin_df` dataset from Quinn & Keough (2002). This tutorial aims to help students understand how the partitioning of variance differs between these two approaches and why accounting for nested factors is crucial in certain experimental designs.

In the `urchin_df` dataset, the experimental design consists of:

-   Four urchin density **treatments** (`treat`): Control, 66% Density, 33% Density, and Removed
-   Each treatment was replicated within four random **patches** (`patch`)
-   Five replicate **quadrats** were measured within each treatment-patch combination
-   The response variable is percentage cover of filamentous **algae**

## Dataset Overview

First, let's load and explore the dataset:




::: {.cell}

```{.r .cell-code}
# Read in the urchin_df dataset
urchin_df <- read_csv("data/andrew.csv")


# Summary statistics
summary_stats <- urchin_df %>%
  group_by(treat) %>%
  summarise(
    n = n(),
    mean = mean(algae),
    sd = sd(algae),
    se = sd / sqrt(n),
    min = min(algae),
    max = max(algae),
    .groups = 'drop'
  )

summary_stats
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 4 × 7
  treat       n  mean    sd    se   min   max
  <chr>   <int> <dbl> <dbl> <dbl> <dbl> <dbl>
1 control    20   1.3  3.18 0.711     0    13
2 dens_33    20  21.6 25.1  5.62      0    79
3 dens_66    20  19   25.7  5.74      0    71
4 removal    20  39.2 28.7  6.41      0    83
```


:::
:::




Let's visualize the data distribution by treatment:




::: {.cell}

```{.r .cell-code}
# Create boxplot of algae cover by treatment
ggplot(urchin_df, aes(x = treat, y = algae, fill = treat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Effect of Urchin Density on Filamentous algae Cover",
    x = "Urchin Density treatment",
    y = "Filamentous algae Cover (%)"
  ) +
  theme(legend.position = "none")
```

::: {.cell-output-display}
![](13_05_nested_anova_visualization_files/figure-html/visualize-data-1.png){width=672}
:::
:::




# One-way ANOVA

In a one-way ANOVA, we ignore the nested structure of the data and simply compare the means of the four treatment groups.




::: {.cell}

```{.r .cell-code}
# 1. Fit one-way ANOVA
oneway_model <- aov(algae ~ treat, data = urchin_df)
oneway_summary <- summary(oneway_model)

# Display one-way ANOVA results
oneway_summary
```

::: {.cell-output .cell-output-stdout}

```
            Df Sum Sq Mean Sq F value   Pr(>F)    
treat        3  14429    4810   9.059 3.36e-05 ***
Residuals   76  40352     531                     
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::




In this one-way ANOVA, we find a significant effect of treatment on algae cover (F(3, 76) = 9.06, p = 0).

# Nested ANOVA

Now, let's run a nested ANOVA that accounts for the hierarchical structure where patches are nested within treatments.




::: {.cell}

```{.r .cell-code}
# 2. Fit nested ANOVA model
nested_model <- aov(algae ~ treat + treat:patch, data = urchin_df)
nested_summary <- summary(nested_model)

# Display nested ANOVA results
nested_summary
```

::: {.cell-output .cell-output-stdout}

```
            Df Sum Sq Mean Sq F value   Pr(>F)    
treat        3  14429    4810   9.988 1.39e-05 ***
treat:patch  4   5681    1420   2.949   0.0257 *  
Residuals   72  34671     482                     
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::




**Important Note**: The ANOVA table above does not use the correct error terms for testing the treatment effect. In a nested design, the treatment effect should be tested against the patch variation, not the residual error.

## Corrected Nested ANOVA with Proper F-tests

Let's calculate the correct F-ratios and p-values for the nested design:




::: {.cell}

```{.r .cell-code}
# Extract MS values
MS_treat <- nested_summary[[1]]["treat      ", "Mean Sq"] 
MS_patch <- nested_summary[[1]]["treat:patch", "Mean Sq"]
MS_residual <- nested_summary[[1]]["Residuals", "Mean Sq"]

# Extract df values
df_treat <- nested_summary[[1]]["treat      ", "Df"]
df_patch <- nested_summary[[1]]["treat:patch", "Df"]
df_residual <- nested_summary[[1]]["Residuals", "Df"]

# Calculate correct F ratios for nested design
F_treat <- MS_treat / MS_patch
F_patch <- MS_patch / MS_residual

# Calculate p-values using the correct denominator df
p_treat <- pf(F_treat, df_treat, df_patch, lower.tail = FALSE)
p_patch <- pf(F_patch, df_patch, df_residual, lower.tail = FALSE)

# Create ANOVA table with corrected F-tests
anova_table <- data.frame(
  Source = c("treatment", "patches (within treatment)", "Residual"),
  df = c(df_treat, df_patch, df_residual),
  SS = c(nested_summary[[1]]["treat      ", "Sum Sq"], 
         nested_summary[[1]]["treat:patch", "Sum Sq"], 
         nested_summary[[1]]["Residuals", "Sum Sq"]),
  MS = c(MS_treat, MS_patch, MS_residual),
  F = c(F_treat, F_patch, NA),
  p = c(p_treat, p_patch, NA)
)

# Format p-values
anova_table$p <- ifelse(anova_table$p < 0.001, "<0.001", 
                       ifelse(is.na(anova_table$p), NA, 
                              format(anova_table$p, digits = 3)))

# Display corrected ANOVA table
anova_table
```

::: {.cell-output .cell-output-stdout}

```
                      Source df       SS        MS        F      p
1                  treatment  3 14429.14 4809.7125 3.386424 0.1347
2 patches (within treatment)  4  5681.17 1420.2925 2.949454 0.0257
3                   Residual 72 34671.18  481.5442       NA   <NA>
```


:::
:::




With the corrected nested ANOVA, we find:

1.  The treatment effect is not significant (F = 3.39, p = 0.135) when tested against the patch variation.
2.  There is significant variation among patches within treatments (F = 2.95, p = 0.026)

This is a different conclusion than the one-way ANOVA, which found a significant treatment effect.

# Variance Decomposition Comparison

## Visual Decomposition of Variance Components

First, let's create a visual representation of how variance is partitioned in a standard one-way ANOVA, and then contrast it with how a nested ANOVA further divides the variance components.




::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "treatment means:"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 4 × 2
  treat   treat_mean
  <chr>        <dbl>
1 control        1.3
2 dens_33       21.6
3 dens_66       19  
4 removal       39.2
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "First few rows of joined patch_means:"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 4
  treat   patch patch_mean treat_mean
  <chr>   <dbl>      <dbl>      <dbl>
1 control     1        1.6        1.3
2 control     2        0          1.3
3 control     3        1          1.3
4 control     4        2.6        1.3
5 dens_33     5       28.4       21.6
6 dens_33     6       36.8       21.6
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "treat_mean column is present in patch_means"
```


:::

::: {.cell-output-display}
![](13_05_nested_anova_visualization_files/figure-html/visualization-of-variance-1.png){width=576}
:::

::: {.cell-output-display}
![](13_05_nested_anova_visualization_files/figure-html/visualization-of-variance-2.png){width=576}
:::
:::




The plots above visually demonstrate the key differences in how variance is partitioned between one-way and nested ANOVA:

1.  **One-way ANOVA** (first plot):
    -   Total variance is split into just two components: Among Groups (treatment) and Within Groups (Error)
    -   The Within Groups component includes all variation not explained by treatments
2.  **Nested ANOVA** (second plot):
    -   Total variance is split into three components: Among treatments, Among patches within treatments, and Within patches (Residual Error)
    -   The important addition is the "Among patches within treatments" component, which captures the spatial heterogeneity
    -   The actual residual error (Within patches) is smaller than the Within Groups error in one-way ANOVA

This visualization demonstrates why we get different conclusions: in one-way ANOVA, the patch-to-patch variation is incorrectly included in the error term, leading to an artificially inflated F-ratio for treatments.

## Numerical Decomposition of Variance




::: {.cell}

```{.r .cell-code}
# Calculate sums of squares for both models
SS_Total <- sum(nested_summary[[1]][, "Sum Sq"])
SS_treatment <- nested_summary[[1]]["treat      ", "Sum Sq"]
SS_patch_within_treatment <- nested_summary[[1]]["treat:patch", "Sum Sq"]
SS_Error_Nested <- nested_summary[[1]]["Residuals", "Sum Sq"]
SS_Error_OneWay <- oneway_summary[[1]]["Residuals", "Sum Sq"]

# Calculate percentages for visualization
percent_treatment_oneway <- (SS_treatment / SS_Total) * 100
percent_error_oneway <- (SS_Error_OneWay / SS_Total) * 100

percent_treatment_nested <- (SS_treatment / SS_Total) * 100
percent_patch_nested <- (SS_patch_within_treatment / SS_Total) * 100
percent_error_nested <- (SS_Error_Nested / SS_Total) * 100

# Create data frame for visualization
ss_comparison <- data.frame(
  Model = c(
    rep("One-way ANOVA", 2), 
    rep("Nested ANOVA", 3)
  ),
  Source = c(
    "treatment", "Error (within)",
    "treatment", "patch(treatment)", "Error"
  ),
  Percent = c(
    percent_treatment_oneway, percent_error_oneway,
    percent_treatment_nested, percent_patch_nested, percent_error_nested
  ),
  SS = c(
    SS_treatment, SS_Error_OneWay,
    SS_treatment, SS_patch_within_treatment, SS_Error_Nested
  )
)

# Add factor levels for ordering
ss_comparison$Source <- factor(
  ss_comparison$Source,
  levels = c("treatment", "patch(treatment)", "Error", "Error (within)")
)

# Create colors for the sources
source_colors <- c(
  "treatment" = "#1b9e77", 
  "patch(treatment)" = "#d95f02", 
  "Error" = "#7570b3",
  "Error (within)" = "#7570b3"
)

# Create the stacked bar plot
p1 <- ggplot(ss_comparison, aes(x = Model, y = Percent, fill = Source)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = source_colors) +
  labs(
    title = "Comparison of Variance Partitioning",
    subtitle = "One-way vs. Nested ANOVA",
    x = "",
    y = "Percentage of Total Variance",
    fill = "Source of Variation"
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percent)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    axis.text.x = element_text(face = "bold", size = 12)
  )

# Create a pie chart version for one-way ANOVA
oneway_data <- ss_comparison %>% filter(Model == "One-way ANOVA")
oneway_data$ypos <- cumsum(oneway_data$Percent) - 0.5 * oneway_data$Percent

p2 <- ggplot(oneway_data, aes(x = "", y = Percent, fill = Source)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(
    title = "One-way ANOVA",
    subtitle = "Variance Components",
    fill = "Source of Variation"
  ) +
  scale_fill_manual(values = source_colors) +
  geom_text(
    aes(y = ypos, label = sprintf("%.1f%%", Percent)),
    color = "white",
    fontface = "bold"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Create a pie chart version for nested ANOVA
nested_data <- ss_comparison %>% filter(Model == "Nested ANOVA")
nested_data$ypos <- cumsum(nested_data$Percent) - 0.5 * nested_data$Percent

p3 <- ggplot(nested_data, aes(x = "", y = Percent, fill = Source)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(
    title = "Nested ANOVA",
    subtitle = "Variance Components",
    fill = "Source of Variation"
  ) +
  scale_fill_manual(values = source_colors) +
  geom_text(
    aes(y = ypos, label = sprintf("%.1f%%", Percent)),
    color = "white",
    fontface = "bold"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Display all plots
p1
```

::: {.cell-output-display}
![](13_05_nested_anova_visualization_files/figure-html/variance-components-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
# Combine the pie charts
p2 + p3 + plot_layout(ncol = 2)
```

::: {.cell-output-display}
![](13_05_nested_anova_visualization_files/figure-html/pie-charts-1.png){width=672}
:::
:::




# Mixed-Effects Model Approach

A modern way to analyze nested designs is to use mixed-effects models. Let's compare the results with our previous analyses:




::: {.cell}

```{.r .cell-code}
# Fit linear mixed model with patch nested in treat
mixed_model <- lmer(algae ~ treat + (1|treat:patch), data = urchin_df)

# Show model summary
summary(mixed_model)
```

::: {.cell-output .cell-output-stdout}

```
Linear mixed model fit by REML. t-tests use Satterthwaite's method [
lmerModLmerTest]
Formula: algae ~ treat + (1 | treat:patch)
   Data: urchin_df

REML criterion at convergence: 682.2

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.9808 -0.3106 -0.1093  0.2831  2.5910 

Random effects:
 Groups      Name        Variance Std.Dev.
 treat:patch (Intercept) 294.3    17.16   
 Residual                298.6    17.28   
Number of obs: 80, groups:  treat:patch, 16

Fixed effects:
             Estimate Std. Error     df t value Pr(>|t|)  
(Intercept)     1.300      9.408 12.000   0.138   0.8924  
treatdens_33   20.250     13.305 12.000   1.522   0.1539  
treatdens_66   17.700     13.305 12.000   1.330   0.2081  
treatremoval   37.900     13.305 12.000   2.849   0.0147 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
            (Intr) trt_33 trt_66
treatdns_33 -0.707              
treatdns_66 -0.707  0.500       
treatremovl -0.707  0.500  0.500
```


:::

```{.r .cell-code}
# ANOVA-style results
anova(mixed_model, type = 3, ddf = "Satterthwaite")
```

::: {.cell-output .cell-output-stdout}

```
Type III Analysis of Variance Table with Satterthwaite's method
      Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)  
treat   2434  811.33     3    12  2.7171 0.09126 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::




The mixed model approach gives us similar conclusions to the correctly specified nested ANOVA. The treatment effect is non-significant when accounting for the nested structure of patches within treatments.

# Visualizing the Nested Structure

One way to understand why we get different results is to visualize the data by patch within treatment:




::: {.cell}

```{.r .cell-code}
# Calculate means for each patch
patch_means <- urchin_df %>%
  group_by(treat, patch) %>%
  summarize(algae_mean = mean(algae), .groups = "drop")

# Plot patch means
ggplot() +
  # Plot patch means
  geom_point(data = patch_means, 
             aes(x = treat, y = algae_mean, color = patch),
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
             size = 3) +
  # Add treatment means
  geom_point(data = summary_stats, 
             aes(x = treat, y = mean),
             size = 5, shape = 18, color = "black") +
  # Add error bars for treatment means
  geom_errorbar(data = summary_stats,
                aes(x = treat, ymin = mean - se, ymax = mean + se),
                width = 0.2, color = "black", linewidth = 1) +
  # Plot styling
  labs(
    title = "Nested Structure: patch Means within treatments",
    subtitle = "Large symbols show treatment means (± SE)",
    x = "Urchin Density treatment",
    y = "Mean Filamentous algae Cover (%)",
    color = "patch"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
```

::: {.cell-output-display}
![](13_05_nested_anova_visualization_files/figure-html/nested-structure-viz-1.png){width=672}
:::
:::




This plot clearly shows the high variation among patches within each treatment. This patch-to-patch variation contributes substantially to the total variance, which is not accounted for in the one-way ANOVA.

# F-ratio Demonstration

Let's illustrate how the F-ratio for treatments differs between one-way and nested ANOVA:




::: {.cell}

```{.r .cell-code}
# Create a data frame for comparison
f_ratio_comparison <- data.frame(
  Model = c("One-way ANOVA", "Nested ANOVA"),
  F_numerator = c("MS treatment", "MS treatment"),
  F_denominator = c("MS Residual", "MS patch(treatment)"),
  F_value = c(oneway_summary[[1]][1, "F value"], F_treat),
  p_value = c(oneway_summary[[1]][1, "Pr(>F)"], p_treat)
)

# Display the comparison
f_ratio_comparison
```

::: {.cell-output .cell-output-stdout}

```
          Model  F_numerator       F_denominator  F_value      p_value
1 One-way ANOVA MS treatment         MS Residual 9.058658 3.362391e-05
2  Nested ANOVA MS treatment MS patch(treatment) 3.386424 1.347006e-01
```


:::
:::




# Key Differences and Implications

The comparison between one-way ANOVA and nested ANOVA reveals several important differences:

1.  **Variance Partitioning**:
    -   In one-way ANOVA, all variation not explained by treatments is lumped into the "Error" term.
    -   In nested ANOVA, this variation is partitioned into "patch(treatment)" and "Error" components.
2.  **F-ratio for Testing treatment Effects**:
    -   One-way ANOVA tests treatments against residual error (MS treatment / MS Residual).
    -   Nested ANOVA tests treatments against patch variation (MS treatment / MS patch(treatment)).
3.  **Biological Interpretation**:
    -   One-way ANOVA suggests significant treatment effects (p = 0).
    -   Nested ANOVA reveals that most variation is among patches, with non-significant treatment effects (p = 0.135).
    -   The substantial patch variability (38.8% of total variation) masks the treatment effect when properly accounted for.
4.  **Statistical Power and Type I Error**:
    -   Ignoring the nested structure leads to pseudoreplication and inflated Type I error rates.
    -   The one-way ANOVA effectively treats each quadrat as an independent sample, inflating the degrees of freedom for the error term.

# Conclusion

This demonstration illustrates why accounting for hierarchical or nested structures in experimental designs is crucial for valid statistical inference. Failure to account for such structures can lead to:

1.  Pseudoreplication (treating non-independent samples as independent)
2.  Inflation of Type I error rates (finding spurious "significant" effects)
3.  Incorrect partitioning of variance sources
4.  Misleading biological interpretations

In the `urchin_df` dataset, the nested ANOVA reveals that spatial heterogeneity (patch-to-patch variation) is the dominant factor influencing algae cover, not the experimental treatment. This insight would be missed if only a one-way ANOVA were used.

## Alternative Code for Mixed Models

For advanced users, we could also fit this as a mixed model with `lmerTest`:




::: {.cell}

```{.r .cell-code}
library(lmerTest)

# Mixed model with patches nested within treatments
mixed_model_alt <- lmer(algae ~ treat + (1|treat:patch), data = urchin_df)
anova(mixed_model_alt, type = 3, ddf = "Satterthwaite")
```

::: {.cell-output .cell-output-stdout}

```
Type III Analysis of Variance Table with Satterthwaite's method
      Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)  
treat   2434  811.33     3    12  2.7171 0.09126 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# We could also try a simpler random effects structure
simple_mixed_model <- lmer(algae ~ treat + (1|patch), data = urchin_df)
anova(simple_mixed_model, type = 3, ddf = "Satterthwaite")
```

::: {.cell-output .cell-output-stdout}

```
Type III Analysis of Variance Table with Satterthwaite's method
      Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)  
treat   2434  811.33     3    12  2.7171 0.09126 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::




Mixed models provide a more flexible approach to handle nested designs and are the recommended approach in modern statistical practice.