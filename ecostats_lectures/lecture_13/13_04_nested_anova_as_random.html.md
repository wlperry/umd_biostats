---
title: "Lecture 13 - NESTED ANOVA"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "13_04_nested_anova_random_html.html"
  revealjs:
    output-file: "13_04_nested_anova_random_slides.html"
---



# Introduction

This analysis examines the effects of varying sea urchin densities on
the percentage cover of filamentous algae. The experiment was designed
with four urchin density treatments (control, 66% of original density,
33% of original density, and all urchins removed) nested within four
random patches. Five replicate quadrats were measured within each
treatment-patch combination.

The traditional nested ANOVA approach can be implemented using a linear
mixed-effects model, which provides a more flexible framework for
analyzing hierarchical designs. In this case, we'll use the `lme4`
package to fit a model where treatment is a fixed effect and patch is a
random effect nested within treatment.

# Data Overview

The dataframe contains 80 observations with the
following variables:

-   treat: Urchin density treatment (Control, 66% Density, 33% Density,
    Removed)
-   patch: Random patches (1-16) where treatments were applied
-   QUAD: Replicate quadrats within each treatment-patch combination
-   algae: Percentage cover of filamentous algae (response variable)


::: {.cell}

```{.r .cell-code}
# Create a summary table with flextable

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
  <fct>   <int> <dbl> <dbl> <dbl> <dbl> <dbl>
1 control    20   1.3  3.18 0.711     0    13
2 dens_33    20  21.6 25.1  5.62      0    79
3 dens_66    20  19   25.7  5.74      0    71
4 removal    20  39.2 28.7  6.41      0    83
```


:::

```{.r .cell-code}
# 
# summary_stats %>%
#   select(treat, n, mean, sd, se, min, max) %>%
#   flextable() %>%
#   set_header_labels(
#     treat = "treatment",
#     n = "N",
#     mean = "Mean",
#     sd = "SD",
#     se = "SE",
#     min = "Min",
#     max = "Max"
#   ) %>%
#   colformat_double(j = c("mean", "sd", "se", "min", "max"), digits = 2) %>%
#   autofit() %>%
#   add_header_lines("Summary statistics of algae cover (%) across treatments") %>%
#   theme_box()
```
:::


# Mixed Model Analysis

In this experimental design, patch is nested within treat because each
patch received only one treatment level. This hierarchical design is
well-suited for analysis using linear mixed-effects models.

## Model Specification

We'll use the following model specification:

$algae_{ijk} = \mu + \alpha_i + \beta_{j(i)} + \epsilon_{ijk}$

Where: - $\mu$ is the overall mean - $\alpha_i$ is the fixed effect of
treatment $i$ - $\beta_{j(i)}$ is the random effect of patch $j$ nested
within treatment $i$ - $\epsilon_{ijk}$ is the residual error for
quadrat $k$ in patch $j$ within treatment $i$

In `lme4`, this model is specified as


::: {.cell}

```{.r .cell-code}
# Fit the mixed model
mixed_model <- lmer(algae ~ treat + (1|treat:patch), data = urchin_df)

# Display model summary
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
:::


## ANOVA Table

The ANOVA table for the mixed model:


::: {.cell}

```{.r .cell-code}
# Get ANOVA table with Type III tests
anova_table <- anova(mixed_model, type = 3)
print(anova_table)
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
# For comparison, also run a traditional nested ANOVA
nested_aov <- aov(algae ~ treat + treat:patch, data = urchin_df)
std_summary <- summary(nested_aov)[[1]]

# Extract MS values - using exact row names
MS_treat <- std_summary["treat      ", "Mean Sq"] 
MS_patch <- std_summary["treat:patch", "Mean Sq"]
MS_residual <- std_summary["Residuals", "Mean Sq"]

# Print MS values to check
print("MS values:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "MS values:"
```


:::

```{.r .cell-code}
print(c(treatment = MS_treat, patches = MS_patch, Residual = MS_residual))
```

::: {.cell-output .cell-output-stdout}

```
treatment   patches  Residual 
 4809.712  1770.162   298.600 
```


:::

```{.r .cell-code}
# Extract df values
df_treat <- std_summary["treat      ", "Df"]
df_patch <- std_summary["treat:patch", "Df"]
df_residual <- std_summary["Residuals", "Df"]


# Calculate correct F ratios for nested design
F_treat <- MS_treat / MS_patch
F_patch <- MS_patch / MS_residual

# Calculate p-values
p_treat <- pf(F_treat, df_treat, df_patch, lower.tail = FALSE)
p_patch <- pf(F_patch, df_patch, df_residual, lower.tail = FALSE)

# Create ANOVA table
trad_anova_table <- data.frame(
  Source = c("treatment", "patches (treatment)", "Residual"),
  df = c(df_treat, df_patch, df_residual),
  MS = c(MS_treat, MS_patch, MS_residual),
  F = c(F_treat, F_patch, NA),
  p = c(p_treat, p_patch, NA)
)

# Format p-values
trad_anova_table$p <- ifelse(trad_anova_table$p < 0.001, "<0.001",
                       ifelse(is.na(trad_anova_table$p), NA,
                              format(trad_anova_table$p, digits = 3)))
trad_anova_table
```

::: {.cell-output .cell-output-stdout}

```
               Source df       MS        F           p
1           treatment  3 4809.712 2.717102 0.091262004
2 patches (treatment) 12 1770.162 5.928207      <0.001
3            Residual 64  298.600       NA        <NA>
```


:::

```{.r .cell-code}
# # Display traditional ANOVA table with flextable
# trad_anova_table %>%
#   flextable() %>%
#   set_header_labels(
#     Source = "Source of variation",
#     df = "df",
#     MS = "MS",
#     F = "F",
#     p = "p"
#   ) %>%
#   colformat_double(j = c("MS", "F"), digits = 2) %>%
#   autofit() %>%
#   add_header_lines("ANOVA table for nested design") %>%
#   theme_box()
```
:::


## Variance Components

We can extract the variance components from the mixed model:


::: {.cell}

```{.r .cell-code}
# Print corrected results

# Extract variance components
vc <- VarCorr(mixed_model)
print(vc)
```

::: {.cell-output .cell-output-stdout}

```
 Groups      Name        Std.Dev.
 treat:patch (Intercept) 17.156  
 Residual                17.280  
```


:::

```{.r .cell-code}
# Extract variance components
var_comp_patch <- as.numeric(vc$`treat:patch`)
var_comp_residual <- attr(vc, "sc")^2

# Calculate percentage of total variance
total_var <- var_comp_patch + var_comp_residual
pct_patch <- var_comp_patch / total_var * 100
pct_residual <- var_comp_residual / total_var * 100

# Calculate treatment variance component
n_quad <- 5  # Number of quadrats per patch
n_patch <- 4  # Number of patches per treatment
var_comp_treatment <- (MS_treat - MS_patch) / (n_quad * n_patch)

# Format variance components for display
var_comp_treatment_display <- ifelse(var_comp_treatment < 0, 
                                    paste0("(", format(abs(var_comp_treatment), digits = 2), ")"),
                                    format(var_comp_treatment, digits = 2))

# Create variance components table
var_comp_table <- data.frame(
  Source = c("treatment", "patches (treatment)", "Residual"),
  `Var.comp` = c(var_comp_treatment_display, 
               format(var_comp_patch, digits = 2),
               format(var_comp_residual, digits = 2))
)

# Display variance components table
var_comp_table %>%
  flextable() %>%
  set_header_labels(
    Source = "Source of variation",
    Var.comp = "Variance component"
  ) %>%
  autofit() %>%
  add_header_lines("Variance components") %>%
  theme_box()
```

::: {.cell-output-display}

```{=html}
<div class="tabwid"><style>.cl-a95df12a{}.cl-a95a453e{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a95a4548{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a95bb770{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a95bc508{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc509{width:1.687in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc512{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc513{width:1.687in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc51c{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc51d{width:1.687in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc51e{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a95bc526{width:1.687in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-a95df12a'><thead><tr style="overflow-wrap:break-word;"><th  colspan="2"class="cl-a95bc508"><p class="cl-a95bb770"><span class="cl-a95a453e">Variance components</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-a95bc508"><p class="cl-a95bb770"><span class="cl-a95a453e">Source of variation</span></p></th><th class="cl-a95bc509"><p class="cl-a95bb770"><span class="cl-a95a453e">Variance component</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a95bc512"><p class="cl-a95bb770"><span class="cl-a95a4548">treatment</span></p></td><td class="cl-a95bc513"><p class="cl-a95bb770"><span class="cl-a95a4548">152</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a95bc51c"><p class="cl-a95bb770"><span class="cl-a95a4548">patches (treatment)</span></p></td><td class="cl-a95bc51d"><p class="cl-a95bb770"><span class="cl-a95a4548">294</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a95bc51e"><p class="cl-a95bb770"><span class="cl-a95a4548">Residual</span></p></td><td class="cl-a95bc526"><p class="cl-a95bb770"><span class="cl-a95a4548">299</span></p></td></tr></tbody></table></div>
```

:::

```{.r .cell-code}
# Complete table with all information
complete_table <- data.frame(
  Source = c("treatment", "patches (treatment)", "Residual"),
  df = c(df_treat, df_patch, df_residual),
  MS = c(MS_treat, MS_patch, MS_residual),
  F = c(F_treat, F_patch, NA),
  p = c(trad_anova_table$p[1], trad_anova_table$p[2], NA),
  `Var.comp` = c(var_comp_treatment_display, 
                format(var_comp_patch, digits = 2),
                format(var_comp_residual, digits = 2))
)

# Display complete table
complete_table %>%
  flextable() %>%
  set_header_labels(
    Source = "Source of variation",
    df = "df",
    MS = "MS",
    F = "F",
    p = "p",
    Var.comp = "Var. comp."
  ) %>%
  colformat_double(j = c("MS", "F"), digits = 2) %>%
  autofit() %>%
  add_header_lines("Complete ANOVA table with variance components") %>%
  theme_box()
```

::: {.cell-output-display}

```{=html}
<div class="tabwid"><style>.cl-a96df84a{}.cl-a96ac026{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a96ac030{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a96bf626{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a96bf627{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a96c0292{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c029c{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c029d{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c029e{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02a6{width:1.177in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02a7{width:1.024in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02a8{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02a9{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02b0{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02b1{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02b2{width:1.177in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02ba{width:1.024in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02bb{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02bc{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02c4{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02c5{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02ce{width:1.177in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02cf{width:1.024in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02d0{width:1.61in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02d8{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02e2{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02e3{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02e4{width:1.177in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a96c02e5{width:1.024in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-a96df84a'><thead><tr style="overflow-wrap:break-word;"><th  colspan="6"class="cl-a96c0292"><p class="cl-a96bf626"><span class="cl-a96ac026">Complete ANOVA table with variance components</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-a96c0292"><p class="cl-a96bf626"><span class="cl-a96ac026">Source of variation</span></p></th><th class="cl-a96c029c"><p class="cl-a96bf627"><span class="cl-a96ac026">df</span></p></th><th class="cl-a96c029d"><p class="cl-a96bf627"><span class="cl-a96ac026">MS</span></p></th><th class="cl-a96c029e"><p class="cl-a96bf627"><span class="cl-a96ac026">F</span></p></th><th class="cl-a96c02a6"><p class="cl-a96bf626"><span class="cl-a96ac026">p</span></p></th><th class="cl-a96c02a7"><p class="cl-a96bf626"><span class="cl-a96ac026">Var. comp.</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a96c02a8"><p class="cl-a96bf626"><span class="cl-a96ac030">treatment</span></p></td><td class="cl-a96c02a9"><p class="cl-a96bf627"><span class="cl-a96ac030">3</span></p></td><td class="cl-a96c02b0"><p class="cl-a96bf627"><span class="cl-a96ac030">4,809.71</span></p></td><td class="cl-a96c02b1"><p class="cl-a96bf627"><span class="cl-a96ac030">2.72</span></p></td><td class="cl-a96c02b2"><p class="cl-a96bf626"><span class="cl-a96ac030">0.091262004</span></p></td><td class="cl-a96c02ba"><p class="cl-a96bf626"><span class="cl-a96ac030">152</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a96c02bb"><p class="cl-a96bf626"><span class="cl-a96ac030">patches (treatment)</span></p></td><td class="cl-a96c02bc"><p class="cl-a96bf627"><span class="cl-a96ac030">12</span></p></td><td class="cl-a96c02c4"><p class="cl-a96bf627"><span class="cl-a96ac030">1,770.16</span></p></td><td class="cl-a96c02c5"><p class="cl-a96bf627"><span class="cl-a96ac030">5.93</span></p></td><td class="cl-a96c02ce"><p class="cl-a96bf626"><span class="cl-a96ac030">&lt;0.001</span></p></td><td class="cl-a96c02cf"><p class="cl-a96bf626"><span class="cl-a96ac030">294</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a96c02d0"><p class="cl-a96bf626"><span class="cl-a96ac030">Residual</span></p></td><td class="cl-a96c02d8"><p class="cl-a96bf627"><span class="cl-a96ac030">64</span></p></td><td class="cl-a96c02e2"><p class="cl-a96bf627"><span class="cl-a96ac030">298.60</span></p></td><td class="cl-a96c02e3"><p class="cl-a96bf627"><span class="cl-a96ac030"></span></p></td><td class="cl-a96c02e4"><p class="cl-a96bf626"><span class="cl-a96ac030"></span></p></td><td class="cl-a96c02e5"><p class="cl-a96bf626"><span class="cl-a96ac030">299</span></p></td></tr></tbody></table></div>
```

:::
:::


::: {.callout-important appearance="simple"}
**Interpretation of ANOVA Results**

The nested ANOVA using mixed models reveals that there was no
significant effect of urchin density treatment on algae cover (F =
2.72, df = 3, 12, p =
0.0913). However, there was significant variation
among patches within treatments (F = 5.93, df =
12, 64, p \< 0.001).

The variance component for patches nested within treatments
(294) indicates substantial spatial
heterogeneity in algae cover, highlighting the importance of accounting
for this spatial variation in the analysis. The negative variance
component for treatment suggests that there is more variation among
patches within treatments than among treatments themselves.
:::

# Lecture 13: Post-hoc Comparisons

Although the main effect of treatment was not significant in the nested
ANOVA (p = r format(p_treat, digits=3)), we can still examine the mean
differences between treatments to understand patterns in the data.
However, we should interpret these with caution given the lack of
statistical significance at the α = 0.05 level.


::: {.cell}

```{.r .cell-code}
# Calculate estimated marginal means
emm <- emmeans(mixed_model, ~ treat)

# Display EMMs with flextable
as.data.frame(summary(emm)) %>%
  flextable() %>%
  set_header_labels(
    treat = "treatment",
    emmean = "Estimated Marginal Mean",
    SE = "Standard Error",
    df = "df",
    lower.CL = "Lower CL",
    upper.CL = "Upper CL"
  ) %>%
  colformat_double(j = c("emmean", "SE", "lower.CL", "upper.CL"), digits = 2) %>%
  autofit() %>%
  add_header_lines("Estimated marginal means for each treatment") %>%
  theme_box()
```

::: {.cell-output-display}

```{=html}
<div class="tabwid"><style>.cl-a9986c88{}.cl-a99502a0{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a99502aa{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a9966a28{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a9966a32{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a9967afe{width:0.931in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b08{width:2.017in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b09{width:1.287in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b0a{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b12{width:0.939in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b13{width:0.931in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b1c{width:2.017in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b1d{width:1.287in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b26{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b27{width:0.939in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b30{width:0.931in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b31{width:2.017in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b3a{width:1.287in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b3b{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b44{width:0.939in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b45{width:0.931in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b46{width:2.017in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b4e{width:1.287in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b58{width:0.455in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9967b59{width:0.939in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0.75pt solid rgba(102, 102, 102, 1.00);border-right: 0.75pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-a9986c88'><thead><tr style="overflow-wrap:break-word;"><th  colspan="6"class="cl-a9967afe"><p class="cl-a9966a28"><span class="cl-a99502a0">Estimated marginal means for each treatment</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-a9967afe"><p class="cl-a9966a28"><span class="cl-a99502a0">treatment</span></p></th><th class="cl-a9967b08"><p class="cl-a9966a32"><span class="cl-a99502a0">Estimated Marginal Mean</span></p></th><th class="cl-a9967b09"><p class="cl-a9966a32"><span class="cl-a99502a0">Standard Error</span></p></th><th class="cl-a9967b0a"><p class="cl-a9966a32"><span class="cl-a99502a0">df</span></p></th><th class="cl-a9967b12"><p class="cl-a9966a32"><span class="cl-a99502a0">Lower CL</span></p></th><th class="cl-a9967b12"><p class="cl-a9966a32"><span class="cl-a99502a0">Upper CL</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-a9967b13"><p class="cl-a9966a28"><span class="cl-a99502aa">control</span></p></td><td class="cl-a9967b1c"><p class="cl-a9966a32"><span class="cl-a99502aa">1.30</span></p></td><td class="cl-a9967b1d"><p class="cl-a9966a32"><span class="cl-a99502aa">9.41</span></p></td><td class="cl-a9967b26"><p class="cl-a9966a32"><span class="cl-a99502aa">12</span></p></td><td class="cl-a9967b27"><p class="cl-a9966a32"><span class="cl-a99502aa">-19.20</span></p></td><td class="cl-a9967b27"><p class="cl-a9966a32"><span class="cl-a99502aa">21.80</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a9967b30"><p class="cl-a9966a28"><span class="cl-a99502aa">dens_33</span></p></td><td class="cl-a9967b31"><p class="cl-a9966a32"><span class="cl-a99502aa">21.55</span></p></td><td class="cl-a9967b3a"><p class="cl-a9966a32"><span class="cl-a99502aa">9.41</span></p></td><td class="cl-a9967b3b"><p class="cl-a9966a32"><span class="cl-a99502aa">12</span></p></td><td class="cl-a9967b44"><p class="cl-a9966a32"><span class="cl-a99502aa">1.05</span></p></td><td class="cl-a9967b44"><p class="cl-a9966a32"><span class="cl-a99502aa">42.05</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a9967b30"><p class="cl-a9966a28"><span class="cl-a99502aa">dens_66</span></p></td><td class="cl-a9967b31"><p class="cl-a9966a32"><span class="cl-a99502aa">19.00</span></p></td><td class="cl-a9967b3a"><p class="cl-a9966a32"><span class="cl-a99502aa">9.41</span></p></td><td class="cl-a9967b3b"><p class="cl-a9966a32"><span class="cl-a99502aa">12</span></p></td><td class="cl-a9967b44"><p class="cl-a9966a32"><span class="cl-a99502aa">-1.50</span></p></td><td class="cl-a9967b44"><p class="cl-a9966a32"><span class="cl-a99502aa">39.50</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-a9967b45"><p class="cl-a9966a28"><span class="cl-a99502aa">removal</span></p></td><td class="cl-a9967b46"><p class="cl-a9966a32"><span class="cl-a99502aa">39.20</span></p></td><td class="cl-a9967b4e"><p class="cl-a9966a32"><span class="cl-a99502aa">9.41</span></p></td><td class="cl-a9967b58"><p class="cl-a9966a32"><span class="cl-a99502aa">12</span></p></td><td class="cl-a9967b59"><p class="cl-a9966a32"><span class="cl-a99502aa">18.70</span></p></td><td class="cl-a9967b59"><p class="cl-a9966a32"><span class="cl-a99502aa">59.70</span></p></td></tr></tbody></table></div>
```

:::
:::


# Lecture 13: Tukey Pairwise Comparisons

-   text


::: {.cell}

```{.r .cell-code}
# Pairwise comparisons with Tukey adjustment
pairs <- pairs(emm, adjust = "tukey")
pairs
```

::: {.cell-output .cell-output-stdout}

```
 contrast          estimate   SE df t.ratio p.value
 control - dens_33   -20.25 13.3 12  -1.522  0.4553
 control - dens_66   -17.70 13.3 12  -1.330  0.5625
 control - removal   -37.90 13.3 12  -2.849  0.0615
 dens_33 - dens_66     2.55 13.3 12   0.192  0.9974
 dens_33 - removal   -17.65 13.3 12  -1.327  0.5646
 dens_66 - removal   -20.20 13.3 12  -1.518  0.4573

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 4 estimates 
```


:::

```{.r .cell-code}
# # Display pairwise comparisons with flextable
# as.data.frame(summary(pairs)) %>%
#   flextable() %>%
#   set_header_labels(
#     contrast = "Contrast",
#     estimate = "Estimate",
#     SE = "Standard Error",
#     df = "df",
#     t.ratio = "t ratio",
#     p.value = "p-value"
#   ) %>%
#   colformat_double(j = c("estimate", "SE", "t.ratio", "p.value"), digits = 3) %>%
#   autofit() %>%
#   add_header_lines("Pairwise comparisons between treatments (Tukey-adjusted)") %>%
#   theme_box()
```
:::


# Lecture 13: Letter Display


::: {.cell}

```{.r .cell-code}
# Extract compact letter display for plotting
cld <- multcomp::cld(emm, alpha = 0.05, Letters = letters)

cld
```

::: {.cell-output .cell-output-stdout}

```
 treat   emmean   SE df lower.CL upper.CL .group
 control    1.3 9.41 12   -19.20     21.8  a    
 dens_66   19.0 9.41 12    -1.50     39.5  a    
 dens_33   21.6 9.41 12     1.05     42.0  a    
 removal   39.2 9.41 12    18.70     59.7  a    

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95 
P value adjustment: tukey method for comparing a family of 4 estimates 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::

```{.r .cell-code}
# # Display CLD with flextable
# as.data.frame(cld) %>%
#   flextable() %>%
#   set_header_labels(
#     treat = "treatment",
#     emmean = "Estimated Marginal Mean",
#     SE = "Standard Error",
#     df = "df",
#     lower.CL = "Lower CL",
#     upper.CL = "Upper CL",
#     .group = "Group"
#   ) %>%
#   colformat_double(j = c("emmean", "SE", "lower.CL", "upper.CL"), digits = 2) %>%
#   autofit() %>%
#   add_header_lines("Compact letter display of treatment means") %>%
#   theme_box()
```
:::


::: {.callout-important appearance="simple"}
Interpretation of treatment Comparisons The mean algae cover for the
Control treatment (1.30%) appears considerably lower than for the
reduced urchin density treatments (66% Density: 21.55%, 33% Density:
19.00%, Removed: 39.20%). While the visual pattern suggests an inverse
relationship between urchin density and algae cover, with complete
removal showing the highest algae cover, the nested ANOVA showed that
these differences were not statistically significant at the α = 0.05
level (p = r format(p_treat, digits=3)). The high variability among
patches within treatments likely contributed to the lack of statistical
significance for the treatment effect.
:::

# Assumption Testing

For valid inference from mixed models, several assumptions must be met.
We test these assumptions below.

## Normality of Residuals


::: {.cell}

```{.r .cell-code}
# QQ plot of residuals
qqnorm(resid(mixed_model))
qqline(resid(mixed_model))
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/normality-1.png){width=480}
:::

```{.r .cell-code}
# Histogram of residuals
hist(resid(mixed_model), main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 15)
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/normality-2.png){width=480}
:::

```{.r .cell-code}
# More advanced residual diagnostics using DHARMa
sim_residuals <- simulateResiduals(fittedModel = mixed_model)
plot(sim_residuals)
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/normality-3.png){width=480}
:::
:::


## Homogeneity of Variance


::: {.cell}

```{.r .cell-code}
# Residuals vs. fitted values plot
plot(fitted(mixed_model), resid(mixed_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 2, col = "red")
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/homogeneity-1.png){width=480}
:::
:::



::: {.cell}

```{.r .cell-code}
# Levene's test for homogeneity of variance
levene_test <- leveneTest(algae ~ treat, data = urchin_df)
levene_test
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value     Pr(>F)    
group  3  8.1694 0.00008785 ***
      76                       
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


::: {.callout-important appearance="simple"}
**Interpretation of Assumption Tests**

The Q-Q plot shows some deviation from normality, particularly in the
tails, and Levene's test indicates significant heterogeneity of
variances across treatments (F = 8.17,
p \< 0.001). As noted in the original analysis, there were "large
differences in within-cell variances" in this dataset, and
transformations did not improve variance homogeneity.

The DHARMa residual diagnostics also indicate potential issues with the
distribution of residuals and homogeneity of variance. The residuals vs.
fitted plot shows a pattern of increasing variance with increasing
fitted values, confirming the heteroscedasticity.

However, mixed models are generally robust to moderate violations of
assumptions, especially with balanced designs. Since transformations
were not effective in improving the data properties, analyzing the
untransformed data is a reasonable approach in this case.
:::

# Post-hoc Comparisons

Although the main effect of treatment was not significant in the nested
ANOVA (p = 0.0913), we can still examine the mean
differences between treatments to understand patterns in the data.


::: {.cell}

```{.r .cell-code}
# Calculate estimated marginal means
emm <- emmeans(mixed_model, ~ treat)
emm
```

::: {.cell-output .cell-output-stdout}

```
 treat   emmean   SE df lower.CL upper.CL
 control    1.3 9.41 12   -19.20     21.8
 dens_33   21.6 9.41 12     1.05     42.0
 dens_66   19.0 9.41 12    -1.50     39.5
 removal   39.2 9.41 12    18.70     59.7

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95 
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Pairwise comparisons with Tukey adjustment
pairs <- pairs(emm, adjust = "tukey")
pairs
```

::: {.cell-output .cell-output-stdout}

```
 contrast          estimate   SE df t.ratio p.value
 control - dens_33   -20.25 13.3 12  -1.522  0.4553
 control - dens_66   -17.70 13.3 12  -1.330  0.5625
 control - removal   -37.90 13.3 12  -2.849  0.0615
 dens_33 - dens_66     2.55 13.3 12   0.192  0.9974
 dens_33 - removal   -17.65 13.3 12  -1.327  0.5646
 dens_66 - removal   -20.20 13.3 12  -1.518  0.4573

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 4 estimates 
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Compact letter display
cld <- multcomp::cld(emm, alpha = 0.05, Letters = letters)
cld
```

::: {.cell-output .cell-output-stdout}

```
 treat   emmean   SE df lower.CL upper.CL .group
 control    1.3 9.41 12   -19.20     21.8  a    
 dens_66   19.0 9.41 12    -1.50     39.5  a    
 dens_33   21.6 9.41 12     1.05     42.0  a    
 removal   39.2 9.41 12    18.70     59.7  a    

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95 
P value adjustment: tukey method for comparing a family of 4 estimates 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::


# Visualization


::: {.cell}

```{.r .cell-code}
# Create boxplot with jittered points
ggplot_boxplot <- ggplot(urchin_df, aes(x = treat, y = algae, fill = treat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Urchin Density effect on algae Cover",
    x = "Urchin Density ",
    y = "algae Cover (%)",
    caption = "Figure 1: Boxplots showing the distribution of algal cover across urchin density.\nDespite visual differences, the treatment effect was not statistically significant (p = 0.091)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )

# Create means plot with error bars
means_plot <- ggplot(summary_stats, aes(x = treat, y = mean, group = 1)) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(
    title = "Mean algae Cover by Urchin Density",
    x = "Urchin Density",
    y = "algae Cover (%)",
    caption = "Figure 2: Mean (± SE) percentage cover of algae across urchin density treatments."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )
```
:::



::: {.cell}

```{.r .cell-code}
# Display plots
ggplot_boxplot
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/unnamed-chunk-6-1.png){width=480}
:::
:::



::: {.cell}

```{.r .cell-code}
means_plot
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/unnamed-chunk-7-1.png){width=480}
:::
:::


::: {.cell}

```{.r .cell-code}
# Combined plot using patchwork
ggplot_boxplot + means_plot + plot_layout(ncol = 1)
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-html/unnamed-chunk-8-1.png){width=576}
:::
:::

# Discussion

::: {.callout-important appearance="simple"}
**Scientific Interpretation**

Our mixed model analysis of the nested design revealed substantial
spatial heterogeneity in algae cover, with significant variation among
patches within each treatment (p \< 0.001). Surprisingly, the effect of
urchin density treatments on filamentous algae cover was not
statistically significant at the α = 0.05 level (p = 0.091), despite
apparent trends in the data.

The descriptive statistics show a pattern where algae cover appears to
increase as urchin density decreases, with the Control treatment (mean =
1.3%) showing minimal algae cover compared to reduced density treatments
(66% Density: 21.55%, 33% Density: 19.00%, and Removed: 39.20%). This
pattern suggests a potential density-dependent relationship between
urchin grazing and algal abundance, but the high variability among
patches masked the treatment effect.

The substantial variance component associated with patches nested within
treatments (294.31, approximately 39.5% of total variance) underscores
the importance of spatial heterogeneity in structuring algal
communities. This finding highlights the necessity of accounting for
spatial variability when designing and analyzing ecological field
experiments.

From an ecological perspective, these results suggest that while sea
urchins may influence algal communities through grazing, local
environmental factors and patch-specific conditions play a dominant role
in determining algae cover. This has important implications for
ecosystem management, as it indicates that the effects of urchin density
manipulations may be context-dependent and influenced by local
environmental conditions.
:::

# Comparison with Traditional Nested ANOVA

The linear mixed model approach provides similar results to the
traditional nested ANOVA approach. The main advantage of the mixed model
is the more elegant handling of random effects and the extensive
diagnostic tools available through packages like DHARMa.

The mixed model approach confirms that:

1.  treatment effects are not significant (p = 0.091)
2.  patches within treatments show significant variation (p \< 0.001)
3.  The variance components are similar to those from the traditional
    approach

In both methods, the key ecological finding is the strong spatial
heterogeneity in algal cover that overrides the grazing effect of
urchins at different densities.

# References

Andrew, N. L., & Underwood, A. J. (1993). Density-dependent foraging in
the sea urchin Centrostephanus rodgersii on shallow subtidal reefs in
New South Wales, Australia. Marine Ecology Progress Series, 99, 89-98.

Quinn, G. P., & Keough, M. J. (2002). Experimental design and data
analysis for biologists. Cambridge University Press.
