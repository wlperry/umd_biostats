---
title: "Lecture 11 - ANOVA in class "
author: "Bill Perry"
metadata-files:
  - ../../_templates/activities.yml
format:
  html:
      output-file: "11_02_class_activity.html"
  typst:
    output-file: "11_02_class_activity.pdf" 
---




::: {.cell}
::: {.cell-output .cell-output-stdout}

```
# A tibble: 22 × 2
   treatment phase_shift
   <chr>           <dbl>
 1 Control          0.53
 2 Control          0.36
 3 Control          0.2 
 4 Control         -0.37
 5 Control         -0.6 
 6 Control         -0.64
 7 Control         -0.68
 8 Control         -1.27
 9 Knees            0.73
10 Knees            0.31
# ℹ 12 more rows
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Plot the data
c_plot <- ggplot(c_df, aes(x = treatment, y = phase_shift, color = treatment)) +
  geom_point(position = position_jitter(width=0.1)) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 18) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) 
c_plot
```

::: {.cell-output-display}
![](11_02_class_activity_files/figure-html/plot-1.png){width=672}
:::
:::

::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Anova Table (Type III tests)

Response: phase_shift
            Sum Sq Df F value   Pr(>F)   
(Intercept) 0.7626  1  1.5389 0.229877   
treatment   7.2245  2  7.2894 0.004472 **
Residuals   9.4153 19                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::

::: {.cell}
::: {.cell-output-display}
![](11_02_class_activity_files/figure-html/unnamed-chunk-3-1.png){width=384}
:::
:::






# ANOVA-Assumptions and Diagnostics

ANOVA has the same assumptions as the two-sample t-test, but applied to all k groups:

1.  **Random samples** from corresponding populations\
2.  **Normality**: Y values are normally distributed in each population\
3.  **Homogeneity of variance**: variance is the same in all populations\
4.  **Independence**: observations are independent

**Checking assumptions**:

-   Normality: Q-Q plots, histogram of residuals, Shapiro-Wilk test\
-   Homogeneity: plot residuals vs. predicted values or x-values\
-   Independence: examine experimental design

# **Lecture 12:** ANOVA diagnostics

This is the default output of base R






::: {.cell}
::: {.cell-output-display}
![](11_02_class_activity_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::






# ANOVA Diagnostics

Levene's test of homogeneity of variance

-   Null Hypothesis is that they are homogeneous
-   So you want a non significant result here






::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  2  0.1586 0.8545
      19               
```


:::
:::






# **Lecture 12:** ANOVA Diagnostics

Shapiro-Wilk Normality Test

-   Null Hypothesis is that they are normally distributed\
-   So you want a **non significant** result here






::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(model_aov)
W = 0.95893, p-value = 0.468
```


:::
:::






# **Lecture 12:** ANOVA Post-Hoc Testing

When ANOVA rejects H₀, we need to determine which groups differ.

**Example**: Using Tukey's HSD to compare all pairs of treatments in the circadian rhythm data.






::: {.cell}
::: {.cell-output .cell-output-stdout}

```
 contrast        estimate    SE df t.ratio p.value
 Control - Eyes     1.243 0.364 19   3.411  0.0088
 Control - Knees    0.027 0.364 19   0.074  0.9998
 Eyes - Knees      -1.216 0.376 19  -3.231  0.0131

P value adjustment: sidak method for 3 tests 
```


:::
:::

::: {.cell}

```{.r .cell-code}
emmeans_df <- emmeans(model_aov, "treatment")
emmeans_df
```

::: {.cell-output .cell-output-stdout}

```
 treatment emmean    SE df lower.CL upper.CL
 Control   -0.309 0.249 19   -0.830    0.212
 Eyes      -1.551 0.266 19   -2.108   -0.995
 Knees     -0.336 0.266 19   -0.893    0.221

Confidence level used: 0.95 
```


:::
:::

::: {.cell}

```{.r .cell-code}
pairwise_comparisons <- pairs(emmeans_df, adjust = "sidak")
pairwise_comparisons
```

::: {.cell-output .cell-output-stdout}

```
 contrast        estimate    SE df t.ratio p.value
 Control - Eyes     1.243 0.364 19   3.411  0.0088
 Control - Knees    0.027 0.364 19   0.074  0.9998
 Eyes - Knees      -1.216 0.376 19  -3.231  0.0131

P value adjustment: sidak method for 3 tests 
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Letters to indicate groups with similar means
letter_groups <- multcomp::cld(emmeans_df, Letters = letters, adjust = "sidak")
letter_groups
```

::: {.cell-output .cell-output-stdout}

```
 treatment emmean    SE df lower.CL upper.CL .group
 Eyes      -1.551 0.266 19    -2.25   -0.855  a    
 Knees     -0.336 0.266 19    -1.03    0.361   b   
 Control   -0.309 0.249 19    -0.96    0.343   b   

Confidence level used: 0.95 
Conf-level adjustment: sidak method for 3 estimates 
P value adjustment: sidak method for 3 tests 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::






## Can do in one go as well






::: {.cell}
::: {.cell-output .cell-output-stdout}

```
 treatment emmean    SE df lower.CL upper.CL .group
 Eyes      -1.551 0.266 19    -2.25   -0.855  a    
 Knees     -0.336 0.266 19    -1.03    0.361   b   
 Control   -0.309 0.249 19    -0.96    0.343   b   

Confidence level used: 0.95 
Conf-level adjustment: sidak method for 3 estimates 
P value adjustment: sidak method for 3 tests 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::






# **Lecture 12:** ANOVA Post-Hoc Testing






::: {.cell}
::: {.cell-output-display}
![](11_02_class_activity_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::






# **Lecture 12:** ANOVA Post-Hoc Testing






::: {.cell}
::: {.cell-output-display}
![](11_02_class_activity_files/figure-html/unnamed-chunk-13-1.png){width=672}
:::
:::






# **Lecture 12:** ANOVA Reporting results

**Formal scientific writing example:**

"The effect of light treatment on circadian rhythm phase shift was analyzed using a one-way ANOVA. There was a significant effect of treatment on phase shift (F(2, 19) = 7.29, p = 0.004, η² = 0.43). Post-hoc comparisons using Tukey's HSD test indicated that the mean phase shift for the Eyes treatment (M = -1.55 h, SD = 0.71) was significantly different from both the Control treatment (M = -0.31 h, SD = 0.62) and the Knees treatment (M = -0.34 h, SD = 0.79). However, the Control and Knees treatments did not significantly differ from each other. These results suggest that light exposure to the eyes, but not to the knees, impacts circadian rhythm phase shifts."