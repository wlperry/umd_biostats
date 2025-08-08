---
title: "Lecture 11 - Single factor analysis of variance - ANOVA"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "11_02_lecture_powerpoint_html.html"
  revealjs:
    output-file: "11_01_lecture_powerpoint_slides.html"
---









# Lecture 11: Review

::::: columns
::: {.column width="60%"}
## **Multiple Regression**

-   MLR model
-   Regression parameters
-   Analysis of variance
-   Null hypotheses
-   Explained variance
-   Assumptions and diagnostics
-   Collinearity
-   Interactions
-   Dummy variables
-   Model selection
-   Importance of predictors
:::

::: {.column width="40%"}
![](images/clipboard-2698541257.png){width="400"}
:::
:::::

# Lecture 12: Overview

### ANOVA

Analysis of variance: single and multi-factor designs

-   Examples: diatoms, circadian rhythms
-   Predictor variables: fixed vs. random
-   ANOVA model
-   Analysis and partitioning of variance
-   Null hypothesis
-   Assumptions and diagnostics
-   Post F Tests - Tukey and others
-   Reporting the results

# **Lecture 12:** ANOVA Introduction

What if response continuous and predictor(s) categorical?

|                        | Independent variable |                 |
|:-----------------------|:---------------------|:----------------|
| **Dependent variable** | **Continuous**       | **Categorical** |
| **Continuous**         | Regression           | ANOVA           |
| **Categorical**        | Logistic regression  | Tabular         |

# **Lecture 12:** ANOVA and Regression Connection

::: callout-note
# Remember

## Key Insight

Both regression and ANOVA:

-   Partition the total variation in Y
-   Use F-tests for significance
-   Are based on the General Linear Model
-   Test if explanatory variables predict Y ANOVA is fundamentally
    connected to regression analysis - both are special cases of the
    General Linear Model.






::: {.cell}
::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-eb867536{}.cl-eb823d22{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-eb823d2c{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-eb839e56{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-eb83aba8{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83aba9{width:1.275in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abaa{width:1.438in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abb2{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abb3{width:1.275in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abb4{width:1.438in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abbc{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abbd{width:1.275in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-eb83abc6{width:1.438in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-eb867536'><thead><tr style="overflow-wrap:break-word;"><th class="cl-eb83aba8"><p class="cl-eb839e56"><span class="cl-eb823d22">Model</span></p></th><th class="cl-eb83aba9"><p class="cl-eb839e56"><span class="cl-eb823d22">Form</span></p></th><th class="cl-eb83abaa"><p class="cl-eb839e56"><span class="cl-eb823d22">Tests</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-eb83abb2"><p class="cl-eb839e56"><span class="cl-eb823d2c">Regression</span></p></td><td class="cl-eb83abb3"><p class="cl-eb839e56"><span class="cl-eb823d2c">Y = β₀ + β₁X + ε</span></p></td><td class="cl-eb83abb4"><p class="cl-eb839e56"><span class="cl-eb823d2c">H₀: β₁ = 0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-eb83abbc"><p class="cl-eb839e56"><span class="cl-eb823d2c">ANOVA</span></p></td><td class="cl-eb83abbd"><p class="cl-eb839e56"><span class="cl-eb823d2c">Yᵢⱼ = μ + Aᵢ + εᵢⱼ</span></p></td><td class="cl-eb83abc6"><p class="cl-eb839e56"><span class="cl-eb823d2c">H₀: μ₁ = μ₂ = ... = μₖ</span></p></td></tr></tbody></table></div>
```


:::
:::





:::

# **Lecture 12:** ANOVA Partitioning

::::: columns
::: {.column width="60%"}
General method for partitioning variation in continuous dependent
variable

-   One or more continuous (and categorical) predictors:
    -   regression
-   One or more categorical predictors:
    -   ANOVA
-   Categorical predictor variables:
    -   groups or experimental treatments
:::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::
:::





:::
:::::

# **Lecture 12:** ANOVA as Regression

::: callout-tip
# ANOVA as Regression

With one categorical variable, ANOVA is equivalent to regression with
dummy variables.

In fact when we will run ANOVAs we will use he smae code as for
regression! See explanation on oher web page - Will link here






::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/regression-anova-1.png){width=384}
:::
:::





:::

# **Lecture 12:** ANOVA Goals

ANOVA aims to compare means of groups:

-   Contribution of predictors + "error" to variability
-   Test H₀ that population (random effects) or group (fixed effects)
    means are equal
-   Single factor (1-way) and multifactor (2-, 3-way designs)
    -   Single factor: one factor, more than two levels.
-   Multifactor:
    -   two or three factors, two or more levels.
    -   Examines variation due to factors **AND** their interaction

# The Analysis of Variance

:::::: columns
:::: {.column width="60%"}
Analysis of variance is the most powerful approach known for
simultaneously testing whether the means of k groups are equal. It works
by assessing whether individuals chosen from different groups are, on
average, more different than individuals chosen from the same group.

The null hypothesis of ANOVA is that the population means μᵢ are the
same for all treatments.

**H₀**: μ₁ = μ₂ = ... = μₖ

**H₁**: At least one μᵢ is different from the others.

::: callout-note
Rejecting H₀ in ANOVA is evidence that the mean of at least one group is
different from the others. It does not indicate *which* means differ.
:::
::::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-3-1.png){width=432}
:::
:::





:::
::::::

# **Lecture 12:** ANOVA Logic

::::: columns
::: {.column width="60%"}
Even if all groups had the same true mean, the data would likely show
different sample means for each group due to sampling error.

The key insight of ANOVA is that we can estimate how much variation
among group means ought to be present from sampling error alone if the
null hypothesis is true.

ANOVA lets us determine whether there is more variance among the sample
means than we would expect by chance alone. If so, then we can infer
that there are real differences among the population means.

Two key measures of variation are calculated and compared:

1.  **Group mean square (MSgroups)** - variation among subjects from
    different groups
2.  **Error mean square (MSerror)** - variation among subjects within
    the same group

The comparison is done with an F-ratio:

$$F = \frac{MS_{groups}}{MS_{error}}$$
:::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-4-1.png){width=576}
:::
:::





:::
:::::

# **Lecture 12:** Partitioning the Sum of Squares

The total variation in Y can be expressed as a sum of squares:

$SS_{total} = \sum_{i=1}^{a}\sum_{j=1}^{n}(Y_{ij} - \bar{Y})^2$

This can be partitioned into two components:

1.  **Among Groups (Treatment)**:
    $SS_{among} = \sum_{i=1}^{a}\sum_{j=1}^{n}(\bar{Y}_i - \bar{Y})^2 = n\sum_{i=1}^{a}(\bar{Y}_i - \bar{Y})^2$

2.  **Within Groups (Error)**:
    $SS_{within} = \sum_{i=1}^{a}\sum_{j=1}^{n}(Y_{ij} - \bar{Y}_i)^2$

These components are additive: $SS_{total} = SS_{among} + SS_{within}$

# **Lecture 12:** Sum of Squares Example






::: {.cell}
::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-ed1ac636{}.cl-ed17c4e0{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed17c4ea{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed1909ae{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed1909af{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed191642{width:0.821in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed19164c{width:0.358in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed19164d{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed19164e{width:0.945in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191656{width:1.022in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191657{width:0.821in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191658{width:0.358in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191659{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191660{width:0.945in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191661{width:1.022in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191662{width:0.821in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191663{width:0.358in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed19166a{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed19166b{width:0.945in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed191674{width:1.022in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ed1ac636'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ed191642"><p class="cl-ed1909ae"><span class="cl-ed17c4e0">Source</span></p></th><th class="cl-ed19164c"><p class="cl-ed1909af"><span class="cl-ed17c4e0">Df</span></p></th><th class="cl-ed19164d"><p class="cl-ed1909af"><span class="cl-ed17c4e0">Sum Sq</span></p></th><th class="cl-ed19164e"><p class="cl-ed1909af"><span class="cl-ed17c4e0">Mean Sq</span></p></th><th class="cl-ed19164d"><p class="cl-ed1909af"><span class="cl-ed17c4e0">F value</span></p></th><th class="cl-ed191656"><p class="cl-ed1909af"><span class="cl-ed17c4e0">Pr(&gt;F)</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ed191657"><p class="cl-ed1909ae"><span class="cl-ed17c4ea">treatment</span></p></td><td class="cl-ed191658"><p class="cl-ed1909af"><span class="cl-ed17c4ea">2</span></p></td><td class="cl-ed191659"><p class="cl-ed1909af"><span class="cl-ed17c4ea">2.236862</span></p></td><td class="cl-ed191660"><p class="cl-ed1909af"><span class="cl-ed17c4ea">1.11843079</span></p></td><td class="cl-ed191659"><p class="cl-ed1909af"><span class="cl-ed17c4ea">16.05032</span></p></td><td class="cl-ed191661"><p class="cl-ed1909af"><span class="cl-ed17c4ea">0.001075902</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed191662"><p class="cl-ed1909ae"><span class="cl-ed17c4ea">Residuals</span></p></td><td class="cl-ed191663"><p class="cl-ed1909af"><span class="cl-ed17c4ea">9</span></p></td><td class="cl-ed19166a"><p class="cl-ed1909af"><span class="cl-ed17c4ea">0.627145</span></p></td><td class="cl-ed19166b"><p class="cl-ed1909af"><span class="cl-ed17c4ea">0.06968277</span></p></td><td class="cl-ed19166a"><p class="cl-ed1909af"><span class="cl-ed17c4ea"></span></p></td><td class="cl-ed191674"><p class="cl-ed1909af"><span class="cl-ed17c4ea"></span></p></td></tr></tbody></table></div>
```


:::

::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-ed2542d2{}.cl-ed21ac80{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed21ac8a{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed22d696{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed22d697{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed22e85c{width:1.145in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e866{width:1.268in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e870{width:1.554in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e871{width:1.076in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e872{width:1.145in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e87a{width:1.268in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e87b{width:1.554in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e884{width:1.076in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e885{width:1.145in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e886{width:1.268in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e88e{width:1.554in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e88f{width:1.076in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e890{width:1.145in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e898{width:1.268in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e899{width:1.554in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed22e89a{width:1.076in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ed2542d2'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ed22e85c"><p class="cl-ed22d696"><span class="cl-ed21ac80">Component</span></p></th><th class="cl-ed22e866"><p class="cl-ed22d697"><span class="cl-ed21ac80">Sum of Squares</span></p></th><th class="cl-ed22e870"><p class="cl-ed22d697"><span class="cl-ed21ac80">Degrees of Freedom</span></p></th><th class="cl-ed22e871"><p class="cl-ed22d697"><span class="cl-ed21ac80">Mean Square</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ed22e872"><p class="cl-ed22d696"><span class="cl-ed21ac8a">Total</span></p></td><td class="cl-ed22e87a"><p class="cl-ed22d697"><span class="cl-ed21ac8a">2.864007</span></p></td><td class="cl-ed22e87b"><p class="cl-ed22d697"><span class="cl-ed21ac8a">11</span></p></td><td class="cl-ed22e884"><p class="cl-ed22d697"><span class="cl-ed21ac8a"></span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed22e885"><p class="cl-ed22d696"><span class="cl-ed21ac8a">Among Groups</span></p></td><td class="cl-ed22e886"><p class="cl-ed22d697"><span class="cl-ed21ac8a">2.236862</span></p></td><td class="cl-ed22e88e"><p class="cl-ed22d697"><span class="cl-ed21ac8a">2</span></p></td><td class="cl-ed22e88f"><p class="cl-ed22d697"><span class="cl-ed21ac8a">1.11843079</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed22e890"><p class="cl-ed22d696"><span class="cl-ed21ac8a">Within Groups</span></p></td><td class="cl-ed22e898"><p class="cl-ed22d697"><span class="cl-ed21ac8a">0.627145</span></p></td><td class="cl-ed22e899"><p class="cl-ed22d697"><span class="cl-ed21ac8a">9</span></p></td><td class="cl-ed22e89a"><p class="cl-ed22d697"><span class="cl-ed21ac8a">0.06968277</span></p></td></tr></tbody></table></div>
```


:::
:::






::: callout-important
# Key Connection to Regression

This is the same partitioning we saw in regression analysis:
$SS_{total} = SS_{regression} + SS_{residual}$

Where:

-   $SS_{among}$ in ANOVA = $SS_{regression}$ in regression
-   $SS_{within}$ in ANOVA = $SS_{residual}$ in regression

Both measure how much variation is explained by our model vs.
unexplained (error).
:::

# **Lecture 12:** ANOVA Tables

The ANOVA table organizes all computations leading to a test of the null
hypothesis of no differences among population means.

-   **Source of variation**: What is being tested
-   **Sum of squares**: Measure of total variation for each source
-   **df**: Degrees of freedom for each source
-   **Mean squares**: Sum of squares divided by df
-   **F-ratio**: Ratio of mean squares, used to test significance
-   **P-value**: Probability of observing our results if H₀ is true

**Example**: For a one-way ANOVA with 3 groups and 4 replicates per
group:

-   df for treatments = (a - 1) = 2
-   df for error = a(n - 1) = 3(4 - 1) = 9
-   df total = an - 1 = 11

# **Lecture 12:** Circadian Rhythm Data Example






::: {.cell}
::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-ed368c18{}.cl-ed336a24{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed336a2e{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed34bd48{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed34bd52{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed34ca22{width:0.821in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca23{width:0.366in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca24{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca2c{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca2d{width:1.022in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca2e{width:0.821in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca36{width:0.366in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca37{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca38{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca40{width:1.022in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca41{width:0.821in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca42{width:0.366in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca4a{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca4b{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed34ca4c{width:1.022in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ed368c18'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ed34ca22"><p class="cl-ed34bd48"><span class="cl-ed336a24">Source</span></p></th><th class="cl-ed34ca23"><p class="cl-ed34bd52"><span class="cl-ed336a24">Df</span></p></th><th class="cl-ed34ca24"><p class="cl-ed34bd52"><span class="cl-ed336a24">Sum Sq</span></p></th><th class="cl-ed34ca2c"><p class="cl-ed34bd52"><span class="cl-ed336a24">Mean Sq</span></p></th><th class="cl-ed34ca24"><p class="cl-ed34bd52"><span class="cl-ed336a24">F value</span></p></th><th class="cl-ed34ca2d"><p class="cl-ed34bd52"><span class="cl-ed336a24">Pr(&gt;F)</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ed34ca2e"><p class="cl-ed34bd48"><span class="cl-ed336a2e">treatment</span></p></td><td class="cl-ed34ca36"><p class="cl-ed34bd52"><span class="cl-ed336a2e">2</span></p></td><td class="cl-ed34ca37"><p class="cl-ed34bd52"><span class="cl-ed336a2e">7.224492</span></p></td><td class="cl-ed34ca38"><p class="cl-ed34bd52"><span class="cl-ed336a2e">3.6122459</span></p></td><td class="cl-ed34ca37"><p class="cl-ed34bd52"><span class="cl-ed336a2e">7.289449</span></p></td><td class="cl-ed34ca40"><p class="cl-ed34bd52"><span class="cl-ed336a2e">0.004472271</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed34ca41"><p class="cl-ed34bd48"><span class="cl-ed336a2e">Residuals</span></p></td><td class="cl-ed34ca42"><p class="cl-ed34bd52"><span class="cl-ed336a2e">19</span></p></td><td class="cl-ed34ca4a"><p class="cl-ed34bd52"><span class="cl-ed336a2e">9.415345</span></p></td><td class="cl-ed34ca4b"><p class="cl-ed34bd52"><span class="cl-ed336a2e">0.4955445</span></p></td><td class="cl-ed34ca4a"><p class="cl-ed34bd52"><span class="cl-ed336a2e"></span></p></td><td class="cl-ed34ca4c"><p class="cl-ed34bd52"><span class="cl-ed336a2e"></span></p></td></tr></tbody></table></div>
```


:::

::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-ed429e36{}.cl-ed3fabe0{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed3fabea{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed40e30c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed40e30d{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed40eeec{width:0.844in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40eef6{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40eef7{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40eef8{width:0.311in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef00{width:0.844in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef01{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef02{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef0a{width:0.311in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef0b{width:0.844in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef0c{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef0d{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef14{width:0.311in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef15{width:0.844in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef16{width:0.914in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef17{width:0.868in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed40ef1e{width:0.311in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ed429e36'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ed40eeec"><p class="cl-ed40e30c"><span class="cl-ed3fabe0">treatment</span></p></th><th class="cl-ed40eef6"><p class="cl-ed40e30d"><span class="cl-ed3fabe0">Mean</span></p></th><th class="cl-ed40eef7"><p class="cl-ed40e30d"><span class="cl-ed3fabe0">SD</span></p></th><th class="cl-ed40eef8"><p class="cl-ed40e30d"><span class="cl-ed3fabe0">N</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ed40ef00"><p class="cl-ed40e30c"><span class="cl-ed3fabea">Control</span></p></td><td class="cl-ed40ef01"><p class="cl-ed40e30d"><span class="cl-ed3fabea">-0.3087500</span></p></td><td class="cl-ed40ef02"><p class="cl-ed40e30d"><span class="cl-ed3fabea">0.6175629</span></p></td><td class="cl-ed40ef0a"><p class="cl-ed40e30d"><span class="cl-ed3fabea">8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed40ef0b"><p class="cl-ed40e30c"><span class="cl-ed3fabea">Eyes</span></p></td><td class="cl-ed40ef0c"><p class="cl-ed40e30d"><span class="cl-ed3fabea">-1.5514286</span></p></td><td class="cl-ed40ef0d"><p class="cl-ed40e30d"><span class="cl-ed3fabea">0.7063151</span></p></td><td class="cl-ed40ef14"><p class="cl-ed40e30d"><span class="cl-ed3fabea">7</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed40ef15"><p class="cl-ed40e30c"><span class="cl-ed3fabea">Knees</span></p></td><td class="cl-ed40ef16"><p class="cl-ed40e30d"><span class="cl-ed3fabea">-0.3357143</span></p></td><td class="cl-ed40ef17"><p class="cl-ed40e30d"><span class="cl-ed3fabea">0.7908193</span></p></td><td class="cl-ed40ef1e"><p class="cl-ed40e30d"><span class="cl-ed3fabea">7</span></p></td></tr></tbody></table></div>
```


:::
:::






# **Lecture 12:** ANOVA vs Regression Tables

::: callout-important
# Comparing ANOVA and Regression Tables

An ANOVA table from an ANOVA model:

| Source    | df     | SS           | MS           | F   | p   |
|-----------|--------|--------------|--------------|-----|-----|
| Treatment | a-1    | SS_treatment | MS_treatment | F   | p   |
| Error     | a(n-1) | SS_error     | MS_error     |     |     |
| Total     | an-1   | SS_total     |              |     |     |

Is equivalent to an ANOVA table from a regression model:

| Source     | df    | SS            | MS            | F   | p   |
|------------|-------|---------------|---------------|-----|-----|
| Regression | k     | SS_regression | MS_regression | F   | p   |
| Error      | n-k-1 | SS_residual   | MS_residual   |     |     |
| Total      | n-1   | SS_total      |               |     |     |

where k = number of dummy variables = a-1
:::

# **Lecture 12:** F ratio

::::: columns
::: {.column width="60%"}
The F-ratio is calculated as:

$$F = \frac{MS_{among}}{MS_{error}}$$

Under the null hypothesis (all means equal): - The F-ratio should be
approximately 1 - Larger F-ratios suggest the among-group variance
exceeds what would be expected by chance

With the circadian rhythm data: - F = 7.29 - p = 0.004 - We reject the
null hypothesis

The F-ratio follows an F-distribution with (a - 1) and (a(n - 1))
degrees of freedom.






::: {.cell}
::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-ed53bff4{}.cl-ed50d14a{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed50d154{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed521136{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed521140{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed521d8e{width:1.38in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed521d98{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed521d99{width:1.38in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed521d9a{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed521da2{width:1.38in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed521da3{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ed53bff4'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ed521d8e"><p class="cl-ed521136"><span class="cl-ed50d14a">Metric</span></p></th><th class="cl-ed521d98"><p class="cl-ed521140"><span class="cl-ed50d14a">Value</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ed521d99"><p class="cl-ed521136"><span class="cl-ed50d154">F-observed</span></p></td><td class="cl-ed521d9a"><p class="cl-ed521140"><span class="cl-ed50d154">7.289449</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed521da2"><p class="cl-ed521136"><span class="cl-ed50d154">F-critical (α = 0.05)</span></p></td><td class="cl-ed521da3"><p class="cl-ed521140"><span class="cl-ed50d154">3.521893</span></p></td></tr></tbody></table></div>
```


:::
:::





:::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-8-1.png){width=384}
:::
:::





:::
:::::

::: callout-note
# Connection to t-test

An ANOVA with two groups (a = 2) is equivalent to a t-test: $$F = t^2$$
:::

# **Lecture 12:** F ratio Visualization

::::: columns
::: {.column width="60%"}
The F-ratio is calculated as:

$$F = \frac{MS_{among}}{MS_{error}}$$

Under the null hypothesis (all means equal): - The F-ratio should be
approximately 1 - Larger F-ratios suggest the among-group variance
exceeds what would be expected by chance

With the circadian rhythm data: - F = 7.29 - p = 0.004 - We reject the
null hypothesis

The F-ratio follows an F-distribution with (a - 1) and (a(n - 1))
degrees of freedom.






::: {.cell}
::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-ed7ab06e{}.cl-ed77bf8a{font-family:'Helvetica';font-size:10pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed77bf94{font-family:'Helvetica';font-size:10pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ed78fae4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed78fae5{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:3pt;padding-top:3pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-ed79064c{width:1.38in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed79064d{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed79064e{width:1.38in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed790656{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed790657{width:1.38in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ed790658{width:0.79in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-ed7ab06e'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ed79064c"><p class="cl-ed78fae4"><span class="cl-ed77bf8a">Metric</span></p></th><th class="cl-ed79064d"><p class="cl-ed78fae5"><span class="cl-ed77bf8a">Value</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ed79064e"><p class="cl-ed78fae4"><span class="cl-ed77bf94">F-observed</span></p></td><td class="cl-ed790656"><p class="cl-ed78fae5"><span class="cl-ed77bf94">7.289449</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ed790657"><p class="cl-ed78fae4"><span class="cl-ed77bf94">F-critical (α = 0.05)</span></p></td><td class="cl-ed790658"><p class="cl-ed78fae5"><span class="cl-ed77bf94">3.521893</span></p></td></tr></tbody></table></div>
```


:::
:::





:::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-10-1.png){width=672}
:::
:::





:::
:::::

# **Lecture 12:** Variation Explained: R²

::::: columns
::: {.column width="60%"}
R² summarizes the contribution of group differences to total variation:

$$R^2 = \frac{SS_{among}}{SS_{total}}$$

This is interpreted as the "fraction of the variation in Y that is
explained by groups."

For the circadian rhythm data: $$R^2 = \frac{7.224}{16.639} = 0.43$$

43% of the total variation in phase shift is explained by differences in
light treatment, with the remaining 57% being unexplained variation.

## Connection to Regression

This is exactly the same calculation as R² in regression:
$$R^2 = \frac{SS_{regression}}{SS_{total}}$$
:::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-11-1.png){width=384}
:::
:::





:::
:::::

# **Lecture 12:** ANOVA Assumptions

ANOVA has the same assumptions as the two-sample t-test, but applied to
all k groups:

1.  **Random samples** from corresponding populations
2.  **Normality**: Y values are normally distributed in each population
3.  **Homogeneity of variance**: variance is the same in all populations
4.  **Independence**: observations are independent

**Checking assumptions**:

-   Normality: Q-Q plots, histogram of residuals, Shapiro-Wilk test
-   Homogeneity: plot residuals vs. predicted values or x-values
-   Independence: examine experimental design

**If assumptions are violated**:

-   Transform Y (e.g., log, square root)
-   Use robust or non-parametric alternatives
-   Use generalized linear models (GLMs)

# **Lecture 12:** ANOVA diagnostics

This is the default output of base R






::: {.cell}

```{.r .cell-code}
# Model diagnostics
par(mfrow = c(2, 2))
plot(circ_model)
dev.off() # This forces the plot to be written
```

::: {.cell-output .cell-output-stdout}

```
null device 
          1 
```


:::

::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::






# A newer way to check with the performance library






::: {.cell}

```{.r .cell-code}
# install.packages("performance")
library(performance)
check_model(circ_model)
```

::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-13-1.png){width=672}
:::
:::






# **Lecture 12:** Levene's Test

Levene's test of homogeneity of variance Null Hypothesis is that they
are homogeneous So you want a non significant result here






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






# **Lecture 12:** Shapiro-Wilk Test

Shapiro-Wilk Normality Test Null Hypothesis is that they are normally
distributed So you want a non significant result here






::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(circ_model)
W = 0.95893, p-value = 0.468
```


:::
:::






::: callout-note
# Shared Assumptions with Regression

ANOVA and regression share virtually identical assumptions because they
are both linear models:

| Assumption | ANOVA | Regression |
|----|----|----|
| Linearity | Relationship between group membership and Y is additive | Relationship between X and Y is linear |
| Normality | Residuals within each group are normal | Residuals are normal |
| Equal variance | Variance is the same across all groups | Variance is the same across all X values |
| Independence | Observations are independent | Observations are independent |
:::

# **Lecture 12:** ANOVA Post-Hoc Testing Overview

::::: columns
::: {.column width="50%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="50%"}





::: {.cell}
::: {.cell-output .cell-output-stdout}

```
 contrast        estimate    SE df t.ratio p.value
 Control - Eyes     1.243 0.364 19   3.411  0.0079
 Control - Knees    0.027 0.364 19   0.074  0.9970
 Eyes - Knees      -1.216 0.376 19  -3.231  0.0117

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::
:::





:::
:::::

# **Lecture 12:** Post-Hoc Testing Results

::::: columns
::: {.column width="50%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="50%"}





::: {.cell}
::: {.cell-output .cell-output-stdout}

```
 treatment emmean    SE df lower.CL upper.CL .group
 Eyes      -1.551 0.266 19   -2.108   -0.995  a    
 Knees     -0.336 0.266 19   -0.893    0.221   b   
 Control   -0.309 0.249 19   -0.830    0.212   b   

Confidence level used: 0.95 
P value adjustment: tukey method for comparing a family of 3 estimates 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::





:::
:::::

# **Lecture 12:** Post-Hoc Visualization

::::: columns
::: {.column width="50%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="50%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-18-1.png){width=672}
:::
:::





:::
:::::

# **Lecture 12:** Significance Groups Plot

::::: columns
::: {.column width="60%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="40%"}





::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-html/unnamed-chunk-19-1.png){width=672}
:::
:::





:::
:::::

# **Lecture 12:** Reporting ANOVA Results

**Formal scientific writing example:**

"The effect of light treatment on circadian rhythm phase shift was
analyzed using a one-way ANOVA. There was a significant effect of
treatment on phase shift (F(2, 19) = 7.29, p = 0.004, η² = 0.43).
Post-hoc comparisons using Tukey's HSD test indicated that the mean
phase shift for the Eyes treatment (M = -1.55 h, SD = 0.71) was
significantly different from both the Control treatment (M = -0.31 h, SD
= 0.62) and the Knees treatment (M = -0.34 h, SD = 0.79). However, the
Control and Knees treatments did not significantly differ from each
other. These results suggest that light exposure to the eyes, but not to
the knees, impacts circadian rhythm phase shifts."

# **Lecture 12:** ANOVA Summary

### Key ANOVA Principles

1.  **Purpose**: ANOVA (Analysis of Variance) compares means across
    multiple groups simultaneously

2.  **Connection to Regression**:

    -   Both are special cases of the General Linear Model
    -   ANOVA with categorical predictors = Regression with dummy
        variables
    -   Both partition variance into explained and unexplained
        components

3.  **The Analysis of Variance**:

    -   Partitions total variation into components
    -   Tests whether differences among groups exceed what would be
        expected by chance
    -   Uses F-tests to compare variance between groups to variance
        within groups

4.  **Sum of Squares Partitioning**:

    -   SS(Total) = SS(Between Groups) + SS(Within Groups)
    -   Same as SS(Total) = SS(Regression) + SS(Error) in regression

5.  **Fixed vs. Random Effects**:

    -   Fixed effects: specific groups of interest (most common)
    -   Random effects: sampling from a larger population

# ANOVA Assumptions

1.  Independence of observations
2.  Normal distribution of residuals
3.  Homogeneity of variances
