---
title: "How an ANOVA IS A REGRESSION WIHT DUMMY VARIABLES "
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "11_04_how_anova_is_regression_html.html"
  revealjs:
    output-file: "11_04_how_anova_is_regression_slides.html"
---





# Introduction

This document demonstrates how Analysis of Variance (ANOVA) is
mathematically equivalent to a regression model with dummy variables
using an example with R code and visualizations.

# Setup and Data Creation

Let's begin by loading necessary packages and creating a dataframe about
plant heights with three different fertilizer treatments.




::: {.cell}

```{.r .cell-code}
# install.packages("flextable")
library(tidyverse)
library(flextable)

# Create the dataset
fertilizer_data <- tibble(
  fertilizer = rep(c("A", "B", "C"), each = 3),
  height = c(10, 12, 8,   # Fertilizer A
             14, 16, 18,  # Fertilizer B
             20, 22, 24)  # Fertilizer C
)

# Display the dataset using flextable
flextable(fertilizer_data) %>%
  set_caption("Plant Heights by Fertilizer Type") %>%
  theme_vanilla() %>%
  autofit()
```

::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-e0af7342{}.cl-e0ac3c90{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e0ac3ca4{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e0ad8776{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e0ad8777{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e0ad9536{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9540{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9541{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9542{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9543{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9544{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad954a{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad954b{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad954c{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9554{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9555{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9556{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad9557{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0ad955e{width:0.743in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-e0af7342'><thead><tr style="overflow-wrap:break-word;"><th class="cl-e0ad9536"><p class="cl-e0ad8776"><span class="cl-e0ac3c90">fertilizer</span></p></th><th class="cl-e0ad9540"><p class="cl-e0ad8777"><span class="cl-e0ac3c90">height</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-e0ad9541"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">A</span></p></td><td class="cl-e0ad9542"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad9543"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">A</span></p></td><td class="cl-e0ad9544"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">12</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad954a"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">A</span></p></td><td class="cl-e0ad954b"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad9543"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">B</span></p></td><td class="cl-e0ad9544"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">14</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad954c"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">B</span></p></td><td class="cl-e0ad9554"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">16</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad954a"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">B</span></p></td><td class="cl-e0ad954b"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">18</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad9555"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">C</span></p></td><td class="cl-e0ad9556"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">20</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad9555"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">C</span></p></td><td class="cl-e0ad9556"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">22</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0ad9557"><p class="cl-e0ad8776"><span class="cl-e0ac3ca4">C</span></p></td><td class="cl-e0ad955e"><p class="cl-e0ad8777"><span class="cl-e0ac3ca4">24</span></p></td></tr></tbody></table></div>
```


:::
:::




# Calculating Group Means (ANOVA Approach)

In ANOVA, we calculate the mean of each group and compare variation
between groups to variation within groups.




::: {.cell}

```{.r .cell-code}
group_means <- fertilizer_data %>%
  group_by(fertilizer) %>%
  summarize(mean_height = mean(height))

flextable(group_means) %>%
  set_caption("Group Means (ANOVA Approach)") %>%
  theme_vanilla() %>%
  autofit()
```

::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-e0c3e700{}.cl-e0c0e2d0{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e0c0e2da{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e0c22726{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e0c22727{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e0c23324{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c2332e{width:1.228in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c2332f{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c23330{width:1.228in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c23338{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c23339{width:1.228in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c23342{width:0.88in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0c23343{width:1.228in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-e0c3e700'><thead><tr style="overflow-wrap:break-word;"><th class="cl-e0c23324"><p class="cl-e0c22726"><span class="cl-e0c0e2d0">fertilizer</span></p></th><th class="cl-e0c2332e"><p class="cl-e0c22727"><span class="cl-e0c0e2d0">mean_height</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-e0c2332f"><p class="cl-e0c22726"><span class="cl-e0c0e2da">A</span></p></td><td class="cl-e0c23330"><p class="cl-e0c22727"><span class="cl-e0c0e2da">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0c23338"><p class="cl-e0c22726"><span class="cl-e0c0e2da">B</span></p></td><td class="cl-e0c23339"><p class="cl-e0c22727"><span class="cl-e0c0e2da">16</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0c23342"><p class="cl-e0c22726"><span class="cl-e0c0e2da">C</span></p></td><td class="cl-e0c23343"><p class="cl-e0c22727"><span class="cl-e0c0e2da">22</span></p></td></tr></tbody></table></div>
```


:::
:::




Let's visualize the raw data and group means:




::: {.cell}

```{.r .cell-code}
ggplot(fertilizer_data, aes(x = fertilizer, y = height)) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  geom_point(data = group_means, aes(y = mean_height), 
             color = "red", size = 3) +
  labs(title = "Plant Height by Fertilizer Type",
       x = "Fertilizer Type",
       y = "Plant Height (cm)") +
  theme_minimal()
```

::: {.cell-output-display}
![](11_04_how_anova_is_dummy_var_regression_files/figure-html/plot-raw-data-1.png){width=672}
:::
:::




# Running the ANOVA




::: {.cell}

```{.r .cell-code}
# Run ANOVA
anova_model <- aov(height ~ fertilizer, data = fertilizer_data)
anova_summary <- summary(anova_model)
anova_summary
```

::: {.cell-output .cell-output-stdout}

```
            Df Sum Sq Mean Sq F value Pr(>F)   
fertilizer   2    216     108      27  0.001 **
Residuals    6     24       4                  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::




# Regression with Dummy Variables

For the regression approach, we'll create dummy variables for fertilizer
types, using fertilizer A as the reference level.




::: {.cell}

```{.r .cell-code}
# Set fertilizer A as the reference level
fertilizer_data$fertilizer <- factor(fertilizer_data$fertilizer, levels = c("A", "B", "C"))

# Run regression with dummy variables
reg_model <- lm(height ~ fertilizer, data = fertilizer_data)
reg_summary <- summary(reg_model)
reg_summary
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = height ~ fertilizer, data = fertilizer_data)

Residuals:
   Min     1Q Median     3Q    Max 
    -2     -2      0      2      2 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   10.000      1.155   8.660 0.000131 ***
fertilizerB    6.000      1.633   3.674 0.010402 *  
fertilizerC   12.000      1.633   7.348 0.000325 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2 on 6 degrees of freedom
Multiple R-squared:    0.9,	Adjusted R-squared:  0.8667 
F-statistic:    27 on 2 and 6 DF,  p-value: 0.001
```


:::
:::




# Understanding the Regression Coefficients

In our regression model:

-   The intercept (10) is equal to the mean of the reference group (A)
-   The coefficient for fertilizer B (6) is the difference between mean
    of group B and mean of group A
-   The coefficient for fertilizer C (12) is the difference between mean
    of group C and mean of group A




::: {.cell}

```{.r .cell-code}
# Create a table showing the relationship between coefficients and means
coefs <- coef(reg_model)

coefficients_explained <- tibble(
  Term = c("Intercept", "fertilizerB", "fertilizerC"),
  Coefficient = coefs,
  Meaning = c(
    "Mean of Group A (reference group)",
    "Difference between Group B and Group A means",
    "Difference between Group C and Group A means"
  ),
  Mathematical_Expression = c(
    "β₀ = μA",
    "β₁ = μB - μA",
    "β₂ = μC - μA"
  ),
  Numeric_Value = c(coefs[1],
    paste0(round(group_means$mean_height[2], 1), " - ", 
           round(group_means$mean_height[1], 1), " = ", 
           round(coefs[2], 1)),
    paste0(round(group_means$mean_height[3], 1), " - ", 
           round(group_means$mean_height[1], 1), " = ", 
           round(coefs[3], 1))))

# Use flextable to format the table
flextable(coefficients_explained) %>%
  set_caption("Regression Coefficients Explained") %>%
  theme_vanilla() %>%
  fit_to_width(max_width = 8, unit = "in") %>%
  bold(j = 1) %>%
  colformat_double(j = 2, digits = 2)
```

::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-e0f98234{}.cl-e0f6203a{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e0f62044{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e0f75374{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e0f75375{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e0f75ffe{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f75fff{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f76008{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f76009{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f7600a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f76012{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f76013{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e0f76014{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-e0f98234'><thead><tr style="overflow-wrap:break-word;"><th class="cl-e0f75ffe"><p class="cl-e0f75374"><span class="cl-e0f6203a">Term</span></p></th><th class="cl-e0f75fff"><p class="cl-e0f75375"><span class="cl-e0f6203a">Coefficient</span></p></th><th class="cl-e0f75ffe"><p class="cl-e0f75374"><span class="cl-e0f6203a">Meaning</span></p></th><th class="cl-e0f75ffe"><p class="cl-e0f75374"><span class="cl-e0f6203a">Mathematical_Expression</span></p></th><th class="cl-e0f75ffe"><p class="cl-e0f75374"><span class="cl-e0f6203a">Numeric_Value</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-e0f76008"><p class="cl-e0f75374"><span class="cl-e0f6203a">Intercept</span></p></td><td class="cl-e0f76009"><p class="cl-e0f75375"><span class="cl-e0f62044">10.00</span></p></td><td class="cl-e0f76008"><p class="cl-e0f75374"><span class="cl-e0f62044">Mean of Group A (reference group)</span></p></td><td class="cl-e0f76008"><p class="cl-e0f75374"><span class="cl-e0f62044">β₀ = μA</span></p></td><td class="cl-e0f76008"><p class="cl-e0f75374"><span class="cl-e0f62044">10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0f7600a"><p class="cl-e0f75374"><span class="cl-e0f6203a">fertilizerB</span></p></td><td class="cl-e0f76012"><p class="cl-e0f75375"><span class="cl-e0f62044">6.00</span></p></td><td class="cl-e0f7600a"><p class="cl-e0f75374"><span class="cl-e0f62044">Difference between Group B and Group A means</span></p></td><td class="cl-e0f7600a"><p class="cl-e0f75374"><span class="cl-e0f62044">β₁ = μB - μA</span></p></td><td class="cl-e0f7600a"><p class="cl-e0f75374"><span class="cl-e0f62044">16 - 10 = 6</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e0f76013"><p class="cl-e0f75374"><span class="cl-e0f6203a">fertilizerC</span></p></td><td class="cl-e0f76014"><p class="cl-e0f75375"><span class="cl-e0f62044">12.00</span></p></td><td class="cl-e0f76013"><p class="cl-e0f75374"><span class="cl-e0f62044">Difference between Group C and Group A means</span></p></td><td class="cl-e0f76013"><p class="cl-e0f75374"><span class="cl-e0f62044">β₂ = μC - μA</span></p></td><td class="cl-e0f76013"><p class="cl-e0f75374"><span class="cl-e0f62044">22 - 10 = 12</span></p></td></tr></tbody></table></div>
```


:::
:::




Let's visualize these coefficients:




::: {.cell}

```{.r .cell-code}
coef_data <- tibble(
  Term = factor(c("Intercept\n(Group A Mean)", "Group B - Group A", "Group C - Group A"),
                levels = c("Intercept\n(Group A Mean)", "Group B - Group A", "Group C - Group A")),
  Value = c(coefs[1], coefs[2], coefs[3])
)

ggplot(coef_data, aes(x = Term, y = Value)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5) +
  labs(title = "Regression Coefficients with Dummy Variables",
       subtitle = "Intercept represents Group A mean; other coefficients show differences from reference",
       x = "",
       y = "Coefficient Value (cm)") +
  theme_minimal()
```

::: {.cell-output-display}
![](11_04_how_anova_is_dummy_var_regression_files/figure-html/plot-coefficients-1.png){width=672}
:::
:::




# Demonstrating the Equivalence

Now, let's prove that the regression model predictions are identical to
the ANOVA group means:




::: {.cell}

```{.r .cell-code}
# Get predictions from regression model
predicted_values <- predict(reg_model, fertilizer_data)

# Create a dataframe for comparison
comparison_data <- fertilizer_data %>%
  mutate(predicted = predicted_values) %>%
  group_by(fertilizer) %>%
  mutate(group_mean = mean(height))

# Generate the predicted values for each group
predicted_values_by_group <- comparison_data %>%
  group_by(fertilizer) %>%
  reframe(
    anova_mean = mean(height),
    regression_prediction = mean(predicted),
    formula = case_when(
      fertilizer == "A" ~ paste0(round(coefs[1], 1), " + 0 + 0 = ", round(coefs[1], 1)),
      fertilizer == "B" ~ paste0(round(coefs[1], 1), " + ", round(coefs[2], 1), " + 0 = ", round(coefs[1] + coefs[2], 1)),
      fertilizer == "C" ~ paste0(round(coefs[1], 1), " + 0 + ", round(coefs[3], 1), " = ", round(coefs[1] + coefs[3], 1))
    )
  )
```
:::




Let's visualize this equivalence:




::: {.cell}

```{.r .cell-code}
# Create data for plotting the equivalence
plot_data <- predicted_values_by_group %>%
  pivot_longer(cols = c(anova_mean, regression_prediction),
               names_to = "method",
               values_to = "value") %>%
  mutate(method = ifelse(method == "anova_mean", "ANOVA Group Mean", "Regression Prediction"))

ggplot(plot_data, aes(x = fertilizer, y = value, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  geom_text(aes(label = round(value, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "ANOVA Mean vs. Regression Prediction by Fertilizer Type",
       subtitle = "Both methods produce identical values",
       x = "Fertilizer Type",
       y = "Plant Height (cm)",
       fill = "Method") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
```

::: {.cell-output-display}
![](11_04_how_anova_is_dummy_var_regression_files/figure-html/equivalence-plot-1.png){width=672}
:::
:::




# Comparing Statistical Tests

Both ANOVA and regression provide an F-test. Let's compare them:




::: {.cell}

```{.r .cell-code}
# ANOVA: Extract F-value and p-value
anova_f <- anova_summary[[1]]$`F value`[1]
anova_p <- anova_summary[[1]]$`Pr(>F)`[1]

# Regression: Extract F-value and p-value
reg_f <- reg_summary$fstatistic[1]
reg_p <- pf(reg_f, reg_summary$fstatistic[2], reg_summary$fstatistic[3], lower.tail = FALSE)

# Compare them
test_comparison <- tibble(
  Test = c("ANOVA F-test", "Regression F-test"),
  `F-value` = c(anova_f, reg_f),
  `p-value` = c(anova_p, reg_p)
)

# Format with flextable
flextable(test_comparison) %>%
  set_caption("Comparison of Statistical Tests") %>%
  theme_vanilla() %>%
  autofit() %>%
  colformat_double(j = 2:3, digits = 4)
```

::: {.cell-output-display}


```{=html}
<div class="tabwid"><style>.cl-e13ad540{}.cl-e137682e{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e1376838{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e138ab26{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e138ab30{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e138b756{width:1.491in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e138b757{width:0.82in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e138b760{width:1.491in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e138b761{width:0.82in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e138b762{width:1.491in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e138b76a{width:0.82in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-e13ad540'><thead><tr style="overflow-wrap:break-word;"><th class="cl-e138b756"><p class="cl-e138ab26"><span class="cl-e137682e">Test</span></p></th><th class="cl-e138b757"><p class="cl-e138ab30"><span class="cl-e137682e">F-value</span></p></th><th class="cl-e138b757"><p class="cl-e138ab30"><span class="cl-e137682e">p-value</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-e138b760"><p class="cl-e138ab26"><span class="cl-e1376838">ANOVA F-test</span></p></td><td class="cl-e138b761"><p class="cl-e138ab30"><span class="cl-e1376838">27.0000</span></p></td><td class="cl-e138b761"><p class="cl-e138ab30"><span class="cl-e1376838">0.0010</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-e138b762"><p class="cl-e138ab26"><span class="cl-e1376838">Regression F-test</span></p></td><td class="cl-e138b76a"><p class="cl-e138ab30"><span class="cl-e1376838">27.0000</span></p></td><td class="cl-e138b76a"><p class="cl-e138ab30"><span class="cl-e1376838">0.0010</span></p></td></tr></tbody></table></div>
```


:::
:::




# The Mathematical Relationship

For a one-way ANOVA with a categorical variable having `k` levels, we
can express the relationship with regression as:

$$Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + ... + \beta_{k-1}X_{k-1} + \epsilon$$

Where: - $\beta_0$ is the mean of the reference group -
$\beta_1, \beta_2, ..., \beta_{k-1}$ are the differences between each
group's mean and the reference group mean - $X_1, X_2, ..., X_{k-1}$ are
dummy variables (0 or 1)

In our example: - $\beta_0 = 10$ (mean of group A) - $\beta_1 = 6$
(difference between B and A) - $\beta_2 = 12$ (difference between C and
A)

# Conclusion

This demonstration shows that one-way ANOVA is mathematically equivalent
to regression with dummy variables. The key equivalences are:

1.  ANOVA group means = Regression predictions for each group
2.  F-statistic from ANOVA = F-statistic from regression
3.  p-values are identical in both approaches

This confirms that both techniques are special cases of the General
Linear Model, just expressed in different ways. For a categorical
predictor with `k` levels, we need `k-1` dummy variables in the
regression approach, with one level serving as the reference category.
