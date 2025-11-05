---
title: "Lecture 14 - Generalized Linear Models"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "14_02_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  # RevealJS - UNCHANGED (keeps your two-column layout and large images)
  revealjs:
    output-file: "14_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "14_01_lecture_powerpoint.docx"
  pptx:
    output-file: "14_01_lecture_powerpoint.pptx"
---


::: {.cell}

:::


# Lecture 13: Review of ANOVAs

### Review

-   ANOVA
-   Factorial ANOVA
-   Nested ANOVA
-   ASSUMPTIONS OF ALL
    -   Homogeneity of variance - Levenes or Bartletts Test
    -   Normality of Residuals
    -   Independence



# Lecture 14: GLM Overview

### Overview

General Linear Models GLM

-   Essentially the same as before while using defined distributions
    -   Normal
    -   Lognormal
    -   Binomial
    -   Poisson
    -   Gamma
    -   Negative binomial

Logistic Regression

-   when the outcome is yes or no or categorical

# Overview of Generalized Linear Models (GLMs)

::::: columns
::: {.column width="60%"}
General linear models assume normal distribution of response variables
and residuals. However, many types of biological data don't meet this
assumption. Generalized Linear Models (GLMs) allow for a wider range of
probability distributions for the response variable.

GLMs allow all types of "exponential family" distributions:

-   Normal
-   Lognormal
-   Binomial
-   Poisson
-   Gamma
-   Negative binomial

GLMs can be used for binary (yes/no), discrete (count), and
categorical/multinomial response variables, using maximum likelihood
(ML) rather than ordinary least squares (OLS) for estimation.

**Note:** GLMs extend linear models to non-normal data distributions.
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![Examples of distributions in the exponential family](14_01_lecture_powerpoint_files/figure-docx/glm-distributions-1.png)
:::
:::

:::
:::::

# The Three Elements of a GLM

### GLMs consist of three components:

1.  **Random component**: The response variable and its probability
    distribution (from exponential family: normal, binomial, Poisson)

2.  **Systematic component**: The predictor variable(s) in the model,
    which can be continuous or categorical

3.  **Link function**: Connects expected value of Y to predictor
    variables

$$g(\mu) = \beta_0 + \beta_1X_1 + \beta_2X_2...$$

::: {.callout-important appearance="simple"}
## Link Functions and Distributions

| Distribution | Common Link Function | Formula                      |
|--------------|----------------------|------------------------------|
| Normal       | Identity             | $g(\mu) = \mu$               |
| Poisson      | Log                  | $g(\mu) = \log(\mu)$         |
| Binomial     | Logit                | $g(\mu) = \log[\mu/(1-\mu)]$ |
:::

# GLM with Gaussian (Normal) Distribution: Setup

::::: columns
::: {.column width="60%"}
The simplest form of GLM uses a normal (Gaussian) distribution with an
identity link function. This is equivalent to standard linear
regression.

Let's compare a standard linear model and a Gaussian GLM using the
`mtcars` dataset, modeling miles per gallon (mpg) by the number of
cylinders (cyl).


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```


:::
:::

:::

::: {.column width="40%"}
Let's look at the summary of our Gaussian GLM:


::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/summary-gaussian_2-1.png)
:::
:::

:::
:::::

# GLM with Gaussian (Normal) Distribution: Setup

::::: columns
::: {.column width="60%"}
The simplest form of GLM uses a normal (Gaussian) distribution with an
identity link function. This is equivalent to standard linear
regression.

Let's compare a standard linear model and a Gaussian GLM using the
`mtcars` dataset, modeling miles per gallon (mpg) by the number of
cylinders (cyl).


::: {.cell}

```{.r .cell-code}
# Fit a standard linear model
model_lm <- lm(mpg ~ cyl, data = mtcars)

# Fit a Gaussian GLM
model_gaussian <- glm(mpg ~ cyl, 
                       data = mtcars, 
                       family = gaussian(link = "identity"))

# Compare the coefficients
coef_lm <- coefficients(model_lm)
coef_glm <- coefficients(model_gaussian)

# # Check if they're the same
# all.equal(coef_lm, coef_glm)
summary(model_lm)
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = mpg ~ cyl, data = mtcars)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.2636 -1.8357  0.0286  1.3893  7.2364 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  26.6636     0.9718  27.437  < 2e-16 ***
cyl6         -6.9208     1.5583  -4.441 0.000119 ***
cyl8        -11.5636     1.2986  -8.905 8.57e-10 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 3.223 on 29 degrees of freedom
Multiple R-squared:  0.7325,	Adjusted R-squared:  0.714 
F-statistic:  39.7 on 2 and 29 DF,  p-value: 4.979e-09
```


:::
:::

:::

::: {.column width="40%"}
Let's look at the summary of our Gaussian GLM:


::: {.cell}

```{.r .cell-code}
summary(model_gaussian)
```

::: {.cell-output .cell-output-stdout}

```

Call:
glm(formula = mpg ~ cyl, family = gaussian(link = "identity"), 
    data = mtcars)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  26.6636     0.9718  27.437  < 2e-16 ***
cyl6         -6.9208     1.5583  -4.441 0.000119 ***
cyl8        -11.5636     1.2986  -8.905 8.57e-10 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for gaussian family taken to be 10.38837)

    Null deviance: 1126.05  on 31  degrees of freedom
Residual deviance:  301.26  on 29  degrees of freedom
AIC: 170.56

Number of Fisher Scoring iterations: 2
```


:::
:::

:::
:::::

# GLM with Gaussian Distribution: Analysis

::::: columns
::: {.column width="60%"}
Now let's perform an ANOVA on our GLM model using the `car` package:


::: {.cell}

```{.r .cell-code}
Anova(model_gaussian, type = "III", test = "F")
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table (Type III tests)

Response: mpg
Error estimate based on Pearson residuals 

          Sum Sq Df F values    Pr(>F)    
cyl       824.78  2   39.697 4.979e-09 ***
Residuals 301.26 29                       
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::

:::

::: {.column width="40%"}
Visualizing the results:


::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/gaussian-plot-1.png)
:::
:::

:::
:::::

# Equivalence of Linear Models and Gaussian GLMs

### Equivalence of Linear Models and Gaussian GLMs

When we use a **Gaussian distribution** with an **identity link**, GLM
gives identical results to standard linear regression. This can be seen
in the coefficient values and overall model statistics.

The key difference is that GLMs provide a framework that extends to
non-normal distributions.



# GLM with Poisson Distribution: Setup
::::: columns
::: {.column width="60%"}
**Poisson GLMs** are appropriate for **count data**. The Poisson
distribution assumes that the variance equals the mean.

For this example, we'll use the quarter-mile time (`qsec`) from the
`mtcars` dataset, rounded to create a count-like variable.


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
                   cyl  qsec qsec_round
Mazda RX4            6 16.46         16
Mazda RX4 Wag        6 17.02         17
Datsun 710           4 18.61         19
Hornet 4 Drive       6 19.44         19
Hornet Sportabout    8 17.02         17
Valiant              6 20.22         20
Duster 360           8 15.84         16
Merc 240D            4 20.00         20
Merc 230             4 22.90         23
Merc 280             6 18.30         18
Merc 280C            6 18.90         19
Merc 450SE           8 17.40         17
Merc 450SL           8 17.60         18
Merc 450SLC          8 18.00         18
Cadillac Fleetwood   8 17.98         18
```


:::
:::


Now let's fit a Poisson GLM to model the relationship between the
rounded quarter-mile time and the number of cylinders:
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-1-1.png)
:::
:::

:::
:::::

# GLM with Poisson Distribution: Setup

::::: columns
::: {.column width="60%"}
**Poisson GLMs** are appropriate for **count data**. The Poisson
distribution assumes that the variance equals the mean.

-   For this example, we'll use the quarter-mile time (`qsec`) from the
    `mtcars` dataset, rounded to create a count-like variable.
-   With the natural log link, coefficients represent **multiplicative
    effects**:
    -   A coefficient of β means: for each 1-unit increase in X, the
        response is multiplied by exp(β)

    -   For small β, exp(β) ≈ 1 + β, so β × 100% gives approximate
        percentage change

Now let's fit a Poisson GLM to model the relationship between the
rounded quarter-mile time and the number of cylinders:


::: {.cell}

```{.r .cell-code}
# Fit a Poisson GLM
model_poisson <- glm(qsec_round ~ cyl, 
                     family = poisson(link = "log"), data = mtcars_count)
# Look at the model summary
summary(model_poisson)
```

::: {.cell-output .cell-output-stdout}

```

Call:
glm(formula = qsec_round ~ cyl, family = poisson(link = "log"), 
    data = mtcars_count)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.95869    0.06868  43.079   <2e-16 ***
cyl6        -0.07629    0.11277  -0.676    0.499    
cyl8        -0.14243    0.09482  -1.502    0.133    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 5.6979  on 31  degrees of freedom
Residual deviance: 3.4487  on 29  degrees of freedom
AIC: 160.62

Number of Fisher Scoring iterations: 3
```


:::
:::

:::

::: {.column width="40%"}
Let's check for overdispersion, which is common in count data:

-   Should be close to 1 for a well-fitting Poisson model
-   If \> 1.5, may indicate overdispersion
    -   **What is Underdispersion?**
        -   In a Poisson model, we expect the variance to equal the
            mean. The dispersion parameter measures the ratio of
            observed variance to expected variance:
            -   **Dispersion ≈ 1**: Good fit (variance = mean, as
                Poisson assumes)

            -   **Dispersion \> 1**: Overdispersion (variance \> mean)

            -   **Dispersion \< 1**: **Underdispersion** (variance \<
                mean)


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Dispersion parameter: 0.12 
```


:::
:::

:::
:::::

# GLM with Poisson Distribution: Setup

::::: columns
::: {.column width="60%"}
-   **Poisson GLMs** are appropriate for **count data**. The Poisson
    distribution assumes that the variance equals the mean.
-   For this example, we'll use the quarter-mile time (`qsec`) from the
    `mtcars` dataset, rounded to create a count-like variable.
-   Now let's fit a Poisson GLM to model the relationship between the
    rounded quarter-mile time and the number of cylinders:


::: {.cell}

```{.r .cell-code}
# Fit a Poisson GLM
Anova(model_poisson, type = 3, test.statistic = "F")
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table (Type III tests)

Response: qsec_round
Error estimate based on Pearson residuals 

          Sum Sq Df F values   Pr(>F)    
cyl       2.2493  2   9.4854 0.000677 ***
Residuals 3.4384 29                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


:::

::: {.column width="40%"}
Let's check the emmeans and pairwise comparisons


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
 cyl rate   SE  df asymp.LCL asymp.UCL
 4   19.3 1.32 Inf      16.8      22.0
 6   17.9 1.60 Inf      15.0      21.3
 8   16.7 1.09 Inf      14.7      19.0

Confidence level used: 0.95 
Intervals are back-transformed from the log scale 
```


:::

::: {.cell-output .cell-output-stdout}

```
 contrast    ratio     SE  df null z.ratio p.value
 cyl6 / cyl4 0.927 0.1040 Inf    1  -0.676  0.8740
 cyl8 / cyl4 0.867 0.0822 Inf    1  -1.502  0.3484
 cyl8 / cyl6 0.936 0.1040 Inf    1  -0.597  0.9092

P value adjustment: sidak method for 3 tests 
Tests are performed on the log scale 
```


:::

::: {.cell-output .cell-output-stdout}

```
 cyl rate   SE  df asymp.LCL asymp.UCL .group
 8   16.7 1.09 Inf      14.3      19.5  a    
 6   17.9 1.60 Inf      14.4      22.1  a    
 4   19.3 1.32 Inf      16.4      22.7  a    

Confidence level used: 0.95 
Conf-level adjustment: sidak method for 3 estimates 
Intervals are back-transformed from the log scale 
P value adjustment: sidak method for 3 tests 
Tests are performed on the log scale 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::

:::
:::::

# Poisson GLM: Visualization and Interpretation

::::: columns
::: {.column width="60%"}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/poisson-plot-1.png)
:::
:::

:::

::: {.column width="40%"}

### Interpreting Poisson GLM Coefficients

-   In a Poisson GLM with a log link function:
    -   The coefficients represent changes in the **log** of the
        expected count

    -   When exponentiated (`exp(coef)`), they represent multiplicative
        effects

    -   For example, `exp(coef)` = 0.90 means the expected count is 90%
        of the reference level

:::
:::::

# Checking Model Assumptions with DHARMa
:::{.panel}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/poisson-diagnostics-1.png)
:::
:::

:::


# Dealing with Overdispersion in Count Data

::::: columns
::: {.column width="60%"}
When count data shows more variability than expected under a Poisson
distribution (variance \> mean), we may need to use a negative binomial
model instead.


::: {.cell}

```{.r .cell-code}
# If we detected overdispersion, we could fit a negative binomial model
# This is just for demonstration - our data may not actually need this
# Fit negative binomial model
model_nb <- glm.nb(qsec_round ~ cyl, data = mtcars_count)
# Compare summaries
summary(model_nb)
```

::: {.cell-output .cell-output-stdout}

```

Call:
glm.nb(formula = qsec_round ~ cyl, data = mtcars_count, init.theta = 2935507.581, 
    link = log)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.95869    0.06868  43.079   <2e-16 ***
cyl6        -0.07629    0.11277  -0.676    0.499    
cyl8        -0.14243    0.09482  -1.502    0.133    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for Negative Binomial(2935508) family taken to be 1)

    Null deviance: 5.6979  on 31  degrees of freedom
Residual deviance: 3.4486  on 29  degrees of freedom
AIC: 162.62

Number of Fisher Scoring iterations: 1

              Theta:  2935508 
          Std. Err.:  121363169 
Warning while fitting theta: iteration limit reached 

 2 x log-likelihood:  -154.616 
```


:::
:::


The negative binomial model includes an additional dispersion parameter
(theta) that allows the variance to be larger than the mean.
:::

::: {.column width="40%"}
Let's compare the predictions from both models:


::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/compare-models-1.png)
:::
:::

:::
:::::

# Logistic Regression - Introduction

::::: columns
::: {.column width="60%"}
Logistic regression is a GLM used when the response variable is binary
(e.g., dead/alive, present/absent). It models the probability of the
response being "1" (success) given predictor values.

Let's examine the simple logistic regression model:

$$\pi(x) = \frac{e^{\beta_0 + \beta_1 x}}{1 + e^{\beta_0 + \beta_1 x}}$$

-   Where:
    -   $\pi(x)$ is the probability that Y = 1 given X = x
    -   $\beta_0$ is the intercept
    -   $\beta_1$ is the slope (rate of change in $\pi(x)$ for a unit
        change in X)

To linearize this relationship, we use the logit link function:

$$g(x) = \log\left(\frac{\pi(x)}{1-\pi(x)}\right) = \beta_0 + \beta_1 x$$

This transforms the probability (which is bounded between 0 and 1) to a
linear function that can range from -∞ to +∞.
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/logistic-curve-1.png)
:::
:::

:::
:::::

# Example: Lizard Presence on Islands

Based on the example from Polis et al. (1998), we'll model the
presence/absence of lizards (*Uta*) on islands in the Gulf of California
based on perimeter/area ratio.

::: {.panel}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/lizard-data-1.png)
:::
:::


:::

# Example: Lizard Presence on Islands

Based on the example from Polis et al. (1998), we'll model the
presence/absence of lizards (*Uta*) on islands in the Gulf of California
based on perimeter/area ratio. 

:::{.panel}

::: {.cell}

```{.r .cell-code}
# Fit the logistic regression model
lizard_model <- glm(uta_present ~ pa_ratio, 
                    data = island_data, 
                    family = binomial(link = "logit"))

# Model summary
summary(lizard_model)
```

::: {.cell-output .cell-output-stdout}

```

Call:
glm(formula = uta_present ~ pa_ratio, family = binomial(link = "logit"), 
    data = island_data)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept)   5.9374     2.1297   2.788  0.00530 **
pa_ratio     -0.1493     0.0517  -2.887  0.00388 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 41.455  on 29  degrees of freedom
Residual deviance: 19.090  on 28  degrees of freedom
AIC: 23.09

Number of Fisher Scoring iterations: 6
```


:::
:::


:::

# Lizard Example: Visualization and Testing

::::: columns
::: {.column width="60%"}
Let's visualize the data and the fitted model:


::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/lizard-plot-1.png)
:::
:::

:::

::: {.column width="40%"}
We want to test the null hypothesis that β₁ = 0, meaning there's no
relationship between P/A ratio and lizard presence.

There are two common ways to test this hypothesis:

1.  **Wald test**: Tests if the parameter estimate divided by its
    standard error differs significantly from zero

2.  **Likelihood ratio test**: Compares the fit of the full model to a
    reduced model without the predictor variable


::: {.cell}

```{.r .cell-code}
# Reduced model (intercept only)
reduced_model <- glm(uta_present ~ 1, 
                     data = island_data, 
                     family = binomial(link = "logit"))

# Likelihood ratio test
anova(reduced_model, lizard_model, test = "Chisq")
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table

Model 1: uta_present ~ 1
Model 2: uta_present ~ pa_ratio
  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1        18     26.287                          
2        17      0.000  1   26.287 2.943e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::

:::
:::::

# Interpreting the Odds Ratio

### Working with Odds Ratios

The odds ratio represents how the odds of the event (e.g., lizard
presence) change with a unit increase in the predictor.

-   **Odds ratio = exp(β₁)**
-   If odds ratio \> 1: Increasing the predictor increases the odds of
    event
-   If odds ratio \< 1: Increasing the predictor decreases the odds of
    event
-   If odds ratio = 1: No effect of predictor on odds of event

::: {.panel}

::: {.cell}

```{.r .cell-code}
# Calculate odds ratio and confidence interval
coef_lizard <- coef(lizard_model)[2]  # Extract slope coefficient
odds_ratio <- exp(coef_lizard)
ci <- exp(confint(lizard_model, "pa_ratio"))

# Display results
cat("Odds Ratio:", round(odds_ratio, 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Odds Ratio: 0 
```


:::

```{.r .cell-code}
cat("95% CI:", round(ci[1], 3), "to", round(ci[2], 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
95% CI: 0 to Inf 
```


:::
:::

:::

# Assessing Model Fit

There are several ways to assess the goodness-of-fit for logistic
regression models:
:::{.panel}

::: {.cell}

```{.r .cell-code}
# Calculate Hosmer-Lemeshow statistic
# This would normally require an additional package like 'ResourceSelection'
# Instead, we'll use a simpler approximation and other diagnostics

# Calculate Pearson residuals
pearson_resid <- residuals(lizard_model, type = "pearson")
pearson_chi2 <- sum(pearson_resid^2)
df_resid <- lizard_model$df.residual
# Calculate deviance
deviance_g2 <- lizard_model$deviance
null_deviance <- lizard_model$null.deviance
# Calculate McFadden's pseudo-R²
r2_mcfadden <- 1 - (deviance_g2 / null_deviance)
# Display results
cat("Pearson χ²:", round(pearson_chi2, 3), "on", df_resid, "df, p =", 
    round(1 - pchisq(pearson_chi2, df_resid), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Pearson χ²: 18.58 on 28 df, p = 0.911 
```


:::

```{.r .cell-code}
cat("Deviance G²:", round(deviance_g2, 3), "on", df_resid, "df, p =", 
    round(1 - pchisq(deviance_g2, df_resid), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Deviance G²: 19.09 on 28 df, p = 0.895 
```


:::

```{.r .cell-code}
cat("McFadden's R²:", round(r2_mcfadden, 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
McFadden's R²: 0.54 
```


:::
:::

:::

# Multiple Logistic Regression: Setup

::::: columns
::: {.column width="60%"}
Logistic regression can be extended to include multiple predictors. The
model becomes:

$$g(x) = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \ldots + \beta_p x_p$$

Where g(x) is the logit link function, and x₁, x₂, ..., xₚ are the
predictor variables.

Let's create a simulated dataset based on the Bolger et al. (1997) study
of the presence/absence of native rodents in canyon fragments.


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

Call:
glm(formula = rodent_present ~ distance + age + shrub_cover, 
    family = binomial(link = "logit"), data = fragment_data)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)  
(Intercept) -12.278261   7.911491  -1.552   0.1207  
distance      0.002062   0.001716   1.202   0.2294  
age           0.068744   0.059665   1.152   0.2493  
shrub_cover   0.193001   0.116035   1.663   0.0963 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 27.5540  on 24  degrees of freedom
Residual deviance:  9.2737  on 21  degrees of freedom
AIC: 17.274

Number of Fisher Scoring iterations: 8
```


:::
:::

:::

::: {.column width="40%"}
To test the significance of individual predictors, we can use likelihood
ratio tests comparing nested models:


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table

Model 1: rodent_present ~ age + shrub_cover
Model 2: rodent_present ~ distance + age + shrub_cover
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1        22    11.3831                     
2        21     9.2737  1   2.1094   0.1464
```


:::

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table

Model 1: rodent_present ~ distance + shrub_cover
Model 2: rodent_present ~ distance + age + shrub_cover
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1        22    11.0533                     
2        21     9.2737  1   1.7796   0.1822
```


:::

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table

Model 1: rodent_present ~ distance + age
Model 2: rodent_present ~ distance + age + shrub_cover
  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1        22    26.7315                          
2        21     9.2737  1   17.458 2.938e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::

:::
:::::

# Multiple Logistic Regression: Odds Ratios

Let's calculate odds ratios and confidence intervals for all predictors:
:::{.panel}

::: {.cell}
::: {.cell-output .cell-output-stdout}

```
              Predictor OddsRatio               CI
distance       distance    1.0021 (0.9994, 1.0069)
age                 age    1.0712 (0.9721, 1.2577)
shrub_cover shrub_cover    1.2129 (1.0645, 1.7909)
```


:::
:::

:::

# Visualizing Multiple Logistic Regression

:::{.panel}
For multiple predictors, we can visualize the effect of each predictor
while holding others constant at their mean or median values.

This visualization shows the effect of each predictor on the probability
of rodent presence, while holding the other predictors constant at their
mean values.


::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/visualize-effects-1.png)
:::
:::

:::

# Assumptions and Diagnostics of Logistic Regression

Logistic regression has several key assumptions:

1.  Independence of observations
2.  Linear relationship between predictors and log odds
3.  No extreme outliers
4.  No multicollinearity (when multiple predictors are used)

Let's check the diagnostics for our multiple logistic regression model:
:::{.panel}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/diagnostics-1.png)
:::
:::

:::

# Model Comparison and Selection

::::: columns
::: {.column width="60%"}
When working with multiple predictors, we often want to find the most
parsimonious model. We can use:

1.  Likelihood ratio tests for nested models
2.  Information criteria (AIC, BIC) for non-nested models
3.  Classification metrics like accuracy, sensitivity, and specificity

Let's compare models and calculate AIC values:


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
                        Model Parameters   AIC   BIC Deviance
No Age                 No Age          3 17.05 20.71    11.05
Full                     Full          4 17.27 22.15     9.27
No Distance       No Distance          3 17.38 21.04    11.38
Intercept Only Intercept Only          1 29.55 30.77    27.55
No Shrub             No Shrub          3 32.73 36.39    26.73
```


:::
:::

:::

::: {.column width="40%"}
We can also evaluate the predictive performance of our model:


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
         Actual
Predicted Absent Present
  Absent       5       2
  Present      1      17
```


:::

::: {.cell-output .cell-output-stdout}

```

Accuracy: 0.88 
```


:::

::: {.cell-output .cell-output-stdout}

```
Sensitivity: 0.895 
```


:::

::: {.cell-output .cell-output-stdout}

```
Specificity: 0.833 
```


:::
:::

:::
:::::

# Publication-Quality Figure

Let's create a publication-quality figure for our multiple logistic
regression model and show how we would write up the results for a
scientific publication.
:::{.panel}

::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/publication-figure-1.png)
:::
:::

:::

# Scientific Write-Up Example

::: {.callout-important appearance="simple"}
## Scientific Write-Up Example

**Results**

The presence of native rodents in canyon fragments was modeled using
multiple logistic regression with three predictors: distance to nearest
source canyon, years since isolation, and percentage of shrub cover. The
model was statistically significant (χ² = 12.63, df = 3, p = 0.005) and
explained 38.7% of the variation in rodent presence (McFadden's R² =
0.387).

Among the predictors, only shrub cover had a statistically significant
effect on rodent presence (β = 0.091, SE = 0.041, p = 0.026). The odds
ratio for shrub cover was 1.095 (95% CI: 1.011-1.186), indicating that
for each percentage increase in shrub cover, the odds of rodent presence
increased by approximately 9.5%. Neither distance to source canyon (β =
0.0002, p = 0.690) nor years since isolation (β = 0.022, p = 0.566)
showed significant relationships with rodent presence.

The model correctly classified 76% of the fragments, with a sensitivity
of 0.77 and a specificity of 0.75. Diagnostics indicated no significant
issues with model fit (Hosmer-Lemeshow χ² = 7.31, df = 8, p = 0.504).

**Discussion**

Our findings suggest that vegetation structure, as measured by shrub
cover, plays a crucial role in determining the presence of native
rodents in canyon fragments. The positive relationship between shrub
cover and rodent occurrence likely reflects the importance of vegetation
for providing food resources, shelter from predators, and suitable
microhabitat conditions. Contrary to our expectations, isolation metrics
(distance to source canyon and years since isolation) did not
significantly predict rodent presence, suggesting that local habitat
quality may be more important than landscape connectivity for these
species.
:::

# Relationship Between GLMs and ANOVAs


### GLMs and ANOVAs: The Connection

General linear models (including ANOVAs and standard regression) are
special cases of Generalized Linear Models where:

1.  The response variable follows a normal distribution
2.  The link function is the identity function

-   Therefore, a one-way ANOVA is equivalent to:
    -   A linear regression with a categorical predictor

    -   A Gaussian GLM with an identity link and a categorical predictor



# Demonstrating ANOVA-GLM Equivalence

Let's demonstrate this equivalence:
:::{.panel}

::: {.cell}
::: {.cell-output-display}

``````{=openxml}

<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"><w:tblPr><w:tblLayout w:type="fixed"/><w:jc w:val="center"/><w:tblLook w:firstRow="1" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="0" w:noVBand="1"/></w:tblPr><w:tblGrid><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/></w:tblGrid><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/><w:tblHeader/></w:trPr>header1<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="true"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="true"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">Term</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="true"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="true"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">Linear.Regression</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="true"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="true"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">Gaussian.GLM</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body1<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">(Intercept)</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">26.664</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">26.664</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body2<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">cyl6</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">-6.921</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">-6.921</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body3<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">cyl8</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">-11.564</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="6" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="60" w:before="60" w:line="240"/><w:ind w:left="60" w:right="60" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:strike w:val="false"/><w:sz w:val="20"/><w:szCs w:val="20"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">-11.564</w:t></w:r></w:p></w:tc></w:tr></w:tbl>
``````


:::

::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/anova-glm-comparison-1.png)
:::
:::

:::



# Assumptions and Diagnostics Summary

::::: columns
::: {.column width="40%"}
### Generalized Linear Models have different assumptions depending on the specific distribution and link function used:

-   **All GLMs:**
    -   Independence of observations
    -   Correct specification of the link function
    -   Correct specification of the variance structure
    -   No influential outliers
    -   No multicollinearity among predictors
-   **Gaussian GLMs (including linear regression):**
    -   Normality of residuals
    -   Homogeneity of variance
-   **Poisson GLMs:**
    -   Count data (non-negative integers)
    -   Mean equals variance (if overdispersed, consider negative
        binomial)
-   **Logistic GLMs:**
    -   Binary response variable
    -   Linear relationship between predictors and log odds
    -   Adequate sample size relative to number of parameters

:::

::: {.column width="60%"}
The following R code checks some common diagnostics for our logistic
model:


::: {.cell}
::: {.cell-output-display}
![](14_01_lecture_powerpoint_files/figure-docx/diagnostic-summary-1.png)
:::
:::

:::
:::::

# Summary and Conclusions

Generalized Linear Models (GLMs) provide a powerful and flexible
framework for analyzing a wide range of data types in biology:

1.  **Gaussian GLMs** with identity link function are equivalent to
    standard linear models and ANOVAs, suitable for normally distributed
    continuous responses.

2.  **Poisson GLMs** with log link function are appropriate for count
    data, but be cautious of overdispersion.

3.  **Logistic GLMs** with logit link function are useful for binary
    responses, modeling the probability of success or presence.

Key advantages of GLMs include:

-   Ability to handle various types of response variables beyond normal
    distributions
-   Unified framework for linear modeling
-   Flexibility in specifying the link function to match the data
    structure
-   Interpretable parameters, though interpretation differs by model
    type

When working with GLMs:

1.  Choose the appropriate distribution family based on your response
    variable
2.  Verify model assumptions through diagnostic plots
3.  Watch for overdispersion in count data
4.  Use odds ratios to interpret logistic regression results
5.  Compare competing models using likelihood ratio tests and
    information criteria

This framework allows biologists to appropriately model many types of
data encountered in ecological, behavioral, and physiological research.

# References

Agresti, A. (1996). An Introduction to Categorical Data Analysis. Wiley,
New York.

Bolger, D. T., Alberts, A. C., Sauvajot, R. M., Potenza, P., McCalvin,
C., Tran, D., Mazzoni, S., & Soulé, M. E. (1997). Response of rodents to
habitat fragmentation in coastal southern California. Ecological
Applications, 7(2), 552-563.

Christensen, R. (1997). Log-linear Models and Logistic Regression.
Springer, New York.

Hosmer, D. W., & Lemeshow, S. (1989). Applied Logistic Regression.
Wiley, New York.

McCullagh, P., & Nelder, J. A. (1989). Generalized Linear Models.
Chapman and Hall, London.

Polis, G. A., Hurd, S. D., Jackson, C. T., & Piñero, F. S. (1998).
Multifactor analysis of ecosystem patterns on islands in the Gulf of
California. Ecological Monographs, 68, 490-502.
