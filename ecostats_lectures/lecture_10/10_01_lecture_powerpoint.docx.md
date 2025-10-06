---
title: "Lecture 10 - Multiple Regression"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "10_01_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  revealjs:
    output-file: "10_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "10_01_lecture_powerpoint.docx"
  pptx:
    output-file: "10_01_lecture_powerpoint.pptx"
  typst:
    output-file: "10_01_lecture_powerpoint.pdf"
---



# Lecture 09: Review

::::: columns
::: {.column width="60%"}
Covered

-   Regression T-Test Anova
-   Regression Assumptions
-   sum of squared allocation and subdivision from total sums of squares
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/review-plot-1.png)
:::
:::

:::
:::::

# Lecture 10: Overview

Multiple Linear Regression model

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

# **Lecture 10:** Analyses

::::: columns
::: {.column width="60%"}
What if more than one predictor (X) variable?

-   If predictors continuous
-   Mix between categorical and continuous
-   Can use multiple linear regression
:::

::: {.column width="40%"}
|                        | Independent variable |                 |
|:-----------------------|:---------------------|:----------------|
| **Dependent variable** | **Continuous**       | **Categorical** |
| **Continuous**         | Regression           | ANOVA           |
| **Categorical**        | Logistic regression  |                 |
:::
:::::

# **Lecture 10:** Analyses

::::: columns
::: {.column width="20%"}
Abundance of ants can be modeled as function of

-   latitude
-   longitude
-   both

Instead of line, modeled with (hyper)plane
:::

::: {.column width="80%"}

::: {.cell}
::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-1-1.png)
:::
:::

:::
:::::

# **Lecture 10:** Analyses

::::: columns
::: {.column width="20%"}
Used in similar way to simple linear regression:

-   Describe nature of relationship between Y and X's
-   Determine explained / unexplained variation in Y
-   Predict new Ys from X
-   Find the “best” model
:::

::: {.column width="80%"}

::: {.cell}
::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-2-1.png)
:::
:::

:::
:::::

# **Lecture 10:** Analyses

::::: columns
::: {.column width="60%"}
Crawley 2012: “Multiple regression models provide some of the most
profound challenges faced by the analyst”:

-   Overfitting
-   Parameter proliferation
-   Multicollinearity
-   Model selection
:::

::: {.column width="40%"}
![](images/clipboard-1736453741.png){width="277"}
:::
:::::

# **Lecture 10:** Analyses

Multiple Regression:

-   Set of i= 1 to n observations
-   fixed X-values for p predictor variables (X1, X2…Xp)
-   random Y-values:

$$y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + ... + \beta_p x_{ip} + \epsilon_i$$

-   y~i~: value of Y for i^th^ observation X~1~ = x~i1~, X~2~ = x~i2~,…,
    x~p~ = x~ip~

-   β~0~: population intercept, the mean value of Y when X~1~= 0, X~2~ =
    0,…, X~p~ = 0

# **Lecture 10:** Multiple linear regression model

Multiple Regression:

$$y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + ... + \beta_p x_{ip} + \epsilon_i$$

-   β~1~: partial regression slope, change in Y per unit change in X~1~
    holding other X-vars constant

-   β~2~: partial regression slope, change in Y per unit change in X~2~
    holding other X-vars constant

-   β~p~: partial regression slope, change in Y per unit change in X~p~
    holding other X-vars constant

What is partial - the relationship between a predictor variable and the
response variable **while holding all other predictor variables
constant**. It tells you the isolated effect of one variable,
controlling for the influence of others.

# Lecture 10: Partial regression slopes

::::: columns
::: {.column width="60%"}
model: ant_spp \~ elevation + latitude

-   The partial slope for elevation tells you how ant species richness
    changes with elevation when latitude is held constant
    -   1-m increase in elevation - ant spp decrease by 0.012 spp,
        holding latitude constant

    -   Or 100-meter increase in elevation, lose 1.2 spp
-   The partial slope for latitude tells you how ant species richness
    changes with latitude when elevation is held constant
    -   1-degree increase in latitude, ant spp decrease by 2 species,
        holding elevation constant

    -   Moving north = fewer ant spp, even when comparing sites at same
        elevation
:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
model <- lm(ant_spp ~ elevation + latitude, data = ant_df)
summary(model)
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = ant_spp ~ elevation + latitude, data = ant_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.1180 -2.3759  0.3218  1.9070  5.8369 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 98.49651   26.50701   3.716  0.00147 **
elevation   -0.01226    0.00411  -2.983  0.00765 **
latitude    -2.00981    0.61956  -3.244  0.00427 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 3.022 on 19 degrees of freedom
Multiple R-squared:  0.5543,	Adjusted R-squared:  0.5074 
F-statistic: 11.82 on 2 and 19 DF,  p-value: 0.000463
```


:::
:::

:::
:::::

# **Lecture 10:** Regression parameters

Multiple Regression:

$$y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + ... + \beta_p x_{ip} + \epsilon_i$$

-   ε~i~: unexplained error - difference between y~i~ and value
    predicted by model (ŷ~i~)
-   ant_spp = β~0~ + β~1~(lat) + β~2~(elevation) + ε~i~

# **Lecture 10:** Regression parameters

Multiple Regression:

$$y_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + ... + \beta_p x_{ip} + \epsilon_i$$

-   Estimate multiple regression parameters (intercept, partial slopes)
    using OLS to fit the regression line
-   OLS minimize ∑(y~i~-ŷ~i~)^2^, the SS (vertical distance) between
    observed y~i~ and predicted ŷ~i~ for each x~ij~
-   ε estimated as residuals: ε~i~ = y~i~-ŷ~i~
-   Calculation solves set of simultaneous normal equations with matrix
    algebra

# **Lecture 10:** Regression parameters

Regression equation can be used for prediction by subbing new values for
predictor (X) variables

-   Confidence intervals calculated for parameters
-   Confidence and prediction intervals depend on number of observations
    and number of predictors
    -   More observations decrease interval width
    -   More predictors increase interval width
-   Prediction should be restricted to within range of X variables

# **Lecture 10:** Analyses of variance or sums of squares

Variance - SS~total~ partitioned into SS~regression~ and SS~residual~

-   SS~regression~ is variance in Y explained by model

-   SS~residual~ is variance not explained by model

| Source of variation | SS | df | MS | Interpretation |
|:--------------|:--------------|:--------------|:--------------|:--------------|
| Regression | $\sum_{i=1}^{n} (y_i - \bar{y})^2$ | $p$ | $\frac{\sum_{i=1}^{n} (y_i - \bar{y})^2}{p}$ | Difference between predicted observation and mean |
| Residual | $\sum_{i=1}^{n} (y_i - \hat{y}_i)^2$ | $n-p-1$ | $\frac{\sum_{i=1}^{n} (y_i - \hat{y}_i)^2}{n-p-1}$ | Difference between each observation and predicted |
| Total | $\sum_{i=1}^{n} (y_i - \bar{y})^2$ | $n-1$ |  | Difference between each observation and mean |

# **Lecture 10:** Analyses

### SS converted to non-additive MS=(SS/df)

-   MS~residual~: estimate population variance
-   MS~regression~: estimate population variance + variation due to
    strength of X-Y relationships
-   MS is an estimate of variance that doesn't increase with sample size
    the way Sum of Squares (SS) does

| Source of variation | SS | df | MS |
|:-----------------|:-----------------|:-----------------|:-----------------|
| Regression | $\sum_{i=1}^{n} (y_i - \bar{y})^2$ | $p$ | $\frac{\sum_{i=1}^{n} (y_i - \bar{y})^2}{p}$ |
| Residual | $\sum_{i=1}^{n} (y_i - \hat{y}_i)^2$ | $n-p-1$ | $\frac{\sum_{i=1}^{n} (y_i - \hat{y}_i)^2}{n-p-1}$ |
| Total | $\sum_{i=1}^{n} (y_i - \bar{y})^2$ | $n-1$ |  |

# **Lecture 10:** Hypotheses

### Two Null Hypotheses usually tested in MLR:

-   “Basic” H~o~: all partial regression slopes equal 0; β~1~ = β~2~ = …
    = β~p~ = 0
-   If “basic” H~o~ true, MS~regression~ and MS~residual~ estimate
    variance and their ratio (F-ratio) = 1
-   If “basic” H~o~ false (at least one β ≠ 0) MS~regression~ estimates
    variance + partial regression slope and their ratio (F-ratio) will
    be \> 1 - F-ratio compared to F-distribution for p-value

# **Lecture 10:** Hypotheses

### Also: is any specific β = 0 (explanatory role)?

-   E.g., does LAT have effect on Ant species?
-   These H's tested through model comparison
-   Model 1 - model with 2 predictors X~1~, X~2~:
    -   yi= β~0~ +β~1~x~i1~+β~2~x~i2~+ ε~i~
-   Model 2 - To test H~o~ that β~2~ = 0 compare fit of **model 1** to
    **model 2** with 1 predictor:
    -   y~i~= β~0~ +β~1~x~i1~+ ε~i~

# **Lecture 10:** Hypotheses

-   If SS~regression~ of model~1~=model~2~, cannot reject H~o~ β~1~ = 0
    -   adding X~2~ did not explain any additional variance
-   If SS~regression~ of mod1 \> mod2, evidence to reject H~o~ β~1~ = 0
    -   adding X2 explains more variance
    -   evidence to reject H0: β~2~ = 2
-   SS for β~1~ is SS~extra~β~1~ = Full SS~regression~ - Reduced
    SS~regression~
    -   This quantifies how much additional variance X~2~ explains
-   Use partial F-test to test Ho β~1~ = 0 :

$$F_{w,n-p} = \frac{MS_{Extra}}{FULL\ MS_{Residual}}  $$ **Can also use
t-test (R provides this value)**

# **Lecture 10:** Explained variance

Explained variance (r^2^) is calculated the same way as for simple
regression:

$$r^2 = \frac{SS_{Regression}}{SS_{Total}} = 1 - \frac{SS_{Residual}}{SS_{Total}}  $$

-   r^2^ values can not be used to directly compare models
-   r^2^ values will always increase as predictors added
-   r^2^ values with different transformation will differ

# **Lecture 10:** Assumptions and diagnostics

::::: columns
::: {.column width="60%"}
-   Assume fixed Xs; unrealistic in most biological settings
-   No major (influential) outliers
-   Check leverage, influence- Cook’s Di
:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
# Plot Cook's distance
plot(model, which = 4)
```

::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-4-1.png)
:::
:::

:::
:::::

# **Lecture 10:** Assumptions and diagnostics

::::: columns
::: {.column width="60%"}
-   Normality, equal variance, independence
-   Residual QQ-plots, residuals vs. predicted values plot
-   Distribution/variance often corrected by transforming Y
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/qq_and_resid-1.png)
:::
:::

:::
:::::

# **Lecture 10:** Assumptions and diagnostics

-   **More observations than predictor variables**
    -   Ideally **at least 10x observations than predictors** to avoid
        “overfitting” **10:1**

        -   2 predictors = 20 observations!!!
-   **No colinearity**
    -   **Need Uncorrelated predictor variables (assessed using
        scatterplot matrix; VIFs)**
-   **Each X has linear relationship with Y after accounting for other
    predictors**
    -   **Added Variable (AV) plots** (also called partial regression
        plots)

        -   These show the relationship between Y and X₁ after removing
            the linear effects of all other predictors

        -   Create AV plots for all predictors - `avPlots(model)`

        -   Or for just one predictor -
            `avPlot(model, variable = "proportion_black")`

# **Lecture 10:** Analyses

::::: columns
::: {.column width="40%"}
Regression of Y vs. each X does not consider effect of other predictors:

want to know shape of relationship while holding other predictors
constant
:::

::: {.column width="60%"}

::: {.cell}
::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-5-1.png)
:::
:::

:::
:::::

# **Lecture 10:** Collinearity

-   Potential predictor variables are often correlated (e.g.,
    morphometrics, nutrients, climatic parameters)
-   Multicollinearity (strong correlation between predictors) causes
    problems for parameter estimates
-   Severe collinearity causes unstable parameter estimates: small
    change in a single value can result in large changes in βp -
    estimates
-   Inflates partial slope error estimates, loss of power


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
           elevation   latitude    ant_spp
elevation  1.0000000  0.1787454 -0.5545244
latitude   0.1787454  1.0000000 -0.5879407
ant_spp   -0.5545244 -0.5879407  1.0000000
```


:::
:::


# **Lecture 10:** Collinearity

### Collinearity can be detected by:

-   Variance inflation Factors **(VIF)**:
    -   VIF for X~j~=1/ (1-r^2^ )
    -   VIF \> 10 = bad
-   Best/simplest solution:
    -   exclude variables that are highly correlated with other
        variables
    -   they are probably measuring similar things and are redundant

# **Lecture 10:** Interactions

### Predictors can be modeled as:

-   [**additive (effect of temp, plus precip, plus fertility)
    or**]{.underline}
-   [**multiplicative (interactive)**]{.underline}
-   **Interaction: effect of Xi depends on levels of Xj**
-   The partial slope of Y vs. X~1~ is different for different levels of
    X~2~ (and vice versa); measured by β~3~

$$y_i = \beta_0 + \beta_1X_{i1} + \beta_2X_{i2} + \epsilon_i \quad \text{vs.} \quad y_i = \beta_0 + \beta_1X_{i1} + \beta_2X_{i2} + \beta_3X_{i1}*X_{i2} \epsilon_i$$

This leads to “Curvature” of the regression (hyper)plane

# **Lecture 10:** Analyses


::: {.cell}
::: {.cell-output-display}
![](10_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-7-1.png)
:::
:::


# **Lecture 10:** Analyses

Adding interactions:

-   many more predictors (“parameter proliferation”):
-   2n is required; 6 parameters= 64 terms; 7 parameters= 128
-   interpretation more complex
-   When to include interactions? When they make biological sense!!!

# **Lecture 10:** Dummy variables

**Multiple Linear Regression accommodates continuous and categorical
variables** (gender, vegetation type, etc.) Categorical vars as “dummy
vars”, n of dummy variables = n-1 categories

-   **Sex M/F:**
    -   Need 1 dummy var with two values (0, 1)
-   **Fertility L/M/H:**
    -   Need 2 dummy var, each with two values (0, 1): fert1 (0 if L or
        H, 1 if M), fert2 (1 if H, 0 if L or M)

| Fertility | fert1 | fert2 |
|:----------|:------|:------|
| Low       | 0     | 0     |
| Med       | 1     | 0     |
| High      | 0     | 1     |

# **Lecture 10:** Analyses

### Coefficients interpreted relative to reference condition

-   R codes dummy variables automatically
-   picks “reference” level alphabetically
-   Dummy variables with more than 2 levels add extra predictor
    variables to model

| Fertility | fert1 | fert2 |
|-----------|-------|-------|
| Low       | 0     | 0     |
| Med       | 1     | 0     |
| High      | 0     | 1     |

# **Lecture 10:** Analyses

::::: columns
::: {.column width="60%"}
![](images/clipboard-3847962697.png){width="540"}
:::

::: {.column width="40%"}
![](images/clipboard-3285444079.png){width="250"}
:::
:::::

# **Lecture 10:** Comparing models

-   When have multiple predictors (and interactions!)
    -   how to choose “best” model?
    -   Which predictors to include?
    -   Occam’s razor: “best” model balances complexity with fit to data
-   To chose:
    -   compare “nested” models
-   Overfitting
    -   getting high r2 just by having more (useless predictors)
    -   so r2 is not a good way of choosing between nested models

# **Lecture 10:** Comparing models

### Need to account for increase in fit with added predictors:

-   Adjusted r2
-   Akaike’s information criterion (AIC)
-   Both “penalize” models for extra predictors
-   Higher adjusted r2 and lower AIC are better when comparing models
    -   p = predictors
    -   n = \#

$$\text{Adjusted } r^2 = 1 - \frac{SS_{\text{Residual}}/(n - (p + 1))}{SS_{\text{Total}}/(n - 1)}$$
$$\text{Akaike Information Criterion (AIC)} = n[\ln(SS_{\text{Residual}})] + 2(p + 1) - n\ln(n)$$

# **Lecture 10:** Comparing models

-   But how to compare models?
    -   Can fit all possible models

        -   compare AICs or adj- r2,
        -   tedious w lots of predictors

    -   Automated forward (and backward) stepwise procedures: start w no
        terms (all terms), add (remove) terms w largest (smallest)

        -   partial F statistic
-   We will use manual form of backward selection

# **Lecture 10:** Analyses


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

========== FULL MODEL (Both Variables) ==========
```


:::

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = ant_spp ~ elevation + latitude, data = ant_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.1180 -2.3759  0.3218  1.9070  5.8369 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 98.49651   26.50701   3.716  0.00147 **
elevation   -0.01226    0.00411  -2.983  0.00765 **
latitude    -2.00981    0.61956  -3.244  0.00427 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 3.022 on 19 degrees of freedom
Multiple R-squared:  0.5543,	Adjusted R-squared:  0.5074 
F-statistic: 11.82 on 2 and 19 DF,  p-value: 0.000463
```


:::
:::


# **Lecture 10:** Analyses


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

========== ELEVATION ONLY MODEL ==========
```


:::

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = ant_spp ~ elevation, data = ant_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.9010 -2.6947 -0.9502  2.9657  7.6363 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 12.589088   1.385615   9.086 1.55e-08 ***
elevation   -0.014641   0.004913  -2.980   0.0074 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 3.671 on 20 degrees of freedom
Multiple R-squared:  0.3075,	Adjusted R-squared:  0.2729 
F-statistic: 8.881 on 1 and 20 DF,  p-value: 0.0074
```


:::
:::


# **Lecture 10:** Analyses


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

========== LATITUDE ONLY MODEL ==========
```


:::

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = ant_spp ~ latitude, data = ant_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.2223 -2.1188  0.0599  2.1267  6.4990 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 109.8532    30.9803   3.546  0.00203 **
latitude     -2.3401     0.7199  -3.251  0.00401 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 3.569 on 20 degrees of freedom
Multiple R-squared:  0.3457,	Adjusted R-squared:  0.313 
F-statistic: 10.57 on 1 and 20 DF,  p-value: 0.004006
```


:::
:::


# **Lecture 10:** Analyses


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

========== MODEL COMPARISON TABLE ==========
```


:::

::: {.cell-output .cell-output-stdout}

```
           Model Adjusted_R2      AIC R2_vs_Full AIC_vs_Full
1 Both Variables   0.5074180 115.8646  0.0000000    0.000000
2 Elevation Only   0.2728722 123.5608 -0.2345459    7.696164
3  Latitude Only   0.3129580 122.3132 -0.1944600    6.448613
```


:::

::: {.cell-output .cell-output-stdout}

```

========== KEY FINDINGS ==========
```


:::

::: {.cell-output .cell-output-stdout}

```
Full Model AIC: 115.86 (Adjusted R²: 0.5074)
```


:::

::: {.cell-output .cell-output-stdout}

```
Elevation Only AIC: 123.56 (Adjusted R²: 0.2729)
```


:::

::: {.cell-output .cell-output-stdout}

```
Latitude Only AIC: 122.31 (Adjusted R²: 0.3130)
```


:::

::: {.cell-output .cell-output-stdout}

```

--- Differences from Full Model ---
```


:::

::: {.cell-output .cell-output-stdout}

```
Removing latitude: AIC increases by 7.70, Adj R² decreases by -0.2345
```


:::

::: {.cell-output .cell-output-stdout}

```
Removing elevation: AIC increases by 6.45, Adj R² decreases by -0.1945
```


:::

::: {.cell-output .cell-output-stdout}

```

========== CONCLUSION ==========
```


:::

::: {.cell-output .cell-output-stdout}

```
✓ Full model has LOWEST AIC (best fit)
```


:::

::: {.cell-output .cell-output-stdout}

```
✓ Full model has HIGHEST Adjusted R² (explains most variance)
```


:::

::: {.cell-output .cell-output-stdout}

```
✗ Removing either variable worsens model performance
```


:::

::: {.cell-output .cell-output-stdout}

```

Both elevation and latitude are important predictors of ant species diversity!
```


:::
:::


# **Lecture 10:** Predictors

Usually want to know relative importance of predictors to explaining Y

-   Three general approaches:
-   Using F-tests (or t-tests) on partial regression slopes
-   Using coefficient of partial determination
-   Using standardized partial regression slopes

# **Lecture 10:** Predictors

Using F-tests (or t-tests) on partial regression slopes:

-   Conduct F tests of Ho that each partial regression slope = 0
-   If cannot reject Ho, discard predictor
-   Can get additional clues from relative size of F-values
-   Does not tell us absolute importance of predictor (usually can not
    directly compare slope parameters)

# **Lecture 10:** Predictors

Using coefficient of partial determination:

-   the reduction in variation of Y due to addition of predictor (Xj)

$$r_{X_j}^2 = \frac{SS_{\text{Extra}}}{\text{Reduced }SS_{\text{Residual}}}$$

SSextra

-   Increased in SSregression when Xj is added to model
-   Reduced SSresidual is the unexplained SS from model without Xj

# **Lecture 10:** Predictors

### Using standardized partial regression slopes:

-   predictors of predictor variables can not be directly compared
-   Why?
    -   Standardize all vars (mean = 0, sd= 1)

    -   Scales are identical and larger PRS mean more important variable

# **Lecture 10:** Predictors

:::: columns
::: {.column width="60%"}
-   Using partial r^2^ values
    -   **pEta_sqr (Partial Eta Squared):**

        -   **Elevation: 0.3189 (31.9%)** - Elevation explains 31.9% of
            the variance *after controlling for latitude*

        -   **Latitude: 0.3564 (35.6%)** - Latitude explains 35.6% of
            the variance *after controlling for elevation*

        -   Both are important predictors! Latitude is slightly stronger
-   dR_sqr (Delta R Squared / Semi-partial R²):
    -   Elevation: 0.2087 (20.9%)

        -   If you removed elevation from the model, R² would drop by
            20.9%

    -   Latitude: 0.2468 (24.7%)

    -   If you removed latitude from the model, R² would drop by 24.7%
-   Shows the unique contribution of each predictor to the total
    variance explained
:::


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

== Method 2: Type II ANOVA (car package) ==
```


:::

::: {.cell-output .cell-output-stdout}

```
Anova Table (Type II tests)

Response: ant_spp
           Sum Sq Df F value   Pr(>F)   
elevation  81.224  1  8.8955 0.007652 **
latitude   96.085  1 10.5231 0.004272 **
Residuals 173.487 19                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

::: {.cell-output .cell-output-stdout}

```
== Coefficients Table ==
```


:::

::: {.cell-output .cell-output-stdout}

```
 Coefficients    SSR df pEta_sqr dR_sqr
    elevation  81.22  1   0.3189 0.2087
     latitude  96.09  1   0.3564 0.2468
    Residuals 173.49 19   0.5000 0.4457
```


:::

::: {.cell-output .cell-output-stdout}

```

== Summary Statistics ==
```


:::

::: {.cell-output .cell-output-stdout}

```
Sum of squared errors (SSE): 173.5
```


:::

::: {.cell-output .cell-output-stdout}

```
Sum of squared total (SST): 389.3
```


:::

::: {.cell-output .cell-output-stdout}

```
Model R²: 0.5543
```


:::
:::

::::
