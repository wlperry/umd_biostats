---
title: "Lecture 14 - Generalized Linear Models"
author: "Bill Perry"
metadata-files:
  - ../../_templates/activities.yml
format:
  html:
    output-file: "14_02_class_activity.html"
  docx:
    output-file: "14_02_class_activity.docx"
---



# Lecture 14: Generalized Linear Models Activity

### Generalized Linear Models (GLMs) extend linear models to handle different types of response variables:

-   **Normal distribution**: Continuous data (like regular
    ANOVA/regression)
-   **Poisson distribution**: Count data
-   **Binomial distribution**: Binary data (presence/absence,
    success/failure)
-   **Gamma distribution**: Positive continuous data
-   **Negative binomial**: Overdispersed count data

## The Three Components of GLMs

1.  **Random component**: The response variable and its probability
    distribution
2.  **Systematic component**: The predictor variables (continuous or
    categorical)
3.  **Link function**: Connects expected value of Y to predictor
    variables

# How to approach the problem

-   **What is the question?**
    -   unclear data mining can lead to lost time
-   **Data Variable type**: what does the data look like - types of
    variable read in
-   **Data Completeness**: Is there a lot of sparcity
-   **Data Structure**: what does the data look like graphically
-   **Model Choice:** what is the right model to analyze the data to
    answer your question
-   **Model Run:** run model - summary
-   **Model Assumptions**: test early before you get excited and bend
    the rules
-   **Model Statistics** : run the final stats
-   Model Followup tests: post F pairwise comparisons or others
-   **Graphical display of results:** highlighting the data and
    statistics

# Part 1: Gaussian GLM (equivalent to normal ANOVA)

The simplest form of GLM uses a normal (Gaussian) distribution with an
identity link function. This is equivalent to standard ANOVA

Let's compare a standard linear model and a Gaussian GLM

#### Island Biogeography Data

The `gala` dataframe from the `faraway` package contains data on 30
Galapagos islands, testing MacArthur-Wilson's theory of island
biogeography.

-   **Variables in the dataframe:**
    -   `Species`Number of plant species (count data)
    -   `Endemics`Number of endemic species (count data)
    -   `Area`Island area (km²)
    -   `Elevation -`Maximum elevation (m)
    -   `Nearest` - Distance to nearest island (km)
    -   `Scruz` - Distance to Santa Cruz island (km)
    -   `Adjacent` - Area of adjacent island (km²)

### The data - variable types


::: {.cell}

```{.r .cell-code}
# Create a categorical variable for demonstration
gala <- gala %>%
  mutate(size_category = case_when(
    Area < 1 ~ "Small",
    Area >= 1 & Area < 100 ~ "Medium",
    Area >= 100 ~ "Large"
  ),
  size_category = factor(size_category, levels = c("Small", "Medium", "Large")))

head(gala %>% dplyr::select(-Scruz, -Adjacent), 10)
```

::: {.cell-output .cell-output-stdout}

```
             Species Endemics  Area Elevation Nearest size_category
Baltra            58       23 25.09       346     0.6        Medium
Bartolome         31       21  1.24       109     0.6        Medium
Caldwell           3        3  0.21       114     2.8         Small
Champion          25        9  0.10        46     1.9         Small
Coamano            2        1  0.05        77     1.9         Small
Daphne.Major      18       11  0.34       119     8.0         Small
Daphne.Minor      24        0  0.08        93     6.0         Small
Darwin            10        7  2.33       168    34.1        Medium
Eden               8        4  0.03        71     0.4         Small
Enderby            2        2  0.18       112     2.6         Small
```


:::
:::


# Data completeness


::: {.cell}

```{.r .cell-code}
gala %>% skim()
```

::: {.cell-output-display}

Table: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |Piped data |
|Number of rows           |30         |
|Number of columns        |8          |
|_______________________  |           |
|Column type frequency:   |           |
|factor                   |1          |
|numeric                  |7          |
|________________________ |           |
|Group variables          |None       |


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts               |
|:-------------|---------:|-------------:|:-------|--------:|:------------------------|
|size_category |         0|             1|FALSE   |        3|Med: 12, Sma: 11, Lar: 7 |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|   mean|     sd|    p0|   p25|    p50|    p75|    p100|hist  |
|:-------------|---------:|-------------:|------:|------:|-----:|-----:|------:|------:|-------:|:-----|
|Species       |         0|             1|  85.23| 114.63|  2.00| 13.00|  42.00|  96.00|  444.00|▇▂▁▁▁ |
|Endemics      |         0|             1|  26.10|  27.33|  0.00|  7.25|  18.00|  32.25|   95.00|▇▅▁▁▂ |
|Area          |         0|             1| 261.71| 864.11|  0.01|  0.26|   2.59|  59.24| 4669.32|▇▁▁▁▁ |
|Elevation     |         0|             1| 368.03| 421.60| 25.00| 97.75| 192.00| 435.25| 1707.00|▇▁▂▁▁ |
|Nearest       |         0|             1|  10.06|  14.27|  0.20|  0.80|   3.05|  10.02|   47.40|▇▁▁▂▁ |
|Scruz         |         0|             1|  56.98|  68.03|  0.00| 11.02|  46.65|  81.08|  290.20|▇▃▁▁▁ |
|Adjacent      |         0|             1| 261.10| 864.52|  0.03|  0.52|   2.59|  59.24| 4669.32|▇▁▁▁▁ |


:::
:::


# Look at structure of the data graphically:


::: {.cell}

```{.r .cell-code}
#| message: false
#| warning: false
#| paged-print: false

ggplot(gala, aes(x = Species)) +
  geom_histogram(binwidth = 25, fill = "darkblue", color = "black") +
  labs(title = "Distribution of Species Richness",
       subtitle = "Galapagos Islands",
       x = "Number of Plant Species",
       y = "Number of Islands") +
  theme_minimal()
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/summary-gaussian_2-1.jpeg)
:::
:::


# Look at data by size category


::: {.cell}

```{.r .cell-code}
ggplot(gala, aes(x = size_category, y = Species, fill = size_category)) +
  geom_boxplot(color = "darkblue") +
  labs(title = "Distribution of Species Richness",
       subtitle = "Galapagos Islands",
       x = "Number of Plant Species",
       y = "Number of Islands") +
  theme_minimal()
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-2-1.jpeg)
:::
:::


# GLM with Gaussian (Normal) Distribution: Setup

The simplest form of GLM uses a normal (Gaussian) distribution with an
identity link function. This is equivalent to standard linear model

Let's compare a standard linear model and a Gaussian GLM using the
Galapagos dataset, modeling endemic species richness by island size
category.

# The linear model summary


::: {.cell}

```{.r .cell-code}
# Fit a standard linear model
model_lm <- lm(Endemics ~ size_category, data = gala)
summary(model_lm)
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = Endemics ~ size_category, data = gala)

Residuals:
    Min      1Q  Median      3Q     Max 
-42.857  -4.386  -0.762   6.940  29.143 

Coefficients:
                    Estimate Std. Error t value      Pr(>|t|)    
(Intercept)            5.636      4.402    1.28        0.2113    
size_categoryMedium   16.030      6.095    2.63        0.0139 *  
size_categoryLarge    60.221      7.059    8.53 0.00000000382 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 14.6 on 27 degrees of freedom
Multiple R-squared:  0.7343,	Adjusted R-squared:  0.7146 
F-statistic: 37.31 on 2 and 27 DF,  p-value: 0.00000001697
```


:::
:::


# The ANOVA model


::: {.cell}

```{.r .cell-code}
Anova(model_lm, type = 3 )
```

::: {.cell-output .cell-output-stdout}

```
Anova Table (Type III tests)

Response: Endemics
               Sum Sq Df F value        Pr(>F)    
(Intercept)     349.5  1  1.6392        0.2113    
size_category 15906.6  2 37.3066 0.00000001697 ***
Residuals      5756.1 27                          
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


# The Gaussian GLM model


::: {.cell}

```{.r .cell-code}
# Fit a Gaussian GLM
model_gaussian <- glm(Endemics ~ size_category,  data = gala, 
                       family = gaussian(link = "identity"))

summary(model_gaussian)
```

::: {.cell-output .cell-output-stdout}

```

Call:
glm(formula = Endemics ~ size_category, family = gaussian(link = "identity"), 
    data = gala)

Coefficients:
                    Estimate Std. Error t value      Pr(>|t|)    
(Intercept)            5.636      4.402    1.28        0.2113    
size_categoryMedium   16.030      6.095    2.63        0.0139 *  
size_categoryLarge    60.221      7.059    8.53 0.00000000382 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for gaussian family taken to be 213.1878)

    Null deviance: 21662.7  on 29  degrees of freedom
Residual deviance:  5756.1  on 27  degrees of freedom
AIC: 250.84

Number of Fisher Scoring iterations: 2
```


:::
:::


# GLM ANOVA


::: {.cell}

```{.r .cell-code}
Anova(model_gaussian, type = "III", test = "F")
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table (Type III tests)

Response: Endemics
Error estimate based on Pearson residuals 

               Sum Sq Df F values        Pr(>F)    
size_category 15906.6  2   37.307 0.00000001697 ***
Residuals      5756.1 27                           
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


# Assumption Tests of Both Models


::: {.cell}

```{.r .cell-code}
# Create diagnostic plots
par(mfrow = c(2, 2))
plot(model_lm)
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-7-1.jpeg)
:::

```{.r .cell-code}
par(mfrow = c(1, 1))
```
:::



::: {.cell}

```{.r .cell-code}
# Create diagnostic plots
# par(mfrow = c(2, 2))
plot(model_gaussian)
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-8-1.jpeg)
:::

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-8-2.jpeg)
:::

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-8-3.jpeg)
:::

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-8-4.jpeg)
:::

```{.r .cell-code}
# par(mfrow = c(1, 1))
```
:::


### Shapiro Wilk Test Linear Model


::: {.cell}

```{.r .cell-code}
shapiro.test(residuals(model_lm))
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(model_lm)
W = 0.92782, p-value = 0.04298
```


:::
:::


### Shapiro Test Gaussian Model


::: {.cell}

```{.r .cell-code}
shapiro.test(residuals(model_gaussian))
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(model_gaussian)
W = 0.92782, p-value = 0.04298
```


:::
:::


## Levenes Test


::: {.cell}

```{.r .cell-code}
leveneTest(Endemics ~ size_category,  data = gala)
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value   Pr(>F)   
group  2  6.8514 0.003922 **
      27                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


# Emmeans Linear Model


::: {.cell}

```{.r .cell-code}
# Calculate estimated marginal means
lm_emmeans <- emmeans(model_lm, ~ size_category)
lm_emmeans
```

::: {.cell-output .cell-output-stdout}

```
 size_category emmean   SE df lower.CL upper.CL
 Small           5.64 4.40 27     -3.4     14.7
 Medium         21.67 4.21 27     13.0     30.3
 Large          65.86 5.52 27     54.5     77.2

Confidence level used: 0.95 
```


:::
:::



::: {.cell}

:::


# GLM with Poisson Distribution: regression

-   **Poisson GLMs** Poisson model used when response variable is
    **count data**:
    -   Number of species on an island
    -   Number of parasites in a host
    -   Number of bird nests in a plot
    -   Number of seeds produced by a plant
-   The Poisson distribution assumes:
    -   Counts are non-negative integers (0, 1, 2, 3, ...)
    -   The mean equals the variance
    -   Events occur independently
-   **Key consideration:** If variance \> mean (overdispersion),
    consider negative binomial regression instead.
-   Now let's fit a Poisson GLM to model the relationship between the
    rounded quarter-mile time and the number of cylinders:

## Fit Poisson GLM with size_category as predictor


::: {.cell}

```{.r .cell-code}
model_poisson_gala <- glm(Species ~ size_category, 
                          data = gala,
                          family = poisson(link = "log"))
summary(model_poisson_gala)
```

::: {.cell-output .cell-output-stdout}

```

Call:
glm(formula = Species ~ size_category, family = poisson(link = "log"), 
    data = gala)

Coefficients:
                    Estimate Std. Error z value Pr(>|z|)    
(Intercept)          2.67101    0.07930   33.68   <2e-16 ***
size_categoryMedium  1.33784    0.08833   15.15   <2e-16 ***
size_categoryLarge   2.84300    0.08285   34.31   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 3510.73  on 29  degrees of freedom
Residual deviance:  939.74  on 27  degrees of freedom
AIC: 1106.6

Number of Fisher Scoring iterations: 5
```


:::
:::


# GLM with Poisson Distribution: Setup

Does island size category, as a whole, have a statistically significant
effect on the number of plant species?

-   `test = "LR"`: important part!
    -   normal ANOVA (with a Gaussian/normal distribution) test is an
        F-test.
    -   GLM (like Poisson) can't use F-test in the same way
        -   use a Likelihood Ratio (LR) test
        -   LR test statistically compares fit of full model (the one
            with size_category) to simpler null model (one without
            size_category)
        -   LR test tells us if it is significant


::: {.cell}

```{.r .cell-code}
Anova(model_poisson_gala, type = "III", test = "LR")
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table (Type III tests)

Response: Species
              LR Chisq Df Pr(>Chisq)    
size_category     2571  2  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


# Let's check for overdispersion, which is common in count data:

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
    -   a dispersion parameter this large is a warning
    -   our data more variable than a Poisson model expects
    -   use a Negative Binomial model


::: {.cell}

```{.r .cell-code}
# Calculate the dispersion parameter
# (Pearson's Chi-Squared statistic / residual degrees of freedom)
dispersion_gala <- sum(residuals(model_poisson_gala, type = "pearson")^2) / 
                   model_poisson_gala$df.residual

# Print dispersion parameter
cat("Dispersion parameter:", round(dispersion_gala, 2), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Dispersion parameter: 32.9 
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Just pass your model to the function
performance::check_overdispersion(model_poisson_gala)
```

::: {.cell-output .cell-output-stdout}

```
# Overdispersion test

       dispersion ratio =  32.900
  Pearson's Chi-Squared = 888.308
                p-value = < 0.001
```


:::

::: {.cell-output .cell-output-stderr}

```
Overdispersion detected.
```


:::
:::



::: {.cell}

```{.r .cell-code}
# 1. Simulate residuals
# (This is the standard first step for all DHARMa diagnostics)
sim_res <- simulateResiduals(fittedModel = model_poisson_gala, n = 1000)

# 2. Test for dispersion
# This will provide a p-value and the ratio
testDispersion(sim_res)
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-17-1.jpeg)
:::

::: {.cell-output .cell-output-stdout}

```

	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
	simulated

data:  simulationOutput
dispersion = 53.462, p-value < 2.2e-16
alternative hypothesis: two.sided
```


:::

```{.r .cell-code}
# Plot diagnostic plots
plot(sim_res)
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-17-2.jpeg)
:::
:::


# Emmeans


::: {.cell}

```{.r .cell-code}
# 1. Calculate Estimated Marginal Means (EMMs)
# type = "response" converts the log-means back to the "Species count" scale
emm_gala <- emmeans(model_poisson_gala, 
                    specs = ~ size_category,
                    type = "response")
print(emm_gala)
```

::: {.cell-output .cell-output-stdout}

```
 size_category  rate   SE  df asymp.LCL asymp.UCL
 Small          14.5 1.15 Inf      12.4      16.9
 Medium         55.1 2.14 Inf      51.0      59.4
 Large         248.1 5.95 Inf     236.7     260.1

Confidence level used: 0.95 
Intervals are back-transformed from the log scale 
```


:::
:::



::: {.cell}

```{.r .cell-code}
# 2. Pairwise Comparisons
# Compares all pairs: Small-Medium, Small-Large, Medium-Large
pairs_gala <- pairs(emm_gala, adjust = "tukey")
print(pairs_gala)
```

::: {.cell-output .cell-output-stdout}

```
 contrast        ratio      SE  df null z.ratio p.value
 Small / Medium 0.2624 0.02320 Inf    1 -15.146  <.0001
 Small / Large  0.0583 0.00483 Inf    1 -34.313  <.0001
 Medium / Large 0.2220 0.01010 Inf    1 -32.935  <.0001

P value adjustment: tukey method for comparing a family of 3 estimates 
Tests are performed on the log scale 
```


:::

```{.r .cell-code}
# 3. Compact Letter Display (CLD)
# The easiest way to see the groupings
cld_gala <- multcomp::cld(emm_gala, 
                          Letters = letters,  
                          alpha = 0.05)
print(cld_gala)
```

::: {.cell-output .cell-output-stdout}

```
 size_category  rate   SE  df asymp.LCL asymp.UCL .group
 Small          14.5 1.15 Inf      12.4      16.9  a    
 Medium         55.1 2.14 Inf      51.0      59.4   b   
 Large         248.1 5.95 Inf     236.7     260.1    c  

Confidence level used: 0.95 
Intervals are back-transformed from the log scale 
P value adjustment: tukey method for comparing a family of 3 estimates 
Tests are performed on the log scale 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::



::: {.cell}

```{.r .cell-code}
#| paged-print: false


# 1. Get the estimated means and CIs into a dataframe
emm_gala_df <- as.data.frame(emm_gala)

# 2. Create visualization
ggplot() +
  # Plot raw data (jittered so we can see the points)
  geom_jitter(data = gala,
              aes(x = size_category, y = Species),
              width = 0.2, # Spreads points horizontally
              alpha = 0.5) +
  # Add estimated means (points)
  geom_point(data = emm_gala_df, 
             aes(x = size_category, y = rate), # 'rate' is the mean
             size = 4, color = "blue") +
  # Add confidence intervals (error bars)
  geom_errorbar(data = emm_gala_df, 
                aes(x = size_category, 
                    ymin = asymp.LCL, # Lower Confidence Limit
                    ymax = asymp.UCL), # Upper Confidence Limit
                width = 0.2, color = "blue", linewidth = 1) +
  labs(title = "Species Richness by Island Size Category",
       subtitle = "Poisson GLM predictions (on the response scale)",
       x = "Island Size Category",
       y = "Number of Plant Species") +
  theme_minimal()
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/poisson-plot-1.jpeg)
:::
:::


# Negative Binomial GLM

-   Dealing with Overdispersion in Count Data
    -   count data shows more variability than expected under a Poisson
        distribution (variance \> mean
    -   need to use negative binomial model
    -   `model_nb <- glm.nb(qsec_round ~ cyl, data = mtcars_count)`
-   negative binomial model includes a dispersion parameter (theta)
-   allows the variance to be larger than the mean
-   standard errors bigger because NB model accounts for high
    variability (overdispersion)
-   estimates dispersion parameter 'Theta' (or 1/theta)
-   how it models the overdispersion.


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

Call:
glm.nb(formula = Species ~ size_category, data = gala, init.theta = 1.709503171, 
    link = log)

Coefficients:
                    Estimate Std. Error z value Pr(>|z|)    
(Intercept)           2.6710     0.2439  10.953  < 2e-16 ***
size_categoryMedium   1.3378     0.3313   4.039 5.37e-05 ***
size_categoryLarge    2.8430     0.3790   7.502 6.28e-14 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for Negative Binomial(1.7095) family taken to be 1)

    Null deviance: 90.168  on 29  degrees of freedom
Residual deviance: 32.932  on 27  degrees of freedom
AIC: 297.35

Number of Fisher Scoring iterations: 1

              Theta:  1.710 
          Std. Err.:  0.449 

 2 x log-likelihood:  -289.348 
```


:::
:::


## Assumptions


::: {.cell}

```{.r .cell-code}
# 1. Simulate residuals
sim_res_nb <- simulateResiduals(fittedModel = model_nb_gala)

# 2. Plot the diagnostics
plot(sim_res_nb)
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-18-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# This will test if there is *still* significant overdispersion
check_overdispersion(model_nb_gala)
```

::: {.cell-output .cell-output-stdout}

```
# Overdispersion test

 dispersion ratio = 0.487
          p-value = 0.416
```


:::

::: {.cell-output .cell-output-stderr}

```
No overdispersion detected.
```


:::
:::


# ANOVA GLM Negative Binmial


::: {.cell}

```{.r .cell-code}
# Get the overall Anova (Type III Likelihood Ratio test)
anova_nb <- Anova(model_nb_gala, type = "III", test = "LR")

print(anova_nb)
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table (Type III tests)

Response: Species
              LR Chisq Df Pr(>Chisq)    
size_category   57.237  2  3.726e-13 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::



::: {.cell}

```{.r .cell-code}
# 1. Calculate the Estimated Marginal Means on the "response" scale
emmeans_nb <- emmeans(model_nb_gala, spec = ~ size_category, type = "response")

print(emmeans_nb)
```

::: {.cell-output .cell-output-stdout}

```
 size_category response    SE  df asymp.LCL asymp.UCL
 Small             14.5  3.52 Inf      8.96      23.3
 Medium            55.1 12.30 Inf     35.50      85.5
 Large            248.1 72.00 Inf    140.54     438.1

Confidence level used: 0.95 
Intervals are back-transformed from the log scale 
```


:::
:::


### Pairwise


::: {.cell}

```{.r .cell-code}
# 2. Run pairwise comparisons on those means
pairs_nb <- pairs(emmeans_nb)

print(pairs_nb)
```

::: {.cell-output .cell-output-stdout}

```
 contrast        ratio     SE  df null z.ratio p.value
 Small / Medium 0.2624 0.0869 Inf    1  -4.039  0.0002
 Small / Large  0.0583 0.0221 Inf    1  -7.502  <.0001
 Medium / Large 0.2220 0.0814 Inf    1  -4.106  0.0001

P value adjustment: tukey method for comparing a family of 3 estimates 
Tests are performed on the log scale 
```


:::
:::



::: {.cell}

```{.r .cell-code}
# 3. Get the Compact Letter Display (CLD)
cld_nb <- multcomp::cld(emmeans_nb, Letters = letters)

print(cld_nb)
```

::: {.cell-output .cell-output-stdout}

```
 size_category response    SE  df asymp.LCL asymp.UCL .group
 Small             14.5  3.52 Inf      8.96      23.3  a    
 Medium            55.1 12.30 Inf     35.50      85.5   b   
 Large            248.1 72.00 Inf    140.54     438.1    c  

Confidence level used: 0.95 
Intervals are back-transformed from the log scale 
P value adjustment: tukey method for comparing a family of 3 estimates 
Tests are performed on the log scale 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::


# Logistic Regression

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

# Example: Lizard Presence on Islands

Based on the example from Polis et al. (1998), we'll model the
presence/absence of lizards (*Uta*) on islands in the Gulf of California
based on perimeter/area ratio.


::: {.cell}

```{.r .cell-code}
set.seed(123)
island_data <- data.frame(
  island_id = 1:30,
  pa_ratio = seq(5, 70, length.out = 30),
  uta_present = c(rep(1, 10), 
                  rbinom(10, 1, prob = 0.5),  # Mixed outcomes in middle
                  rep(0, 10)))%>%
  mutate(uta_present_factor = factor(uta_present, levels = c(0, 1), 
         labels = c("Absent", "Present")))
```
:::



::: {.cell}

```{.r .cell-code}
ggplot() +
  # Add jittered points for observed data
  geom_point(data = island_data, 
              aes(x = pa_ratio, y = uta_present),
              position = position_dodge2(width=.1), alpha = 0.7) +
  labs(title = "Probability of Uta Presence vs. Perimeter/Area Ratio",
       x = "Perimeter/Area Ratio",
       y = "Probability of Presence") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/lizard_plot-1.jpeg)
:::
:::


# Example: Lizard Presence on Islands

Based on the example from Polis et al. (1998), we'll model the
presence/absence of lizards (*Uta*) on islands in the Gulf of California
based on perimeter/area ratio.


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


# Lizard Example: Visualization and Testing

Let's visualize the data and the fitted model:


::: {.cell}

```{.r .cell-code}
#| paged-print: false
# Create a dataframe for predictions
pred_data <- data.frame(
  pa_ratio = seq(min(island_data$pa_ratio), 
                max(island_data$pa_ratio), 
                length.out = 100)
)

# Get predicted probabilities
pred_data$prob <- predict(lizard_model, 
                         newdata = pred_data, 
                         type = "response")

# Plot
ggplot() +
  # Add jittered points for observed data
  geom_point(data = island_data, 
              aes(x = pa_ratio, y = uta_present),
              position = position_dodge2(width=.1), alpha = 0.7) +
  # Add predicted probability curve
  geom_line(data = pred_data, 
            aes(x = pa_ratio, y = prob), 
            color = "blue", size = 1) +
  # Add confidence intervals (optional)
  labs(title = "Probability of Uta Presence vs. Perimeter/Area Ratio",
       x = "Perimeter/Area Ratio",
       y = "Probability of Presence") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()
```

::: {.cell-output .cell-output-stderr}

```
Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
ℹ Please use `linewidth` instead.
```


:::

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/lizard-plot-1.jpeg)
:::
:::


We want to test the null hypothesis that β₁ = 0, meaning there's no
relationship between P/A ratio and lizard presence.

There are two common ways to test this hypothesis:

1.  **Wald test**: Tests if the parameter estimate divided by its
    standard error differs significantly from zero

2.  **Likelihood ratio test**: Compares the fit of the full model to a
    reduced model without the predictor variable


::: {.cell}

```{.r .cell-code}
reduced_model <- glm(uta_present ~ 1, 
                     data = island_data, 
                     family = binomial(link = "logit"))
anova(reduced_model, lizard_model, test = "Chisq")
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Deviance Table

Model 1: uta_present ~ 1
Model 2: uta_present ~ pa_ratio
  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1        29     41.455                          
2        28     19.090  1   22.365 2.254e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


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
-   For every one-unit increase in island's Perimeter/Area Ratio - odds
    of finding a lizard present multiplied by 0.898
-   the odds decrease by 10.2% (which is 1 - 0.898) for every one-unit
    increase in the P/A ratio
-   entire interval is below 1.0, you can be confident the relationship
    is negative: more P/A ratio means lower odds of lizards


::: {.cell}

```{.r .cell-code}
coef_lizard <- coef(lizard_model)[2]  # Extract slope coefficient
odds_ratio <- exp(coef_lizard)
ci <- exp(confint(lizard_model, "pa_ratio"))
```

::: {.cell-output .cell-output-stderr}

```
Waiting for profiling to be done...
```


:::

```{.r .cell-code}
cat("Odds Ratio:", round(odds_ratio, 3), "\n\n",
"95% CI:", round(ci[1], 3), "to", round(ci[2], 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Odds Ratio: 0.861 

 95% CI: 0.753 to 0.932 
```


:::
:::



::: {.cell}

```{.r .cell-code}
library(parameters)

# This function is specifically for model parameters
model_parameters(lizard_model, exponentiate = TRUE)
```

::: {.cell-output .cell-output-stdout}

```
Parameter   | Odds Ratio |     SE |            95% CI |     z |     p
---------------------------------------------------------------------
(Intercept) |     378.94 | 807.02 | [14.47, 94371.40] |  2.79 | 0.005
pa ratio    |       0.86 |   0.04 | [ 0.75,     0.93] | -2.89 | 0.004
```


:::

::: {.cell-output .cell-output-stderr}

```

Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
  computed using a Wald z-distribution approximation.
```


:::
:::


This gives you the odds ratio (in the estimate column) and the
exponentiated CIs (conf.low, conf.high) for all terms in your model, all
in one clean table.


::: {.cell}

```{.r .cell-code}
library(broom)

# This one function does everything
tidy(lizard_model, exponentiate = TRUE, conf.int = TRUE)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 7
  term        estimate std.error statistic p.value conf.low conf.high
  <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
1 (Intercept)  379.       2.13        2.79 0.00530   14.5   94371.   
2 pa_ratio       0.861    0.0517     -2.89 0.00388    0.753     0.932
```


:::
:::


# Assessing Model Fit

There are several ways to assess the goodness-of-fit for logistic
regression models:


::: {.cell}

```{.r .cell-code}
# This one function gives McFadden's and other popular R² values
performance::r2(lizard_model)
```

::: {.cell-output .cell-output-stdout}

```
# R2 for Logistic Regression
  Tjur's R2: 0.588
```


:::

```{.r .cell-code}
pscl::pR2(lizard_model)
```

::: {.cell-output .cell-output-stdout}

```
fitting null model for pseudo-r2
```


:::

::: {.cell-output .cell-output-stdout}

```
        llh     llhNull          G2    McFadden        r2ML        r2CU 
 -9.5450726 -20.7276993  22.3652534   0.5395016   0.5255070   0.7017187 
```


:::

```{.r .cell-code}
# 'g' is the number of groups to test (e.g., 10 for deciles)
# Note: You need to provide the observed 'y' values
hoslem.test(lizard_model$y, fitted(lizard_model), g = 10)
```

::: {.cell-output .cell-output-stdout}

```

	Hosmer and Lemeshow goodness of fit (GOF) test

data:  lizard_model$y, fitted(lizard_model)
X-squared = 2.4032, df = 8, p-value = 0.9661
```


:::
:::


Logistic regression has different and generally fewer assumptions to
test than standard linear regression.

-   The main "assumptions" for logistic regression are:

    -   Binary Outcome: The dependent variable must be binary (0/1) or
        proportional (e.g., number of successes / number of trials).
        Your uta_present is 0/1, so this is met.

    -   Independence of Observations: Each observation (each island)
        must be independent. This is a study design assumption.

    -   Linearity of the Logit: This is the most important one to test.
        It assumes a linear relationship between any continuous
        predictors and the log-odds (logit) of the outcome.

    -   No (or little) Multicollinearity: If you have multiple
        predictors, they shouldn't be highly correlated with each other.

1.  Linearity of the Logit (The Most Important Check) continuous
    predictor (pa_ratio) has a linear relationship with the log-odds of
    the outcome looking for a flat, non-curved line.


::: {.cell}

```{.r .cell-code}
# This runs multiple checks, but the "Linearity" one is key
check_model(lizard_model, residual_type = "normal")
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-27-1.jpeg)
:::
:::


DHARMa is excellent for GLMs. It simulates residuals and plots them
against predictors. This is a very robust way to check for all kinds of
model misfit, including non-linearity. plotResiduals function will show
three quantile regression lines. You want all three (the solid red one
and two dashed ones) to be flat and near 0.5. If they are sloped or
curved, it indicates a pattern that your model missed (i.e.,
non-linearity).


::: {.cell}

```{.r .cell-code}
# 1. Simulate the residuals
sim_res <- simulateResiduals(fittedModel = lizard_model)

# 2. Plot residuals against the predictor
# DHARMa has a specific function for this
plotResiduals(sim_res, lizard_model$model$pa_ratio, 
              xlab = "Perimeter/Area Ratio", 
              main = "DHARMa Residuals vs. Predictor")
```

::: {.cell-output-display}
![](14_02_class_activity_files/figure-docx/unnamed-chunk-28-1.jpeg)
:::
:::


2.  Multicollinearity only with 2 or more predictors

if you did have more predictors (e.g., pa_ratio and island_area), you
would test it like this:

3.  Overall Model Fit (Goodness-of-Fit) This isn't an "assumption" so
    much as a check that the model as a whole is adequate. You already
    have the two main tests in your file!

Hosmer-Lemeshow Test (from your code) Interpretation: For this test, a
GOOD model has a non-significant p-value (p \> 0.05). This means your
model's predicted probabilities are not significantly different from the
observed probabilities in the data, which is what you want.


::: {.cell}

```{.r .cell-code}
# You need the pscl library
# library(pscl) 

# Note: g = 10 (deciles) is a common choice
hoslem.test(lizard_model$y, fitted(lizard_model), g = 10)
```

::: {.cell-output .cell-output-stdout}

```

	Hosmer and Lemeshow goodness of fit (GOF) test

data:  lizard_model$y, fitted(lizard_model)
X-squared = 2.4032, df = 8, p-value = 0.9661
```


:::
:::

