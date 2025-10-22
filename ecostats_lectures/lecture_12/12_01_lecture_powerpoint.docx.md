---
title: "Lecture 12 - Factorial ANOVA of Penguin Mass Balanced"
author: "Bill Perry"

metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "12_02_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  revealjs:
    output-file: "12_01_factorial_anova_slides.html"
  docx:
    output-file: "12_01_lecture_powerpoint.docx"
  pptx:
    output-file: "12_01_lecture_powerpoint.pptx"
---

# Lecture 12: Review

::::: columns
::: {.column width="60%"}
### ANOVA

-   Analysis of variance: single and multi-factor designs
-   Examples: diatoms, circadian rhythms
-   Predictor variables: fixed vs. random
-   ANOVA model
-   Analysis and partitioning of variance
-   Null hypothesis
-   Assumptions and diagnostics
-   Post F Tests - Tukey and others
-   Reporting the results
:::

::: {.column width="40%"}
![](images/clipboard-1642768928.png){width="301"}
:::
:::::

# Lecture 12: Factorial ANOVA

::::: columns
::: {.column width="60%"}
### 2-factor designs (2-way ANOVA)

-   very common in ecology
-   can have more factors (e.g., 3-way ANOVA) but interpretation gets
    very challenging...
-   Most multifactor designs: are factorail or nested
-   We will cover a 2 Factor Anova - this could be fertilizer and
    sunlight for plant biomass produced...
    -   each can have an effect independently
    -   the effect of one factor may interact with the other factor
    -   in this example...
        -   a low light and high light treatment might affect biomass

        -   fertilizer also might affect plat biomass

        -   but when you have higher light the high fertilizer treatment
            may grow better than expected
:::

::: {.column width="40%"}
![](images/clipboard-3287272591.png){width="233"}

![](images/clipboard-327078330.png)
:::
:::::

# Factorial ANOVA: Design Structure

::::: columns
::: {.column width="60%"}
### Consider two factors:

-   Factorial/crossed:
    -   every level of B in every level of A
:::

::: {.column width="40%"}
![](images/clipboard-2737690912.png){width="256"}
:::
:::::

# Factorial ANOVA: Effect Types

::::: columns
::: {.column width="60%"}
### In factorial designs

-   look at two types of factor effects:
    -   **Main effect of each factor** (polling across other factor)
    -   **Interaction effects;** is there synergistic/ antagonistic
        effect of factors?
:::

::: {.column width="40%"}
![](images/clipboard-1252866044.png){width="432"}
:::
:::::

# Introduction to Two-Way ANOVA

::::: columns
::: {.column width="60%"}
## Factorial Designs

Two-way ANOVA examines:

-   Main effect of Factor A (Species)
-   Main effect of Factor B (Sex)
-   Interaction between A × B
-   Balanced design: equal sample sizes in all cells
-   Type III sums of squares for unbiased estimates - if balanced it
    does not matter

Statistical Model:
$$Y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \varepsilon_{ijk}$$

Where: - $\mu$ = grand mean - $\alpha_i$ = effect of species i -
$\beta_j$ = effect of sex j - $(\alpha\beta)_{ij}$ = interaction
effect - $\varepsilon_{ijk}$ = random error
:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
# Load required packages
library(palmerpenguins)
library(emmeans)
library(car)
library(broom)
library(tidyverse)
# Load the penguin data
p_df <- penguins %>%
  filter(!is.na(body_mass_g), 
         !is.na(sex),
         !is.na(species)) %>%
  select(species, sex, body_mass_g)
head(p_df)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 3
  species sex    body_mass_g
  <fct>   <fct>        <int>
1 Adelie  male          3750
2 Adelie  female        3800
3 Adelie  female        3250
4 Adelie  female        3450
5 Adelie  male          3650
6 Adelie  female        3625
```


:::
:::

:::
:::::

# Data Preparation - Creating Balanced Design

::::: columns
::: {.column width="60%"}
### Balancing the Penguin Data

The dataframe of penguins is unbalanced - meaning that there are unequal
samples per cell


::: {.cell}

```{.r .cell-code}
# Check original sample sizes
original_n <- p_df %>%
  count(species, sex) %>%
  arrange(species, sex)

p_df %>%
  count(species, sex) %>%
  pivot_wider(names_from = sex, 
              values_from = n,
              values_fill = 0) 
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 3
  species   female  male
  <fct>      <int> <int>
1 Adelie        73    73
2 Chinstrap     34    34
3 Gentoo        58    61
```


:::
:::

:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
# Find minimum sample size
min_n <- min(original_n$n)
print(paste("Minimum n =", min_n))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Minimum n = 34"
```


:::

```{.r .cell-code}
# Create balanced dataset
set.seed(123) # for reproducibility
balanced_df <- p_df %>%
  group_by(species, sex) %>%
  sample_n(min_n) %>%
  ungroup()

# Verify balanced design
balanced_n <- balanced_df %>% 
  count(species, sex) %>%
  pivot_wider(names_from = sex, 
              values_from = n,
              values_fill = 0) 
balanced_n
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 3
  species   female  male
  <fct>      <int> <int>
1 Adelie        34    34
2 Chinstrap     34    34
3 Gentoo        34    34
```


:::
:::

:::
:::::

# Descriptive Statistics

::::: columns
::: {.column width="60%"}
## Summary Statistics by Groups


::: {.cell}

```{.r .cell-code}
# Calculate group means and SDs
summary_df <- balanced_df %>%
  group_by(species, sex) %>%
  summarise(
    n = sum(!is.na(body_mass_g)),
    mean_mass = mean(body_mass_g),
    sd_mass = sd(body_mass_g),
    se_mass = sd_mass/sqrt(n),
    .groups = 'drop'
  ) %>%
  arrange(species, sex)

print(summary_df)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 6
  species   sex        n mean_mass sd_mass se_mass
  <fct>     <fct>  <int>     <dbl>   <dbl>   <dbl>
1 Adelie    female    34     3335.    260.    44.5
2 Adelie    male      34     4049.    318.    54.5
3 Chinstrap female    34     3527.    285.    48.9
4 Chinstrap male      34     3939.    362.    62.1
5 Gentoo    female    34     4681.    309.    53.0
6 Gentoo    male      34     5525     274.    47.0
```


:::
:::

:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
p_box_plot <- balanced_df %>% 
  ggplot(aes(species, body_mass_g, fill=sex))+
  geom_boxplot()
p_box_plot
```

::: {.cell-output-display}
![](12_01_lecture_powerpoint_files/figure-docx/box_plot-1.png)
:::
:::

:::
:::::

# Factorial ANOVA Models

Factorial designs can be of 3 types:

-   **Model 1** - 2 fixed factors - the focus for today....
-   Model 2 - 2 random
-   **Model 3** - 1 fixed, 1 random (mixed model) - often nested

Model 1 ANOVA:

### $$y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \varepsilon_{ijk}$$

# Model Components Explained

### $$y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \varepsilon_{ijk}$$

-   $y_{ijk}$: value of the k~th~ observation from jth and ith
    combination of B and A (fecundity on 2nd plate, in "8 per plate"
    density in summer)
-   µ: overall mean (overall fecundity)
-   αi: effect of the ith level of A, pooling across all levels of B:
    µi- µ (difference between average fecundity in all "8 per plate"
    treatments and overall mean)

# Interaction Effects

### $$y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \varepsilon_{ijk}$$

-   Βj: effect of jth level of B, pooling across all levels of A: µj- µ
    (difference between average fecundity in all winter treatments and
    overall mean)
-   (αβ)ij: effect of interaction of ith level of A and jth level of B
    (µij - µi - µj + µ).
    -   Does effect of B depend on level of A? (is effect of density
        different in winter and summer?)

# Model Types and Interpretation

### $$y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \varepsilon_{ijk}$$

-   Model 2 ANOVA rare in ecology
-   **Model 3 interpretation is different:**
    -   βj: random variable measuring variance in y across all possible
        levels of B, pooling across all levels of A
    -   (αβ)ij is random variable measuring variance of interaction
        between A and B across all possible levels of B ("is effect of A
        consistent across all possible levels of B that could have been
        chosen?")

# Estimated Marginal Means

::::: columns
::: {.column width="60%"}
-   Before we go further we need to define what estimated marginal means
    are
-   Balanced data - it is just the means of the groups... easy
-   Unbalanced data - it is the mean of cells that represent the lowest
    average of the groups


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "Combined table with means and counts:"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 5
  species   female_mean male_mean female_n male_n
  <fct>           <dbl>     <dbl>    <int>  <int>
1 Adelie          3369.     4043.       73     73
2 Chinstrap       3527.     3939.       34     34
3 Gentoo          4680.     5485.       58     61
```


:::
:::

:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "\nRegular means (WRONG for marginal means):"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 3
  species   total_n regular_mean
  <fct>       <int>        <dbl>
1 Adelie        146        3706.
2 Chinstrap      68        3733.
3 Gentoo        119        5092.
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "\nEstimated Marginal Means for Species (CORRECT):"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 3
  species   emm_mean calculation                   
  <fct>        <dbl> <chr>                         
1 Adelie       3706. (3368.8 + 4043.5) / 2 = 3706.2
2 Chinstrap    3733. (3527.2 + 3939) / 2 = 3733.1  
3 Gentoo       5082. (4679.7 + 5484.8) / 2 = 5082.3
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "\nComparison showing EMM calculation:"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 6
  species   cell_mean_female cell_mean_male species_emm regular_mean difference
  <fct>                <dbl>          <dbl>       <dbl>        <dbl>      <dbl>
1 Adelie               3369.          4043.       3706.        3706.   4.55e-13
2 Chinstrap            3527.          3939.       3733.        3733.   0       
3 Gentoo               4680.          5485.       5082.        5092.  -1.01e+ 1
```


:::
:::

:::
:::::

# Estimated Marginal Means

::::: columns
::: {.column width="60%"}
-   Before we go further we need to define what estimated marginal means
    are
-   Balanced data - it is just the means of the groups... easy
-   Unbalanced data - it is the mean of cells that represent the lowest
    average of the groups


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "Combined table with means and counts:"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 3 × 5
  species   female_mean male_mean female_n male_n
  <fct>           <dbl>     <dbl>    <int>  <int>
1 Adelie          3369.     4043.       73     73
2 Chinstrap       3527.     3939.       34     34
3 Gentoo          4680.     5485.       58     61
```


:::
:::

:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "\nRegular means sex (WRONG for marginal means):"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 3
  sex    total_n regular_mean
  <fct>    <int>        <dbl>
1 female     165        3862.
2 male       168        4546.
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "\nEstimated Marginal Means for Sex (CORRECT):"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 3
  sex    emm_mean calculation                            
  <fct>     <dbl> <chr>                                  
1 female    3859. (3368.8 + 3527.2 + 4679.7) / 3 = 3858.6
2 male      4489. (4043.5 + 3939 + 5484.8) / 3 = 4489.1  
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "\nComparison showing difference between regular and marginal means:"
```


:::

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  sex    total_n regular_mean emm_mean difference
  <fct>    <int>        <dbl>    <dbl>      <dbl>
1 female     165        3862.    3859.      -3.68
2 male       168        4546.    4489.     -56.6 
```


:::
:::

:::
:::::

# ANOVA Table Structure

-   SStotal = SSA + SSB + SSAB+ SSresidual = (Y - Grand Mean Y)\^2
-   SSresidual = (Y - Yhat)\^2 or the difference between each
    observation and the appropriate cell mean, summed over all
    observations

| Source | SS | df | MS |
|:-----------------|:-----------------|:-----------------|:-----------------|
| A | $nq \sum_{i=1}^{p} (\bar{y}_{i.} - \bar{y})^2$ | $p-1$ | $\frac{SS_A}{p-1}$ |
| B | $np \sum_{j=1}^{q} (\bar{y}_{.j} - \bar{y})^2$ | $q-1$ | $\frac{SS_B}{q-1}$ |
| AB | $n \sum_{i=1}^{p} \sum_{j=1}^{q} (\bar{y}_{ij} - \bar{y}_{i.} - \bar{y}_{.j} + \bar{y})^2$ | $(p-1)(q-1)$ | $\frac{SS_{AB}}{(p-1)(q-1)}$ |
| Residual | $\sum_{i=1}^{p} \sum_{j=1}^{q} \sum_{k=1}^{n} (y_{ijk} - \bar{y}_{ij})^2$ | $pq(n-1)$ | $\frac{SS_{\text{Residual}}}{pq(n-1)}$ |
| Total | $\sum_{i=1}^{p} \sum_{j=1}^{q} \sum_{k=1}^{n} (y_{ijk} - \bar{y})^2$ | $pqn-1$ |  |

# SSA: Factor A Effects

SSA is SS of differences between each marginal mean of A and overall
mean

If A is species then get the emmeans for factor A down and subtract from
overall mean

![](images/clipboard-4218970939.png){width="500"}

![](images/clipboard-4251414171.png){width="474"}

# SSB: Factor B Effects

SSB is SS of differences between each marginal mean of B and overall
mean

If B is sex then get the emmeans for factor B across and subtract from
overall mean

![](images/clipboard-4218970939.png){width="500"}

![](images/clipboard-4251414171.png){width="474"}

# SSAB: Interaction Effects

SSAB is SS of cell means minus marginal means plus overall mean

![](images/clipboard-3485990288.png){width="500"}

![](images/clipboard-4251414171.png){width="474"}

# F-ratio Calculations

SS converted to MS;

F-ratio calculations are different depending on whether factors are
fixed, random or mixed

| Source | A and B fixed | A and B random | A fixed, B random |
|:-----------------|:----------------:|:----------------:|:----------------:|
| A | $\frac{MS_A}{MS_{Residual}}$ | $\frac{MS_A}{MS_{AB}}$ | $\frac{MS_A}{MS_{AB}}$ |
| B | $\frac{MS_B}{MS_{Residual}}$ | $\frac{MS_B}{MS_{AB}}$ | $\frac{MS_B}{MS_{AB}}$ |
| AB | $\frac{MS_{AB}}{MS_{Residual}}$ | $\frac{MS_{AB}}{MS_{Residual}}$ | $\frac{MS_{AB}}{MS_{Residual}}$ |

# Hypotheses: Fixed Factors

3 hypotheses are tested in a two-way factorial ANOVA:

A, B, A\*B Both factors fixed:

-   Ho(A): µ1= µ2= µ3=…. µi= µp (no diff. in marginal means of A,
    pooling across all levels of B)
-   Ho(B): µ1= µ2= µ3=…. µj= µq (no diff. in marginal means of B,
    pooling across all levels of A)
-   Ho(AB): µij- µi - µj + µ = 0 (no effect of interaction)

# Hypotheses: Mixed Model

3 hypotheses are tested in a two-way factorial ANOVA: A, B, A\*B

One fixed, one random:

-   Ho(A): µ1= µ2= µ3=…. µi= µp (no diff. in marginal means of A,
    pooling across all levels of B)
-   Ho(B): σB2= 0 (no added variance due to levels of B that could have
    been used)
-   Ho(AB): σAB2= 0 (no added variance due to interaction between all
    levels of A and B that could have been used)

# Example Study Details

::::: columns
::: {.column width="60%"}
So lets try the example with the penguin data that is in the package
penguin

Effect of species and sex on body_mass_g

-   3 species (factor A)
-   2 sexes (factor B)
-   34 replicates in each cell
-   This analysis examines the effects of species and sex on the body
    mass of penguins.
:::

::: {.column width="40%"}
![](images/clipboard-1581875934.png){width="487"}
:::
:::::

# Two-Way ANOVA with Type III Sums of Squares

::::: columns
::: {.column width="60%"}
## Fitting the Model


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = body_mass_g ~ species * sex, data = balanced_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-827.21 -178.13    6.25  175.00  861.03 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    4175.86      21.23 196.727  < 2e-16 ***
species1       -484.31      30.02 -16.134  < 2e-16 ***
species2       -442.77      30.02 -14.750  < 2e-16 ***
sex1           -328.31      21.23 -15.467  < 2e-16 ***
species1:sex1   -28.68      30.02  -0.955    0.341    
species2:sex1   122.43      30.02   4.078 6.57e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 303.2 on 198 degrees of freedom
Multiple R-squared:  0.8596,	Adjusted R-squared:  0.856 
F-statistic: 242.4 on 5 and 198 DF,  p-value: < 2.2e-16
```


:::
:::

:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
# Type III ANOVA table using car::Anova
anova_type3 <- Anova(anova_model, type = "III")
print("Type III Sums of Squares ANOVA:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Type III Sums of Squares ANOVA:"
```


:::

```{.r .cell-code}
print(anova_type3)
```

::: {.cell-output .cell-output-stdout}

```
Anova Table (Type III tests)

Response: body_mass_g
                Sum Sq  Df    F value    Pr(>F)    
(Intercept) 3557308900   1 38701.5271 < 2.2e-16 ***
species       87725999   2   477.2049 < 2.2e-16 ***
sex           21988483   1   239.2224 < 2.2e-16 ***
species:sex    1672776   2     9.0994 0.0001657 ***
Residuals     18199467 198                         
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::

:::
:::::

# Understanding Type III SS

::::: columns
::: {.column width="60%"}
Type III sums of squares test each effect after adjusting for all other
effects in the model:

-   **Species effect**: Tested after adjusting for sex and interaction
-   **Sex effect**: Tested after adjusting for species and interaction
-   **Interaction**: Tested after adjusting for both main effects

This is especially important for unbalanced designs, but we're using it
here for consistency with the next analysis.
:::

::: {.column width="40%"}

::: {.cell}

```{.r .cell-code}
# Calculate effect sizes (eta-squared)
ss_total <- sum(anova_type3$`Sum Sq`[2:4]) + 
            anova_type3$`Sum Sq`[5]

eta_squared_df <- data.frame(
  Effect = c("Species", "Sex", "Interaction"),
  Eta_Squared = anova_type3$`Sum Sq`[2:4] / ss_total
)

print("Effect Sizes (Eta-squared):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Effect Sizes (Eta-squared):"
```


:::

```{.r .cell-code}
print(eta_squared_df)
```

::: {.cell-output .cell-output-stdout}

```
       Effect Eta_Squared
1     Species  0.67696748
2         Sex  0.16968160
3 Interaction  0.01290854
```


:::
:::

:::
:::::

# Checking Model Assumptions


::: {.cell}

```{.r .cell-code}
# Create diagnostic plots
par(mfrow = c(2, 2))

# 1. Residuals vs Fitted
residuals_plot <- plot(anova_model, which = 1,
                       main = "Residuals vs Fitted")

# 2. Q-Q plot
qq_plot <- plot(anova_model, which = 2,
               main = "Normal Q-Q")

# 3. Scale-Location
scale_plot <- plot(anova_model, which = 3,
                   main = "Scale-Location")

# 4. Residuals vs Leverage
leverage_plot <- plot(anova_model, which = 5,
                     main = "Residuals vs Leverage")
```

::: {.cell-output-display}
![](12_01_lecture_powerpoint_files/figure-docx/diagnostics-1.png)
:::
:::


# Formal Tests

-   These are the formal tests of

    -   normality of residuuals
    -   homogenetiy of variances


::: {.cell}

```{.r .cell-code}
# Normality test
shapiro_test <- shapiro.test(residuals(anova_model))
print("Shapiro-Wilk Normality Test:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Shapiro-Wilk Normality Test:"
```


:::

```{.r .cell-code}
print(shapiro_test)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(anova_model)
W = 0.99612, p-value = 0.8886
```


:::

```{.r .cell-code}
# Homogeneity of variance
levene_test <- leveneTest(body_mass_g ~ species * sex,
                          data = balanced_df)
print("Levene's Test for Homogeneity:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Levene's Test for Homogeneity:"
```


:::

```{.r .cell-code}
print(levene_test)
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   5  0.7841 0.5622
      198               
```


:::

```{.r .cell-code}
# Check for outliers
outliers_df <- balanced_df %>%
  mutate(
    fitted = fitted(anova_model),
    residuals = residuals(anova_model),
    std_resid = rstandard(anova_model)
  ) %>%
  filter(abs(std_resid) > 3)

print(paste("Number of outliers (|z| > 3):",
            nrow(outliers_df)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Number of outliers (|z| > 3): 0"
```


:::
:::


# Estimated Marginal Means (EMMs)

::::: columns
::: {.column width="60%"}
## Computing EMMs


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "Species EMMs:"
```


:::

::: {.cell-output .cell-output-stdout}

```
 species   emmean   SE  df lower.CL upper.CL
 Adelie      3692 36.8 198     3619     3764
 Chinstrap   3733 36.8 198     3661     3806
 Gentoo      5103 36.8 198     5030     5175

Results are averaged over the levels of: sex 
Confidence level used: 0.95 
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "Sex EMMs:"
```


:::

::: {.cell-output .cell-output-stdout}

```
 sex    emmean SE  df lower.CL upper.CL
 female   3848 30 198     3788     3907
 male     4504 30 198     4445     4563

Results are averaged over the levels of: species 
Confidence level used: 0.95 
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "Species by Sex EMMs:"
```


:::

::: {.cell-output .cell-output-stdout}

```
 species   sex    emmean SE  df lower.CL upper.CL
 Adelie    female   3335 52 198     3232     3437
 Chinstrap female   3527 52 198     3425     3630
 Gentoo    female   4681 52 198     4578     4783
 Adelie    male     4049 52 198     3946     4151
 Chinstrap male     3939 52 198     3836     4042
 Gentoo    male     5525 52 198     5422     5628

Confidence level used: 0.95 
```


:::
:::

:::

::: {.column width="40%"}
## Pairwise Comparisons


::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "Species pairwise comparisons:"
```


:::

::: {.cell-output .cell-output-stdout}

```
 contrast           estimate SE  df t.ratio p.value
 Adelie - Chinstrap    -41.5 52 198  -0.799  0.7041
 Adelie - Gentoo     -1411.4 52 198 -27.145  <.0001
 Chinstrap - Gentoo  -1369.9 52 198 -26.346  <.0001

Results are averaged over the levels of: sex 
P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "Species comparisons within sex:"
```


:::

::: {.cell-output .cell-output-stdout}

```
sex = female:
 contrast           estimate   SE  df t.ratio p.value
 Adelie - Chinstrap     -193 73.5 198  -2.620  0.0256
 Adelie - Gentoo       -1346 73.5 198 -18.310  <.0001
 Chinstrap - Gentoo    -1154 73.5 198 -15.690  <.0001

sex = male:
 contrast           estimate   SE  df t.ratio p.value
 Adelie - Chinstrap      110 73.5 198   1.490  0.2979
 Adelie - Gentoo       -1476 73.5 198 -20.079  <.0001
 Chinstrap - Gentoo    -1586 73.5 198 -21.569  <.0001

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::
:::

:::
:::::

# Post F Test of the interaction

What we need to do if the interaction is significant it test the overall
interaction


::: {.cell}

```{.r .cell-code}
# Get estimated marginal means for the interaction
emm_interaction <- emmeans(anova_model, ~ species * sex)  # Replace factor1 and factor2 with your actual factor names

# Pairwise comparisons with Tukey adjustment
pairs_interaction <- pairs(emm_interaction, adjust = "tukey")
```
:::


# Interaction tests continued


::: {.cell}

```{.r .cell-code}
# Get compact letter display (cld)
cld_interaction <- multcomp::cld(emm_interaction, 
                       Letters = letters,
                       adjust = "sidak")

# Display the results
# print(cld_interaction)

cld_df <- as.data.frame(cld_interaction) %>%
  arrange(species, sex)  # Sort by estimated marginal mean

# View the results with means and grouping letters
print(cld_df)
```

::: {.cell-output .cell-output-stdout}

```
 species   sex      emmean       SE  df lower.CL upper.CL .group
 Adelie    female 3334.559 51.99448 198 3196.378 3472.740  a    
 Adelie    male   4048.529 51.99448 198 3910.349 4186.710   b   
 Chinstrap female 3527.206 51.99448 198 3389.025 3665.387  a    
 Chinstrap male   3938.971 51.99448 198 3800.790 4077.151   b   
 Gentoo    female 4680.882 51.99448 198 4542.702 4819.063    c  
 Gentoo    male   5525.000 51.99448 198 5386.819 5663.181     d 

Confidence level used: 0.95 
Conf-level adjustment: sidak method for 6 estimates 
P value adjustment: sidak method for 15 tests 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::


# interaction plot


::: {.cell}
::: {.cell-output-display}
![](12_01_lecture_powerpoint_files/figure-docx/interaction_plot-1.png)
:::
:::


# Introduction to Unbalanced Designs

::::: columns
::: {.column width="60%"}
The Challenge of Unbalanced Data

Real-world data is often unbalanced: - Unequal sample sizes across
groups - Missing data patterns - Natural variation in sampling

Key Issues: - Type I, II, and III SS give different results - Order of
terms matters for Type I SS - Marginal means ≠ Simple averages -
Interpretation becomes complex

Statistical Model (same as balanced):
$$Y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \varepsilon_{ijk}$$

But parameter estimation differs!

# Data Preparation - Using Natural Unbalanced Data


::: {.cell}

```{.r .cell-code}
# Check sample sizes - naturally unbalanced
unbalanced_n <- p_df %>%
  count(species, sex) %>%
  arrange(species, sex)

print("Unbalanced sample sizes:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Unbalanced sample sizes:"
```


:::

```{.r .cell-code}
print(unbalanced_n)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 3
  species   sex        n
  <fct>     <fct>  <int>
1 Adelie    female    73
2 Adelie    male      73
3 Chinstrap female    34
4 Chinstrap male      34
5 Gentoo    female    58
6 Gentoo    male      61
```


:::

```{.r .cell-code}
# Calculate total N and imbalance ratio
total_n <- sum(unbalanced_n$n)
max_n <- max(unbalanced_n$n)
min_n <- min(unbalanced_n$n)
imbalance_ratio <- max_n / min_n

print(paste("Total N =", total_n))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Total N = 333"
```


:::

```{.r .cell-code}
print(paste("Imbalance ratio =", round(imbalance_ratio, 2)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Imbalance ratio = 2.15"
```


:::
:::

:::

::: {.column width="40%"}
## Visualizing Imbalance


::: {.cell}

```{.r .cell-code}
# Create sample size plot
imbalance_plot <- ggplot(unbalanced_n, 
       aes(x = species, y = n, fill = sex)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), 
            position = position_dodge(0.9),
            vjust = -0.5) +
  scale_fill_manual(values = c("#1f78b4", "#b2df8a")) +
  labs(title = "Sample Sizes by Group",
       subtitle = "Unbalanced Design",
       x = "Species", 
       y = "Sample Size",
       fill = "Sex") +
  theme_minimal() +
  theme(legend.position = "top")

print(imbalance_plot)
```

::: {.cell-output-display}
![](12_01_lecture_powerpoint_files/figure-docx/visualize_imbalance-1.png)
:::
:::

:::
:::::

# Understanding Sums of Squares Types

::::: columns
::: {.column width="60%"}
## Type I (Sequential) SS


::: {.cell}

```{.r .cell-code}
# Type I SS - Order matters!
# Model 1: Species then Sex
model1_type1 <- lm(body_mass_g ~ species + sex + species:sex,
                   data = p_df)
anova1_type1 <- anova(model1_type1)
print("Type I SS (Species → Sex → Interaction):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Type I SS (Species → Sex → Interaction):"
```


:::

```{.r .cell-code}
print(anova1_type1)
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Variance Table

Response: body_mass_g
             Df    Sum Sq  Mean Sq F value    Pr(>F)    
species       2 145190219 72595110 758.358 < 2.2e-16 ***
sex           1  37090262 37090262 387.460 < 2.2e-16 ***
species:sex   2   1676557   838278   8.757 0.0001973 ***
Residuals   327  31302628    95727                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Model 2: Sex then Species (different order)
model2_type1 <- lm(body_mass_g ~ sex + species + sex:species,
                   data = p_df)
anova2_type1 <- anova(model2_type1)
print("\nType I SS (Sex → Species → Interaction):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nType I SS (Sex → Species → Interaction):"
```


:::

```{.r .cell-code}
print(anova2_type1)
```

::: {.cell-output .cell-output-stdout}

```
Analysis of Variance Table

Response: body_mass_g
             Df    Sum Sq  Mean Sq F value    Pr(>F)    
sex           1  38878897 38878897 406.145 < 2.2e-16 ***
species       2 143401584 71700792 749.016 < 2.2e-16 ***
sex:species   2   1676557   838278   8.757 0.0001973 ***
Residuals   327  31302628    95727                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Compare SS for main effects
ss_comparison_df <- data.frame(
  Effect = c("Species", "Sex"),
  Order1_SS = c(anova1_type1$`Sum Sq`[1], 
                anova1_type1$`Sum Sq`[2]),
  Order2_SS = c(anova2_type1$`Sum Sq`[2], 
                anova2_type1$`Sum Sq`[1])
)
print("\nType I SS depends on order:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nType I SS depends on order:"
```


:::

```{.r .cell-code}
print(ss_comparison_df)
```

::: {.cell-output .cell-output-stdout}

```
   Effect Order1_SS Order2_SS
1 Species 145190219 143401584
2     Sex  37090262  38878897
```


:::
:::

:::

::: {.column width="40%"}
## How Type I SS Works

**Sequential decomposition:**

1.  **First term** gets all SS it can explain
2.  **Second term** gets SS after removing first
3.  **Third term** gets SS after removing first two

**Example calculation:** - SS(Species) = reduction in SS from null to
species-only model - SS(Sex\|Species) = additional reduction adding
sex - SS(Interaction\|Species,Sex) = additional reduction adding
interaction

**Problems with unbalanced data:** - Order dependency - Biased if
factors are correlated - Not invariant to coding
:::
:::::

# Type III Sums of Squares

::::: columns
::: {.column width="60%"}
## Type III (Marginal) SS


::: {.cell}

```{.r .cell-code}
# Set contrasts for Type III SS
options(contrasts = c("contr.sum", "contr.poly"))

# Fit model for Type III SS
model_u <- lm(body_mass_g ~ species * sex, 
              data = p_df)

# Type III ANOVA
anova_type3_u <- Anova(model_u, type = "III")
print("Type III Sums of Squares:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Type III Sums of Squares:"
```


:::

```{.r .cell-code}
print(anova_type3_u)
```

::: {.cell-output .cell-output-stdout}

```
Anova Table (Type III tests)

Response: body_mass_g
                Sum Sq  Df   F value    Pr(>F)    
(Intercept) 5232595969   1 54661.828 < 2.2e-16 ***
species      143001222   2   746.924 < 2.2e-16 ***
sex           29851220   1   311.838 < 2.2e-16 ***
species:sex    1676557   2     8.757 0.0001973 ***
Residuals     31302628 327                        
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Compare with Type I
print("\nComparison of F-values:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nComparison of F-values:"
```


:::

```{.r .cell-code}
comparison_f_df <- data.frame(
  Effect = c("Species", "Sex", "Interaction"),
  Type_I_F = round(anova1_type1$`F value`[1:3], 2),
  Type_III_F = round(anova_type3_u$`F value`[2:4], 2)
)
print(comparison_f_df)
```

::: {.cell-output .cell-output-stdout}

```
       Effect Type_I_F Type_III_F
1     Species   758.36     746.92
2         Sex   387.46     311.84
3 Interaction     8.76       8.76
```


:::
:::

:::

::: {.column width="40%"}
## How Type III SS Works

**Marginal decomposition:**

Each effect tested after adjusting for all others: -
SS(Species\|Sex,Interaction) - SS(Sex\|Species,Interaction) -
SS(Interaction\|Species,Sex)

**Advantages:** - Order invariant - Tests hypotheses about unweighted
means - Standard in most software

**Disadvantages:** - Lower power with missing cells - Tests may not be
orthogonal - Requires careful interpretation


::: {.cell}

```{.r .cell-code}
# Calculate effect sizes
ss_total_u <- sum(anova_type3_u$`Sum Sq`[-1])

eta_squared_p_df <- data.frame(
  Effect = c("Species", "Sex", "Interaction"),
  Eta_Squared = round(
    anova_type3_u$`Sum Sq`[2:4] / ss_total_u, 
    4)
)
print("Effect Sizes (Type III):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Effect Sizes (Type III):"
```


:::

```{.r .cell-code}
print(eta_squared_p_df)
```

::: {.cell-output .cell-output-stdout}

```
       Effect Eta_Squared
1     Species      0.6947
2         Sex      0.1450
3 Interaction      0.0081
```


:::
:::

:::
:::::

# Manual SS Calculation Demonstration

::::: columns
::: {.column width="60%"}
## Computing SS Step-by-Step


::: {.cell}

```{.r .cell-code}
# Grand mean
grand_mean <- mean(p_df$body_mass_g)
print(paste("Grand mean:", round(grand_mean, 1)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Grand mean: 4207.1"
```


:::

```{.r .cell-code}
# Total SS
ss_total_manual <- sum((p_df$body_mass_g - grand_mean)^2)
print(paste("Total SS:", round(ss_total_manual, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Total SS: 215259666"
```


:::

```{.r .cell-code}
# Between-group SS (full model)
group_means_df <- p_df %>%
  group_by(species, sex) %>%
  mutate(group_mean = mean(body_mass_g)) %>%
  ungroup()

ss_between <- sum((group_means_df$group_mean - grand_mean)^2)
print(paste("Between-groups SS:", round(ss_between, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Between-groups SS: 183957038"
```


:::

```{.r .cell-code}
# Within-group SS (Error SS)
ss_within <- sum((group_means_df$body_mass_g - 
                  group_means_df$group_mean)^2)
print(paste("Within-groups SS:", round(ss_within, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Within-groups SS: 31302628"
```


:::

```{.r .cell-code}
# Verify: Total = Between + Within
print(paste("Check: Between + Within =", 
            round(ss_between + ss_within, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Check: Between + Within = 215259666"
```


:::
:::

:::

::: {.column width="40%"}
## Decomposing Between-Group SS


::: {.cell}

```{.r .cell-code}
# For Type I SS (sequential)
# SS for species (ignoring sex)
species_only_model <- lm(body_mass_g ~ species, 
                         data = p_df)
ss_species_only <- sum((fitted(species_only_model) - 
                       grand_mean)^2)
print(paste("SS(Species alone):", 
            round(ss_species_only, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "SS(Species alone): 145190219"
```


:::

```{.r .cell-code}
# SS for sex (ignoring species)  
sex_only_model <- lm(body_mass_g ~ sex, 
                     data = p_df)
ss_sex_only <- sum((fitted(sex_only_model) - 
                   grand_mean)^2)
print(paste("SS(Sex alone):", 
            round(ss_sex_only, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "SS(Sex alone): 38878897"
```


:::

```{.r .cell-code}
# Note: These don't add up to between SS
# because effects are not orthogonal!
print(paste("Sum of main effects:", 
            round(ss_species_only + ss_sex_only, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Sum of main effects: 184069116"
```


:::

```{.r .cell-code}
print(paste("Actual between SS:", 
            round(ss_between, 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Actual between SS: 183957038"
```


:::

```{.r .cell-code}
print(paste("Difference (due to correlation):", 
            round(ss_between - 
                  (ss_species_only + ss_sex_only), 0)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Difference (due to correlation): -112078"
```


:::
:::

:::
:::::

# Estimated Marginal Means - Unbalanced Data

### Computing EMMs


::: {.cell}

```{.r .cell-code}
# EMMs for main effects
species_emm_u <- emmeans(model_u, ~ species)
sex_emm_u <- emmeans(model_u, ~ sex)

print("Species EMMs (model-based):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Species EMMs (model-based):"
```


:::

```{.r .cell-code}
print(species_emm_u)
```

::: {.cell-output .cell-output-stdout}

```
 species   emmean   SE  df lower.CL upper.CL
 Adelie      3706 25.6 327     3656     3757
 Chinstrap   3733 37.5 327     3659     3807
 Gentoo      5082 28.4 327     5026     5138

Results are averaged over the levels of: sex 
Confidence level used: 0.95 
```


:::

```{.r .cell-code}
print("\nSex EMMs (model-based):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nSex EMMs (model-based):"
```


:::

```{.r .cell-code}
print(sex_emm_u)
```

::: {.cell-output .cell-output-stdout}

```
 sex    emmean   SE  df lower.CL upper.CL
 female   3859 25.3 327     3809     3908
 male     4489 25.2 327     4440     4539

Results are averaged over the levels of: species 
Confidence level used: 0.95 
```


:::

```{.r .cell-code}
# Get estimated marginal means for the interaction
emm_interaction_u <- emmeans(model_u, ~ species * sex)  # Replace factor1 and factor2 with your actual factor names

# Pairwise comparisons with Tukey adjustment
pairs_interaction_u <- pairs(emm_interaction, adjust = "tukey")
pairs_interaction_u
```

::: {.cell-output .cell-output-stdout}

```
 contrast                          estimate   SE  df t.ratio p.value
 Adelie female - Chinstrap female      -193 73.5 198  -2.620  0.0973
 Adelie female - Gentoo female        -1346 73.5 198 -18.310  <.0001
 Adelie female - Adelie male           -714 73.5 198  -9.710  <.0001
 Adelie female - Chinstrap male        -604 73.5 198  -8.220  <.0001
 Adelie female - Gentoo male          -2190 73.5 198 -29.789  <.0001
 Chinstrap female - Gentoo female     -1154 73.5 198 -15.690  <.0001
 Chinstrap female - Adelie male        -521 73.5 198  -7.090  <.0001
 Chinstrap female - Chinstrap male     -412 73.5 198  -5.600  <.0001
 Chinstrap female - Gentoo male       -1998 73.5 198 -27.169  <.0001
 Gentoo female - Adelie male            632 73.5 198   8.600  <.0001
 Gentoo female - Chinstrap male         742 73.5 198  10.090  <.0001
 Gentoo female - Gentoo male           -844 73.5 198 -11.480  <.0001
 Adelie male - Chinstrap male           110 73.5 198   1.490  0.6711
 Adelie male - Gentoo male            -1476 73.5 198 -20.079  <.0001
 Chinstrap male - Gentoo male         -1586 73.5 198 -21.569  <.0001

P value adjustment: tukey method for comparing a family of 6 estimates 
```


:::
:::


# Interaction tests continued


::: {.cell}

```{.r .cell-code}
# Get compact letter display (cld)
cld_interaction_u <- multcomp::cld(emm_interaction_u, 
                       Letters = letters,
                       adjust = "sidak")

# Display the results
# print(cld_interaction)

cld_df_u <- as.data.frame(cld_interaction_u) %>%
  arrange(species, sex)  # Sort by estimated marginal mean

# View the results with means and grouping letters
print(cld_df_u)
```

::: {.cell-output .cell-output-stdout}

```
 species   sex      emmean       SE  df lower.CL upper.CL .group
 Adelie    female 3368.836 36.21222 327 3272.980 3464.692  a    
 Adelie    male   4043.493 36.21222 327 3947.637 4139.349   b   
 Chinstrap female 3527.206 53.06120 327 3386.749 3667.662  a    
 Chinstrap male   3938.971 53.06120 327 3798.514 4079.427   b   
 Gentoo    female 4679.741 40.62586 327 4572.202 4787.281    c  
 Gentoo    male   5484.836 39.61427 327 5379.975 5589.698     d 

Confidence level used: 0.95 
Conf-level adjustment: sidak method for 6 estimates 
P value adjustment: sidak method for 15 tests 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::


# Pairwise Comparisons

::::: columns
::: {.column width="60%"}
### Tukey HSD Comparisons


::: {.cell}

```{.r .cell-code}
# Pairwise comparisons for species
species_pairs_u <- pairs(species_emm_u, 
                         adjust = "tukey")
print("Species pairwise comparisons (Tukey):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Species pairwise comparisons (Tukey):"
```


:::

```{.r .cell-code}
print(species_pairs_u)
```

::: {.cell-output .cell-output-stdout}

```
 contrast           estimate   SE  df t.ratio p.value
 Adelie - Chinstrap    -26.9 45.4 327  -0.593  0.8241
 Adelie - Gentoo     -1376.1 38.2 327 -36.007  <.0001
 Chinstrap - Gentoo  -1349.2 47.0 327 -28.682  <.0001

Results are averaged over the levels of: sex 
P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::

```{.r .cell-code}
# Effect of sex within each species
sex_by_species_emm <- emmeans(model_u, ~ sex | species)
sex_effects_df <- pairs(sex_by_species_emm)
print("\nSex effect within each species:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nSex effect within each species:"
```


:::

```{.r .cell-code}
print(sex_effects_df)
```

::: {.cell-output .cell-output-stdout}

```
species = Adelie:
 contrast      estimate   SE  df t.ratio p.value
 female - male     -675 51.2 327 -13.174  <.0001

species = Chinstrap:
 contrast      estimate   SE  df t.ratio p.value
 female - male     -412 75.0 327  -5.487  <.0001

species = Gentoo:
 contrast      estimate   SE  df t.ratio p.value
 female - male     -805 56.7 327 -14.188  <.0001
```


:::
:::

:::

::: {.column width="40%"}
### Interaction Contrasts


::: {.cell}

```{.r .cell-code}
# Test if sex effect differs across species
interaction_test <- emmeans(model_u, 
                            pairwise ~ sex | species)

# Extract contrasts
contrasts_df <- as.data.frame(interaction_test$contrasts)

# Calculate difference in sex effects
sex_diff_adelie <- contrasts_df[1, "estimate"]
sex_diff_chinstrap <- contrasts_df[2, "estimate"]
sex_diff_gentoo <- contrasts_df[3, "estimate"]

interaction_summary_df <- data.frame(
  Species = c("Adelie", "Chinstrap", "Gentoo"),
  Sex_Effect = round(c(sex_diff_adelie, 
                       sex_diff_chinstrap,
                       sex_diff_gentoo), 0)
)

print("Sex effect (Male - Female) by species:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Sex effect (Male - Female) by species:"
```


:::

```{.r .cell-code}
print(interaction_summary_df)
```

::: {.cell-output .cell-output-stdout}

```
    Species Sex_Effect
1    Adelie       -675
2 Chinstrap       -412
3    Gentoo       -805
```


:::

```{.r .cell-code}
# Test if these differ significantly
print("\nInteraction interpretation:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nInteraction interpretation:"
```


:::

```{.r .cell-code}
if(anova_type3_u$`Pr(>F)`[4] < 0.05) {
  print("Sex effects differ across species")
} else {
  print("Sex effects similar across species")
}
```

::: {.cell-output .cell-output-stdout}

```
[1] "Sex effects differ across species"
```


:::
:::

:::
:::::

# Diagnostic Plots - Unbalanced Design

::::: columns
::: {.column width="60%"}
## Model Diagnostics


::: {.cell}

```{.r .cell-code}
# Diagnostic plots
par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(model_u, which = 1,
     main = "Residuals vs Fitted")

# Q-Q plot
plot(model_u, which = 2,
     main = "Normal Q-Q")

# Scale-Location  
plot(model_u, which = 3,
     main = "Scale-Location")

# Residuals by group
plot(fitted(model_u), residuals(model_u),
     col = as.numeric(interaction(p_df$species, 
                                  p_df$sex)),
     main = "Residuals by Group",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)
```

::: {.cell-output-display}
![](12_01_lecture_powerpoint_files/figure-docx/diagnostics_unbalanced-1.png)
:::
:::

:::

::: {.column width="40%"}
## Assumption Tests


::: {.cell}

```{.r .cell-code}
# Normality test
shapiro_u <- shapiro.test(residuals(model_u))
print("Shapiro-Wilk test:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Shapiro-Wilk test:"
```


:::

```{.r .cell-code}
print(shapiro_u)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(model_u)
W = 0.99776, p-value = 0.9367
```


:::

```{.r .cell-code}
# Homogeneity of variance
levene_u <- leveneTest(body_mass_g ~ species * sex,
                       data = p_df,
                       center = median)
print("\nLevene's test:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nLevene's test:"
```


:::

```{.r .cell-code}
print(levene_u)
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   5  1.3908 0.2272
      327               
```


:::

```{.r .cell-code}
# Check residuals by group
residual_summary_df <- p_df %>%
  mutate(
    residuals = residuals(model_u),
    fitted = fitted(model_u)
  ) %>%
  group_by(species, sex) %>%
  summarise(
    n = n(),
    mean_resid = mean(residuals),
    sd_resid = sd(residuals),
    .groups = 'drop'
  )

print("\nResidual SD by group:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nResidual SD by group:"
```


:::

```{.r .cell-code}
print(residual_summary_df[, c("species", "sex", 
                              "sd_resid")])
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 3
  species   sex    sd_resid
  <fct>     <fct>     <dbl>
1 Adelie    female     269.
2 Adelie    male       347.
3 Chinstrap female     285.
4 Chinstrap male       362.
5 Gentoo    female     282.
6 Gentoo    male       313.
```


:::
:::

:::
:::::

# Comparing Balanced vs Unbalanced Results

::::: columns
::: {.column width="60%"}
## Side-by-Side Comparison


::: {.cell}

```{.r .cell-code}
# Load balanced results (simulated)
set.seed(123)
b_df <- p_df %>%
  group_by(species, sex) %>%
  sample_n(34) %>%
  ungroup()

# Fit balanced model
options(contrasts = c("contr.sum", "contr.poly"))
model_b <- lm(body_mass_g ~ species * sex, data = b_df)
anova_b <- Anova(model_b, type = "III")

# Create comparison table
comparison_results_df <- data.frame(
  Effect = c("Species", "Sex", "Interaction"),
  Balanced_F = round(anova_b$`F value`[2:4], 2),
  Balanced_p = round(anova_b$`Pr(>F)`[2:4], 4),
  Unbalanced_F = round(anova_type3_u$`F value`[2:4], 2),
  Unbalanced_p = round(anova_type3_u$`Pr(>F)`[2:4], 4)
)

print("Balanced vs Unbalanced Results:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Balanced vs Unbalanced Results:"
```


:::

```{.r .cell-code}
print(comparison_results_df)
```

::: {.cell-output .cell-output-stdout}

```
       Effect Balanced_F Balanced_p Unbalanced_F Unbalanced_p
1     Species     477.20      0e+00       746.92        0e+00
2         Sex     239.22      0e+00       311.84        0e+00
3 Interaction       9.10      2e-04         8.76        2e-04
```


:::

```{.r .cell-code}
# Compare power
print("\nSample sizes:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "\nSample sizes:"
```


:::

```{.r .cell-code}
print(paste("Balanced total N:", nrow(b_df)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Balanced total N: 204"
```


:::

```{.r .cell-code}
print(paste("Unbalanced total N:", nrow(p_df)))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Unbalanced total N: 333"
```


:::

```{.r .cell-code}
print(paste("Data discarded:", 
            nrow(p_df) - nrow(b_df), 
            "observations"))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Data discarded: 129 observations"
```


:::
:::

:::

::: {.column width="40%"}
## Visual Comparison


::: {.cell}

```{.r .cell-code}
# Combine EMMs from both analyses
emm_b <- as.data.frame(emmeans(model_b, ~ species))
emm_u <- as.data.frame(emmeans(model_u, ~ species))

comparison_plot_df <- data.frame(
  Species = rep(levels(as.factor(p_df$species)), 2),
  Design = rep(c("Balanced", "Unbalanced"), 
               each = 3),
  EMM = c(as.numeric(emm_b$emmean),
          as.numeric(emm_u$emmean)),
  SE = c(as.numeric(emm_b$SE),
         as.numeric(emm_u$SE))
)

comparison_plot <- ggplot(comparison_plot_df, 
       aes(x = Species, y = EMM, 
           color = Design, group = Design)) +
  geom_point(position = position_dodge(0.3), 
             size = 3) +
  geom_errorbar(aes(ymin = EMM - SE, 
                    ymax = EMM + SE),
                position = position_dodge(0.3),
                width = 0.2) +
  labs(title = "EMMs: Balanced vs Unbalanced",
       y = "Estimated Marginal Mean (g)",
       color = "Design") +
  theme_minimal() +
  theme(legend.position = "top")

print(comparison_plot)
```

::: {.cell-output-display}
![](12_01_lecture_powerpoint_files/figure-docx/visual_comparison-1.png)
:::
:::

:::
:::::

# Summary and Best Practices

### Key Takeaways

### Unbalanced Designs:

**Challenges:** - Type I SS depends on order - Simple means ≠ EMMs -
Reduced power for interactions - Complex interpretation

**Solutions:** - Use Type III SS for main effects - Report EMMs, not
simple means - Check assumptions carefully - Consider impact of
imbalance

### Recommendations:

1.  **Always report:**
    -   Sample sizes per cell
    -   Type of SS used
    -   EMMs with CIs
2.  **Visualization:**
    -   Show actual data points
    -   Include error bars
    -   Note unequal sample sizes
3.  **Interpretation:**
    -   Focus on effect sizes
    -   Consider practical significance
    -   Acknowledge limitations

# Final Recommendations

**For Experimental Studies:** - Design for balance when possible -
Randomize allocation - Plan for attrition

**For Observational Studies:** - Accept imbalance as reality - Use Type
III SS - Report EMMs - Consider covariates (ANCOVA)

**Statistical Software Defaults:** - R: Type I (sequential) - SAS: Type
III (marginal) - SPSS: Type III (marginal)

Always specify which type you're using!
