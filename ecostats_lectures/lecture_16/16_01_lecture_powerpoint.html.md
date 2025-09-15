---
title: "Lecture 16 - Multivariate Statistics"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "16_02_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  # RevealJS - UNCHANGED (keeps your two-column layout and large images)
  revealjs:
    output-file: "16_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "16_01_lecture_powerpoint.docx"
  pptx:
    output-file: "16_01_lecture_powerpoint.pptx"
  typst:
    output-file: "16_01_lecture_powerpoint.pdf"
---














# Introduction to Multivariate Statistics

::::: columns
::: {.column width="60%"}
## Overview

-   Multivariate data: multiple variables per object
-   Types of multivariate analyses
    -   Functional vs. structural methods
    -   R-mode vs. Q-mode analyses
-   Eigenvectors, eigenvalues, and components
-   Distance and dissimilarity measures
-   Data transformations and standardization
-   Screening multivariate data
-   MANOVA
:::

::: {.column width="40%"}
![](images/clipboard-3313005791.png){width="500"}
:::
:::::

# Multivariate Data Structure

::::: columns
::: {.column width="60%"}
-   Multiple variables recorded about each object (individual, quadrat, site, etc.)
    -   or responses that are from the same treatment factor
    -   length, weight, width, color, spines, etc
-   Objects: rows (i = 1 to n)
-   Variables: columns (j = 1 to p)
-   Examples:
    -   Stream sites with multiple chemical parameters
    -   Species with multiple morphological traits
    -   Sample units with multiple species abundances
:::

::: {.column width="40%"}
![](images/clipboard-2611839949.png){width="380"}
:::
:::::

::: callout-tip
# Multivariate data vs. multivariate analysis

We've already seen multivariate data in multiple regression and multi-factor ANOVA

**Now we'll look at cases with multiple response variables.**
:::

# Functional vs. Structural Methods

::::: columns
::: {.column width="60%"}
## Functional vs. Structural Methods

**Functional methods**: - Clear response and predictor variables - Goal: relate Y's to X's - Examples: MANOVA, PERMANOVA

**Structural methods**: - Find patterns/structure in data - Often no clear predictors - Examples: PCA, NMDS, Cluster Analysis
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::
:::











:::
:::::

# Functional Methods Examples

::::: columns
::: {.column width="60%"}
Example 1:

-   sample 30 stream sites (objects)
-   record TP, TN, pH, DO, chloride concentration, etc.
-   each parameter is a variable

Example 2:

-   sample 30 stream sites (objects)
-   collect benthic invertebrates
-   each species is now a variable

Sometimes combine both…
:::

::: {.column width="40%"}
![](images/clipboard-3995348330.png){width="500"}
:::
:::::

# Functional Methods Visualization

::::: columns
::: {.column width="60%"}
![](images/clipboard-610257889.png){width="600"}
:::

::: {.column width="40%"}
![](images/clipboard-3995348330.png){width="500"}
:::
:::::

# Ecological Multivariate Methods Overview

::::: columns
::: {.column width="60%"}
Can divide ecological MV methods into "functional" and "structural"

-   Functional methods: clear response variable(s) and predictor variables. Goal is to relate Ys to Xs (regression, MANOVA, ANOSIM, PERMANOVA).

-   Structural methods: concerned with finding structure /pattern in the data. Often no clear predictor variables (PCA, NMDS, Cluster analysis).
:::

::: {.column width="40%"}
![](images/clipboard-1164906951.png){width="500"}
:::
:::::

# Structural Methods: Two Approaches

::::: columns
::: {.column width="60%"}
## Two Main Approaches

**Scaling/Ordination Methods**: - Reduce dimensions with new derived variables - Summarize patterns in data - Examples: PCA, CCA

**Dissimilarity-Based Methods**: - Measure dissimilarity between objects - Visualize relationships between objects - Examples: NMDS, Cluster Analysis
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/label_code_chunk-1.png){width=672}
:::
:::











:::
:::::

# Structural Methods: Scaling/Ordination

::::: columns
::: {.column width="60%"}
Structural methods can be divided further into:

Methods based on [scaling or ordination]{.underline}

Goal: reduce number of vars by deriving new variables that summarize data.

Examples include PCA, CCA
:::

::: {.column width="40%"}
![](images/clipboard-2269872697.png){width="500"}
:::
:::::

# Structural Methods: Dissimilarity-Based

::::: columns
::: {.column width="60%"}
Structural methods can be divided further into:

-   Methods based on dissimilarity measurements

-   Goal: measure and graphically show degree of dissimilarity between objects.

-   Examples include (N)MDS and cluster analysis
:::

::: {.column width="40%"}
![](images/clipboard-3989037993.png){width="500"}
:::
:::::

# Eigenvectors, Eigenvalues, and Components: Concept

::::: columns
::: {.column width="60%"}
-   Goal: derive new variables (principal components) that explain variation in data
-   Components are linear combinations of original variables:
    -   z<sub>ik</sub> = c<sub>1</sub>y<sub>i1</sub> + c<sub>2</sub>y<sub>i2</sub> + ... + c<sub>p</sub>y<sub>ip</sub>
-   Properties of derived variables:
    -   First component explains most variation
    -   Second explains most remaining variation
    -   Components are uncorrelated with each other
    -   As many components as original variables
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-2-1.png){width=672}
:::
:::











:::
:::::

# Eigenvectors and Components: Interpretation

::::: columns
::: {.column width="60%"}
How to think about the new values

-   zik is value of new variable k for object I
-   yi1- yip are values of original variables for object i
-   c1-cp are coefficients that show importance of the original variables to new derived variable
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::











:::
:::::

::: callout-tip
# Key concept

Eigenvalues (λ) represent the amount of variation explained by each new derived variable, while eigenvectors contain the coefficients showing how original variables contribute to each component.
:::

# Eigenvalues and Components: Properties

::::: columns
::: {.column width="60%"}
Derived variables are found so that:

-   First derived variable explains most of the variation in the data
-   Second most of the remaining variation
-   And so on…
-   As many derived variables as original variables (p)
-   Derived variables are uncorrelated with each other
:::

::: {.column width="40%"}
![](images/clipboard-1938305800.png){width="500"}
:::
:::::

# Eigenvalues and Eigenvectors: Mathematical Details

-   Eigenvalues (latent roots) represent amount of variation in data explained by the new k= 1 to p derived variables (λ1, λ2 …λp).

-   Eigenvalues are population parameters and are estimated using ML to get sample statistics (l1, l2…lp)

-   Eigenvectors are lists of coefficients (c) that show contribution of original variables to new, derived variables

-   Each new variable has an eigenvalue and an eigenvector

-   New variables (components) are derived from a p x p covariance or correlation matrix of original variables

# Eigenvalue Matrix Representation

![](images/clipboard-2714419027.png){width="800"}

# Distance and Dissimilarity Measures: Concept

::::: columns
::: {.column width="60%"}
-   Measure how different objects are in multivariate space
-   Common measures:
    -   **Euclidean distance**: direct geometric distance
    -   **Manhattan distance**: sum of absolute differences
    -   **Bray-Curtis**: good for species abundance data
    -   **Kulczynski**: for abundance data with zeros
-   Used in cluster analysis, MDS, and other techniques
-   Create dissimilarity matrices for analysis
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::











:::
:::::

# Distance and Dissimilarity: Background

::::: columns
::: {.column width="60%"}
-   Previous approach relies on analysis of covariance/ correlation bw variables
-   Another class of MV analysis uses measures of similarity/dissimilarity bw objects (MDS, cluster analysis)
-   Similarity/dissimilarity indices measure how alike/different objects (e.g. Lakes) are in MV space
-   Many measures of dissimilarity (Euclidean, Manhattan, Bray-Curtis, etc, etc)
:::

::: {.column width="40%"}
![](images/clipboard-1914234757.png){width="500"}
:::
:::::

# Dissimilarity Matrix Representation

Dissimilarity is often represented as a dissimilarity matrix

![](images/clipboard-3253069105.png){width="800"}

# Data Transformations: Common Approaches

Data transformation is common and useful in MV analyses

-   Log transformation is common in PCA/CCA analyses based on eigenvectors, since linearizing relationships bw variables will improve extraction of eigenvectors
-   Fourth root transform is very common and sometimes "blanket recommended" for analysis of species composition data (each variable is a species w counts- MDS, cluster analysis). Idea is to lessen importance of common and abundant species

# Data Standardization: Methods

::::: columns
::: {.column width="60%"}
-   Data standardization is also common; adjusts data so all variables have same means and/or variance
    -   Centering- mean subtracted from each value (new mean=0)
    -   Standardization- centered observations divided by SD (mean=0, sd=1)
-   Crucial for analyses of variables measured in different units
-   More ambiguous for species abundance data
:::

::: {.column width="40%"}
![](images/clipboard-2420239610.png){width="500"}
:::
:::::

# Data Transformations & Standardization: Visual

::::: columns
::: {.column width="60%"}
## Common Approaches

**Transformations**: - Log transformation for skewed data - Root transformations for count data\
- Fourth-root for species abundance data

**Standardization**: - Centering: subtract mean (mean = 0) - Standardization: divide by SD (SD = 1) - Crucial for variables with different units - May not be appropriate for species data
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-5-1.png){width=672}
:::
:::











:::
:::::

::: callout-tip
# Why standardize?

Standardization ensures all variables contribute equally to the analysis regardless of their original units or scales of measurement. Without it, variables with larger values or variances would dominate the results.
:::

# Multivariate Graphics Options

::::: columns
::: {.column width="60%"}
## Visual Representation Methods

-   **SPLOMS/Scatterplot Matrices**: show bivariate relationships
-   **Star plots**: display multiple variables per object
-   **Chernoff faces**: represent variables as facial features
-   **Heatmaps**: visualize data matrices with color
-   **Biplots**: show objects and variables together
-   **Ordination plots**: visualize relationships in reduced dimensions
:::

::: {.column width="40%"}
![](images/clipboard-1166717588.png){width="500"}
:::
:::::

# Screening Multivariate Data: Outliers and Missing Data

::::: columns
::: {.column width="60%"}
## Key Issues to Check

**Multivariate Outliers**: - Objects with unusual patterns across variables - Detected with Mahalanobis distance (d²) - Test against χ² distribution with p df

**Missing Observations**: - Common approaches: - Deletion: remove affected object or variable - Imputation: estimate missing values - Maximum likelihood methods - Multiple imputation
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::
:::











:::
:::::

# MANOVA: Introduction

::::: columns
::: {.column width="60%"}
-   Multivariate extension of ANOVA
-   Tests for differences in group centroids based on multiple response variables
-   Advantages over multiple ANOVAs:
    -   Controls family-wise error rate
    -   Accounts for correlations between variables
    -   More powerful when variables are correlated
-   Common test statistics:
    -   Wilk's lambda (λ)
    -   Pillai's trace
    -   Hotelling-Lawley trace
-   Famous dataframe built into R is the iris dataset
:::

::: {.column width="40%"}
![](images/clipboard-2761498364.png){width="500"}
:::
:::::

# MANOVA: Iris Dataset Example

::::: columns
::: {.column width="60%"}
Morphometric measurements on n=150 flowers

Response vars: Septal length + width, petal length + width

Predictor variable: species

Question: are there differences bw species?
:::

::: {.column width="40%"}
![](images/clipboard-2761498364.png){width="500"}
:::
:::::

# MANOVA: Data Structure

::::: columns
::: {.column width="60%"}
Morphometric measurements on n=150 flowers

Response vars: Septal length + width, petal length + width

Predictor variable: species

Question: are there differences bw species?
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
iris_df <- iris %>% clean_names()
iris_long_df <- iris_df %>% pivot_longer(cols = -species, 
               names_to = "variable", 
               values_to = "measure")
write_csv(iris_df, "data/iris.csv")
head(iris_df)
```

::: {.cell-output .cell-output-stdout}

```
  sepal_length sepal_width petal_length petal_width species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
```


:::
:::











:::
:::::

# MANOVA: Data Visualization

::::: columns
::: {.column width="60%"}
Morphometric measurements on n=150 flowers

Response vars: Septal length + width, petal length + width

Predictor variable: species

Question: are there differences bw species?
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
iris_long_df %>% ggplot(aes(species, measure, fill=species))+
  geom_boxplot()+
  facet_wrap(.~variable)
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-8-1.png){width=672}
:::
:::











:::
:::::

# MANOVA vs. Multiple ANOVAs

::::: columns
::: {.column width="60%"}
One approach:

series of 1-way ANOVAs

for example ---\>

But:

-   Variables and tests are not independent
-   Multiple testing problem can reduce power
-   MANOVA considers all response variables simultaneously
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
sepal_model <- aov(sepal_length~species, data = iris_df)
Anova(sepal_model, type = 3)
```

::: {.cell-output .cell-output-stdout}

```
Anova Table (Type III tests)

Response: sepal_length
             Sum Sq  Df F value    Pr(>F)    
(Intercept) 1253.00   1 4728.16 < 2.2e-16 ***
species       63.21   2  119.26 < 2.2e-16 ***
Residuals     38.96 147                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::











:::
:::::

# MANOVA: Centroids vs. Means

::::: columns
::: {.column width="60%"}
Instead of means compare centroids

but for all of the variables not just two
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
mean_points <- iris_df %>% 
  group_by(species) %>% 
  summarise(mean_length = mean(petal_length), 
            mean_width = mean(petal_width),
            .groups = 'drop')

iris_plot<-iris_df %>% 
  ggplot(aes(x=petal_length, y=petal_width, color=species)) +
  geom_point(alpha = 0.85, size = 2) +
  geom_point(data=mean_points, 
             aes(x=mean_length, y=mean_width, fill=species), 
             shape=23, color="black", stroke=1.2,alpha = .7,
             size=6) +
  theme_light()
iris_plot
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-10-1.png){width=672}
:::
:::











:::
:::::

# MANOVA: SSCP Matrices

::::: columns
::: {.column width="60%"}
A one-way MANOVA tests the Ho that there are no differences in population centroids

-   Ho tested by partitioning variance, but instead of SS, use SSCP matrices:
-   H matrix: between group SSCP
-   E matrix: within group SSCP
-   T matrix: total SSCP
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
iris_plot
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-11-1.png){width=672}
:::
:::











:::
:::::

# MANOVA: Test Statistics

::::: columns
::: {.column width="60%"}
Several test statistics can be determined:

-   Wilk's λ: ratio of matrix determinants:\|E\|/\|T\|
-   Smaller values: larger group differences
-   Can be converted to approximate F ratios, compared to F distribution to find p
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
iris_plot
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::











:::
:::::

::: callout-tip
# MANOVA Assumptions

-   Normal distribution:
    -   response vars should be normally distributed within groups (relatively robust - No outliers (use di2 to diagnose; very sensitive to this assumption
    -   Equal variance of the response variables across groups
    -   Linearity: response variables linearly related to each other
    -   No strong multicollinearity in response variables
    -   Best performance in balances designs
:::

# MANOVA: Model Fitting












::: {.cell}

```{.r .cell-code}
iris_manova_model <- manova(cbind(sepal_length, sepal_width, petal_length, petal_width) ~ species, data = iris_df)

summary(iris_manova_model)
```

::: {.cell-output .cell-output-stdout}

```
           Df Pillai approx F num Df den Df    Pr(>F)    
species     2 1.1919   53.466      8    290 < 2.2e-16 ***
Residuals 147                                            
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::












# MANOVA: Assumption Testing - Normality

::::: columns
::: {.column width="60%"}
Assumption test

1.  Multivariate Normality
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Test multivariate normality for all data together
response_matrix <- iris_df %>% 
  dplyr::select(sepal_length, sepal_width, petal_length, petal_width) %>%
  as.matrix()

# Multivariate Shapiro-Wilk test for entire dataset
mshapiro.test(t(response_matrix))
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  Z
W = 0.97935, p-value = 0.02342
```


:::
:::











:::
:::::

# MANOVA: Assumption Testing - Homogeneity

::::: columns
::: {.column width="60%"}
Assumption test

2.  Homogeneity of Covariance Matrices (Box's M Test)
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
response_vars <- iris_df %>% 
  dplyr::select(sepal_length, sepal_width, petal_length, petal_width) 

# Box's M test for equality of covariance matrices
box_m_result <- boxM(response_vars, iris_df$species)
print(box_m_result)
```

::: {.cell-output .cell-output-stdout}

```

	Box's M-test for Homogeneity of Covariance Matrices

data:  response_vars
Chi-Sq (approx.) = 140.94, df = 20, p-value < 2.2e-16
```


:::
:::











:::
:::::

# MANOVA: Visual Assumption Assessment

::::: columns
::: {.column width="60%"}
Assumption test

3.  Visual Assessment of Assumptions
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Create Q-Q plots for each variable by species
iris_long <- iris_df %>%
  pivot_longer(cols = c(sepal_length, sepal_width, petal_length, petal_width),
               names_to = "variable",
               values_to = "value")

iris_long %>%
  ggplot(aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(variable ~ species, scales = "free") +
  labs(title = "Q-Q Plots by Species and Variable") +
  theme_light()
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/assumption-plots-1.png){width=672}
:::
:::











:::
:::::

# MANOVA: Follow-up Univariate ANOVAs

::::: columns
::: {.column width="60%"}
## Follow-up Univariate ANOVAs
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Univariate ANOVAs for each response variable

# Sepal Length ANOVA
sepal_length_aov <- aov(sepal_length ~ species, data = iris_df)
summary(sepal_length_aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df Sum Sq Mean Sq F value Pr(>F)    
species       2  63.21  31.606   119.3 <2e-16 ***
Residuals   147  38.96   0.265                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Sepal Width ANOVA
sepal_width_aov <- aov(sepal_width ~ species, data = iris_df)
summary(sepal_width_aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df Sum Sq Mean Sq F value Pr(>F)    
species       2  11.35   5.672   49.16 <2e-16 ***
Residuals   147  16.96   0.115                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Petal Length ANOVA
petal_length_aov <- aov(petal_length ~ species, data = iris_df)
summary(petal_length_aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df Sum Sq Mean Sq F value Pr(>F)    
species       2  437.1  218.55    1180 <2e-16 ***
Residuals   147   27.2    0.19                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Petal Width ANOVA
petal_width_aov <- aov(petal_width ~ species, data = iris_df)
summary(petal_width_aov)
```

::: {.cell-output .cell-output-stdout}

```
             Df Sum Sq Mean Sq F value Pr(>F)    
species       2  80.41   40.21     960 <2e-16 ***
Residuals   147   6.16    0.04                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::











:::
:::::

# MANOVA: Post-hoc Comparisons

::::: columns
::: {.column width="60%"}
## Post-hoc Comparisons using emmeans
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Sepal Length comparisons
print("Sepal Length comparisons")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Sepal Length comparisons"
```


:::

```{.r .cell-code}
sepal_length_emm <- emmeans(sepal_length_aov, ~ species)
pairs(sepal_length_emm)
```

::: {.cell-output .cell-output-stdout}

```
 contrast               estimate    SE  df t.ratio p.value
 setosa - versicolor      -0.930 0.103 147  -9.033  <.0001
 setosa - virginica       -1.582 0.103 147 -15.366  <.0001
 versicolor - virginica   -0.652 0.103 147  -6.333  <.0001

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::

```{.r .cell-code}
# Sepal Width comparisons
print("Sepal Width comparisons")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Sepal Width comparisons"
```


:::

```{.r .cell-code}
sepal_width_emm <- emmeans(sepal_width_aov, ~ species)
pairs(sepal_width_emm)
```

::: {.cell-output .cell-output-stdout}

```
 contrast               estimate     SE  df t.ratio p.value
 setosa - versicolor       0.658 0.0679 147   9.685  <.0001
 setosa - virginica        0.454 0.0679 147   6.683  <.0001
 versicolor - virginica   -0.204 0.0679 147  -3.003  0.0088

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::

```{.r .cell-code}
# Petal Length comparisons
print("Petal Length comparisons")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Petal Length comparisons"
```


:::

```{.r .cell-code}
petal_length_emm <- emmeans(petal_length_aov, ~ species)
pairs(petal_length_emm)
```

::: {.cell-output .cell-output-stdout}

```
 contrast               estimate     SE  df t.ratio p.value
 setosa - versicolor       -2.80 0.0861 147 -32.510  <.0001
 setosa - virginica        -4.09 0.0861 147 -47.521  <.0001
 versicolor - virginica    -1.29 0.0861 147 -15.012  <.0001

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::

```{.r .cell-code}
# Petal Width comparisons
print("Petal Width comparisons")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Petal Width comparisons"
```


:::

```{.r .cell-code}
petal_width_emm <- emmeans(petal_width_aov, ~ species)
pairs(petal_width_emm)
```

::: {.cell-output .cell-output-stdout}

```
 contrast               estimate     SE  df t.ratio p.value
 setosa - versicolor       -1.08 0.0409 147 -26.387  <.0001
 setosa - virginica        -1.78 0.0409 147 -43.489  <.0001
 versicolor - virginica    -0.70 0.0409 147 -17.102  <.0001

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::
:::











:::
:::::

# Canonical Discriminant Analysis: Eigenvalues

::::: columns
::: {.column width="60%"}
## Eigenvalues and Canonical Variates
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Perform canonical discriminant analysis
iris_candisc <- candisc(iris_manova_model)

# Display eigenvalues and canonical correlations
cat("Canonical Discriminant Analysis Results:\n\n")
```

::: {.cell-output .cell-output-stdout}

```
Canonical Discriminant Analysis Results:
```


:::

```{.r .cell-code}
print(iris_candisc)
```

::: {.cell-output .cell-output-stdout}

```

Canonical Discriminant Analysis for species:

   CanRsq Eigenvalue Difference  Percent Cumulative
1 0.96987   32.19193     31.907 99.12126     99.121
2 0.22203    0.28539     31.907  0.87874    100.000

Test of H0: The canonical correlations in the 
current row and all that follow are zero

  LR test stat approx F numDF denDF   Pr(> F)    
1      0.02344  199.145     8   288 < 2.2e-16 ***
2      0.77797   13.794     3   145 5.794e-08 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# Extract eigenvalues
eigenvalues <- iris_candisc$eigenvalues
cat("\nEigenvalues:\n")
```

::: {.cell-output .cell-output-stdout}

```

Eigenvalues:
```


:::

```{.r .cell-code}
print(eigenvalues)
```

::: {.cell-output .cell-output-stdout}

```
[1]  3.219193e+01  2.853910e-01 -7.801056e-17 -1.398429e-15
```


:::

```{.r .cell-code}
# Calculate proportion of variance explained
prop_variance <- eigenvalues / sum(eigenvalues)
cat("\nProportion of variance explained by each canonical variate:\n")
```

::: {.cell-output .cell-output-stdout}

```

Proportion of variance explained by each canonical variate:
```


:::

```{.r .cell-code}
print(prop_variance)
```

::: {.cell-output .cell-output-stdout}

```
[1]  9.912126e-01  8.787395e-03 -2.402001e-18 -4.305862e-17
```


:::

```{.r .cell-code}
# Cumulative proportion
cumulative_prop <- cumsum(prop_variance)
cat("\nCumulative proportion of variance explained:\n")
```

::: {.cell-output .cell-output-stdout}

```

Cumulative proportion of variance explained:
```


:::

```{.r .cell-code}
print(cumulative_prop)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.9912126 1.0000000 1.0000000 1.0000000
```


:::
:::











:::
:::::

# Canonical Discriminant Analysis: Coefficients

::::: columns
::: {.column width="60%"}
## Canonical Coefficients (Eigenvectors)
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Display canonical coefficients (eigenvectors)
cat("Raw Canonical Coefficients (Eigenvectors):\n")
```

::: {.cell-output .cell-output-stdout}

```
Raw Canonical Coefficients (Eigenvectors):
```


:::

```{.r .cell-code}
print(iris_candisc$coeffs.raw)
```

::: {.cell-output .cell-output-stdout}

```
                   Can1        Can2
sepal_length  0.8293776  0.02410215
sepal_width   1.5344731  2.16452123
petal_length -2.2012117 -0.93192121
petal_width  -2.8104603  2.83918785
```


:::

```{.r .cell-code}
cat("\nStandardized Canonical Coefficients:\n")
```

::: {.cell-output .cell-output-stdout}

```

Standardized Canonical Coefficients:
```


:::

```{.r .cell-code}
print(iris_candisc$coeffs.std)
```

::: {.cell-output .cell-output-stdout}

```
                   Can1        Can2
sepal_length  0.4269548  0.01240753
sepal_width   0.5212417  0.73526131
petal_length -0.9472572 -0.40103782
petal_width  -0.5751608  0.58103986
```


:::

```{.r .cell-code}
# Structure coefficients (correlations between original variables and canonical variates)
cat("\nStructure Coefficients (Variable-Canonical Variate Correlations):\n")
```

::: {.cell-output .cell-output-stdout}

```

Structure Coefficients (Variable-Canonical Variate Correlations):
```


:::

```{.r .cell-code}
print(iris_candisc$structure)
```

::: {.cell-output .cell-output-stdout}

```
                   Can1       Can2
sepal_length -0.7918878 0.21759312
sepal_width   0.5307590 0.75798931
petal_length -0.9849513 0.04603709
petal_width  -0.9728120 0.22290236
```


:::
:::











:::
:::::

# Multivariate Visualization

::::: columns
::: {.column width="60%"}
Multivariate Visualization
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Extract canonical scores using the correct method

# Alternative simpler approach - use lda from MASS package
# library(MASS)
iris_lda <- MASS::lda(species ~ sepal_length + sepal_width + petal_length + petal_width, data = iris_df)
lda_pred <- predict(iris_lda)

# Create dataframe with LDA scores (equivalent to canonical scores)
canonical_df_plot <- data.frame(
  Can1 = lda_pred$x[, 1],
  Can2 = lda_pred$x[, 2], 
  species = iris_df$species
)

# Calculate group centroids
centroids_plot <- canonical_df_plot %>%
  group_by(species) %>%
  summarise(Can1_mean = mean(Can1),
            Can2_mean = mean(Can2),
            .groups = 'drop')

# Create ggplot
canonical_df_plot %>%
  ggplot(aes(x = Can1, y = Can2, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = centroids_plot,
             aes(x = Can1_mean, y = Can2_mean, fill = species),
             shape = 23, color = "black", size = 8, stroke = 2) +
  labs(title = "Canonical Discriminant Analysis",
       subtitle = "Iris Species in Optimal Multivariate Space",
       x = "Linear Discriminant 1",
       y = "Linear Discriminant 2",
       color = "Species",
       fill = "Species") +
  theme_light() +
  theme(legend.position = "bottom")
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/multivariate-plots-1.png){width=672}
:::
:::











:::
:::::

# MANOVA Results: Key Interpretation

::::: columns
::: {.column width="60%"}
### Interpretation of MANOVA

## Key Interpretation

**Pillai's Trace (1.1919)**: This is large, indicating substantial group differences across the multivariate space.

**F-statistic (53.466)**: Very large F-value indicates strong evidence against the null hypothesis.

**P-value**: Essentially zero, meaning we reject the null hypothesis that all three species have the same multivariate means.

**Conclusion**: The three iris species are significantly different when considering all four morphological measurements simultaneously in multivariate space.
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# MANOVA Test Results
summary(iris_manova_model)
```

::: {.cell-output .cell-output-stdout}

```
           Df Pillai approx F num Df den Df    Pr(>F)    
species     2 1.1919   53.466      8    290 < 2.2e-16 ***
Residuals 147                                            
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::











:::
:::::

# MANOVA Results: Wilks' Lambda

::::: columns
::: {.column width="60%"}
### Interpretation of MANOVA

**Wilks' Lambda (0.023439)**: Very small value (close to 0) indicates:

-   Only about 2.3% of the total variance is unexplained by group differences
-   About 97.7% of the multivariate variance is explained by species differences
-   Extremely strong group separation in multivariate space

**Effect Size**: Partial η² ≈ 1 - 0.023439 = 0.977 (very large effect size)

**F-statistic (199.15)**: Much larger than Pillai's F-value because Wilks' Lambda is often more powerful when assumptions are met

**Conclusion**: The three iris species show extremely large multivariate differences - they are very well separated in the 4-dimensional morphological space, with species explaining nearly 98% of the multivariate variance.

**Wilks' vs Pillai's**: Wilks' Lambda is generally preferred when assumptions are met, while Pillai's trace is more robust to assumption violations.
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Get Wilks' Lambda from manova for effect size
manova_summary <- summary(iris_manova_model, test = "Wilks")
manova_summary
```

::: {.cell-output .cell-output-stdout}

```
           Df    Wilks approx F num Df den Df    Pr(>F)    
species     2 0.023439   199.15      8    288 < 2.2e-16 ***
Residuals 147                                              
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::











:::
:::::

# MANOVA Results: Effect Size

::::: columns
::: {.column width="60%"}
### Interpretation of MANOVA

**Meaning**: Approximately 97.7% of the total multivariate variance is explained by species differences.

**Effect Size Guidelines:** - **Small effect**: η² ≈ 0.01 (1% of variance explained) - **Medium effect**: η² ≈ 0.06 (6% of variance explained) - **Large effect**: η² ≈ 0.14 (14% of variance explained) - **Our result**: η² = 0.977 (**extremely large effect**)

**Practical Interpretation:** - Species are almost perfectly separated in multivariate morphological space - Only 2.3% of the variation in the four measurements is due to within-species differences - Species membership explains nearly all the multivariate variation - This represents one of the strongest group separations possible in real biological data

**Conclusion**: The iris species show dramatically different morphological profiles - they are essentially non-overlapping in the 4-dimensional space of sepal/petal measurements. This effect size indicates that species is an extremely powerful predictor of morphological characteristics.
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Effect Size (Partial Eta-squared approximation)
wilks_lambda <- manova_summary$stats[1, "Wilks"]
partial_eta_sq <- 1 - wilks_lambda
partial_eta_sq
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.9765614
```


:::
:::











:::
:::::

# Canonical Variates: Variance Explained

::::: columns
::: {.column width="60%"}
### Interpretation of manova

**Element \[1\] = 0.9912**: - First canonical variate explains 99.12% of the between-group variance - This dimension captures almost all the multivariate group differences

**Element \[2\] = 0.0088**: - Second canonical variate explains 0.88% of the between-group variance - This dimension captures the remaining small group differences

## Interpretation

**Dimensionality**: The group differences are essentially **one-dimensional** - 99% of separation occurs along the first canonical axis.

**Biological Meaning**: There's one primary "direction" in morphological space that best separates the three iris species, with a very minor secondary pattern.

**Practical Implication**: You could visualize almost all the group separation using just the first canonical variate, though plotting both dimensions shows the complete picture.
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Canonical Analysis Summary
prop_variance
```

::: {.cell-output .cell-output-stdout}

```
[1]  9.912126e-01  8.787395e-03 -2.402001e-18 -4.305862e-17
```


:::

```{.r .cell-code}
sum(prop_variance)
```

::: {.cell-output .cell-output-stdout}

```
[1] 1
```


:::
:::











:::
:::::

# Linear Discriminant Analysis: Detailed Results

::::: columns
::: {.column width="60%"}
### Interpretation of manova

Group means: - **Setosa**: Smallest overall, widest sepals, tiny petals - **Versicolor**: Medium-sized in most dimensions - **Virginica**: Largest overall, especially in petal dimensions - Clear size progression: setosa \< versicolor \< virginica

Coefficients of linear discriminants: - **LD1**: Positive weights for sepal measurements, negative for petal measurements - Separates small-petaled from large-petaled species - **LD2**: Mainly contrasts sepal width vs petal width - Fine-tunes separation between versicolor and virginica

Proportion of trace: - **LD1**: Explains 99.12% of between-group discrimination - **LD2**: Explains 0.88% of between-group discrimination - Confirms the separation is essentially one-dimensional (petal vs sepal contrast)
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# LDA results for interpretation
iris_lda
```

::: {.cell-output .cell-output-stdout}

```
Call:
lda(species ~ sepal_length + sepal_width + petal_length + petal_width, 
    data = iris_df)

Prior probabilities of groups:
    setosa versicolor  virginica 
 0.3333333  0.3333333  0.3333333 

Group means:
           sepal_length sepal_width petal_length petal_width
setosa            5.006       3.428        1.462       0.246
versicolor        5.936       2.770        4.260       1.326
virginica         6.588       2.974        5.552       2.026

Coefficients of linear discriminants:
                    LD1         LD2
sepal_length  0.8293776 -0.02410215
sepal_width   1.5344731 -2.16452123
petal_length -2.2012117  0.93192121
petal_width  -2.8104603 -2.83918785

Proportion of trace:
   LD1    LD2 
0.9912 0.0088 
```


:::
:::











:::
:::::

# MANOVA Advantages over Multiple ANOVAs

## Advantages of MANOVA over Multiple ANOVAs

### Statistical Advantages

-   Controls family-wise error rate (no need for Bonferroni correction)
-   Accounts for correlations between response variables
-   More powerful when variables are correlated
-   Tests the 'global' null hypothesis

### Interpretational Advantages

-   Reveals patterns in multivariate space that univariate tests miss
-   Canonical variates show optimal linear combinations for group separation
-   Provides insight into which variables work together to discriminate groups
-   Shows the dimensionality of group differences

### Biological Relevance

-   Organisms function as integrated wholes, not independent traits
-   Natural selection acts on trait combinations, not isolated traits
-   Multivariate approaches better reflect biological reality