---
title: "Principal Component Analysis (PCA)"
subtitle: "Understanding Multivariate Data with *Darlingtonia californica*"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
execute: 
  collapse: true
knitr:
  opts_chunk:
    paged.print: false
    collapse: true
format:
  html:
    output-file: "16_01_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  # RevealJS - UNCHANGED (keeps your two-column layout and large images)
  revealjs:
    output-file: "16_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "16_01_lecture_powerpoint.docx"
  pptx:
    output-file: "16_01_lecture_powerpoint.pptx"
---



# Introduction to Multivariate Analysis

::::: columns
::: {.column width="60%"}
### Why Study Multivariate Data?

-   In ecology, often measure many variables on the same organisms:
    -   **Plant morphology**: height, width, leaf size, flower size
    -   **Environmental conditions**: temperature, pH, nutrients
    -   **Species composition**: presence/absence, abundance
-   **Challenge**: How do we make sense of all these variables at once?
-   **PCA helps by**:
    -   Reducing many correlated variables to a few key dimensions
    -   Visualizing patterns in high-dimensional data
    -   Identifying the most important sources of variation
:::

::: {.column width="40%"}
Darlingtonia californica (Cobra Lily)

![image of a cobra lily Darlingtonia
californica](images/clipboard-3752708543.png){width="335"}
:::
:::::

# Our Study System: Darlingtonia

::::: columns
::: {.column width="60%"}
### The Cobra Lily Dataset

-   Analyze morphological measurements from 89 individual *Darlingtonia
    californica* plants collected from **4 sites** in the Siskiyou
    Mountains of Oregon.
-   **Variables measured** (10 total):
    -   **Morphological measurements (mm)**:
        -   height - Height of pitcher
        -   mouth_diam - Mouth diameter
        -   tube_diam - Tube diameter
        -   keel_diam - Keel diameter
        -   wing1_length, wing2_length
        -   Wing lengths (2 measurements)
        -   wingsprea - Wing spread
    -   **Biomass measurements (g)**:
        -   hoodmass_g - Hood mass

        -   tubemass_g - Tube mass

        -   wingmass_g - Wing mass
:::

::: {.column width="40%"}
```         
![Darlingtonia pitcher structure showing measured features\]
```

![](images/clipboard-4005369252.png){width="211"}

-   \*\*Sites\*\*:
-   TJH: T.J. Howell's Fen
-   DG: Day's Gulch
-   LEH: L.E. Hunter's Fen
-   HD: High Divide
:::
:::::

# Loading the Data

### Step 1: Import and Prepare the Data

::: panel

::: {.cell}

```{.r .cell-code}
# Load the data
darlingtonia <- read_csv("darlingtonia.csv")

# View the structure
head(darlingtonia)
## # A tibble: 6 × 11
##   site  height mouth_diam tube_diam keel_diam wing1_length wing2_length
##   <chr>  <dbl>      <dbl>     <dbl>     <dbl>        <dbl>        <dbl>
## 1 TJH      654       38.4      16.6       6.4           85           76
## 2 TJH      413       22.2      17.2       5.9           55           26
## 3 TJH      610       31.2      19.9       6.7           62           60
## 4 TJH      546       34.4      20.8       6.3           84           79
## 5 TJH      665       30.5      20.4       6.6           60           51
## 6 TJH      665       33.6      19.5       6.6           84           66
## # ℹ 4 more variables: wingsprea <dbl>, hoodmass_g <dbl>, tubemass_g <dbl>,
## #   wingmass_g <dbl>

darlingtonia %>%
  count(site, name = "n_plants")
## # A tibble: 4 × 2
##   site  n_plants
##   <chr>    <int>
## 1 DG          25
## 2 HD          12
## 3 LEH         25
## 4 TJH         25
```
:::

:::

# Exploring the Raw Data

::::: columns
::: {.column width="60%"}
## Initial Data Exploration

Before any analysis, we should **always look at our data**:

-   Check for outliers
-   Examine distributions
-   Look for patterns by site
-   Understand variable scales

**Key observations**:

-   Variables measured in different units (mm vs. g)
-   Different ranges: height (hundreds) vs. biomass (fractions)
-   Need to standardize before PCA!
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/explore_data-1.png){width=768}
:::
:::

:::
:::::

# The Problem of Different Scales

::::: columns
::: {.column width="60%"}
### Why Standardization Matters

-   **Without standardization**:
    -   Variables with larger values (like height in mm) would dominate
        the analysis over variables with smaller values (like biomass in
        g).
-   **Example from data**:
    -   Height: mean ≈ 600 mm, range 322-845 mm -
    -   Wing mass: mean ≈ 0.16 g, range 0.02-5.00 g
-   calculated distances without standardizing, height would have 1000×
    more influence than wing mass!
-   **Solution: Z-score standardization**
    -   $Z = \frac{(Y_i - \bar{Y})}{s}$
-   transforms each variable to:
    -   Mean = 0
    -   Standard deviation = 1
    -   Units: "standard deviations from the mean"
:::

::: {.column width="40%"}
-   **Before standardization:** - Height dominates (larger scale)
-   **After standardization:**
    -   All variables contribute equally
    -   Measured in "standard deviations"


::: {.cell}

```{.r .cell-code}
# Example: standardize height
darlingtonia %>%
  summarize(
    mean_height = mean(height),
    sd_height = sd(height),
    mean_wingmass = mean(wingmass_g),
    sd_wingmass = sd(wingmass_g)
  )
## # A tibble: 1 × 4
##   mean_height sd_height mean_wingmass sd_wingmass
##         <dbl>     <dbl>         <dbl>       <dbl>
## 1        616.      101.         0.154       0.102

# After standardization, both will have:
# mean = 0, sd = 1
```
:::

:::
:::::

# Measuring Multivariate Distance

::::: columns
::: {.column width="60%"}
### Euclidean Distance Between Individuals

-   How different are two plants?
    -   For two plants measured on multiple variables, calculate
        **Euclidean distance**:
        -   **For 2 variables** (height and spread):

            -   $d_{i,j} = \sqrt{(y_{i,1} - y_{j,1})^2 + (y_{i,2} - y_{j,2})^2}$

        -   **For n variables**:

            -   $d_{i,j} = \sqrt{\sum_{k=1}^{n}(y_{i,k} - y_{j,k})^2}$

        -   Pythagorean theorem extended to many dimensions!

        -   **Key point**: Distance should use *standardized* values so
            all variables contribute equally
:::

::: {.column width="40%"}



::: {.cell}

```
## 
## Distance between plants 1 and 2 (standardized):
##  3.53 standard deviations
## 
##  
## Note: Standardization uses mean and SD from ALL plants,
##  not just these two plants!
```
:::

:::
:::::

# Standardizing the Data

::::: columns
::: {.column width="60%"}
### Step 2: Z-score Transformation


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/standardize-1.png){width=480}
:::
:::

:::

::: {.column width="40%"}
### Visualizing Standardized Data


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/scaled_comparison-1.png){width=480}
:::
:::

:::
:::::

# Understanding Correlations

::::: columns
::: {.column width="60%"}
### Why Correlations Matter for PCA

-   PCA works best when variables are **correlated**:
    -   If variables are uncorrelated → PCA provides no benefit
    -   If variables are highly correlated → PCA can reduce dimensions
        effectively
-   **In data**, expect correlations because:
    -   Larger plants have larger measurements overall
    -   Wing dimensions likely correlate with each other
    -   Mass measurements correlate with size measurements
-   **The correlation matrix** shows relationships between all pairs of
    variables.
-   Strong correlations (positive or negative) suggest PCA will be
    useful!
:::

::: {.column width="40%"}
**Strong positive correlations** (dark blue) suggest variables measure
similar aspects of plant size/shape.


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/correlation_plot-1.png){width=576}
:::
:::

:::
:::::

# Correlation Matrix Details

### Examining Variable Relationships

::: panel

::: {.cell}

```
##              height mouth_diam tube_diam keel_diam wing1_length wing2_length
## height         1.00       0.61      0.24     -0.06         0.28         0.29
## mouth_diam     0.61       1.00     -0.05     -0.39         0.57         0.44
## tube_diam      0.24      -0.05      1.00      0.65        -0.09         0.01
## keel_diam     -0.06      -0.39      0.65      1.00        -0.35        -0.24
## wing1_length   0.28       0.57     -0.09     -0.35         1.00         0.84
## wing2_length   0.29       0.44      0.01     -0.24         0.84         1.00
## wingsprea      0.12       0.25      0.10     -0.19         0.64         0.71
## hoodmass_g     0.49       0.76     -0.11     -0.34         0.61         0.49
## tubemass_g     0.84       0.72      0.10     -0.24         0.44         0.36
## wingmass_g     0.40       0.62     -0.16     -0.36         0.76         0.69
##              wingsprea hoodmass_g tubemass_g wingmass_g
## height            0.12       0.49       0.84       0.40
## mouth_diam        0.25       0.76       0.72       0.62
## tube_diam         0.10      -0.11       0.10      -0.16
## keel_diam        -0.19      -0.34      -0.24      -0.36
## wing1_length      0.64       0.61       0.44       0.76
## wing2_length      0.71       0.49       0.36       0.69
## wingsprea         1.00       0.25       0.16       0.39
## hoodmass_g        0.25       1.00       0.76       0.80
## tubemass_g        0.16       0.76       1.00       0.63
## wingmass_g        0.39       0.80       0.63       1.00
```
:::

:::

# Key Observations

::::: columns
::: {.column width="50%"}
-   **Highly correlated variables**:
    -   Height with tube area (r=0.89)
        -   Wing lengths with each other (r=0.80)
        -   Wing area with wing spread (r=0.81)
-   These correlations indicate **redundancy**
-   multiple variables measuring similar things.
:::

::: {.column width="50%"}
-   **Moderate correlations**:
    -   Most morphological variables
    -   Mass measurements with size
-   **Lower correlations**:
    -   Some specific features like keel diameter
-   **PCA will help** by combining correlated variables into fewer
    components!
:::
:::::

# What is Principal Component Analysis?

::::: columns
::: {.column width="60%"}
### The Concept of PCA

-   PCA creates new variables (components) that are:
    -   **Linear combinations** of original variables
    -   **Uncorrelated** with each other
    -   **Ordered by importance** (amount of variance explained)
-   **The mathematical idea**:
    -   For each plant *i* and component *k*:
    -   $Z_{ik} = a_{i1}Y_1 + a_{i2}Y_2 + ... + a_{in}Y_n$
-   Where:
    -   $Z_{ik}$ = score on principal component *k*
    -   $Y_j$ = standardized value of original variable *j*
    -   $a_{ij}$ = loading (weight) of variable *j* on component *i*
-   **Goal**: Find loadings that maximize variance in first component,
    then second, etc.
:::

::: {.column width="40%"}
![https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/GaussianScatterPCA.svg/500px-GaussianScatterPCA.svg.png](images/clipboard-2517015273.png){width="246"}\
Conceptual diagram of PCA: reducing 2 variables to 1 principal axis

-   **The first principal axis** passes through maximum variance.
-   **Key properties**: - As many components as variables - First
    components explain most variation - Later components explain
    residual variation
:::
:::::

# Principal Components: Properties

::::: columns
::: {.column width="50%"}
### Understanding Components are:

-   **1. Orthogonal (perpendicular)**
    -   Components are uncorrelated
    -   Each captures independent information
-   **2. Ordered by variance**
    -   PC1 explains the most variance
    -   PC2 explains second most (of remaining variance)
    -   And so on...
-   **3. Linear combinations**
    -   Each component is a weighted sum of original variables
    -   Weights (loadings) show variable importance
:::

::: {.column width="50%"}
### Why this matters:

-   **Dimension reduction**:
    -   Start with 10 correlated variables
    -   End with 2-3 uncorrelated components
    -   Retain most information with fewer dimensions
-   **Easier interpretation**:
    -   PC1 might represent "overall size"
    -   PC2 might represent "shape"
    -   Components often have biological meaning
-   **Better for analysis**:
    -   Can use in regression, ANOVA
    -   No multicollinearity problems
    -   Variables are standardized
:::
:::::

# Performing PCA in R

### Step 3: Run the Analysis

::: panel

::: {.cell}

```{.r .cell-code}
# Perform PCA using prcomp()
# center = TRUE: center variables (mean = 0)
# scale. = TRUE: scale variables (sd = 1)
pca_result <- prcomp(darl_numeric, 
                     center = TRUE, 
                     scale = TRUE)

# What's in the result?
names(pca_result)
## [1] "sdev"     "rotation" "center"   "scale"    "x"
```
:::

:::

# Understanding the Output

::: panel

::: {.cell}

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     2.2332 1.3288 1.2354 0.75701 0.59301 0.51062 0.46177
## Proportion of Variance 0.4987 0.1766 0.1526 0.05731 0.03517 0.02607 0.02132
## Cumulative Proportion  0.4987 0.6753 0.8279 0.88521 0.92038 0.94645 0.96777
##                           PC8     PC9    PC10
## Standard deviation     0.3768 0.33982 0.25457
## Proportion of Variance 0.0142 0.01155 0.00648
## Cumulative Proportion  0.9820 0.99352 1.00000
```
:::


-   **Output components**:
-   `sdev`: standard deviations of each PC (square root of eigenvalues)
-   `rotation`: loadings matrix (how variables load on PCs)
-   `x`: PC scores for each observation
-   `center`: variable means
-   `scale`: variable standard deviations
:::

# Eigenvalues and Variance Explained

::::: columns
::: {.column width="60%"}
### How Much Variance Does Each PC Explain?

-   **Eigenvalues** ($\lambda$) measure variance explained by each
    component.
-   The proportion of variance for PC *j* is:
    -   $\text{Proportion}_j = \frac{\lambda_j}{\sum_{k=1}^{n}\lambda_k}$
-   **In practice**:
    -   PC1 always explains the most variance

    -   We want the first few PCs to explain most (\>70%) of total
        variance

    -   allows dimension reduction
:::

::: {.column width="40%"}

::: {.cell}

```
## # A tibble: 10 × 4
##    PC    Eigenvalue Proportion Cumulative
##    <chr>      <dbl>      <dbl>      <dbl>
##  1 PC1        4.99       0.499      0.499
##  2 PC2        1.77       0.177      0.675
##  3 PC3        1.53       0.153      0.828
##  4 PC4        0.573      0.057      0.885
##  5 PC5        0.352      0.035      0.92 
##  6 PC6        0.261      0.026      0.946
##  7 PC7        0.213      0.021      0.968
##  8 PC8        0.142      0.014      0.982
##  9 PC9        0.115      0.012      0.994
## 10 PC10       0.065      0.006      1
```
:::

:::
:::::

# Scree Plot: Choosing Components

::::: columns
::: {.column width="60%"}
### How Many Components to Use?

-   A **scree plot** shows variance explained by each component.
-   **How to read it**:
    -   Look for an "elbow" - sharp change in slope
    -   Keep components before the elbow
    -   Discard components in the "scree" (rubble)
-   **In this data**:
    -   PC1 explains 49.9% of variance
    -   PC2 explains 17.7%
    -   PC3 explains 15.3%
    -   **First 3 PCs explain 77.3% of variance**
-   **Decision**: Use PC1, PC2, and PC3 for interpretation.
-   remaining 7 components explain 17.1% (mostly noise).
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/scree_plot-1.png){width=576}
:::
:::

:::
:::::

# Understanding Loadings

::::: columns
::: {.column width="60%"}
### What are Loadings?

-   **Loadings** are the weights ($a_{ij}$) that show how each original
    variable contributes to each principal component.
-   **For PC1**:
    -   $Z_1 = a_{11}Y_1 + a_{12}Y_2 + ... + a_{1p}Y_p$
-   **Interpreting loadings**:
    -   **Large positive loading**: variable increases with PC
    -   **Large negative loading**: variable decreases with PC
    -   **Near-zero loading**: variable unimportant for PC
-   **Guidelines**:
    -   Loadings \> 0.3 or \< -0.3 are worth considering
    -   Loadings \> 0.5 or \< -0.5 are important
    -   Sign (+ or -) can flip; patterns matter more
:::

::: {.column width="40%"}
-   **PC1 loadings**:
    -   Most variables have similar positive values
    -   All \~0.31 to 0.40 except keel (-0.18) and tube (-0.002)
    -   Suggests PC1 measures "overall size"


::: {.cell}

```
##                 PC1    PC2    PC3
## height       -0.278 -0.449  0.198
## mouth_diam   -0.369 -0.110  0.214
## tube_diam     0.040 -0.602 -0.390
## keel_diam     0.202 -0.491 -0.328
## wing1_length -0.375  0.171 -0.280
## wing2_length -0.342  0.131 -0.415
## wingsprea    -0.239  0.156 -0.549
## hoodmass_g   -0.385 -0.057  0.185
## tubemass_g   -0.355 -0.320  0.263
## wingmass_g   -0.393  0.080 -0.007
```
:::

:::
:::::

# Interpreting PC1: Overall Size

::::: columns
::: {.column width="60%"}
-   PC1 Interpretation - Looking at PC1 loadings:
-   **Key patterns**:
-   Most variables load positively and similarly: - Height: 0.310 -
    Mouth diameter: 0.400
    -   Wing lengths: 0.386, 0.372
    -   Wing spread: 0.256
    -   Biomass: 0.397, 0.380, 0.230
-   **Exception**: Keel diameter (-0.177) and tube diameter (\~0)
-   **Biological meaning**: PC1 represents **overall plant size**
    -   most measurements scale together (bigger plants have bigger
        everything).


::: {.cell}

```
## # A tibble: 10 × 2
##    Variable     Loading
##    <chr>          <dbl>
##  1 wingmass_g   -0.393 
##  2 hoodmass_g   -0.385 
##  3 wing1_length -0.375 
##  4 mouth_diam   -0.369 
##  5 tubemass_g   -0.355 
##  6 wing2_length -0.342 
##  7 height       -0.278 
##  8 wingsprea    -0.239 
##  9 keel_diam     0.202 
## 10 tube_diam     0.0402
```
:::

:::

::: {.column width="40%"}
-   High PC1 → large plants
-   Low PC1 → small plants


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/pc1_loading_plot-1.png){width=384}
:::
:::

:::
:::::

# Interpreting PC2: Shape Contrast

::::: columns
::: {.column width="60%"}
### PC2 Interpretation

-   Looking at PC2 loadings:
-   **Key patterns**:
    -   Height: -0.419 (negative, moderate)
    -   Mouth: -0.250 (negative, moderate)
    -   Wing measurements: positive (0.275-0.434)
    -   Wing spread: 0.434 (positive, strong)
    -   Biomass: mostly negative
-   **Biological meaning**: PC2 represents **shape trade-offs**
    -   tall narrow pitchers vs. short wide pitchers with large wings.


::: {.cell}

```
## # A tibble: 10 × 2
##    Variable     Loading
##    <chr>          <dbl>
##  1 tube_diam    -0.602 
##  2 keel_diam    -0.491 
##  3 height       -0.449 
##  4 tubemass_g   -0.320 
##  5 wing1_length  0.171 
##  6 wingsprea     0.156 
##  7 wing2_length  0.131 
##  8 mouth_diam   -0.110 
##  9 wingmass_g    0.0800
## 10 hoodmass_g   -0.0575
```
:::

:::

::: {.column width="40%"}
-   High PC2 → short plants with large wings
-   Low PC2 → tall narrow plants


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/pc2_loading_plot-1.png){width=480}
:::
:::

:::
:::::

# Interpreting PC3

::::: columns
::: {.column width="60%"}
### PC3 Interpretation

-   Looking at PC3 loadings:
-   **Key patterns**:
-   Dominated by:
    -   Tube diameter: 0.743 (very strong positive)
    -   Keel diameter: 0.576 (strong positive)
    -   Most other variables near zero
-   **Biological meaning**: PC3 represents **tube girth**
    -   width/diameter of tube structure independent of height or wing
        size.
    -   Plants with high PC3 scores = **fat tubes**, allowing to trap
        larger prey?


::: {.cell}

```
## # A tibble: 10 × 2
##    Variable      Loading
##    <chr>           <dbl>
##  1 wingsprea    -0.549  
##  2 wing2_length -0.415  
##  3 tube_diam    -0.390  
##  4 keel_diam    -0.328  
##  5 wing1_length -0.280  
##  6 tubemass_g    0.263  
##  7 mouth_diam    0.214  
##  8 height        0.198  
##  9 hoodmass_g    0.185  
## 10 wingmass_g   -0.00743
```
:::

:::

::: {.column width="40%"}
-   High PC3 → fat tubes
-   Low PC3 → skinny tubes


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/pc3_loading_plot-1.png){width=480}
:::
:::

:::
:::::

# Principal Component Scores

::::: columns
::: {.column width="50%"}
### What are PC Scores?

-   **PC scores** are values of each principal component for each
    obs./plant
-   Each plant gets a score on each PC:
    -   PC1 score = measure of size
    -   PC2 score = measure of shape
    -   PC3 score = measure of tube girth
-   **How they're calculated**:
-   For plant *i* on PC1:
    -   $\text{PC1}_i = 0.31×height_i + 0.40×mouth_i + ...$
    -   Using the standardized values and loadings we saw earlier.
:::

::: {.column width="50%"}

::: {.cell}

```
## # A tibble: 10 × 5
##    plant_id site     PC1     PC2    PC3
##       <int> <chr>  <dbl>   <dbl>  <dbl>
##  1        1 TJH   -1.92   0.0772  1.53 
##  2        2 TJH    3.35   1.53    0.755
##  3        3 TJH    0.918 -0.0320  0.248
##  4        4 TJH   -0.983  0.236  -0.242
##  5        5 TJH    1.15  -1.02    1.54 
##  6        6 TJH   -1.37  -0.613   0.847
##  7        7 TJH   -1.50  -1.63    0.693
##  8        8 TJH   -0.651  0.121  -0.412
##  9        9 TJH    2.48  -0.0786 -0.260
## 10       10 TJH   -1.11  -1.27    2.27
```
:::

:::
:::::

# Visualizing PCA Results: Scores Plot

::::: columns
::: {.column width="60%"}
### Ordination of Sites

-   We can plot PC scores to visualize:
    -   How plants are distributed in PC space
    -   Whether sites differ in morphology
    -   Patterns and groupings
-   **In this plot**:
    -   Each point = one plant
    -   Position determined by PC1 and PC2 scores
    -   Colors indicate collection site
-   **Observations**:
    -   Sites show some separation
    -   HD (orange) plants have lower PC1 (smaller)
    -   TJH (pink) plants variable but lower PC2
    -   Some overlap among sites
-   This is called **ordination** - ordering objects along axes.
:::

::: {.column width="40%"}
The ordination


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/scores_plot_basic-1.png){width=576}
:::
:::

:::
:::::

# Different visualization of PCA Scores

::: panel

::: {.cell}
::: {.cell-output-display}

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-c03f81d64294e892cb28" style="width:100%;height:650px;"></div>
<script type="application/json" data-for="htmlwidget-c03f81d64294e892cb28">{"x":{"visdat":{"2ea53f0ecf6e":["function () ","plotlyVisDat"]},"cur_data":"2ea53f0ecf6e","attrs":{"2ea53f0ecf6e":{"x":{},"y":{},"z":{},"mode":"markers","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter3d"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"scene":{"xaxis":{"title":"PC1"},"yaxis":{"title":"PC2"},"zaxis":{"title":"PC3"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[-2.2417084302101471,-3.1488111996745194,-0.031357247571229749,-1.6292262054838647,0.1786488794736231,-2.8137205962749712,-1.5136271507468295,0.48047835290602364,-2.423558155889308,-2.4207859653813832,-1.7134269393451567,-2.836543931789175,1.2828622649504531,-0.67781996575422798,-2.3395163552335601,-1.9577431510255821,1.1390722661876607,-1.1455578611843449,-1.3783995382486094,0.10266145762430612,0.51843584966760015,1.3830217303046739,-0.094323016367073437,-0.9243184682774982,-1.4055358982329349],"y":[-0.095831265119555031,-0.17938019412960352,-1.2637405550382284,0.12719197448844013,0.68120234770657362,-1.2201747859334842,0.15017946717123298,1.9063329825517348,-0.65192516841892589,1.9405292592885268,0.83075710209431686,-0.061505549807053091,1.0450338094306293,1.575728764276918,-2.1115894796080559,2.3799587171442416,0.60251064373720142,0.75510383660089575,2.1865585469030591,0.78706210460312864,0.9738371677156501,2.172023145976024,1.2321344985110791,0.96095079415852502,0.37764596968802761],"z":[0.098742836455033831,-1.0343461335030324,2.0761936135999344,0.74632879266233798,0.41890583932227665,-0.30478111450845813,0.44104000807534605,-0.81898748834020629,1.5251514698526034,-0.49142084414175485,-0.23363784478466904,-0.040276923734024764,-0.72612091951923952,-0.83338789604054342,2.8846790041299277,0.56426987795403882,-0.50888350106630253,0.18713023961205283,1.4029812725704285,1.3836929474690498,1.2305002449427296,2.2340039565314482,0.913524213517098,1.619824624356351,0.81480896389273916],"mode":"markers","type":"scatter3d","name":"DG","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"frame":null},{"x":[3.0695543372075615,2.5698291790774683,3.4152914347624499,1.5250888497266195,1.6546722227528465,3.6137024574162835,1.608783560571601,2.9781234964510688,3.3834367255909426,3.7796218062665559,1.6647119613379064,2.2522500324053354],"y":[-1.0747707829442761,-1.916299100291204,0.90516782805261642,-4.2391205309431124,-0.66142974558611656,-1.1577323280076015,0.18773179313927399,-1.4975315584230398,-0.54356104351226375,-1.3005775081041899,-0.6206374277052209,1.5636573250450565],"z":[-1.3691542016650275,-1.4902359590376686,-0.84073587601482902,-0.43420286112158346,-2.52049421702353,-2.9843543192027728,-2.499866747429516,-1.3830146500820604,-1.4497714496960943,0.46288596754746719,-0.68468238939608228,0.065482850202375709],"mode":"markers","type":"scatter3d","name":"HD","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"line":{"color":"rgba(252,141,98,1)"},"frame":null},{"x":[3.3893861534747325,0.32951405185554217,0.080752236782147033,-2.2592753167821908,-2.9118986242159335,-1.7227211355727985,0.030878093641676565,0.79998341692505348,-0.18106883719102052,-1.1378224537895203,-0.11528437599363452,0.20649941380454426,3.5783094953764865,0.40358824121884179,1.1328890334867503,1.6902086874735258,-0.23799959809853619,2.3465745379848815,0.17780660427383929,-0.7578615384730174,0.22733493991378589,-4.1956970001066169,-5.9251916806860763,-7.0006379895354138,-4.4201288004318666],"y":[0.31187931289457976,-0.5784601496578573,-0.58292140027887396,1.7483418286793235,1.4511768294798162,-0.23976890403647216,-0.027181811234002977,0.82575778335349659,-1.3156607250662988,-0.28511180925329788,0.56526892624012437,-2.1016207483400078,2.9157078187512533,-0.36433496097997581,0.58249871502575712,3.1893384058039498,-2.0520950294853262,0.92397660743579024,0.79459254413788083,0.69339765095368833,0.021499730336817909,0.6170064968665292,1.1290868542094086,-0.58144928437060872,-3.7682581555512797],"z":[0.29110352965563102,0.62313754959790046,1.4972708501513416,-0.62260520991658508,-1.5334043272083979,0.76303017903471015,1.5970137288248027,0.47311858257850609,-1.9360968667375273,-0.30806535603275553,-0.71780570926704457,-0.30165404405928486,-0.14834176675419791,-0.38756479256775195,-1.5089943923020754,0.23138974544181889,-1.4989137648530444,-1.2715372904046374,-1.0987584815992133,-1.1516321867704029,-1.153005028950064,0.26291950309547868,-3.1047473327126549,-2.0740832353846153,-0.80305585146950831],"mode":"markers","type":"scatter3d","name":"LEH","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"line":{"color":"rgba(141,160,203,1)"},"frame":null},{"x":[-1.9215219426413916,3.349998803081184,0.91827709837975502,-0.98257870519661417,1.1500885487363426,-1.3714136655991702,-1.5044504781464785,-0.65128080835888058,2.4769129137958559,-1.1086700213034906,2.8785450826012289,-1.8186796099754226,-3.8769412473540275,-0.98706777065364215,0.89125024275521114,2.3898893935763699,-0.88544150326496052,0.73556658257246421,1.8933598123170241,0.40339776062422583,-0.45273522722207948,1.6260513886731716,3.3702739742634957,2.0179804628088385,2.0267845722053019],"y":[0.077198456418084122,1.530840621609352,-0.032013230717069213,0.2361604666669645,-1.0237391318751681,-0.61295599331081529,-1.6319655460983491,0.12102596107119297,-0.078642920265715441,-1.2739075321409774,0.24118629629930863,0.10930924058571036,-0.75802845023592746,-0.75638619559834286,-0.34953006493301864,1.1803608638928509,-0.37077628525753031,0.63643038793810869,-0.13498245826379698,-0.3579357883777981,-0.18311125105099896,-1.689954109345301,-2.7926499919267327,0.56695837770330781,-1.2710493034129655],"z":[1.5264877253014526,0.75470164647235538,0.24750403494827161,-0.24238412638514986,1.5372036163838716,0.84661071258278897,0.69262951931478067,-0.41170574097695534,-0.26039969437698512,2.2702913843488313,0.81350601338027095,-0.54076718980083716,-0.24119783839284598,1.5041223875831566,1.3644732863094289,1.4471270444656925,1.1607980074510738,-0.86373545579863842,0.12636711095476882,0.64144935990648788,0.30557502071889975,0.025175590948727483,1.7186316449570029,-0.53991037678088438,1.5769368987081747],"mode":"markers","type":"scatter3d","name":"TJH","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"line":{"color":"rgba(231,138,195,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

:::
:::

:::

# PCA Biplot: Combining Scores and Loadings

::::: columns
::: {.column width="60%"}
### Reading a Biplot

-   A **biplot** shows both:
    -   **Points** = plants (PC scores)
    -   **Arrows** = original variables (loadings)
-   **How to interpret arrows**:
-   **Length** = how important the variable is
-   **Direction** = how the variable relates to PCs
-   **Angle between arrows** = correlation
    -   Small angle = positively correlated
    -   90° = uncorrelated
    -   180° = negatively correlated
-   **How to interpret points**:
    -   Plants in the direction of an arrow are high in that variable
    -   Plants opposite an arrow are low in that variable
:::

::: {.column width="40%"}
The plot


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/biplot-1.png){width=576}
:::
:::

:::
:::::

# Biplot Interpretation

::::: columns
::: {.column width="50%"}
### Key Patterns in the Biplot

-   **Variable relationships**:
    -   Most morphological and biomass variables point together (right
        and slightly down):
        -   Height, mouth, masses - These are highly correlated

        -   All contribute to PC1 (size)
-   Wing variables (wing1_length, wing2_length, wingspread) point more
    upward:
    -   Contribute positively to PC2
        -   Represent the "wing dimension" of shape
-   **Site separation**:
    -   **HD (blue)**: Lower PC1, plants are smaller overall

    -   **DG (orange)**: Higher PC1, plants are larger

    -   **TJH (yellow)** and **LEH (green)**: Intermediate and variable
:::

::: {.column width="50%"}
### Biological Interpretation

-   **What determines plant morphology?**
    -   **Size** (PC1, 45.8% variance)
        -   Primary source of variation

        -   Most measurements scale together

        -   May reflect resource availability or age
    -   **Shape trade-offs** (PC2, 16.7% variance)
        -   Tall narrow vs. short wide with large wings
        -   May reflect different trapping strategies
        -   Or developmental/environmental constraints
    -   **Tube dimensions** (PC3, 14.8% variance)
        -   Independent aspect of morphology
        -   May affect prey capture efficiency
    -   **Sites differ** primarily in overall size, with some shape
        variation.
:::
:::::

# Testing Site Differences with ANOVA

### Step 4: Statistical Analysis

-   Now that we have PC scores, we can test if sites differ:
    -   **Result**: Sites differ significantly in PC1 (F = 9.26, P \<
        0.001)

::: panel

::: {.cell}

```{.r .cell-code}
# Test for site differences on PC1 (size)
anova_pc1 <- aov(PC1 ~ site, data = pc_scores)
summary(anova_pc1)
##             Df Sum Sq Mean Sq F value   Pr(>F)    
## site         3  124.3   41.44   11.29 2.75e-06 ***
## Residuals   83  304.6    3.67                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Calculate means by site
pc_scores %>%
  group_by(site) %>%
  summarize(
    mean_PC1 = mean(PC1),
    sd_PC1 = sd(PC1),
    n = n()
  )
## # A tibble: 4 × 4
##   site  mean_PC1 sd_PC1     n
##   <chr>    <dbl>  <dbl> <int>
## 1 DG      -1.02   1.38     25
## 2 HD       2.63   0.857    12
## 3 LEH     -0.659  2.61     25
## 4 TJH      0.423  1.90     25
```
:::

:::

# ANOVA for PC2 and PC3

### Testing Shape and Tube Differences

-   **Results**:
    -   PC2 (shape): F = 5.36, P = 0.002 (significant)
        -   PC3 (tube girth): F = 1.45, P = 0.234 (not significant)

::: panel

::: {.cell}

```{.r .cell-code}
# Test for site differences on PC2 (shape)
anova_pc2 <- aov(PC2 ~ site, data = pc_scores)
summary(anova_pc2)
##             Df Sum Sq Mean Sq F value  Pr(>F)   
## site         3  21.63   7.209   4.595 0.00502 **
## Residuals   83 130.22   1.569                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Test for site differences on PC3 (tube girth)
anova_pc3 <- aov(PC3 ~ site, data = pc_scores)
summary(anova_pc3)
##             Df Sum Sq Mean Sq F value  Pr(>F)    
## site         3  43.68  14.561    13.8 2.2e-07 ***
## Residuals   83  87.58   1.055                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Summary statistics
pc_scores %>% group_by(site) %>% summarize(mean_PC2 = mean(PC2),mean_PC3 = mean(PC3))
## # A tibble: 4 × 3
##   site  mean_PC2 mean_PC3
##   <chr>    <dbl>    <dbl>
## 1 DG       0.604    0.542
## 2 HD      -0.863   -1.26 
## 3 LEH      0.155   -0.555
## 4 TJH     -0.345    0.618
```
:::

:::

# Visualizing Site Differences

::::: columns
::: {.column width="50%"}
### Box Plots of PC Scores

**HD plants are significantly smaller** than plants at other sites.


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/boxplots_pc1-1.png){width=576}
:::
:::

:::

::: {.column width="50%"}
-   **TJH plants have different shapes**

    -   taller and narrower with smaller wings.


    ::: {.cell}
    ::: {.cell-output-display}
    ![](16_01_lecture_powerpoint_files/figure-html/boxplots_pc2-1.png){width=576}
    :::
    :::

:::
:::::

# Post-hoc Comparisons

### Which Sites Differ?

::: panel

::: {.cell}

```
##  contrast  estimate    SE df t.ratio p.value
##  DG - HD     -3.651 0.673 83  -5.427  <.0001
##  DG - LEH    -0.366 0.542 83  -0.675  0.9064
##  DG - TJH    -1.447 0.542 83  -2.671  0.0442
##  HD - LEH     3.285 0.673 83   4.883  <.0001
##  HD - TJH     2.204 0.673 83   3.276  0.0082
##  LEH - TJH   -1.082 0.542 83  -1.996  0.1977
## 
## P value adjustment: tukey method for comparing a family of 4 estimates
##  site emmean    SE df lower.CL upper.CL .group
##  DG   -1.024 0.383 83   -1.786   -0.262  a    
##  LEH  -0.659 0.383 83   -1.421    0.103  ab   
##  TJH   0.423 0.383 83   -0.339    1.185   b   
##  HD    2.626 0.553 83    1.526    3.726    c  
## 
## Confidence level used: 0.95 
## P value adjustment: tukey method for comparing a family of 4 estimates 
## significance level used: alpha = 0.05 
## NOTE: If two or more means share the same grouping symbol,
##       then we cannot show them to be different.
##       But we also did not show them to be the same.
```
:::

:::

# Distance Between Sites

::::: columns
::: {.column width="60%"}
### Calculating Euclidean Distance Between Groups

-   Calculate the distance between site centroids (mean PC scores):
    -   $d_{k,j} = \sqrt{\sum_{i=1}^{p}(\bar{Z}_{i,k} - \bar{Z}_{i,j})^2}$
    -   Where $\bar{Z}_{i,k}$ is the mean of PC *i* for site *k*.
-   **Using all PCs** preserves the true Euclidean distance from the
    original data.
-   **Using just PC1-PC3** (77% of variance) gives approximate
    distances.
:::

::: {.column width="40%"}
**HD is most different** from other sites (smallest plants).


::: {.cell}

```{.r .cell-code}
# Calculate site centroids (means)
site_means <- pc_scores %>%
  group_by(site) %>%
  summarize(across(PC1:PC3, mean))

site_means
## # A tibble: 4 × 4
##   site     PC1    PC2    PC3
##   <chr>  <dbl>  <dbl>  <dbl>
## 1 DG    -1.02   0.604  0.542
## 2 HD     2.63  -0.863 -1.26 
## 3 LEH   -0.659  0.155 -0.555
## 4 TJH    0.423 -0.345  0.618

# Calculate distance matrix
site_dist <- site_means %>%
  dplyr::select(PC1, PC2, PC3) %>%
  dist() %>%
  round(3)
site_dist
##       1     2     3
## 2 4.328            
## 3 1.241 3.511      
## 4 1.732 2.942 1.672
```
:::

:::
:::::

# Visualization of the distances

::: panel

::: {.cell}

```{.r .cell-code}
# 1. Prepare PC Scores with Site
pc_scores_for_dist <- as.data.frame(pca_result$x) %>%
  mutate(site = darlingtonia$site)

# 2. Calculate the mean PC scores for each site (the site "centroids")
site_centroids <- pc_scores_for_dist %>%
  group_by(site) %>%
  summarise(across(starts_with("PC"), mean)) %>%
  ungroup()

# 3. Set site names as row names BEFORE calculating distances
site_centroids_matrix <- site_centroids %>%
  column_to_rownames("site")  # This keeps site names as row names

# 4. Calculate the Euclidean distance matrix between site centroids
dist_matrix <- dist(site_centroids_matrix, method = "euclidean")

# Display the distance matrix
dist_matrix
##           DG       HD      LEH
## HD  4.346434                  
## LEH 1.621198 3.659711         
## TJH 1.843120 3.021386 1.960547

# Visualize the distance matrix
fviz_dist(dist_matrix, 
          gradient = list(low = "#00AFBB", high = "#FC4E07"))
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/euclidian distances-1.png){width=480}
:::
:::

:::

# Summary: PCA Results for Darlingtonia

::::: columns
::: {.column width="50%"}
### What We Learned

-   Dimension Reduction Success
    -   [x] **Started with**: 10 correlated variables\
    -   [x] **Reduced to**: 3 principal components\
    -   [x] **Retained**: 77.3% of variance
-   This makes data **easier to visualize, interpret, and analyze**.
-   Biological Insights - **Three major axes of morphological
    variation**: - **PC1 (45.8%)**: Overall size
    -   Larger vs. smaller plants
    -   HD site has smallest plants - **PC2 (16.7%)**: Shape contrast
    -   Tall/narrow vs. short/wide with large wings
    -   TJH site has taller, narrower plants - **PC3 (14.8%)**: Tube
        girth
    -   Fat vs. skinny tubes
    -   No significant site differences
:::

::: {.column width="50%"}
-   Statistical Findings
    -   **Sites differ significantly** in size (PC1)
    -   **Sites differ significantly** in shape (PC2)
    -   **Sites do not differ** in tube girth (PC3)
-   PCA Advantages
    -   [x] Reduced 10 dimensions to 3\
    -   [x] Created uncorrelated variables\
    -   [x] Revealed biological patterns\
    -   [x] Enabled statistical testing\
    -   [x] Simplified visualization
-   Key Take-Home Messages
    -   PCA good for **exploratory analysis** of multivariate data
    -   Always **standardize** when different units/scales
    -   **Loadings** reveal variables contributions to each component
    -   **Scores** used in subsequent analyses (ANOVA, regression)
    -   **Biplots** visualize both samples and variables simultaneously
:::
:::::

# When to Use PCA

-   PCA is Appropriate When:
    -   [x] Variables are **continuous** (not categorical)
    -   [x] Variables are **correlated** (redundancy exists) + **Linear
        relationships**
    -   [x] Goal is **dimension reduction** or **data exploration**
    -   [x] **Sample size** adequate (at least 3× number of variables)
-   PCA Assumptions:
    -   **Linearity**: relationships between variables are linear
    -   **Adequate correlation**: variables must be correlated
    -   **No severe outliers**: can distort results
    -   **Sample size**: need enough observations
-   Always Check:
    -   Correlation matrix (are variables correlated?)
    -   Scree plot (how many components needed?)
    -   Loadings (biological interpretation makes sense?)
    -   Outliers (check scores plots)

# When to **Consider Alternatives:**

-   **Different data types**:
    -   Categorical variables → MCA (Multiple
    -   Correspondence Analysis) - Species composition data → CA
        (Correspondence Analysis)
    -   Distance/dissimilarity matrices → PCoA (Principal Coordinates
        Analysis)
-   **Different goals**: - Classification → Discriminant Analysis
    -   Non-linear patterns → NMDS (Non-metric Multidimensional Scaling)
    -   Hypothesis testing → MANOVA
-   **Problems with PCA**:
    -   Strong outliers present
    -   Non-linear relationships
    -   Variables are uncorrelated
    -   Many zeros in data
-   **PCA works best** for morphological, physiological, and
    environmental variables!

# Additional PCA Diagnostics

::::: columns
::: {.column width="50%"}
### Checking Assumptions and Quality

1.  Sampling Adequacy

-   **Kaiser-Meyer-Olkin (KMO) Test**:
-   Measures sampling adequacy
-   Values \> 0.6 = acceptable
-   Values \> 0.8 = good

Our overall KMO = 0.80 (good!)


::: {.cell}

```{.r .cell-code}
# KMO test requires psych package
KMO(cor(darl_numeric))
## Kaiser-Meyer-Olkin factor adequacy
## Call: KMO(r = cor(darl_numeric))
## Overall MSA =  0.76
## MSA for each item = 
##       height   mouth_diam    tube_diam    keel_diam wing1_length wing2_length 
##         0.64         0.84         0.47         0.61         0.85         0.80 
##    wingsprea   hoodmass_g   tubemass_g   wingmass_g 
##         0.76         0.78         0.72         0.86
```
:::

:::

::: {.column width="50%"}
### 2. Outlier Detection

Check for influential observations:


::: {.cell}
::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/outliers-1.png){width=576}
:::
:::

:::
:::::

# PCA vs. Other Ordination Methods

## Comparison Table

| Method | Data Type | Distance/Similarity | Linear? | Eigenanalysis? |
|----|----|----|----|----|
| **PCA** | Continuous, correlated | Euclidean (correlation) | Yes | Yes |
| **CA** | Count data, species | Chi-square | No | Yes |
| **PCoA** | Any (via distance matrix) | Any distance metric | N/A | Yes |
| **NMDS** | Any (via distance matrix) | Any distance metric | No | No |
| **DCA** | Species abundance | Weighted averaging | No | Yes |

# Advanced Topics in PCA

::::: columns
::: {.column width="50%"}
## Topics We Haven't Covered

-   **Factor Analysis**: - Similar to PCA but with different goals -
    Assumes latent "factors" cause observed variables - Uses rotation to
    improve interpretation - More common in social sciences
-   **Robust PCA**: - Less sensitive to outliers - Uses robust
    covariance estimators - Useful with messy ecological data
-   **Sparse PCA**: - Forces many loadings to exactly zero - Easier to
    interpret - Better for very high-dimensional data
:::

::: {.column width="50%"}
-   **Functional PCA**: - For time series or spatial data - Treats
    curves as observations - Common in physiology
-   **Probabilistic PCA**: - Includes uncertainty estimation - Can
    handle missing data - Maximum likelihood framework
-   **Kernel PCA**: - Handles non-linear relationships - Uses kernel
    trick from machine learning - More flexible but harder to interpret\
-   These are beyond our scope but worth knowing exist!
:::
:::::

# Practical Tips for PCA

## Best Practices

::::: columns
::: {.column width="50%"}
### Before PCA:

1.  **Explore your data**
    -   Check distributions
    -   Identify outliers
    -   Understand variable scales
2.  **Check correlations**
    -   Make correlation plot
    -   Ensure variables are correlated
    -   Consider removing redundant variables
3.  **Standardize appropriately**
    -   Use `scale = TRUE` for different units
    -   Consider centering only for same units
4.  **Check sample size**
    -   Need at least 3× variables
    -   More is better (50+ ideal)
:::

::: {.column width="50%"}
### During PCA:

1.  **Examine scree plot**
    -   Look for elbow
    -   Consider cumulative variance
    -   Usually keep 2-5 components
2.  **Interpret loadings**
    -   Look for patterns
    -   Consider biological meaning
    -   Ignore very small loadings
3.  **Check scores plots**
    -   Look for outliers
    -   Identify patterns/groups
    -   Consider site/treatment effects

### After PCA:

1.  **Report clearly**
    -   State why you used PCA
    -   Report variance explained
    -   Describe component interpretation
2.  **Use results appropriately**
    -   PC scores can go into other analyses
    -   Don't over-interpret small components
    -   Remember: exploratory tool!
:::
:::::

# Reporting PCA Results

## What to Include in Papers/Theses

::::: columns
::: {.column width="50%"}
### Methods Section:

"We performed principal component analysis (PCA) on 10 morphological and
biomass variables measured on 89 *Darlingtonia californica* plants from
four sites. Variables were standardized (mean = 0, SD = 1) prior to
analysis to account for different measurement scales. PCA was conducted
using the `prcomp()` function in R. We retained principal components
with eigenvalues \> 1 and that cumulatively explained \>70% of
variance."

### Results Section:

"The first three principal components explained 77.3% of variance (PC1:
45.8%, PC2: 16.7%, PC3: 14.8%). PC1 represented overall plant size, with
high positive loadings for most morphological and biomass variables..."
:::

::: {.column width="50%"}
### Essential Figures:

1.  **Scree plot** - shows variance explained
2.  **Loadings plot** - shows variable contributions
3.  **Scores plot** or **biplot** - shows samples in PC space

### Essential Tables:

1.  **Variance explained** by each PC
2.  **Loadings** for retained PCs (usually PC1-PC3)
3.  **ANOVA results** if testing groups

### Don't Report:

-   All 10 PCs if only 3 are meaningful
-   Tiny loadings (\< 0.3)
-   Every individual PC score
-   Raw eigenvalues (report % variance instead)
:::
:::::

# Common PCA Mistakes to Avoid

::::: columns
::: {.column width="50%"}
## Mistakes Students Make:

❌ **Not standardizing** data with different units\
→ ✓ Always use `scale = TRUE` for different units

❌ **Forgetting to check correlations**\
→ ✓ Make correlation plot first

❌ **Using too many components**\
→ ✓ Use scree plot, keep components above elbow

❌ **Over-interpreting small loadings**\
→ ✓ Focus on loadings \> \|0.3\|

❌ **Ignoring signs on loadings**\
→ ✓ Sign can flip; look at patterns

❌ **Treating PC scores as original data**\
→ ✓ Remember PCs are derived variables
:::

::: {.column width="50%"}
❌ **Using PCA on categorical data**\
→ ✓ Use appropriate method (CA, MCA)

❌ **Not checking for outliers**\
→ ✓ Plot scores, check for extreme values

❌ **Assuming PCs have biological meaning**\
→ ✓ Interpret carefully, components are mathematical

❌ **Using PCA with too few samples**\
→ ✓ Need n \> 3p (preferably n \> 50)

❌ **Not reporting variance explained**\
→ ✓ Always report % variance for each PC

❌ **Forgetting that PCA is exploratory**\
→ ✓ Use for patterns, not hypothesis testing
:::
:::::

# Practice Exercise

::::: columns
::: {.column width="50%"}
### Exercise 1: Subset Analysis

Try running PCA on just the **morphological variables** (exclude
biomass):


::: {.cell}

```{.r .cell-code}
# Select only morphological variables
darl_morph <- darlingtonia %>%
  select(height, mouth_diam, tube_diam, 
         keel_diam, wing1_length, 
         wing2_length, wingsprea)

# Run PCA
pca_morph <- prcomp(darl_morph, 
                    scale. = TRUE)

# Examine results
summary(pca_morph)

# How do results differ from full PCA?
```
:::

:::

::: {.column width="50%"}
### Exercise 2: Site-Specific PCA

Run PCA on just one site (e.g., DG):


::: {.cell}

```{.r .cell-code}
# Filter to one site
darl_dg <- darlingtonia %>%
  filter(site == "DG") %>%
  select(-site)

# Run PCA
pca_dg <- prcomp(darl_dg, 
                 scale. = TRUE)

# Compare to overall PCA
summary(pca_dg)

# Do the same patterns emerge?
```
:::


### Exercise 3: Interpretation

1.  What does PC1 represent in each analysis?
2.  How much variance is explained?
3.  Do you see the same patterns?
:::
:::::

# Some other ways


::: {.cell}

```{.r .cell-code}
# Create dataset with row names
darl_pca_data <- darlingtonia %>% 
  select(height, mouth_diam, tube_diam, keel_diam, 
         wing1_length, wing2_length, wingsprea, 
         hoodmass_g, tubemass_g)

# PCA
pca_facto <- PCA(darl_pca_data, 
                 graph = FALSE, 
                 scale.unit = TRUE)

# Create grouping factor with same length
site_groups <- factor(darlingtonia$site)

# Plot with grouping
fviz_pca_biplot(pca_facto, 
                col.ind = site_groups,  # Use col.ind instead
                palette = "jco",
                addEllipses = TRUE,
                ellipse.level = 0.95,
                repel = TRUE)
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-3-1.png){width=480}
:::

```{.r .cell-code}

# Individual plot with better options
fviz_pca_ind(pca_facto,
             geom.ind = "point",
             col.ind = darlingtonia$site,
             palette = "jco",
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Site",
             repel = TRUE)
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-3-2.png){width=480}
:::
:::



::: {.cell}

```{.r .cell-code}


# Quick PCA plot with ggplot2
autoplot(pca_result, 
         data = darlingtonia, 
         colour = 'site',
         loadings = TRUE, 
         loadings.label = TRUE,
         frame = TRUE,  # Adds ellipses
         frame.type = 'norm')  # or 't' for confidence
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-4-1.png){width=480}
:::
:::



::: {.cell}

```{.r .cell-code}
# Prepare data matrix (samples × variables)
darl_matrix <- darlingtonia %>%
  select(height, mouth_diam, tube_diam, keel_diam, 
         wing1_length, wing2_length, wingsprea, 
         hoodmass_g, tubemass_g) %>%
  scale()

# PCA with vegan (uses SVD, similar to prcomp)
pca_vegan <- rda(darl_matrix)

# Quick plot
biplot(pca_vegan, scaling = 2)
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-5-1.png){width=480}
:::

```{.r .cell-code}

# Calculate distances between sites
site_matrix <- darlingtonia %>%
  select(height, mouth_diam, wingmass_g) %>%
  mutate(site = darlingtonia$site) %>%
  group_by(site) %>%
  summarise(across(everything(), mean)) %>%
  column_to_rownames("site")

# Euclidean distance (many distance metrics available!)
dist_euclidean <- vegdist(site_matrix, method = "euclidean")
dist_bray <- vegdist(site_matrix, method = "bray")  # Bray-Curtis
dist_manhattan <- vegdist(site_matrix, method = "manhattan")

# Create a nice color palette
site_factor <- factor(darlingtonia$site)
n_sites <- length(levels(site_factor))
colors <- brewer.pal(min(n_sites, 8), "Set1")  # Use Set1 palette
site_colors <- colors[as.numeric(site_factor)]

# Ordination plot with sites
ordiplot(pca_vegan, type = "none")
points(pca_vegan, display = "sites", col = site_colors, pch = 19, cex = 1.5)
ordispider(pca_vegan, groups = darlingtonia$site, col = colors)
ordiellipse(pca_vegan, groups = darlingtonia$site, draw = "polygon", 
            col = colors, alpha = 0.2)

# Add legend
legend("topright", legend = levels(site_factor), 
       col = colors, pch = 19, bty = "n")
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-5-2.png){width=480}
:::
:::



::: {.cell}

```{.r .cell-code}
# Analysis
pca_vegan <- rda(darl_matrix)
permanova <- adonis2(darl_matrix ~ site, data = darlingtonia)

# Visualization
fviz_pca_biplot(pca_result,
                habillage = darlingtonia$site,
                addEllipses = TRUE) +
  labs(title = paste0("PERMANOVA: p = ", 
                      round(permanova$`Pr(>F)`[1], 3)))
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-6-1.png){width=480}
:::

```{.r .cell-code}

# Scale the data properly
darl_scaled <- darlingtonia %>%
  select(height, mouth_diam, tube_diam, keel_diam, 
         wing1_length, wing2_length, wingsprea, 
         hoodmass_g, tubemass_g) %>%
  scale() %>%
  as.data.frame()  # Convert back to data frame

# PERMANOVA
permanova_scaled <- adonis2(darl_scaled ~ site, 
                           data = darlingtonia, 
                           method = "euclidean",
                           permutations = 999)
permanova_scaled
## Permutation test for adonis under reduced model
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = darl_scaled ~ site, data = darlingtonia, permutations = 999, method = "euclidean")
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     3   189.72 0.24512 8.9836  0.001 ***
## Residual 83   584.28 0.75488                  
## Total    86   774.00 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
:::



::: {.cell}

```{.r .cell-code}
# Ordination plot with convex hulls (simpler)
ordiplot(pca_vegan, type = "none")
ordihull(pca_vegan, groups = darlingtonia$site, 
         draw = "polygon", col = 1:n_sites, alpha = 50)
points(pca_vegan, display = "sites", pch = 21, 
       bg = as.numeric(factor(darlingtonia$site)), cex = 1.5)
legend("topright", legend = levels(factor(darlingtonia$site)), 
       pt.bg = 1:n_sites, pch = 21, bty = "n")
```

::: {.cell-output-display}
![](16_01_lecture_powerpoint_files/figure-html/unnamed-chunk-7-1.png){width=480}
:::
:::



::: {.cell}

```{.r .cell-code}
# Pairwise comparisons between sites
pairwise_result <- pairwise.adonis2(darl_scaled ~ site, 
                                    data = darlingtonia,
                                    method = "euclidean",  # Add this!
                                    p.adjust.m = "bonferroni")
pairwise_result
## $parent_call
## [1] "darl_scaled ~ site , strata = Null , permutations 999"
## 
## $TJH_vs_DG
##          Df SumOfSqs      R2      F Pr(>F)   
## Model     1   32.602 0.10636 5.7128  0.002 **
## Residual 48  273.930 0.89364                 
## Total    49  306.532 1.00000                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $TJH_vs_LEH
##          Df SumOfSqs      R2      F Pr(>F)   
## Model     1    47.72 0.10517 5.6414  0.004 **
## Residual 48   406.06 0.89483                 
## Total    49   453.78 1.00000                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $TJH_vs_HD
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     1   69.308 0.25135 11.751  0.001 ***
## Residual 35  206.432 0.74865                  
## Total    36  275.740 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $DG_vs_LEH
##          Df SumOfSqs      R2      F Pr(>F)  
## Model     1    26.24 0.06493 3.3333  0.022 *
## Residual 48   377.85 0.93507                
## Total    49   404.09 1.00000                
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $DG_vs_HD
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     1   131.09 0.42382 25.745  0.001 ***
## Residual 35   178.22 0.57618                  
## Total    36   309.31 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $LEH_vs_HD
##          Df SumOfSqs     R2      F Pr(>F)    
## Model     1   101.69 0.2468 11.468  0.001 ***
## Residual 35   310.35 0.7532                  
## Total    36   412.04 1.0000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## attr(,"class")
## [1] "pwadstrata" "list"
```
:::



::: {.cell}

```{.r .cell-code}
# install.packages("RVAideMemoire")
library(RVAideMemoire)

# Pairwise PERMANOVA
pairwise.perm.manova(vegdist(darl_scaled, method = "euclidean"),
                     darlingtonia$site,
                     test = "Wilks",
                     nperm = 999,
                     p.method = "bonferroni")
## 
## 	Pairwise comparisons using permutation MANOVAs on a distance matrix 
## 
## data:  vegdist(darl_scaled, method = "euclidean") by darlingtonia$site
## 999 permutations 
## 
##     DG    HD    LEH  
## HD  0.006 -     -    
## LEH 0.114 0.006 -    
## TJH 0.012 0.006 0.018
## 
## P value adjustment method: bonferroni
```
:::


results come from different statistical tests being used. Let me break
down what each is doing: 1. pairwise.adonis2() - PERMANOVA on subsets
What it does: For each pair, it subsets the data to just those 2 sites
and runs a separate PERMANOVA Results: All p-values ≤ 0.016 (5 of 6 are
p = 0.001) Interpretation: Tests if centroids (mean positions) differ
between site pairs 2. pairwise.perm.manova() - Different test entirely
What it does: Uses a MANOVA-based approach with permutations on the full
distance matrix Results: More conservative - DG vs LEH now p = 0.078
(not significant) Interpretation: Also tests centroid differences but
uses a different statistical framework

Why the differences matter: Location (Centroid) Tests:

pairwise.adonis2() and pairwise.perm.manova() both test if site means
differ They give slightly different p-values due to different
algorithms, but similar conclusions Use these to answer: "Are sites
morphologically different?"
