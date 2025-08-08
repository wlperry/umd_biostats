---
title: "Lecture 17 - Principal Component Analysis (PCA)"
author: "Bill Perry"
execute:
  freeze: auto
  cache: true
  echo: true
  keep-md: true
  message: false
  warning: false
  fig-height: 4
  fig-width: 6
  paged-print: false

format:
  html:
    code-overflow: scroll
    toc: false
    output-file: "17_01_lecture_powerpoint_html.html"
    embed-resources: true
    self-contained: true
    max-width: 80ch
    css: ../../css/lecture.css
    fig-width: 7
    fig-height: 5

  # RevealJS - UNCHANGED (keeps your two-column layout and large images)
  revealjs:
    output-file: "17_01_lecture_powerpoint_slides.html"
    self-contained: true
    css: ../../css/lecture.css
    slide-number: true
    transition: fade
    background-transition: none
    scrollable: true
    smaller: true
    width: 1280
    height: 720
    margin: 0.1
    min-scale: 0.2
    max-scale: 2.0
    fig-width: 8
    fig-height: 5

  # Word format - optimized for printing and document flow
  docx:
    default: true
    toc: false
    toc-depth: 3
    number-sections: false
    highlight-style: github
    reference-doc: ../../ms_templates/custom-reference.docx
    css: msword.css
    embed-resources: true
    fig-width: 5.5      # Smaller figures for better document layout
    fig-height: 3.5     # Better proportions for printing
    fig-dpi: 300        # High resolution for crisp printing
    df-print: kable     # Better table formatting

  # PowerPoint format - optimized for traditional PowerPoint presentation
  pptx:
    reference-doc: ../../ms_templates/lecture_template.pptx
    embed-resources: true
    fig-width: 6.5      # Good size for PowerPoint slides
    fig-height: 4       # Proper aspect ratio for slides
    fig-dpi: 300        # High resolution for projection
    df-print: kable     # Better table formatting

editor: visual
---













# Lecture 16: Review

::::: columns
::: {.column width="60%"}
## Review

-   Multivariate data
-   Multivariate statistics in ecology: overview
-   Eigenvectors, eigenvalues, components
-   Distance and dissimilarity in MV space
-   Data standardization
-   Graphics
-   Screening MV data
-   MANOVA
:::

::: {.column width="40%"}
![](images/clipboard-2539526410.png){width="500"}
:::
:::::

# Review: Eigenvectors and Components

Eigenvectors, eigenvalues and components

-   Common goal of MV analysis is variable reduction: can we derive new
    variables (based on linear combinations of "original" variables)
    that explain variation in data?

-   For data set with i= 1 to n objects and j = 1 to p original
    variables we seek new variables (principal components) using the
    equation:

### $$z_{ik} = c_1y_{i1} + c_2y_{i2} + \cdots c_jy_{ij} + \cdots + c_py_{ip}$$

# Review: Component Interpretation

-   zik is value of new variable k for object I
-   yi1- yip are values of original variables for object i
-   c1-cp are coefficients that show importance of the original
    variables to new derived variable

### $$z_{ik} = c_1y_{i1} + c_2y_{i2} + \cdots c_jy_{ij} + \cdots + c_py_{ip}$$

# Review: Component Properties

::::: columns
::: {.column width="60%"}
Eigenvectors, eigenvalues and components

Derived variables are found so that:

-   First derived variable explains most of the variation in the data
-   Second most of the remaining variation
-   And so on…
-   As many derived variables as original variables (p)
-   Derived variables are uncorrelated with each other
:::

::: {.column width="40%"}
![](images/clipboard-2044711521.png){width="500"}
:::
:::::

# Review: Eigenvalues and Eigenvectors

Eigenvectors, eigenvalues and components

-   Eigenvalues (latent roots) represent amount of variation in data
    explained by the new k= 1 to p derived variables (λ1, λ2 …λp).
-   Eigenvalues are population parameters and are estimated using ML to
    get sample statistics (l1, l2…lp)
-   Eigenvectors are lists of coefficients (c) that show contribution of
    original variables to new, derived variables
-   Each new variable has an eigenvalue and an eigenvector
-   New variables (components) are derived from a p x p covariance or
    correlation matrix of original variables

# Lecture 17: PCA Goals and Introduction

-   Common goals of MV data analysis are variable reduction (finding
    derived variables that summarize data) and exploration of patterns
    in data (scaling/ordination)
-   Can use association (correlation/ covariance) matrices (PCA) or
    dissimilarity measures (MDS)
-   In PCA: take p old variables and transform them into p "new/derived"
    uncorrelated variables (principal components)

# Data for PCA Analysis










::: {.cell}

```{.r .cell-code}
# Load the iris dataset
iris_df <- iris %>% clean_names() %>% mutate(ind = row_number()) %>% 
  mutate(species_ind = paste(species, ind, sep="_"))

# get values only
iris_data <- iris_df  %>% select(-species, -ind, -species_ind)

# Keep species for later visualization
iris_species <- iris_df  %>% select(species, ind, species_ind)

# pivot to long formt for viewing
iris_long_df <- iris_df %>%
  pivot_longer(
    cols = -c(species, ind, species_ind),
    names_to = "variable",
    values_to = "values")
 
iris_df 
```

::: {.cell-output-display}

| sepal_length| sepal_width| petal_length| petal_width|species    | ind|species_ind    |
|------------:|-----------:|------------:|-----------:|:----------|---:|:--------------|
|          5.1|         3.5|          1.4|         0.2|setosa     |   1|setosa_1       |
|          4.9|         3.0|          1.4|         0.2|setosa     |   2|setosa_2       |
|          4.7|         3.2|          1.3|         0.2|setosa     |   3|setosa_3       |
|          4.6|         3.1|          1.5|         0.2|setosa     |   4|setosa_4       |
|          5.0|         3.6|          1.4|         0.2|setosa     |   5|setosa_5       |
|          5.4|         3.9|          1.7|         0.4|setosa     |   6|setosa_6       |
|          4.6|         3.4|          1.4|         0.3|setosa     |   7|setosa_7       |
|          5.0|         3.4|          1.5|         0.2|setosa     |   8|setosa_8       |
|          4.4|         2.9|          1.4|         0.2|setosa     |   9|setosa_9       |
|          4.9|         3.1|          1.5|         0.1|setosa     |  10|setosa_10      |
|          5.4|         3.7|          1.5|         0.2|setosa     |  11|setosa_11      |
|          4.8|         3.4|          1.6|         0.2|setosa     |  12|setosa_12      |
|          4.8|         3.0|          1.4|         0.1|setosa     |  13|setosa_13      |
|          4.3|         3.0|          1.1|         0.1|setosa     |  14|setosa_14      |
|          5.8|         4.0|          1.2|         0.2|setosa     |  15|setosa_15      |
|          5.7|         4.4|          1.5|         0.4|setosa     |  16|setosa_16      |
|          5.4|         3.9|          1.3|         0.4|setosa     |  17|setosa_17      |
|          5.1|         3.5|          1.4|         0.3|setosa     |  18|setosa_18      |
|          5.7|         3.8|          1.7|         0.3|setosa     |  19|setosa_19      |
|          5.1|         3.8|          1.5|         0.3|setosa     |  20|setosa_20      |
|          5.4|         3.4|          1.7|         0.2|setosa     |  21|setosa_21      |
|          5.1|         3.7|          1.5|         0.4|setosa     |  22|setosa_22      |
|          4.6|         3.6|          1.0|         0.2|setosa     |  23|setosa_23      |
|          5.1|         3.3|          1.7|         0.5|setosa     |  24|setosa_24      |
|          4.8|         3.4|          1.9|         0.2|setosa     |  25|setosa_25      |
|          5.0|         3.0|          1.6|         0.2|setosa     |  26|setosa_26      |
|          5.0|         3.4|          1.6|         0.4|setosa     |  27|setosa_27      |
|          5.2|         3.5|          1.5|         0.2|setosa     |  28|setosa_28      |
|          5.2|         3.4|          1.4|         0.2|setosa     |  29|setosa_29      |
|          4.7|         3.2|          1.6|         0.2|setosa     |  30|setosa_30      |
|          4.8|         3.1|          1.6|         0.2|setosa     |  31|setosa_31      |
|          5.4|         3.4|          1.5|         0.4|setosa     |  32|setosa_32      |
|          5.2|         4.1|          1.5|         0.1|setosa     |  33|setosa_33      |
|          5.5|         4.2|          1.4|         0.2|setosa     |  34|setosa_34      |
|          4.9|         3.1|          1.5|         0.2|setosa     |  35|setosa_35      |
|          5.0|         3.2|          1.2|         0.2|setosa     |  36|setosa_36      |
|          5.5|         3.5|          1.3|         0.2|setosa     |  37|setosa_37      |
|          4.9|         3.6|          1.4|         0.1|setosa     |  38|setosa_38      |
|          4.4|         3.0|          1.3|         0.2|setosa     |  39|setosa_39      |
|          5.1|         3.4|          1.5|         0.2|setosa     |  40|setosa_40      |
|          5.0|         3.5|          1.3|         0.3|setosa     |  41|setosa_41      |
|          4.5|         2.3|          1.3|         0.3|setosa     |  42|setosa_42      |
|          4.4|         3.2|          1.3|         0.2|setosa     |  43|setosa_43      |
|          5.0|         3.5|          1.6|         0.6|setosa     |  44|setosa_44      |
|          5.1|         3.8|          1.9|         0.4|setosa     |  45|setosa_45      |
|          4.8|         3.0|          1.4|         0.3|setosa     |  46|setosa_46      |
|          5.1|         3.8|          1.6|         0.2|setosa     |  47|setosa_47      |
|          4.6|         3.2|          1.4|         0.2|setosa     |  48|setosa_48      |
|          5.3|         3.7|          1.5|         0.2|setosa     |  49|setosa_49      |
|          5.0|         3.3|          1.4|         0.2|setosa     |  50|setosa_50      |
|          7.0|         3.2|          4.7|         1.4|versicolor |  51|versicolor_51  |
|          6.4|         3.2|          4.5|         1.5|versicolor |  52|versicolor_52  |
|          6.9|         3.1|          4.9|         1.5|versicolor |  53|versicolor_53  |
|          5.5|         2.3|          4.0|         1.3|versicolor |  54|versicolor_54  |
|          6.5|         2.8|          4.6|         1.5|versicolor |  55|versicolor_55  |
|          5.7|         2.8|          4.5|         1.3|versicolor |  56|versicolor_56  |
|          6.3|         3.3|          4.7|         1.6|versicolor |  57|versicolor_57  |
|          4.9|         2.4|          3.3|         1.0|versicolor |  58|versicolor_58  |
|          6.6|         2.9|          4.6|         1.3|versicolor |  59|versicolor_59  |
|          5.2|         2.7|          3.9|         1.4|versicolor |  60|versicolor_60  |
|          5.0|         2.0|          3.5|         1.0|versicolor |  61|versicolor_61  |
|          5.9|         3.0|          4.2|         1.5|versicolor |  62|versicolor_62  |
|          6.0|         2.2|          4.0|         1.0|versicolor |  63|versicolor_63  |
|          6.1|         2.9|          4.7|         1.4|versicolor |  64|versicolor_64  |
|          5.6|         2.9|          3.6|         1.3|versicolor |  65|versicolor_65  |
|          6.7|         3.1|          4.4|         1.4|versicolor |  66|versicolor_66  |
|          5.6|         3.0|          4.5|         1.5|versicolor |  67|versicolor_67  |
|          5.8|         2.7|          4.1|         1.0|versicolor |  68|versicolor_68  |
|          6.2|         2.2|          4.5|         1.5|versicolor |  69|versicolor_69  |
|          5.6|         2.5|          3.9|         1.1|versicolor |  70|versicolor_70  |
|          5.9|         3.2|          4.8|         1.8|versicolor |  71|versicolor_71  |
|          6.1|         2.8|          4.0|         1.3|versicolor |  72|versicolor_72  |
|          6.3|         2.5|          4.9|         1.5|versicolor |  73|versicolor_73  |
|          6.1|         2.8|          4.7|         1.2|versicolor |  74|versicolor_74  |
|          6.4|         2.9|          4.3|         1.3|versicolor |  75|versicolor_75  |
|          6.6|         3.0|          4.4|         1.4|versicolor |  76|versicolor_76  |
|          6.8|         2.8|          4.8|         1.4|versicolor |  77|versicolor_77  |
|          6.7|         3.0|          5.0|         1.7|versicolor |  78|versicolor_78  |
|          6.0|         2.9|          4.5|         1.5|versicolor |  79|versicolor_79  |
|          5.7|         2.6|          3.5|         1.0|versicolor |  80|versicolor_80  |
|          5.5|         2.4|          3.8|         1.1|versicolor |  81|versicolor_81  |
|          5.5|         2.4|          3.7|         1.0|versicolor |  82|versicolor_82  |
|          5.8|         2.7|          3.9|         1.2|versicolor |  83|versicolor_83  |
|          6.0|         2.7|          5.1|         1.6|versicolor |  84|versicolor_84  |
|          5.4|         3.0|          4.5|         1.5|versicolor |  85|versicolor_85  |
|          6.0|         3.4|          4.5|         1.6|versicolor |  86|versicolor_86  |
|          6.7|         3.1|          4.7|         1.5|versicolor |  87|versicolor_87  |
|          6.3|         2.3|          4.4|         1.3|versicolor |  88|versicolor_88  |
|          5.6|         3.0|          4.1|         1.3|versicolor |  89|versicolor_89  |
|          5.5|         2.5|          4.0|         1.3|versicolor |  90|versicolor_90  |
|          5.5|         2.6|          4.4|         1.2|versicolor |  91|versicolor_91  |
|          6.1|         3.0|          4.6|         1.4|versicolor |  92|versicolor_92  |
|          5.8|         2.6|          4.0|         1.2|versicolor |  93|versicolor_93  |
|          5.0|         2.3|          3.3|         1.0|versicolor |  94|versicolor_94  |
|          5.6|         2.7|          4.2|         1.3|versicolor |  95|versicolor_95  |
|          5.7|         3.0|          4.2|         1.2|versicolor |  96|versicolor_96  |
|          5.7|         2.9|          4.2|         1.3|versicolor |  97|versicolor_97  |
|          6.2|         2.9|          4.3|         1.3|versicolor |  98|versicolor_98  |
|          5.1|         2.5|          3.0|         1.1|versicolor |  99|versicolor_99  |
|          5.7|         2.8|          4.1|         1.3|versicolor | 100|versicolor_100 |
|          6.3|         3.3|          6.0|         2.5|virginica  | 101|virginica_101  |
|          5.8|         2.7|          5.1|         1.9|virginica  | 102|virginica_102  |
|          7.1|         3.0|          5.9|         2.1|virginica  | 103|virginica_103  |
|          6.3|         2.9|          5.6|         1.8|virginica  | 104|virginica_104  |
|          6.5|         3.0|          5.8|         2.2|virginica  | 105|virginica_105  |
|          7.6|         3.0|          6.6|         2.1|virginica  | 106|virginica_106  |
|          4.9|         2.5|          4.5|         1.7|virginica  | 107|virginica_107  |
|          7.3|         2.9|          6.3|         1.8|virginica  | 108|virginica_108  |
|          6.7|         2.5|          5.8|         1.8|virginica  | 109|virginica_109  |
|          7.2|         3.6|          6.1|         2.5|virginica  | 110|virginica_110  |
|          6.5|         3.2|          5.1|         2.0|virginica  | 111|virginica_111  |
|          6.4|         2.7|          5.3|         1.9|virginica  | 112|virginica_112  |
|          6.8|         3.0|          5.5|         2.1|virginica  | 113|virginica_113  |
|          5.7|         2.5|          5.0|         2.0|virginica  | 114|virginica_114  |
|          5.8|         2.8|          5.1|         2.4|virginica  | 115|virginica_115  |
|          6.4|         3.2|          5.3|         2.3|virginica  | 116|virginica_116  |
|          6.5|         3.0|          5.5|         1.8|virginica  | 117|virginica_117  |
|          7.7|         3.8|          6.7|         2.2|virginica  | 118|virginica_118  |
|          7.7|         2.6|          6.9|         2.3|virginica  | 119|virginica_119  |
|          6.0|         2.2|          5.0|         1.5|virginica  | 120|virginica_120  |
|          6.9|         3.2|          5.7|         2.3|virginica  | 121|virginica_121  |
|          5.6|         2.8|          4.9|         2.0|virginica  | 122|virginica_122  |
|          7.7|         2.8|          6.7|         2.0|virginica  | 123|virginica_123  |
|          6.3|         2.7|          4.9|         1.8|virginica  | 124|virginica_124  |
|          6.7|         3.3|          5.7|         2.1|virginica  | 125|virginica_125  |
|          7.2|         3.2|          6.0|         1.8|virginica  | 126|virginica_126  |
|          6.2|         2.8|          4.8|         1.8|virginica  | 127|virginica_127  |
|          6.1|         3.0|          4.9|         1.8|virginica  | 128|virginica_128  |
|          6.4|         2.8|          5.6|         2.1|virginica  | 129|virginica_129  |
|          7.2|         3.0|          5.8|         1.6|virginica  | 130|virginica_130  |
|          7.4|         2.8|          6.1|         1.9|virginica  | 131|virginica_131  |
|          7.9|         3.8|          6.4|         2.0|virginica  | 132|virginica_132  |
|          6.4|         2.8|          5.6|         2.2|virginica  | 133|virginica_133  |
|          6.3|         2.8|          5.1|         1.5|virginica  | 134|virginica_134  |
|          6.1|         2.6|          5.6|         1.4|virginica  | 135|virginica_135  |
|          7.7|         3.0|          6.1|         2.3|virginica  | 136|virginica_136  |
|          6.3|         3.4|          5.6|         2.4|virginica  | 137|virginica_137  |
|          6.4|         3.1|          5.5|         1.8|virginica  | 138|virginica_138  |
|          6.0|         3.0|          4.8|         1.8|virginica  | 139|virginica_139  |
|          6.9|         3.1|          5.4|         2.1|virginica  | 140|virginica_140  |
|          6.7|         3.1|          5.6|         2.4|virginica  | 141|virginica_141  |
|          6.9|         3.1|          5.1|         2.3|virginica  | 142|virginica_142  |
|          5.8|         2.7|          5.1|         1.9|virginica  | 143|virginica_143  |
|          6.8|         3.2|          5.9|         2.3|virginica  | 144|virginica_144  |
|          6.7|         3.3|          5.7|         2.5|virginica  | 145|virginica_145  |
|          6.7|         3.0|          5.2|         2.3|virginica  | 146|virginica_146  |
|          6.3|         2.5|          5.0|         1.9|virginica  | 147|virginica_147  |
|          6.5|         3.0|          5.2|         2.0|virginica  | 148|virginica_148  |
|          6.2|         3.4|          5.4|         2.3|virginica  | 149|virginica_149  |
|          5.9|         3.0|          5.1|         1.8|virginica  | 150|virginica_150  |

:::
:::










# Step 1: Explore the Iris Dataset

::::: columns
::: {.column width="60%"}
As in every case you should be looking at the data first - every time...

Right is the data on iris from a long dataframe
:::

::: {.column width="40%"}









::: {.cell}

```{.r .cell-code}
overview_plot <- iris_long_df %>% 
  ggplot(aes(species, values, color=species)) + 
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")
overview_plot
```

::: {.cell-output-display}
![](17_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-1-1.png)
:::
:::









:::
:::::

# What is PCA? Goals and Overview

::::: columns
::: {.column width="40%"}
## Principal Component Analysis Goals:

-   **Variable Reduction**: Transform many correlated variables into
    fewer uncorrelated components
-   **Data Exploration**: Visualize patterns and relationships in
    high-dimensional data
-   **Noise Reduction**: Focus on the most important sources of
    variation
-   **Dimension Reduction**: Make complex datasets easier to analyze and
    interpret

**Today's Example**: Iris flower measurements - can we reduce 4
measurements to 2-3 components that capture most variation?
:::

::: {.column width="60%"}









::: {.cell}
::: {.cell-output-display}
![](17_01_lecture_powerpoint_files/figure-docx/correaltions-1.png)
:::
:::









:::
:::::

# High-Dimensional Data Visualization

::::: columns
::: {.column width="40%"}
## Principal Component Analysis Goals:

-   **Variable Reduction**: Transform many correlated variables into
    fewer uncorrelated components
-   **Data Exploration**: Visualize patterns and relationships in
    high-dimensional data
-   **Noise Reduction**: Focus on the most important sources of
    variation
-   **Dimension Reduction**: Make complex datasets easier to analyze and
    interpret

**Today's Example**: Iris flower measurements - can we reduce 4
measurements to fewer components that capture most variation?
:::

::: {.column width="60%"}









::: {.cell}

:::









:::
:::::

# PCA Assumptions - Critical to Check First!

## Key Assumptions:

1.  **Linear relationships** between variables
2.  **No extreme outliers** (can distort results)
3.  **Variables should be correlated** (if not, PCA won't reduce
    dimensions)
4.  **Adequate sample size** (generally n \> 50, preferably n \> 100)
5.  **No missing data** (complete cases only)
6.  **Consider standardization** when variables have different scales

**Important**: PCA works best when original variables are moderately to
highly correlated!

**Let's check these assumptions with our iris data...**

# Step 2: Check PCA Assumptions - Correlations










::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "Correlation Matrix:"
```


:::

::: {.cell-output .cell-output-stdout}

```
             sepal_length sepal_width petal_length petal_width
sepal_length        1.000      -0.118        0.872       0.818
sepal_width        -0.118       1.000       -0.428      -0.366
petal_length        0.872      -0.428        1.000       0.963
petal_width         0.818      -0.366        0.963       1.000
```


:::

::: {.cell-output-display}
![](17_01_lecture_powerpoint_files/figure-docx/check_correlations-1.png)
:::
:::










# Step 2: Check PCA Assumptions - Linearity










::: {.cell}
::: {.cell-output-display}
![](17_01_lecture_powerpoint_files/figure-docx/check_linearity-1.png)
:::
:::










# Step 2: Check PCA Assumptions - Outliers










::: {.cell}
::: {.cell-output-display}
![](17_01_lecture_powerpoint_files/figure-docx/check_outliers-1.png)
:::
:::










# Step 3: Standardize the Data

## STANDARDIZATION: Making all variables comparable

::::: columns
::: {.column width="60%"}
Why standardize?

Our measurements have different units and scales:

-   Sepal length: ranges from \~4-8 cm
-   Sepal width: ranges from \~2-4 cm
-   Petal length: ranges from \~1-7 cm
-   Petal width: ranges from \~0.1-2.5 cm

Without standardization, PCA would be dominated by variables with larger
numbers (like petal length) simply because they have bigger values, not
because they're more important biologically

What does standardization do?

-   Converts each variable to have:
    -   Mean = 0 (centered at zero)
    -   Standard deviation = 1 (same spread)
    -   This gives all variables equal weight in the analysis

How to interpret standardized values: Example: A sepal length of 5.1 cm
might become -0.9 after standardization, meaning it's 0.9 standard
deviations below the average sepal length
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output .cell-output-stdout}

```
[1] "Means after standardization (should be ~0):"
```


:::

::: {.cell-output .cell-output-stdout}

```
sepal_length  sepal_width petal_length  petal_width 
           0            0            0            0 
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] "Standard deviations after standardization (should be 1):"
```


:::

::: {.cell-output .cell-output-stdout}

```
sepal_length  sepal_width petal_length  petal_width 
           1            1            1            1 
```


:::
:::









:::
:::::

# Step 4: Perform PCA - The Mathematics

::::: columns
::: {.column width="60%"}
## What is PCA doing?

Principal Component Analysis finds new variables (called components)
that capture the most variation in your data. Think of it as finding the
"best viewing angles" to see differences between flowers.

## The mathematics (simplified):

-   PCA rotates your data to find the direction with maximum spread
    (PC1)
-   Then finds the next direction with maximum spread perpendicular to
    PC1 (PC2)
-   Continues until it has as many components as original variables (4
    in our case)

## Why center = FALSE and scale = FALSE?

We already standardized our data in Step 3, so we tell R not to do it
again: - center = FALSE: Don't subtract the mean (we already did) -
scale = FALSE: Don't divide by standard deviation (we already did)

## What the summary shows:

-   **Standard deviation**: How much variation each component captures
-   **Proportion of Variance**: Percentage of total variation explained
    by each component
-   **Cumulative Proportion**: Running total of variance explained
:::

::: {.column width="40%"}









::: {.cell}

```{.r .cell-code}
# Perform PCA on standardized data
iris_pca <- prcomp(iris_scaled, center = FALSE, scale. = FALSE)
# Note: center and scale are FALSE because we already standardized

# Alternative using vegan package
iris_pca_vegan <- rda(iris_scaled)

# Summary of PCA results
summary(iris_pca)
```

::: {.cell-output .cell-output-stdout}

```
Importance of components:
                          PC1    PC2     PC3     PC4
Standard deviation     1.7084 0.9560 0.38309 0.14393
Proportion of Variance 0.7296 0.2285 0.03669 0.00518
Cumulative Proportion  0.7296 0.9581 0.99482 1.00000
```


:::
:::









:::
:::::

# Deriving Components: 2D Visualization

::::: columns
::: {.column width="60%"}
How are new uncorrelated components derived? One way to think it is in
terms of axis rotation Consider a 2-variable dataset:
:::

::: {.column width="40%"}
![](images/clipboard-1945433117.png){width="500"}
:::
:::::

# Component Derivation: Axis Rotation

::::: columns
::: {.column width="60%"}
Goal is to "rotate the axes" around center of the data "cloud" in such a
way that most of the variation lies along the first axis Then find
second axis that explains the second-most variation AND is orthogonal to
first axis
:::

::: {.column width="40%"}
![](images/clipboard-369995082.png){width="500"}
:::
:::::

# Component Derivation: Multivariate Extension

::::: columns
::: {.column width="60%"}
Easy to picture in 2D (or even 3D), but harder in multivariate space
Practically, components are "extracted" from a covariance of correlation
matrix among original variables Will extract as many principal
components as original variables
:::

::: {.column width="40%"}
![](images/clipboard-1512013712.png){width="500"}
:::
:::::

# Component Information: Eigenvalues and Eigenvectors

-   Get two important pieces of information from PCA: eigenvectors and
    eigenvalues
-   Eigenvalues (latent roots)- how much of the variation is explained
    by each component?
-   Eigenvectors- list of coefficients for original variables. There are
    p coefficients in an eigenvector and p eigenvectors
-   Correlation bw original variables will result in fewer components
    explaining more variance; variable reduction will fail if original
    variables are not correlated

# Step 4: Understanding Eigenvalues and Variance

::::: columns
::: {.column width="60%"}
## Understanding Eigenvalues and Variance

### What are eigenvalues?

-   Eigenvalues tell us how much variation each principal component
    captures.
-   Larger eigenvalues = more important components.

### Key terms explained:

-   **Eigenvalue**: The amount of variance captured by each component
    (always positive)
-   **Proportion of Variance**: What percentage of total variation this
    component explains
-   **Cumulative Variance**: Running total - helps us decide how many
    components we need

### How to read the results:

-   If PC1 has eigenvalue = 2.9, it captures 2.9 "units" of variance
-   If Prop_Variance = 0.728, PC1 explains 72.8% of all variation in the
    data
-   If Cumsum_Variance = 0.959 at PC2, the first 2 components together
    explain 95.9% of variation

### Why this matters:

This table helps us decide how many components to keep.

-   If 2 components explain 95% of variance, we've successfully reduced
    4 variables to 2
-   We only lose 5% of information without including the other
    variables!
:::

::: {.column width="40%"}









::: {.cell}

```{.r .cell-code}
# Extract eigenvalues (variance explained by each component)
eigenvalues <- iris_pca$sdev^2
prop_variance <- eigenvalues / sum(eigenvalues)
cumsum_variance <- cumsum(prop_variance)

# Create a summary table
pca_summary <- data.frame(
  Component = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = eigenvalues,
  Prop_Variance = prop_variance,
  Cumsum_Variance = cumsum_variance
)

print("PCA Summary:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "PCA Summary:"
```


:::

```{.r .cell-code}
kable(pca_summary, digits = 3)
```

::: {.cell-output-display}


|Component | Eigenvalue| Prop_Variance| Cumsum_Variance|
|:---------|----------:|-------------:|---------------:|
|PC1       |      2.918|         0.730|           0.730|
|PC2       |      0.914|         0.229|           0.958|
|PC3       |      0.147|         0.037|           0.995|
|PC4       |      0.021|         0.005|           1.000|


:::
:::









:::
:::::

# Step 5: Determine Number of Components - Scree Plot

::::: columns
::: {.column width="60%"}
## What is a Scree Plot?

A scree plot shows how much variance each component explains, helping us
decide how many components we need. The name comes from the geological
term "scree" - loose rocks at the base of a cliff - because the plot
often looks like a steep cliff followed by rubble.

## How to read a Scree Plot:

-   **Y-axis**: Percentage of variance explained by each component
-   **X-axis**: Component number (PC1, PC2, etc.)
-   **The pattern**: Usually shows a steep drop followed by a leveling
    off

## The "Elbow Method":

Look for where the line "bends" or forms an elbow:

-   Components before the elbow = important (steep slope)
-   Components after the elbow = less important (gentle slope)
-   Keep components up to and including the elbow

## What to look for in our plot:

-   If PC1 explains 70% and PC2 explains 20%, but PC3 only explains 5%,
    the elbow is at PC2
-   This suggests keeping the first 2 components
-   The dramatic drop from PC1 to PC2, then gentle decline after,
    confirms our dimension reduction worked well
:::

::: {.column width="40%"}









::: {.cell}

:::









:::
:::::

# Step 5: Component Selection Rules










::: {.cell}

```{.r .cell-code}
# Eigenvalue > 1 rule (Kaiser criterion)
components_to_keep <- sum(eigenvalues > 1)
print(paste("Components with eigenvalue > 1:", components_to_keep))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Components with eigenvalue > 1: 1"
```


:::

```{.r .cell-code}
# Components explaining at least 80% of variance
components_80_percent <- which(cumsum_variance >= 0.80)[1]
print(paste("Components needed for 80% variance:", components_80_percent))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Components needed for 80% variance: 2"
```


:::
:::










# Step 6: Interpret the Components - Loadings

::::: columns
::: {.column width="60%"}
## What are Component Loadings

Loadings tell us how much each original variable contributes to each
principal component. Think of them as "recipes" that show how to mix
your original measurements to create the new components.

## How to read the loadings table

-   **Values range from -1 to +1** (like correlations)
-   **Large positive values** (e.g., 0.8): This variable contributes
    strongly in the positive direction
-   **Large negative values** (e.g., -0.8): This variable contributes
    strongly in the negative direction
-   **Values near 0**: This variable doesn't contribute much to this
    component

## Interpreting the patterns:

-   **If all loadings have similar signs**: Component represents overall
    size (all measurements increase/decrease together)
-   **If loadings have mixed signs**: Component represents shape or
    proportions (some measurements increase while others decrease)
-   **Dominant variables**: Variables with the largest absolute loadings
    drive that component's meaning

## Example interpretation:

If PC1 has all negative loadings around -0.5, it means:

-   Flowers with high PC1 scores have small values for ALL measurements
-   This component captures "overall flower size"
-   The negative sign just indicates direction (could flip signs and
    interpretation)
:::

::: {.column width="40%"}









::: {.cell}

```{.r .cell-code}
# Component loadings (how much each original variable contributes)
loadings_df <- data.frame(
  Variable = rownames(iris_pca$rotation),
  PC1 = iris_pca$rotation[, 1],
  PC2 = iris_pca$rotation[, 2],
  PC3 = iris_pca$rotation[, 3],
  PC4 = iris_pca$rotation[, 4]
)

print("Component Loadings:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Component Loadings:"
```


:::

```{.r .cell-code}
loadings_df
```

::: {.cell-output-display}

|             |Variable     |        PC1|        PC2|        PC3|        PC4|
|:------------|:------------|----------:|----------:|----------:|----------:|
|sepal_length |sepal_length |  0.5210659| -0.3774176|  0.7195664|  0.2612863|
|sepal_width  |sepal_width  | -0.2693474| -0.9232957| -0.2443818| -0.1235096|
|petal_length |petal_length |  0.5804131| -0.0244916| -0.1421264| -0.8014492|
|petal_width  |petal_width  |  0.5648565| -0.0669420| -0.6342727|  0.5235971|

:::
:::









:::
:::::

# Step 6: Eigenvector Properties

### Key properties of eigenvectors/loadings:

-   **Unit length**: Each eigenvector has length 1 (sum of squares = 1)
-   **Orthogonal**: Eigenvectors are perpendicular to each other (dot
    product = 0)
-   **Ordered by importance**: First eigenvector (PC1) explains most
    variance

### The complete picture:

-   **Eigenvectors** = The directions (loadings)
-   **Eigenvalues** = The importance of each direction (variance
    explained)
-   Together they fully describe the PCA transformation

# Step 6b: Visualization of Component Loadings

::::: columns
::: {.column width="60%"}
What does this plot show?

This is a visual representation of the loadings table, showing how each
original variable contributes to PC1 and PC2. It's like a map of how
your original measurements relate to the new principal components.

How to read the plot:

-   Arrows represent your original variables (sepal_length, sepal_width,
    etc.)
-   Arrow direction shows which PC the variable contributes to
-   Arrow length indicates the strength of contribution (longer =
    stronger)
-   Arrow color shows the overall contribution magnitude (red = highest,
    blue = lowest)
-   The circle represents the maximum possible contribution

Key interpretations from this plot:

-   PC1 (horizontal axis, 73% variance):
    -   All arrows point roughly left (negative direction)
    -   All variables contribute almost equally to PC1
    -   This confirms PC1 represents "overall flower size"

PC2 (vertical axis, 22.9% variance):

-   Sepal_width points down (negative)
-   Other variables point slightly up (positive)
-   This creates a contrast: sepal width vs. everything else
-   PC2 captures "flower shape" - wide sepals vs. long petals
:::

::: {.column width="40%"}









::: {.cell}

:::









:::
:::::

# Loading Plot Interpretation

What the arrow positions tell us:

-   Variables pointing in same direction = positively correlated
-   Variables at 90° angles = uncorrelated
-   Variables pointing opposite directions = negatively correlated

The practical meaning:

-   Flowers with high PC1 scores have large values for all measurements
-   Flowers with high PC2 scores have narrow sepals but long/wide petals
-   The plot confirms our dimension reduction worked - we've captured
    95.9% of variation in just 2 dimensions!

# Step 7: PCA Biplot - The Main Result

::::: columns
::: {.column width="60%"}
## Key insights from this biplot:

### Species separation:

-   **Setosa (blue)**: Clearly separated on the left (negative PC1)
-   **Versicolor (yellow)**: In the middle
-   **Virginica (red)**: On the right (positive PC1)
-   PCA successfully separates species without being told about them!

### Understanding flower characteristics:

-   **Setosa flowers**: Small overall (negative PC1), relatively wide
    sepals (positive PC2)
-   **Virginica flowers**: Large overall (positive PC1), especially long
    petals
-   **Versicolor flowers**: Intermediate in most characteristics

### Variable relationships:

-   Petal measurements point together → highly correlated
-   Sepal width points differently → captures different information
-   All arrows point right → all measurements increase from setosa to
    virginica
:::

::: {.column width="40%"}









::: {.cell}

:::









:::
:::::

# Step 7: PCA Scores Plot - Alternative Visualization










::: {.cell}

```{.r .cell-code}
# Alternative scores plot
ggplot(pca_scores, aes(x = PC1, y = PC2, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.68, linetype = 2) +  # Add ellipses
  labs(title = "PCA Scores Plot",
       subtitle = paste0("PC1 explains ", round(prop_variance[1]*100, 1), 
                        "% of variance, PC2 explains ", round(prop_variance[2]*100, 1), "%"),
       x = paste0("PC1 (", round(prop_variance[1]*100, 1), "%)"),
       y = paste0("PC2 (", round(prop_variance[2]*100, 1), "%)"),
       color = "Species") +
  theme_minimal() +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
```
:::










# Step 8: Interpret PC1 Results

### Understanding PC1 Loadings:

The loadings show how each original variable contributes to PC1:

-   **Sepal length: 0.521** - Strong positive contribution
-   **Sepal width: -0.269** - Moderate negative contribution
-   **Petal length: 0.580** - Strong positive contribution
-   **Petal width: 0.565** - Strong positive contribution










::: {.cell}

```{.r .cell-code}
# What does PC1 represent?
pc1_loadings <- iris_pca$rotation[, 1]
print("PC1 Loadings (all variables contribute similarly):")
```

::: {.cell-output .cell-output-stdout}

```
[1] "PC1 Loadings (all variables contribute similarly):"
```


:::

```{.r .cell-code}
round(pc1_loadings, 3)
```

::: {.cell-output .cell-output-stdout}

```
sepal_length  sepal_width petal_length  petal_width 
       0.521       -0.269        0.580        0.565 
```


:::

```{.r .cell-code}
cat("\nPC1 Interpretation: Overall flower size")
```

::: {.cell-output .cell-output-stdout}

```

PC1 Interpretation: Overall flower size
```


:::

```{.r .cell-code}
cat("\n- All variables have similar negative loadings")
```

::: {.cell-output .cell-output-stdout}

```

- All variables have similar negative loadings
```


:::

```{.r .cell-code}
cat("\n- Higher PC1 values = smaller flowers overall")
```

::: {.cell-output .cell-output-stdout}

```

- Higher PC1 values = smaller flowers overall
```


:::

```{.r .cell-code}
cat("\n- Lower PC1 values = larger flowers overall")
```

::: {.cell-output .cell-output-stdout}

```

- Lower PC1 values = larger flowers overall
```


:::
:::










# PC1 Interpretation: Overall Flower Size

### PC1 Interpretation: Overall flower size (with a twist)

Note: The output says "all variables have similar negative loadings" but
the actual values show mostly positive loadings. This is likely due to a
sign flip - PCA signs can be arbitrary. Let's interpret based on the
actual values shown:

-   **Three variables (sepal length, petal length, petal width) have
    similar positive loadings** (\~0.52-0.58)
-   **Sepal width has a negative loading** (-0.269)
-   This means PC1 captures flowers where length and width measurements
    (except sepal width) vary together

### What PC1 scores mean:

-   **Higher PC1 values** = Longer petals, longer sepals, wider petals,
    but narrower sepals
-   **Lower PC1 values** = Shorter petals, shorter sepals, narrower
    petals, but wider sepals
-   PC1 essentially captures "overall flower size except sepal width
    goes opposite"

### Biological interpretation:

PC1 distinguishes between:

-   Small flowers with relatively wide sepals (negative PC1) - typical
    of setosa
-   Large flowers with relatively narrow sepals (positive PC1) - typical
    of virginica

# Step 8: Interpret PC2 Results

### Understanding PC2 Loadings:

The loadings show how each original variable contributes to PC2:

-   **Sepal length: -0.377** - Moderate negative contribution
-   **Sepal width: -0.923** - Very strong negative contribution
-   **Petal length: -0.024** - Almost no contribution
-   **Petal width: -0.067** - Very small negative contribution










::: {.cell}

```{.r .cell-code}
# What does PC2 represent?
pc2_loadings <- iris_pca$rotation[, 2]
print("PC2 Loadings:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "PC2 Loadings:"
```


:::

```{.r .cell-code}
round(pc2_loadings, 3)
```

::: {.cell-output .cell-output-stdout}

```
sepal_length  sepal_width petal_length  petal_width 
      -0.377       -0.923       -0.024       -0.067 
```


:::

```{.r .cell-code}
cat("\nPC2 Interpretation: Flower shape contrast")
```

::: {.cell-output .cell-output-stdout}

```

PC2 Interpretation: Flower shape contrast
```


:::

```{.r .cell-code}
cat("\n- Positive loadings: sepal width")
```

::: {.cell-output .cell-output-stdout}

```

- Positive loadings: sepal width
```


:::

```{.r .cell-code}
cat("\n- Negative loadings: petal length and width, sepal length")
```

::: {.cell-output .cell-output-stdout}

```

- Negative loadings: petal length and width, sepal length
```


:::

```{.r .cell-code}
cat("\n- Higher PC2 = wider sepals relative to petal size")
```

::: {.cell-output .cell-output-stdout}

```

- Higher PC2 = wider sepals relative to petal size
```


:::

```{.r .cell-code}
cat("\n- Lower PC2 = longer/wider petals relative to sepal width")
```

::: {.cell-output .cell-output-stdout}

```

- Lower PC2 = longer/wider petals relative to sepal width
```


:::
:::










# PC2 Interpretation: Flower Shape Contrast

### PC2 Interpretation: Correcting the output

Note: The output says "Positive loadings: sepal width" but the actual
value is -0.923 (negative). All loadings are actually negative, with
sepal width being the most strongly negative.

### What PC2 actually represents:

-   **All variables have negative loadings**, but sepal width is
    dominant (-0.923)
-   **Petal measurements contribute very little** (-0.024 and -0.067)
-   **This component is primarily driven by sepal width**, with some
    contribution from sepal length

### What PC2 scores mean:

-   **Higher PC2 values** = Smaller measurements overall, especially
    narrow sepals
-   **Lower PC2 values** = Larger measurements overall, especially wide
    sepals
-   Since sepal width has the strongest loading, PC2 primarily captures
    sepal width variation

### Biological interpretation:

PC2 helps distinguish:

-   Flowers with narrow sepals and smaller overall size (positive PC2)
-   Flowers with wide sepals and larger overall size (negative PC2)
-   This dimension helps separate species that have similar PC1 scores
    but different sepal proportions

# Step 9: How Well Does PCA Work?










::: {.cell}

```{.r .cell-code}
# Calculate total variance explained by first 2 components
variance_explained_2pc <- sum(prop_variance[1:2])
caption_pca <- paste("Variance explained by first 2 components:", round(variance_explained_2pc * 100, 1), "%")

# This means we reduced 4 variables to 2 components while retaining most information!

# Create a summary plot showing dimension reduction success
tibble(
  Component = factor(paste0("PC", 1:4), levels = paste0("PC", 1:4)),
  Variance = prop_variance * 100,
  Cumulative = cumsum_variance * 100
) %>% 
  ggplot(aes(x = Component)) +
  geom_col(aes(y = Variance), fill = "lightblue", alpha = 0.7) +
  geom_line(aes(y = Cumulative, group = 1), color = "red", size = 1) +
  geom_point(aes(y = Cumulative), color = "red", size = 3) +
  labs(title = "PCA Dimension Reduction Success",
       subtitle = "Blue bars = individual variance, Red line = cumulative variance",
       caption = caption_pca,
       x = "Principal Component", 
       y = "Percentage of Variance Explained") +
  theme_minimal()
```
:::










# Summary: What We Learned

## Key Findings:

1.  **Successful dimension reduction**: 4 variables → 2 components
    explaining \~96% of variance

2.  **PC1 (72.8% variance)**: Overall flower size

    -   All measurements contribute similarly
    -   Separates large from small flowers

3.  **PC2 (23.1% variance)**: Shape contrast

    -   Sepal width vs. petal dimensions
    -   Separates flower shape types

4.  **Species separation**: PCA naturally groups the three iris species
    based on their morphological differences

## PCA Success Criteria Met:

✓ Variables were correlated\
✓ Linear relationships\
✓ No major outliers\
✓ Adequate sample size\
✓ Clear dimension reduction\
✓ Interpretable components

# When to Use PCA vs. Other Methods

## Use PCA when:

-   Variables are **continuous and correlated**
-   Goal is **dimension reduction** or **data exploration**
-   Linear relationships between variables
-   Want to **remove redundancy** in measurements

## Consider alternatives when:

-   Variables are categorical → use MCA (Multiple Correspondence
    Analysis)
-   Focus on **species composition** → use ordination methods like NMDS
-   Want to **classify/predict** → use discriminant analysis or machine
    learning

**PCA is excellent for exploring patterns in biological measurements
like morphology, physiology, or environmental variables!**
