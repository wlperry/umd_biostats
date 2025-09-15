---
title: "05_homeowork_correlation_regression"
author: "Bill Perry"
metadata-files:
  - ../_templates/homeworks.yml
format:
  html:
    output-file: "05_03_homework_correlation_regression_html.html"
  typst:
    output-file: "05_homeowork_correlation_regression.pdf"
---








# Assignment Overview

This homework assignment will test your understanding of correlation and regression analysis using real forest inventory data from European beech (*Fagus sylvatica*) trees. You will apply the statistical concepts from Lectures 9 and 10 to analyze relationships between tree measurements.

## Learning Objectives

By completing this assignment, you will be able to:

1.  **Distinguish between correlation and regression analysis**
2.  **Perform and interpret correlation analyses**
3.  **Conduct simple linear regression**
4.  **Test statistical assumptions**
5.  **Create publication-quality figures**

## Data Description

The dataset `fagus_data.csv` contains forest inventory measurements from European beech trees across different regions and time periods. Key variables include:

-   `diameter_mm`: Tree diameter at breast height (millimeters)
-   `stem_volume_m3`: Calculated stem volume (cubic meters)
-   `tree_age`: Estimated tree age (years)
-   `year_of_inventory`: Year when measurements were taken
-   `region`: Geographic region
-   `period`: Time period classification
-   `landuse_code`: Land use classification code
-   `landuse_char`: Land use description
-   `within_formally_protected_areas`: Protection status (0/1)

------------------------------------------------------------------------

# Part 1: Data Loading and Initial Exploration

## 1.1 Load and Examine the Data






::: {.cell}

```{.r .cell-code}
# Load the Fagus data from jacobsson_et_al_fagus_data.csv
```
:::

::: {.cell}

```{.r .cell-code}
# summary(fagus_data)
```
:::






## 1.2 Initial Data Cleaning and Preparation






::: {.cell}

:::






------------------------------------------------------------------------

# Part 2: Descriptive Statistics and Data Exploration

## 2.1 Summary Statistics for Key Variables

::: callout-important
## Question 1: Summary Statistics

**Task:** Calculate and interpret summary statistics for the three main continuous variables: diameter, volume, and age.

**Instructions:**

-   Use appropriate R functions to calculate mean, median, standard deviation, and quartiles
-   Comment on the distribution shape (symmetric, skewed) based on these statistics
-   Identify any potential outliers or unusual values
:::






::: {.cell}

```{.r .cell-code}
# YOUR CODE HERE: Calculate summary statistics for diameter_mm, stem_volume_m3, and tree_age

# Example structure (replace with your code):
# summary_stats <- fagus_clean %>%
#   select(diameter_mm, stem_volume_m3, tree_age) %>%
#   [YOUR SUMMARY FUNCTION]

# Your interpretation:
# [Write 2-3 sentences about what the summary statistics tell you about each variable]
```
:::






## 2.2 Graphical Data Exploration

::: callout-important
## Question 2: Data Visualization

**Task:** Create appropriate plots to visualize the distribution of each variable and relationships between variables.

**Instructions:**

-   Create histograms for each continuous variable
-   Create a pairs plot or correlation matrix plot
-   Comment on distributions and potential relationships
:::






::: {.cell}

```{.r .cell-code}
# YOUR CODE HERE: Create exploratory plots

# Hint: You might want to create:
# 1. Histograms for each variable
# 2. A pairs plot using GGally::ggpairs()
# 3. Individual scatterplots for key relationships

# Your interpretation:
# [Write 3-4 sentences describing what you observe in the plots]
```
:::






------------------------------------------------------------------------

# Part 3: Correlation Analysis

## 3.1 Correlation Between Diameter and Volume

::: callout-important
## Question 3: Correlation Analysis Setup

**Task:** Before conducting the correlation analysis, clearly state:

1.  **Type of analysis:** What type of statistical test are you using and why?
2.  **Hypotheses:** State your null and alternative hypotheses
3.  **Assumptions:** List the assumptions for this test
4.  **Variables:** Identify which variables you're analyzing and their measurement scales
:::

**Your Answer:**

**Type of Analysis:** \[State whether this is a correlation or regression analysis and explain why this choice is appropriate\]

**Hypotheses:**

-   H₀: \[State your null hypothesis\]
-   H₁: \[State your alternative hypothesis\]

**Assumptions:**

**Variables:**

-   Variable 1: \[Name and describe\]
-   Variable 2: \[Name and describe\]

## 3.2 Perform Correlation Analysis






::: {.cell}

```{.r .cell-code}
# YOUR CODE HERE: Perform correlation analysis between diameter and volume

# Steps to include:
# 1. Calculate correlation coefficient
# 2. Perform correlation test
# 3. Check assumptions (normality tests, scatter plot)
# 4. Consider non-parametric alternative if needed

# Your code here:
```
:::






## 3.3 Interpret Correlation Results

::: callout-important
## Question 4: Correlation Interpretation

**Task:** Interpret your correlation results by addressing:

1.  **Strength and direction:** How strong is the relationship and in what direction?
2.  **Statistical significance:** Is the correlation statistically significant?
3.  **Practical significance:** What does this mean biologically/ecologically?
4.  **Limitations:** What are the limitations of this analysis?
:::

**Your Interpretation:**

\[Write 4-5 sentences interpreting your correlation results, addressing all the points above\]

------------------------------------------------------------------------

# Part 4: Regression Analysis

## 4.1 Regression Analysis Setup

::: callout-important
## Question 5: Regression Analysis Setup

**Task:** For the regression of diameter on tree age, clearly state:

1.  **Type of analysis:** What type of statistical analysis are you using?
2.  **Model equation:** Write out your regression model
3.  **Hypotheses:** State your null and alternative hypotheses about the slope
4.  **Variables:** Clearly identify your predictor and response variables
:::

**Your Answer:**

**Type of Analysis:** \[State this is simple linear regression and explain the goal\]

**Model Equation:** \[Write the regression equation: Y = α + βX + ε, defining each term\]

**Hypotheses:**

-   H₀: \[State null hypothesis about the slope\]
-   H₁: \[State alternative hypothesis about the slope\]

**Variables:**

-   Predictor (X): \[Name and describe\]
-   Response (Y): \[Name and describe\]

## 4.2 Perform Regression Analysis






::: {.cell}

```{.r .cell-code}
# YOUR CODE HERE: Perform regression analysis of diameter on tree age

# Steps to include:
# 1. Fit the linear model
# 2. Get model summary
# 3. Create ANOVA table
# 4. Check assumptions with diagnostic plots
# 5. Perform formal assumption tests

# Your code here:
```
:::






## 4.3 Test Regression Assumptions






::: {.cell}

```{.r .cell-code}
# YOUR CODE HERE: Create diagnostic plots and test assumptions

# Include:
# 1. Residuals vs fitted plot
# 2. Q-Q plot for normality
# 3. Scale-location plot for homoscedasticity
# 4. Influence plot (Cook's distance)
# 5. Formal tests (Shapiro-Wilk, Breusch-Pagan)

# Your code here:
```
:::






## 4.4 Interpret Regression Results

::: callout-important
## Question 6: Regression Interpretation

**Task:** Provide a complete interpretation of your regression results:

1.  **Parameter estimates:** What are the slope and intercept estimates and their meanings?
2.  **Statistical significance:** Is the relationship statistically significant?
3.  **Model fit:** How much variance is explained (R²)?
4.  **Assumptions:** Were the assumptions met? Any concerns?
5.  **Biological interpretation:** What does this relationship mean for forest ecology?
:::

**Your Interpretation:**

\[Write a interpretation of your regression results, addressing all points above\]

------------------------------------------------------------------------

# Part 5: Publication-Quality Figure and Write-up

## 5.1 Create Publication-Quality Figure

::: callout-important
## Question 7: Publication Figure

**Task:** Create a publication-quality figure that effectively displays your regression results.

**Requirements:**

-   Professional appearance with clear labels and appropriate sizing
-   Include regression line with confidence interval
-   Use appropriate colors and themes
-   Include informative title and axis labels
-   Consider the target audience (scientific journal)
:::






::: {.cell}

```{.r .cell-code}
# YOUR CODE HERE: Create publication-quality figure

# Your code here:
```
:::






# Submission Guidelines

## What to turn in -

1.  a quarto markdown file and dataframe. Note that your code should be able to run with what you turn in.

2.  a self-contained word and html file showing the code and output

3.  annotations in the quarto file that shows or tells what is being done in the r code chunks describing what you are trying to do - credit will be given even if it does not work as long as you detail what you are doing. As we start to move into more statistics you will be expected to interpret the results.

## Points

-   summary stats - 1 points
-   exploratory graphs - 1 point
-   Hypotheses - 2 points
-   Assumptions - 2 points
-   Model and Test Results - 4
-   interpretation - 3 points
-   Assumption Tests - 4 points
-   Final figure - 3 points