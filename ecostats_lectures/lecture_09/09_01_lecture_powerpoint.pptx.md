---
title: "Lecture 09 Correlation and Regression"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "09_02_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  revealjs:
    output-file: "09_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "09_01_lecture_powerpoint.docx"
  pptx:
    output-file: "09_01_lecture_powerpoint.pptx"
  typst:
    output-file: "09_01_lecture_powerpoint.pdf"
---



# Lecture 8: Review

::::: columns
::: {.column width="60%"}
Covered

-   Study design
-   Causality in ecology
-   Experimental design:
    -   Replication, controls, randomization, independence
-   Sampling in field studies
-   Power analysis: *a priori* and *post hoc*
-   Study design and analysis
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/review-plot-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Overview

### The objectives:

This lecture covers two fundamental statistical techniques in biology:
correlation and regression analysis. Based on Chapters 16-17 from
Whitlock & Schluter's *The Analysis of Biological Data* (3rd edition),
we'll explore:

-   Correlation analysis: measuring relationships between variables
-   The distinction between correlation and regression
-   Simple linear regression: predicting one variable from another
-   Estimating and interpreting regression parameters
-   Testing assumptions and handling violations
-   Analysis of variance in regression
-   Model selection and comparison

# **Lecture 9:** Correlation vs. Regression:

### What's the Difference?

::::: columns
::: {.column width="60%"}
### **Correlation Analysis:**

-   Measures the strength and direction of a relationship between two
    numerical variables
-   Both X and Y are random variables (both measured, neither
    manipulated)
-   Variables are typically on equal footing (either could be X or Y)
-   No cause-effect relationship implied
-   Quantifies the degree to which variables are related
-   Expressed as a correlation coefficient (r) from -1 to +1

### **Regression Analysis:**

-   Predicts one variable (Y) from another (X)
-   X is often fixed or controlled (manipulated)
-   Y is the response variable of interest
-   Often implies a cause-effect relationship
-   Produces an equation for prediction with slope and intercept
    parameters
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-1-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Correlation Analysis

::::: columns
::: {.column width="60%"}
### What Is Correlation?

**Correlation analysis** measures the strength and direction of a
relationship between two numerical variables:

-   Ranges from -1 to +1
-   +1 indicates perfect positive correlation
-   0 indicates no correlation
-   -1 indicates perfect negative correlation

The **Pearson correlation coefficient (r)** is defined as:

$$r = \frac{\sum_{i}(X_i - \bar{X})(Y_i - \bar{Y})}{\sqrt{\sum_{i}(X_i - \bar{X})^2 \sum_{i}(Y_i - \bar{Y})^2}}$$

This can be simplified as:

$$r = \frac{\text{Covariance}(X, Y)}{s_X \cdot s_Y}$$

Where $s_X$ and $s_Y$ are the standard deviations of X and Y.
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1i-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Correlation Analysis

### Example 16.1: Flipping the Bird

Nazca boobies (*Sula granti*) - Do aggressive behaviors as a chick
predict future aggressive behavior as an adult?

-   correlation is r = 0.534 - moderate positive relationship
-   p-value = 0.007 correlation is statistically significant.

For a Pearson correlation coefficient (r) of 0.53372:

-   This is r (not rho as Spearman nonparametric below), as indicated by
    "cor" in your output
-   To determine the amount of variation explained, you square this
    value: r² = 0.53372² = 0.2849 (or approximately 28.49%)
-   means about 28.49% of the variance in one variable can be explained
    by the other variable

### Note that it is tested by a t test: $\text{t}=\frac{r}{SE_r}$


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Pearson's product-moment correlation

data:  booby_data$visits_as_nestling and booby_data$future_aggression
t = 2.9603, df = 22, p-value = 0.007229
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.1660840 0.7710999
sample estimates:
      cor 
0.5337225 
```


:::
:::


# **Lecture 9:** Correlation Analysis

::::: columns
::: {.column width="60%"}
### Example 16.1: Flipping the Bird

**Interpretation:** The correlation coefficient of r = 0.534 suggests
that Nazca boobies who experienced more visits from non-parent adults as
nestlings tend to display more aggressive behavior as adults. This
supports the hypothesis that early experiences influence adult behavior
patterns in this species.

**Standard Error:**

### $\text{SE}_r = \sqrt{\frac{1-r^2}{n-2}}$

### SE = 0.180

Need to be sure relationship is not curved - note below
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1k-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Correlation Analysis

::::: columns
::: {.column width="60%"}
### Testing Assumptions for Correlation

As described in Section 16.3, correlation analysis has key assumptions:

1.  **Random sampling**: Observations should be a random sample from the
    population
2.  **Bivariate normality**: Both variables follow a normal
    distribution, and their joint distribution is bivariate normal
3.  **Linear relationship**: The relationship between variables is
    linear, not curved

Let's check these assumptions booby data:
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  booby_data$visits_as_nestling
W = 0.95783, p-value = 0.3965
```


:::

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  booby_data$future_aggression
W = 0.91575, p-value = 0.04709
```


:::
:::

:::
:::::

# **Lecture 9:** Correlation Analysis

::::: columns
::: {.column width="60%"}
### Testing Assumptions for Correlation

As described in Section 16.3, correlation analysis has key assumptions:

1.  **Random sampling**: Observations should be a random sample from the
    population
2.  **Bivariate normality**: Both variables follow a normal
    distribution, and their joint distribution is bivariate normal
3.  **Linear relationship**: The relationship between variables is
    linear, not curved

Let's check these assumptions using the booby data
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1m-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Correlation Analysis

### **What to do if assumptions are violated:**

Transform one or both variables (log, square root, etc.)

Use non-parametric correlation (**Spearman's rank correlation**) or
Kendall's tau 𝛕

Examine the data for outliers or influential points


::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-2m-1.png)
:::
:::


# **Lecture 9:** Correlation Analysis

To understand the amount of variation explained, you can square the non
parametric Spearman's rho value.

For your value of 0.74485:

ρ² = 0.74485² = 0.5548

This means approximately 55.48% of the variance in ranks of one variable
can be explained by the ranks of the other variable. This is similar to
how R² works in linear regression, but specifically for ranked data.

::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Spearman's rank correlation rho

data:  lion_data$proportion_black and lion_data$age_years
S = 1392.1, p-value = 1.013e-06
alternative hypothesis: true rho is not equal to 0
sample estimates:
      rho 
0.7448561 
```


:::
:::

# **Lecture 9:** Correlation Analysis

::::: columns
::: {.column width="60%"}
### Correlation: Important Considerations

-   **The correlation coefficient depends on the range**
    -   Restricting range of values can reduce the correlation
        coefficient
    -   Comparing correlations between studies requires similar ranges
        of values
-   **Measurement error affects correlation**
    -   Measurement error in X or Y tends to weaken observed correlation
    -   This bias is called **attenuation**
    -   True correlation typically stronger than observed correlation
-   **Correlation vs. Causation**
    -   Correlation does not imply causation
    -   Three possible explanations for correlation:
        1.  X causes Y
        2.  Y causes X
        3.  Z (a third variable) causes both X and Y
-   **Correlation significance test**
    -   H₀: ρ = 0 (no correlation in population)
    -   H₁: ρ ≠ 0 (correlation exists in population)
    -   **Test statistic: t = r / SE(r) with df = n-2**
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1o-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Simple Linear Regression Model

**Simple linear regression** models the relationship between a response
variable (Y) and a predictor variable (X).

The **population** regression model $Y = \alpha + \beta X + \varepsilon$

-   Where:
    -   Y is the response variable
    -   X is the predictor variable
    -   α (alpha) is the intercept (value of Y when X=0)
    -   β (beta) is the slope (change in Y per unit change in X)
    -   ε (epsilon) is the error term (random deviation from the line)

The **sample** regression equation is: $\hat{Y} = a + bX$

-   Where:
    -   $\hat{Y}$ is the predicted value of Y
    -   a is the estimate of α (intercept)
    -   b is the estimate of β (slope)

**Method of Least Squares**: The line is chosen to minimize the sum of
squared vertical distances (residuals) between observed and predicted Y
values.
:::

::: {.column width="40%"}
![](images/clipboard-3360244964.png){width="238"}
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Simple Linear Regression Model

**Simple linear regression** models the relationship between a response
variable (Y) and a predictor variable (X).

The **population** regression model is:
$Y = \alpha + \beta X + \varepsilon$

-   Where:
    -   Y is the response variable
    -   X is the predictor variable
    -   α (alpha) is the intercept (value of Y when X=0)
    -   β (beta) is the slope (change in Y per unit change in X)
    -   ε (epsilon) is the error term (random deviation from the line)

The **sample** regression equation is: $\hat{Y} = a + bX$

-   Where:
    -   $\hat{Y}$ is the predicted value of Y
    -   a is the estimate of α (intercept)
    -   b is the estimate of β (slope)

**Method of Least Squares**: The line is chosen to minimize the sum of
squared vertical distances (residuals) between observed and predicted Y
values.
:::

::: {.column width="40%"}
::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1p-1.png)
:::
:::
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="40%"}
From Example 17.1 in the textbook the regression line for the lion data
is:

### $\text{age} = 0.88 + 10.65 \times \text{proportion}_{black}$

This means:

-   When a lion has no black on its nose (proportion = 0), its predicted
    age is 0.88 years
-   For each 0.1 increase in the proportion of black, age increases by
    1.065 years
-   The slope (10.65) indicates that lions with more black on their
    noses tend to be older
:::

::: {.column width="60%"}

::: {.cell}
::: {.cell-output .cell-output-stdout}

```
$data
# A tibble: 32 × 2
   proportion_black age_years
              <dbl>     <dbl>
 1             0.21       1.1
 2             0.14       1.5
 3             0.11       1.9
 4             0.13       2.2
 5             0.12       2.6
 6             0.13       3.2
 7             0.12       3.2
 8             0.18       2.9
 9             0.23       2.4
10             0.22       2.1
# ℹ 22 more rows

$layers
$layers[[1]]
geom_point: na.rm = FALSE
stat_identity: na.rm = FALSE
position_identity 

$layers[[2]]
geom_smooth: se = FALSE, na.rm = FALSE, orientation = NA
stat_smooth: method = lm, formula = NULL, se = FALSE, n = 80, fullrange = TRUE, level = 0.95, na.rm = FALSE, orientation = NA, method.args = list(), span = 0.75, xseq = c(-0.1, 1)
position_identity 

$layers[[3]]
mapping: x = ~proportion_black[10], y = ~age_years[10], xend = ~proportion_black[10], yend = ~predict(lion_model)[10] 
geom_segment: arrow = NULL, arrow.fill = NULL, lineend = butt, linejoin = round, na.rm = FALSE
stat_identity: na.rm = FALSE
position_identity 

$layers[[4]]
mapping: x = ~x, y = ~y 
geom_text: na.rm = FALSE
stat_identity: na.rm = FALSE
position_identity 

$layers[[5]]
mapping: x = ~x, y = ~y 
geom_text: na.rm = FALSE
stat_identity: na.rm = FALSE
position_identity 

$layers[[6]]
mapping: x = ~x, y = ~y 
geom_text: na.rm = FALSE
stat_identity: na.rm = FALSE
position_identity 

$layers[[7]]
mapping: x = ~x, y = ~y 
geom_text: na.rm = FALSE
stat_identity: na.rm = FALSE
position_identity 


$scales
<ggproto object: Class ScalesList, gg>
    add: function
    add_defaults: function
    add_missing: function
    backtransform_df: function
    clone: function
    find: function
    get_scales: function
    has_scale: function
    input: function
    map_df: function
    n: function
    non_position_scales: function
    scales: list
    set_palettes: function
    train_df: function
    transform_df: function
    super:  <ggproto object: Class ScalesList, gg>

$guides
<Guides[0] ggproto object>

<empty>

$mapping
$x
<quosure>
expr: ^proportion_black
env:  global

$y
<quosure>
expr: ^age_years
env:  global

attr(,"class")
[1] "uneval"

$theme
$line
$colour
[1] "black"

$linewidth
[1] 0.5

$linetype
[1] 1

$lineend
[1] "butt"

$arrow
[1] FALSE

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_line" "element"     

$rect
$fill
[1] "white"

$colour
[1] "black"

$linewidth
[1] 0.5

$linetype
[1] 1

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_rect" "element"     

$text
$family
[1] ""

$face
[1] "plain"

$colour
[1] "black"

$size
[1] 11

$hjust
[1] 0.5

$vjust
[1] 0.5

$angle
[1] 0

$lineheight
[1] 0.9

$margin
[1] 0points 0points 0points 0points

$debug
[1] FALSE

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$title
NULL

$aspect.ratio
NULL

$axis.title
NULL

$axis.title.x
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
[1] 1

$angle
NULL

$lineheight
NULL

$margin
[1] 2.75points 0points    0points    0points   

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.title.x.top
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
[1] 0

$angle
NULL

$lineheight
NULL

$margin
[1] 0points    0points    2.75points 0points   

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.title.x.bottom
NULL

$axis.title.y
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
[1] 1

$angle
[1] 90

$lineheight
NULL

$margin
[1] 0points    2.75points 0points    0points   

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.title.y.left
NULL

$axis.title.y.right
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
[1] 1

$angle
[1] -90

$lineheight
NULL

$margin
[1] 0points    0points    0points    2.75points

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.text
$family
NULL

$face
NULL

$colour
[1] "grey30"

$size
[1] 0.8 *

$hjust
NULL

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
NULL

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.text.x
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
[1] 1

$angle
NULL

$lineheight
NULL

$margin
[1] 2.2points 0points   0points   0points  

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.text.x.top
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
[1] 0

$angle
NULL

$lineheight
NULL

$margin
[1] 0points   0points   2.2points 0points  

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.text.x.bottom
NULL

$axis.text.y
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
[1] 1

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
[1] 0points   2.2points 0points   0points  

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.text.y.left
NULL

$axis.text.y.right
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
[1] 0

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
[1] 0points   0points   0points   2.2points

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.text.theta
NULL

$axis.text.r
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
[1] 0.5

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
[1] 0points   2.2points 0points   2.2points

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$axis.ticks
list()
attr(,"class")
[1] "element_blank" "element"      

$axis.ticks.x
NULL

$axis.ticks.x.top
NULL

$axis.ticks.x.bottom
NULL

$axis.ticks.y
NULL

$axis.ticks.y.left
NULL

$axis.ticks.y.right
NULL

$axis.ticks.theta
NULL

$axis.ticks.r
NULL

$axis.minor.ticks.x.top
NULL

$axis.minor.ticks.x.bottom
NULL

$axis.minor.ticks.y.left
NULL

$axis.minor.ticks.y.right
NULL

$axis.minor.ticks.theta
NULL

$axis.minor.ticks.r
NULL

$axis.ticks.length
[1] 2.75points

$axis.ticks.length.x
NULL

$axis.ticks.length.x.top
NULL

$axis.ticks.length.x.bottom
NULL

$axis.ticks.length.y
NULL

$axis.ticks.length.y.left
NULL

$axis.ticks.length.y.right
NULL

$axis.ticks.length.theta
NULL

$axis.ticks.length.r
NULL

$axis.minor.ticks.length
[1] 0.75 *

$axis.minor.ticks.length.x
NULL

$axis.minor.ticks.length.x.top
NULL

$axis.minor.ticks.length.x.bottom
NULL

$axis.minor.ticks.length.y
NULL

$axis.minor.ticks.length.y.left
NULL

$axis.minor.ticks.length.y.right
NULL

$axis.minor.ticks.length.theta
NULL

$axis.minor.ticks.length.r
NULL

$axis.line
list()
attr(,"class")
[1] "element_blank" "element"      

$axis.line.x
NULL

$axis.line.x.top
NULL

$axis.line.x.bottom
NULL

$axis.line.y
NULL

$axis.line.y.left
NULL

$axis.line.y.right
NULL

$axis.line.theta
NULL

$axis.line.r
NULL

$legend.background
list()
attr(,"class")
[1] "element_blank" "element"      

$legend.margin
[1] 5.5points 5.5points 5.5points 5.5points

$legend.spacing
[1] 11points

$legend.spacing.x
NULL

$legend.spacing.y
NULL

$legend.key
list()
attr(,"class")
[1] "element_blank" "element"      

$legend.key.size
[1] 1.2lines

$legend.key.height
NULL

$legend.key.width
NULL

$legend.key.spacing
[1] 5.5points

$legend.key.spacing.x
NULL

$legend.key.spacing.y
NULL

$legend.frame
NULL

$legend.ticks
NULL

$legend.ticks.length
[1] 0.2 *

$legend.axis.line
NULL

$legend.text
$family
NULL

$face
NULL

$colour
NULL

$size
[1] 0.8 *

$hjust
NULL

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
NULL

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$legend.text.position
NULL

$legend.title
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
[1] 0

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
NULL

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$legend.title.position
NULL

$legend.position
[1] "right"

$legend.position.inside
NULL

$legend.direction
NULL

$legend.byrow
NULL

$legend.justification
[1] "center"

$legend.justification.top
NULL

$legend.justification.bottom
NULL

$legend.justification.left
NULL

$legend.justification.right
NULL

$legend.justification.inside
NULL

$legend.location
NULL

$legend.box
NULL

$legend.box.just
NULL

$legend.box.margin
[1] 0cm 0cm 0cm 0cm

$legend.box.background
list()
attr(,"class")
[1] "element_blank" "element"      

$legend.box.spacing
[1] 11points

$panel.background
list()
attr(,"class")
[1] "element_blank" "element"      

$panel.border
list()
attr(,"class")
[1] "element_blank" "element"      

$panel.spacing
[1] 5.5points

$panel.spacing.x
NULL

$panel.spacing.y
NULL

$panel.grid
$colour
[1] "grey92"

$linewidth
NULL

$linetype
NULL

$lineend
NULL

$arrow
[1] FALSE

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_line" "element"     

$panel.grid.major
NULL

$panel.grid.minor
$colour
NULL

$linewidth
[1] 0.5 *

$linetype
NULL

$lineend
NULL

$arrow
[1] FALSE

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_line" "element"     

$panel.grid.major.x
NULL

$panel.grid.major.y
NULL

$panel.grid.minor.x
NULL

$panel.grid.minor.y
NULL

$panel.ontop
[1] FALSE

$plot.background
list()
attr(,"class")
[1] "element_blank" "element"      

$plot.title
$family
NULL

$face
NULL

$colour
NULL

$size
[1] 1.2 *

$hjust
[1] 0

$vjust
[1] 1

$angle
NULL

$lineheight
NULL

$margin
[1] 0points   0points   5.5points 0points  

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$plot.title.position
[1] "panel"

$plot.subtitle
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
[1] 0

$vjust
[1] 1

$angle
NULL

$lineheight
NULL

$margin
[1] 0points   0points   5.5points 0points  

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$plot.caption
$family
NULL

$face
NULL

$colour
NULL

$size
[1] 0.8 *

$hjust
[1] 1

$vjust
[1] 1

$angle
NULL

$lineheight
NULL

$margin
[1] 5.5points 0points   0points   0points  

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$plot.caption.position
[1] "panel"

$plot.tag
$family
NULL

$face
NULL

$colour
NULL

$size
[1] 1.2 *

$hjust
[1] 0.5

$vjust
[1] 0.5

$angle
NULL

$lineheight
NULL

$margin
NULL

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$plot.tag.position
[1] "topleft"

$plot.tag.location
NULL

$plot.margin
[1] 5.5points 5.5points 5.5points 5.5points

$strip.background
list()
attr(,"class")
[1] "element_blank" "element"      

$strip.background.x
NULL

$strip.background.y
NULL

$strip.clip
[1] "inherit"

$strip.placement
[1] "inside"

$strip.text
$family
NULL

$face
NULL

$colour
[1] "grey10"

$size
[1] 0.8 *

$hjust
NULL

$vjust
NULL

$angle
NULL

$lineheight
NULL

$margin
[1] 4.4points 4.4points 4.4points 4.4points

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$strip.text.x
NULL

$strip.text.x.bottom
NULL

$strip.text.x.top
NULL

$strip.text.y
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
NULL

$angle
[1] -90

$lineheight
NULL

$margin
NULL

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$strip.text.y.left
$family
NULL

$face
NULL

$colour
NULL

$size
NULL

$hjust
NULL

$vjust
NULL

$angle
[1] 90

$lineheight
NULL

$margin
NULL

$debug
NULL

$inherit.blank
[1] TRUE

attr(,"class")
[1] "element_text" "element"     

$strip.text.y.right
NULL

$strip.switch.pad.grid
[1] 2.75points

$strip.switch.pad.wrap
[1] 2.75points

attr(,"class")
[1] "theme" "gg"   
attr(,"complete")
[1] TRUE
attr(,"validate")
[1] TRUE

$coordinates
<ggproto object: Class CoordCartesian, Coord, gg>
    aspect: function
    backtransform_range: function
    clip: on
    default: FALSE
    distance: function
    draw_panel: function
    expand: TRUE
    is_free: function
    is_linear: function
    labels: function
    limits: list
    modify_scales: function
    range: function
    render_axis_h: function
    render_axis_v: function
    render_bg: function
    render_fg: function
    reverse: none
    setup_data: function
    setup_layout: function
    setup_panel_guides: function
    setup_panel_params: function
    setup_params: function
    train_panel_guides: function
    transform: function
    super:  <ggproto object: Class CoordCartesian, Coord, gg>

$facet
<ggproto object: Class FacetNull, Facet, gg>
    attach_axes: function
    attach_strips: function
    compute_layout: function
    draw_back: function
    draw_front: function
    draw_labels: function
    draw_panel_content: function
    draw_panels: function
    finish_data: function
    format_strip_labels: function
    init_gtable: function
    init_scales: function
    map_data: function
    params: list
    set_panel_size: function
    setup_data: function
    setup_panel_params: function
    setup_params: function
    shrink: TRUE
    train_scales: function
    vars: function
    super:  <ggproto object: Class FacetNull, Facet, gg>

$plot_env
<environment: R_GlobalEnv>

$layout
<ggproto object: Class Layout, gg>
    coord: NULL
    coord_params: list
    facet: NULL
    facet_params: list
    finish_data: function
    get_scales: function
    layout: NULL
    map_position: function
    panel_params: NULL
    panel_scales_x: NULL
    panel_scales_y: NULL
    render: function
    render_labels: function
    reset_scales: function
    resolve_label: function
    setup: function
    setup_panel_guides: function
    setup_panel_params: function
    train_position: function
    super:  <ggproto object: Class Layout, gg>

$labels
$labels$x
[1] "Proportion of black on nose"

$labels$y
[1] "Age (years)"

$labels$title
[1] "Lion Age vs. Nose Blackness"

$labels$xend
[1] "proportion_black[10]"

$labels$yend
[1] "predict(lion_model)[10]"


attr(,"class")
[1] "gg"     "ggplot"
```


:::
:::

:::
:::::

# **Lecture 9:** Linear Regression

### Simple Linear Regression Model

-   male lions develop more black pigmentation on their noses as they
    age.
-   can be used to estimate the age of lions in the field.


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = age_years ~ proportion_black, data = lion_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.5449 -1.1117 -0.5285  0.9635  4.3421 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)        0.8790     0.5688   1.545    0.133    
proportion_black  10.6471     1.5095   7.053 7.68e-08 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.669 on 30 degrees of freedom
Multiple R-squared:  0.6238,	Adjusted R-squared:  0.6113 
F-statistic: 49.75 on 1 and 30 DF,  p-value: 7.677e-08
```


:::
:::


# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Simple Linear Regression Model

The calculation for slope (b) is:\
$$b = \frac{\sum_i(X_i - \bar{X})(Y_i - \bar{Y})}{\sum_i(X_i - \bar{X})^2}$$

-   Given:

    -   \- $\bar{X} = 0.3222$

    -   \- $\bar{Y} = 4.3094$

    -   \- $\sum_i(X_i - \bar{X})^2 = 1.2221$

    -   \- $\sum_i(X_i - \bar{X})(Y_i - \bar{Y}) = 13.0123$

b = 13.0123 / 1.2221 = 10.647

Intercept (a):
$a = \bar{Y} - b\bar{X} = 4.3094 - 10.647(0.3222) = 0.879$
:::

::: {.column width="40%"}
::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1b-1.png)
:::
:::
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Simple Linear Regression Model

**Making predictions:**

To predict the age of a lion with 0.50 proportion of black on its nose:

$$\hat{Y} = 0.88 + 10.65(0.50) = 6.2 \text{ years}$$

**Confidence intervals vs. Prediction intervals:**

-   **Confidence interval**: Range for the mean age of all lions with
    0.50 black
-   **Prediction interval**: Range for an individual lion with 0.50
    black

Both intervals are narrowest near $\bar{X}$ and widen as X moves away
from the mean.
:::

::: {.column width="40%"}

::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1bb-1.png)
:::
:::

:::
:::::

# **Lecture 9:** Linear Regression

### Example Prairie Home Companion

-   Does biodiversity affect ecosystem stability?
-   Tilman et al. (2006) investigated using experimental plots varying
    plant species


::: {.cell}
::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = log_stability ~ species_number, data = prairie_data)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.82774 -0.25344 -0.00426  0.27498  0.75240 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    1.252629   0.041023  30.535  < 2e-16 ***
species_number 0.025984   0.004926   5.275 4.28e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3433 on 159 degrees of freedom
Multiple R-squared:  0.149,	Adjusted R-squared:  0.1436 
F-statistic: 27.83 on 1 and 159 DF,  p-value: 4.276e-07
```


:::

::: {.cell-output .cell-output-stdout}

```
Analysis of Variance Table

Response: log_stability
                Df  Sum Sq Mean Sq F value    Pr(>F)    
species_number   1  3.2792  3.2792  27.829 4.276e-07 ***
Residuals      159 18.7358  0.1178                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::


# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
The hypothesis test asks whether the slope equals zero:

-   H₀: β = 0 (species number does not affect stability)
-   H₁: β ≠ 0 (species number does affect stability)

t = estimate/SE

**Interpretation:**

The slope estimate is 0.033, indicating that log stability increases by
0.033 units for each additional plant species in the plot.

The p-value is very small (2.73e-10), providing strong evidence to
reject the null hypothesis that species number has no effect on
ecosystem stability.

R² = 0.222, meaning that approximately 22.2% of the variation in log
stability is explained by the number of plant species.

This supports the biodiversity-stability hypothesis: more diverse plant
communities have more stable biomass production over time.
:::

::: {.column width="40%"}
::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1d-1.png)
:::
:::
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Testing Regression Assumptions

linear regression has four key assumptions:

1.  **Linearity**: The relationship between X and Y is linear
2.  **Independence**: Observations are independent
3.  **Homoscedasticity**: Equal variance across all values of X
4.  **Normality**: Residuals are normally distributed

Let's check these assumptions for the lion regression model:\
\
Assume that **error 𝞮 i**s $e_i = y_i - \hat{y}_i$

-   normally distributed for each x~i~

-   has the same variance

-   has a mean of 0 at each xi
:::

::: {.column width="40%"}
![](images/clipboard-2092034232.png){width="387"}
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Testing Regression Assumptions

linear regression has four key assumptions:

1.  **Linearity**: The relationship between X and Y is linear
2.  **Independence**: Observations are independent
3.  **Homoscedasticity**: Equal variance across all values of X
4.  **Normality**: Residuals are normally distributed

Let's check these assumptions for the lion regression model:\
\
Assume that **error 𝞮 i**s - estimated as the residuals:
$e_i = y_i - \hat{y}_i$

-   ordinary lease square estimates a and b or slope and intercept to
    minimize the sum of the residuals squared as

### $\sum_{i=1}^{n} = (y_i - \hat{y}_i)^2$
:::

::: {.column width="40%"}
![](images/clipboard-1411052425.png)
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Testing Regression Assumptions

linear regression has four key assumptions:

1.  **Linearity**: The relationship between X and Y is linear
2.  **Independence**: Observations are independent
3.  **Homoscedasticity**: Equal variance across all values of X
4.  **Normality**: Residuals are normally distributed

Let's check these assumptions for the lion regression model:

Linearity - how does this plot work
:::

::: {.column width="40%"}
::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1e-1.png)
:::
:::
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Testing Regression Assumptions

linear regression has four key assumptions:

1.  **Linearity**: The relationship between X and Y is linear
2.  **Independence**: Observations are independent
3.  **Homoscedasticity**: Equal variance across all values of X
4.  **Normality**: Residuals are normally distributed

Let's check these assumptions for the lion regression model:
:::

::: {.column width="40%"}
::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1f-1.png)
:::
:::
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="60%"}
### Testing Regression Assumptions

linear regression has four key assumptions:

1.  **Linearity**: The relationship between X and Y is linear
2.  **Independence**: Observations are independent
3.  **Homoscedasticity**: Equal variance across all values of X
4.  **Normality**: Residuals are normally distributed

Let's check these assumptions for the lion regression model:
:::

::: {.column width="40%"}
::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(lion_model)
W = 0.93879, p-value = 0.0692
```


:::
:::
:::
:::::

# **Lecture 9:** Linear Regression

::::: columns
::: {.column width="40%"}
### Simple Linear Regression Model

linear regression has four key assumptions:

1.  **Linearity**: The relationship between X and Y is linear
2.  **Independence**: Observations are independent
3.  **Homoscedasticity**: Equal variance across all values of X
4.  **Normality**: Residuals are normally distributed

If assumptions are violated: 1. Transform the data (Section 17.6) 2. Use
weighted least squares for heteroscedasticity 3. Consider non-linear
models (Section 17.8)
:::

::: {.column width="60%"}
::: {.cell}
::: {.cell-output-display}
![](09_01_lecture_powerpoint_files/figure-pptx/overview-plot-1h-1.png)
:::
:::
:::
:::::

# **Lecture 9:** Linear Regression - estimates of error and significance

::::: columns
::: {.column width="60%"}
-   Estimates of standard error and confidence intervals for slow and
    intercept to determine confidence bands

-   the 95% confidence band will contain the true population line 95/100
    under repeated sampling

-   this is usually done in R
:::

::: {.column width="40%"}
![](images/clipboard-934594306.png)
:::
:::::

# **Lecture 9:** Linear Regression - estimates of error and significance

::::: columns
::: {.column width="60%"}
In addition to getting estimates of population parameters (β0 , β1),
want to test hypotheses about them

-   This is accomplished by analysis of variance
-   Partition variance in Y: due to variation in X, due to other things
    (error)
:::

::: {.column width="40%"}
![](images/clipboard-2817787184.png)
:::
:::::

# **Lecture 9:** Linear Regression - estimates of variance

::::: columns
::: {.column width="60%"}
Total variation in Y is “partitioned” into 3 components:

-   $SS_{regression}$: variation explained by regression
    -   difference between predicted values (ŷi ) and mean y (ȳ)
    -   dfs= 1 for simple linear (parameters-1)
-   $SS_{residual}$: variation not explained by regression
    -   difference between observed ($y_i$) and predicted ($\hat{y}_i$)
        values
    -   dfs= n-2
-   $SS_{total}$: total variation
    -   sum of squared deviations of each observation ($y_i$) from mean
        ($\bar{y}$)

    -   dfs = n-1
:::

::: {.column width="40%"}
![](images/clipboard-2817787184.png)
:::
:::::

# **Lecture 9:** Linear Regression - estimates of variance

::::: columns
::: {.column width="60%"}
Total variation in Y is “partitioned” into 3 components:

-   $SS_{regression}$: variation explained by regression
    -   difference between predicted values (ŷi ) and mean y (ȳ)
    -   dfs= 1 for simple linear (parameters-1)
-   $SS_{residual}$: variation not explained by regression
    -   difference between observed ($y_i$) and predicted ($\hat{y}_i$)
        values
    -   dfs= n-2
-   $SS_{total}$: total variation
    -   sum of squared deviations of each observation ($y_i$) from mean
        ($\bar{y}$)

    -   dfs = n-1
:::

::: {.column width="40%"}
![](images/clipboard-77063277.png){width="498"}
:::
:::::

# **Lecture 9:** Linear Regression - estimates of variance

::::: columns
::: {.column width="60%"}
Total variation in Y is “partitioned” into 3 components:

-   $SS_{regression}$: variation explained by regression
    -   GREATER IN C than D
-   $SS_{residual}$: variation not explained by regression
    -   GREATER IN B THAN A
-   $SS_{total}$: total variation
:::

::: {.column width="40%"}
![](images/clipboard-217151541.png){width="456"}
:::
:::::

# **Lecture 9:** Linear Regression - estimates of variance

::::: columns
::: {.column width="60%"}
Sums of Squares and degress of freedome are:

$SS_{regression} +SS_{residual} = SS_{total}$

$df_{regression}+df_{residual} = df_{total}$

-   Sums of Squares depends on n
-   We need a different estimate of variance
:::

::: {.column width="40%"}
![](images/clipboard-2758972276.png){width="541"}
:::
:::::

# **Lecture 9:** Linear Regression - estimates of variance

::::: columns
::: {.column width="60%"}
Sums of Squares converted to Mean Squares

-   Sums of Squares divided by degrees of freedom - does not depend on n
-   $MS_{residual}$: estimate population variation
-   $MS_{regression}$: estimate pop variation and variation due to X-Y
    relationship
-   Mean Squares are not additive
:::

::: {.column width="40%"}
![](images/clipboard-2758972276.png){width="505"}
:::
:::::
