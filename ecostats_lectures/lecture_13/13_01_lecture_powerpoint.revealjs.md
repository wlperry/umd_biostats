---
title: "Lecture 13 - Nested ANOVA"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "13_02_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  revealjs:
    output-file: "13_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "13_01_lecture_powerpoint.docx"
  pptx:
    output-file: "13_01_lecture_powerpoint.pptx"
---



# Lecture 13: Review

::::: columns
::: {.column width="60%"}
Multifactor ANOVA

-   Example
-   Linear model
-   Analysis of variance
-   Null hypotheses
-   Interactions and main effects
-   Unequal sample size
-   Assumptions
:::

::: {.column width="40%"}
![](images/clipboard-1642768928.png){width="244"}
:::
:::::

# Lecture 13: 2 Factor or 2 Way ANOVA

::::: columns
::: {.column width="60%"}
Often consider more than 1 factor (independent categorical variable):

-   reduce unexplained variance
-   look at interactions

2-factor designs (2-way ANOVA) very common in ecology

-   Can have more factors (e.g., 3-way ANOVA)
    -   interpretation tricky…

Most multifactor designs: nested or factorial
:::

::: {.column width="40%"}
![](images/clipboard-1173869376.png){width="164"}
:::
:::::

# Factorial Versus Nested Designs

::::: columns
::: {.column width="60%"}
Consider two factors: A and B

-   Factorial/crossed: every level of B in every level of A
-   Nested/hierarchical: levels of B occur only in 1 level of A
:::

::: {.column width="40%"}
![](images/clipboard-758019042.png){width="234"}
:::
:::::

# Lecture 13: Nested ANOVA Overview

Nested design examples

-   Nested designs
-   Linear model
-   Analysis of variance
-   Null hypotheses
-   Unbalanced designs
-   Assumptions

# Nested Designs Overview

::::: columns
::: {.column width="60%"}
Nested Designs:

-   Factor A usually fixed
-   Factor B usually random
:::

::: {.column width="40%"}
![](images/clipboard-3125256618.png){width="500"}
:::
:::::

# Factorial Designs Overview

::::: columns
::: {.column width="60%"}
Factorial Designs:

-   Both factors typically fixed (but not always)
:::

::: {.column width="40%"}
![](images/clipboard-723648250.png){width="500"}
:::
:::::

# Nested Design Example: Limpet Growth

::::: columns
::: {.column width="60%"}
Study on effects of enclosure size on limpet growth:

-   2 enclosure sizes (factor A)
-   5 replicate enclosures (factor B)
-   5 replicate limpets per enclosure
:::

::: {.column width="40%"}
![](images/clipboard-1867917958.png){width="500"}
:::
:::::

# Nested Design Example: Reef Fish

::::: columns
::: {.column width="60%"}
Study on reef fish recruitment: 5 sites (factor A) 6 transects at each
site (factor B) replicate observations along each transect
:::

::: {.column width="40%"}
![](images/clipboard-1621781711.png){width="500"}
:::
:::::

# Nested Design Example: Sea Urchin Grazing

::::: columns
::: {.column width="60%"}
Effects of sea urchin grazing on biomass of filamentous algae:

-   4 levels of urchin grazing: none, L, M, H
-   4 patches of rocky bottom (3-4 m2) nested in each level of grazing
-   5 replicate quadrats per patch
:::

::: {.column width="40%"}
![](images/clipboard-3570106410.png){width="500"}
:::
:::::

# Factorial Design Example: Seedling Growth

::::: columns
::: {.column width="60%"}
Effects of light level on growth of seedlings of different size:

-   3 light levels (factor A)
-   3 size classes (factor B)
-   5 replicate seeding in each cell
:::

::: {.column width="40%"}
![](images/clipboard-1057376698.png){width="500"}
:::
:::::

# Factorial Design Example: Salamander Growth

::::: columns
::: {.column width="60%"}
Effects of food level and tadpole presence on larval salamander growth

-   2 food levels (factor A)
-   presence/absence of tadpoles (factor B)
-   8 replicates in each cell
:::

::: {.column width="40%"}
![](images/clipboard-3697840818.png){width="500"}
:::
:::::

# Factorial Design Example: Limpet Fecundity

::::: columns
::: {.column width="60%"}
Effect of season and density on limpet fecundity.

-   2 seasons (factor A)
-   4 density treatments (factor B)
-   3 replicates in each cell
:::

::: {.column width="40%"}
![](images/clipboard-1223732443.png){width="500"}
:::
:::::

# Nested Design: Linear Model Structure

::::: columns
::: {.column width="60%"}
Consider a nested design with:

-   p levels of factor A (i= 1…p) (e.g., 4 grazing levels)
-   q levels of factor B (j= 1…q), nested within each level of A (e.g.,
    4 - diff. patches per grazing level)
-   n replicates (k= 1…n) in each combination of A and B (5 replicate -
    quadrats in each patch in each grazing level)
:::

::: {.column width="40%"}
![](images/clipboard-2460153431.png){width="270"}
:::
:::::

# Calculating Means in Nested Design

::::: columns
::: {.column width="60%"}
Can calculate several means:

-   overall mean (across all levels of A and B)= ȳ;
-   a mean for each level of A (across all levels of B in that A)= ȳi;
-   a mean for each level of B within each A= ȳj(i)
:::

::: {.column width="40%"}
![](images/clipboard-1602183443.png){width="500"}
:::
:::::

# Nested Design Means Visualization

![](images/clipboard-1474611051.png){width="800"}

# Nested Design Linear Model

The linear model for a nested design is:

## $$y_{ijk} = \mu + \alpha_i + \beta_{j(i)} + \varepsilon_{ijk}$$

Where:

-   $y_{ijk}$ is the response variable
    -   value of the k-th replicate in j-th level of B in the i-th level
        of A
    -   (algal biomass in 3rd quadrat, in 2nd patch in low grazing
        treatment)
-   $\mu$ is the overall mean
    -   (overall average algal biomass)

# Fixed Effects in Nested Model

The linear model for a nested design is:

## $$y_{ijk} = \mu + \alpha_i + \beta_{j(i)} + \varepsilon_{ijk}$$

-   $\alpha_i$ is the fixed effect of factor $i$
-   (difference between average biomass in all low grazing level
    quadrats and overall mean)
-   $\beta_{j(i)}$ is the random effect of factor $j$ nested within
    factor $i$
-   usually random variable, measuring variance among all possible
    levels of B within each level of A
-   (variance among all possible patches that may have been used in the
    low grazing treatment)

# Error Term in Nested Model

The linear model for a nested design is:

## $$y_{ijk} = \mu + \alpha_i + \beta_{j(i)} + \varepsilon_{ijk}$$

-   $\varepsilon_{ijk}$ is the error term
-   αi: is the effect of the ith level of A: µi- µ
-   unexplained variance associated with the kth replicate in jth level
    of B in the ith level of A
-   (difference bw observed algal biomass in 3rd quadrat in 2nd patch in
    low grazing treatment and predicted biomass - average biomass in 2nd
    patch in low grazing treatment)

# Analysis of Variance: SSA

::::: columns
::: {.column width="60%"}
As before, partition the variance in the response variable using SS SSA
is SS of differences between means in each level of A and overall mean
:::

::: {.column width="40%"}
![](images/clipboard-1219870939.png){width="500"}
:::
:::::

# Analysis of Variance: SSB

::::: columns
::: {.column width="60%"}
SSB is SS of difference between means in each level of B and the mean of
corresponding level of A summed across levels of A
:::

::: {.column width="40%"}
![](images/clipboard-2467119661.png){width="500"}
:::
:::::

# Analysis of Variance: Residual and Total

::::: columns
::: {.column width="60%"}
-   SSresid is difference bw each observation and mean for its level of
    factor B, summed over all observations
-   SStotal = SSA + SSB + SSresid
-   SS can be turned into MS by dividing by appropriate df
:::

::: {.column width="40%"}
![](images/clipboard-2357756170.png){width="500"}
:::
:::::

# Analysis of Variance Table

![](images/clipboard-449121940.png){width="800"}

# Null Hypotheses: Factor A

::::: columns
::: {.column width="60%"}
Two hypotheses tested on values of MS:

1.  no effects of factor A

-   Assuming A is fixed:
-   Ho(A): µ1= µ2= µ3=…. µi= µ
-   Same as in 1-factor ANOVA, using means from B factors nested within
    each - level of A
-   (no difference in algal biomass across all levels of grazing:
    µnone= - µlow= µmed= µhigh)
:::

::: {.column width="40%"}
![](images/clipboard-1310685300.png){width="500"}
:::
:::::

# Null Hypotheses: Factor B

::::: columns
::: {.column width="60%"}
Two hypotheses tested on values of MS:

2.  No effects of factor B nested in A

-   Assuming B is random:
-   Ho(B): σβ2= 0 (no variance added due to differences between all
    possible - levels of B)
-   (no variance added due to differences between patches)
:::

::: {.column width="40%"}
![](images/clipboard-1310685300.png){width="500"}
:::
:::::

# Conclusions from Analysis

::::: columns
::: {.column width="60%"}
**Conclusions?**

"significant variation between replicate patches within each treatment,
but no significant difference in amount of filamentous algae between
treatments"
:::

::: {.column width="40%"}
![](images/clipboard-3596669693.png){width="500"}
:::
:::::

# Unbalanced Nested Designs

::::: columns
::: {.column width="60%"}
Unequal sample sizes can be because of:

-   uneven number of B levels within each A
-   uneven number of replicates within each level of B

Not a problem, unless have unequal variance or large deviation from -
normality
:::

::: {.column width="40%"}
![](images/clipboard-3055813474.png){width="500"}
:::
:::::

# Nested Design Assumptions

As usual, we assume

-   equal variance
-   normality
-   independence of observations

Equal variance + normality need to be assessed at both levels:

-   Since means for each level of B within each A are used for the
    H-test about A, need to assess whether those means meet normality
    and equal variance
-   Examine residuals for H-test about B
-   Transformations can be used
