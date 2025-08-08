---
title: "Lecture 11 - Single factor analysis of variance - ANOVA"
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
    output-file: "11_01_lecture_powerpoint_html.html"
    embed-resources: true
    self-contained: true
    max-width: 80ch
    css: ../../css/lecture.css
    fig-width: 7
    fig-height: 5

  revealjs:
    output-file: "11_01_lecture_powerpoint_slides.html"
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

  pptx:
    reference-doc: ../../ms_templates/lecture_template.pptx 
    embed-resources: true
    fig-width: 6.5      # Good size for PowerPoint slides
    fig-height: 4       # Proper aspect ratio for slides
    fig-dpi: 300        # High resolution for projection
    df-print: kable     # Better table formatting
        
editor: visual
---













# Lecture 11: Review

::::: columns
::: {.column width="60%"}
## **Multiple Regression**

-   MLR model
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
:::

::: {.column width="40%"}
![](images/clipboard-2698541257.png){width="400"}
:::
:::::

# Lecture 12: Overview

### ANOVA

Analysis of variance: single and multi-factor designs

-   Examples: diatoms, circadian rhythms
-   Predictor variables: fixed vs. random
-   ANOVA model
-   Analysis and partitioning of variance
-   Null hypothesis
-   Assumptions and diagnostics
-   Post F Tests - Tukey and others
-   Reporting the results

# **Lecture 12:** ANOVA Introduction

What if response continuous and predictor(s) categorical?

|                        | Independent variable |                 |
|:-----------------------|:---------------------|:----------------|
| **Dependent variable** | **Continuous**       | **Categorical** |
| **Continuous**         | Regression           | ANOVA           |
| **Categorical**        | Logistic regression  | Tabular         |

# **Lecture 12:** ANOVA and Regression Connection

::: callout-note
# Remember

## Key Insight

Both regression and ANOVA:

-   Partition the total variation in Y
-   Use F-tests for significance
-   Are based on the General Linear Model
-   Test if explanatory variables predict Y ANOVA is fundamentally
    connected to regression analysis - both are special cases of the
    General Linear Model.










::: {.cell}
::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="987567340" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="835419"/><a:gridCol w="1165942"/><a:gridCol w="1314584"/></a:tblGrid><a:tr h="286802"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Model</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Form</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Tests</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="329590"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Regression</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Y = β₀ + β₁X + ε</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>H₀: β₁ = 0</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="328722"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>ANOVA</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Yᵢⱼ = μ + Aᵢ + εᵢⱼ</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>H₀: μ₁ = μ₂ = ... = μₖ</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::









:::

# **Lecture 12:** ANOVA Partitioning

::::: columns
::: {.column width="60%"}
General method for partitioning variation in continuous dependent
variable

-   One or more continuous (and categorical) predictors:
    -   regression
-   One or more categorical predictors:
    -   ANOVA
-   Categorical predictor variables:
    -   groups or experimental treatments
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-2-1.png)
:::
:::









:::
:::::

# **Lecture 12:** ANOVA as Regression

::: callout-tip
# ANOVA as Regression

With one categorical variable, ANOVA is equivalent to regression with
dummy variables.

In fact when we will run ANOVAs we will use he smae code as for
regression! See explanation on oher web page - Will link here










::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/regression-anova-1.png)
:::
:::









:::

# **Lecture 12:** ANOVA Goals

ANOVA aims to compare means of groups:

-   Contribution of predictors + "error" to variability
-   Test H₀ that population (random effects) or group (fixed effects)
    means are equal
-   Single factor (1-way) and multifactor (2-, 3-way designs)
    -   Single factor: one factor, more than two levels.
-   Multifactor:
    -   two or three factors, two or more levels.
    -   Examines variation due to factors **AND** their interaction

# The Analysis of Variance

:::::: columns
:::: {.column width="60%"}
Analysis of variance is the most powerful approach known for
simultaneously testing whether the means of k groups are equal. It works
by assessing whether individuals chosen from different groups are, on
average, more different than individuals chosen from the same group.

The null hypothesis of ANOVA is that the population means μᵢ are the
same for all treatments.

**H₀**: μ₁ = μ₂ = ... = μₖ

**H₁**: At least one μᵢ is different from the others.

::: callout-note
Rejecting H₀ in ANOVA is evidence that the mean of at least one group is
different from the others. It does not indicate *which* means differ.
:::
::::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-3-1.png)
:::
:::









:::
::::::

# **Lecture 12:** ANOVA Logic

::::: columns
::: {.column width="60%"}
Even if all groups had the same true mean, the data would likely show
different sample means for each group due to sampling error.

The key insight of ANOVA is that we can estimate how much variation
among group means ought to be present from sampling error alone if the
null hypothesis is true.

ANOVA lets us determine whether there is more variance among the sample
means than we would expect by chance alone. If so, then we can infer
that there are real differences among the population means.

Two key measures of variation are calculated and compared:

1.  **Group mean square (MSgroups)** - variation among subjects from
    different groups
2.  **Error mean square (MSerror)** - variation among subjects within
    the same group

The comparison is done with an F-ratio:

$$F = \frac{MS_{groups}}{MS_{error}}$$
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-4-1.png)
:::
:::









:::
:::::

# **Lecture 12:** Partitioning the Sum of Squares

The total variation in Y can be expressed as a sum of squares:

$SS_{total} = \sum_{i=1}^{a}\sum_{j=1}^{n}(Y_{ij} - \bar{Y})^2$

This can be partitioned into two components:

1.  **Among Groups (Treatment)**:
    $SS_{among} = \sum_{i=1}^{a}\sum_{j=1}^{n}(\bar{Y}_i - \bar{Y})^2 = n\sum_{i=1}^{a}(\bar{Y}_i - \bar{Y})^2$

2.  **Within Groups (Error)**:
    $SS_{within} = \sum_{i=1}^{a}\sum_{j=1}^{n}(Y_{ij} - \bar{Y}_i)^2$

These components are additive: $SS_{total} = SS_{among} + SS_{within}$

# **Lecture 12:** Sum of Squares Example










::: {.cell}
::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="82437558" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="750711"/><a:gridCol w="327047"/><a:gridCol w="722744"/><a:gridCol w="864007"/><a:gridCol w="722744"/><a:gridCol w="934638"/></a:tblGrid><a:tr h="313901"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Source</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Df</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Sum Sq</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Mean Sq</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Pr(&gt;F)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="284755"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2.236862</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>1.11843079</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>16.05032</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.001075902</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="286988"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Residuals</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.627145</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.06968277</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="514211784" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="1047189"/><a:gridCol w="1159865"/><a:gridCol w="1421058"/><a:gridCol w="983503"/></a:tblGrid><a:tr h="313901"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Component</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Sum of Squares</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Degrees of Freedom</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Mean Square</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="286554"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Total</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2.864007</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>11</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="314707"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Among Groups</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2.236862</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>1.11843079</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="313095"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Within Groups</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.627145</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.06968277</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










::: callout-important
# Key Connection to Regression

This is the same partitioning we saw in regression analysis:
$SS_{total} = SS_{regression} + SS_{residual}$

Where:

-   $SS_{among}$ in ANOVA = $SS_{regression}$ in regression
-   $SS_{within}$ in ANOVA = $SS_{residual}$ in regression

Both measure how much variation is explained by our model vs.
unexplained (error).
:::

# **Lecture 12:** ANOVA Tables

The ANOVA table organizes all computations leading to a test of the null
hypothesis of no differences among population means.

-   **Source of variation**: What is being tested
-   **Sum of squares**: Measure of total variation for each source
-   **df**: Degrees of freedom for each source
-   **Mean squares**: Sum of squares divided by df
-   **F-ratio**: Ratio of mean squares, used to test significance
-   **P-value**: Probability of observing our results if H₀ is true

**Example**: For a one-way ANOVA with 3 groups and 4 replicates per
group:

-   df for treatments = (a - 1) = 2
-   df for error = a(n - 1) = 3(4 - 1) = 9
-   df total = an - 1 = 11

# **Lecture 12:** Circadian Rhythm Data Example










::: {.cell}
::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="390203467" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="750711"/><a:gridCol w="334303"/><a:gridCol w="722744"/><a:gridCol w="793375"/><a:gridCol w="722744"/><a:gridCol w="934638"/></a:tblGrid><a:tr h="313901"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Source</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Df</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Sum Sq</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Mean Sq</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Pr(&gt;F)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="284693"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>7.224492</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>3.6122459</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>7.289449</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.004472271</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="286988"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Residuals</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>19</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9.415345</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.4955445</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="905738130" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="771733"/><a:gridCol w="835667"/><a:gridCol w="793375"/><a:gridCol w="284755"/></a:tblGrid><a:tr h="289468"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Mean</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>SD</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>N</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="289034"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Control</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>-0.3087500</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.6175629</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>8</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="311358"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Eyes</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>-1.5514286</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.7063151</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>7</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="286678"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Knees</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>-0.3357143</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.7908193</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>7</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










# **Lecture 12:** ANOVA vs Regression Tables

::: callout-important
# Comparing ANOVA and Regression Tables

An ANOVA table from an ANOVA model:

| Source    | df     | SS           | MS           | F   | p   |
|-----------|--------|--------------|--------------|-----|-----|
| Treatment | a-1    | SS_treatment | MS_treatment | F   | p   |
| Error     | a(n-1) | SS_error     | MS_error     |     |     |
| Total     | an-1   | SS_total     |              |     |     |

Is equivalent to an ANOVA table from a regression model:

| Source     | df    | SS            | MS            | F   | p   |
|------------|-------|---------------|---------------|-----|-----|
| Regression | k     | SS_regression | MS_regression | F   | p   |
| Error      | n-k-1 | SS_residual   | MS_residual   |     |     |
| Total      | n-1   | SS_total      |               |     |     |

where k = number of dummy variables = a-1
:::

# **Lecture 12:** F ratio

::::: columns
::: {.column width="60%"}
The F-ratio is calculated as:

$$F = \frac{MS_{among}}{MS_{error}}$$

Under the null hypothesis (all means equal): - The F-ratio should be
approximately 1 - Larger F-ratios suggest the among-group variance
exceeds what would be expected by chance

With the circadian rhythm data: - F = 7.29 - p = 0.004 - We reject the
null hypothesis

The F-ratio follows an F-distribution with (a - 1) and (a(n - 1))
degrees of freedom.










::: {.cell}
::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="446969628" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="1262122"/><a:gridCol w="722744"/></a:tblGrid><a:tr h="287360"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Metric</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="286988"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F-observed</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>7.289449</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="311544"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F-critical (α = 0.05)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>3.521893</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::









:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-8-1.png)
:::
:::









:::
:::::

::: callout-note
# Connection to t-test

An ANOVA with two groups (a = 2) is equivalent to a t-test: $$F = t^2$$
:::

# **Lecture 12:** F ratio Visualization

::::: columns
::: {.column width="60%"}
The F-ratio is calculated as:

$$F = \frac{MS_{among}}{MS_{error}}$$

Under the null hypothesis (all means equal): - The F-ratio should be
approximately 1 - Larger F-ratios suggest the among-group variance
exceeds what would be expected by chance

With the circadian rhythm data: - F = 7.29 - p = 0.004 - We reject the
null hypothesis

The F-ratio follows an F-distribution with (a - 1) and (a(n - 1))
degrees of freedom.










::: {.cell}
::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="836004259" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="1262122"/><a:gridCol w="722744"/></a:tblGrid><a:tr h="287360"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Metric</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="286988"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F-observed</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>7.289449</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="311544"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F-critical (α = 0.05)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="38100" marR="38100"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="300" /></a:spcBef><a:spcAft><a:spcPts val="300" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1000" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>3.521893</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="38100" marT="38100" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::









:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-10-1.png)
:::
:::









:::
:::::

# **Lecture 12:** Variation Explained: R²

::::: columns
::: {.column width="60%"}
R² summarizes the contribution of group differences to total variation:

$$R^2 = \frac{SS_{among}}{SS_{total}}$$

This is interpreted as the "fraction of the variation in Y that is
explained by groups."

For the circadian rhythm data: $$R^2 = \frac{7.224}{16.639} = 0.43$$

43% of the total variation in phase shift is explained by differences in
light treatment, with the remaining 57% being unexplained variation.

## Connection to Regression

This is exactly the same calculation as R² in regression:
$$R^2 = \frac{SS_{regression}}{SS_{total}}$$
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-11-1.png)
:::
:::









:::
:::::

# **Lecture 12:** ANOVA Assumptions

ANOVA has the same assumptions as the two-sample t-test, but applied to
all k groups:

1.  **Random samples** from corresponding populations
2.  **Normality**: Y values are normally distributed in each population
3.  **Homogeneity of variance**: variance is the same in all populations
4.  **Independence**: observations are independent

**Checking assumptions**:

-   Normality: Q-Q plots, histogram of residuals, Shapiro-Wilk test
-   Homogeneity: plot residuals vs. predicted values or x-values
-   Independence: examine experimental design

**If assumptions are violated**:

-   Transform Y (e.g., log, square root)
-   Use robust or non-parametric alternatives
-   Use generalized linear models (GLMs)

# **Lecture 12:** ANOVA diagnostics

This is the default output of base R










::: {.cell}

```{.r .cell-code}
# Model diagnostics
par(mfrow = c(2, 2))
plot(circ_model)
dev.off() # This forces the plot to be written
```

::: {.cell-output .cell-output-stdout}

```
null device 
          1 
```


:::

::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-12-1.png)
:::
:::










# A newer way to check with the performance library










::: {.cell}

```{.r .cell-code}
# install.packages("performance")
library(performance)
check_model(circ_model)
```

::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-13-1.png)
:::
:::










# **Lecture 12:** Levene's Test

Levene's test of homogeneity of variance Null Hypothesis is that they
are homogeneous So you want a non significant result here










::: {.cell}
::: {.cell-output-display}

|      | Df|   F value|    Pr(>F)|
|:-----|--:|---------:|---------:|
|group |  2| 0.1585662| 0.8544841|
|      | 19|        NA|        NA|

:::
:::










# **Lecture 12:** Shapiro-Wilk Test

Shapiro-Wilk Normality Test Null Hypothesis is that they are normally
distributed So you want a non significant result here










::: {.cell}
::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  residuals(circ_model)
W = 0.95893, p-value = 0.468
```


:::
:::










::: callout-note
# Shared Assumptions with Regression

ANOVA and regression share virtually identical assumptions because they
are both linear models:

| Assumption | ANOVA | Regression |
|------------------------|------------------------|------------------------|
| Linearity | Relationship between group membership and Y is additive | Relationship between X and Y is linear |
| Normality | Residuals within each group are normal | Residuals are normal |
| Equal variance | Variance is the same across all groups | Variance is the same across all X values |
| Independence | Observations are independent | Observations are independent |
:::

# **Lecture 12:** ANOVA Post-Hoc Testing Overview

::::: columns
::: {.column width="50%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="50%"}









::: {.cell}
::: {.cell-output .cell-output-stdout}

```
 contrast        estimate    SE df t.ratio p.value
 Control - Eyes     1.243 0.364 19   3.411  0.0079
 Control - Knees    0.027 0.364 19   0.074  0.9970
 Eyes - Knees      -1.216 0.376 19  -3.231  0.0117

P value adjustment: tukey method for comparing a family of 3 estimates 
```


:::
:::









:::
:::::

# **Lecture 12:** Post-Hoc Testing Results

::::: columns
::: {.column width="50%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="50%"}









::: {.cell}
::: {.cell-output-display}

|   |treatment |     emmean|        SE| df|   lower.CL|   upper.CL|.group |
|:--|:---------|----------:|---------:|--:|----------:|----------:|:------|
|2  |Eyes      | -1.5514286| 0.2660678| 19| -2.1083148| -0.9945423|a      |
|3  |Knees     | -0.3357143| 0.2660678| 19| -0.8926006|  0.2211720|b      |
|1  |Control   | -0.3087500| 0.2488836| 19| -0.8296694|  0.2121694|b      |

:::
:::









:::
:::::

# **Lecture 12:** Post-Hoc Visualization

::::: columns
::: {.column width="50%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="50%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-18-1.png)
:::
:::









:::
:::::

# **Lecture 12:** Significance Groups Plot

::::: columns
::: {.column width="60%"}
When ANOVA rejects H₀, we need to determine which groups differ.

**Planned comparisons**: - Identified during study design - Have strong
prior justification - Use pooled variance from all groups - Have higher
precision than separate t-tests

**Unplanned (post hoc) comparisons**: - Used when no specific
comparisons were planned - Must adjust for multiple testing - Common
methods: Tukey-Kramer, Bonferroni, Scheffé

**Example**: Using Tukey's HSD to compare all pairs of treatments in the
circadian rhythm data.
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](11_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-19-1.png)
:::
:::









:::
:::::

# **Lecture 12:** Reporting ANOVA Results

**Formal scientific writing example:**

"The effect of light treatment on circadian rhythm phase shift was
analyzed using a one-way ANOVA. There was a significant effect of
treatment on phase shift (F(2, 19) = 7.29, p = 0.004, η² = 0.43).
Post-hoc comparisons using Tukey's HSD test indicated that the mean
phase shift for the Eyes treatment (M = -1.55 h, SD = 0.71) was
significantly different from both the Control treatment (M = -0.31 h, SD
= 0.62) and the Knees treatment (M = -0.34 h, SD = 0.79). However, the
Control and Knees treatments did not significantly differ from each
other. These results suggest that light exposure to the eyes, but not to
the knees, impacts circadian rhythm phase shifts."

# **Lecture 12:** ANOVA Summary

### Key ANOVA Principles

1.  **Purpose**: ANOVA (Analysis of Variance) compares means across
    multiple groups simultaneously

2.  **Connection to Regression**:

    -   Both are special cases of the General Linear Model
    -   ANOVA with categorical predictors = Regression with dummy
        variables
    -   Both partition variance into explained and unexplained
        components

3.  **The Analysis of Variance**:

    -   Partitions total variation into components
    -   Tests whether differences among groups exceed what would be
        expected by chance
    -   Uses F-tests to compare variance between groups to variance
        within groups

4.  **Sum of Squares Partitioning**:

    -   SS(Total) = SS(Between Groups) + SS(Within Groups)
    -   Same as SS(Total) = SS(Regression) + SS(Error) in regression

5.  **Fixed vs. Random Effects**:

    -   Fixed effects: specific groups of interest (most common)
    -   Random effects: sampling from a larger population

# ANOVA Assumptions

1.  Independence of observations
2.  Normal distribution of residuals
3.  Homogeneity of variances
