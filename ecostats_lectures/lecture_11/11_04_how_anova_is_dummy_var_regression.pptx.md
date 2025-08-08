---
title: "How an ANOVA IS A REGRESSION WIHT DUMMY VARIABLES "
author: "Bill Perry"
execute:
  freeze: auto
  cache: true
  echo: true
  keep-md: true
  message: false
  warning: false
  fig-height: 5
  fig-width: 4
  paged-print: false
format:
  html:
    toc: false
    output-file: "11_04_how_anova_is_regression_html.html"
    embed-resources: true
    self-contained: true
    max-width: 80ch
    css: ../../css/lecture.css
  revealjs:
    output-file: "11_04_how_anova_is_regression_slides.html"
    self-contained: true
    css: ../../css/lecture.css
    slide-number: true
    transition: fade
  docx:
    default: true
    toc: false
    toc-depth: 3
    number-sections: true
    highlight-style: github
    reference-doc: ../../ms_templates/custom-reference.docx
    css: msword.css
    embed-resources: true
  pptx:
    reference-doc: ../../ms_templates/lecture_template.pptx 
    embed-resources: true
editor: visual
---











# Introduction

This document demonstrates how Analysis of Variance (ANOVA) is
mathematically equivalent to a regression model with dummy variables
using an example with R code and visualizations.

# Setup and Data Creation

Let's begin by loading necessary packages and creating a dataframe about
plant heights with three different fertilizer treatments.










::: {.cell}

```{.r .cell-code}
# install.packages("flextable")
library(tidyverse)
library(flextable)

# Create the dataset
fertilizer_data <- tibble(
  fertilizer = rep(c("A", "B", "C"), each = 3),
  height = c(10, 12, 8,   # Fertilizer A
             14, 16, 18,  # Fertilizer B
             20, 22, 24)  # Fertilizer C
)

# Display the dataset using flextable
flextable(fertilizer_data) %>%
  set_caption("Plant Heights by Fertilizer Type") %>%
  theme_vanilla() %>%
  autofit()
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="597313041" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="804225"/><a:gridCol w="679805"/></a:tblGrid></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










# Calculating Group Means (ANOVA Approach)

In ANOVA, we calculate the mean of each group and compare variation
between groups to variation within groups.










::: {.cell}

```{.r .cell-code}
group_means <- fertilizer_data %>%
  group_by(fertilizer) %>%
  summarize(mean_height = mean(height))

flextable(group_means) %>%
  set_caption("Group Means (ANOVA Approach)") %>%
  theme_vanilla() %>%
  autofit()
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="666437412" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="804225"/><a:gridCol w="1122439"/></a:tblGrid><a:tr h="392356"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>fertilizer</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>mean_height</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="361114"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>A</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>10</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="361387"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>B</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>16</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="366299"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>C</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>22</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










Let's visualize the raw data and group means:










::: {.cell}

```{.r .cell-code}
ggplot(fertilizer_data, aes(x = fertilizer, y = height)) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  geom_point(data = group_means, aes(y = mean_height), 
             color = "red", size = 3) +
  labs(title = "Plant Height by Fertilizer Type",
       x = "Fertilizer Type",
       y = "Plant Height (cm)") +
  theme_minimal()
```

::: {.cell-output-display}
![](11_04_how_anova_is_dummy_var_regression_files/figure-pptx/plot-raw-data-1.png)
:::
:::










# Running the ANOVA










::: {.cell}

```{.r .cell-code}
# Run ANOVA
anova_model <- aov(height ~ fertilizer, data = fertilizer_data)
anova_summary <- summary(anova_model)
anova_summary
```

::: {.cell-output .cell-output-stdout}

```
            Df Sum Sq Mean Sq F value Pr(>F)   
fertilizer   2    216     108      27  0.001 **
Residuals    6     24       4                  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::










# Regression with Dummy Variables

For the regression approach, we'll create dummy variables for fertilizer
types, using fertilizer A as the reference level.










::: {.cell}

```{.r .cell-code}
# Set fertilizer A as the reference level
fertilizer_data$fertilizer <- factor(fertilizer_data$fertilizer, levels = c("A", "B", "C"))

# Run regression with dummy variables
reg_model <- lm(height ~ fertilizer, data = fertilizer_data)
reg_summary <- summary(reg_model)
reg_summary
```

::: {.cell-output .cell-output-stdout}

```

Call:
lm(formula = height ~ fertilizer, data = fertilizer_data)

Residuals:
   Min     1Q Median     3Q    Max 
    -2     -2      0      2      2 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   10.000      1.155   8.660 0.000131 ***
fertilizerB    6.000      1.633   3.674 0.010402 *  
fertilizerC   12.000      1.633   7.348 0.000325 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2 on 6 degrees of freedom
Multiple R-squared:    0.9,	Adjusted R-squared:  0.8667 
F-statistic:    27 on 2 and 6 DF,  p-value: 0.001
```


:::
:::










# Understanding the Regression Coefficients

In our regression model:

-   The intercept (10) is equal to the mean of the reference group (A)
-   The coefficient for fertilizer B (6) is the difference between mean
    of group B and mean of group A
-   The coefficient for fertilizer C (12) is the difference between mean
    of group C and mean of group A










::: {.cell}

```{.r .cell-code}
# Create a table showing the relationship between coefficients and means
coefs <- coef(reg_model)

coefficients_explained <- tibble(
  Term = c("Intercept", "fertilizerB", "fertilizerC"),
  Coefficient = coefs,
  Meaning = c(
    "Mean of Group A (reference group)",
    "Difference between Group B and Group A means",
    "Difference between Group C and Group A means"
  ),
  Mathematical_Expression = c(
    "β₀ = μA",
    "β₁ = μB - μA",
    "β₂ = μC - μA"
  ),
  Numeric_Value = c(coefs[1],
    paste0(round(group_means$mean_height[2], 1), " - ", 
           round(group_means$mean_height[1], 1), " = ", 
           round(coefs[2], 1)),
    paste0(round(group_means$mean_height[3], 1), " - ", 
           round(group_means$mean_height[1], 1), " = ", 
           round(coefs[3], 1))))

# Use flextable to format the table
flextable(coefficients_explained) %>%
  set_caption("Regression Coefficients Explained") %>%
  theme_vanilla() %>%
  fit_to_width(max_width = 8, unit = "in") %>%
  bold(j = 1) %>%
  colformat_double(j = 2, digits = 2)
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="759524406" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="685800"/><a:gridCol w="685800"/><a:gridCol w="685800"/><a:gridCol w="685800"/><a:gridCol w="685800"/></a:tblGrid><a:tr h="228600"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Term</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Coefficient</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Meaning</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Mathematical_Expression</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Numeric_Value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="228600"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Intercept</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>10.00</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Mean of Group A (reference group)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>β₀ = μA</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>10</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="228600"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>fertilizerB</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>6.00</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Difference between Group B and Group A means</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>β₁ = μB - μA</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>16 - 10 = 6</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="228600"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>fertilizerC</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>12.00</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Difference between Group C and Group A means</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>β₂ = μC - μA</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>22 - 10 = 12</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










Let's visualize these coefficients:










::: {.cell}

```{.r .cell-code}
coef_data <- tibble(
  Term = factor(c("Intercept\n(Group A Mean)", "Group B - Group A", "Group C - Group A"),
                levels = c("Intercept\n(Group A Mean)", "Group B - Group A", "Group C - Group A")),
  Value = c(coefs[1], coefs[2], coefs[3])
)

ggplot(coef_data, aes(x = Term, y = Value)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5) +
  labs(title = "Regression Coefficients with Dummy Variables",
       subtitle = "Intercept represents Group A mean; other coefficients show differences from reference",
       x = "",
       y = "Coefficient Value (cm)") +
  theme_minimal()
```

::: {.cell-output-display}
![](11_04_how_anova_is_dummy_var_regression_files/figure-pptx/plot-coefficients-1.png)
:::
:::










# Demonstrating the Equivalence

Now, let's prove that the regression model predictions are identical to
the ANOVA group means:










::: {.cell}

```{.r .cell-code}
# Get predictions from regression model
predicted_values <- predict(reg_model, fertilizer_data)

# Create a dataframe for comparison
comparison_data <- fertilizer_data %>%
  mutate(predicted = predicted_values) %>%
  group_by(fertilizer) %>%
  mutate(group_mean = mean(height))

# Generate the predicted values for each group
predicted_values_by_group <- comparison_data %>%
  group_by(fertilizer) %>%
  reframe(
    anova_mean = mean(height),
    regression_prediction = mean(predicted),
    formula = case_when(
      fertilizer == "A" ~ paste0(round(coefs[1], 1), " + 0 + 0 = ", round(coefs[1], 1)),
      fertilizer == "B" ~ paste0(round(coefs[1], 1), " + ", round(coefs[2], 1), " + 0 = ", round(coefs[1] + coefs[2], 1)),
      fertilizer == "C" ~ paste0(round(coefs[1], 1), " + 0 + ", round(coefs[3], 1), " = ", round(coefs[1] + coefs[3], 1))
    )
  )
```
:::










Let's visualize this equivalence:










::: {.cell}

```{.r .cell-code}
# Create data for plotting the equivalence
plot_data <- predicted_values_by_group %>%
  pivot_longer(cols = c(anova_mean, regression_prediction),
               names_to = "method",
               values_to = "value") %>%
  mutate(method = ifelse(method == "anova_mean", "ANOVA Group Mean", "Regression Prediction"))

ggplot(plot_data, aes(x = fertilizer, y = value, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  geom_text(aes(label = round(value, 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "ANOVA Mean vs. Regression Prediction by Fertilizer Type",
       subtitle = "Both methods produce identical values",
       x = "Fertilizer Type",
       y = "Plant Height (cm)",
       fill = "Method") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
```

::: {.cell-output-display}
![](11_04_how_anova_is_dummy_var_regression_files/figure-pptx/equivalence-plot-1.png)
:::
:::










# Comparing Statistical Tests

Both ANOVA and regression provide an F-test. Let's compare them:










::: {.cell}

```{.r .cell-code}
# ANOVA: Extract F-value and p-value
anova_f <- anova_summary[[1]]$`F value`[1]
anova_p <- anova_summary[[1]]$`Pr(>F)`[1]

# Regression: Extract F-value and p-value
reg_f <- reg_summary$fstatistic[1]
reg_p <- pf(reg_f, reg_summary$fstatistic[2], reg_summary$fstatistic[3], lower.tail = FALSE)

# Compare them
test_comparison <- tibble(
  Test = c("ANOVA F-test", "Regression F-test"),
  `F-value` = c(anova_f, reg_f),
  `p-value` = c(anova_p, reg_p)
)

# Format with flextable
flextable(test_comparison) %>%
  set_caption("Comparison of Statistical Tests") %>%
  theme_vanilla() %>%
  autofit() %>%
  colformat_double(j = 2:3, digits = 4)
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="721513983" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="1363230"/><a:gridCol w="749860"/><a:gridCol w="749860"/></a:tblGrid><a:tr h="390582"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Test</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F-value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>p-value</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="366640"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>ANOVA F-test</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>27.0000</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.0010</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="391879"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Regression F-test</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>27.0000</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.0010</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="19050"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










# The Mathematical Relationship

For a one-way ANOVA with a categorical variable having `k` levels, we
can express the relationship with regression as:

$$Y = \beta_0 + \beta_1X_1 + \beta_2X_2 + ... + \beta_{k-1}X_{k-1} + \epsilon$$

Where: - $\beta_0$ is the mean of the reference group -
$\beta_1, \beta_2, ..., \beta_{k-1}$ are the differences between each
group's mean and the reference group mean - $X_1, X_2, ..., X_{k-1}$ are
dummy variables (0 or 1)

In our example: - $\beta_0 = 10$ (mean of group A) - $\beta_1 = 6$
(difference between B and A) - $\beta_2 = 12$ (difference between C and
A)

# Conclusion

This demonstration shows that one-way ANOVA is mathematically equivalent
to regression with dummy variables. The key equivalences are:

1.  ANOVA group means = Regression predictions for each group
2.  F-statistic from ANOVA = F-statistic from regression
3.  p-values are identical in both approaches

This confirms that both techniques are special cases of the General
Linear Model, just expressed in different ways. For a categorical
predictor with `k` levels, we need `k-1` dummy variables in the
regression approach, with one level serving as the reference category.
