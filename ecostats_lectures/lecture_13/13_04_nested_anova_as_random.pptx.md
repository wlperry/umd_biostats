---
title: "Lecture 13 - NESTED ANOVA"
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
    output-file: "13_04_nested_anova_random_html.html"
    embed-resources: true
    self-contained: true
    max-width: 80ch
    css: ../../css/lecture.css
  revealjs:
    output-file: "13_04_nested_anova_random_slides.html"
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

This analysis examines the effects of varying sea urchin densities on
the percentage cover of filamentous algae. The experiment was designed
with four urchin density treatments (control, 66% of original density,
33% of original density, and all urchins removed) nested within four
random patches. Five replicate quadrats were measured within each
treatment-patch combination.

The traditional nested ANOVA approach can be implemented using a linear
mixed-effects model, which provides a more flexible framework for
analyzing hierarchical designs. In this case, we'll use the `lme4`
package to fit a model where treatment is a fixed effect and patch is a
random effect nested within treatment.

# Data Overview

The dataframe contains 80 observations with the following
variables:

-   treat: Urchin density treatment (Control, 66% Density, 33% Density,
    Removed)
-   patch: Random patches (1-16) where treatments were applied
-   QUAD: Replicate quadrats within each treatment-patch combination
-   algae: Percentage cover of filamentous algae (response variable)










::: {.cell}

```{.r .cell-code}
# Create a summary table with flextable

# Summary statistics
summary_stats <- urchin_df %>%
  group_by(treat) %>%
  summarise(
    n = n(),
    mean = mean(algae),
    sd = sd(algae),
    se = sd / sqrt(n),
    min = min(algae),
    max = max(algae),
    .groups = 'drop'
  )

summary_stats
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 4 × 7
  treat       n  mean    sd    se   min   max
  <fct>   <int> <dbl> <dbl> <dbl> <dbl> <dbl>
1 control    20   1.3  3.18 0.711     0    13
2 dens_33    20  21.6 25.1  5.62      0    79
3 dens_66    20  19   25.7  5.74      0    71
4 removal    20  39.2 28.7  6.41      0    83
```


:::

```{.r .cell-code}
# 
# summary_stats %>%
#   select(treat, n, mean, sd, se, min, max) %>%
#   flextable() %>%
#   set_header_labels(
#     treat = "treatment",
#     n = "N",
#     mean = "Mean",
#     sd = "SD",
#     se = "SE",
#     min = "Min",
#     max = "Max"
#   ) %>%
#   colformat_double(j = c("mean", "sd", "se", "min", "max"), digits = 2) %>%
#   autofit() %>%
#   add_header_lines("Summary statistics of algae cover (%) across treatments") %>%
#   theme_box()
```
:::










# Mixed Model Analysis

In this experimental design, patch is nested within treat because each
patch received only one treatment level. This hierarchical design is
well-suited for analysis using linear mixed-effects models.

## Model Specification

We'll use the following model specification:

$algae_{ijk} = \mu + \alpha_i + \beta_{j(i)} + \epsilon_{ijk}$

Where: - $\mu$ is the overall mean - $\alpha_i$ is the fixed effect of
treatment $i$ - $\beta_{j(i)}$ is the random effect of patch $j$ nested
within treatment $i$ - $\epsilon_{ijk}$ is the residual error for
quadrat $k$ in patch $j$ within treatment $i$

In `lme4`, this model is specified as










::: {.cell}

```{.r .cell-code}
# Fit the mixed model
mixed_model <- lmer(algae ~ treat + (1|treat:patch), data = urchin_df)

# Display model summary
summary(mixed_model)
```

::: {.cell-output .cell-output-stdout}

```
Linear mixed model fit by REML. t-tests use Satterthwaite's method [
lmerModLmerTest]
Formula: algae ~ treat + (1 | treat:patch)
   Data: urchin_df

REML criterion at convergence: 682.2

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.9808 -0.3106 -0.1093  0.2831  2.5910 

Random effects:
 Groups      Name        Variance Std.Dev.
 treat:patch (Intercept) 294.3    17.16   
 Residual                298.6    17.28   
Number of obs: 80, groups:  treat:patch, 16

Fixed effects:
             Estimate Std. Error     df t value Pr(>|t|)  
(Intercept)     1.300      9.408 12.000   0.138   0.8924  
treatdens_33   20.250     13.305 12.000   1.522   0.1539  
treatdens_66   17.700     13.305 12.000   1.330   0.2081  
treatremoval   37.900     13.305 12.000   2.849   0.0147 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
            (Intr) trt_33 trt_66
treatdns_33 -0.707              
treatdns_66 -0.707  0.500       
treatremovl -0.707  0.500  0.500
```


:::
:::










## ANOVA Table

The ANOVA table for the mixed model:










::: {.cell}

```{.r .cell-code}
# Get ANOVA table with Type III tests
anova_table <- anova(mixed_model, type = 3)
print(anova_table)
```

::: {.cell-output .cell-output-stdout}

```
Type III Analysis of Variance Table with Satterthwaite's method
      Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)  
treat   2434  811.33     3    12  2.7171 0.09126 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::

```{.r .cell-code}
# For comparison, also run a traditional nested ANOVA
nested_aov <- aov(algae ~ treat + treat:patch, data = urchin_df)
std_summary <- summary(nested_aov)[[1]]

# Extract MS values - using exact row names
MS_treat <- std_summary["treat      ", "Mean Sq"] 
MS_patch <- std_summary["treat:patch", "Mean Sq"]
MS_residual <- std_summary["Residuals", "Mean Sq"]

# Print MS values to check
print("MS values:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "MS values:"
```


:::

```{.r .cell-code}
print(c(treatment = MS_treat, patches = MS_patch, Residual = MS_residual))
```

::: {.cell-output .cell-output-stdout}

```
treatment   patches  Residual 
 4809.712  1770.162   298.600 
```


:::

```{.r .cell-code}
# Extract df values
df_treat <- std_summary["treat      ", "Df"]
df_patch <- std_summary["treat:patch", "Df"]
df_residual <- std_summary["Residuals", "Df"]


# Calculate correct F ratios for nested design
F_treat <- MS_treat / MS_patch
F_patch <- MS_patch / MS_residual

# Calculate p-values
p_treat <- pf(F_treat, df_treat, df_patch, lower.tail = FALSE)
p_patch <- pf(F_patch, df_patch, df_residual, lower.tail = FALSE)

# Create ANOVA table
trad_anova_table <- data.frame(
  Source = c("treatment", "patches (treatment)", "Residual"),
  df = c(df_treat, df_patch, df_residual),
  MS = c(MS_treat, MS_patch, MS_residual),
  F = c(F_treat, F_patch, NA),
  p = c(p_treat, p_patch, NA)
)

# Format p-values
trad_anova_table$p <- ifelse(trad_anova_table$p < 0.001, "<0.001",
                       ifelse(is.na(trad_anova_table$p), NA,
                              format(trad_anova_table$p, digits = 3)))
trad_anova_table
```

::: {.cell-output .cell-output-stdout}

```
               Source df       MS        F           p
1           treatment  3 4809.712 2.717102 0.091262004
2 patches (treatment) 12 1770.162 5.928207      <0.001
3            Residual 64  298.600       NA        <NA>
```


:::

```{.r .cell-code}
# # Display traditional ANOVA table with flextable
# trad_anova_table %>%
#   flextable() %>%
#   set_header_labels(
#     Source = "Source of variation",
#     df = "df",
#     MS = "MS",
#     F = "F",
#     p = "p"
#   ) %>%
#   colformat_double(j = c("MS", "F"), digits = 2) %>%
#   autofit() %>%
#   add_header_lines("ANOVA table for nested design") %>%
#   theme_box()
```
:::










## Variance Components

We can extract the variance components from the mixed model:










::: {.cell}

```{.r .cell-code}
# Print corrected results

# Extract variance components
vc <- VarCorr(mixed_model)
print(vc)
```

::: {.cell-output .cell-output-stdout}

```
 Groups      Name        Std.Dev.
 treat:patch (Intercept) 17.156  
 Residual                17.280  
```


:::

```{.r .cell-code}
# Extract variance components
var_comp_patch <- as.numeric(vc$`treat:patch`)
var_comp_residual <- attr(vc, "sc")^2

# Calculate percentage of total variance
total_var <- var_comp_patch + var_comp_residual
pct_patch <- var_comp_patch / total_var * 100
pct_residual <- var_comp_residual / total_var * 100

# Calculate treatment variance component
n_quad <- 5  # Number of quadrats per patch
n_patch <- 4  # Number of patches per treatment
var_comp_treatment <- (MS_treat - MS_patch) / (n_quad * n_patch)

# Format variance components for display
var_comp_treatment_display <- ifelse(var_comp_treatment < 0, 
                                    paste0("(", format(abs(var_comp_treatment), digits = 2), ")"),
                                    format(var_comp_treatment, digits = 2))

# Create variance components table
var_comp_table <- data.frame(
  Source = c("treatment", "patches (treatment)", "Residual"),
  `Var.comp` = c(var_comp_treatment_display, 
               format(var_comp_patch, digits = 2),
               format(var_comp_residual, digits = 2))
)

# Display variance components table
var_comp_table %>%
  flextable() %>%
  set_header_labels(
    Source = "Source of variation",
    Var.comp = "Variance component"
  ) %>%
  autofit() %>%
  add_header_lines("Variance components") %>%
  theme_box()
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="663888151" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="1472030"/><a:gridCol w="1542152"/></a:tblGrid><a:tr h="390105"><a:tc gridSpan="2"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="390105"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Source of variation</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Variance component</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="361251"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>152</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="391742"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>patches (treatment)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>294</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="364116"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Residual</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>299</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::

```{.r .cell-code}
# Complete table with all information
complete_table <- data.frame(
  Source = c("treatment", "patches (treatment)", "Residual"),
  df = c(df_treat, df_patch, df_residual),
  MS = c(MS_treat, MS_patch, MS_residual),
  F = c(F_treat, F_patch, NA),
  p = c(trad_anova_table$p[1], trad_anova_table$p[2], NA),
  `Var.comp` = c(var_comp_treatment_display, 
                format(var_comp_patch, digits = 2),
                format(var_comp_residual, digits = 2))
)

# Display complete table
complete_table %>%
  flextable() %>%
  set_header_labels(
    Source = "Source of variation",
    df = "df",
    MS = "MS",
    F = "F",
    p = "p",
    Var.comp = "Var. comp."
  ) %>%
  colformat_double(j = c("MS", "F"), digits = 2) %>%
  autofit() %>%
  add_header_lines("Complete ANOVA table with variance components") %>%
  theme_box()
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="470537276" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="1472030"/><a:gridCol w="416162"/><a:gridCol w="804566"/><a:gridCol w="532670"/><a:gridCol w="1076531"/><a:gridCol w="936217"/></a:tblGrid><a:tr h="390105"><a:tc gridSpan="6"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Complete ANOVA table with variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Complete ANOVA table with variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Complete ANOVA table with variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Complete ANOVA table with variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Complete ANOVA table with variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Complete ANOVA table with variance components</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="390105"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Source of variation</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>df</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>MS</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>F</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>p</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Var. comp.</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="379464"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>3</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>4,809.71</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>2.72</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>0.091262004</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>152</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="391742"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>patches (treatment)</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>12</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>1,770.16</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>5.93</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>&lt;0.001</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>294</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="364116"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Residual</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>64</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>298.60</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:endParaRPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:endParaRPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t></a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>299</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










::: {.callout-important appearance="simple"}
**Interpretation of ANOVA Results**

The nested ANOVA using mixed models reveals that there was no
significant effect of urchin density treatment on algae cover (F =
2.72, df = 3, 12, p =
0.0913). However, there was significant variation
among patches within treatments (F = 5.93, df =
12, 64, p \< 0.001).

The variance component for patches nested within treatments
(294) indicates substantial spatial
heterogeneity in algae cover, highlighting the importance of accounting
for this spatial variation in the analysis. The negative variance
component for treatment suggests that there is more variation among
patches within treatments than among treatments themselves.
:::

# Lecture 13: Post-hoc Comparisons

Although the main effect of treatment was not significant in the nested
ANOVA (p = r format(p_treat, digits=3)), we can still examine the mean
differences between treatments to understand patterns in the data.
However, we should interpret these with caution given the lack of
statistical significance at the α = 0.05 level.










::: {.cell}

```{.r .cell-code}
# Calculate estimated marginal means
emm <- emmeans(mixed_model, ~ treat)

# Display EMMs with flextable
as.data.frame(summary(emm)) %>%
  flextable() %>%
  set_header_labels(
    treat = "treatment",
    emmean = "Estimated Marginal Mean",
    SE = "Standard Error",
    df = "df",
    lower.CL = "Lower CL",
    upper.CL = "Upper CL"
  ) %>%
  colformat_double(j = c("emmean", "SE", "lower.CL", "upper.CL"), digits = 2) %>%
  autofit() %>%
  add_header_lines("Estimated marginal means for each treatment") %>%
  theme_box()
```

::: {.cell-output-display}


``````{=openxml}
<p:graphicFrame xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"><p:nvGraphicFramePr><p:cNvPr id="561475646" name=""/><p:cNvGraphicFramePr><a:graphicFrameLocks noGrp="true"/></p:cNvGraphicFramePr><p:nvPr/></p:nvGraphicFramePr><p:xfrm rot="0"><a:off x="914400" y="1828800"/><a:ext cx="9144000" cy="5486400"/></p:xfrm><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table"><a:tbl><a:tblPr/><a:tblGrid><a:gridCol w="850883"/><a:gridCol w="1844745"/><a:gridCol w="1177009"/><a:gridCol w="416162"/><a:gridCol w="858659"/><a:gridCol w="858659"/></a:tblGrid><a:tr h="392833"><a:tc gridSpan="6"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated marginal means for each treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated marginal means for each treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated marginal means for each treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated marginal means for each treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated marginal means for each treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc hMerge="true"><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated marginal means for each treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="392833"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>treatment</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Estimated Marginal Mean</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Standard Error</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>df</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Lower CL</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="1" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>Upper CL</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="363638"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>control</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>1.30</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9.41</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>12</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>-19.20</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>21.80</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="0"><a:noFill/><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="378782"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>dens_33</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>21.55</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9.41</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>12</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>1.05</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>42.05</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="378782"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>dens_66</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>19.00</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9.41</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>12</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>-1.50</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>39.50</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr><a:tr h="363638"><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>removal</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>39.20</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>9.41</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>12</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>18.70</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc><a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="r" marL="63500" marR="63500"><a:lnSpc><a:spcPct val="100000"/></a:lnSpc><a:spcBef><a:spcPts val="500" /></a:spcBef><a:spcAft><a:spcPts val="500" /></a:spcAft><a:buNone/></a:pPr><a:r><a:rPr cap="none" sz="1100" i="0" b="0" u="none"><a:solidFill><a:srgbClr val="000000"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:latin typeface="Helvetica"/><a:cs typeface="Helvetica"/><a:ea typeface="Helvetica"/><a:sym typeface="Helvetica"/></a:rPr><a:t>59.70</a:t></a:r></a:p></a:txBody><a:tcPr anchor="ctr" marB="63500" marT="63500" marR="0" marL="0"><a:lnL algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnL><a:lnR algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnR><a:lnT algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnT><a:lnB algn="ctr" cmpd="sng" cap="flat" w="9525"><a:solidFill><a:srgbClr val="666666"><a:alpha val="100000"/></a:srgbClr></a:solidFill><a:prstDash val="solid"/></a:lnB><a:solidFill><a:srgbClr val="FFFFFF"><a:alpha val="0"/></a:srgbClr></a:solidFill></a:tcPr></a:tc></a:tr></a:tbl></a:graphicData></a:graphic></p:graphicFrame>
``````



:::
:::










# Lecture 13: Tukey Pairwise Comparisons

-   text










::: {.cell}

```{.r .cell-code}
# Pairwise comparisons with Tukey adjustment
pairs <- pairs(emm, adjust = "tukey")
pairs
```

::: {.cell-output .cell-output-stdout}

```
 contrast          estimate   SE df t.ratio p.value
 control - dens_33   -20.25 13.3 12  -1.522  0.4553
 control - dens_66   -17.70 13.3 12  -1.330  0.5625
 control - removal   -37.90 13.3 12  -2.849  0.0615
 dens_33 - dens_66     2.55 13.3 12   0.192  0.9974
 dens_33 - removal   -17.65 13.3 12  -1.327  0.5646
 dens_66 - removal   -20.20 13.3 12  -1.518  0.4573

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 4 estimates 
```


:::

```{.r .cell-code}
# # Display pairwise comparisons with flextable
# as.data.frame(summary(pairs)) %>%
#   flextable() %>%
#   set_header_labels(
#     contrast = "Contrast",
#     estimate = "Estimate",
#     SE = "Standard Error",
#     df = "df",
#     t.ratio = "t ratio",
#     p.value = "p-value"
#   ) %>%
#   colformat_double(j = c("estimate", "SE", "t.ratio", "p.value"), digits = 3) %>%
#   autofit() %>%
#   add_header_lines("Pairwise comparisons between treatments (Tukey-adjusted)") %>%
#   theme_box()
```
:::










# Lecture 13: Letter Display










::: {.cell}

```{.r .cell-code}
# Extract compact letter display for plotting
cld <- multcomp::cld(emm, alpha = 0.05, Letters = letters)

cld
```

::: {.cell-output .cell-output-stdout}

```
 treat   emmean   SE df lower.CL upper.CL .group
 control    1.3 9.41 12   -19.20     21.8  a    
 dens_66   19.0 9.41 12    -1.50     39.5  a    
 dens_33   21.6 9.41 12     1.05     42.0  a    
 removal   39.2 9.41 12    18.70     59.7  a    

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95 
P value adjustment: tukey method for comparing a family of 4 estimates 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::

```{.r .cell-code}
# # Display CLD with flextable
# as.data.frame(cld) %>%
#   flextable() %>%
#   set_header_labels(
#     treat = "treatment",
#     emmean = "Estimated Marginal Mean",
#     SE = "Standard Error",
#     df = "df",
#     lower.CL = "Lower CL",
#     upper.CL = "Upper CL",
#     .group = "Group"
#   ) %>%
#   colformat_double(j = c("emmean", "SE", "lower.CL", "upper.CL"), digits = 2) %>%
#   autofit() %>%
#   add_header_lines("Compact letter display of treatment means") %>%
#   theme_box()
```
:::










::: {.callout-important appearance="simple"}
Interpretation of treatment Comparisons The mean algae cover for the
Control treatment (1.30%) appears considerably lower than for the
reduced urchin density treatments (66% Density: 21.55%, 33% Density:
19.00%, Removed: 39.20%). While the visual pattern suggests an inverse
relationship between urchin density and algae cover, with complete
removal showing the highest algae cover, the nested ANOVA showed that
these differences were not statistically significant at the α = 0.05
level (p = r format(p_treat, digits=3)). The high variability among
patches within treatments likely contributed to the lack of statistical
significance for the treatment effect.
:::

# Assumption Testing

For valid inference from mixed models, several assumptions must be met.
We test these assumptions below.

## Normality of Residuals










::: {.cell}

```{.r .cell-code}
# QQ plot of residuals
qqnorm(resid(mixed_model))
qqline(resid(mixed_model))
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/normality-1.png)
:::

```{.r .cell-code}
# Histogram of residuals
hist(resid(mixed_model), main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 15)
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/normality-2.png)
:::

```{.r .cell-code}
# More advanced residual diagnostics using DHARMa
sim_residuals <- simulateResiduals(fittedModel = mixed_model)
plot(sim_residuals)
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/normality-3.png)
:::
:::










## Homogeneity of Variance










::: {.cell}

```{.r .cell-code}
# Residuals vs. fitted values plot
plot(fitted(mixed_model), resid(mixed_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, lty = 2, col = "red")
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/homogeneity-1.png)
:::
:::

::: {.cell}

```{.r .cell-code}
# Levene's test for homogeneity of variance
levene_test <- leveneTest(algae ~ treat, data = urchin_df)
levene_test
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value     Pr(>F)    
group  3  8.1694 0.00008785 ***
      76                       
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::










::: {.callout-important appearance="simple"}
**Interpretation of Assumption Tests**

The Q-Q plot shows some deviation from normality, particularly in the
tails, and Levene's test indicates significant heterogeneity of
variances across treatments (F = 8.17,
p \< 0.001). As noted in the original analysis, there were "large
differences in within-cell variances" in this dataset, and
transformations did not improve variance homogeneity.

The DHARMa residual diagnostics also indicate potential issues with the
distribution of residuals and homogeneity of variance. The residuals vs.
fitted plot shows a pattern of increasing variance with increasing
fitted values, confirming the heteroscedasticity.

However, mixed models are generally robust to moderate violations of
assumptions, especially with balanced designs. Since transformations
were not effective in improving the data properties, analyzing the
untransformed data is a reasonable approach in this case.
:::

# Post-hoc Comparisons

Although the main effect of treatment was not significant in the nested
ANOVA (p = 0.0913), we can still examine the mean
differences between treatments to understand patterns in the data.










::: {.cell}

```{.r .cell-code}
# Calculate estimated marginal means
emm <- emmeans(mixed_model, ~ treat)
emm
```

::: {.cell-output .cell-output-stdout}

```
 treat   emmean   SE df lower.CL upper.CL
 control    1.3 9.41 12   -19.20     21.8
 dens_33   21.6 9.41 12     1.05     42.0
 dens_66   19.0 9.41 12    -1.50     39.5
 removal   39.2 9.41 12    18.70     59.7

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95 
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Pairwise comparisons with Tukey adjustment
pairs <- pairs(emm, adjust = "tukey")
pairs
```

::: {.cell-output .cell-output-stdout}

```
 contrast          estimate   SE df t.ratio p.value
 control - dens_33   -20.25 13.3 12  -1.522  0.4553
 control - dens_66   -17.70 13.3 12  -1.330  0.5625
 control - removal   -37.90 13.3 12  -2.849  0.0615
 dens_33 - dens_66     2.55 13.3 12   0.192  0.9974
 dens_33 - removal   -17.65 13.3 12  -1.327  0.5646
 dens_66 - removal   -20.20 13.3 12  -1.518  0.4573

Degrees-of-freedom method: kenward-roger 
P value adjustment: tukey method for comparing a family of 4 estimates 
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Compact letter display
cld <- multcomp::cld(emm, alpha = 0.05, Letters = letters)
cld
```

::: {.cell-output .cell-output-stdout}

```
 treat   emmean   SE df lower.CL upper.CL .group
 control    1.3 9.41 12   -19.20     21.8  a    
 dens_66   19.0 9.41 12    -1.50     39.5  a    
 dens_33   21.6 9.41 12     1.05     42.0  a    
 removal   39.2 9.41 12    18.70     59.7  a    

Degrees-of-freedom method: kenward-roger 
Confidence level used: 0.95 
P value adjustment: tukey method for comparing a family of 4 estimates 
significance level used: alpha = 0.05 
NOTE: If two or more means share the same grouping symbol,
      then we cannot show them to be different.
      But we also did not show them to be the same. 
```


:::
:::










# Visualization










::: {.cell}

```{.r .cell-code}
# Create boxplot with jittered points
ggplot_boxplot <- ggplot(urchin_df, aes(x = treat, y = algae, fill = treat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Urchin Density effect on algae Cover",
    x = "Urchin Density ",
    y = "algae Cover (%)",
    caption = "Figure 1: Boxplots showing the distribution of algal cover across urchin density.\nDespite visual differences, the treatment effect was not statistically significant (p = 0.091)."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )

# Create means plot with error bars
means_plot <- ggplot(summary_stats, aes(x = treat, y = mean, group = 1)) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(
    title = "Mean algae Cover by Urchin Density",
    x = "Urchin Density",
    y = "algae Cover (%)",
    caption = "Figure 2: Mean (± SE) percentage cover of algae across urchin density treatments."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )
```
:::

::: {.cell}

```{.r .cell-code}
# Display plots
ggplot_boxplot
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/unnamed-chunk-6-1.png)
:::
:::

::: {.cell}

```{.r .cell-code}
means_plot
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/unnamed-chunk-7-1.png)
:::
:::

::: {.cell}

```{.r .cell-code}
# Combined plot using patchwork
ggplot_boxplot + means_plot + plot_layout(ncol = 1)
```

::: {.cell-output-display}
![](13_04_nested_anova_as_random_files/figure-pptx/unnamed-chunk-8-1.png)
:::
:::










# Discussion

::: {.callout-important appearance="simple"}
**Scientific Interpretation**

Our mixed model analysis of the nested design revealed substantial
spatial heterogeneity in algae cover, with significant variation among
patches within each treatment (p \< 0.001). Surprisingly, the effect of
urchin density treatments on filamentous algae cover was not
statistically significant at the α = 0.05 level (p = 0.091), despite
apparent trends in the data.

The descriptive statistics show a pattern where algae cover appears to
increase as urchin density decreases, with the Control treatment (mean =
1.3%) showing minimal algae cover compared to reduced density treatments
(66% Density: 21.55%, 33% Density: 19.00%, and Removed: 39.20%). This
pattern suggests a potential density-dependent relationship between
urchin grazing and algal abundance, but the high variability among
patches masked the treatment effect.

The substantial variance component associated with patches nested within
treatments (294.31, approximately 39.5% of total variance) underscores
the importance of spatial heterogeneity in structuring algal
communities. This finding highlights the necessity of accounting for
spatial variability when designing and analyzing ecological field
experiments.

From an ecological perspective, these results suggest that while sea
urchins may influence algal communities through grazing, local
environmental factors and patch-specific conditions play a dominant role
in determining algae cover. This has important implications for
ecosystem management, as it indicates that the effects of urchin density
manipulations may be context-dependent and influenced by local
environmental conditions.
:::

# Comparison with Traditional Nested ANOVA

The linear mixed model approach provides similar results to the
traditional nested ANOVA approach. The main advantage of the mixed model
is the more elegant handling of random effects and the extensive
diagnostic tools available through packages like DHARMa.

The mixed model approach confirms that:

1.  treatment effects are not significant (p = 0.091)
2.  patches within treatments show significant variation (p \< 0.001)
3.  The variance components are similar to those from the traditional
    approach

In both methods, the key ecological finding is the strong spatial
heterogeneity in algal cover that overrides the grazing effect of
urchins at different densities.

# References

Andrew, N. L., & Underwood, A. J. (1993). Density-dependent foraging in
the sea urchin Centrostephanus rodgersii on shallow subtidal reefs in
New South Wales, Australia. Marine Ecology Progress Series, 99, 89-98.

Quinn, G. P., & Keough, M. J. (2002). Experimental design and data
analysis for biologists. Cambridge University Press.
