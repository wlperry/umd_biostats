---
title: "Two Sample T-Test"
author: "Bill Perry"
execute:
  freeze: auto
  cache: true
  echo: true
  keep-md: true # retains the images when you start again
  fig-width: 3
  fig-height: 4
format:
  html:
    toc: false
    output-file: "two_sample_ttest_html.html"
    embed-resources: true
    self-contained: true
    css: ../css/activity.css
  docx:
    toc: false
    number-sections: false
    highlight-style: github
    reference-doc: ../ms_templates/custom-reference.docx
    embed-resources: true
---

# Introduction to Two-Sample t-Test

## Background and Theory

The two-sample t-test (also known as independent samples t-test) is used
to determine whether there is a statistically significant difference
between the means of two independent groups. In this analysis, we will
examine whether there are significant differences in the total length of
slimy sculpin fish between two different lakes.

The two-sample t-test makes the following comparison:

$$H_0: \mu_1 = \mu_2$$ $$H_A: \mu_1 \neq \mu_2$$

Where:

-   \- $H_0$ is the null hypothesis stating that the population means
    are equal

-   \- $H_A$ is the alternative hypothesis stating that the population
    means are different

-   \- $\mu_1$ is the population mean of the first group

-   \- $\mu_2$ is the population mean of the second group

## Formula

The formula for the two-sample t-test with equal variances (pooled
variance) is:

$$t = \frac{\bar{x}_1 - \bar{x}_2}{s_p \sqrt{\frac{1}{n_1} + \frac{1}{n_2}}}$$

Where:

-   \- $\bar{x}_1$ is the sample mean of the first group

-   \- $\bar{x}_2$ is the sample mean of the second group

-   \- $s_p$ is the pooled standard deviation

-   \- $n_1$ is the sample size of the first group

-   \- $n_2$ is the sample size of the second group

The pooled standard deviation is calculated as:

$$s_p = \sqrt{\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1 + n_2 - 2}}$$

Where:

-   \- $s_1^2$ is the variance of the first group

-   \- $s_2^2$ is the variance of the second group

-   The degrees of freedom (df) for this test is $n_1 + n_2 - 2$.

**For unequal variances (Welch's t-test), the formula is slightly
different:**

$$t = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}}$$

**With degrees of freedom approximated using the Welch-Satterthwaite
equation:**

$$df = \frac{(\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2})^2}{\frac{(s_1^2/n_1)^2}{n_1-1} + \frac{(s_2^2/n_2)^2}{n_2-1}}$$

# Data Analysis

## Loading Libraries and Data

::: {.cell}

```{.r .cell-code}
# Load required libraries
# install.packages("gt")
library(gt)
library(broom)
library(car)  # For Levene's test
```

::: {.cell-output .cell-output-stderr}

```
Loading required package: carData
```


:::

```{.r .cell-code}
# library(ggpubr)  # For adding p-values to plots
library(coin)  # For permutation tests
```

::: {.cell-output .cell-output-stderr}

```
Loading required package: survival
```


:::

```{.r .cell-code}
library(rcompanion)  # For plotNormalHistogram
library(skimr)
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}

```
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.2     ✔ tibble    3.3.0
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
✔ purrr     1.1.0     
```


:::

::: {.cell-output .cell-output-stderr}

```
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
✖ dplyr::recode() masks car::recode()
✖ purrr::some()   masks car::some()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```


:::

```{.r .cell-code}
# Load the data
sculpin_df <- read_csv("data/t_test_sculpin_s07_ne14.csv")
```

::: {.cell-output .cell-output-stderr}

```
Rows: 110 Columns: 5
── Column specification ────────────────────────────────────────────────────────
Delimiter: ","
chr (2): lake, species
dbl (3): site, length_mm, mass_g

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


:::

```{.r .cell-code}
# Preview the data
head(sculpin_df)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 5
   site lake  species       length_mm mass_g
  <dbl> <chr> <chr>             <dbl>  <dbl>
1   109 NE 14 slimy sculpin        47   0.7 
2   109 NE 14 slimy sculpin        49   0.9 
3   109 NE 14 slimy sculpin        46   0.7 
4   109 NE 14 slimy sculpin        28   0.15
5   109 NE 14 slimy sculpin        45   0.65
6   109 NE 14 slimy sculpin        40   0.3 
```


:::
:::

## Data Overview

Let's first examine the structure of our dataset:

::: {.cell}

```{.r .cell-code}
sculpin_df %>% 
  group_by(lake) %>% 
  skim()
```

::: {.cell-output-display}

Table: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |Piped data |
|Number of rows           |110        |
|Number of columns        |5          |
|_______________________  |           |
|Column type frequency:   |           |
|character                |1          |
|numeric                  |3          |
|________________________ |           |
|Group variables          |lake       |


**Variable type: character**

|skim_variable |lake  | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:-------------|:-----|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|species       |NE 14 |         0|             1|  13|  13|     0|        1|          0|
|species       |S 07  |         0|             1|  13|  13|     0|        1|          0|


**Variable type: numeric**

|skim_variable |lake  | n_missing| complete_rate|   mean|    sd|     p0|    p25|    p50|    p75|   p100|hist  |
|:-------------|:-----|---------:|-------------:|------:|-----:|------:|------:|------:|------:|------:|:-----|
|site          |NE 14 |         0|             1| 109.00|  0.00| 109.00| 109.00| 109.00| 109.00| 109.00|▁▁▇▁▁ |
|site          |S 07  |         0|             1| 152.00|  0.00| 152.00| 152.00| 152.00| 152.00| 152.00|▁▁▇▁▁ |
|length_mm     |NE 14 |         0|             1|  47.27| 10.49|  25.00|  42.00|  48.00|  54.00|  67.00|▂▃▇▇▂ |
|length_mm     |S 07  |         0|             1|  55.56| 12.65|  31.00|  45.00|  57.00|  64.00|  87.00|▅▅▇▃▂ |
|mass_g        |NE 14 |         0|             1|   0.89|  0.52|   0.10|   0.45|   0.85|   1.25|   2.30|▇▇▇▂▁ |
|mass_g        |S 07  |         0|             1|   1.66|  1.23|   0.25|   0.80|   1.45|   2.10|   7.37|▇▃▁▁▁ |


:::
:::

### Manual Summary Method


::: {.cell}

```{.r .cell-code}
stats_df <- sculpin_df %>% 
  group_by(lake) %>% 
  summarize(mean_length_mm = round(mean(length_mm, na.rm=TRUE),2),
            stddev_length_mm = round(sd(length_mm, na.rm=TRUE),2),
            stderr_length_mm = round(sd(length_mm, na.rm=TRUE)/sum(!is.na(length_mm)),2),
            coef_var_length_mm = 
              round((sd(length_mm, na.rm=TRUE)/mean(length_mm, na.rm=TRUE))*100,2))
stats_df
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  lake  mean_length_mm stddev_length_mm stderr_length_mm coef_var_length_mm
  <chr>          <dbl>            <dbl>            <dbl>              <dbl>
1 NE 14           47.3             10.5             0.28               22.2
2 S 07            55.6             12.6             0.17               22.8
```


:::
:::


### Fancy Table with the tidytable package

::: {.cell}

```{.r .cell-code}
gt_table <- stats_df %>% 
  gt() %>% 
  tab_header(
    title = "Table 1. Sculpin Length Statistics by Lake") %>% 
  cols_label(
    lake = "Lake",
    mean_length_mm = "Mean Length (mm)",
    stddev_length_mm = "Std Dev (mm)",
    stderr_length_mm = "Std Error (mm)",
    coef_var_length_mm = "CV (%)"
  )%>%
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid",
    table_body.border.top.style = "none",
    table_body.hlines.style = "none")%>%
   opt_align_table_header(align = "left")
gt_table
```

::: {.cell-output-display}
```{=openxml}
<w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="start"/>
    <w:pStyle w:val="caption"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="24"/>
    </w:rPr>
    <w:t xml:space="preserve">Table </w:t>
  </w:r>
  <w:r>
    <w:fldChar w:fldCharType="begin" w:dirty="true"/>
  </w:r>
  <w:r>
    <w:instrText xml:space="preserve" w:dirty="true"> SEQ Table \* ARABIC </w:instrText>
  </w:r>
  <w:r>
    <w:fldChar w:fldCharType="separate" w:dirty="true"/>
  </w:r>
  <w:r>
    <w:rPr>
      <w:noProof/>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="24"/>
    </w:rPr>
    <w:t xml:space="default">1</w:t>
  </w:r>
  <w:r>
    <w:fldChar w:fldCharType="end" w:dirty="true"/>
  </w:r>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="24"/>
    </w:rPr>
    <w:t xml:space="preserve">: </w:t>
  </w:r>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="24"/>
      <w:color w:val="333333"/>
    </w:rPr>
    <w:t xml:space="default">Table 1. Sculpin Length Statistics by Lake</w:t>
  </w:r>
</w:p><w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:tblPr><w:tblCellMar><w:top w:w="0" w:type="dxa"></w:top><w:bottom w:w="0" w:type="dxa"></w:bottom><w:start w:w="60" w:type="dxa"></w:start><w:end w:w="60" w:type="dxa"></w:end></w:tblCellMar><w:tblW w:type="auto" w:w="0"></w:tblW><w:tblLook w:firstRow="0" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="0" w:noVBand="0"></w:tblLook><w:jc w:val="center"></w:jc></w:tblPr><w:tr><w:trPr><w:cantSplit></w:cantSplit><w:tblHeader></w:tblHeader></w:trPr><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="start"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">Lake</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:bottom></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">Mean Length (mm)</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:bottom></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">Std Dev (mm)</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:bottom></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">Std Error (mm)</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:sz="16" w:space="0" w:color="D3D3D3"></w:bottom><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">CV (%)</w:t>
  </w:r>
</w:p></w:tc></w:tr><w:tr><w:trPr><w:cantSplit></w:cantSplit></w:trPr><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="start"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">NE 14</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">47.27</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">10.49</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">0.28</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">22.19</w:t>
  </w:r>
</w:p></w:tc></w:tr>
<w:tr><w:trPr><w:cantSplit></w:cantSplit></w:trPr><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="start"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">S 07</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">55.56</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">12.65</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">0.17</w:t>
  </w:r>
</w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:top w:val="single" w:space="0" w:color="D3D3D3"></w:top><w:bottom w:val="single" w:space="0" w:color="D3D3D3"></w:bottom><w:start w:val="single" w:space="0" w:color="D3D3D3"></w:start><w:end w:val="single" w:space="0" w:color="D3D3D3"></w:end></w:tcBorders></w:tcPr><w:p>
  <w:pPr>
    <w:spacing w:before="0" w:after="60"/>
    <w:keepNext/>
    <w:jc w:val="end"/>
  </w:pPr>
  <w:r>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri"/>
      <w:sz w:val="20"/>
    </w:rPr>
    <w:t xml:space="default">22.77</w:t>
  </w:r>
</w:p></w:tc></w:tr></w:tbl>
```


:::
:::

## 

## Data Visualization

### Box Plot with Individual Data Points

Let's create a box plot with individual data points to visualize the
distribution of total length in the two lakes:


::: {.cell}

```{.r .cell-code}
# Create boxplot with individual points
sculpin_histo_plot <- sculpin_df %>%  
  ggplot(aes(x = lake, y = length_mm, fill = lake)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 2) +
  labs(
    title = "Total Length of Slimy Sculpin Fish by Lake",
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  ) 
sculpin_histo_plot
```

::: {.cell-output-display}
![](01_two_sample_ttest_files/figure-docx/unnamed-chunk-4-1.png)
:::
:::


### Mean and Standard Error Plot

Now, let's create a plot showing the mean and standard error for each
lake, with individual data points in the background:

::: {.cell}

```{.r .cell-code}
# Create mean and standard error plot with data points

sculpin_df %>% 
ggplot( aes(x = lake, y = length_mm, color = lake)) +
  # Add individual data points in the background
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 1.5) +
  # Add mean and standard error
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(
    title = "Mean Total Length (± SE) of Slimy Sculpin Fish by Lake",
    x = "Lake",
    y = "Total Length (mm)",
    color = "Lake"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  ) 
```

::: {.cell-output-display}
![](01_two_sample_ttest_files/figure-docx/unnamed-chunk-5-1.png)
:::
:::

# Testing t-Test Assumptions

Before conducting the t-test, we need to verify that our data meets the
underlying assumptions:

## Assumptions of the Two-Sample t-Test

1.  **Independence**: The observations within each group are
    independent, and the two groups are independent of each other.
2.  **Normality**: The data in each group follow a normal distribution.
3.  **Homogeneity of Variances**: The variances of the two groups are
    approximately equal (for the standard t-test).

Let's test each of these assumptions:

### 1. Independence Assumption

Independence is a design issue and can't be tested statistically. We
assume our sampling design ensures independence between and within
groups.

### 2. Normality Assumption

We'll check normality using:

-   \- Histograms

-   \- Q-Q plots

-   \- Shapiro-Wilk test

#### Histograms


::: {.cell}

```{.r .cell-code}
sculpin_histo_plot
```

::: {.cell-output-display}
![](01_two_sample_ttest_files/figure-docx/sculpin_histo_2-1.png)
:::
:::


### QQ Plots


::: {.cell}

```{.r .cell-code}
# QQ plot for lakes
sculpin_df %>%  
  ggplot( aes(sample = length_mm, color=lake)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~lake, scales = "free") +
  labs(title = "Normal Q-Q Plots by Lake",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
```

::: {.cell-output-display}
![](01_two_sample_ttest_files/figure-docx/unnamed-chunk-6-1.png)
:::
:::


### Shapiro-Wilk Test


::: {.cell}

```{.r .cell-code}
# Simple approach - just split by lake and run the test
sculpin_df %>%
  filter(lake == "S 07") %>%
  pull(length_mm) %>%
  shapiro.test()
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  .
W = 0.98035, p-value = 0.3125
```


:::

```{.r .cell-code}
sculpin_df %>%
  filter(lake == "NE 14") %>%
  pull(length_mm) %>%
  shapiro.test()
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  .
W = 0.9479, p-value = 0.08258
```


:::
:::


### nicer way #1


::: {.cell}

```{.r .cell-code}
# using the broom package
sculpin_df %>%
  group_by(lake) %>%
  group_modify(~ broom::tidy(shapiro.test(.x$length_mm)))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
# Groups:   lake [2]
  lake  statistic p.value method                     
  <chr>     <dbl>   <dbl> <chr>                      
1 NE 14     0.948  0.0826 Shapiro-Wilk normality test
2 S 07      0.980  0.313  Shapiro-Wilk normality test
```


:::
:::


### nicer way #2


::: {.cell}

```{.r .cell-code}
normality_results <- sculpin_df %>%
  group_by(lake) %>%
  summarize(
    shapiro_stat = shapiro.test(length_mm)$statistic,
    shapiro_p_value = shapiro.test(length_mm)$p.value,
    normal_distribution = if_else(shapiro_p_value > 0.05, "Normal", "Non-normal"))
normality_results
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
  lake  shapiro_stat shapiro_p_value normal_distribution
  <chr>        <dbl>           <dbl> <chr>              
1 NE 14        0.948          0.0826 Normal             
2 S 07         0.980          0.313  Normal             
```


:::
:::


### nicer way #3

::: {.cell}

```{.r .cell-code}
sculpin_df %>%
  group_by(lake) %>%
  group_walk(~ {
    cat("Shapiro-Wilk test for Lake", .y$lake, ":\n")
    test_result <- shapiro.test(.x$length_mm)
    print(test_result)
    cat("\n")
  })
```

::: {.cell-output .cell-output-stdout}

```
Shapiro-Wilk test for Lake NE 14 :

	Shapiro-Wilk normality test

data:  .x$length_mm
W = 0.9479, p-value = 0.08258


Shapiro-Wilk test for Lake S 07 :

	Shapiro-Wilk normality test

data:  .x$length_mm
W = 0.98035, p-value = 0.3125
```


:::
:::

### 3. Homogeneity of Variances

We'll check for homogeneity of variances using:

-   Visual inspection of boxplots (already done above)
-   Levene's test

::: {.cell}

```{.r .cell-code}
# Levene's test for homogeneity of variances
leveneTest(length_mm ~ lake, data = sculpin_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in leveneTest.default(y = y, group = group, ...): group coerced to
factor.
```


:::

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   1   2.029 0.1572
      108               
```


:::
:::

## Interpretation of Assumption Tests

Based on the results of our assumption tests:

1.  **Independence**: We assume this is met based on the data collection
    process, as samples from each lake were collected independently of
    one another.

2.  **Normality**:

    -   The Q-Q plots show that the data points largely follow the
        theoretical normal distribution line for both lakes, with some
        minor deviations at the extremes.
    -   The Shapiro-Wilk test results will help us formally assess
        normality. If the p-value is greater than 0.05, we fail to
        reject the null hypothesis that the data is normally
        distributed.
    -   For samples larger than 30, the Central Limit Theorem suggests
        that the sampling distribution of means will be approximately
        normal regardless of the underlying distribution.

3.  **Homogeneity of Variances**:

    -   Levene's test evaluates whether the variances between groups are
        equal.
    -   A p-value greater than 0.05 indicates that we cannot reject the
        null hypothesis of equal variances.
    -   As a rule of thumb, if the variance ratio is less than 4:1, the
        t-test is reasonably robust to violations of this assumption.
    -   If this assumption is violated, we should consider using Welch's
        t-test instead, which does not assume equal variances.

# Two-Sample t-Test

Now that we've checked the assumptions, we can perform the two-sample
t-test:

::: {.cell}

```{.r .cell-code}
# Perform the t-test with equal variance (standard t-test)
t_test_equal_var <- t.test(
  length_mm ~ lake,
  data = sculpin_df,
  var.equal = TRUE  # Use pooled variance
)

# Display the results
t_test_equal_var
```

::: {.cell-output .cell-output-stdout}

```

	Two Sample t-test

data:  length_mm by lake
t = -3.4314, df = 108, p-value = 0.0008519
alternative hypothesis: true difference in means between group NE 14 and group S 07 is not equal to 0
95 percent confidence interval:
 -13.080929  -3.501818
sample estimates:
mean in group NE 14  mean in group S 07 
           47.27027            55.56164 
```


:::
:::

::: {.cell}

```{.r .cell-code}
# For comparison, also perform Welch's t-test (unequal variances)
t_test_welch <- t.test(
  length_mm ~ lake,
  data = sculpin_df,
  var.equal = FALSE  # Use Welch's correction
)


t_test_welch
```

::: {.cell-output .cell-output-stdout}

```

	Welch Two Sample t-test

data:  length_mm by lake
t = -3.6483, df = 85.45, p-value = 0.0004533
alternative hypothesis: true difference in means between group NE 14 and group S 07 is not equal to 0
95 percent confidence interval:
 -12.809687  -3.773061
sample estimates:
mean in group NE 14  mean in group S 07 
           47.27027            55.56164 
```


:::
:::

## Line-by-Line Interpretation of t-Test Results

Let's break down the t-test output:

1.  **Test Type**: Two Sample t-test
2.  **Formula**: `length_mm ~ lake` means we're testing if total length
    differs by lake
3.  **Data**: Our filtered sculpin dataset
4.  **t-value**: The calculated t-statistic
5.  **Degrees of Freedom (df)**: n₁ + n₂ - 2
6.  **p-value**: The probability of observing this data (or more
    extreme) if the null hypothesis is true
7.  **Alternative Hypothesis**: The means are different
8.  **95% Confidence Interval**: The estimated range for the true
    difference in means
9.  **Sample Estimates**: The means of each group

## Visual Representation of t-Test Results


::: {.cell}

```{.r .cell-code}
sculpin_df %>% 
  ggplot( aes(x = lake, y = length_mm, fill = lake)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_dodge2(width = 0.3), 
             alpha = 0.5, size = 2) +
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Set2")
```

::: {.cell-output-display}
![](01_two_sample_ttest_files/figure-docx/final_histo_plot-1.png)
:::
:::



::: {.cell}

```{.r .cell-code}
sculpin_df %>% 
  ggplot(aes(x = lake, y = length_mm, color=lake, shape = lake, fill = lake)) +
  stat_summary(fun = mean, geom = "point", alpha = 0.7, size=3) +  # bars for means
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # error bars for SE
  labs(
    x = "Lake",
    y = "Total Length (mm)",
    fill = "Lake",
    color="Lake",
    shape = "Lake"
  ) +
  coord_cartesian(ylim = c(0, 60))+
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  ) 
```

::: {.cell-output-display}
![](01_two_sample_ttest_files/figure-docx/final_mean_se-1.png)
:::
:::


# Conclusion and Scientific Reporting


::: {.cell}

```{.r .cell-code}
# Calculate means and standard errors for reporting
mean_se_by_lake <- sculpin_df %>%
  group_by(lake) %>%
  summarize(
    n = n(),
    mean = mean(length_mm),
    sd = sd(length_mm),
    se = sd / sqrt(n)
  )

mean_se_by_lake
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  lake      n  mean    sd    se
  <chr> <int> <dbl> <dbl> <dbl>
1 NE 14    37  47.3  10.5  1.72
2 S 07     73  55.6  12.7  1.48
```


:::

```{.r .cell-code}
# Calculate percent difference
percent_diff <- abs(diff(mean_se_by_lake$mean)) / min(mean_se_by_lake$mean) * 100
percent_diff
```

::: {.cell-output .cell-output-stdout}

```
[1] 17.54036
```


:::
:::


Based on our analysis, we can conclude:

The total length of slimy sculpin fish differs significantly between
Lake S 07 and Lake NE 14 (two-sample t-test: t(`df`) = `t_statistic`, p
\< 0.001). Fish from Lake S 07 were on average `(mean_diff)` mm longer
than those from Lake NE 14 (mean ± SE: mm).

## How to Report These Results in a Scientific Publication

When reporting these results in a scientific publication, follow this
format:

"Slimy sculpin (*Cottus cognatus*) from Lake S 07 were significantly
larger than those from Lake NE 14

respectively; two-sample t-test: t(df) = `t_statistic`, p \< 0.001).
This represents an approximately`percent_diff`% difference in total
length between the two populations."

For figures, include:

1.  A boxplot or mean/SE plot showing the difference
2.  Clear labels and scales
3.  Sample sizes
4.  Statistical test information in the figure caption

A typical caption would read:

Note I would also add the mean and SE of each lake

"Figure X. Total length (mean ± SE) of slimy sculpin fish from two
Arctic lakes. Fish from Lake S 07 (n = 73) were significantly larger
than those from Lake NE 14 (n = 37) (two-sample t-test: t(108) = 3.46, p
\< 0.001)."
