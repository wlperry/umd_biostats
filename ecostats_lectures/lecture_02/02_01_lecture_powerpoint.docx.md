---
title: "Lecture 02"
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
    output-file: "02_01_lecture_powerpoint_html.html"
    code-overflow: scroll
    toc: false
    embed-resources: true
    self-contained: true
    max-width: 80ch
    css: ../../css/lecture.css
    fig-width: 7
    fig-height: 5


  revealjs:
    output-file: "02_01_lecture_powerpoint_slides.html"
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











# Review of Lecture 1

::::: columns
::: {.column width="60%"}
Covered

-   Inductive vs deductive reasoning
-   Formulating research questions
-   Accuracy vs precision
-   Data types and classifications
-   Setting up R projects
-   Installing and loading libraries
-   Reading files into R
-   Creating basic graphs
:::

::: {.column width="40%"}
:::
:::::

# Lecture 2: Project Design & Data Visualization

::::: columns
::: {.column width="60%"}
## The objectives:

1.  Design a well-organized project
2.  Implement good naming conventions
    -   Controlled vocabulary
    -   Including units in names
3.  Create and use metadata effectively
4.  Build tidy, well-structured spreadsheets
5.  Understand data repositories
6.  Create effective visualizations with ggplot2
:::

::: {.column width="40%"}
![](images/data_pasta.png){width="300" height="250"}
:::
:::::

# Project Design: Step 1

::::: columns
::: {.column width="60%"}
-   Data: the raw material of science
-   Wide variety of formats, sizes, complexity
-   Data management and curation often under emphasized
-   Good data management: owe it to our funding agencies, colleagues,
    supervisors, and study systems
:::

::: {.column width="40%"}
![](images/clipboard-2800996557.png){width="300" height="250"}
:::
:::::

# **Lecture 2:** Project Design: Step 1

:::::: columns
:::: {.column width="60%"}
1.  **Determine data types** you'll collect
2.  **Establish controlled vocabulary**
    -   Example: `do_mgl` for dissolved oxygen in mg/L
    -   Example: `drp_ugl` for dissolved reactive phosphorus in μg/L
3.  **Plan your data flow** from collection to analysis
4.  **Organize your project structure** (folders, files)
5.  **Enter data promptly** after collection
6.  **Save in multiple formats** (Excel and CSV)
7.  **Ensure tidy data principles** from the start

::: callout-note
See Hadley Wickham's [Tidy Data
principles](https://r4ds.hadley.nz/data-tidy.html) for best practices
:::
::::

::: {.column width="40%"}
![](images/clipboard-531200679.png){width="300" height="250"}
:::
::::::

# Project Design: Step 2

::::: columns
::: {.column width="60%"}
Create a **Metadata Sheet** that includes:

-   Variable descriptions
-   Units of measurement
-   Collection methods
-   Instrument details
-   Dates and locations
-   Any other relevant contextual information
:::

::: {.column width="40%"}
![](images/clipboard-531200679.png){width="300" height="250"}
:::
:::::

# Practice Exercise 1: Pine Data Organization

::: callout-tip
### Practice Exercise 1: Pine Data Organization

Let's examine our pine needle data: - What naming conventions did you
choose? - How did you organize the data? - How can you verify data
formats (numeric vs categorical)? - What's your plan for organizing
outputs and figures?










::: {.cell}

```{.r .cell-code}
# Code to read and examine data
library(tidyverse)
library(patchwork)
library(flextable)

pine_df <- read_csv("data/pine_needles.csv")
pine_df
```

::: {.cell-output-display}

|date    |group       |n_s |wind | tree_no| length_mm|
|:-------|:-----------|:---|:----|-------:|---------:|
|3/20/25 |cephalopods |n   |lee  |       1|        20|
|3/20/25 |cephalopods |n   |lee  |       1|        21|
|3/20/25 |cephalopods |n   |lee  |       1|        23|
|3/20/25 |cephalopods |n   |lee  |       1|        25|
|3/20/25 |cephalopods |n   |lee  |       1|        21|
|3/20/25 |cephalopods |n   |lee  |       1|        16|
|3/20/25 |cephalopods |s   |wind |       1|        15|
|3/20/25 |cephalopods |s   |wind |       1|        16|
|3/20/25 |cephalopods |s   |wind |       1|        14|
|3/20/25 |cephalopods |s   |wind |       1|        17|
|3/20/25 |cephalopods |s   |wind |       1|        13|
|3/20/25 |cephalopods |s   |wind |       1|        15|
|3/20/25 |salmon      |n   |lee  |       2|        19|
|3/20/25 |salmon      |n   |lee  |       2|        18|
|3/20/25 |salmon      |n   |lee  |       2|        20|
|3/20/25 |salmon      |n   |lee  |       2|        23|
|3/20/25 |salmon      |n   |lee  |       2|        21|
|3/20/25 |salmon      |n   |lee  |       2|        18|
|3/20/25 |salmon      |s   |wind |       2|        12|
|3/20/25 |salmon      |s   |wind |       2|        13|
|3/20/25 |salmon      |s   |wind |       2|        14|
|3/20/25 |salmon      |s   |wind |       2|        12|
|3/20/25 |salmon      |s   |wind |       2|        12|
|3/20/25 |salmon      |s   |wind |       2|        14|
|3/20/25 |crayfish    |n   |lee  |       3|        20|
|3/20/25 |crayfish    |n   |lee  |       3|        21|
|3/20/25 |crayfish    |n   |lee  |       3|        23|
|3/20/25 |crayfish    |n   |lee  |       3|        25|
|3/20/25 |crayfish    |n   |lee  |       3|        21|
|3/20/25 |crayfish    |n   |lee  |       3|        16|
|3/20/25 |crayfish    |s   |wind |       3|        15|
|3/20/25 |crayfish    |s   |wind |       3|        16|
|3/20/25 |crayfish    |s   |wind |       3|        14|
|3/20/25 |crayfish    |s   |wind |       3|        17|
|3/20/25 |crayfish    |s   |wind |       3|        13|
|3/20/25 |crayfish    |s   |wind |       3|        15|
|3/20/25 |snail       |n   |lee  |       4|        19|
|3/20/25 |snail       |n   |lee  |       4|        18|
|3/20/25 |snail       |n   |lee  |       4|        20|
|3/20/25 |snail       |n   |lee  |       4|        23|
|3/20/25 |snail       |n   |lee  |       4|        21|
|3/20/25 |snail       |n   |lee  |       4|        18|
|3/20/25 |snail       |s   |wind |       4|        17|
|3/20/25 |snail       |s   |wind |       4|        16|
|3/20/25 |snail       |s   |wind |       4|        19|
|3/20/25 |snail       |s   |wind |       4|        18|
|3/20/25 |snail       |s   |wind |       4|        15|
|3/20/25 |snail       |s   |wind |       4|        16|

:::
:::









:::

# **Lecture 2:** Data Management: Step 3

::::: columns
::: {.column width="60%"}
**Storage and Backup Strategy**:

1.  Store raw data and metadata securely
    -   Save in both Excel and CSV formats
    -   Consider write-protecting raw data files
2.  Implement the 3-2-1 backup rule:
    -   3 total copies of data
    -   2 different storage media
    -   1 offsite location (cloud storage)
3.  Establish a regular backup schedule
:::

::: {.column width="40%"}
![](images/clipboard-127112346.png){width="100" height="80"}
![](images/clipboard-3792512345.png){width="128" height="100"}
:::
:::::

# **Lecture 2:** Data Management: Step 4

::::: columns
::: {.column width="60%"}
**Initial Data Inspection**:

1.  Examine data in the Environment tab
2.  Run summary() and glimpse() functions
3.  Create exploratory visualizations
4.  Check for outliers, errors, and missing data

``` r
# Specify how to handle missing values during import
pine_df <- read_csv("data/pine_needles.csv", 
                    na = c("", "NA", "N/A", "missing", "null"))

# Get a quick summary
summary(pine_df)
```
:::

::: {.column width="40%"}
![](images/clipboard-247511122.png){width="350" height="280"}
:::
:::::

# Practice Exercise 2: Try plotting a histogram

::: callout-tip
## Practice Exercise 2: Try plotting a histogram of your data

Create a histogram of pine needle lengths to check the distribution:










::: {.cell}

```{.r .cell-code}
# Write your code here to make a plot
# How do you examine the data - what are the ways you think and lets try it!
```
:::









:::

# Lecture 2: Data Management: Step 5

::::: columns
::: {.column width="60%"}
**Data Cleaning**:

1.  Correct errors and inconsistencies
2.  Replace missing values with proper NA codes
3.  Document all changes made to raw data
4.  Save a clean, master version (consider making read-only)
5.  Keep notes on data cleaning procedures
:::

::: {.column width="40%"}
![](images/clipboard-814185367.png){width="350" height="280"}
:::
:::::

# Lecture 2: Data Management: Step 6

::::: columns
::: {.column width="60%"}
**Analysis and Visualization Workflow**:

1.  Create exploratory visualizations
2.  Summarize and transform data as needed
3.  Document all analysis steps
4.  Save outputs systematically
:::

::: {.column width="40%"}
A good way to organize script files is number them in the order they get
run.

![](images/clipboard-2204984243.png){width="300" height="250"}
:::
:::::

# Lecture 2: Effective Data Visualization

::::: columns
::: {.column width="60%"}
## Why make plots?

## Get in a group and discuss

-   What is the purpose of a data visualization?
-   What elements are essential in an effective plot?
-   What characteristics define a "good" plot?
-   What common mistakes make plots ineffective?

[Napoleon's Disastrous Invasion of Russia Detailed in an 1869 Data
Visualization: It's Been Called "the Best Statistical Graphic Ever
Drawn"](https://www.openculture.com/2019/07/napoleons-disastrous-invasion-of-russia-explained-in-an-1869-data-visualization.html)
:::

::: {.column width="40%"}
![](images/clipboard-1704774013.png){width="350" height="280"}
:::
:::::

# **Lecture 2:** Tables vs. Visualizations

**How readable are tables?**

We will get to what these number mean and how to make them in the next
lecture.

-   Tables
    -   are they useful in a presentation?










::: {.cell tbl-responsive='false' tbl-striped='true' css='.table { font-size: 0.85rem; }'}
::: {.cell-output-display}


``````{=openxml}

<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"><w:tblPr><w:tblLayout w:type="fixed"/><w:jc w:val="center"/><w:tblLook w:firstRow="1" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="0" w:noVBand="1"/></w:tblPr><w:tblGrid><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/></w:tblGrid><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/><w:tblHeader/></w:trPr>header1<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">wind</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">n</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">mean_mm</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">sd_mm</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">min_mms</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">max_mm</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body1<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">lee</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">24</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">20</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">2.45</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">16</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">25</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body2<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">wind</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">24</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">15</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">1.91</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">12</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">19</w:t></w:r></w:p></w:tc></w:tr></w:tbl>
``````



:::
:::










# **Lecture 2:** Displaying data

::::: columns
::: {.column width="60%"}
-   **how does a table compare to a plot?**

-   Does this help?

-   What is this plot?

    -   if you don't explain does the audience know?
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](02_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-3-1.png)
:::
:::









:::
:::::

# **Lecture 2:** Principles of Effective Graphics

According to [Tufte
(2001)](https://www.edwardtufte.com/book/the-visual-display-of-quantitative-information/),
good scientific graphics:

1.  **Show the data** without distortion
2.  **Maximize data-ink ratio** (minimize non-data elements)
3.  **Make large datasets coherent** and understandable
4.  **Encourage comparison** between elements
5.  **Reveal multiple layers** of information
6.  **Serve a clear purpose** in telling your story
7.  **Integrate with statistical methods** appropriately










::: {.cell}
::: {.cell-output-display}

|group       |  mean_mm|    sd_mm|  n|     se_mm| conf_low| conf_high|
|:-----------|--------:|--------:|--:|---------:|--------:|---------:|
|cephalopods | 18.00000| 3.861229| 12| 1.1146409| 15.54669|  20.45331|
|crayfish    | 18.00000| 3.861229| 12| 1.1146409| 15.54669|  20.45331|
|salmon      | 16.33333| 3.938928| 12| 1.1370705| 13.83066|  18.83601|
|snail       | 18.33333| 2.269695| 12| 0.6552045| 16.89124|  19.77543|

:::
:::










# **Lecture 2:** Creating Effective Graphics

::::: columns
::: {.column width="60%"}
According to [Tufte
(2001)](https://www.edwardtufte.com/book/the-visual-display-of-quantitative-information/),
good scientific graphics:

-   To implement these principles:
    -   Focus on the data, not decorative elements
    -   Ensure proportional representation of numbers
    -   Provide clear and informative labels
    -   Remove unnecessary elements ("chart junk")
    -   Revise and refine visualizations iteratively
:::

::: {.column width="40%"}









::: {.cell}

:::









:::
:::::

# **Lecture 2:** Displaying data - Good Graphics

::::: columns
::: {.column width="60%"}
To make good graphics:

-   Above all, focus on data
-   Do not distort data
-   Graphical representation of numbers → directly proportional to
    numbers
-   Strive for clarity through labeling
-   Maximize data-ink ratio
    -   Remove non-data ink
    -   Reduce redundant data ink
-   Revise and redraw
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](02_01_lecture_powerpoint_files/figure-docx/good-graphics-2-1.png)
:::
:::









:::
:::::

# **Lecture 2:** Displaying data - Poor Example

::::: columns
::: {.column width="60%"}
What do you think?

Does this -

-   Focus on data
-   Distort data
-   Is it directly proportional to numbers
-   Is labeling clear
-   Maximize data-ink ratio
    -   Remove non-data ink
    -   Reduce redundant data ink
-   Revise and redraw
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](02_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-5-1.png)
:::
:::









:::
:::::

# **Lecture 2:** Displaying data - Better Example

::::: columns
::: {.column width="60%"}
What do you think?

Does this -

-   Focus on data
-   Distort data
-   Is it directly proportional to numbers
-   Is labeling clear
-   Maximize data-ink ratio
    -   Remove non-data ink
    -   Reduce redundant data ink
-   Revise and redraw

What is one of the most common plots you make all the time?
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](02_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-6-1.png)
:::
:::









:::
:::::

# **Lecture 2:** Displaying data - Common Problems

::::: columns
::: {.column width="60%"}
## Common Visualization Problems

1.  **Data distortion**:
    -   Non-zero baselines on bar charts
    -   3D effects that skew perspective
    -   Inappropriate scales
2.  **Excessive "chart junk"**:
    -   Too many gridlines
    -   Unnecessary decorative elements
    -   Redundant information
3.  **Poor color choices**:
    -   Too many colors
    -   Non-colorblind-friendly palettes
    -   Colors that don't print well in grayscale
4.  **Misleading representations**:
    -   Pie charts with too many categories
    -   Dual y-axes with different scales
    -   Truncated axes without clear indication
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](02_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-7-1.png)
:::
:::









:::
:::::

# Practice Exercise 3: Basic Plots with Pine Data

::: callout-tip
## Practice Exercise 3: Lets try some plots with pine data first

Lets try to make some basic plots

```         
# Write your code here to make a dot plot or X y plot
# How do you examine the data - what are the ways you think and lets try it!
# what is missing - hwo do you tell the effect of wind?
```
:::

# Practice Exercise 4: Colors, Shapes, and Fills

::: callout-tip
## Practice Exercise 4: OK we are closer but what about colors or shape or fills

Lets try to make some more basic plots

This is free time - we will free code this....

Below are some examples of code you will need for the future

```         
# Write your code here to make a dot plot or X y plot
# How do you examine the data - what are the ways you think and lets try it!
# what is missing - hwo do you tell the effect of wind?
```
:::

# **Lecture 2:** Introduction to the Grammar of Graphics - ggPLOT

We will learn the anatomy of a GGplot is layers

-   ggplot2 uses a **layered grammar of graphics** approach:
    1.  **Data**: The dataset you're visualizing
    2.  **Aesthetics**: Mapping variables to visual properties
    3.  **Geometries**: The visual elements representing data
    4.  **Facets**: Splitting visualization into subplots
    5.  **Statistics**: Statistical transformations of the data
    6.  **Coordinates**: The space in which data is plotted
    7.  **Themes**: Overall visual style of the plotWe have aesthetics

# **Lecture 2:** Building a ggplot Visualization

### Key Components:

1.  **Aesthetics (aes)** map variables to visual properties:
    -   x and y positions
    -   color, fill, shape, size, alpha
    -   group, linetype
2.  \*\*Geometries (geom\_\*)\*\* determine how data is displayed:
    -   `geom_point()`: Scatter plots
    -   `geom_line()`: Line graphs
    -   `geom_boxplot()`: Box-and-whisker plots
    -   `geom_violin()`: Violin plots
    -   `geom_histogram()`: Histograms
    -   `geom_bar()`: Bar charts
3.  **Position adjustments** control how elements are arranged:
    -   `position_dodge()`: Side-by-side elements
    -   `position_jitter()`: Add random noise to points
    -   `position_stack()`: Stack elements on top of each other
4.  **Labels and annotations** provide context:
    -   `labs()`: Title, subtitle, caption, axis labels
    -   `annotate()`: Add text, shapes, etc.

# **Lecture 2:** Fine-tuning your visualizations

1.  **Colors, fills, and shapes**:

    ```         
    scale_color_manual(
      values = c("wind" = "darkblue", "lee" = "darkred"),
      labels = c("wind" = "Windward", "lee" = "Leeward")
    )
    ```

2.  **Coordinate systems**:

    ```         
    coord_cartesian(ylim = c(10, 30))  # Zoom in without dropping data
    ```

3.  **Themes**:

    ```         
    theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      legend.position = "bottom"
    )
    ```

4.  **Combining plots with patchwork**:

    ```         
    plot1 + plot2 + plot_layout(ncol = 2)
    ```

# Practice Exercise 5: Publication-Quality Plot

::: callout-tip
### Practice Exercise 4: Creating a Publication-Quality Plot

Create a fully customized plot that would be suitable for publication:

```         
# Create a publication-quality plot
pine_df %>%
  ggplot(aes(x = wind, y = length_mm, fill = wind)) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "gray30", size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Pine Needle Length Varies with Wind Exposure",
    subtitle = "Needles on the leeward side tend to be longer",
    x = "Tree Side", 
    y = "Needle Length (mm)",
    caption = "Data collected Spring 2023") +
  scale_fill_manual(
    values = c("wind" = "#1b9e77", "lee" = "#d95f02"),
    labels = c("wind" = "Windward", "lee" = "Leeward")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom")
```
:::

# Key Takeaways

1.  **Plan your data management** from the beginning
    -   Consistent naming conventions
    -   Good organization
    -   Regular backups
2.  **Make your data tidy** from the start
    -   One observation per row
    -   One variable per column
    -   One value per cell
3.  **Create effective visualizations** by:
    -   Focusing on data, not decoration
    -   Using appropriate plot types
    -   Following good design principles
    -   Customizing for clear communication
4.  **Master the grammar of graphics** to:
    -   Build plots layer by layer
    -   Communicate patterns clearly
    -   Tell compelling stories with data

# Next Steps

-   Practice creating different types of plots
-   Learn to combine multiple plots effectively
-   Explore statistical transformations in ggplot2
-   Develop a consistent visualization style
