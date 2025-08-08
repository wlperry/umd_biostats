---
title: "Lecture 05: Probability and Statistical Inference"
author: "Bill Perry"
execute:
  freeze: auto
  cache: true
  keep-md: true # retains the images when you start again
  message: false
  warning: false
  fig-height: 4
  fig-width: 6
format:
  html:
    toc: false
    output-file: "05_01_lecture_powerpoint_html.html"
    embed-resources: true
    self-contained: true
    max-width: 80ch  # Limits line length to approximately 80 characters
    css: ../../css/lecture.css
    fig-width: 7
    fig-height: 5

  # RevealJS - optimized for presentation (keeps two-column layout and large images)
  revealjs:
    output-file: "05_01_lecture_powerpoint_slides.html"
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











# **Lecture 4: Review**

::::: columns
::: {.column width="60%"}
-   Introduction to histograms or frequency distributions
-   Probability Distribution Functions (PDF)
-   Descriptive Statistics
    -   Center - mean, median, mode

    -   Spread - range, variance, standard deviation
:::

::: {.column width="40%"}
![](images/clipboard-536528302.png){width="241" height="269"}
:::
:::::

# **Lecture 4: Review - Statistical Concepts**

::::: columns
::: {.column width="60%"}
-   Introduction to histograms or frequency distributions
-   Probability Distribution Functions (PDF)
-   Descriptive Statistics
    -   Center - mean, median, mode

    -   Spread - range, variance, standard deviation
:::

::: {.column width="40%"}
![](images/clipboard-3257239263.png){width="350" height="250"}
:::
:::::

# **Lecture 4: Review - Summary Statistics**

-   Introduction to histograms or frequency distributions
-   Probability Distribution Functions (PDF)
-   Descriptive Statistics
    -   Center - mean, median, mode

    -   Spread - range, variance, standard deviation










::: {.cell}
::: {.cell-output-display}


``````{=openxml}

<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"><w:tblPr><w:tblLayout w:type="fixed"/><w:jc w:val="center"/><w:tblLook w:firstRow="1" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="0" w:noVBand="1"/></w:tblPr><w:tblGrid><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/><w:gridCol w:w="1080"/></w:tblGrid><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/><w:tblHeader/></w:trPr>header1<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">lake</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">mean_length</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">sd_length</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">se_length</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">count</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body1<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">I3</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">265.6061</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">28.30378</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">3.483954</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">66</w:t></w:r></w:p></w:tc></w:tr><w:tr><w:trPr><w:trHeight w:val="360" w:hRule="auto"/></w:trPr>body2<w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">I8</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">362.5980</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">52.33901</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">5.182334</w:t></w:r></w:p></w:tc><w:tc><w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="12" w:space="0" w:color="666666"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="FFFFFF"/><w:tcMar><w:top w:w="0" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/><w:left w:w="0" w:type="dxa"/><w:right w:w="0" w:type="dxa"/></w:tcMar><w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:jc w:val="right"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="100" w:before="100" w:line="240"/><w:ind w:left="100" w:right="100" w:firstLine="0" w:firstLineChars="0"/><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr></w:pPr><w:r xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/><w:i w:val="false"/><w:b w:val="false"/><w:u w:val="none"/><w:sz w:val="22"/><w:szCs w:val="22"/><w:color w:val="000000"/></w:rPr><w:t xml:space="preserve">102</w:t></w:r></w:p></w:tc></w:tr></w:tbl>
``````



:::
:::










# **Lecture 5: Probability and Statistical Inference**

::::: columns
::: {.column width="60%"}
The goals for today

-   Statistical inference fundamentals
-   Hypothesis testing principles
-   T Distributions
-   One sample T Tests
-   Two sample T Test
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-2-1.png)
:::
:::









:::
:::::

# **Lecture 5:** Confidence intervals

:::: columns
::: {.column width="60%"}
In the more typical case DON'T know the population σ or standard
deviation

-   estimate it from the samples
-   and when sample size is \<\~30)
-   can't use the standard normal (z) distribution

*Instead, we use Student's t distribution*
:::

![](images/clipboard-1052237789.png){width="300" height="250"}
::::

# **Lecture 5:** Understanding t-distribution

::::: columns
::: {.column width="60%"}
When sample sizes are small, the **t-distribution** is more appropriate
than the normal distribution.

-   Similar to normal distribution but with heavier tails
-   Shape depends on **degrees of freedom** (df = n-1)
-   With large df (\>30), approaches the normal distribution
-   Used for:
    -   Small sample sizes

    -   When population standard deviation is unknown

    -   Calculating confidence intervals

    -   Conducting t-tests
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-3-1.png)
:::
:::









:::
:::::

# **Lecture 5:** t-distribution Properties

::::: columns
::: {.column width="50%"}
When sample sizes are small, the **t-distribution** is more appropriate
than the normal distribution.

-   Similar to normal distribution (1.96 = 2.5% tails) but with heavier
    tails
-   Shape depends on **degrees of freedom** (df = n-1)
-   With large df (\>30), approaches the normal distribution
-   Used for:
    -   Small sample sizes

    -   When population standard deviation is unknown

    -   Calculating confidence intervals

    -   Conducting t-tests
:::

::: {.column width="50%"}
![](images/clipboard-3203878802.png){width="350" height="280"}
:::
:::::

# Practice Exercise 4: Using the t-distribution

::: callout-tip
## Practice Exercise 4: Using the t-distribution

Let's compare confidence intervals using the normal approximation (z)
versus the t-distribution for our fish data.










::: {.cell}

```{.r .cell-code}
# Calculate CI using both z and t distributions for a smaller subset
small_sample <- grayling_df %>% 
  filter(lake == "I3") %>% 
  slice_sample(n = 10)

# Calculate statistics
sample_mean <- mean(small_sample$length_mm)
sample_sd <- sd(small_sample$length_mm)
sample_n <- nrow(small_sample)
sample_se <- sample_sd / sqrt(sample_n)

# Calculate confidence intervals
z_ci_lower <- sample_mean - 1.96 * sample_se
z_ci_upper <- sample_mean + 1.96 * sample_se

# For t-distribution, get critical value for 95% CI with df = n-1
t_crit <- qt(0.975, df = sample_n - 1)
t_ci_lower <- sample_mean - t_crit * sample_se
t_ci_upper <- sample_mean + t_crit * sample_se

# Display results
cat("Mean:", round(sample_mean, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Mean: 272.2 mm
```


:::

```{.r .cell-code}
cat("Standard deviation:", round(sample_sd, 2), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Standard deviation: 38.09 mm
```


:::

```{.r .cell-code}
cat("Standard error:", round(sample_se, 2), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Standard error: 12.05 mm
```


:::

```{.r .cell-code}
cat("95% CI using z:", round(z_ci_lower, 1), "to", round(z_ci_upper, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
95% CI using z: 248.6 to 295.8 mm
```


:::

```{.r .cell-code}
cat("95% CI using t:", round(t_ci_lower, 1), "to", round(t_ci_upper, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
95% CI using t: 245 to 299.4 mm
```


:::

```{.r .cell-code}
cat("t critical value:", round(t_crit, 3), "vs z critical value: 1.96\n")
```

::: {.cell-output .cell-output-stdout}

```
t critical value: 2.262 vs z critical value: 1.96
```


:::
:::









:::

# Student's t-distribution Formula

::::: columns
::: {.column width="60%"}
To calculate CI for sample from "unknown" population:

## $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$

Where:

-   ȳ is sample mean
-   𝑛 is sample size
-   s is sample standard deviation
-   t t-value corresponding the probability of the CI
-   t in t-table for different degrees of freedom (n-1)
:::

::: {.column width="40%"}
![](images/clipboard-3203878802.png){width="300" height="250"}
:::
:::::

# **Lecture 5:** Student's t-distribution Table

::::: columns
::: {.column width="60%"}
Here is a t-table

-   Values of t that correspond to probabilities
-   Probabilities listed along top
-   Sample dfs are listed in the left-most column
-   Probabilities are given for one-tailed and two-tailed "questions"
:::

::: {.column width="40%"}
![](images/clipboard-3203878802.png){width="300" height="250"}
:::
:::::

# **Lecture 5:** One-tailed Questions

::::: columns
::: {.column width="60%"}
One-tailed questions: area of distribution left or (right) of a certain
value

-   n=20 (df=19) - 90% of the observations found left
-   t= 1.328 (10% are outside)
:::

::: {.column width="40%"}
![](images/clipboard-1822465473.png){width="180" height="120"}

![](images/clipboard-641796945.png){width="280" height="180"}
:::
:::::

# **Lecture 5:** Two-tailed Questions

::::: columns
::: {.column width="60%"}
Two-tailed questions refer to area between certain values

-   n= 20 (df=19), 90% of the observations are between
-   t=-1.729 and t=1.729 (10% are outside)
:::

::: {.column width="40%"}
![](images/clipboard-2234740159.png){width="200" height="150"}

![](images/clipboard-1734806681.png){width="350" height="200"}
:::
:::::

# **Lecture 5:** Calculating CI Example

::::: columns
::: {.column width="60%"}
Let's calculate CIs again:

Use two-sided test

-   $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$
-   95% CI Sample A: = 272.8 ± 2.262 \* (37.81/(9\^0.5)) = 1.650788
-   The 95% CI is between 244.3 and 301.3
-   "The 95% CI for the population mean from sample A is 272.8 ± 28.5"
:::

::: {.column width="40%"}
![](images/clipboard-3203878802.png){width="300" height="250"}
:::
:::::

# **Lecture 5:** Applications of t-distribution

So:

-   Can assess confidence that population mean is within a certain range
-   Can use t distribution to ask questions like:
    -   "What is probability of getting sample with mean = ȳ from
        population with mean = µ?" (1 sample t-test)
    -   "What is the probability that two samples came from same
        population?" (2 sample t-test)

# **Lecture 5:** Single Sample T-Test

We want to test if the mean fish length in I3 differs from 240mm.

**Activity: Define hypotheses and identify assumptions**

H₀: μ = 240 (The mean fish length in I3 is 240mm)

H₁: μ ≠ 240 (The mean fish length in I3 is not 240mm)

## Assumptions for t-test:

1.  Data is normally distributed
2.  Observations are independent
3.  No significant outliers

# Assumptions in R - qqplots from car










::: {.cell exercise='true'}

```{.r .cell-code}
# Filter for just windward side needles

# YOUR TASK: Test normality of windward pine needle lengths
# QQ Plot
qqPlot(i3_df$length_mm, 
       main = "QQ Plot for length of Grayling",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/test_assumptions-1.png)
:::

::: {.cell-output .cell-output-stdout}

```
[1] 53 35
```


:::
:::










# Statistical Test of Normality

## Shapiro-Wilk test










::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test
shapiro_test <- shapiro.test(i3_df$length_mm)
print(shapiro_test)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  i3_df$length_mm
W = 0.91051, p-value = 0.0001623
```


:::
:::










# Checking for Outliers










::: {.cell}

```{.r .cell-code}
# Check for outliers using boxplot
# YOUR CODE HERE
i3_df %>% ggplot(aes(lake, length_mm))+geom_boxplot()
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-6-1.png)
:::
:::










# Practice Exercise 1: One-Sample t-Test

::: callout-tip
## Practice Exercise 1: One-Sample t-Test

Let's perform a one-sample t-test to determine if the mean fish length
in I3 Lake differs from 240 mm:










::: {.cell}

```{.r .cell-code}
# what is the mean
i3_mean <- mean(i3_df$length_mm, na.rm=TRUE)
cat("Mean:", round(i3_mean, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Mean: 265.6 mm
```


:::

```{.r .cell-code}
# Perform a one-sample t-test
t_test_result <- t.test(i3_df$length_mm, mu = 240)

# View the test results
t_test_result
```

::: {.cell-output .cell-output-stdout}

```

	One Sample t-test

data:  i3_df$length_mm
t = 7.3497, df = 65, p-value = 4.17e-10
alternative hypothesis: true mean is not equal to 240
95 percent confidence interval:
 258.6481 272.5640
sample estimates:
mean of x 
 265.6061 
```


:::
:::










Interpret this test result by answering these questions:

1.  What was the null hypothesis?
2.  What was the alternative hypothesis?
3.  What does the p-value tell us?
4.  Should we reject or fail to reject the null hypothesis at α = 0.05?
5.  What is the practical interpretation of this result for fish
    biologists?
:::

# **Lecture 5:** Hypothesis Testing Framework

::::: columns
::: {.column width="60%"}
Hypothesis testing is a systematic way to evaluate research questions
using data.

**Key components:**

1.  **Null hypothesis (Ho)**: Typically assumes "no effect" or "no
    difference"
2.  **Alternative hypothesis (Ha)**: The claim we're trying to support
3.  **Statistical test**: Method for evaluating evidence against H₀
4.  **P-value**: Probability of observing our results (or more extreme)
    if H₀ is true
5.  **Significance level (α)**: Threshold for rejecting H₀, typically
    0.05

**Decision rule**: Reject Ho if p-value \< α == p \< 0.05
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-8-1.png)
:::
:::









:::
:::::

# **Lecture 5:** Hypothesis Testing - Original Scale

::::: columns
::: {.column width="60%"}
Hypothesis testing is a systematic way to evaluate research questions
using data.

**Key components:**

1.  **Null hypothesis (H₀)**: Typically assumes "no effect" or "no
    difference"

2.  **Alternative hypothesis (Hₐ)**: The claim we're trying to support

3.  **Statistical test**: Method for evaluating evidence against H₀

4.  **P-value**: Probability of observing our results (or more extreme)
    if H₀ is true

5.  **Significance level (α)**: Threshold for rejecting H₀, typically
    0.05

**Decision rule**: Reject H₀ if p-value \< α
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-9-1.png)
:::
:::









:::
:::::

# Lecture 5: Interpreting One-Sample T-Test Results

**Activity: Interpret the t-test results**

-   What does the p-value tell us?
-   Should we reject or fail to reject the null hypothesis?

**How to report this result in a scientific paper:**

"A two-tailed, one-sample t-test at α=0.05 showed that the mean pine
needle length on the windward side (... mm, SD = ...) \[was/was not\]
significantly different from the expected 55 mm, t(...) = ..., p = ..."

# **Lecture 5:** Two Sample T-Tests Introduction

::::: columns
::: {.column width="60%"}
For example

-   what is probability that population X is the same as population Y?

How would you assess this question using what we learned?

This is what we will do with the pine needles...
:::

::: {.column width="40%"}
![](images/pine_needles.jpg){width="300" height="200"}
:::
:::::

# **Lecture 5:** Comparing Two Samples

::::: columns
::: {.column width="60%"}
For example

-   what is probability that population X is the same as population Y?

How would you assess this question using what we learned?
:::

::: {.column width="40%"}









::: {.cell}

```{.r .cell-code}
# Now create a boxplot to visualize the difference in fish lengths between these lakes:
pine_df <- read_csv("data/pine_needles.csv")

# Create a boxplot comparing the two lakes
pine_wind_plot <- pine_df %>%
  ggplot(aes(x = wind, y = length_mm, fill = wind)) +
  geom_boxplot() +
  labs(title = "Pine Needle Lengths by Wind Exposure",
       x = "Position",
       y = "Length (mm)",
       fill = "Wind Position") +
  scale_fill_manual(values = c("lee" = "forestgreen", "wind" = "skyblue"),
                   labels = c("lee" = "Leeward", "wind" = "Windward"))
pine_wind_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-10-1.png)
:::

```{.r .cell-code}
# Based on the t-test results and the boxplot
# 
# what can you conclude about the fish populations in these two lakes?
```
:::









:::
:::::

# Practice Exercise 2: Formulating Hypotheses

::: callout-tip
## Practice Exercise 2: Formulating Hypotheses

For the following research questions about pine needles write the null
and alternative hypotheses:

1.  Are needles on the lee side longer than the needles on the windy
    side?

What are the hypotheses?

Ho =

Ha =
:::

# **Lecture 5:** Two-Sample T-Test Framework

Now, let's compare pine needle lengths between windward and leeward
sides of trees.

Question: **Is there a significant difference in needle length between
the windward and leeward sides?**

This requires a two-sample t-test.

Two-sample t-test compares means from two independent groups.

## $t = \frac{\bar{x}_1 - \bar{x}_2}{S_p\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}}$

where:

-   x̄₁ and x̄₂: These represent the sample means of the two groups you're
    comparing
-   s²ₚ: This is the pooled variance, calculated as: s²ₚ = \[(n₁ -
    1)s₁² + (n₂ - 1)s₂²\] / (n₁ + n₂ - 2), where s₁² and s₂² are the
    sample variances of the two groups.
-   **n₁ and n₂:** These are the sample sizes of the two groups.
-   **√(1/n₁ + 1/n₂):** This represents the pooled standard error.

## $t = \frac{SIGNAL}{NOISE}$

# Practice Exercise 3: Summary Statistics

::: callout-tip
## Practice Exercise 3: **Calculate summary statistics grouped by wind exposure**

Before conducting the test, we need to understand the data for each
group.

1.  You need this and the graph to see what is going on ....










    ::: {.cell}
    
    ```{.r .cell-code}
    group_summary <- pine_df %>%
      group_by(wind) %>%
      summarize(
        mean_length = mean(length_mm),
        sd_length = sd(length_mm),
        n = n(),
        se_length = sd_length / sqrt(n)
      )
    
    print(group_summary)
    ```
    
    ::: {.cell-output .cell-output-stdout}
    
    ```
    # A tibble: 2 × 5
      wind  mean_length sd_length     n se_length
      <chr>       <dbl>     <dbl> <int>     <dbl>
    1 lee          20.4      2.45    24     0.500
    2 wind         14.9      1.91    24     0.390
    ```
    
    
    :::
    :::









:::

# Visualizing Group Differences










::: {.cell}

```{.r .cell-code}
# Create a boxplot comparing the two sides
pine_wind_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-12-1.png)
:::
:::










# Practice Exercise 4: Effect Size

::: callout-tip
## Practice Exercise 4: Effect size

We could also look at the difference in means... some cool code here










::: {.cell}

```{.r .cell-code}
# Assuming your dataframe is called df
group_summary %>%
  summarize(difference = mean_length[wind == "wind"] - mean_length[wind == "lee"])
```

::: {.cell-output-display}

| difference|
|----------:|
|       -5.5|

:::
:::









:::

# Practice Exercise 5: ggplot Summary Statistics

::: callout-tip
## Practice Exercise 5: Using GGPLOT to get summary stats

GGplot also has code to make the mean and standard error plots we are
interested in along whit a lot of others










::: {.cell}

```{.r .cell-code}
# Assuming your dataframe is called df
pine_mean_se_plot <- ggplot(pine_df, aes(x = wind, y = length_mm, color = wind)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Pine Needle Length by Wind Exposure",
       x = "Wind Exposure",
       y = "Mean Length (mm)") +
  coord_cartesian(ylim = c(0,25))+
  scale_color_manual(values = c("lee" = "forestgreen", "wind" = "skyblue"),
                   labels = c("lee" = "Leeward", "wind" = "Windward"))+
  theme_classic()
pine_mean_se_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-14-1.png)
:::
:::









:::

# **Lecture 5:** Testing Assumptions for Two-Sample T-Test

For a two-sample t-test, we need to check:

1.  Normality within each group
2.  Equal variances between groups (for standard t-test)
3.  Independent observations

If assumptions are violated:

-   Welch's t-test (unequal variances)
-   Non-parametric alternatives (Mann-Whitney U test)

# Practice Exercise 6: Creating Group Data

::: callout-tip
## Practice Exercise 6: Test normality of windward pine needle lengths

qqplots

Note you need to test each groups separately...










::: {.cell}

```{.r .cell-code}
# Assuming your dataframe is called df
pine_mean_se_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-15-1.png)
:::
:::









:::

# Practice Exercise 7: Separate Group Data

::: callout-tip
## Practice Exercise 7: Test normality of windward pine needle lengths

qqplots

Note you need to test each groups separately...










::: {.cell}

```{.r .cell-code}
# how do you make separate dataframes to do this on?
# Separate data by groups
windward_data <- pine_df %>% filter(wind == "wind")
leeward_data <- pine_df %>% filter(wind == "lee")
head(leeward_data)
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

:::
:::









:::

# Practice Exercise 8: QQ Plot for Windward Data

::: callout-tip
## Practice Exercise 8: Test normality of windward pine needle lengths

qqplots

Note you need to test each groups separately...










::: {.cell}

```{.r .cell-code}
# QQ Plot for windward group
qqPlot(windward_data$length_mm, 
       main = "QQ Plot for Windward Pine Needles",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-17-1.png)
:::

::: {.cell-output .cell-output-stdout}

```
[1] 21 22
```


:::
:::









:::

# Practice Exercise 9: Shapiro-Wilk Test

::: callout-tip
## Practice Exercise 9: Test normality of windward pine needle lengths

Shapiro-Wilk test

Note you need to test each groups separately...










::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test for windward group
shapiro_windward <- shapiro.test(windward_data$length_mm)
print("Shapiro-Wilk test for windward data:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Shapiro-Wilk test for windward data:"
```


:::

```{.r .cell-code}
print(shapiro_windward)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  windward_data$length_mm
W = 0.96062, p-value = 0.451
```


:::
:::









:::

# Practice Exercise 10: QQ Plot for Leeward Data

::: callout-tip
## Practice Exercise 10: Test normality of leeward pine needle lengths

qqplots

Note you need to test each groups separately...










::: {.cell}

```{.r .cell-code}
# You can also test the leeward group
# QQ Plot for leeward group
qqPlot(leeward_data$length_mm, 
       main = "QQ Plot for Leeward Pine Needles",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-19-1.png)
:::

::: {.cell-output .cell-output-stdout}

```
[1]  4 16
```


:::
:::









:::

# Practice Exercise 11: Shapiro-Wilk for Leeward

::: callout-tip
## Practice Exercise 11: Test normality of leeward pine needle lengths

Shapiro-Wilk test

Note you need to test each groups separately...










::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test for leeward group
shapiro_lee <- shapiro.test(leeward_data$length_mm)
print("Shapiro-Wilk test for leeward data:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Shapiro-Wilk test for leeward data:"
```


:::

```{.r .cell-code}
print(shapiro_lee)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  leeward_data$length_mm
W = 0.95477, p-value = 0.3425
```


:::
:::









:::

# Practice Exercise 12: Combined Normality Test

::: callout-tip
## Practice Exercise 12: Test Normality at one time

There are always a lot of ways to do this in R










::: {.cell}

```{.r .cell-code}
# there are always two ways
# Test for normality using Shapiro-Wilk test for each wind group
# All in one pipeline using tidyverse approach
normality_results <- pine_df %>%
  group_by(wind) %>%
  summarize(
    shapiro_stat = shapiro.test(length_mm)$statistic,
    shapiro_p_value = shapiro.test(length_mm)$p.value,
    normal_distribution = if_else(shapiro_p_value > 0.05, "Normal", "Non-normal")
  )

# Print the results
print(normality_results)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
  wind  shapiro_stat shapiro_p_value normal_distribution
  <chr>        <dbl>           <dbl> <chr>              
1 lee          0.955           0.343 Normal             
2 wind         0.961           0.451 Normal             
```


:::
:::









:::

# Practice Exercise 13: Test Equal Variances

::: callout-tip
## Practice Exercise 13: Test equal variances

Levenes test can be done on the original dataframe










::: {.cell}

```{.r .cell-code}
# Method 1: Using car package's leveneTest
# This is often preferred as it's more robust to departures from normality
levene_result <- leveneTest(length_mm ~ wind, data = pine_df)
print("Levene's Test for Homogeneity of Variance:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Levene's Test for Homogeneity of Variance:"
```


:::

```{.r .cell-code}
print(levene_result)
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  1  1.2004 0.2789
      46               
```


:::
:::









:::

# **Lecture 5:** Conducting the Two-Sample T-Test

::::: columns
::: {.column width="60%"}
Now we can compare the mean pine needle lengths between windward and
leeward sides.

Ho: μ₁ = μ₂ (The mean needle lengths are equal)

Ha: μ₁ ≠ μ₂ (The mean needle lengths are different)

Deciding between:

-   Standard t-test (equal variances)

-   Welch's t-test (unequal variances)

**Note the Levenes Test should be NOT SIGNIFICANT - What is the null
hypothesis**
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}

|      | Df|  F value|    Pr(>F)|
|:-----|--:|--------:|---------:|
|group |  1| 1.200417| 0.2789416|
|      | 46|       NA|        NA|

:::
:::









:::
:::::

# **Lecture 5:** Running the Two-Sample T-Test

Now we can do a two sample TTEST

Calculate t-statistic manually (optional)

YOUR CODE HERE:

t = (mean1 - mean2) / sqrt((s1\^2/n1) + (s2\^2/n2))

::: callout-tip









::: {.cell}

```{.r .cell-code}
# YOUR TASK: Conduct a two-sample t-test
# Use var.equal=TRUE for standard t-test or var.equal=FALSE for Welch's t-test

# Standard t-test (if variances are equal)
t_test_result <- t.test(length_mm ~ wind, data = pine_df, var.equal = TRUE)
print("Standard two-sample t-test:")
```

::: {.cell-output .cell-output-stdout}

```
[1] "Standard two-sample t-test:"
```


:::

```{.r .cell-code}
print(t_test_result)
```

::: {.cell-output .cell-output-stdout}

```

	Two Sample t-test

data:  length_mm by wind
t = 8.6792, df = 46, p-value = 3.01e-11
alternative hypothesis: true difference in means between group lee and group wind is not equal to 0
95 percent confidence interval:
 4.224437 6.775563
sample estimates:
 mean in group lee mean in group wind 
          20.41667           14.91667 
```


:::

```{.r .cell-code}
# Welch's t-test (if variances are unequal)
# YOUR CODE HERE
```
:::









:::

# **Lecture 5:** Interpreting Two-Sample T-Test Results

::::: columns
::: {.column width="60%"}
**Interpret the results of the two-sample t-test**

What can we conclude about the needle lengths on windward vs. leeward
sides?

**How to report this result in a scientific paper:**

"A two-tailed, two-sample t-test at α=0.05 showed \[a significant/no
significant\] difference in needle length between windward (M = ..., SD
= ...) and leeward (M = ..., SD = ...) sides of pine trees, t(...) =
..., p = ...."
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-24-1.png)
:::
:::









:::
:::::

# **Lecture 5:** Visualizing the Results

::::: columns
::: {.column width="60%"}
**Interpret the results of the two-sample t-test**

What can we conclude about the needle lengths on windward vs. leeward
sides?

**How to report this result in a scientific paper:**

"A two-tailed, two-sample t-test at α=0.05 showed \[a significant/no
significant\] difference in needle length between windward (M = ..., SD
= ...) and leeward (M = ..., SD = ...) sides of pine trees, t(...) =
..., p = ...."
:::

::: {.column width="40%"}









::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-25-1.png)
:::
:::









:::
:::::

# **Lecture 5:** Assumptions of Parametric Tests

**Common assumptions for t-tests:**

1.  Normality: Data comes from normally distributed populations
2.  Equal variances (for two-sample tests)
3.  Independence: Observations are independent
4.  No outliers: Extreme values can influence results

What can we do if our data violates these assumptions?

Alternatives when assumptions are violated:

-   Data transformation (log, square root, etc.)
-   Non-parametric tests
-   Robust statistical methods

# **Lecture 5:** Summary and Conclusions

In this activity, we've:

1.  Formulated hypotheses about pine needle length
2.  Tested assumptions for parametric tests
3.  Conducted one-sample and two-sample t-tests
4.  Visualized data using appropriate methods
5.  Learned how to interpret and report t-test results

**Key takeaways:**

-   Always check assumptions before conducting tests
-   Visualize your data to understand patterns
-   Report results comprehensively
-   Consider alternatives when assumptions are violated
