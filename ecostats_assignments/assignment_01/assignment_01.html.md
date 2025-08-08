---
title: "assignment_01"
author: "Bill Perry"
metadata-files: 
  - ../../_templates/assignments.yml
format:
  html:
    output-file: "assignment_01.html"
  typst:
    output-file: "assignment_01.pdf"
---





# Ecological Statistics Assignment 01

In this assignment you will be doing 2-sample tests on a dataframe describing phosphorus concentrations and phytoplankton biomass from 26 lakes. The data (chl_p_data_assignment_01.csv and chl_p_data_assignment_wide_01b.csv) comes in part from a data synthesis project (Ecology Under Lake Ice) and in part from sampling conducted by Ted Ozersky et al.  Two samples have been collected in each lake in the dataframe: one in winter and one in summer.

## There are 5 columns in the dataframe:

-   year: year in which samples were collected.
-   season: samples were collected either in winter (ice was on the lake) or during summer (ice was off the lake).
-   lakename: name of the lake where samples were collected.
-   lakecountry: country where lake is located.
-   avg_tp_ugl: average epilimnetic total P concentration in µg/L. Phosphorus is the productivity limiting nutrient in most lakes.
-   avg_phytobiomass_mgl: average epilimentic phytoplankton biomass as mg/L. Phytoplankton form the base of the pelagic food web in many lakes.

## Your primary task is to answer the following questions:

1.  Is there a significant difference in total phosphorus) (avg_tp_ugl) concentrations between winter and summer?
2.  Is there a significant difference in phytoplankton biomass (avg_phytobiomass_mgl) between winter and summer?
3.  Is there a significant difference in **winter** phytoplankton biomass between low nutrient (TP ≤2 µg/L) and high nutrient lakes ( TP \> 2 µg/L)?
4.  Is there a significant difference in **summer** phytoplankton biomass between low nutrient (TP ≤2 µg/L) and high nutrient lakes ( TP \> 2 µg/L)?

## To complete the task you will submit two files

-   one containing your write-up as a word file

-   one containing fully functional **and annotated R code** that was used to generate summary statistics, figures and perform analyses

## Your write-up will include the following sections:

### Hypothesis statements

-   In this section you will state the hypotheses you are testing in this assignment in verbal form **as well as** in mathematical symbol notation for each of the 4 tests.

    -   In this study, we tested four hypotheses about the winter and summer phosphorus concetrations and phytoplankton abundance in seasonally frozen lakes. First (H1), we tested the null hypothesis that average chl. a concentrations did not differ for the same set of lakes between winter and summer (Ho: µ sumchla= µ sumchla; Ha: µ sumchla≠ µ sumchla))

### Statistical methods description

-   This section will include a detailed description of the statistical analyses you performed:

    -   assumptions of tests

    -   how these assumptions were checked

    -   name and version of the software used to perform analyses (R and the libraries used)

### Statistical results description

In this section you should present a description of the data, report the main trends and results of statistical analyses (together with information about results of assumption testing) in text form (use standard format for reporting results of tests) and refer the reader to any figures and tables summarizing the data. Each figure or table presented **must** be explicitly mentioned in this section, with a short description of the information contained in the table or figure. Remember to include statements on what your results mean for the hypotheses you are testing. Keep track of significant digits.

### Tables and figures

Here you will place all the figures and tables mentioned in the text.

-   Tables go first and followed by figures

-   Tables and figures should be placed in the same order in which they are mentioned in the body of the text and numbered accordingly

-   Tables should only have horizontal lines and a minimum number of them

    -   Generally, line should only appear above the first row of a table, below the first row of a table and at the bottom of the last row of a table

-   Each table or figure should be accompanied by an informative caption that will allow the reader to understand what is shown in the table/figure without reference to the text

-   Captions go above tables and below figures

-   Include a table with the mean, median, range, standard deviation and coefficient of variation for winter and summer TP concentrations and phytoplankton biomass generated with R code and displayed in the output

-   Include figures that show that assumptions of tests were met

-   Figures summarizing the results for each of the research questions/ hypotheses must be properly constructed

    -   use clear and concise graphics
    -   make sure all axes are labelled with the measurement and units (if applicable)
    -   do not use color or patterns to indicate different groups - only white, black and shades of grey
    -   do not show grid lines
    -   scale the axes to make data stand out
    -   different groups should be easily distinguishable with shades of grey or symbols
    -   Figures (and tables) should be accompanied by an informative caption that provides all the information a reader needs to interpret the figure without having to refer to the text

## **Grading Rubric:**

Total is 100 points

-   Hypotheses Statements - 16 points
-   Statistical Methods - 16 points
-   Results Statement - 20 points
-   Figure and Table Quality - 24 points
-   R Code - 20 points
-   Grammar/Writing - 4 points