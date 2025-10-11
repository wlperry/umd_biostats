---
title: "assignment_02"
author: "Bill Perry"
metadata-files:
  - ../../_templates/assignments.yml
format:
  html:
    output-file: "assignment_02.html"
  docx:
    output-file: "assignment_02.docx"
editor: visual
---

# Ecological Statistics Assignment 02

In this assignment you will analyze a dataframe of various limnological
parameters from 594 lakes in Iowa, Wisconsin and Michigan. This
dataframe is a trimmed version of the full dataframe used by Cheruvelil
et al. 2013 in a study of the interactions between landscape parameters
and limnological variables. The full dataframe is available online:
[Dryad Full
Dataframe](https://datadryad.org/dataset/doi:10.5061/dryad.75s9s)

## There are 16 columns in the dataframe:

1.  lake - Lake Name

2.  longitude_utm - longitude in utm format

3.  latitude_utm - latitude in utm format

4.  state - state abbreviation

5.  source - source of data

6.  state_lake_id - lake ID for the state

7.  epa_lake_id - lake Id for EPA

8.  date - data sample taken

9.  year - year

10. CHL_ugL - chlorophyll a concentration (μg/L)

11. TN_ugL - total nitrogen (TN) concentration (μg/L)

12. TP_ugL - total phosphorus (TP) concentration (μg/L)

13. Area_km2 - lake area (km2)

14. Z_mean_m - lake mean depth (m)

15. Waterhshed_km2 - watershed area (km2)

16. AgUrb.prop - categorical variable describing portion of watershed
    that is agricultural or urban (low; 0-49.99%, high; 50-100%).

::: callout-tip
**Note: When doing a multiple regression and the assumptions are not
met, often the best approach is to take the log of the response
variable - chl_ugl in this case - and see how the model improves. The
next approach is to do a log of each variable and see how the AIC values
improve or take the log of all values. For the sake of this class you
can do the log of all variables and see how it improves the AIC values
(in a separate script I found that the log of TN, TP, and area were the
optimal variables to transform. I strongly suggest doing a log of the
chlorophyll a and leaving the rest along for ease of interpretation. You
will also need to conver AgUbr.prop to a 0 and 1 variable**
:::

One of the limnological parameters lake ecologists care most about is
planktonic chlorophyll *a* concentration. Chlorophyll *a* concentration
in the water is an indicator of the biomass of phytoplankton, the tiny
algae that form an important part of the base of aquatic food webs. Some
algal growth is a good thing; too much algal growth can be associated
with water quality problems like cyanobacterial blooms (which can
produce toxins), oxygen depletion, fish kills, and reduced aesthetic
value of lakes (think green, scummy, gross lakes). Lakes with too much
algal growth are known as “eutrophic” lakes. An important question
limnologists and aquatic ecologists have been grappling with in the past
century is understanding the causes of eutrophication (excessive algal
growth).

## Your primary task is to answer the following questions:

Your task is to use the trimmed Cheruvelil et al. (2013) dataset to
examine patterns in chlorophyll a concentration in the different lakes
and to use multiple linear regression and model comparisons to come up
with the best model for predicting chlorophyll a concentration in a lake
based on limnological (TP, TN, lake area, lake mean depth) and watershed
(watershed area, land use) characteristics. Thus, chlorophyll
concentration is the dependent (response) variable and other parameters
are potential predictor (independent) variables.

## To complete the task you will submit two files

1.  one containing your write-up
2.  another containing fully functional and annotated R code that was
    used to generate summary statistics, figures and perform analyses.

Your write-up will include the following sections:

## Your write-up will include the following sections:

### Hypothesis statements

-   In this section you will state the hypotheses you are testing in
    this assignment in verbal form as well as in mathematical symbol
    notation.

### Statistical methods description

-   This section will include a detailed description of the statistical
    analyses you performed:

    -   assumptions of tests

    -   how these assumptions were checked

    -   name and version of the software used to perform analyses (R and
        the libraries used)

### Statistical results description

In this section you should present a description of the data, report the
main trends and results of statistical analyses (together with
information about results of assumption testing) in text form (use
standard format for reporting results of tests) and refer the reader to
any figures and tables summarizing the data. Each figure or table
presented **must** be explicitly mentioned in this section, with a short
description of the information contained in the table or figure.
Remember to include statements on what your results mean for the
hypotheses you are testing.

### Tables and figures

Here you will place all the figures and tables mentioned in the text.
Tables go first, followed by figures. Tables and figures should be
placed in the same order in which they are mentioned in the body of the
text. Each table or figure should be accompanied by an informative
caption that will allow the reader to understand what is shown in the
table/figure without reference to the text. Captions go below figures
and above tables.

### Appendix

This section will contain any additional figures (e.g., residual and qq
plots). All these figures should also be referred to in the text in the
appropriate order.

## Assignment Tasks:

-   In the methods section, justify any pre-analysis decisions to drop
    variables as redundant, add variables, combine variables, or include
    interactions.

-   A scatterplot matrix with all the variables that you initially
    decided to include in the model (before multicollinearity
    assessment). Add loess smoothers to the data in the lower left
    portion of the plot and correlation coefficients (and associated
    p-values) in the upper right. Justify the decision to use a
    particular correlation coefficient (Pearson, Spearman, Kendall).

-   A table with VIF values for all the variables that you intended to
    include in the original model.

-   If you decide to drop/modify/combine any variables based on results
    of multicollinearity assessment provide the correlation matrix and
    VIFs for the “final” set of variables that you will allow into the
    model.

-   Partial regression plots (from the `car` package) that show the
    relationship between a predictor variables and chlorophyll a while
    holding the other predictors constant. Make sure the figure is
    properly labelled (see ?avPlots for details on how to change the
    look of graphics).

-   If you decide that the partial regression plots indicate
    non-linearity, describe the transformations that were used to
    approximate linearity for the predictor variables and provide a
    second set of partial regression plots showing that the
    non-linearity issue was addressed.

-   Provide evidence that your response variable values, as specified in
    your first full model, meet (or don’t meet) the assumptions of
    multiple linear regression. If a transformation was necessary,
    explain what transformation was used and provide evidence it helped
    address assumption violations issues.

-   Once a full model has been specified you will need to go through a
    model simplification process, dropping the non-significant term with
    the highest p-value (lowest F-value) and rechecking the results of
    the output until all remaining predictor parameters are significant.
    Provide a table summarizing all the models that were tested,
    starting with the full model and ending with the final, best model.
    Include full model F-ratios, p-values adjusted r\[2\] and AIC values
    to compare the models.

-   Model Number

-   Model terms

-   F-ratio

-   p-values

-   Adjusted r2

-   AIC

-   Remember!!! The `summary(model)` command will give you information
    about the significance of different levels of a categorical
    variable. The `anova(model)` command will tell you if the
    categorical predictor variable is significant overall.

-   Provide a table with the dfs, F-ratios, p-values, and partial r2
    values for the terms remaining in your final, “best” model.

-   Use any other tables or figures that may help readers understand
    patterns in the data and the results of your analyses.

-   Remember to include a statement in your results section that
    verbally summarizes the overall findings of your analysis in a form
    that will be understandable without reference to tables or figures.

## **Grading Rubric:**

Total is 100 points

-   Hypotheses Statements - 10 points
-   Statistical Methods - 20 points
-   Results Statement - 30 points
-   Figure and Table Quality - 20 points
-   R Code - 16 points
-   Grammar/Writing - 4 points

## References:

Cheruvelil, K.S., P.A. Soranno, M.T. Bremigan, and K.E. Webster. 2013.
Multi-scaled drivers of ecosystem state: Quantifying the importance of
the regional spatial scale. Ecological Applications 23: 1603-1618.
