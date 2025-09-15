---
title: "assignment_03"
author: "Bill Perry"
metadata-files:
  - ../../_templates/assignments.yml
format:
  html:
    output-file: "assignment_03.html"
  typst:
    output-file: "assignment_03.pdf"
editor: visual
---

# Ecological Statistics Assignment 03

The goal of this assignment is to test whether crayfish chelae (claw)
length differs between upstream and downstream locations in six
different rivers where waterfalls separate them. We hypothesize that
downstream crayfish will have larger chelae due to increased food
availability and different selective pressures.

**Yo goal is to use an ANOVA of some sort to analyze this data.**

## There are 16 columns in the dataframe:

1.  crayfish_id - crayfish ID sampled

2.  river - the river it was collected form

3.  position - upstream downstream

4.  chelae_length - length of chelae

## Your primary task is to answer the following questions:

Your task is to use the dataframe to examine patterns in crayfish chelae
lenght in the different rivers upstream and downstream using some form
of an ANOVA.

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

-   State the question and hypotheses using words and math

-   State the type of test you are doing and why

-   State the assumptions of the test

-   Show the model statement you chose and what it means

-   show the assumption tests in the appendix

-   Do the post f tests of the model

-   present the methods and results

-   with final figures for publication

## **Grading Rubric:**

Total is 100 points

-   Hypotheses Statements - 10 points
-   Statistical Methods - 20 points
-   Results Statement - 30 points
-   Figure and Table Quality - 20 points
-   R Code - 16 points
-   Grammar/Writing - 4 points
