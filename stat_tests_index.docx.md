---
title: "Common Statistical Tests"
author: "Bill Perry"
execute:
  freeze: auto
  cache: true
  echo: true
  keep-md: true # retains the images when you start again
  message: false
  warning: false
  fig-height: 3
  fig-width: 3
  paged-print: false
format:
  html:
    toc: false
    output-file: "Common_Statistical_Tests.html"
    embed-resources: true
    self-contained: true
    css: css/activity.css
  docx:
    default: true
    toc: false
    toc-depth: 3
    number-sections: false
    highlight-style: github
    reference-doc: ms_templates/custom-reference.docx
    css: css/msword.css
    embed-resources: true
---







# This is a list of the common statistical tests we have used in class

I hope this serves as a resource you can use in the future when you are doing data analysis.

I have provided the common tests we have done with a lot of detail that will help you remember how to approach these questions and how to run the tests.

My hope is to organize them into:

-   types of data used for the test

-   assumptions of the test

-   hypotheses tested

-   example data

-   mechanics of the test

-   explicit tests of the assumptions

-   follow up tests if appropriate

-   example graphs and associated code

-   how to write up the references section

| Statistical Test | webpage | code |
|--------------------------------------|------------|------------|
| **Two Sample Test** |  | r_code_only |
| Parametric Two Sample TTest | [web](test_overviews/01_two_sample_ttest.qmd) | code |
| NonParametric Welches Two Sample Test | [web](test_overviews/02_two_sample_welches_ttest.qmd) | code |
| NonParametric Permutation Two Sample Test | [web](test_overviews/03_two_sample_permutation_test.qmd) | code |
| NonParametric Mann Whitney U Two Sample Test | [web](test_overviews/04_two_sample_mann_whitney_u_test.qmd) | code |
