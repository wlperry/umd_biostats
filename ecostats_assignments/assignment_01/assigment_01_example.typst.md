---
title: "Assignment 01 example"
author: "Bill Perry"
metadata-files: 
  - ../../_templates/assignments.yml
format:
  html:
    output-file: "assignment_01_example.html"
  typst:
    output-file: "assignment_01_example.pdf"
---






# Ecological Statistics Assignment 01

The goal of this assignment is to analyze data from Ted Ozersky using
the approaches for comparing two samples using some sort of T-Test. Note
you may need to use a Two Sample T-Test with or without equal variances,
a paired t-Test on the original data or transformed data.

The set up for the analyses - load libraries - read in the data - make
transformations up front in this case - you might thing through this and
name things so you can copy the whole code chunk for each question ; )






::: {.cell}

```{.r .cell-code}
# load the libraries
library(broom)        # for cleaning statisical model outputs
library(car)          # For diagnostic tests
```

::: {.cell-output .cell-output-stderr}

```
Loading required package: carData
```


:::

```{.r .cell-code}
library(skimr)        # summary stats if you want
library(patchwork)    # combining graphs
library(tidyverse)    # needed for almost all of the code and plotting
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
:::

::: {.cell}

```{.r .cell-code}
# # read in data and do transforms
# # read in the long formatted data - the origina.
# l_df <- read_csv("data/chl_p_data_assignment_01.csv")
# 
# 
# # in some tests you need to transform the data to a wide format. 
# # I have provided a wide format data set so you dont need to do this.
# # The code below will transform the data into a wide formated dataset
# 
# # Long to Wide dataframe --- 
# season_df <- l_df %>%
#   pivot_wider(
#     names_from = season,
#     values_from = c(tp_ugl, phytobiomass_mgl),
#     names_sep = "_"
#   ) 
# 
# # read in the wide formated data 
# # season_df <- read_csv("data/chl_p_data_assignment_wide_01b.csv")
# 
# 
# # transform variables using a log base 10 transform
# l_df <- l_df %>% mutate(log_tp_ugl = log10(tp_ugl),
#                         log_phytobiomass = log10(phytobiomass_mgl))
# 
# # transform the data in the season dataframe
# season_df <- season_df %>% 
#   mutate(
#     log_tp_ugl_winter = log10(tp_ugl_winter),
#      log_tp_ugl_summer = log10(tp_ugl_summer),
#      log_phytobiomass_mgl_winter = log10(phytobiomass_mgl_winter),
#      log_phytobiomass_mgl_summer = log10(phytobiomass_mgl_summer),
#   )
```
:::






# Question 1: Hypothesis statements:

1.  Question 1: is there a difference in total phosphorus concentrations
    between winter and summer?

    1.  H~o~: µ𝚫~TPsummer-winter~ = 0

    2.  H~a~: µ𝚫~TPsummer-winter~ ≠ 0

    3.  The null hypothesis tested is that the population mean
        difference between summer and winter TP values is equal to 0.
        The alternative hypothesis is that the population mean
        difference between summer and winter TP values is not equal to
        zero.

## Data exploration of TP

## two ways to calcualte effect size






::: {.cell}

```{.r .cell-code}
# l_df %>%
#   summarize(
#     winter_mean = mean(phytobiomass_mgl[season == "winter"], na.rm = TRUE),
#     summer_mean = mean(phytobiomass_mgl[season == "summer"], na.rm = TRUE),
#     effect_size = winter_mean - summer_mean
#   )


# phyto_effect_size <- l_df %>%
#   summarize(
#     winter_mean = mean(phytobiomass_mgl[season == "winter"], na.rm = TRUE),
#     summer_mean = mean(phytobiomass_mgl[season == "summer"], na.rm = TRUE),
#     effect_size = winter_mean - summer_mean
#   ) %>%
#   pull(effect_size)
# 
# effect_size
```
:::






# the way to make a new variable






::: {.cell}

```{.r .cell-code}
# l_df <- l_df %>% 
#   mutate(tp_level_2 = if_else(tp_ugl >= 2, "high", "low"))
```
:::
