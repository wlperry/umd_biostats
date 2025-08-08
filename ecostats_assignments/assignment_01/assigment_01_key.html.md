---
title: "Assignment 01 key"
author: "Bill Perry"
metadata-files: 
  - ../../_templates/assignments.yml
format:
  html:
    output-file: "assignment_01_key_html.html"
  typst:
    output-file: "assignment_01_key_pdf.pdf"
---





# Ecological Statistics Assignment 01

The goal of this assignment is to analyze data from Ted Ozersky using
the approaches for comparing two samples using some sort of T-Test. Note
you may need to use a Two Sample T-Test with or without equal variances,
a paired t-Test on the original data or transformed data.

The set up for the analyses - load libraries - read in the data - make
transformations up front in this case




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
# read in data and do transforms
# read in the long formatted data - the origina.
l_df <- read_csv("data/chl_p_data_assignment_01.csv")
```

::: {.cell-output .cell-output-stderr}

```
Rows: 52 Columns: 6
── Column specification ────────────────────────────────────────────────────────
Delimiter: ","
chr (3): season, lakename, lakecountry
dbl (3): year, tp_ugl, phytobiomass_mgl

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


:::

```{.r .cell-code}
# in some tests you need to transform the data to a wide format. 
# I have provided a wide format data set so you dont need to do this.
# The code below will transform the data into a wide formated dataset

# Long to Wide dataframe --- 
season_df <- l_df %>%
  pivot_wider(
    names_from = season,
    values_from = c(tp_ugl, phytobiomass_mgl),
    names_sep = "_"
  ) 

# read in the wide formated data 
# season_df <- read_csv("data/chl_p_data_assignment_wide_01b.csv")


# transform variables using a log base 10 transform
l_df <- l_df %>% mutate(log_tp_ugl = log10(tp_ugl),
                        log_phytobiomass = log10(phytobiomass_mgl))

# transform the data in the season dataframe
season_df <- season_df %>% 
  mutate(
    log_tp_ugl_winter = log10(tp_ugl_winter),
     log_tp_ugl_summer = log10(tp_ugl_summer),
     log_phytobiomass_mgl_winter = log10(phytobiomass_mgl_winter),
     log_phytobiomass_mgl_summer = log10(phytobiomass_mgl_summer),
  )
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




::: {.cell}

```{.r .cell-code}
# boxplot of data
l_df %>% 
  ggplot(aes(season, tp_ugl)) + geom_boxplot()
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::




### note that the data is heavily skewed and hard to even see with boxplot so maybe a histogram would work better




::: {.cell}

```{.r .cell-code}
# histogram of data
l_df %>% 
  ggplot(aes(tp_ugl)) + geom_histogram(binwidth = 5) + coord_cartesian(xlim = c(0,30)) +facet_grid(.~season)
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::
:::




## What are the summary statistics of the data?




::: {.cell}

```{.r .cell-code}
q1_summary_stats <- l_df %>%
  group_by(season) %>% 
  summarize(mean_total_p = mean(tp_ugl, na.rm=TRUE),
            stddev_total_p = sd(tp_ugl, na.rm=TRUE),
            stderr_total_p = sd(tp_ugl, na.rm=TRUE)/sum(!is.na(tp_ugl)),
            coef_var = (sd(tp_ugl, na.rm=TRUE)/mean(tp_ugl, na.rm=TRUE))*100,
            )
q1_summary_stats
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  season mean_total_p stddev_total_p stderr_total_p coef_var
  <chr>         <dbl>          <dbl>          <dbl>    <dbl>
1 summer         3.62           4.39          0.169     121.
2 winter        16.3           47.7           1.84      293.
```


:::
:::




## How to calculate the effect size or differnece in means

Note that you can subset a column of data with
`variable[name_ _of_column_to_filter == "term_of_interst]`




::: {.cell}

```{.r .cell-code}
q1_effect_size <- l_df %>%
  summarize(
    winter_mean = mean(tp_ugl[season == "winter"], na.rm = TRUE),
    summer_mean = mean(tp_ugl[season == "summer"], na.rm = TRUE),
    effect_size = winter_mean - summer_mean
  )
q1_effect_size
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 3
  winter_mean summer_mean effect_size
        <dbl>       <dbl>       <dbl>
1        16.3        3.62        12.7
```


:::

```{.r .cell-code}
# another way to do the caluclation
# tp_effect_size <- l_df %>%
#   group_by(season) %>%
#   summarize(mean_tp = mean(tp_ugl, na.rm = TRUE), .groups = "drop") %>%
#   summarize(effect_size = diff(mean_tp)) %>%
#   pull(effect_size)
# effect_size
```
:::




## looking at the assumptions for a T-Test

For a two-sample t-test, we need to check:

1.  Normality within each group
2.  Equal variances between groups (for standard t-test)
3.  Independent observations

If assumptions are violated:

-   Welch's t-test (unequal variances)
-   Non-parametric alternatives (Mann-Whitney U test)

### Winter qqplots




::: {.cell}

```{.r .cell-code}
# QQ Plot for winter TP
qqPlot(season_df$tp_ugl_winter, 
       main = "QQ Plot for Winter TP",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-7-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 18 11
```


:::
:::




### Summer qqplots




::: {.cell}

```{.r .cell-code}
# QQ Plot for summer TP
qqPlot(season_df$tp_ugl_summer, 
       main = "QQ Plot for Summer TP",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-8-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 18 23
```


:::
:::




## Log Transformed data

### Winter log10 TP qqplots




::: {.cell}

```{.r .cell-code}
# Note you can enter the log transformed data or you can do math in some of this as well....
qqPlot(log10(season_df$tp_ugl_winter), 
       main = "QQ Plot for Winter LOG10 TP",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-9-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 18 11
```


:::
:::




### Summer log10 TP qqplots




::: {.cell}

```{.r .cell-code}
# QQ Plot for summer TP
qqPlot(log10(season_df$tp_ugl_summer), 
       main = "QQ Plot for Summer LOG10 TP",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-10-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 29 18
```


:::
:::




## Here is the shortcut to be able to do this all at one time

### GGPlot code for qqplots




::: {.cell}

```{.r .cell-code}
# Both seasons in one plot
l_df %>%
  ggplot(aes(sample = tp_ugl)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ season) +
  labs(title = "QQ Plots for TP by Season",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-11-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
# Both log value seasons in one plot
l_df %>%
  ggplot(aes(sample = log_tp_ugl)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ season) +
  labs(title = "QQ Plots for Log10 TP by Season",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::




## Shapiro Wilk Normality test




::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test
shapiro.test(log10(season_df$tp_ugl_winter))
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  log10(season_df$tp_ugl_winter)
W = 0.96494, p-value = 0.498
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test
shapiro.test(log10(season_df$tp_ugl_summer))
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  log10(season_df$tp_ugl_summer)
W = 0.98163, p-value = 0.9065
```


:::
:::




# How to do Shapiro Wilk Normality test with the Broom package




::: {.cell}

```{.r .cell-code}
# using the broom package
normality_results <- l_df %>%
  group_by(season) %>%
  group_modify(~ tidy(shapiro.test(log10(.x$tp_ugl))))

# Print the results
normality_results
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
# Groups:   season [2]
  season statistic p.value method                     
  <chr>      <dbl>   <dbl> <chr>                      
1 summer     0.982   0.906 Shapiro-Wilk normality test
2 winter     0.965   0.498 Shapiro-Wilk normality test
```


:::
:::




# How to do Shapiro Wilk Normality test with the purrr package




::: {.cell}

```{.r .cell-code}
library(purrr)

normality_results_purr <- l_df %>%
  group_split(season) %>%
  map_dfr(~ {
    test_result <- shapiro.test(log10(.x$tp_ugl))
    data.frame(
      season = unique(.x$season),
      statistic = test_result$statistic,
      p_value = test_result$p.value
    )
  })

normality_results_purr
```

::: {.cell-output .cell-output-stdout}

```
      season statistic   p_value
W...1 summer 0.9816291 0.9064592
W...2 winter 0.9649410 0.4979540
```


:::
:::




## Test for equal variance of the two samples




::: {.cell}

```{.r .cell-code}
# Method 1: Using car package's leveneTest
# This is often preferred as it's more robust to departures from normality
levene_result <- leveneTest(log_tp_ugl ~ season, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in leveneTest.default(y = y, group = group, ...): group coerced to
factor.
```


:::

```{.r .cell-code}
levene_result
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  1  0.3686 0.5465
      50               
```


:::
:::




## T-Test

\*Note: they have equal variance But I usually do unqueal variance just
in case...




::: {.cell}

```{.r .cell-code}
# Standard t-test (if variances are NOT equal)
t_test_result <- t.test(log_tp_ugl ~ season, data = l_df, var.equal = FALSE)
t_test_result
```

::: {.cell-output .cell-output-stdout}

```

	Welch Two Sample t-test

data:  log_tp_ugl by season
t = -1.9182, df = 48.256, p-value = 0.06101
alternative hypothesis: true difference in means between group summer and group winter is not equal to 0
95 percent confidence interval:
 -0.65947108  0.01546496
sample estimates:
mean in group summer mean in group winter 
           0.2662312            0.5882343 
```


:::
:::




## Paired T-Test

-   Note for this you need to have the data in wide format Note that
    with this it is significant where it is not significant when not
    paired




::: {.cell}

```{.r .cell-code}
# Paired T-Test
t_test_paired_result <- t.test(season_df$log_tp_ugl_winter, season_df$log_tp_ugl_summer, paired = TRUE)
t_test_paired_result
```

::: {.cell-output .cell-output-stdout}

```

	Paired t-test

data:  season_df$log_tp_ugl_winter and season_df$log_tp_ugl_summer
t = 3.1125, df = 22, p-value = 0.005075
alternative hypothesis: true mean difference is not equal to 0
95 percent confidence interval:
 0.1081736 0.5401651
sample estimates:
mean difference 
      0.3241693 
```


:::
:::




## Non-parametric T-Test




::: {.cell}

```{.r .cell-code}
# Nonparametric TTest
wilcox_result <- wilcox.test(log_tp_ugl ~ season, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
compute exact p-value with ties
```


:::

```{.r .cell-code}
wilcox_result
```

::: {.cell-output .cell-output-stdout}

```

	Wilcoxon rank sum test with continuity correction

data:  log_tp_ugl by season
W = 247.5, p-value = 0.09953
alternative hypothesis: true location shift is not equal to 0
```


:::
:::




# QUESTION 2:

Data exploration of Phytoplankton Biomass \## Box plot exploration




::: {.cell}

```{.r .cell-code}
l_df %>% 
  ggplot(aes(season, phytobiomass_mgl)) + geom_boxplot()
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-21-1.png){width=672}
:::
:::




## Histrogram plot




::: {.cell}

```{.r .cell-code}
l_df %>% 
  ggplot(aes(phytobiomass_mgl)) + geom_histogram() + facet_grid(.~season)
```

::: {.cell-output .cell-output-stderr}

```
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```


:::

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-22-1.png){width=672}
:::
:::




-   Note that this also needs to be log transformed

## What are the phytobiomass summary statistics?




::: {.cell}

```{.r .cell-code}
q2_summary_stats <- l_df %>%
  group_by(season) %>% 
  summarize(mean_total_p = mean(phytobiomass_mgl, na.rm=TRUE),
            stddev_total_p = sd(phytobiomass_mgl, na.rm=TRUE),
            stderr_total_p = sd(phytobiomass_mgl, na.rm=TRUE)/sum(!is.na(tp_ugl)),
            coef_var = (sd(phytobiomass_mgl, na.rm=TRUE)/mean(phytobiomass_mgl, na.rm=TRUE))*100,
            )
q2_summary_stats
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  season mean_total_p stddev_total_p stderr_total_p coef_var
  <chr>         <dbl>          <dbl>          <dbl>    <dbl>
1 summer         52.5           128.           4.93     244.
2 winter        160.            215.           8.28     134.
```


:::
:::




## Phytobiomass eEffect size




::: {.cell}

```{.r .cell-code}
l_df %>%
  summarize(
    winter_mean = mean(phytobiomass_mgl[season == "winter"], na.rm = TRUE),
    summer_mean = mean(phytobiomass_mgl[season == "summer"], na.rm = TRUE),
    effect_size = winter_mean - summer_mean
  )
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 3
  winter_mean summer_mean effect_size
        <dbl>       <dbl>       <dbl>
1        160.        52.5        108.
```


:::

```{.r .cell-code}
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




## Phytobiomass qqplots

### Winter phytobiomass




::: {.cell}

```{.r .cell-code}
# QQ Plot for winter TP
qqPlot(season_df$phytobiomass_mgl_winter, 
       main = "QQ Plot for Winter Phytobiomass",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-25-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1]  8 10
```


:::
:::

::: {.cell}

```{.r .cell-code}
# QQ Plot for summer TP
qqPlot(season_df$phytobiomass_mgl_summer, 
       main = "QQ Plot for Summer Phytobioamss",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-26-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 18 26
```


:::
:::




## log phytobioamss transform

### log Phytobiomass winter




::: {.cell}

```{.r .cell-code}
# QQ Plot for winter TP
qqPlot(season_df$log_phytobiomass_mgl_winter,
       main = "QQ Plot for Winter Biomass",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-27-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 16  7
```


:::
:::




### log Phytobiomass summer




::: {.cell}

```{.r .cell-code}
# QQ Plot for summer TP
qqPlot(season_df$log_phytobiomass_mgl_summer,
       main = "QQ Plot for Summer Biomass",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-28-1.png){width=672}
:::

::: {.cell-output .cell-output-stdout}

```
[1] 28  7
```


:::
:::




### log phytobiomass both




::: {.cell}

```{.r .cell-code}
# Both seasons in one plot
l_df %>%
  ggplot(aes(sample = log_phytobiomass)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ season) +
  labs(title = "QQ Plots for Log10 Phytobiomass by Season",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-29-1.png){width=672}
:::
:::




## Phytobiomass Shapiro Wilk Normality test

### Winter Log Phytobiomass




::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test
shapiro.test(season_df$log_phytobiomass_mgl_winter)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  season_df$log_phytobiomass_mgl_winter
W = 0.9223, p-value = 0.05093
```


:::
:::




### Summer Log Phytobiomass




::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test
shapiro.test(season_df$log_phytobiomass_mgl_summer)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  season_df$log_phytobiomass_mgl_summer
W = 0.94843, p-value = 0.213
```


:::
:::




### Log PhytiobiomassShapiro Wild both broom




::: {.cell}

```{.r .cell-code}
# using the broom package
normality_log_phyto_results <- l_df %>%
  group_by(season) %>%
  group_modify(~ tidy(shapiro.test(.x$log_phytobiomass)))

# Print the results
normality_log_phyto_results
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
# Groups:   season [2]
  season statistic p.value method                     
  <chr>      <dbl>   <dbl> <chr>                      
1 summer     0.948  0.213  Shapiro-Wilk normality test
2 winter     0.922  0.0509 Shapiro-Wilk normality test
```


:::
:::




### Log PhytiobiomassShapiro Wild both purrr




::: {.cell}

```{.r .cell-code}
library(purrr)

normality_logphyto_results_purr <- l_df %>%
  group_split(season) %>%
  map_dfr(~ {
    test_result <- shapiro.test(.x$log_phytobiomass)
    data.frame(
      season = unique(.x$season),
      statistic = test_result$statistic,
      p_value = test_result$p.value
    )
  })

normality_logphyto_results_purr
```

::: {.cell-output .cell-output-stdout}

```
      season statistic    p_value
W...1 summer 0.9484306 0.21303161
W...2 winter 0.9223032 0.05092741
```


:::
:::




## Log Phytobiomass Levene Test




::: {.cell}

```{.r .cell-code}
# Method 1: Using car package's leveneTest
# This is often preferred as it's more robust to departures from normality
levene_logphyto_result <- leveneTest(log_phytobiomass ~ season, data = l_df)

levene_logphyto_result
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  1   2e-04 0.9882
      50               
```


:::
:::




## T-Test Phytiombiomass




::: {.cell}

```{.r .cell-code}
# Standard t-test (if variances are NOT equal)
t_test_logphyto_result <- t.test(log_phytobiomass ~ season, data = l_df, var.equal = FALSE)
t_test_logphyto_result
```

::: {.cell-output .cell-output-stdout}

```

	Welch Two Sample t-test

data:  log_phytobiomass by season
t = -2.267, df = 49.977, p-value = 0.02775
alternative hypothesis: true difference in means between group summer and group winter is not equal to 0
95 percent confidence interval:
 -1.14725704 -0.06934949
sample estimates:
mean in group summer mean in group winter 
            1.018270             1.626573 
```


:::
:::




## Paired T-Test Phytiobiomass




::: {.cell}

```{.r .cell-code}
# Standard t-test (if variances are NOT equal)
t_test_paired_log_phytoresult <- t.test(season_df$log_phytobiomass_mgl_winter, season_df$log_phytobiomass_mgl_summer,paired = TRUE)
t_test_paired_log_phytoresult
```

::: {.cell-output .cell-output-stdout}

```

	Paired t-test

data:  season_df$log_phytobiomass_mgl_winter and season_df$log_phytobiomass_mgl_summer
t = 6.1529, df = 22, p-value = 3.417e-06
alternative hypothesis: true mean difference is not equal to 0
95 percent confidence interval:
 0.3905015 0.7875842
sample estimates:
mean difference 
      0.5890429 
```


:::
:::




## Non Parametric T-Test Phytiobiomass




::: {.cell}

```{.r .cell-code}
wilcox_logphyto_result <- wilcox.test(log_phytobiomass ~ season, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
compute exact p-value with ties
```


:::

```{.r .cell-code}
wilcox_logphyto_result
```

::: {.cell-output .cell-output-stdout}

```

	Wilcoxon rank sum test with continuity correction

data:  log_phytobiomass by season
W = 208.5, p-value = 0.01823
alternative hypothesis: true location shift is not equal to 0
```


:::
:::




# Question 3

Is there a significant difference in winter phytoplankton biomass
between low nutrient (TP ≤2 µg/L) and high nutrient lakes ( TP \> 2
µg/L)?

## Adding a column of high and low TP below above 2 mg/L TP




::: {.cell}

```{.r .cell-code}
l_df <- l_df %>% 
  mutate(tp_level = if_else(tp_ugl >= 2, "high", "low"))
```
:::




## Summary Stats

## Data exploration of TP




::: {.cell}

```{.r .cell-code}
# boxplot of data
l_df %>% 
  ggplot(aes(tp_level, phytobiomass_mgl)) + geom_boxplot()
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-39-1.png){width=672}
:::
:::




### note that the data is heavily skewed and hard to even see with boxplot so maybe a histogram would work better




::: {.cell}

```{.r .cell-code}
# histogram of data
l_df %>% 
  ggplot(aes(phytobiomass_mgl)) + geom_histogram(binwidth = 5) + coord_cartesian(xlim = c(0,30)) +facet_grid(.~tp_level)
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-40-1.png){width=672}
:::
:::




## What are the summary statistics of the data?




::: {.cell}

```{.r .cell-code}
q3_summary_stats <- l_df %>%
  group_by(tp_level) %>% 
  summarize(mean_total_p = mean(phytobiomass_mgl, na.rm=TRUE),
            stddev_total_p = sd(phytobiomass_mgl, na.rm=TRUE),
            stderr_total_p = sd(phytobiomass_mgl, na.rm=TRUE)/sum(!is.na(phytobiomass_mgl)),
            coef_var = (sd(phytobiomass_mgl, na.rm=TRUE)/mean(phytobiomass_mgl, na.rm=TRUE))*100,
            )
q3_summary_stats
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  tp_level mean_total_p stddev_total_p stderr_total_p coef_var
  <chr>           <dbl>          <dbl>          <dbl>    <dbl>
1 high            148.           218.            7.04     148.
2 low              45.6           90.0           4.29     197.
```


:::
:::




## How to calculate the effect size or differnece in means

Note that you can subset a column of data with
`variable[name_ _of_column_to_filter == "term_of_interst]`




::: {.cell}

```{.r .cell-code}
q3_effect_size <- l_df %>%
  summarize(
    winter_mean = mean(phytobiomass_mgl[tp_level == "high"], na.rm = TRUE),
    summer_mean = mean(phytobiomass_mgl[tp_level == "low"], na.rm = TRUE),
    effect_size = winter_mean - summer_mean
  )
q3_effect_size
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 3
  winter_mean summer_mean effect_size
        <dbl>       <dbl>       <dbl>
1        148.        45.6        102.
```


:::

```{.r .cell-code}
# another way to do the caluclation
# tp_effect_size <- l_df %>%
#   group_by(season) %>%
#   summarize(mean_tp = mean(tp_ugl, na.rm = TRUE), .groups = "drop") %>%
#   summarize(effect_size = diff(mean_tp)) %>%
#   pull(effect_size)
# effect_size
```
:::




## looking at the assumptions for a T-Test

For a two-sample t-test, we need to check:

1.  Normality within each group
2.  Equal variances between groups (for standard t-test)
3.  Independent observations

If assumptions are violated:

-   Welch's t-test (unequal variances)
-   Non-parametric alternatives (Mann-Whitney U test)

## Here is the shortcut to be able to do this all at one time

### GGPlot code for qqplots




::: {.cell}

```{.r .cell-code}
# Both seasons in one plot
l_df %>%
  ggplot(aes(sample = phytobiomass_mgl)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ tp_level) +
  labs(title = "QQ Plots for Phytobiomass by TP Level",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-43-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
# Both log value seasons in one plot
l_df %>%
  ggplot(aes(sample = log_phytobiomass)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ tp_level) +
  labs(title = "QQ Plots for Log10 TP by TP_Level",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-44-1.png){width=672}
:::
:::




## Shapiro Wilk Normality test

# How to do Shapiro Wilk Normality test with the Broom package




::: {.cell}

```{.r .cell-code}
# using the broom package
normality_results <- l_df %>%
  group_by(tp_level) %>%
  group_modify(~ tidy(shapiro.test(.x$phytobiomass_mgl)))

# Print the results
normality_results
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
# Groups:   tp_level [2]
  tp_level statistic     p.value method                     
  <chr>        <dbl>       <dbl> <chr>                      
1 high         0.681 0.000000607 Shapiro-Wilk normality test
2 low          0.537 0.000000450 Shapiro-Wilk normality test
```


:::
:::




# How to do Shapiro Wilk Normality test with the purrr package




::: {.cell}

```{.r .cell-code}
library(purrr)

normality_results_purr <- l_df %>%
  group_split(tp_level) %>%
  map_dfr(~ {
    test_result <- shapiro.test(log10(.x$phytobiomass_mgl))
    data.frame(
      season = unique(.x$tp_level),
      statistic = test_result$statistic,
      p_value = test_result$p.value
    )
  })

normality_results_purr
```

::: {.cell-output .cell-output-stdout}

```
      season statistic      p_value
W...1   high 0.8511777 0.0005368223
W...2    low 0.9558741 0.4372152634
```


:::
:::




## Test for equal variance of the two samples




::: {.cell}

```{.r .cell-code}
# Method 1: Using car package's leveneTest
# This is often preferred as it's more robust to departures from normality
q3_levene_result <- leveneTest(log_phytobiomass ~ tp_level, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in leveneTest.default(y = y, group = group, ...): group coerced to
factor.
```


:::

```{.r .cell-code}
q3_levene_result
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  1  0.2597 0.6126
      50               
```


:::
:::




## T-Test

\*Note: they have equal variance But I usually do unqueal variance just
in case...




::: {.cell}

```{.r .cell-code}
# Standard t-test (if variances are NOT equal)
t_test_result <- t.test(log_phytobiomass ~ tp_level, data = l_df, var.equal = FALSE)
t_test_result
```

::: {.cell-output .cell-output-stdout}

```

	Welch Two Sample t-test

data:  log_phytobiomass by tp_level
t = 2.0169, df = 49.585, p-value = 0.04914
alternative hypothesis: true difference in means between group high and group low is not equal to 0
95 percent confidence interval:
 0.002072605 1.053226540
sample estimates:
mean in group high  mean in group low 
          1.535511           1.007861 
```


:::
:::




## Non-parametric T-Test




::: {.cell}

```{.r .cell-code}
# Nonparametric TTest
wilcox_result <- wilcox.test(log_phytobiomass ~ tp_level, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
compute exact p-value with ties
```


:::

```{.r .cell-code}
wilcox_result
```

::: {.cell-output .cell-output-stdout}

```

	Wilcoxon rank sum test with continuity correction

data:  log_phytobiomass by tp_level
W = 469, p-value = 0.007655
alternative hypothesis: true location shift is not equal to 0
```


:::
:::




# Question 4

Is there a significant difference in winter phytoplankton biomass
between low nutrient (TP ≤3 µg/L) and high nutrient lakes ( TP \> 3
µg/L)?

## Adding a column of high and low TP below above 2 mg/L TP




::: {.cell}

```{.r .cell-code}
l_df <- l_df %>% 
  mutate(tp_level_3 = if_else(tp_ugl >= 3, "high", "low"))
```
:::




## Summary Stats

## Data exploration of TP




::: {.cell}

```{.r .cell-code}
# boxplot of data
l_df %>% 
  ggplot(aes(tp_level_3, phytobiomass_mgl)) + geom_boxplot()
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-51-1.png){width=672}
:::
:::




### note that the data is heavily skewed and hard to even see with boxplot so maybe a histogram would work better




::: {.cell}

```{.r .cell-code}
# histogram of data
l_df %>% 
  ggplot(aes(phytobiomass_mgl)) + geom_histogram(binwidth = 5) + coord_cartesian(xlim = c(0,30)) +facet_grid(.~tp_level_3)
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-52-1.png){width=672}
:::
:::




## What are the summary statistics of the data?




::: {.cell}

```{.r .cell-code}
q4_summary_stats <- l_df %>%
  group_by(tp_level_3) %>% 
  summarize(mean_total_p = mean(tp_ugl, na.rm=TRUE),
            stddev_total_p = sd(tp_ugl, na.rm=TRUE),
            stderr_total_p = sd(tp_ugl, na.rm=TRUE)/sum(!is.na(tp_ugl)),
            coef_var = (sd(tp_ugl, na.rm=TRUE)/mean(tp_ugl, na.rm=TRUE))*100,
            )
q4_summary_stats
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  tp_level_3 mean_total_p stddev_total_p stderr_total_p coef_var
  <chr>             <dbl>          <dbl>          <dbl>    <dbl>
1 high              20.1          48.8           2.04      243. 
2 low                1.23          0.794         0.0284     64.5
```


:::
:::




## How to calculate the effect size or differnece in means

Note that you can subset a column of data with
`variable[name_ _of_column_to_filter == "term_of_interst]`




::: {.cell}

```{.r .cell-code}
q4_effect_size <- l_df %>%
  summarize(
    winter_mean = mean(phytobiomass_mgl[tp_level_3 == "high"], na.rm = TRUE),
    summer_mean = mean(phytobiomass_mgl[tp_level_3 == "low"], na.rm = TRUE),
    effect_size = winter_mean - summer_mean
  )
q4_effect_size
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 3
  winter_mean summer_mean effect_size
        <dbl>       <dbl>       <dbl>
1        183.        40.9        142.
```


:::

```{.r .cell-code}
# another way to do the caluclation
# tp_effect_size <- l_df %>%
#   group_by(season) %>%
#   summarize(mean_tp = mean(tp_ugl, na.rm = TRUE), .groups = "drop") %>%
#   summarize(effect_size = diff(mean_tp)) %>%
#   pull(effect_size)
# effect_size
```
:::




## looking at the assumptions for a T-Test

For a two-sample t-test, we need to check:

1.  Normality within each group
2.  Equal variances between groups (for standard t-test)
3.  Independent observations

If assumptions are violated:

-   Welch's t-test (unequal variances)
-   Non-parametric alternatives (Mann-Whitney U test)

## Here is the shortcut to be able to do this all at one time

### GGPlot code for qqplots




::: {.cell}

```{.r .cell-code}
# Both seasons in one plot
l_df %>%
  ggplot(aes(sample = phytobiomass_mgl)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ tp_level_3) +
  labs(title = "QQ Plots for Phytobiomass by TP Level",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-55-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
# Both log value seasons in one plot
l_df %>%
  ggplot(aes(sample = log_phytobiomass)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ tp_level_3) +
  labs(title = "QQ Plots for Log10 TP by TP_Level",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

::: {.cell-output-display}
![](assigment_01_key_files/figure-html/unnamed-chunk-56-1.png){width=672}
:::
:::




## Shapiro Wilk Normality test

# How to do Shapiro Wilk Normality test with the Broom package




::: {.cell}

```{.r .cell-code}
# using the broom package
normality_results <- l_df %>%
  group_by(tp_level_3) %>%
  group_modify(~ tidy(shapiro.test(.x$phytobiomass_mgl)))

# Print the results
normality_results
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
# Groups:   tp_level_3 [2]
  tp_level_3 statistic      p.value method                     
  <chr>          <dbl>        <dbl> <chr>                      
1 high           0.731 0.0000268    Shapiro-Wilk normality test
2 low            0.539 0.0000000298 Shapiro-Wilk normality test
```


:::
:::




# How to do Shapiro Wilk Normality test with the purrr package




::: {.cell}

```{.r .cell-code}
library(purrr)

normality_results_purr_4 <- l_df %>%
  group_split(tp_level_3) %>%
  map_dfr(~ {
    test_result <- shapiro.test(log10(.x$phytobiomass_mgl))
    data.frame(
      season = unique(.x$tp_level_3),
      statistic = test_result$statistic,
      p_value = test_result$p.value
    )
  })

normality_results_purr_4
```

::: {.cell-output .cell-output-stdout}

```
      season statistic     p_value
W...1   high 0.8347558 0.001154399
W...2    low 0.9698222 0.575824416
```


:::
:::




## Test for equal variance of the two samples




::: {.cell}

```{.r .cell-code}
# Method 1: Using car package's leveneTest
# This is often preferred as it's more robust to departures from normality
q4_levene_result <- leveneTest(log_phytobiomass ~ tp_level_3, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in leveneTest.default(y = y, group = group, ...): group coerced to
factor.
```


:::

```{.r .cell-code}
q4_levene_result
```

::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  1  1.0053 0.3209
      50               
```


:::
:::




## T-Test

\*Note: they have equal variance But I usually do unqueal variance just
in case...




::: {.cell}

```{.r .cell-code}
# Standard t-test (if variances are NOT equal)
t_test_result_4 <- t.test(log_phytobiomass ~ tp_level_3, data = l_df, var.equal = FALSE)
t_test_result_4
```

::: {.cell-output .cell-output-stdout}

```

	Welch Two Sample t-test

data:  log_phytobiomass by tp_level_3
t = 3.7519, df = 49.741, p-value = 0.0004592
alternative hypothesis: true difference in means between group high and group low is not equal to 0
95 percent confidence interval:
 0.4329098 1.4306982
sample estimates:
mean in group high  mean in group low 
         1.8241624          0.8923584 
```


:::
:::




## Non-parametric T-Test




::: {.cell}

```{.r .cell-code}
# Nonparametric TTest
wilcox_result_4 <- wilcox.test(log_phytobiomass ~ tp_level_3, data = l_df)
```

::: {.cell-output .cell-output-stderr}

```
Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
compute exact p-value with ties
```


:::

```{.r .cell-code}
wilcox_result_4
```

::: {.cell-output .cell-output-stdout}

```

	Wilcoxon rank sum test with continuity correction

data:  log_phytobiomass by tp_level_3
W = 543.5, p-value = 0.0001449
alternative hypothesis: true location shift is not equal to 0
```


:::
:::
