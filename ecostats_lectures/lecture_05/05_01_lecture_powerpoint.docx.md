---
title: "Lecture 05: Probability and Statistical Inference"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml
format:
  html:
    output-file: "05_01_lecture_powerpoint_html.html"
    downloads: [docx, pptx, typst]  # This creates download links for all three
  revealjs:
    output-file: "05_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "05_01_lecture_powerpoint.docx"
  pptx:
    output-file: "05_01_lecture_powerpoint.pptx"
  typst:
    output-file: "05_01_lecture_powerpoint.pdf"
---













# **Lecture 4: Review**

::::: columns
::: {.column width="60%"}
-   Introduction to histograms or frequency distributions
-   Probability Distribution Functions (PDF)
-   Descriptive Statistics
    -   Center - mean, median, mode

    -   Spread - range, variance, standard deviation
-   Tests of means using T-Tests
    -   one sample
    -   two sample
:::

::: {.column width="40%"}
![](images/clipboard-536528302.png){width="241" height="330"}
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
-   Tests of means using T-Tests
    -   one sample

    -   two sample
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
-   Tests of means using T-Tests
    -   one sample - is the sample mean different from a hypothesized mean?

    -   two sample - are the sample means from two samples the same or different?












::: {.cell}
::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  lake  mean_length sd_length se_length count
  <chr>       <dbl>     <dbl>     <dbl> <int>
1 I3           266.      28.3      3.48    66
2 I8           363.      52.3      5.18   102
```


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
![](images/clipboard-3203878802.png){width="465" height="410"}
:::
:::::

# **Lecture 5:** One-tailed Questions

::::: columns
::: {.column width="60%"}
One-tailed questions: area of distribution left or (right) of a certain value

-   n=20 (df=19) - 90% of the observations found left
-   t= 1.328 (10% are outside)

![](images/clipboard-1822465473.png){width="267" height="198"}
:::

::: {.column width="40%"}
![](images/clipboard-641796945.png){width="491" height="349"}
:::
:::::

# **Lecture 5:** Two-tailed Questions

::::: columns
::: {.column width="60%"}
Two-tailed questions refer to area between certain values

-   n= 20 (df=19), 90% of the observations are between
-   t=-1.729 and t=1.729 (10% are outside)

![](images/clipboard-2234740159.png){width="200" height="150"}
:::

::: {.column width="40%"}
![](images/clipboard-1734806681.png){width="350" height="200"}
:::
:::::

# **Lecture 5:** Calculating CI Example

::::: columns
::: {.column width="60%"}
Let's calculate CIs again:

Use two-sided test

-   $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$
-   95% CI Sample A: = 272.8 ± 2.262 \* (37.81/(10\^0.5)) = 27.05
-   The 95% CI is between 245.8 and 299.8
-   "The 95% CI for the population mean from sample A is 272.8 ± 27.05
:::

::: {.column width="40%"}
![](images/clipboard-3203878802.png){width="518" height="405"}
:::
:::::

# **Lecture 5:** Applications of t-distribution

So:

-   Can assess confidence that population mean is within a certain range
-   Can use t distribution to ask questions like:
    -   "What is probability of getting sample with mean = ȳ from population with mean = µ?" (1 sample t-test)
    -   "What is the probability that two samples came from same population?" (2 sample t-test)

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
shapiro.test(i3_df$length_mm)
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
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-4-1.png)
:::
:::












# Practice Exercise 1: One-Sample t-Test

::: callout-tip
## Practice Exercise 1: One-Sample t-Test

Let's perform a one-sample t-test to determine if the mean fish length in I3 Lake differs from 240 mm:












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
5.  What is the practical interpretation of this result for fish biologists?
:::

# **Lecture 5:** Hypothesis Testing Framework

::::: columns
::: {.column width="60%"}
Hypothesis testing is a systematic way to evaluate research questions using data.

**Key components:**

1.  **Null hypothesis (Ho)**: Typically assumes "no effect" or "no difference"
2.  **Alternative hypothesis (Ha)**: The claim we're trying to support
3.  **Statistical test**: Method for evaluating evidence against H₀
4.  **P-value**: Probability of observing our results (or more extreme) if H₀ is true
5.  **Significance level (α)**: Threshold for rejecting H₀, typically 0.05

**Decision rule**: Reject Ho if p-value less than α or shorthand p \< 0.05
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-6-1.png)
:::
:::











:::
:::::

# **Lecture 5:** Hypothesis Testing

::::: columns
::: {.column width="60%"}
Hypothesis testing is a systematic way to evaluate research questions using data.

**Key components:**

1.  **Null hypothesis (H₀)**: Typically assumes "no effect" or "no difference"

2.  **Alternative hypothesis (Hₐ)**: The claim we're trying to support

3.  **Statistical test**: Method for evaluating evidence against H₀

4.  **P-value**: Probability of observing our results (or more extreme) if H₀ is true

5.  **Significance level (α)**: Threshold for rejecting H₀, typically 0.05

**Decision rule**: Reject H₀ if p-value \< α
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-7-1.png)
:::
:::











:::
:::::

# Lecture 5: Interpreting One-Sample T-Test Results

**Activity: Interpret the t-test results**

-   What does the p-value tell us?
-   Should we reject or fail to reject the null hypothesis?

**How to report this result in a scientific paper:**

"A one-sample t-test at α=0.05 showed that the mean fish length (... mm, SD = ...) \[was/was not\] significantly different from the expected 240 mm, t(...) = ..., p = ..."

# **Lecture 5:** Two Sample T-Tests Introduction

::::: columns
::: {.column width="60%"}
For example

-   what is probability that population X is the same as population Y?

How would you assess this question using what we learned?

This is what we will do with the fish length again...
:::

::: {.column width="40%"}
![](images/slimy_sculpin_cottus_cognatus.jpg)
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


# Create a boxplot comparing the two lakes
i3_i8_plot <- grayling_df %>%
  ggplot(aes(x = lake, y = length_mm, fill = lake)) +
  geom_boxplot() +
  labs(
       x = "lake",
       y = "Length (mm)",
       fill = "Lake") 
i3_i8_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-8-1.png)
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

For the following research questions about fish lenghts write the null and alternative hypotheses:

1.  Are fish lengths in lake I3 and I8 different?

What are the hypotheses?

Ho =

Ha =
:::

# **Lecture 5:** Two-Sample T-Test Framework

Now, let's compare fish lengths from the two lakes

Question: **Is there a significant difference in fish length between the lakes?**

This requires a two-sample t-test.

Two-sample t-test compares means from two independent groups.

## $t = \frac{\bar{x}_1 - \bar{x}_2}{S_p\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}}$

where:

-   x̄₁ and x̄₂: These represent the sample means of the two groups you're comparing
-   s²ₚ: This is the pooled variance, calculated as: s²ₚ = \[(n₁ - 1)s₁² + (n₂ - 1)s₂²\] / (n₁ + n₂ - 2), where s₁² and s₂² are the sample variances of the two groups.
-   **n₁ and n₂:** These are the sample sizes of the two groups.
-   **√(1/n₁ + 1/n₂):** This represents the pooled standard error.

## $t = \frac{SIGNAL}{NOISE}$

# Practice Exercise 3: Summary Statistics

::: callout-tip
## Practice Exercise 3: **Calculate summary statistics grouped by lake**

Before conducting the test, we need to understand the data for each group.

1.  You need this and the graph to see what is going on ....












    ::: {.cell}
    
    ```{.r .cell-code}
    group_summary <- grayling_df %>%
      group_by(lake) %>%
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
      lake  mean_length sd_length     n se_length
      <chr>       <dbl>     <dbl> <int>     <dbl>
    1 I3           266.      28.3    66      3.48
    2 I8           363.      52.3   102      5.18
    ```
    
    
    :::
    :::











:::

# Visualizing Group Differences












::: {.cell}

```{.r .cell-code}
# Create a boxplot comparing the two sides
i3_i8_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-10-1.png)
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
  summarize(difference = mean_length[lake == "I8"] - mean_length[lake == "I3"])
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 1
  difference
       <dbl>
1       97.0
```


:::
:::











:::

# Practice Exercise 5: ggplot Summary Statistics

::: callout-tip
## Practice Exercise 5: Using GGPLOT to get summary stats

GGplot also has code to make the mean and standard error plots we are interested in along with a lot of others












::: {.cell}

```{.r .cell-code}
# Assuming your dataframe is called df
fish_mean_se_plot <- ggplot(grayling_df, aes(x = lake, y = length_mm, color = lake)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
       x = "lake",
       y = "Mean Length (mm)") +
  theme_classic()
fish_mean_se_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-12-1.png)
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
fish_mean_se_plot
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-13-1.png)
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
i3_data <- grayling_df %>% filter(lake == "I3")
i8_data <- grayling_df %>% filter(lake == "I8")
head(i8_data)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 5
   site lake  species         length_mm mass_g
  <dbl> <chr> <chr>               <dbl>  <dbl>
1   118 I8    arctic grayling       345    375
2   118 I8    arctic grayling       376    485
3   118 I8    arctic grayling       417    600
4   118 I8    arctic grayling       369    445
5   118 I8    arctic grayling       419    625
6   118 I8    arctic grayling       365    430
```


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
qqPlot(i8_data$length_mm, 
       main = "QQ Plot for I8 fish length",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-15-1.png)
:::

::: {.cell-output .cell-output-stdout}

```
[1] 79 62
```


:::
:::











:::

# Practice Exercise 9: Shapiro-Wilk Test

::: callout-tip
## Practice Exercise 9: Test normality of I8 lengths

Shapiro-Wilk test

Note you need to test each groups separately...












::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test for windward group
shapiro.test(i8_data$length_mm)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  i8_data$length_mm
W = 0.91962, p-value = 1.119e-05
```


:::
:::











:::

# Practice Exercise 10: QQ Plot for I3

::: callout-tip
## Practice Exercise 10: Test normality of I3 lengths

qqplots

Note you need to test each groups separately...












::: {.cell}

```{.r .cell-code}
# You can also test the i3
# QQ Plot for leeward group
qqPlot(i3_data$length_mm, 
       main = "QQ Plot for I3",
       ylab = "Sample Quantiles")
```

::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-17-1.png)
:::

::: {.cell-output .cell-output-stdout}

```
[1] 53 35
```


:::
:::











:::

# Practice Exercise 11: Shapiro-Wilk for i3

::: callout-tip
## Practice Exercise 11: Test normality of i3 lengths

Shapiro-Wilk test

Note you need to test each groups separately...












::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test for leeward group
 shapiro.test(i3_data$length_mm)
```

::: {.cell-output .cell-output-stdout}

```

	Shapiro-Wilk normality test

data:  i3_data$length_mm
W = 0.91051, p-value = 0.0001623
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
normality_results <- grayling_df %>%
  group_by(lake) %>%
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
  lake  shapiro_stat shapiro_p_value normal_distribution
  <chr>        <dbl>           <dbl> <chr>              
1 I3           0.911       0.000162  Non-normal         
2 I8           0.920       0.0000112 Non-normal         
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
levene_result <- leveneTest(length_mm ~ lake, data = grayling_df)
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
       Df F value    Pr(>F)    
group   1  13.705 0.0002907 ***
      166                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::











:::

# **Lecture 5:** Conducting the Two-Sample T-Test

::::: columns
::: {.column width="60%"}
Now we can compare the mean fish lengths between i3 and i8.

Ho: μ₁ = μ₂ (The fish lengths do not differ)

Ha: μ₁ ≠ μ₂ (The mean fish lengths differ - direction is not specified)

Deciding between:

-   Standard t-test (equal variances)

-   Welch's t-test (unequal variances)

**Note the Levenes Test should be NOT SIGNIFICANT - What is the null hypothesis**
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Levene's Test for Homogeneity of Variance (center = median)
       Df F value    Pr(>F)    
group   1  13.705 0.0002907 ***
      166                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::











:::
:::::

# **Lecture 5:** Running the Two-Sample T-Test

Now we can do a two sample TTEST

Calculate t-statistic manually (optional)

YOUR CODE HERE:

t = (mean1 - mean2) / sqrt((s1\^2/n1) + (s2\^2/n2))












::: {.cell}

```{.r .cell-code}
grayling_summary
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  lake  mean_length sd_length se_length count
  <chr>       <dbl>     <dbl>     <dbl> <int>
1 I3           266.      28.3      3.48    66
2 I8           363.      52.3      5.18   102
```


:::
:::












::: callout-tip











::: {.cell}

```{.r .cell-code}
# YOUR TASK: Conduct a two-sample t-test
# Use var.equal=TRUE for standard t-test or var.equal=FALSE for Welch's t-test

# Standard t-test (if variances are equal)
t_test_result <- t.test(length_mm ~ lake, data = grayling_df, var.equal = TRUE)
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

data:  length_mm by lake
t = -13.797, df = 166, p-value < 2.2e-16
alternative hypothesis: true difference in means between group I3 and group I8 is not equal to 0
95 percent confidence interval:
 -110.87187  -83.11208
sample estimates:
mean in group I3 mean in group I8 
        265.6061         362.5980 
```


:::

```{.r .cell-code}
# Welch's t-test (if variances are unequal)
# YOUR CODE HERE
```
:::











:::

# **Lecture 5:** Difference between a Two-Sample T and Welch's T Test

## Standard t-test (Student's t-test)

-   **Assumes equal variances** between the two groups being compared
-   Uses a **pooled variance estimate** that combines data from both group
-   Has **higher statistical power** when the equal variance assumption is met
-   **Degrees of freedom** = n₁ + n₂ - 2

## Welch's t-test

-   **Does not assume equal variances** between groups (also called the "unequal variances t-test")
-   Uses **separate variance estimates** for each group
-   More **robust** when group variances are different
-   Uses a more complex **degrees of freedom calculation** (Welch-Satterthwaite equation)
-   **Degrees of freedom** are typically non-integer and usually smaller than the standard t-test

# **Lecture 5:** Interpreting Two-Sample T-Test Results

::::: columns
::: {.column width="60%"}
**Interpret the results of the two-sample t-test**

What can we conclude about the needle lengths on windward vs. leeward sides?

**How to report this result in a scientific paper:**

"A two-tailed, two-sample t-test at α=0.05 showed \[a significant/no significant\] difference in needle length between windward (M = ..., SD = ...) and leeward (M = ..., SD = ...) sides of pine trees, t(...) = ..., p = ...."
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-23-1.png)
:::
:::











:::
:::::

# **Lecture 5:** Visualizing the Results

::::: columns
::: {.column width="60%"}
**Interpret the results of the two-sample t-test**

What can we conclude about the needle lengths on windward vs. leeward sides?

**How to report this result in a scientific paper:**

"A two-tailed, two-sample t-test at α=0.05 showed \[a significant/no significant\] difference in needle length between windward (M = ..., SD = ...) and leeward (M = ..., SD = ...) sides of pine trees, t(...) = ..., p = ...."
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](05_01_lecture_powerpoint_files/figure-docx/unnamed-chunk-24-1.png)
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
-   Consider alternatives when assumptions are violated - non parametric tests...