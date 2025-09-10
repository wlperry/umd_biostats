---
title: "Lecture 04: Probability and Inference"
author: "Bill Perry"
metadata-files:
  - ../../_templates/lectures.yml

format:
  html:
    output-file: "04_01_lecture_powerpoint_html.html"
    downloads: [html, docx, pptx, typst]  # This creates download links for all three
  revealjs:
    output-file: "04_01_lecture_powerpoint_slides.html"
  docx:
    output-file: "04_01_lecture_powerpoint.docx"
  pptx:
    output-file: "04_01_lecture_powerpoint.pptx"
  typst:
    output-file: "04_01_lecture_powerpoint.pdf"
---















# **Lecture 4: Probability and Statistical Inference**

::::: columns
::: {.column width="60%"}
-   Review of probability distributions
-   Standard normal distribution and Z-scores
-   Standard error and confidence intervals
-   Statistical inference fundamentals
-   Hypothesis testing principles
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-2-1.png)
:::
:::











:::
:::::

# Practice Exercise 1: Exploring the Grayling Dataset

::: callout-tip
## Practice Exercise 1: Exploring the Grayling Dataset

Let's explore the Arctic grayling data from lakes I3 and I8. Use the
`grayling_df` data frame to create basic summary statistics.












::: {.cell}

```{.r .cell-code}
# Write your code here to explore the basic structure of the data
# also note plottig a box plot is really useful


# Calculate summary statistics
grayling_summary <- grayling_df %>% 
  group_by(lake) %>%
  summarize(
    mean_length = mean(length_mm, na.rm = TRUE),
    sd_length = sd(length_mm, na.rm = TRUE),
    se_length = sd_length/sqrt(sum(!is.na(length_mm))),
    count = sum(!is.na(length_mm)),
    .groups = "drop")
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











:::

# **Lecture 4:** Probability Distributions

::::: columns
::: {.column width="60%"}
## Probability Distribution Functions

-   A **probability distribution** describes the probability of
    different outcomes in an experiment
-   We've seen histograms of observed data
-   Theoretical distributions help us model and understand real-world
    data
-   We will focus on a **standard normal distribution** and a
    **students** **t distribution**
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-4-1.png)
:::
:::











:::
:::::

# **Lecture 4:** The Standard Normal Distribution

::::: columns
::: {.column width="60%"}
The standard normal distribution is crucial for understanding
statistical inference:

-   Has mean (μ) = 0 and standard deviation (σ) = 1
-   Symmetrical bell-shaped curve
-   Area under the curve = 1 (total probability)
-   **Approximately**:
    -   68% of data within ±1σ of the mean
    -   **95% of data within ±2σ of the mean - really 1.96σ**
    -   99.7% of data within ±3σ of the mean

Z-scores allow us to convert any **normal distribution** to the
**standard normal distribution.**
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-5-1.png)
:::
:::











:::
:::::

# Practice Exercise 2: Calculating Z-scores of lake I3

::: callout-tip
## Practice Exercise 2: Calculating Z-scores

Let's practice converting raw values to Z-scores using the Arctic
grayling data.

**Z Score = (length - mean) / standard deviation**












::: {.cell}

```{.r .cell-code}
# Calculate the mean and standard deviation of fish lengths
mean_length <- mean(i3_df$length_mm, na.rm = TRUE)
sd_length <- sd(i3_df$length_mm, na.rm = TRUE)

# Calculate Z-scores for fish lengths
i3_df <- i3_df %>%
  mutate(z_score = (length_mm - mean_length) / sd_length)

# View the first few rows with Z-scores
head(i3_df)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 6
   site lake  species         length_mm mass_g z_score
  <dbl> <chr> <chr>               <dbl>  <dbl>   <dbl>
1   113 I3    arctic grayling       266    135  0.0139
2   113 I3    arctic grayling       290    185  0.862 
3   113 I3    arctic grayling       262    145 -0.127 
4   113 I3    arctic grayling       275    160  0.332 
5   113 I3    arctic grayling       240    105 -0.905 
6   113 I3    arctic grayling       265    145 -0.0214
```


:::
:::











:::

# **Lecture 4:** The fish data as a z score

::::: columns
::: {.column width="60%"}
So if we plot this data what does it look like in a standard normal
distributon?
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-7-1.png)
:::
:::











:::
:::::

# Z-score Results

### How to get area under 1 STD DEV?

Proportion within 1 standard deviation = sum of absolute values of Z
Scores that are less than or equal to 1 divided by the number in the
sample...

Remember in a true normal distribution it is 68% within 1 std dev.

should be **approximately (varies if distribution is not normal)**:

-   68% of data within ±1σ of the mean
-   **95% of data within ±2σ of the mean - really 1.96σ**
-   99.7% of data within ±3σ of the mean












::: {.cell}

```{.r .cell-code}
# What proportion of fish are within 1 standard deviation of the mean?
within_1sd <- sum(abs(i3_df$z_score) <= 1, na.rm = TRUE) / sum(!is.na(i3_df$z_score))
cat("Proportion within 1 SD:", round(within_1sd * 100, 1), "%\n")
```

::: {.cell-output .cell-output-stdout}

```
Proportion within 1 SD: 81.8 %
```


:::
:::












# Lecture 4: Standard normal distribution - Fish Data

::::: columns
::: {.column width="60%"}
You want to know things about this population like

-   probability of a fish having a certain length (e.g., \> 300 mm)
-   Can solve this by integrating the area under curve
-   But it is tedious to do every time
-   Instead
    -   we can use the *standard normal distribution* (SND)
    -   and can use the proportions from the density curve
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 1
  mean_length
        <dbl>
1        266.
```


:::

::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-9-1.png)
:::
:::











:::
:::::

# Lecture 4: Standard normal distribution properties

::::: columns
::: {.column width="60%"}
Standard Normal Distribution

-   "benchmark" normal distribution with µ = 0, σ = 1
-   The Standard Normal Distribution is defined so that:
    -   \~68% of the curve area within +/- 1 σ of the mean,

    -   \~95% within +/- 2 σ of the mean,

    -   \~99.7% within +/- 3 σ of the mean

\*remember σ = standard deviation
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-10-1.png)
:::
:::











:::
:::::

# Lecture 4: Using Z-tables

::::: columns
::: {.column width="60%"}
Areas under curve of Standard Normal Distribution

-   Have been calculated for a range of sample sizes
-   Can be looked up in z-table
-   No need to integrate
-   Any normally distributed data can be standardized
    -   transformed into the standard normal distribution

    -   a value can be looked up in a table
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-11-1.png)
:::
:::











:::
:::::

# Lecture 4: Z-score Formula

::::: columns
::: {.column width="60%"}
Done by converting original data points to z-scores

-   Z-scores calculated as:

## $\text{Z = }\frac{x_i-\mu}{\sigma}$

-   z = z-score for observation
-   xi = original observation
-   µ = mean of data distribution
-   σ = SD of data distribution

So lets do this for a fish that is 300mm long and guess the probability
of catching something larger

z = (300 - 265.61)/28.3 = 1.215194
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-12-1.png)
:::
:::











:::
:::::

# Lecture 4: Z-score example from table

::::: columns
::: {.column width="60%"}
Done by converting original data points to z-scores

-   Z-scores calculated as:

## $\text{Z = }\frac{X_i-\mu}{\sigma}$

-   z = z-score for observation
-   xi = original observation
-   µ = mean of data distribution
-   σ = SD of data distribution

So lets do this for a fish that is 300mm long and guess the probability
of catching something larger

-   z = (300 - 265.61)/28.3 = 1.22
-   look up 1.2 on left and 0.02 on top to get 0.8888 in table
-   Means 88.9% is the area left of the curve **and**
-   100 - 88.9 = 11.27% of fish are expected to be longer

**At what point do you think its not likely to catch a larger fish -
what percentage?**

do this the other way using that percent and why?
:::

::: {.column width="40%"}
![](images/clipboard-1995791111.png){width="538" height="519"}
:::
:::::

# Lecture 4: Z-score example calculation in r

::::: columns
::: {.column width="60%"}
We can use R to get these values easier...

\# For standard normal distribution (mean=0, sd=1):

-   pnorm(z) \# gives cumulative probability (area to the left)
-   qnorm(p) \# gives z-value for a given probability
-   dnorm(z) \# gives probability density
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Examples:
z_value <-  1.22
prob_left <- pnorm(z_value)          # 0.975 (97.5% to the left)
prob_right <- 1 - pnorm(z_value)     # 0.025 (2.5% to the right)
prob_between <- pnorm(2) - pnorm(-2)  # 0.95 (95% between ±1.96)
# To find z-value for a given probability:
z_for_95_percent <- qnorm(0.888)     # 1.96
print(prob_left)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.8887676
```


:::

```{.r .cell-code}
print(prob_right)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.1112324
```


:::

```{.r .cell-code}
print(prob_between)
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.9544997
```


:::

```{.r .cell-code}
print(z_for_95_percent)
```

::: {.cell-output .cell-output-stdout}

```
[1] 1.21596
```


:::
:::











:::
:::::

# Lecture 4: We can now use this for fun in the fish

::::: columns
::: {.column width="60%"}
Lets say we are interested in knowing at what point from I3 it is not
likely to catch a larger fish?

Maybe we expect 95% of the time to catch a fish that is "common" but the
5% is the unlikely portion....
:::

::: {.column width="40%"}











::: {.cell}

```{.r .cell-code}
# Examples:
# What fish length corresponds to the top 5% (unlikely)?
top_5_percent_z <- qnorm(0.95)  # z-score for 95th percentile
unlikely_length <- mean_length + (top_5_percent_z * sd_length)

cat("Only 5% of fish are longer than:", round(unlikely_length, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Only 5% of fish are longer than: 312.2 mm
```


:::

```{.r .cell-code}
cat("This corresponds to z-score:", round(top_5_percent_z, 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
This corresponds to z-score: 1.645 
```


:::
:::











:::
:::::

# Lecture 4: What this means

Given that we can: transform data to z-scores from standard normal
distribution...

...figure out area under the curve (probability) associated with range
of z-scores...

...can therefore figure out probability associated with a range of
original data

# Lecture 4: So what is next...

We can look at Standard normal distributions and know probability of a
value being in a range under the standard normal curve...

Previously we had calculated Standard Error and Confidence Intervals -

-   Now can assess our confidence that the population mean is within a
    certain range\
-   Can use t distribution to ask questions like:
    -   “What is probability of getting sample with mean = ȳ from
        population with mean = µ?“ (1 sample t-test)\
    -   “What is the probability that two samples came from same
        population?” (2 sample t-test)

# **Lecture 4:** When Population σ is Unknown

:::: columns
::: {.column width="60%"}
### When calculating confidence intervals we usually DON'T know the population σ (standard deviation) or 𝝁 population mean

-   estimate it from the samples when don't know the population σ or 𝝁
-   and when sample size is small \< \~30
-   can't use the standard normal (z) distribution

*Instead, we use Student's t distribution*
:::

![](images/clipboard-1052237789.png){width="239" height="307"}
::::

# **Lecture 4:** Understanding t-distribution

::::: columns
::: {.column width="60%"}
### When sample sizes are small, the **t-distribution** is more appropriate than the normal distribution.

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
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-13-1.png)
:::
:::











:::
:::::

# Student's t-distribution Formula

::::: columns
::: {.column width="60%"}
### To calculate CI for sample from "unknown" population:

## $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$

Where:

-   ȳ is sample mean
-   𝑛 is sample size
-   s is sample standard deviation
-   t t-value corresponding the probability of the CI
-   t in t-table for different degrees of freedom (n-1)
:::

::: {.column width="40%"}
![](images/clipboard-3203878802.png){width="500" height="392"}
:::
:::::

# **Lecture 4:** Student's t-distribution Table

::::: columns
::: {.column width="60%"}
### Here is a t-table

-   Values of t that correspond to probabilities
-   Probabilities listed along top
-   Sample dfs are listed in the left-most column
-   Probabilities are given for one-tailed and two-tailed "questions"
:::

::: {.column width="40%"}
![](images/clipboard-3203878802.png){width="500" height="410"}
:::
:::::

# **Lecture 4:** One-tailed Questions

::::: columns
::: {.column width="60%"}
### One-tailed questions: area of distribution left or (right) of a certain value

-   n=20 (df=19) - 90% of the observations found left
-   t= 1.328 (10% are outside)

![](images/clipboard-1822465473.png){width="180" height="150"}
:::

::: {.column width="40%"}
![](images/clipboard-641796945.png){width="508" height="541"}
:::
:::::

# **Lecture 4:** Two-tailed Questions

::::: columns
::: {.column width="60%"}
Two-tailed questions refer to area between certain values

-   n= 20 (df=19), 90% of the observations are between
-   t=-1.729 and t=1.729 (10% are outside)

![](images/clipboard-2234740159.png){width="276" height="200"}
:::

::: {.column width="40%"}
![](images/clipboard-1734806681.png){width="421" height="514"}
:::
:::::

# **Lecture 4:** t-distribution CI Example

::::: columns
::: {.column width="60%"}
Let's calculate CIs again:

Use two-sided test

## $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$

-   95% CI Sample A: = 272.8 ± 2.306 \* (37.81/(9\^0.5))
-   mean = 272.8, N = 20, and s = 37.81 - t is ?
-   CI = 29.06
-   The 95% CI is between 243.7 and 301.9
-   "The 95% CI for the population mean from sample A is 272.8 ± 29.06
:::

::: {.column width="40%"}
![](images/clipboard-3959630930.png)
:::
:::::

# Practice Exercise 4: Using the t-distribution

::: callout-tip
## Practice Exercise 4: Using the t-distribution

Let's compare confidence intervals using the normal approximation (z)
versus the t-distribution for our fish data. I3 data and 10 fish Mean is
266.7 - sd is 17.12 - se is 5.41

\## $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$
























:::

::: callout-tip

## Practice Exercise 4: Using the t-distribution

Let's compare confidence intervals using the normal approximation (z)
versus the t-distribution for our fish data. I3 data and 10 fish Mean is
266.7 - sd is 17.12 - se is 5.41

\## $\text{CI} = \bar{y} \pm t \cdot \frac{s}{\sqrt{n}}$











::: {.cell}

```{.r .cell-code}
# Display results
cat("Mean:", round(sample_mean, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Mean: 251.2 mm
```


:::

```{.r .cell-code}
cat("Standard deviation:", round(sample_sd, 2), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Standard deviation: 41.47 mm
```


:::

```{.r .cell-code}
cat("Standard error:", round(sample_se, 2), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
Standard error: 13.11 mm
```


:::

```{.r .cell-code}
cat("95% CI using z:", round(z_ci_lower, 1), "to", round(z_ci_upper, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
95% CI using z: 225.5 to 276.9 mm
```


:::

```{.r .cell-code}
cat("95% CI using t:", round(t_ci_lower, 1), "to", round(t_ci_upper, 1), "mm\n")
```

::: {.cell-output .cell-output-stdout}

```
95% CI using t: 221.5 to 280.9 mm
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

# **Lecture 4:** Intro to Hypothesis Testing one tailed

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

**Decision rule**: Reject H₀ if p-value \< α\
\
lets test if our sample mean of 320 is larger than 270 or not?
Essentially we are looking at the confidence intervals!!! But we are
only interested if it is larger
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Summary of One-Tailed Hypothesis Test:
```


:::

::: {.cell-output .cell-output-stdout}

```
Sample mean: 320 
```


:::

::: {.cell-output .cell-output-stdout}

```
Hypothesized mean: 285 
```


:::

::: {.cell-output .cell-output-stdout}

```
Sample size: 12 
```


:::

::: {.cell-output .cell-output-stdout}

```
Standard deviation: 42.15 
```


:::

::: {.cell-output .cell-output-stdout}

```
Standard error: 12.168 
```


:::

::: {.cell-output .cell-output-stdout}

```
t-statistic: 2.876 
```


:::

::: {.cell-output .cell-output-stdout}

```
Critical t-value (one-tailed): 1.796 
```


:::

::: {.cell-output .cell-output-stdout}

```
Critical value: 306.85 
```


:::

::: {.cell-output .cell-output-stdout}

```
Decision: Reject Ho (sample mean falls in upper rejection region)
```


:::
:::











:::
:::::

# **Lecture 4:** Intro to Hypothesis Testing one tailed

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

**Decision rule**: Reject H₀ if p-value \< α\
\
lets test if our sample mean of 320 is larger than 270 or not?
Essentially we are looking at the confidence intervals!!! But we are
only interested if it is larger
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-17-1.png)
:::
:::











:::
:::::

# 

# **Lecture 4:** Hypothesis Testing two tailed

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

lets test if our sample mean of 320 is equal to 270 or not? Essentially
we are looking at the confidence intervals!!!\
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output .cell-output-stdout}

```
Summary of Hypothesis Test:
```


:::

::: {.cell-output .cell-output-stdout}

```
Sample mean: 320 
```


:::

::: {.cell-output .cell-output-stdout}

```
Hypothesized mean: 270 
```


:::

::: {.cell-output .cell-output-stdout}

```
Standard error: 12.603 
```


:::

::: {.cell-output .cell-output-stdout}

```
t-statistic: 3.967 
```


:::

::: {.cell-output .cell-output-stdout}

```
Critical t-value (±): 2.306 
```


:::

::: {.cell-output .cell-output-stdout}

```
Critical values: 240.94 to 299.06 
```


:::

::: {.cell-output .cell-output-stdout}

```
Decision: Reject Ho (sample mean falls in rejection region)
```


:::
:::











:::
:::::

# **Lecture 4:** Hypothesis Testing two tailed

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

lets test if our sample mean of 320 is equal to 270 or not? Essentially
we are looking at the confidence intervals!!!
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-19-1.png)
:::
:::











:::
:::::

# Practice Exercise 5: One-Sample t-Test

::: callout-tip
## Practice Exercise 5: Lets practice a One-Sample t-Test

Let's perform a one-sample t-test to determine if the mean fish length
in Lake I3 differs from 260 mm:












::: {.cell}

```{.r .cell-code}
# get only lake I3
i3_df <- grayling_df %>% filter(lake=="I3")

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
t_test_result <- t.test(i3_df$length_mm, mu = 260)

# View the test results
t_test_result
```

::: {.cell-output .cell-output-stdout}

```

	One Sample t-test

data:  i3_df$length_mm
t = 1.6091, df = 65, p-value = 0.1124
alternative hypothesis: true mean is not equal to 260
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

# Practice Exercise 6: Formulating Hypotheses

::: callout-tip
## Practice Exercise 6: Formulating Hypotheses

For the following research questions about Arctic grayling, write the
null and alternative hypotheses:

1.  Are fish in Lake I8 longer than fish in Lake I3?












::: {.cell}

```{.r .cell-code}
# Let's test one of these hypotheses: Are fish in Lake I8 longer than fish in Lake I3?

# Perform an independent t-test
t_test_result <- t.test(length_mm ~ lake, data = grayling_df, 
                       alternative = "less")  # H₀: μ_I3 ≥ μ_I8, H₁: μ_I3 < μ_I8

# Display the results
t_test_result
```

::: {.cell-output .cell-output-stdout}

```

	Welch Two Sample t-test

data:  length_mm by lake
t = -15.532, df = 161.63, p-value < 2.2e-16
alternative hypothesis: true difference in means between group I3 and group I8 is less than 0
95 percent confidence interval:
      -Inf -86.66138
sample estimates:
mean in group I3 mean in group I8 
        265.6061         362.5980 
```


:::
:::












Based on this t-test, what can we conclude about the difference in fish
length between the two lakes?
:::

# **Lecture 4:** Understanding P-values

::::: columns
::: {.column width="60%"}
A **p-value** is the probability of observing the sample result (or
something more extreme) if the null hypothesis is true.

**Common interpretations:**

-   \- p \< 0.05: Strong evidence against H₀
-   \- 0.05 ≤ p \< 0.10: Moderate evidence against H₀
-   \- p ≥ 0.10: Insufficient evidence against H₀

**Common misinterpretations:**

-   \- p-value is NOT the probability that H₀ is true
-   \- p-value is NOT the probability that results occurred by chance
-   \- Statistical significance ≠ practical significance
-   the smaller the p value does not necessarily mean much... use \<
    0.05 even if is is 10\^-16
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-22-1.png)
:::
:::











:::
:::::

# **Lecture 4:** Type I and Type II Errors

::::: columns
::: {.column width="60%"}
When making decisions based on hypothesis tests, two types of errors can
occur:

**Type I Error (False Positive)**

-   \- Rejecting H₀ when it's actually true
-   \- Probability = α (significance level)
-   \- "Finding an effect that isn't real"

**Type II Error (False Negative)**

-   \- Failing to reject H₀ when it's actually false
-   \- Probability = β - "Missing an effect that is real"

**Statistical Power = 1 - β**

-   \- Probability of correctly rejecting a false H₀
-   \- Increases with:
    -   \- Larger sample size

    -   \- Larger effect size

    -   \- Lower variability

    -   \- Higher α level
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-23-1.png)
:::
:::











:::
:::::

# **Lecture 4:** Type I and Type II Errors

::::: columns
::: {.column width="60%"}
-   What does it mean...
    -   Black curve (Null Distribution): distribution of test statistics
        when Ho is true

    -   Green curve (Alternative Distribution): distribution when Ha is
        true (is an effect)
-   Red shaded area (Type I Error): probability of rejecting Ho when
    it's actually tru
    -   area under the null distribution (black curve) to the right α
        (p=0.05)
-   Blue shaded area (Type II Error): probability of failing to reject
    Ho when alternative is actually true
    -   area under alternative distribution (green curve) left of α
        (depends on effect size, sample size, etc.)

The Key Insight fundamental trade-off in hypothesis testing:

-   as α value moves left or right, change balance between Type I and
    Type II errors
-   Moving left reduces Type II errors increases Type I errors, and vice
    versa
-   power (1 - β) area under the green curve RIGHT of dashed line
-   the probability of correctly detecting a real effect.
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-24-1.png)
:::
:::











:::
:::::

# 

# Practice Exercise 7: Interpreting Errors and Power

::: callout-tip
## Practice Exercise 6: Interpreting P-values and Errors

Given the following scenarios, identify whether a Type I or Type II
error might have occurred:

1.  A researcher concludes that a new fishing regulation increased
    grayling size, when in fact it had no effect.

2.  A study fails to detect a real decline in grayling population due to
    warming water, concluding there was no effect.

3.  Let's calculate the power of our t-test to detect a 30 mm difference
    in length between lakes:












::: {.cell}

```{.r .cell-code}
# Calculate power for detecting a 30 mm difference
# First determine parameters
lake_I3 <- grayling_df %>% filter(lake == "I3")
lake_I8 <- grayling_df %>% filter(lake == "I8") 

n1 <- nrow(lake_I3)
n2 <- nrow(lake_I8)
sd_pooled <- sqrt((var(lake_I3$length_mm) * (n1-1) + 
                  var(lake_I8$length_mm) * (n2-1)) / 
                  (n1 + n2 - 2))

# Calculate power
effect_size <- 30 / sd_pooled  # Cohen's d
df <- n1 + n2 - 2
alpha <- 0.05
power <- power.t.test(n = min(n1, n2), 
                     delta = effect_size,
                     sd = 1,  # Using standardized effect size
                     sig.level = alpha,
                     type = "two.sample",
                     alternative = "two.sided")

# Display results
power
```

::: {.cell-output .cell-output-stdout}

```

     Two-sample t test power calculation 

              n = 66
          delta = 0.6741298
             sd = 1
      sig.level = 0.05
          power = 0.9702076
    alternative = two.sided

NOTE: n is number in *each* group
```


:::
:::











:::

# **Lecture 4:** Summary

::::: columns
::: {.column width="60%"}
**Key concepts covered:**

1.  **Probability distributions** model random phenomena
    -   Normal distribution is especially important
    -   Z-scores standardize measurements
2.  **Standard error** measures precision of estimates
    -   Decreases with larger sample sizes
    -   Used to construct confidence intervals
3.  **Confidence intervals** express uncertainty
    -   Provide plausible range for parameters
    -   95% CI: `mean ± 1.96 × SE`
4.  **Hypothesis testing** evaluates claims
    -   Null vs. alternative hypotheses
    -   P-values quantify evidence against H₀
    -   Consider both statistical and practical significance
:::

::: {.column width="40%"}











::: {.cell}
::: {.cell-output-display}
![](04_01_lecture_powerpoint_files/figure-pptx/unnamed-chunk-26-1.png)
:::
:::











:::
:::::
