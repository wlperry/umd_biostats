---
title: "04_Class_Activity"
author: "Bill Perry"
metadata-files:
  - ../../_templates/activities.yml
format:
  html:
    output-file: "04_02_class_activity.html"
  typst:
    output-file: "04_02_class_activity.pdf"   
  docx:
    output-file: "04_02_class_activity.docx"
---


# In class activity 4:

## What did we do last time in activity 3?

-   Setting up a project and variable names and code names
-   How to use the pipe command %\>%
-   How to create descriptive statistics of a sample

``` r
p_df %>% 
  summarize(
    mean_length = mean(length_mm, na.rm = TRUE),
    sd_length = sd(length_mm, na.rm = TRUE),
    n_length = sum(!is.na(length_mm)))
```

-   More graphs...

    ``` r
    ggplot(data = p_df, aes(x=length_mm, fill = wind)) +
      geom_histogram( binwidth = 2, 
    # sets the width in units of the bins - try different nubmers
       position = position_dodge2(width = 0.5))
    ```

    ![](images/clipboard-1074788880.png){width="455"}

-   What questions do you have and what is unclear - what did not work
    so far when you started the homework?

# Introduction

In this active learning module, we'll explore real data from fish
populations in Alaska. We'll focus on understanding:

-   How to create and interpret frequency distributions
-   How sample size affects our view of a population
-   How distributions differ among lakes

We'll use the `tidyverse` package for data manipulation and
visualization.

## Setup

First, let's load the packages we need and the dataset:

::: {.cell}

```{.r .cell-code}
# # Install the patchwork package if needed

# install.packages("patchwork")
library(patchwork)
library(skimr)
library(tidyverse)


# Read in the data file
g_df <- read_csv("data/gray_I3_I8.csv") 

i3_df <- g_df %>% filter(lake =="I3")

# Look at the first few rows
head(g_df)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 5
   site lake  species         length_mm mass_g
  <dbl> <chr> <chr>               <dbl>  <dbl>
1   113 I3    arctic grayling       266    135
2   113 I3    arctic grayling       290    185
3   113 I3    arctic grayling       262    145
4   113 I3    arctic grayling       275    160
5   113 I3    arctic grayling       240    105
6   113 I3    arctic grayling       265    145
```


:::
:::

## Basic Data Summary

Let's first check what lakes are in our dataframe:

::: {.cell}

```{.r .cell-code}
# Get a list of unique lakes
unique(g_df$lake)
```

::: {.cell-output .cell-output-stdout}

```
[1] "I3" "I8"
```


:::
:::

How many fish do we have from each lake?

::: {.cell}

```{.r .cell-code}
# Count observations by lake
g_df %>%
  group_by(lake) %>% 
  summarize(fish_n = n())
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 2
  lake  fish_n
  <chr>  <int>
1 I3        66
2 I8       102
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Count observations by lake
g_df %>%
  group_by(lake) %>% 
  summarize(fish_n = sum(!is.na(mass_g)))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 2
  lake  fish_n
  <chr>  <int>
1 I3        66
2 I8       100
```


:::
:::

# Part 1: Creating Frequency Distributions

## Basic Histograms

A histogram shows how many observations fall into certain ranges (or
"bins").

Let's create a simple histogram of fish lengths from I3 :


::: {.cell}

```{.r .cell-code}
# Filter for I3 and create a histogram
g_df %>%
  filter(lake == "I3") %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(binwidth = 2) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-5-1.png){width=336}
:::
:::


::: callout-tip
## Activity 1

Try changing the `binwidth` parameter to 5 and then to 1. How does the
appearance of the histogram change?

::: {.cell}

```{.r .cell-code}
# Try it here
```
:::
:::

## Comparing Lakes

Now let's compare two lakes

::: {.cell}

```{.r .cell-code}
# Compare histograms from I3 I8 lakes
g_df %>%
  ggplot(aes(x = length_mm, fill = lake)) +
  geom_histogram(binwidth = 2) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-7-1.png){width=336}
:::
:::

Now let's compare two lakes side by side:


::: {.cell}

```{.r .cell-code}
# Compare histograms from lake I3 I8 
g_df %>%
  ggplot(aes(x = length_mm, fill = lake)) +
  geom_histogram(binwidth = 5) +
  facet_wrap("lake")
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-8-1.png){width=336}
:::
:::


# Part 2: Sample Size Effects

Let's explore how the sample size affects what we see.

## Small vs. Large Samples

We'll randomly select different sample sizes from I8 Lake:

::: {.cell}

```{.r .cell-code}
# Set a seed for reproducibility
set.seed(123)

# Create small sample (15 fish)
small_sample <- i3_df %>%
  sample_n(10)

# Create larger sample (50 fish)
larger_sample <- i3_df %>%
  sample_n(25)

# Plot both samples
p1 <- small_sample %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(binwidth = 2, fill = "red", alpha = 0.7) +
  coord_cartesian(xlim = c(150,300)) +
  labs(title = "Small Sample (n=15)",
       x = "Length (mm)",
       y = "Count") 

p2 <- larger_sample %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7) +
  coord_cartesian(xlim = c(150,300)) +
  labs(title = "Larger Sample (n=50)",
       x = "Length (mm)",
       y = "Count")


# Display the plots side by side
p1 + p2 +
  plot_layout(ncol = 1)
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-9-1.png){width=336}
:::
:::

::: callout-tip
## Activity 3

Try changing the sample sizes. What happens when you use very small
samples (n=5)? What about larger samples (n=60)?

add code here

::: {.cell}

```{.r .cell-code}
# Set a seed for reproducibility
set.seed(123)

# Create small sample (15 fish)
small_sample <- i3_df %>%
  sample_n(3)

# Create larger sample (50 fish)
larger_sample <- i3_df %>%
  sample_n(6)

# Plot both samples
p1 <- small_sample %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(binwidth = 2, fill = "red", alpha = 0.7) +
  coord_cartesian(xlim = c(150,300)) +
  labs(title = "Small Sample (n=15)",
       x = "Length (mm)",
       y = "Count") 

p2 <- larger_sample %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7) +
  coord_cartesian(xlim = c(150,300)) +
  labs(title = "Larger Sample (n=50)",
       x = "Length (mm)",
       y = "Count")


# Display the plots side by side
p1 + p2 +
  plot_layout(ncol = 1)
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-10-1.png){width=336}
:::
:::
:::

# Part 3: From Histograms to Density Plots

Density plots give us a smoothed version of the histogram:

::: {.cell}

```{.r .cell-code}
# Create a density plot
i3_df %>%
  ggplot(aes(x = length_mm)) +
  geom_density(fill = "blue", alpha = 0.5) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-11-1.png){width=336}
:::
:::

We can overlay the histogram and the density plot:

::: {.cell}

```{.r .cell-code}
# Combine histogram and density plot
i3_df %>%
  ggplot(aes(x = length_mm)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, 
                 fill = "lightblue", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-12-1.png){width=336}
:::
:::

::: callout-tip
## Activity 4

Create a density plot comparing multiple lakes I3 to I8. Which lakes
have similar distributions? Which ones are different?

Try code here using patchwork or facet_grid

::: {.cell}

```{.r .cell-code}
 #Enter code here#
```
:::

# 
:::

::: {.cell}

```{.r .cell-code}
# Function to calculate area under density curve
calculate_density_area <- function(data_vector) {
  # Remove NA values
  data_vector <- data_vector[!is.na(data_vector)]
  
  # Calculate density
  dens <- density(data_vector)
  
  # Calculate area using numeric integration (trapezoidal rule)
  # Area should be approximately 1
  dx <- diff(dens$x)
  y_avg <- (dens$y[-1] + dens$y[-length(dens$y)]) / 2
  area <- sum(dx * y_avg)
  return(area)
}

# Apply to Toolik lake data
i3_data <- i3_df %>% 
  pull(length_mm)

area_value <- calculate_density_area(i3_data)

# Create plot with calculated area
i3_df %>%
  ggplot(aes(x = length_mm)) +
  geom_density(fill = "blue", alpha = 0.4) +
  geom_area(stat = "density", fill = "red", alpha = 0.3) +
  labs(title = "Area Under Probability Density Function = 1",
       subtitle = paste("Calculated area =", round(area_value, 4)),
       x = "Length (mm)",
       y = "Density")
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-14-1.png){width=336}
:::
:::

This can be adapted to calculate the area of a subset of the plot

I don't expect you to know or be able to do all of this but is here to
play with the code

::: {.cell}

```{.r .cell-code}
# ------- PART 3: SET  INPUT VALUES -------
# change these values to calculate different probabilities
# For this example, let's calculate the probability of fish between 40mm and 60mm
lower_bound <- 320  # change this value
upper_bound <- 350  # change this value

# ------- PART 1: PREPARE THE DATA -------
# Filter data for just one lake to keep it simple for students
i3_fish <- i3_df %>%
  filter(!is.na(length_mm))  # Remove any missing values

# ------- PART 2: CREATE A FUNCTION TO CALCULATE PROBABILITY -------
# This function calculates the probability of finding a fish with length between
# lower_bound and upper_bound using the empirical distribution of our data
calculate_probability <- function(data_vector, lower_bound, upper_bound) {
  # First, we create a density object from our data
  dens <- density(data_vector)
  
  # Find indices of x-values that fall within our bounds
  indices <- which(dens$x >= lower_bound & dens$x <= upper_bound)
  
  # If we have no points in the range, return 0
  if(length(indices) <= 1) {
    return(0)
  }
  
  # Get x and y values within our bounds
  x_values <- dens$x[indices]
  y_values <- dens$y[indices]
  
  # Calculate the area using the trapezoidal rule
  # (average height × width) for each segment, then sum all segments
  widths <- diff(x_values)
  avg_heights <- (y_values[-1] + y_values[-length(y_values)]) / 2
  area_in_range <- sum(widths * avg_heights)
  
  # Return the calculated probability
  return(area_in_range)
}

# ------- PART 4: CALCULATE THE PROBABILITY -------
# Calculate the probability for the specified range
probability <- calculate_probability(i3_fish$length_mm, lower_bound, upper_bound)

# Calculate the total area to show that the complete distribution sums to approximately 1
total_area <- calculate_probability(i3_fish$length_mm, 
                                   min(i3_fish$length_mm),
                                   max(i3_fish$length_mm))

# ------- PART 5: CREATE THE VISUALIZATION -------
# Create density data for the highlighting
density_data <- density(i3_fish$length_mm)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Create a subset for the area of interest
highlight_df <- density_df %>%
  filter(x >= lower_bound & x <= upper_bound)

# Create the plot
ggplot(i3_fish, aes(x = length_mm)) +
  # First, plot the overall density curve in light blue
  geom_density(fill = "lightblue", alpha = 0.5) +
  
  # Then highlight our region of interest in dark red
  geom_area(data = highlight_df, aes(x = x, y = y), 
            fill = "darkred", alpha = 0.7) +
  
  # Add vertical lines to clearly mark the boundaries
  geom_vline(xintercept = lower_bound, linetype = "dashed", color = "red") +
  geom_vline(xintercept = upper_bound, linetype = "dashed", color = "red") +
  
  # Add informative labels
  labs(
    title = "Probability Distribution of Fish Lengths",
    subtitle = paste0("Probability of fish between ", lower_bound, 
                     " and ", upper_bound, " mm = ", 
                     round(probability * 100, 1), "%"),
    caption = paste("Total area under the curve =", round(total_area, 3)),
    x = "Fish Length (mm)",
    y = "Density"
  ) +
  
  # Add text annotations to explain the areas
  annotate("text", x = (lower_bound + upper_bound)/2, 
           y = max(density(i3_fish$length_mm)$y) * 0.7,
           label = paste0("Area = ", round(probability, 3)),
           color = "white", size = 4) +
  
  # Make the plot look nicer
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "darkred")
  )
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-15-1.png){width=336}
:::
:::

# Part 4: Summary Statistics - descriptive statistics

Let's calculate basic summary statistics for each lake for mass:

::: {.cell}

```{.r .cell-code}
# Calculate mean, standard deviation, and sample size by lake
g_df %>%
  group_by(lake) %>%
  summarize(
    mean_length = mean(mass_g),
    sd_length = sd(mass_g),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(count))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 4
  lake  mean_length sd_length count
  <chr>       <dbl>     <dbl> <int>
1 I8            NA       NA     102
2 I3           150.      42.2    66
```


:::
:::

## WOAH - what happened there - there are NA values in the data

you need to either remove missing values or you can do that in the
formulas

*What is the advantage to manually removing or doing it in formulas?*

::: {.cell}

```{.r .cell-code}
# Calculate mean, standard deviation, and sample size by lake
stats_df <- g_df %>%
  group_by(lake) %>%
  summarize(
    mean_length = mean(length_mm, na.rm = TRUE),
    sd_length = sd(length_mm, na.rm = TRUE),
    se_length = sd(length_mm, na.rm = TRUE)/ sum(!is.na(length_mm))^.5,
    count = sum(!is.na(length_mm)),
    .groups = "drop"
  ) %>%
  arrange(desc(count))
stats_df
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 5
  lake  mean_length sd_length se_length count
  <chr>       <dbl>     <dbl>     <dbl> <int>
1 I8           363.      52.3      5.18   102
2 I3           266.      28.3      3.48    66
```


:::
:::

Now let's visualize these statistics:

::: {.cell}

```{.r .cell-code}
# Create a bar plot of mean lengths with error bars
g_df %>%  
  ggplot(aes(lake, length_mm)) +
  stat_summary(
    fun = mean, na.rm = TRUE, geom = "bar"
    ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.2) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-18-1.png){width=336}
:::
:::

We could also do this from the dataframe we just made

::: {.cell}

```{.r .cell-code}
# Create a bar plot of mean lengths with error bars
stats_df %>%  
  ggplot(aes(x = reorder(lake, mean_length), y = mean_length)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(
    ymin = mean_length - se_length, 
    ymax = mean_length + se_length),
    width = 0.2) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-19-1.png){width=336}
:::
:::

The power of the pipe command is you can do this without having to make
a new dataframe

::: {.cell}

```{.r .cell-code}
# Create a bar plot of mean lengths with error bars
g_df %>%
  group_by(lake) %>%
  summarize(
    mean_length = mean(length_mm, na.rm = TRUE),
    sd_length = sd(length_mm, na.rm = TRUE),
    se_length = sd_length / sqrt(n()),
    count = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(lake, mean_length), y = mean_length)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_length - se_length, 
                    ymax = mean_length + se_length),
                width = 0.2) 
```

::: {.cell-output-display}
![](04_02_class_activity_files/figure-html/unnamed-chunk-20-1.png){width=336}
:::
:::

::: callout-tip
## Activity 4

Based on the mean plot and what you've seen in the distributions, what
can you say about fish sizes in different lakes? Are there lakes with
particularly large or small fish?

We will start to ask how different are they and is it by chance?

Where would you want to fish and why? What is the chance of catching a
fish greater than X size?
:::

# 
