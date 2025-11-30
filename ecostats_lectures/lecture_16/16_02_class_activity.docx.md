---
title: "Lecture 16 - Class Activity MANOVA"
author: "Bill Perry"
metadata-files:
  - ../../_templates/activities.yml
format:
  html:
    output-file: "16_02_class_activity.html"
  docx:
    output-file: "16_02_class_activity.docx"
---


::: {.cell}

```{.r .cell-code}
# Load required packages
library(car)          # For ANOVA tests
library(emmeans)      # For estimated marginal means
library(psych)
library(vegan)
library(ggfortify)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(plotly)
library(broom)        # For model summaries
library(patchwork)    # For combining plots
library(janitor)      # For cleaning names
library(tidyverse)    # For data manipulation and visualization

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))


# # Set options
# options(scipen = 999)
```
:::


# Lecture 16: PCA


::: {.cell}

```{.r .cell-code}
# read file
# Load the data
darlingtonia <- read_csv("darlingtonia.csv")
```

::: {.cell-output .cell-output-stderr}

```
Rows: 87 Columns: 11
── Column specification ────────────────────────────────────────────────────────
Delimiter: ","
chr  (1): site
dbl (10): height, mouth_diam, tube_diam, keel_diam, wing1_length, wing2_leng...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


:::

```{.r .cell-code}
# View the structure
head(darlingtonia)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 11
  site  height mouth_diam tube_diam keel_diam wing1_length wing2_length
  <chr>  <dbl>      <dbl>     <dbl>     <dbl>        <dbl>        <dbl>
1 TJH      654       38.4      16.6       6.4           85           76
2 TJH      413       22.2      17.2       5.9           55           26
3 TJH      610       31.2      19.9       6.7           62           60
4 TJH      546       34.4      20.8       6.3           84           79
5 TJH      665       30.5      20.4       6.6           60           51
6 TJH      665       33.6      19.5       6.6           84           66
# ℹ 4 more variables: wingsprea <dbl>, hoodmass_g <dbl>, tubemass_g <dbl>,
#   wingmass_g <dbl>
```


:::

```{.r .cell-code}
darlingtonia %>%
  count(site, name = "n_plants")
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 4 × 2
  site  n_plants
  <chr>    <int>
1 DG          25
2 HD          12
3 LEH         25
4 TJH         25
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Create long format for easier plotting
darl_long <- darlingtonia %>%
  pivot_longer(cols = -site,
               names_to = "variable",
               values_to = "value")

# Box plots by site
ggplot(darl_long, aes(x = site, y = value, fill = site)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Raw Data by Site",
       subtitle = "Notice the different scales!")
```

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-2-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# Example: standardize height
darlingtonia %>% 
  group_by(site) %>% 
  summarize(
    mean_height = mean(height),
    sd_height = sd(height),
    mean_wingmass = mean(wingmass_g),
    sd_wingmass = sd(wingmass_g)
  )
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 4 × 5
  site  mean_height sd_height mean_wingmass sd_wingmass
  <chr>       <dbl>     <dbl>         <dbl>       <dbl>
1 DG           619.     101.         0.225       0.0682
2 HD           574.     101.         0.0567      0.0202
3 LEH          638.     115.         0.151       0.122 
4 TJH          610.      82.0        0.134       0.0853
```


:::

```{.r .cell-code}
# After standardization, both will have:
# mean = 0, sd = 1
```
:::



::: {.cell}

```{.r .cell-code}
# Select only numeric variables for PCA
darl_numeric <- darlingtonia %>%
  dplyr::select(-site)

# Standardize using scale()
# This centers (mean=0) and scales (sd=1) each variable
darl_scaled <- scale(darl_numeric)

darl_scaled_df  <- as.data.frame(darl_scaled)

darl_scaled_df %>%
  select(height, mouth_diam, wingmass_g) %>%
  pivot_longer(cols = c(height, mouth_diam, wingmass_g) , names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill  = variable)) +
  geom_boxplot() +
  labs(title = "Standardized Data (same scale)") +
  theme(legend.position = "none")
```

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-4-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# Calculate correlation matrix
cor_matrix <- cor(darl_numeric)

# Visualize with corrplot
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.cex = 0.7,
         addCoef.col = "black",
         number.cex = 0.6,
         title = "Correlation Matrix\n(Darlingtonia variables)",
         mar = c(0,0,2,0))
```

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-5-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# Calculate correlation matrix
cor_matrix <- cor(darl_numeric)

# Display rounded values
round(cor_matrix, 2)
```

::: {.cell-output .cell-output-stdout}

```
             height mouth_diam tube_diam keel_diam wing1_length wing2_length
height         1.00       0.61      0.24     -0.06         0.28         0.29
mouth_diam     0.61       1.00     -0.05     -0.39         0.57         0.44
tube_diam      0.24      -0.05      1.00      0.65        -0.09         0.01
keel_diam     -0.06      -0.39      0.65      1.00        -0.35        -0.24
wing1_length   0.28       0.57     -0.09     -0.35         1.00         0.84
wing2_length   0.29       0.44      0.01     -0.24         0.84         1.00
wingsprea      0.12       0.25      0.10     -0.19         0.64         0.71
hoodmass_g     0.49       0.76     -0.11     -0.34         0.61         0.49
tubemass_g     0.84       0.72      0.10     -0.24         0.44         0.36
wingmass_g     0.40       0.62     -0.16     -0.36         0.76         0.69
             wingsprea hoodmass_g tubemass_g wingmass_g
height            0.12       0.49       0.84       0.40
mouth_diam        0.25       0.76       0.72       0.62
tube_diam         0.10      -0.11       0.10      -0.16
keel_diam        -0.19      -0.34      -0.24      -0.36
wing1_length      0.64       0.61       0.44       0.76
wing2_length      0.71       0.49       0.36       0.69
wingsprea         1.00       0.25       0.16       0.39
hoodmass_g        0.25       1.00       0.76       0.80
tubemass_g        0.16       0.76       1.00       0.63
wingmass_g        0.39       0.80       0.63       1.00
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Perform PCA using prcomp()
# center = TRUE: center variables (mean = 0)
# scale. = TRUE: scale variables (sd = 1)
pca_result <- prcomp(darl_numeric, 
                     center = TRUE, 
                     scale = TRUE)


# View summary
summary(pca_result)
```

::: {.cell-output .cell-output-stdout}

```
Importance of components:
                          PC1    PC2    PC3     PC4     PC5     PC6     PC7
Standard deviation     2.2332 1.3288 1.2354 0.75701 0.59301 0.51062 0.46177
Proportion of Variance 0.4987 0.1766 0.1526 0.05731 0.03517 0.02607 0.02132
Cumulative Proportion  0.4987 0.6753 0.8279 0.88521 0.92038 0.94645 0.96777
                          PC8     PC9    PC10
Standard deviation     0.3768 0.33982 0.25457
Proportion of Variance 0.0142 0.01155 0.00648
Cumulative Proportion  0.9820 0.99352 1.00000
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Create scree plot
fviz_eig(pca_result, 
         addlabels = TRUE, 
         ylim = c(0, 50))
```

::: {.cell-output .cell-output-stderr}

```
Warning in geom_bar(stat = "identity", fill = barfill, color = barcolor, :
Ignoring empty aesthetic: `width`.
```


:::

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-8-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# View loadings for first 3 PCs
loadings <- pca_result$rotation[, 1:3]
round(loadings, 3)
```

::: {.cell-output .cell-output-stdout}

```
                PC1    PC2    PC3
height       -0.278 -0.449  0.198
mouth_diam   -0.369 -0.110  0.214
tube_diam     0.040 -0.602 -0.390
keel_diam     0.202 -0.491 -0.328
wing1_length -0.375  0.171 -0.280
wing2_length -0.342  0.131 -0.415
wingsprea    -0.239  0.156 -0.549
hoodmass_g   -0.385 -0.057  0.185
tubemass_g   -0.355 -0.320  0.263
wingmass_g   -0.393  0.080 -0.007
```


:::
:::



::: {.cell}

```{.r .cell-code}
pc1_loads <- tibble(
  Variable = rownames(pca_result$rotation),
  Loading = pca_result$rotation[, 1]) %>%
  arrange(desc(abs(Loading)))
pc1_loads
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 10 × 2
   Variable     Loading
   <chr>          <dbl>
 1 wingmass_g   -0.393 
 2 hoodmass_g   -0.385 
 3 wing1_length -0.375 
 4 mouth_diam   -0.369 
 5 tubemass_g   -0.355 
 6 wing2_length -0.342 
 7 height       -0.278 
 8 wingsprea    -0.239 
 9 keel_diam     0.202 
10 tube_diam     0.0402
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Visualize PC1 loadings
pc1_loads %>%
  ggplot(aes(x = reorder(Variable, Loading), y = Loading)) +
  geom_col(aes(fill = Loading > 0), show.legend = FALSE) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "PC1 Loadings",
       subtitle = "Overall Plant Size",
       x = "Variable",
       y = "Loading") +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue")) +
  theme_minimal()
```

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-11-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
pc2_loads <- tibble(
  Variable = rownames(pca_result$rotation),
  Loading = pca_result$rotation[, 2]
) %>%
  arrange(desc(abs(Loading)))

pc2_loads
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 10 × 2
   Variable     Loading
   <chr>          <dbl>
 1 tube_diam    -0.602 
 2 keel_diam    -0.491 
 3 height       -0.449 
 4 tubemass_g   -0.320 
 5 wing1_length  0.171 
 6 wingsprea     0.156 
 7 wing2_length  0.131 
 8 mouth_diam   -0.110 
 9 wingmass_g    0.0800
10 hoodmass_g   -0.0575
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Visualize PC2 loadings
pc2_loads %>%
  ggplot(aes(x = reorder(Variable, Loading), y = Loading)) +
  geom_col(aes(fill = Loading > 0), show.legend = FALSE) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "PC2 Loadings",
       subtitle = "Shape: Height vs. Wing Size",
       x = "Variable",
       y = "Loading") +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue")) +
  theme_minimal()
```

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-13-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# PC scores are in pca_result$x
pc_scores <- as_tibble(pca_result$x) %>%
  mutate(site = darlingtonia$site,
         plant_id = 1:n())

# First few plants
pc_scores %>%
  select(plant_id, site, PC1, PC2, PC3) %>%
  head(10)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 10 × 5
   plant_id site     PC1     PC2    PC3
      <int> <chr>  <dbl>   <dbl>  <dbl>
 1        1 TJH   -1.92   0.0772  1.53 
 2        2 TJH    3.35   1.53    0.755
 3        3 TJH    0.918 -0.0320  0.248
 4        4 TJH   -0.983  0.236  -0.242
 5        5 TJH    1.15  -1.02    1.54 
 6        6 TJH   -1.37  -0.613   0.847
 7        7 TJH   -1.50  -1.63    0.693
 8        8 TJH   -0.651  0.121  -0.412
 9        9 TJH    2.48  -0.0786 -0.260
10       10 TJH   -1.11  -1.27    2.27 
```


:::
:::



::: {.cell}

```{.r .cell-code}
# Basic scores plot
ggplot(pc_scores, aes(x = PC1, y = PC2, color = site)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.68, linetype = 2) +
  labs(title = "PCA Scores Plot",
       x = paste0("PC1: Overall Size"),
       y = paste0("PC2: Shapeb%)"),
       color = "Site") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")
```

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-15-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
pc_data <- as.data.frame(pca_result$x) %>%
  mutate(site = darlingtonia$site)

plot_ly(pc_data, 
        x = ~PC1, y = ~PC2, z = ~PC3,
        color = ~site,
        type = "scatter3d",
        mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  ))
```

::: {.cell-output .cell-output-stderr}

```
file:////private/var/folders/7n/4qxr28lj6bj3pdwwxn6cc2w40000gq/T/RtmpPmmigP/file10ac54594241/widget10ac300ce4aa.html screenshot completed
```


:::

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-16-1.jpeg)
:::
:::



::: {.cell}

```{.r .cell-code}
# 1. Prepare PC Scores with Site
pc_scores_for_dist <- as.data.frame(pca_result$x) %>%
  mutate(site = darlingtonia$site)

# 2. Calculate the mean PC scores for each site (the site "centroids")
site_centroids <- pc_scores_for_dist %>%
  group_by(site) %>%
  summarise(across(starts_with("PC"), mean)) %>%
  ungroup()

# 3. Set site names as row names BEFORE calculating distances
site_centroids_matrix <- site_centroids %>%
  column_to_rownames("site")  # This keeps site names as row names

# 4. Calculate the Euclidean distance matrix between site centroids
dist_matrix <- dist(site_centroids_matrix, method = "euclidean")

# Display the distance matrix
dist_matrix
```

::: {.cell-output .cell-output-stdout}

```
          DG       HD      LEH
HD  4.346434                  
LEH 1.621198 3.659711         
TJH 1.843120 3.021386 1.960547
```


:::

```{.r .cell-code}
# Visualize the distance matrix
fviz_dist(dist_matrix, 
          gradient = list(low = "#00AFBB", high = "#FC4E07"))
```

::: {.cell-output .cell-output-stderr}

```
Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
ℹ Please use tidy evaluation idioms with `aes()`.
ℹ See also `vignette("ggplot2-in-packages")` for more information.
ℹ The deprecated feature was likely used in the factoextra package.
  Please report the issue at <https://github.com/kassambara/factoextra/issues>.
```


:::

::: {.cell-output-display}
![](16_02_class_activity_files/figure-docx/unnamed-chunk-17-1.jpeg)
:::
:::

