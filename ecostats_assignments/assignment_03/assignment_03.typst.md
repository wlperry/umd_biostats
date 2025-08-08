---
title: "assignment_03 factorial anovas"
author: "Bill Perry"
metadata-files:
  - ../../_templates/assignments.yml
format:
  html:
    output-file: "assignment_03.html"
  typst:
    output-file: "assignment_03 factorial anovas.pdf"
editor: visual
---







# Ecological Statistics Assignment 03

Females of the yellow dung fly, *Scathophaga stercoraria*, mate with multiple males, leading to “sperm competition” among different males trying to fertilize the females eggs.

What determines a sperm’s competitiveness? To explore this, Hosken, Blanckenhorn, and Garner (2002) tested whether the origin of males and females (fUK or Switzerland) and their interaction influenced the percentage of offspring sired by the second male.

Our model is:

Siring by second male = Female + Male + Female \* Male

### **Biological Hypotheses**

There are a few possibilities:

-   Females from the UK populations may reject (or accept) more sperm from the second male than females from Switzerland (main effect of female population).

-   Sperm from UK males may have higher (or lower) siring success than sperm from Swiss males (main effect of male population).

-   Alternatively, sperm from Swiss males might have high siring success with Swiss females, while UK male sperm has high success with UK females, suggesting harmonious co-adaptation (interaction).

-   Or perhaps females have evolved to resist local sperm, so sperm from Swiss males has high success with UK females but low success with Swiss females, suggesting intersexual conflict.

### The Data

The data are in the data subdirectory data - dung_fly_data.csv

These have been modified slightly such that the first data point is the mating between a UK Female and a Swiss Male

## References: