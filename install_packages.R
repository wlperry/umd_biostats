# These are packages that you will be using throughout the class... 
# The essential ones are at the top
# The ones we will use later are at the bottom
# there are also some addins if you are using R studio
# you can run these one line at a time by hitting 
#   ctrl + return or
#   command + return or
#   the run button in the upper right

# Packages for addins in R studio not positron
# run as a library only one time
install.packages("ggThemeAssist") # Interactive ggplot2 theme customization tool
install.packages("styler") # Code formatting and style correction
library(ggThemeAssist)
library(styler)

# Essential packages
install.packages("tidyverse") # Core data science packages (dplyr, ggplot2, tidyr, etc.)
install.packages("readxl") # Read Excel files (.xls and .xlsx)
install.packages("janitor") # Data cleaning and tabulation functions
install.packages("patchwork") # Combine multiple ggplot2 plots
install.packages("skimr") # Quick data summaries and exploration
install.packages("tidyplot") # Simplified ggplot2 syntax for common plots
install.packages("multcompView") # Visualize multiple comparison results # paired comparisons - note this will interfear with DPLYR!!
install.packages("Rmisc") # Miscellaneous statistical functions # stats 
install.packages("Hmisc") # Harrell's miscellaneous statistical functions # stats 

# Special Packages for later - install at your own pace later or all at once now
install.packages("emmeans") # Estimated marginal means for statistical models
install.packages("car") # Companion to Applied Regression (ANOVA, regression diagnostics)

# linear mixed models
install.packages("lme4") # Linear mixed-effects models
install.packages("lmerTest") # Tests for linear mixed-effects models

# cleaning up model output
install.packages("broom") # Convert statistical objects into tidy data frames
install.packages("broom.mixed") # Tidy mixed-effects model outputs

# checking assumptions
install.packages("performance") # Model performance assessment and diagnostics
install.packages("DHARMa") # Residual diagnostics for hierarchical models

# Correlation plots
install.packages("corrplot") # Correlation matrix visualization
install.packages("GGally") # Extension to ggplot2 for correlation plots and more

# tables
install.packages("flextable") # Create formatted tables for Word/PowerPoint
install.packages("tinytable") # Simple, lightweight table creation