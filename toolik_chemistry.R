# install.packages("corr")
# install.packages("corrplot")
library(tidyverse)
library(readxl)
library(janitor)
library(corrr)  # For correlation analysis
library(corrplot)  # For visualization


chem_df <- read_excel("data/ARC_Lakes_Chemistry_2010-2021_cleaned.xlsx",
                      sheet="Data", na=".", guess_max = 5000) %>% 
  clean_names() %>% 
  select(-c(pp_u_m, pn_u_m, pc_u_m, doc_u_m, dic_u_m, 
            nh4_indol_u_m, nh4opa_u_m, so4_u_m, cl_u_m,
            ca_u_m, mg_u_m, na_u_m, k_u_m, si_u_m)) %>% 
  mutate(
    corrected_chl_ug_l = as.numeric(corrected_chl_ug_l),  
    total_chl_ug_l     = as.numeric(total_chl_ug_l    ),      
    pheo_ug_l          = as.numeric(pheo_ug_l         ),          
    pp_ug_l            = as.numeric(pp_ug_l           ),             
    pn_ug_l            = as.numeric(pn_ug_l           ),      
    pc_ug_l            = as.numeric(pc_ug_l           )    
    
  )
 

unique(chem_df$lake)

chem_df <- chem_df %>% 
  mutate(lake = case_when(
    lake == "I6 Headwater Lake" ~ "I6",
    TRUE~ lake
  ))
core_df <- chem_df %>% 
  filter(lake %in% c(
    "Toolik", "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8",
    "NE 14" , "NE 12", "Fog 01", "Fog 02", "Fog 03", "Fog 04",
    "Fog 05"
  ))

unique(core_df$lake)
write_csv(core_df, "data/core_lake_chem.csv")

# 1. Select only numeric columns for correlation analysis
numeric_lake_data <- core_df  %>% 
  select(where(is.numeric))%>% complete()

# Print the names of numeric columns
cat("Numeric columns included in correlation analysis:\n")
print(names(numeric_lake_data))

# 2. Calculate correlation matrix with pairwise complete observations
# This handles missing values appropriately
corr_matrix <- cor(numeric_lake_data, use = "pairwise.complete.obs")

# 3. Basic correlation plot
corrplot(corr_matrix, 
         method = "circle",    # Use circles for visualization
         type = "lower",       # Show only lower triangle
         order = "FPC",        # First principal component order
         tl.col = "black",     # Label color
         tl.srt = 45)          # Rotate text labels

# 4. More informative correlation plot with values
corrplot(corr_matrix, 
         method = "color",     # Color squares
         type = "lower",       # Show only lower triangle
         order = "FPC",        # First principal component order
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.7,     # Text size for coefficients
         tl.col = "black",     # Label color
         tl.srt = 45,          # Rotate text labels
         diag = FALSE)         # Hide diagonal

# 5. Mixed visualization - circles with coefficients
corrplot.mixed(corr_matrix,
               lower = "circle",  # Use circles in lower triangle
               upper = "number",  # Use numbers in upper triangle
               order = "FPC",     # First principal component order
               tl.col = "black",  # Label color
               tl.pos = "lt",     # Text label position
               number.cex = 0.7)  # Text size for coefficients

# 6. Filtering correlations by significance
# Calculate p-values for correlations
# First make sure we can handle NAs appropriately
cor_pmat <- try(cor.mtest(numeric_lake_data, conf.level = 0.95)$p, silent = TRUE)
if(inherits(cor_pmat, "try-error")) {
  cat("Warning: Could not calculate p-values. The correlation significance test will be skipped.\n")
  cor_pmat <- matrix(0.1, nrow = ncol(numeric_lake_data), ncol = ncol(numeric_lake_data))
  skip_sig_test <- TRUE
} else {
  skip_sig_test <- FALSE
}

# Plot with insignificant correlations crossed out (if p-values were calculated)
if(!skip_sig_test) {
  corrplot(corr_matrix,
           p.mat = cor_pmat,     # Matrix of p-values
           method = "circle",    # Use circles
           type = "lower",       # Show only lower triangle
           insig = "pch",        # Mark insignificant correlations with 'X'
           sig.level = 0.05,     # Significance level
           order = "FPC",        # First principal component order
           tl.col = "black",     # Label color
           tl.srt = 45)          # Rotate text labels
} else {
  cat("Skipping significance test plot due to issues with p-value calculation\n")
}

# 7. Analysis by lake (for one selected lake)
# Choose a lake to analyze (change this to any lake in your dataset)
selected_lake <- core_df %>% filter(lake =="Toolik")

lake_specific_data <- core_df %>%
  filter(lake == selected_lake) %>%
  select(where(is.numeric))

lake_corr_matrix <- cor(lake_specific_data, use = "pairwise.complete.obs")

# Plot correlation for the selected lake
corrplot(lake_corr_matrix,
         method = "circle",
         type = "lower",
         order = "FPC",
         tl.col = "black",
         tl.srt = 45,
         # title = paste("Correlations for lake:", selected_lake),
         mar = c(0, 0, 2, 0))

