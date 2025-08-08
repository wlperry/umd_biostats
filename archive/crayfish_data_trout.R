# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.3.22
library(tidyverse)
library(readxl)
library(janitor)
library(corrr)  # For correlation analysis
library(corrplot)  # For visualization

# File path - update this to the location of your file
file_path <- "data/trout_lake_crayfish_1997.xlsx"

# Get all sheet names from the Excel file
sheet_names <- excel_sheets(file_path)

# OPTION 1: Combine all sheets into a single dataframe with map_df
# Best when all sheets have similar structure
cray_df <- sheet_names %>%
  map_df(function(sheet) {
    read_excel(file_path, sheet = sheet) %>%
      mutate(source_sheet = sheet) %>% clean_names()
  })

write_csv(cray_df, "data/trout_lake_crayfish.csv")

# cray_df <- read_excel("data/trout_lake_crayfish_1997.xlsx", sheet="1991") %>% 
#   clean_names()

cray_df <- cray_df %>% 
  filter(species %in% c("OR", "OP", "OV")) %>% 
  select(year, lake, trap, species, sex) 
  # filter(trap < 40)

cray_df <- cray_df %>% 
  group_by(year, lake, trap, species) %>% 
  summarise(n_caught = sum(!is.na(species))) 

cray_wide_df <- cray_df %>% 
  pivot_wider(names_from = species,
              values_from = n_caught)

cray_wide_df <-cray_wide_df %>% 
  mutate(OR = ifelse(is.na(OR), 0, OR)) %>% 
  mutate(OP = ifelse(is.na(OP), 0, OP)) %>% 
  mutate(OV = ifelse(is.na(OV), 0, OV)) 

cray_wide_df %>% ggplot(aes(OR, OP, color=trap))+
  geom_point() +
  geom_smooth(method="lm")

