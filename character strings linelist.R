# =============================================================================
# character strings: working with character strings (stringr package)
# cathy waruiru
# 10th feb 2025
#==============================================================================

# Combine, order, split, arrange - str_c(), str_glue(), str_order(), str_split()
# Clean and standardise
# Adjust length - str_pad(), str_trunc(), str_wrap()
#Change case - str_to_upper(), str_to_title(), str_to_lower(), str_to_sentence()
# Evaluate and extract by position - str_length(), str_sub(), word()
# Patterns:
   # Detect and locate - str_detect(), str_subset(), str_match(), str_extract()
  # Modify and replace - str_sub(), str_replace_all()
# Regular expressions (“regex”)
#==============================================================================

# loading packages
# install/load packages
pacman::p_load(
  stringr,    # many functions for handling strings
  tidyverse,  # for optional data manipulation
  tools)      # alternative for converting to title case

# importing data
linelist <- rio::import("linelist_cleaned.rds")
head(linelist)
