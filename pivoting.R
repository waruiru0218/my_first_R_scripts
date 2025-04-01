# =============================================================================
# pivoting 
# Cathy waruiru
# 11th feb 2025
# =============================================================================

# loading packages
pacman::p_load(
  rio,          # File import
  here,         # File locator
  kableExtra,   # Build and manipulate complex tables
  tidyverse)    # data management + ggplot2 graphics

# importing data & setting working directory
# Import data
count_data <- import("data/malaria_facility_count_data.rds", trust = TRUE)

# =============================================================================
# usually, we pivot_ longer / wider. it depends with the data we are working on
# and whatever we want to do with the data.
# 1. longer format is recommended for plotting, time varying variables and most 
# tidyr & dplyr fuctions work well when data is in longer format. 
# most longitudinal data prefer data arranged in this format.

# 2. wider format is usually good when working with time invarying variables:
# sex. also very good format for display purposes for easier reading and 
# presesntation. anova and manova also works well with data in wider format.

# 3. pivoting data tries to meet the guidelines of tidy data(hadley wickham) :
# each variable should have its own column, each observation should have its 
# own row and each value should have its own cell

# =============================================================================
view(count_data)
# this is wider format : when you view it, the columns for age groups does not 
# contain age group rather they contain values for those age group 
# lets pivot_longer
# provide column with a tidyselect helper function
count_data %>% 
  pivot_longer(
    cols = starts_with("malaria_")
  )
# lets name our new columns: age_group and count
df_long <- 
  count_data %>% 
  pivot_longer(
    cols = c(`malaria_rdt_0-4`, `malaria_rdt_5-14`, malaria_rdt_15),
    names_to = "age_group",
    names_prefix = "malaria_rdt_",
    values_to = "counts"
  )
view(df_long)
 
# lets try plotting: perfecto
ggplot(data = df_long) +
  geom_col(
    mapping = aes(x = data_date, y = counts, fill = age_group),
    width = 1
  )

# =============================================================================
# pivot wider
# import your dataset
linelist <- import("data/linelist_cleaned.xlsx")
view(linelist)
df_wide <- 
  linelist %>% 
  count(age_cat, gender)

df_wide

# this format longer, it is not easy to view or for presesntation. 
# lets pivot wider
df_wide <- df_wide %>% 
  pivot_wider(
    id_cols = age_cat,
    names_from = gender,
    values_from = n
  )
df_wide
