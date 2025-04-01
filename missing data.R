# =============================================================================
# Missing data
# Cathy waruiru
# 19th Feb 2025
# =============================================================================

# loading packages and set working directory
pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar      # assess and visualize missingness
)
install.packages("mice")

# import data
linelist <- import("data/linelist_cleaned.rds", trust = TRUE)

# =============================================================================
# versions of R
# when using case_when and if_else functions, you have to specify the NA vector 
# type to be of same class as the other vector since a vector can only be of 
# one class
# in case_when() the values on right side should be of same class
# use NA for dates/logical, NA_character for char vectors , NA_real for real
# vectors/double
linelist <- linelist %>% 
  
  # Create new "age_years" column from "age" column
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age is given in years, assign original value
    age_unit == "months" ~ age/12,    # if age is given in months, divide by 12
    is.na(age_unit)      ~ age,       # if age UNIT is missing, assume years
    TRUE                 ~ NA_real_)) # any other circumstance, assign missing
linelist$age_years

# NULL repre a statement that is neither true nor false . NULL is usually 
# ignored in a vector
cathy <- c(5, NULL, 10, 70)
cathy

# NaN repre impossible values eg divide 0 by 0
# Inf repre endless values/ infinite values : 5/0

# useful functions
# is.na and !is.na : use the former for missing values and the latter for
# non_missing values
is.na(linelist$gender)
!is.na(linelist$gender)
sum(!is.na(linelist$gender))

# na.omit()
# base R function for dropping NA
na.omit(linelist$gender
        )

# drp_na
# tidyr function for dropping NA: use tidy select helpers to specify columns
linelist %>% 
  drop_na(case_id, gender, age)

# =============================================================================
# assess missingness in a df : naniar package
# counts and % NA values and % complete values in a df
n_miss(linelist)
pct_miss(linelist)
pct_complete(linelist)

# % of rows with NA values
pct_miss_case(linelist)

# counts and % of rows complete : no missing 
n_complete(linelist)
n_complete_row(linelist)
pct_complete_case(linelist)

# visualizing missingness: naniar package :: gg_miss_var()
# use gg_miss_var which will show you missing counts/% of NA in every column
# use facet = to specify column
gg_miss_var(linelist) # counts of NA in every column

# specify colum and use %
linelist %>% 
  gg_miss_var(show_pct = TRUE, facet = gender)

linelist %>% 
  gg_miss_var(show_pct = TRUE, facet = age_cat)

# shadow columns: another way to visualize missingness in a df
# we use bind_shadow() from naniar that creates NA/not NA columns of every
# column in df and adds it to the origi df
shadowed_linelist <- linelist %>% 
  bind_shadow()
names(shadowed_linelist)

view(shadowed_linelist)

# you can use shadow column to plot prop of values missing by another column
ggplot(data = shadowed_linelist,          # data frame with shadow columns
       mapping = aes(x = date_hospitalisation, # numeric or date column
                     colour = age_NA)) + # shadow column of interest
  geom_density()                          # plots the density curves

# using shadow column to stratify statistics
linelist %>%
  bind_shadow() %>%                # create the shows cols
  group_by(date_outcome_NA) %>%    # shadow col for stratifying
  summarise(across(
    .cols = age_years,             # variable of interest for calculations
    .fns = list("mean" = mean,     # stats to calculate
                "sd" = sd,
                "var" = var,
                "min" = min,
                "max" = max),  
    na.rm = TRUE))                 # other arguments for the stat calculations

# =============================================================================
# using dat with missing values
# use filter(is.na()) or drpo_na()
nrow(linelist)
linelist %>% 
  drop_na() %>% 
  nrow()

linelist %>% 
  drop_na(contains("date")) %>% 
  nrow()

linelist %>% 
  filter(is.na(outcome)) %>% 
  nrow()

# handling NA in factors
library(forcats)

linelist <- linelist %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "Missing"))

levels(linelist$gender)

# =============================================================================
# Imputation
