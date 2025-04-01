#==============================================================================
# cleaning raw data (linelist raw data) my very first script
# cathy waruiru
# 7th feb 2025
# =============================================================================

# loading packages we will use
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

# importing our raw linelist data
linelist <- rio::import("data/linelist_raw.xlsx")

# we use skim() from skimr package to vie entire data frame : columns 
# summarised by class types
skimr::skim(linelist)

# view column names to see if they are succinct and informative names(syntax)
names(linelist)

# clean names using clean_names() from janitor package: automatic cleaning
linelist <- janitor::clean_names(linelist)
names(linelist)

# rename some column names using rename() from dplyr package: new=old
linelist <- linelist %>% 
  rename(date_infection = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome = date_of_outcome)
names(linelist)

# select(): used when you want to select certain columns to retain or drop(-) 
# usually used in conjunction with tidy select modifiers 
# lets drop some columns
linelist <- linelist %>% 
  select(-c(row_num, merged_header, x28))

# de-duplication : remove duplicates: complete duplicated rows using distinct()
linelist <- linelist %>% 
  distinct()
head(linelist)

# adding new columns using mutate() from dplyr package
linelist <- linelist %>% 
  mutate(
    bmi = wt_kg / (ht_cm/100)^2,
    across(contains("date"), as.Date),
    
  )
linelist <- linelist %>% 
  mutate(
    generation = as.numeric(generation),
    age = as.numeric(age))
# correct spelling on the hospital column using recode()
linelist <- linelist %>% 
  mutate(hospital = recode(hospital,
                           # for reference: OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  ))
table(linelist$hospital, useNA = "always")

# another scenario of recoding values
# Example: change gender of one specific observation to "Female" 
linelist <- linelist %>% 
  mutate(gender = replace(gender, case_id == "2195", "Female"))
# or we can also use base r functions: in column gender, every row whose value
# is 2195 in case_id column is changed to female (column gender)
# base r method
linelist$gender[linelist$case_id == "2195"] <- "Female"

# creating new column: we now use case_when() inside mutate function: sequence
linelist <- linelist %>% 
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age unit is years
    age_unit == "months" ~ age/12,    # if age unit is months, divide age by 12
    is.na(age_unit)      ~ age))      # if age unit is missing, assume years
# any other circumstance, assign NA (missing)

# another example using case_when()
linelist <- linelist %>% 
  mutate(case_status = case_when(
    
    # if patient had lab test and it is positive,
    # then they are marked as a confirmed case 
    ct_blood < 20                   ~ "Confirmed",
    
    # given that a patient does not have a positive lab result,
    # if patient has a "source" (epidemiological link) AND has fever, 
    # then they are marked as a suspect case
    !is.na(source) & fever == "yes" ~ "Suspect",
    
    # any other patient not addressed above 
    # is marked for follow up
    TRUE                            ~ "To investigate"))
view(linelist)

# replace NA values in column hospital to "missing"
linelist <- linelist %>% 
  mutate(hospital = replace_na(hospital, "Missing"))

# creating categories from numerical columns eg: age 
# we will use age_categories() from epikit package. it can create categories 
# from any numeric column not just age
#check the class of the linelist variable age
class(linelist$age_years)
# Simple example
################
pacman::p_load(epikit)                    # load package

linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(             # create new column
      age_years,                            # numeric column to make groups from
      breakers = c(0, 5, 10, 15, 20,        # break points
                   30, 40, 50, 60, 70)))

# show table
table(linelist$age_cat, useNA = "always")

# add a ceiling argu so that all values above the ceiling become NA
# With ceiling set to TRUE
##########################
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70),
      ceiling = TRUE)) # 70 is ceiling, all above become NA

# show table
table(linelist$age_cat, useNA = "always")# With ceiling set to TRUE
##########################
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70),
      ceiling = TRUE)) # 70 is ceiling, all above become NA

# show table
table(linelist$age_cat, useNA = "always")

# you can also use lower, upper and by argu
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      lower = 0,
      upper = 100,
      by = 10))
#show table
table(linelist$age_cat, useNA = "always")

# examining : cross checking
# Cross tabulation of the numeric and category columns. 
table("Numeric Values" = linelist$age_years,   # names specified in table for clarity.
      "Categories"     = linelist$age_cat,
      useNA = "always")                        # don't forget to examine NA values

linelist <- linelist %>% 
mutate(
  # age categories: custom
  age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70)),
  
  # age categories: 0 to 85 by 5s
  age_cat5 = epikit::age_categories(age_years, breakers = seq(0, 85, 5)))

# filtering rows
linelist <- linelist %>% 
filter(
  # keep only rows where case_id is not missing
  !is.na(case_id),  
  
  # also filter to keep only the second outbreak
  date_onset > as.Date("2013-06-01") | (is.na(date_onset) & !hospital %in% c("Hospital A", "Hospital B")))

# row-wise operations: this is where you want to perform your analysis by rows
# and not column but you need to ungroup() after the operation.
# we may want to use tidyselect modifiers but since we are not using dplyr 
# functions like select() we will have to modify our code by specifying our 
# columns using dplyr c_across()
linelist %>%
  rowwise() %>%
  mutate(num_NA_dates = sum(is.na(c_across(contains("date"))))) %>% 
  ungroup() %>% 
  select(num_NA_dates, contains("date")) # for display
