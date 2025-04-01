# =============================================================================
# working with dates
# Cathy waruiru
# 8th feb 2025
# =============================================================================

# loading necessary packages
# Checks if package is installed, installs if necessary, and loads package for 
# current session

pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  parsedate,  # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  here,       # file management
  rio,        # data import/export
  tidyverse)  # data management and visualization  

# import data
linelist <- rio::import("linelist_cleaned.xlsx")
head(linelist)

# change class of date columns to date class 
# first check their class 
class(linelist$date_onset)
class(linelist$date_hospitalisation)

# their class is posixt so we first change them to class char then to class date
linelist <- linelist %>% 
  mutate(across(contains("date"), as.character))
linelist <- linelist %>% 
  mutate(across(contains("date"), as.Date))

# most of the other functions on working with dates have been written down in 
# r4epi notes(base r) and now on notes on epihandbook. refer there

# date intervals
pacman::p_load(lubridate, tidyverse)   # load packages

linelist <- linelist %>%
  
  # filter out all cases without onset in march
  filter(month(date_onset) == 3) %>%
  
  # find the difference in days between onset and hospitalisation
  mutate(days_onset_to_hosp = date_hospitalisation - date_onset)
head(linelist$days_onset_to_hosp)

