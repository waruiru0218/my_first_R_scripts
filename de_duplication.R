# =============================================================================
# de-duplication
# Cathy waruiru
# 12th feb 2025
# =============================================================================

# loading functions
pacman::p_load(
  tidyverse,   # deduplication, grouping, and slicing functions
  janitor,     # function for reviewing duplicates
  stringr)      # for string searches, can be used in "rolling-up" values

# simulating data
obs <- data.frame(
  recordID  = c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
  personID  = c(1,1,2,2,3,2,4,5,6,7,2,1,3,3,4,5,5,7,8),
  name      = c("adam", "adam", "amrish", "amrish", "mariah", "amrish", "nikhil", "brian", "smita", "raquel", "amrish",
                "adam", "mariah", "mariah", "nikhil", "brian", "brian", "raquel", "natalie"),
  date      = c("1/1/2020", "1/1/2020", "2/1/2020", "2/1/2020", "5/1/2020", "5/1/2020", "5/1/2020", "5/1/2020", "5/1/2020","5/1/2020", "2/1/2020",
                "5/1/2020", "6/1/2020", "6/1/2020", "6/1/2020", "6/1/2020", "7/1/2020", "7/1/2020", "7/1/2020"),
  time      = c("09:00", "09:00", "14:20", "14:20", "12:00", "16:10", "13:01", "15:20", "14:20", "12:30", "10:24",
                "09:40", "07:25", "08:32", "15:36", "15:31", "07:59", "11:13", "17:12"),
  encounter = c(1,1,1,1,1,3,1,1,1,1,2,
                2,2,3,2,2,3,2,1),
  purpose   = c("contact", "contact", "contact", "contact", "case", "case", "contact", "contact", "contact", "contact", "contact",
                "case", "contact", "contact", "contact", "contact", "case", "contact", "case"),
  symptoms_ever = c(NA, NA, "No", "No", "No", "Yes", "Yes", "No", "Yes", NA, "Yes",
                    "No", "No", "No", "Yes", "Yes", "No","No", "No")) %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y"))
obs

# difference between tabyl and count : tabyl(janitor) will lay out the table 
# while count(dplyr)will group and count 
obs %>% 
  tabyl(name, purpose)

obs %>% 
  dplyr::count(purpose)

# several rows are complete duplicates while others are partial duplicates
# =============================================================================

# to view rows with complete100% duplicates use janitor::get_dupes()
# this function returns only rows with 100% duplicates and adds a new column
# dupe_count at the right
obs %>% 
  get_dupes()

# remember record id was computer generated so lets ignore that column and see
# duplicated rows
obs %>% 
  get_dupes(-recordID)

# you can also specify columns you want to see if there are duplicates
obs %>% 
  get_dupes(name, purpose)

# =============================================================================
# keep only unique rows using distinct () # only first row of duplicates is kept
# 2 rows droped adam and amrish
obs %>% 
  dplyr::distinct(across(-recordID),
                  .keep_all = TRUE) # keep all columns even the ones not evaluated

# checking for duplicates using only specific columns: 7 rows dropped
obs %>% 
  distinct(name, purpose, .keep_all = TRUE)

# =============================================================================
# slicing # use slice()
obs %>% 
  slice(4) # give 4th row only

obs %>% 
  slice(1:5) # give row 1 through 5

obs %>% 
  slice_head(n = 5) # return first 5 rows

# slice_tail returns from the last rows

obs %>% 
  slice_max(encounter, n = 1) # return rows with highest encounter
# n specifies how many rows you want returned : those with max encounter


