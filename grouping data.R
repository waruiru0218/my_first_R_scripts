# =============================================================================
# grouping data
# Cathy waruiru
# 11th feb 2025

# =============================================================================
# install packages and set working directory
pacman::p_load(
  rio,       # to import data
  here,      # to locate files
  tidyverse, # to clean, handle, and plot the data (includes dplyr)
  janitor)   # adding total rows and columns

# import data
linelist <- import("data/linelist_cleaned.rds", trust = TRUE)

# show how group_by works
# group one column: group rows by unique values in the column specified
linelist %>% 
  group_by(outcome) %>% 
tally()

# group two columns
linelist %>% 
  group_by(outcome, gender) %>% 
  tally()

# =============================================================================
# grouping by date 
# usually there are dates with zero counts and usually they are not recorded
# especially if the person collecting data is not aware that that data is 
# important.
# we use complete() and seq.Date() to seq those dates. also use fill= to say 
# that those days with no counts should be included and zero assigned to them

# example without using complete: days
daily_counts <- linelist %>% 
  drop_na(date_onset) %>%        # remove that were missing date_onset
  count(date_onset)              # count number of rows per unique date
view(daily_counts)

# using complete: worked perfectly 
daily_counts <- linelist %>% 
  drop_na(date_onset) %>% 
  count(date_onset) %>% 
  complete(
    date_onset = seq.Date(
      from = min(date_onset, na.rm = TRUE),
      to = max(date_onset, na.rm = TRUE),
      by = "days"),
    fill = list(n = 0))
view(daily_counts)

# weeks: cases into weeks use lubridate: floor_date() unit=week
# Make dataset of weekly case counts
weekly_counts <- linelist %>% 
  drop_na(date_onset) %>%                 # remove cases missing date_onset
  mutate(week = lubridate::floor_date(date_onset, unit = "week")) %>%  # new column of week of onset
  count(week) %>%                         # group data by week and count rows per group
  complete(                               # ensure all days appear even if no cases
    week = seq.Date(                      # re-define date colume as daily sequence of dates
      from = min(week, na.rm=T), 
      to = max(week, na.rm=T),
      by = "week"),
    fill = list(n = 0))                   # set new filled-in rows to display 0 in column n (not NA as default) 
view(weekly_counts)

# months: 
# Make dataset of monthly case counts
monthly_counts <- linelist %>% 
  drop_na(date_onset) %>% 
  mutate(month = lubridate::floor_date(date_onset, unit = "months")) %>%  # new column, 1st of month of onset
  count(month) %>%                          # count cases by month
  complete(
    month = seq.Date(
      min(month, na.rm=T),     # include all months with no cases reported
      max(month, na.rm=T),
      by="month"),
    fill = list(n = 0))
