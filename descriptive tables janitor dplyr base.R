# =============================================================================
# descriptive tables
# 14th feb 2025
# Cathy waruiru
# =============================================================================

# loading packages to be used and setting working directory
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)
# import data
linelist <- import("data/linelist_cleaned.rds", trust = TRUE)

# =============================================================================
# skimr package: a detailed overview of our linelist df displayed with summary
# stats of every column(class)
skim(linelist)

# summary statistics 
# 1. summary() from base r: return summary stats from numeric columns only
summary(linelist$wt_kg)

# 2. get_summary_stats : returns summary stats of multiple column in df format
linelist %>% 
  get_summary_stats(
    age, wt_kg, ht_cm, ct_blood, temp, bmi, type = "common"
  )

# =============================================================================
# janitor package
# tabyl() : returns counts, prop, percent: raw and valid(if NA are present)
linelist %>% 
  tabyl(age_cat) # has missing values so 2 percent

linelist %>% 
  tabyl(bmi) # no missing so 1 percent

# cross tabulation :add column to tabyl
linelist %>% 
  tabyl(age_cat,gender)

# adorning
linelist %>%               # case linelist
  tabyl(age_cat) %>%       # tabulate counts and proportions by age category
  adorn_pct_formatting() %>%    # convert proportions to percents
  adorn_totals(where = "both", name = "total")

# add row counts and their % and add row and col names
linelist %>%                                  # case linelist
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

linelist %>%                                  # case linelist
  tabyl(age_cat, gender) %>%                  # cross-tabulate counts
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "row") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")

# using freqtables to diplay a pretty image which can be exported
linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% # for col totals
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()          # format to one line per row 

# lets do it again: row totals
linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "row") %>% 
  adorn_percentages(denominator = "row") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "rear") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()          # format to one line per row 


# adding janitors cool functions in other tables
linelist %>% 
  count(hospital)

linelist %>% 
  count(hospital) %>% 
  adorn_totals()

# saving the pretty table: using freqtable functions
# can be saved as  save_as_html(), save_as_word(), save_as_ppt(), 
# and save_as_image()
linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%                     # convert to image
  flextable::autofit() %>%                       # ensure only one line per row
  flextable::save_as_docx(path = "tabyl.docx")   # save as Word document to filepath

# save as image
linelist %>%
  tabyl(age_cat, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%                     # convert to image
  flextable::autofit() %>%                       # ensure only one line per row
  flextable::save_as_image(path = "tabyl.png")   # save as Word document to filepath

# =============================================================================
# dplyr package
# useful, powerful package in data management
# summarise() and count()
# getting counts
# 1. use summarise
linelist %>% 
  summarise(n_cols = n())

linelist %>% 
  group_by(age_cat) %>%     
  summarise(n_rows = n())   

# 2. using count
linelist %>% 
  count(age_cat, gender)

# calculating prop and percent : we added percent()
age_summary <- linelist %>% 
  count(age_cat) %>%                     # group and count by gender (produces "n" column)
  mutate(                                # create percent of column - note the denominator
    percent = scales::percent(n / sum(n))) 

# print
age_summary

# another example
# first we group our df by outcome then we count age_cat : so now count() will
# count prop of outcome age_cat combination as groups then it will ungroup 
# age_cat and group outcome remains.
# then calculate prop percent as outcome is still a group so denominator is 
# sum of group outcome
age_by_outcome <- linelist %>%                  # begin with linelist
  group_by(outcome) %>%                         # group by outcome 
  count(age_cat) %>%                            # group and count by age_cat, 
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - 

# plotting
# plotting data with a long format is easy coz this long format is naturally
# accepted by ggplot
linelist %>%                      # begin with linelist
  count(age_cat, outcome) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome,              # map outcome to x-axis
      fill = age_cat,           # map age_cat to the fill
      y = n))                   # map the counts column `n` to the height

# summary statistics
# this is my favorite part coz its very easy # always remember na.rm = T
# you can even write a fuction to avoid repetition
# lets calculate summary stat of hospital groups
summary_table <- linelist %>%                                        # begin with linelist, save out as new object
  group_by(hospital) %>%                                             # group all calculations by hospital
  summarise(                                                         # only the below summary columns will be returned
    cases       = n(),                                                # number of rows per group
    delay_max   = max(days_onset_hosp, na.rm = T),                    # max delay
    delay_mean  = round(mean(days_onset_hosp, na.rm=T), digits = 1),  # mean delay, rounded
    delay_sd    = round(sd(days_onset_hosp, na.rm = T), digits = 1),  # standard deviation of delays, rounded
    delay_3     = sum(days_onset_hosp >= 3, na.rm = T),               # number of rows with delay of 3 or more days
    pct_delay_3 = scales::percent(delay_3 / cases)                    # convert previously-defined delay column to percent 
  )

summary_table  # print

# you can even do for age_cat and even other numeric variables and create a
# function for them 

# conditional statistics 

# You may want to return conditional statistics - e.g. the maximum of rows that 
# meet certain criteria. 
linelist %>% 
  group_by(hospital) %>% 
  summarise(
    max_fvr = max(fever == "yes", na.rm = T),
    max_no = max(fever == "no", na.rm = T)
  )

# lets now find max temp for patients with or without fever
# we use [] coz we are referring to two columns at the same time
linelist %>% 
  group_by(hospital) %>% 
  summarise(
    max_temp_fvr = max(temp[fever == "yes"], na.rm = T),
    max_temp_no = max(temp[fever == "no"], na.rm = T)
  )

# another example of sum ofpple with fever using conditional operations
linelist %>%
  group_by(hospital) %>%
  summarize(sick_count = sum(fever == "yes", na.rm = TRUE))

# glueing together : combining
# we use str_glue which take values from selected columns and combines them 
# into one column
# str_glue is recommended than other functions like unite(), paste0() coz it
# is more flexible and have a simple syntax than paste0()
summary_table %>% 
  mutate(delay = str_glue("{delay_mean} ({delay_sd})")) %>%  # combine and format other values
  select(-c(delay_mean, delay_sd)) %>%                       # remove two old columns   
  adorn_totals(where = "row") %>%                            # add total row
  select(                                                    # order and rename cols
    "Hospital Name"   = hospital,
    "Cases"           = cases,
    "Max delay"       = delay_max,
    "Mean (sd)"       = delay,
    "Delay 3+ days"   = delay_3,
    "% delay 3+ days" = pct_delay_3
  )
# calculating percentiles
# can be the default ones or you can specify using probs
# 1. default
linelist %>% 
  summarise(ht_cm_percentiles = quantile(ht_cm, na.rm = TRUE))

# 2. specify 
linelist %>% 
  summarise(
    ht_cm_percentiles = quantile(
      ht_cm,
      probs = c(0.2, 0.5, 0.75),
      na.rm = TRUE
    )
  )

# get_summary_stats() does all these fine things that summarise does 
# you just need to specify type of stat you need : in our case quantile
linelist %>% 
  group_by(hospital) %>% 
  rstatix::get_summary_stats(age, type = "quantile")

# on ungrouped : linelist df
linelist %>% 
  rstatix::get_summary_stats(age, type = "quantile")

# across() : used to calculate summary statistics across multiple columns
# you can use a list() to supply name = function pair to .fns=
# you can use tidy select modifiers to pass columns to across()
# across used inside dplyr verbs
linelist %>% 
  group_by(outcome) %>% 
  summarise(across(.cols = c(age_years, temp, wt_kg, ht_cm), # columns
                   .fns = list("mean" = mean, "sd" = sd),    # multiple functions 
                   na.rm=T))                                 # extra arguments


linelist %>% 
  group_by(age_cat) %>% 
  summarise(across(
    .cols = where(is.numeric),  # all numeric columns in the data frame
    .fns = mean,
    na.rm=T))

# =============================================================================
# gtsummary tbl_summary() allows you to print your table in a pretty way
# default return of tbl_summary()
linelist %>% 
  select(age_years, gender, outcome, fever, temp, hospital) %>%  # keep only the columns of interest
  tbl_summary()                                                  # default

# with adjustments
linelist <- linelist %>%
  mutate(outcome = replace_na(outcome, "Unknown"))  # Replace missing outcome values


linelist %>% 
  select(age_years, gender, outcome, fever, temp, hospital) %>% # keep only columns of interest
  tbl_summary(     
    by = outcome,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} / {N} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      outcome   ~ "Outcome",                           
      age_years ~ "Age (years)",
      gender    ~ "Gender",
      temp      ~ "Temperature",
      hospital  ~ "Hospital"),
    missing_text = "Missing" # how missing values should display
  )
view(linelist$outcome)
sum(is.na(linelist$outcome))
glimpse(linelist)  # Check column names and types
glimpse(linelist)  # Check column names and types

linelist %>%
  select(age_years, gender, outcome, fever, temp, hospital) %>%
  tbl_summary(
    by = fever,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      outcome   ~ "Outcome",
      age_years ~ "Age (years)",
      gender    ~ "Gender",
      temp      ~ "Temperature",
      hospital  ~ "Hospital"),
    missing_text = "Missing"
)
