# =============================================================================
# simple statistical tests
# Cathy waruiru
# 17th feb 2025
# =============================================================================

# simple statistical tests from
# base R, rstatix, gtsummary ( t.test, shapiro, wilcoxon, kruskal, chi-square)
# each package above have adva and disad
# 1. base R : easy code but output is a list which can be hard to read / manipulate
# 2. rstatix: output is a df and recommended when working with grps
# 3. gtsummary: pretty table tabulations

# loading packages
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)

# loading data
# import the linelist
linelist <- import("data/linelist_cleaned.rds", trust = "TRUE")

# =============================================================================
# 1. base R
# t.test
# determine if there is a significant diffe betw means of numeric variable betw 
# 2 grps.
# syntax of vectors from same df
t.test(age_years ~ gender, data = linelist)

# numeric vectors from diffe dfs: t.test(df1$age, df2$age)

# shapiro-wilk test
# determine whether sample is from normally distributed population
shapiro.test(linelist$generation)

# wilcoxon rank sum test
# if sample is not from normally disributed sample, wilcoxon determines if 2 
# are from same distribution
wilcox.test(age_years ~ outcome, data = linelist)

# kruskal-wallis test
# is extension of wilcoxon test, used to determine diffe in distribution of 
# more than 2 samples
# if 2 samples are used, resu resemble wilcoxon resu
kruskal.test(age_years ~ outcome, linelist)

# chi-squared test
# significance differences betw categorical grps
chisq.test(linelist$gender, linelist$outcome)

# =============================================================================
# 2. rstatix package
# t.test
linelist %>% 
  t_test(age_years ~ gender)

# you can also use ~ 1 and mu= 
linelist %>% 
  t_test(age_years ~ 1, mu = 30)
# means we are testing mean of age_years agaist a fixed value which is 30
# mu = 30 is the popu mean to compare agaist: hypothetical mean

# in groups
linelist %>% 
  group_by(gender) %>% 
  t_test(age_years ~ 1, mu = 18)

# shapiro wilk test
linelist %>% 
  head(700) %>%            # first 700 rows of case linelist, for example only
  shapiro_test(age_years)

# wilcoxon rank sum test
linelist %>% 
  wilcox_test(age_years ~ gender)

# chi-squared test
# chisq_test() accepts a table so we create a table first then we remove left 
# most column then pass to chisq_test()
linelist %>% 
  tabyl(gender, outcome) %>% 
  select(-1) %>% 
  chisq_test()

# =============================================================================
# gtsummary package
# performing statistical tests using tbl_summary() from gtsummary, we add 
# add_p() where we specify the test we are doing

# chi_square
# the default statistical test for add_p() is chi_square that is why in the 
# code below we passed add_p with empty parenthesis
linelist %>% 
  select(gender, outcome) %>%    # keep variables of interest
  tbl_summary(by = outcome) %>%  # produce summary table and specify grouping variable
  add_p()                        # specify what test to perform

# t_test
# note we specified we want a t_test in add_p()
linelist %>% 
  select(age_years, outcome) %>%             # keep variables of interest
  tbl_summary(                               # produce summary table
    statistic = age_years ~ "{mean} ({sd})", # specify what statistics to show
    by = outcome) %>%                        # specify the grouping variable
  add_p(age_years ~ "t.test")                # specify what tests to perform


# wilcoxon
# comparing distribution of numeric variable in 2 grps; cat variable
# if it is non normally distributed data or comparing multiple variables, use 
# kruskal willis test
linelist %>% 
  select(age_years, gender) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = age_years ~ "{median} ({p25}, {p75})", # specify what statistic to show (this is default so could remove)
    by = gender) %>%                                  # specify the grouping variable
  add_p(age_years ~ "wilcox.test")                     # specify what test to perform (default so could leave brackets empty)
1

# kruskal-willis
linelist %>% 
select(age_years, outcome) %>%                       # keep variables of interest
  tbl_summary(                                         # produce summary table
    statistic = age_years ~ "{median} ({p25}, {p75})", # specify what statistic to show (default, so could remove)
    by = outcome) %>%                                  # specify the grouping variable
  add_p(age_years ~ "kruskal.test")                    # specify what test to perform


# =============================================================================
# correlations
# tidyverse corrr package enables you get correlation betw your numeric 
# variables using: pearsons, kendall tau, spearman rho 
# corrr creates a table, and used rplot() function to plot graph of correlation

correlation_tab <- linelist %>% 
  select(generation, age, ct_blood, days_onset_hosp, wt_kg, ht_cm) %>%   # keep numeric variables of interest
  correlate()      # create correlation table (using default pearson)

correlation_tab    # print

# remove duplicate entries (the table above is mirrored) 
correlation_tab <- correlation_tab %>% 
  shave()

# view correlation table 
correlation_tab

# plot correlations 
rplot(correlation_tab)

# =============================================================================