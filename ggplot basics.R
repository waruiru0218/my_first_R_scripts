# =============================================================================
# ggplot basics
# Cathy waruiru
# 24th Feb 2025
# =============================================================================

# ggplot2 is the most popular data visualisation package . its function ggplot()
# is its core and is used to create plots

# load packages 
pacman::p_load(
  tidyverse,      # includes ggplot2 and other data management tools
  janitor,        # cleaning and summary tables
  ggforce,        # ggplot extras
  rio,            # import/export
  here,           # file locator
  stringr         # working with characters   
)
# import data
linelist <- rio::import("data/linelist_cleaned.rds", trust = "TRUE")

# =============================================================================
# when preparing data for plotting, there are several cleanings that should be 
# done:
# convert NA in char column to "unknown" char strings
# convert columns to factor so each have ordinal levels 
# pivoting df to longer format

# =============================================================================
# order of the code for creating plots is as follows:
# ggplot(), geomes, design elements (labels, theme,)
# 1. ggplot() is the opening command of ggplot2 and it creates a blank canvas 
# upon which to add layers . ggplot() ends with a plus symbol which opens way 
# for further layers to be added
# data is also specified here

# 2. geomes : we need to create geometries/ shapes from our data thus blank 
# canvas alone is not sufficient
# geomes are so many check in help 

# 3. mapping data to the plot
# you must tell the geoms what to use to map/ assign columns in your data to 
# components of the plot. usually x and y axis.

# example 
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+
  geom_point()

# plot aethetics : visual property f plotted data 
# here they refer to the data being plotted geom/shapes not sorrounding displays 
# such as title, axis labels, background color : these are adjusted in theme()
# these plot aesthetic can be assigned values in 2 ways:

# 1. set to a static value : aesthetics outside mapping = 
# scatterplot
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  # set data and axes mapping
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)         # set static point aesthetics

# histogram
ggplot(data = linelist, mapping = aes(x = age))+       # set data and axes
  geom_histogram(              # display histogram
    binwidth = 7,                # width of bins
    color = "red",               # bin line color
    fill = "blue",               # bin interior color
    alpha = 0.1)                 # bin transparency

# 2. scaled to column values
# here you scale aesthetic to by values in a column
ggplot(data = linelist,
       mapping = aes(
         x = age,
         y = wt_kg,
         colour = age,
       ))+
  geom_point()


# groups
# here you assign the grouping column to appro plot aesthetic within mapping = aes()
# mapping = can be within ggplot() or within geoms
# within ggplot
ggplot(data = linelist, 
       mapping = aes(x = age, y = wt_kg, colour = gender))+
  geom_point()

# within geom
ggplot(data = linelist,
       mapping = aes(x = age, y = wt_kg))+
  geom_point(
    mapping = aes(colour = gender)
  )

# =============================================================================
# import data to use 

malaria_data <- import("data/malaria_facility_count_data.rds", trust = "TRUE") %>% 
  dplyr::select(-submitted_date, -Province, -newid)

# facet/ small multiples
# facets are used to split one plot into a multi panel figure with one panel per 
# grp of data.

# 1. facet_wrap()
# shows differ panels for each level of a single variable 
# you use a ~ infront of the column you want to facet
ggplot(malaria_data, aes(x = data_date, y = malaria_tot))+
  geom_col(width = 1, fill = "darkred")+
  theme_minimal()+
  labs(
    x = "Date of report",
    y = "Malaria cases", 
    title = "Malaria cases by district")+
  facet_wrap(~District)


# 2. facet_grid()
# used when you want to facet two columns/ variables
# use this syntax rows ~ columns
# reme first our data should be in long format 
malaria_age <- malaria_data %>%
  dplyr::select(-malaria_tot) %>% 
  pivot_longer(
    cols = c(starts_with("malaria_rdt_")),  # choose columns to pivot longer
    names_to = "age_group",      # column names become age group
    values_to = "num_cases"      # values to a single column (num_cases)
  ) %>%
  mutate(
    age_group = str_replace(age_group, "malaria_rdt_", ""),
    age_group = forcats::fct_relevel(age_group, "5-14", after = 1))

# lets facet 
ggplot(malaria_age, aes(x = data_date, y = num_cases)) +
  geom_col(fill = "darkred", width = 1) +
  theme_minimal()+
  labs(
    x = "Date of report",
    y = "Malaria cases",
    title = "Malaria cases by district and age group"
  ) +
  facet_grid(District ~ age_group)

# to specify scales use scales = and set to free/ free_x, free_y
# Free y-axis
ggplot(malaria_data, aes(x = data_date, y = malaria_tot)) +
  geom_col(width = 1, fill = "darkred") +       # plot the count data as columns
  theme_minimal()+                              # simplify the background panels
  labs(                                         # add plot labels, title, etc.
    x = "Date of report",
    y = "Malaria cases",
    title = "Malaria cases by district - 'free' x and y axes") +
  facet_wrap(~District, scales = "free")        # the facets are created

ggplot(malaria_age, aes(x = data_date, y = num_cases)) +
  geom_col(fill = "darkred", width = 1) +
  theme_minimal()+
  labs(
    x = "Date of report",
    y = "Malaria cases",
    title = "Malaria cases by district and age group"
  ) +
  facet_grid(District ~ age_group, scales = "free")

# =============================================================================
# labels 
# you use labs() to adjust your labels or add labels: adjusting your x and y
# axis names, adding tittle/ subtittle, caption, and plot aesthetic used to 
# create legend
age_by_wt <- ggplot(
  data = linelist,   # set data
  mapping = aes(     # map aesthetics to column values
    x = age,           # map x-axis to age            
    y = wt_kg,         # map y-axis to weight
    color = age))+     # map color to age
  geom_point()+           # display data as points
  labs(
    title = "Age and weight distribution",
    subtitle = "Fictional Ebola outbreak, 2014",
    x = "Age in years",
    y = "Weight in kilos",
    color = "Age",
    caption = stringr::str_glue("Data as of {max(linelist$date_hospitalisation, na.rm=T)}"))

age_by_wt

# other examples
import("data/linelist_cleaned.rds", trust = "TRUE")
outcomes <- linelist %>% 
  drop_na(outcome) %>% 
  tabyl(outcome)
outcomes

ggplot(outcomes)+
  geom_col(aes(
    x = outcome,
    y = percent
  ))

# another example with width not adjusted
# A) Outcomes in all cases
ggplot(linelist %>% drop_na(outcome)) + 
  geom_bar(aes(y = fct_rev(hospital))) +
  theme_minimal()+
  labs(title = "A) Number of cases by hospital",
       y = "Hospital")


# B) Outcomes in all cases by hosptial
ggplot(linelist %>% drop_na(outcome)) + 
  geom_bar(aes(y = fct_rev(hospital), fill = outcome)) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "B) Number of recovered and dead Ebola cases, by hospital",
       y = "Hospital")

# width adjusted
# A) Outcomes in all cases
ggplot(linelist %>% drop_na(outcome)) + 
  geom_bar(aes(y = fct_rev(hospital)), width = 0.7) +
  theme_minimal()+
  labs(title = "A) Number of cases by hospital",
       y = "Hospital")


# B) Outcomes in all cases by hosptial
ggplot(linelist %>% drop_na(outcome)) + 
  geom_bar(aes(y = fct_rev(hospital), fill = outcome), width = 0.7) +
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(title = "B) Number of recovered and dead Ebola cases, by hospital",
       y = "Hospital")
