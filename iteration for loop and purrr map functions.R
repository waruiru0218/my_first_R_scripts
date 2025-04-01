# =============================================================================
# iteration (for loop and purrr)
# Cathy waruiru
# 13th feb 2025
# =============================================================================

# load packages
pacman::p_load(
  rio,         # import/export
  here,        # file locator
  purrr,       # iteration
  grates,      # scales in ggplot
  tidyverse    # data management and visualization
)

# import data and set working directory
linelist <- import("data/linelist_cleaned.rds", trust = TRUE)

# =============================================================================
# for loop 
# create container to store results - a character vector
cases_demographics <- vector(mode = "character", length = nrow(linelist))

# the for loop
for (i in 1:nrow(linelist)){
  
  # OPERATIONS
  # extract values from linelist for row i, using brackets for indexing
  row_gender  <- linelist$gender[[i]]
  row_age     <- linelist$age_years[[i]]    # don't forget to index!
  
  # combine gender-age and store in container vector at indexed location
  cases_demographics[[i]] <- str_c(row_gender, row_age, sep = ",") 
  
}  # end for loop


# display first 10 rows of container
head(cases_demographics, 10)

# forloop components are:
# 1. container to contains output
# can be a vector , df, or list (empty) created before the for loop
# vector(mode = "character, double, logical", length = ncol() nrows())
# data.frame(matrix(ncol = , nrow =))
# list use vector( mode = "list", length = )

# 2. sequence : item in vector
# for(i in vector)
# you can use seq_along() to specify vector
# for(i in seq_along(df_xyz))

# 3. operations (for loop body): code inside{} 
# this code is run for evert item in the sequence

# =============================================================================
# looping plots
# creating an epiculve
# create 'incidence' object
outbreak <- incidence2::incidence(   
  x = linelist,                   # dataframe - complete linelist
  date_index = "date_onset",        # date column
  interval = "week",              # aggregate counts weekly
  groups = "gender")               # group values by gender
#na_as_group = TRUE)             # missing gender is own group

# tracer la courbe d'épidémie
ggplot(outbreak, # nom de l'objet d'incidence
       aes(x = date_index, #aesthetiques et axes
           y = count, 
           fill = gender), # Fill colour of bars by gender
       color = "black"      # Contour colour of bars
) +  
  geom_col() + 
  facet_wrap(~gender) +
  theme_bw() + 
  labs(title = "Outbreak of all cases", #titre
       x = "Counts", 
       y = "Date", 
       fill = "Gender", 
       color = "Gender")

# To produce a separate plot for each hospital’s cases, we can put this 
# epicurve code within a for loop.

# make vector of the hospital names
hospital_names <- unique(linelist$hospital)

# for each name ("hosp") in hospital_names, create and print the epi curve
for (hosp in hospital_names) {
  
  # create incidence object specific to the current hospital
  outbreak_hosp <- incidence2::incidence(
    x = linelist %>% filter(hospital == hosp),   # linelist is filtered to the current hospital
    date_index = "date_onset",
    interval = "week", 
    groups = "gender"#,
    #na_as_group = TRUE
  )
  
  plot_hosp <- ggplot(outbreak_hosp, # incidence object name
                      aes(x = date_index, #axes
                          y = count, 
                          fill = gender), # fill colour by gender
                      color = "black"      # colour of bar contour
  ) +  
    geom_col() + 
    facet_wrap(~gender) +
    theme_bw() + 
    labs(title = stringr::str_glue("Epidemic of cases admitted to {hosp}"), #title
         x = "Counts", 
         y = "Date", 
         fill = "Gender", 
         color = "Gender")
  
  # With older versions of R, remove the # before na_as_group and use this plot command instead.
  # plot_hosp <- plot(
  #       outbreak_hosp,
  #       fill = "gender",
  #       color = "black",
  #       title = stringr::str_glue("Epidemic of cases admitted to {hosp}")
  #     )
  
  #print the plot for hospitals
  print(plot_hosp)
  
} # end the for loop when it has been run for every hospital in hospital_names 

# =============================================================================
# purrr 
# discussion and introductory part in notes
# load package 
pacman::p_load(
  rio,            # import/export
  here,           # relative filepaths
  tidyverse,      # data mgmt and viz
  writexl,        # write Excel file with multiple sheets
  readxl          # import Excel with multiple sheets
)

# You want to import an Excel workbook with case data, but the data are split 
# across different named sheets in the workbook. 
# Each sheet contains cases from a given hospital.

# 1. extract sheet names and save them. we will use excel_sheets() from readx1.
# we provide excel workbook's file path to excel_sheets() which will extract
# sheet names.
sheet_names <- readxl::excel_sheets("data/hospital_linelists.xlsx")
sheet_names

# 2. Now that we have this vector of names, map() can provide them one-by-one to 
# the function import(). map argu: .x= sheet_names while .f= import()
# using import to import excel sheets we can specify the sheet using which= argu
# we use purrr style lambda so that we use this syntax on which argu:
# which = .x(vectors in .x argu)
# since weve used map() each data in every excel sheet will be saved in its own
# data frame within the list returned by map.
# we want each of these data frames within the list to have a name so before
# we pass sheet_names to map() we first pass it through set_names() from purrr
# which ensures each df within the list gets an appro name

combined <- sheet_names %>% 
  purrr::set_names() %>% 
  map(.f = ~import("data/hospital_linelists.xlsx", which = .x))
combined

# each excel sheet is saved in the list with its name
# lets use bind_rows() to bind this list into one df 
# there will be a new column to contain the names of ach hospital(sheet names)
# we use .id = to provide name of new column

combined <- sheet_names %>%                                     # begin with sheet names
  purrr::set_names() %>%                                        # set their names
  map(.f = ~import("data/hospital_linelists.xlsx", which = .x)) %>%  # iterate, import, save in list
  bind_rows(.id = "origin_sheet") # combine list of data frames, preserving origin in new column  
combined
view(combined) # perfecto!! # we now have one df with all sheets and their data

# to exclude importing a sheet, use map_at() instead of map() and use argu
# .at = c(-1) or minus any sheet you want to exclude
# this would be the code
sheet_names <- readxl::excel_sheets("hospital_linelists.xlsx")

combined <- sheet_names %>% 
  purrr::set_names() %>% 
  # exclude the first sheet
  map_at(.f = ~import( "hospital_linelists.xlsx", which = .x),
         .at = c(-1))

# we would have used map_dfr() instead of map() which will bind rows automatica
# lly but we would not have been able to capture which sheet(hospital) each
# case is from.

# =============================================================================
# make epicurve for each hospital case (read more on ggplotting)
# load package for plotting elements from list
pacman::p_load(ggpubr)

# map across the vector of 6 hospital "names" (created earlier)
# use the ggplot function specified
# output is a list with 6 ggplots

hospital_names <- unique(linelist$hospital)

my_plots <- map(
  .x = hospital_names,
  .f = ~ggplot(data = linelist %>% filter(hospital == .x)) +
    geom_histogram(aes(x = date_onset)) +
    labs(title = .x)
)

# print the ggplots (they are stored in a list)
ggarrange(plotlist = my_plots, ncol = 2, nrow = 3)


# alternatively we can create a function to plot the above
# Create function
make_epicurve <- function(hosp_name){
  
  ggplot(data = linelist %>% filter(hospital == hosp_name)) +
    geom_histogram(aes(x = date_onset)) +
    theme_classic()+
    labs(title = hosp_name)
  
}
# mapping
my_plots <- map(hospital_names, ~make_epicurve(hosp_name = .x))

# print the ggplots (they are stored in a list)
ggarrange(plotlist = my_plots, ncol = 2, nrow = 3)

# perfect 

# =============================================================================
# trying diffe approaches to importing excel sheets (hospital) then binding them
# 1st approach
sheet_names <- readxl::excel_sheets("data/hospital_linelists.xlsx")
trial <- map_dfr(
  .x = sheet_names,
  .f = ~import(sheet_names, which = .x)
)

path <- "data/hospital_linelists.xlsx"
trial <- map_dfr(
  .x = excel_sheets(path),
  .f = ~import(path, which = .x)
)
view(trial)

# we did it howe, we have it combined and we dont have a column identifying
# each is from which hospital

# =============================================================================
# mapping fuctions across columns
# lets we map() the function t.test() (read more on t.test page)across numeric 
# columns in the data frame linelist, comparing the numeric values by gender
# numeric columns of interest become .x  of map()
# The function t.test() is supplied as the .f function, which is applied to 
# each numeric column
# first ~ is for purrr style lambda while second ~ is for t.test() formula :
# t.test(numeric column ~ binary column)
# We supply the vector linelist$gender independently and statically (note that
#it is not included in select()
# Results are saved as a list
t.test_results <- linelist %>% 
  select(age, wt_kg, ht_cm, ct_blood, temp) %>%  # keep only some numeric columns to map across
  map(.f = ~t.test(.x ~ linelist$gender))        # t.test function, with equation NUMERIC ~ CATEGORICAL

# extracting from list
# extract elements from t.test_results list
# base r names()
names(t.test_results)

# by position: extract first element in the list : age
t.test_results[[1]]

t.test_results[[1]]["p.value"]

# using purrr map() and pluck()

t.test_results %>% 
  pluck("age", "p.value")

# extracting pvalue of all elements in list
t.test_results %>% 
  map(pluck, "p.value")

# or use map_dbl() to return a numeric vector
t.test_results %>% 
  map_dbl("p.value")

# =============================================================================
# converting list to data frame
# create a data frame with columns for the variable, its p-value, and the means 
# from the two groups (male and female).
# we want to convert it into a tibble
# in our code, we will surround our tibble() with {} to prevent the t.test_results 
# been created as the first column in the tibble
# the . represents the t.test_results 
# we use names() then inside it is the . so as to create a column with names
# of elements in the list
# then we use map_dbl() to create column with p.value
t.test_results %>% {
  tibble(
    variables = names(.),
    p         = map_dbl(., "p.value"))
}

# lets add column of means of both male and female
# element estimate in t.test_results is the one that contain the means of female 
# and male. estimate contains two elements of mean of f and mean of m within it.
# so we cannot use mapdbl or mapchr we use map which will give us a column of
# class list 
# lets see it
t.test_results %>% 
  {tibble(
    variables = names(.),
    p = map_dbl(., "p.value"),
    means = map(., "estimate"))}

# now we have a list . we will use tidyverse functions to split into means
# column into two columns for m and f
# unnest_wider() - gives each element of a list-column its own column
# unnest_longer() - gives each element of a list-column its own row
# hoist() - acts like unnest_wider() but you specify which elements to unnest

# we will use unnest_wider()
t.test_results %>% 
  {tibble(
    variables = names(.),
    p = map_dbl(., "p.value"),
    means = map(., "estimate")
  )} %>% 
  unnest_wider(means)

# there it is !

# =============================================================================
