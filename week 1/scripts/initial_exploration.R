# Week 1: Initial Data Exploration ====
# Author: [Ella Simms]
# Date: [30/01/2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("week 1/data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)
getwd()
# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   
# - What's being measured?
#   age of mosquitos, body mass in mg, where the data was collected, the date of collection
#   who collected it, which treatment - dose level, how many eggs laid by the mosquito and how many of those hatched

# - How many observations?
#   205
# - Anything surprising?
#   unsure what the treatment is
# - Any obvious problems?
# there are missing values for body mass and eggs laid and eggs hatched
# there is consistency in the cases for treatment
# possible inputs errors as some of the body mass is minus values

#tutorial 1: strings
penguins_clean_names <- readRDS(url("https://github.com/UEABIO/5023B/raw/refs/heads/2026/files/penguins.RDS"))
#reading in the data

#str_trim removes the whitespace on either side of the string
str_trim(" Adelie Penguin (Pygoscelis adeliae) ")

#str_squish - ensures only one space between words
str_squish("  Adelie    Penguin   (Pygoscelis   adeliae)  ")

#str_truncate - shortens long strings to same specific length of phrase
str_trunc("Adelie Penguin (Pygoscelis adeliae)", width = 18, side = "right")
#full string is now "Adelie Penguin..."

#str_split - splits things into small pieces based on separators
str_split("Adelie Penguin (Pygoscelis adeliae)", " ")
#split into "Adelie"      "Penguin"     "(Pygoscelis" "adeliae)"   

#str_c joins text into one string separated by _
str_c("Adelie", "Penguin", sep = "_")
#adelie_penguin

#dyplr - cleaning of strings

penguins_clean_names |>  
  distinct(sex)
#only 3 distinct sex catergories - male, female, na
#no need to update

#case_when functions - changes things on a conditional basis
#checks individual conditions changes when they are met

#conditional change of the names of the values in a variable
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = as.character(species)
  )
  )


#if_else
#two-way decisions, based on if something is true or false

penguins_clean_names |> 
  mutate(sex = if_else(
    sex == "MALE", "Male", "Female"
  )
  )
#need a logical test to decide if something is true or false
#and a value if true and a value is false
#maybe be dangerous if there is more than one true or false value (e.g MALE, male, Male)

#renaming text values
# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = stringr::word(species, 1)
  ) |> 
  mutate(sex = stringr::str_to_title(sex))
#1 word in species title
#made sex a title

#changing species to uppercase
# use mutate and case_when 
# for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = stringr::str_to_upper(species))

#splitting columns
#reducing down values but still retaining important information
penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()"
  ) 
#would work if adelies name wasn't already altered lol

#string matching
#str_detect filters data and produces a true or false
str_detect("Genus specificus", "Genus")
# 3 possible names in species column
penguins_clean_names |> distinct(species)
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie" ~ "Adelie Penguin (Pygoscelis adeliae)",
    species == "Gentoo" ~ "Gentoo penguin (Pygoscelis papua)",
    species == "Chinstrap" ~ "Chinstrap penguin (Pygoscelis antarctica)",
    .default = as.character(species)
  )
  )

penguins_clean_names |>
  filter(str_detect(species, "papua")) |>
  select(species)
#filters out only those names

#removal of patterns
# remove match for Genus (followed by a whitespace)
str_remove("Genus specificus", pattern = "Genus ")

#string removal to remove brackets from split names
penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()" # regex pattern: split before the '('
  ) |> 
  mutate(full_latin_name = str_remove_all(full_latin_name, "[\\(\\)]"))

#tutorial 2
penguins_demo <- penguins_clean_names |> 
  slice(1:50) |> 
  bind_rows(slice(penguins_clean_names, c(1,5,10,15,30)))
#adding duplicates

# check for whole duplicate 
# rows in the data
penguins_clean_names |> 
  filter(duplicated(across(everything())))
sum() 
#still 0
# Keep only unduplicated data with !
penguins_demo |> 
  filter(!duplicated(across(everything())))

#n_distinct counts the number of distinct variables
penguins_clean_names |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )
#344 variables, 190 distinct

#dates
library(lubridate)
#reformatting dates
date("2017-10-11T14:02:00")
#date is now 2017-10-11
dmy("11 October 2020")
#2020-10-11
mdy("10/11/2020")
#2020-10-11
df <- tibble(
  date = c("X2020.01.22",
           "X2020.01.22",
           "X2020.01.22",
           "X2020.01.22")
)
df %>%
  mutate(
    date = as_date(date, format = "X%Y.%m.%d")
  )

#extracting the year from the date - 2017
year("2017-11-28T14:02:00")
#extracting month - 11
month("2017-11-28T14:02:00")
#and so one with week, day

#function need to deal with date that is imported as the number of days
#since an origin date
library(janitor)

excel_numeric_to_date(42370)
#2016-01-01

#penguins is in ymd so dmy function should be used
penguins_clean_names <- penguins_clean_names |>
  mutate(date_egg_proper = lubridate::dmy(date_egg))

penguins_clean_names |> 
  summarise(min_date=min(date_egg),
            max_date=max(date_egg))
#summarising dates

#extracting columns to order them by year
penguins_clean_names <- penguins_clean_names |> 
  mutate(year = lubridate::year(date_egg))

#filtering datasets by date
# return records after 2008
penguins_clean_names |>
  filter(date_egg >= ymd("2008-01-01"))

#missing data

penguins_clean_names |> 
  group_by(species) |> 
  summarise(mean = mean(body_mass_g))
#data for the other two doesn't allow for a mean 
library(naniar)
naniar::vis_miss(penguins_clean_names)
naniar::gg_miss_upset(penguins_clean_names)
#most missing in comments
#next highest is delta 13 and delta 15 together

#returning all rows with missing variables
penguins_clean_names |> 
  filter(if_any(everything(), is.na)) |>
  select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, 
         sex, delta_15_n_o_oo, delta_13_c_o_oo,comments,
         everything()) # reorder columns

#looking at specific columns 
penguins_clean_names |> 
  filter(if_any(culmen_length_mm, is.na))  # reorder columns

#removing na values
#drop.na drops all 
#drop.na within a variable should be temporarily used for calculations
#within other function na.rm=T can be used so na's are excused from the variable


naniar::vis_miss(mosquito_egg_raw)






