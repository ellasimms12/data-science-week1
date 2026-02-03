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
