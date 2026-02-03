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

#str_trim removes the wh


