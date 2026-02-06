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



# FIX 1: [Issue description] ==== there are negative values present in the weight catergory which otherwise would
# be a completely valid measure of body mass in mg. using an assumption that these values are valid and 
# there was an input error by where negatives were entered.

# Show the problem:
# [Code to demonstrate issue exists]
summary(mosquito_egg_raw$body_mass_mg)
#min value is -93, which is biologically impossible



# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_raw |>
   mutate(body_mass_mg = abs(body_mass_mg)) 
  # YOUR CODE HERE
  # converting to absolute values
  
  # Verify it worked:
  # [Code to check change happened]
summary(mosquito_egg_data_step1$body_mass_mg)
#min value is now 3.80 which no longer negative value.
  # What changed and why it matters:
  # [2-3 sentences explaining consequences]
  # Negative values have been converted into their absolute values. I felt this was a valid change as all
  # negative values remained within a range that made sense with the other data present for body mass. Removing
  # all those negative values would exclude a large amount of otherwise valid data.
  
  
  # FIX 2: [Issue description]  ==== there a multiple different cases for the variables within treatment.

# Show the problem:
# [Code]
mosquito_egg_data_step1 |>
  distinct(treatment)
# 1 Medium_dose
# 2 High_dose  
# 3 high_dose  
# 4 Low_dose   
# 5 Control    
# 6 HIGH_DOSE  
# 7 MEDIUM_DOSE
# 8 low_dose   
# 9 control    
# 10 LOW_DOSE   
# 11 CONTROL    
# 12 medium_dose

# Fix it:
mosquito_egg_data_step2 <- mosquito_egg_data_step1 |>
  mutate(treatment = case_when(
    treatment %in% c("CONTROL" , "Control") ~ "control",  #%in% for vectors to be used to convert
    treatment  %in% c("High_dose", "HIGH_DOSE") ~ "high_dose", 
    treatment  %in% c("Low_dose", "LOW_DOSE") ~ "low_dose", 
    treatment  %in% c("Medium_dose", "MEDIUM_DOSE") ~ "medium_dose",
    .default = as.character(treatment)
  ))
  # YOUR CODE

  
  # Verify it worked:
  # [Code]
mosquito_egg_data_step2 |>
  distinct(treatment)
# 1 medium_dose
# 2 high_dose  
# 3 low_dose   
# 4 control

  # What changed and why it matters:
  # [2-3 sentences]
  # It changed because a vector was created containing all the variables which then converted each case into 
  # the desired syntax. %in% was need to read the vector. This is important as otherwise when using the variable
  # of treatment, there would be 12 catergories instead of 4.

#fix 3 - using partners code 
mosquito_egg_raw_cleaned <- mosquito_egg_raw |> 
  drop_na(eggs_laid) |> 
  drop_na(eggs_hatched) |> 
  drop_na(body_mass_mg) 
# drop na values for eggs_laid, eggs_hatched and body_mass_mg
