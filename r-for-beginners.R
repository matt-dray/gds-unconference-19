# GDS unconference: R for beginners
# Task: calculate mean BMI of each Pokemon species
# 8 April 2019
# Matt Dray

# Gotta be concerned about the health of 'em all! The Department for Health &
# Social Care of Pokemon is concerned that the Body Mass Index of certain
# Pokemon species is problematic (almost as problematic as trying to use BMI as
# a measure of anything useful). Can you calculate the mean BMI of an
# (admittedly extremely small) sample in this (extremely contrived) scenario?

# The data are about 700 captures recorded from the Pokemon Go app (it's still
# going!) from about two years ago. They're provided as a comma-separated values
# (CSV) file in the data/ folder of your working directory.

# Disclaimer! The purpose of this session is to quickly get a flavour of some
# basic R commands. It's intended for beginners to R. Please ask me if you have
# any questions. I realy like R and I'd you to like R too.


# Load packages -----------------------------------------------------------


# R uses functions to get stuff done. Functions can be organised into packages,
# which can be shared to extend R's abilities.

# We can use the function install.packages to download some neat packages.
# install.packages(c("readr", "dplyr", "ggplot2"))

library("readr")  # for reading data nicely
library("dplyr")  # nice tools for manipulating data
library("ggplot2")  # nice plotting


# Read --------------------------------------------------------------------


# See help pages
?read_csv

# Read a CSV file from our 'data' folder
# object name 'gets' the contents of a CSV file at this path
pkmn <- read_csv(file = "data/pogo.csv")

# Print the dataset
pkmn

# view the dataset
View(pkmn)

# See the structure of the data
glimpse(pkmn)

# Very quick summary of the data
summary(pkmn)


# Wrangle -----------------------------------------------------------------

pkmn_bmi <- pkmn %>% 
  filter(combat_power < mean(combat_power)) %>% 
  mutate(bmi = weight_kg/height_m) %>%
  group_by(species) %>% 
  summarise(count = n(), mean_bmi = mean(bmi)) %>% 
  arrange(desc(mean_bmi))


# Join --------------------------------------------------------------------

pokedex <- read_csv("data/pokedex.csv")

left_join(pkmn_bmi, pokedex)

# Plot --------------------------------------------------------------------

ggplot()
