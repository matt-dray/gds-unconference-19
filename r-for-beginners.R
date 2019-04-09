# GDS unconference: R for beginners
# Task: calculate mean BMI of each Pokemon species
# 9 April 2019
# Matt Dray

# (This is a shortened version of the training materials here:
# https://matt-dray.github.io/beginner-r-feat-pkmn/)

# Gotta be concerned about the health of 'em all! The Department for Health &
# Social Care of Pokemon has decided that it must calculate the Body Mass Index
# of certain Pokemon for some reason (a poor policy, since BMI is not a good
# measure of anything, really). Can you calculate the mean BMI of an
# (admittedly extremely small) sample in this (extremely contrived) scenario?

# The data are about 700 captures recorded from the Pokemon Go app (it's still
# going!) from about two years ago. They're provided as a comma-separated values
# (CSV) file in the data/ folder of your working directory.

# Disclaimer! The purpose of this session is for beginners to quickly get a
# flavour of some basic R commands. It's intended for beginners to R. Please ask
# me if you have any questions.


# Load packages -----------------------------------------------------------


# R uses functions to get stuff done. Functions are bits of code that do a
# thing. Functions can be organised into packages, which can be shared to extend
# R's abilities.

# We can use the function install.packages() to download some neat packages --
# the {tidyverse} package contains some great tools for reading, manipulating
# and presenting data

install.packages("tidyverse")

# We only need to install a package once, then we just call the functions from
# library when we need them

library("tidyverse")


# Read --------------------------------------------------------------------


# Let's read the CSV file from our 'data' folder. The code means "we're reading
# a dataset and assigning it to the name 'pkmn' 

pkmn <- read_csv(file = "data/pogo.csv")

# If you ever want to know what a function does, see the help pages with '?'

?read_csv

# So now we can check out the contents of the object containing our data

pkmn  # print it to the console

View(pkmn)  # see the data in its own tab

glimpse(pkmn)  # see the structure of the data

summary(pkmn)  # quick summary of the data


# Wrangle -----------------------------------------------------------------


# We can use the special operator %>% (the pipe) to chain together functions
# into a sort of recipe. This reads as 'take the pkmn data, then create a new
# column to calculate BMI from two other columns'

pkmn_bmi <- pkmn %>% 
  mutate(bmi = weight_kg/(height_m)^2)  # create a new column

pkmn_bmi %>% select(species, bmi) %>% sample_n(5)  # five random examples

# Now we have a column containing BMI, let's get the mean BMI by species

pkmn_bmi_mean <- pkmn_bmi %>% 
  group_by(species) %>%  # group the data by levels within a column
  summarise(count = n(), mean_bmi = mean(bmi)) %>%  # summarise by that group
  arrange(desc(mean_bmi))  # arrange the data in descending order

pkmn_bmi_mean  # take a look at the data


# Join --------------------------------------------------------------------


# We can introduce another dataset and join it to our original dataset, using
# the species column as a key

pokedex <- read_csv("data/pokedex.csv")  # read the data

pkmn_bmi_type <- left_join(
  x = pkmn_bmi,  # original dataset
  y = pokedex,  # new dataset
  by = "species"  # key to join by
)

pkmn_bmi_type  # check it


# Plot --------------------------------------------------------------------


# Let's build up a plot of BMI for a few species. First we need to get those
# species that are common. When were there more than 40 individuals?

common_species <- pkmn_bmi_mean %>%  # using the object we created earlier
  filter(count > 40) %>%   # filter for where n > 40
  pull(species)  # extract the contents of the species column

pkmn_bmi_type %>%
  filter(species %in% common_species) %>%  # onlt the species where n > 40
  ggplot(aes(x = species, y = bmi)) +  # variables on each axis
  geom_jitter(color = "gray") +  # points, arbitrarily shifted for visibility
  geom_violin(alpha = 0) +  # see-through violin plot
  stat_summary(fun.y = "median", geom = "point") +  # add median point
  labs(  # add labels
    title = "Rattata has the largest median BMI",
    subtitle = "Pidgey and Spearow have lower mean BMI",
    x = "Species", y = "Body Mass Index (BMI)",
    caption = "The Pokemon Company and Niantic own Pokemon/Pokemon Go"
  ) +
  theme_classic()  # one of many types of theme

# But we can also use themes created by other people. So for fun, here is one
# themed around the look of Pokemon health-point bars on the Game Boy. First,
# install the package and then call it from the library.

remotes::install_github("schochastics/Rokemon")  # this is installed fom GitHub
library(Rokemon)  # load all the package functions

# And now we can use the gghealth() function to create the plot

pkmn_bmi_mean %>%
  filter(count > 20) %>%  # just those where there's more than 20 individuals
  gghealth("species", "mean_bmi") +  # variables for the axes
  labs(  # add labels
    x = "", y = "BMI",
    title = "Pokemon BMI",
    subtitle = "How is this a thing?",
    caption = "Credit: The Pokemon Company/Niantic"
  )
