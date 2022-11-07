# Alberto Rovellini
# 11/07/2022

# This code calculates the parameter FSMG_XXX for migrating groups in Atlantis GOA
# Calculations are based on: 
# 1. Weight at age as calculated in the initial conditions code
# 2. Number of years per age class as defined in the Groups.csv file
# 3. Time spent outside the model domain

# The values are ontogenetic but not by age class. We need to weight them by the proportion of individuals per age class

library(tidyverse)
