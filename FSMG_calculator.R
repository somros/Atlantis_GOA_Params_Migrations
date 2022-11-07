# Alberto Rovellini
# 11/07/2022

# This code calculates the parameter FSMG_XXX for migrating groups in Atlantis GOA
# Calculations are based on: 
# 1. Weight at age as calculated in the initial conditions code
# 2. Number of years per age class as defined in the Groups.csv file
# 3. Age at maturity
# 4. Time spent outside the model domain

# The values are ontogenetic but not by age class. We need to weight them by the proportion of individuals per age class

library(tidyverse)
library(readxl)

# source weight at age and biological parameters data from their location on local - init code
dat_by_age <- read.csv('../../build_init_prm_10COHORTS/data/nums_age_functional_groups.csv')
biopar <- read.csv('../../build_init_prm_10COHORTS/data/life_history_parameters.csv')
# source migration data from Migrations.csv input for runs - update when that changes
migrations <- read.csv('../data/GOAMigrations.csv')
fg <- read.csv('../../build_init_prm_10COHORTS/data/GOA_Groups.csv')

# which groups migrate?
migrators <- migrations %>% pull(GroupCode) %>% unique()

# get weight at age (by age class) and proportion of total individuals
# discard age 0 as pre-settlers
waa <- dat_by_age %>%
  filter(Code %in% migrators, age_class > 0) %>%
  select(Code, age_class, wet_weight_g, numbers_at_age) %>%
  group_by(Code) %>%
  mutate(Prop = numbers_at_age/sum(numbers_at_age)) %>%
  ungroup()

# get years per age class
ypa <- fg %>%
  filter(Code %in% migrators) %>%
  select(Code, NumAgeClassSize)

# get age at maturity
amat <- biopar %>%
  filter(Code %in% migrators) %>%
  select(Code, mat_FUNC)

# have a look at growth according to original prameters
ggplot(data = waa, aes(x = age_class, y = wet_weight_g))+geom_point()+geom_line()+facet_wrap(~Code, scales = 'free')

# Given waa w1 and w2 for two consecutive age classes, calculate rate of annual change per age class as:
# ac = (w2/w1)^(1/NumAgeClassSize) -1

# end result is the annual change (growth rate) on average by life stage (in weight)
waa1 <- waa %>%
  left_join(ypa, by = 'Code') %>%
  left_join(amat, by = 'Code') %>%
  mutate(amat = ceiling(mat_FUNC/NumAgeClassSize)) %>% # calculate first mature age class
  mutate(amat = replace(amat, amat==1, 2)) %>% # avoid maturity at first age class
  group_by(Code) %>%
  mutate(next_wt = lead(wet_weight_g),
         ac = (next_wt/wet_weight_g)^(1/NumAgeClassSize)-1) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(stage = ifelse(age_class < amat, 0, 1)) %>% # get stage of the age class - 0 = juvenile 1 = adult
  ungroup() %>%
  group_by(Code, stage) %>%
  summarise(ac_age_class = weighted.mean(ac, Prop, na.rm = T))

# now need to scale this by the proportion of time away in a year
# this assumes constant growth throughout the year regardless of whether they are inside or outside
# this assumption is wrong for most migrators as they move to either feed or not feed (reproduce) in the first place
# approximate it to this for now

# per MigID this will be: ac / 365 * XX, where XX is the number of days outside the model
mig_span <- migrations %>%
  rowwise() %>%
  mutate(away_days = ifelse(EndTofY < StartTofY, 365-abs(EndTofY- StartTofY), EndTofY- StartTofY),
         prop_away = away_days / 365) %>%
  ungroup() %>%
  select(GroupCode, StartStage, MigID, prop_away) %>%
  rename(Code = GroupCode,
         stage = StartStage)

# now join
fsmg <- mig_span %>%
  left_join(waa1, by = c('Code','stage')) %>%
  mutate(FSMG = ac_age_class * prop_away,
         FSMG = replace(FSMG, is.nan(FSMG), 0)) %>%
  select(-prop_away, -ac_age_class)

write.csv(fsmg, 'fsmg.csv', row.names = F)
  