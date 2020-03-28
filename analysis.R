########################################################
### Same Data - Different Analyst
### Question: "How does grass cover influence Eucalyptus spp. seedling recruitment?"
### Group: BonaRes R-Group
### Participants: Marcus Schmidt, Paul-David Klein, Bernd J. Berauer
### Date: wann immer wir Zeit und Lust haben
########################################################

# How does grass cover influence Eucalyptus spp. seedling recruitment?

library(tidyverse)
library(gridExtra)

#### view instructions ####
read_file("instructions")

#### view variable info ####
 View(read_csv("data/Eucalypt_Variables.csv"))
var_info <- read.csv("data/Eucalypt_Variables.csv") #BJB likes to store it :)

#### get data ####
d <- read_csv("data/Euc_data.csv")
# View(d)
head(d)
names(d)

### why not use: print(tbl_df(d), n=20) ? ;)
### because its more to write and maybe takes longer to calculate

#### exploratory graphs ####

# selecting parameters relevant to date, eucalyptus and grasses
d_grass_euc <- d %>% select (Date, matches("Euc"), matches("grass")) %>% 
  mutate(grass_total = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover) # to get all grasses
names(d_grass_euc)
head(d_grass_euc)

p1 <- d_grass_euc %>% 
  ggplot(aes(grass_total, Euc_canopy_cover)) +
  geom_point() +
  theme_bw()

p2 <- d_grass_euc %>% 
  ggplot(aes(grass_total, euc_sdlgs0_50cm)) +
  geom_point() +
  theme_bw()

p3 <- d_grass_euc %>% 
  ggplot(aes(grass_total, `euc_sdlgs50cm-2m`)) +
  geom_point() +
  theme_bw()

p4 <- d_grass_euc %>% 
  ggplot(aes(grass_total, `euc_sdlgs>2m`)) +
  geom_point() +
  theme_bw()

p_all <- grid.arrange(p1, p2, p3, p4, ncol = 2)







