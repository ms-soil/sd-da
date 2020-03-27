########################################################
### Same Data - Different Analyst
### Question: "How does grass cover influence Eucalyptus spp. seedling recruitment?"
### Group: BonaRes R-Group
### Participants: Marcus Schmidt, Paul-David Klein, Bernd J. Berauer
### Date: wann immer wir Zeit und Lust haben
########################################################

# How does grass cover influence Eucalyptus spp. seedling recruitment?

library(tidyverse)

#### view instructions ####
read_file("instructions")

#### view variable info ####
 View(read_csv("data/Eucalypt_Variables.csv"))
var_info <- read.csv("data/Eucalypt_Variables.csv") #BJB likes to store it :)

#### get data ####
d <- read_csv("data/Euc_data.csv")
# View(d)
head(d)

### why not use: print(tbl_df(d), n=20) ? ;)
### because its more to write and maybe takes longer to calculate
