# How does grass cover influence Eucalyptus spp. seedling recruitment?

library(tidyverse)

#### view instructions ####
# read_file("instructions")

#### view variable info ####
# View(read_csv("data/Eucalypt_Variables.csv"))

#### get data ####
d <- read_csv("data/Euc_data.csv")
# View(d)
head(d)

### why not use: print(tbl_df(d), n=20) ? ;)
