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
#### is there a differnence in seasons generally? ####
season_view <- function(parameter){
  d$Season <- factor(d$Season, levels = c('Winter 2006', 'Spring 2006', 'Autumn 2007'), ordered = TRUE)
  d %>% ggplot(aes(Season, parameter, fill = Season)) +
    geom_boxplot() +
    geom_point()
}
season_view(d$Euc_canopy_cover)
season_view(d$euc_sdlgs0_50cm)
season_view(d$`euc_sdlgs50cm-2m`)
season_view(d$`euc_sdlgs>2m`)
# Time seems to matter, only the autumn 2007 data has seedlings > 2m so we 
# do have a timeline

#### relationship of grass and Euc ####
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

#### how about original grass cover effect on final euc cover? ####
# - get average value of cover and seedling numbers per property
d_prop_beginning <- d %>% filter(Season == "Winter 2006") %>% 
  mutate(grass_total_beginning = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover) %>% 
  group_by(Property) %>% summarize(grass_total_beginning = mean(grass_total_beginning, na.rm = T),
                                   euc_canopy_beginning  = mean(Euc_canopy_cover, na.rm = T),
                                   euc_small_beginning = mean(euc_sdlgs0_50cm, na.rm = T),
                                   euc_medium_beginning = mean(`euc_sdlgs50cm-2m`, na.rm = T), 
                                   euc_large_beginning = mean(`euc_sdlgs>2m`, na.rm = T))
d_prop_beginning

d_prop_end <- d %>% filter(Season == "Autumn 2007") %>% 
  group_by(Property) %>% summarize(euc_canopy_end = mean(Euc_canopy_cover),
                                   euc_small_end = mean(euc_sdlgs0_50cm, na.rm = T),
                                   euc_medium_end = mean(`euc_sdlgs50cm-2m`, na.rm = T),
                                   euc_large_end = mean(`euc_sdlgs>2m`))
d_prop_end

d_prop <- inner_join(d_prop_beginning, d_prop_end, by = "Property")
as_tibble(d_prop)

# fitting for canopy
x <- d_prop$grass_total_beginning
y <- d_prop$euc_canopy_end
fit1 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit1) # a = 13.85; b = 0.07
fun1 <- function(x) {13.85 * 2.718 ^ (-0.07 * x)}

# fitting for small seedling
x <- d_prop$grass_total_beginning
y <- d_prop$euc_small_end
fit2 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit2) # a = 1.41; b = 0.10
fun2 <- function(x) {1.41 * 2.718 ^ (-0.10 * x)}

# fitting for medium seedling
x <- d_prop$grass_total_beginning
y <- d_prop$euc_medium_end
fit3 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit3) # a = 31.17; b = 0.61
fun3 <- function(x) {31.17 * 2.718 ^ (-0.61 * x)}

# fitting for large seedling
x <- d_prop$grass_total_beginning
y <- d_prop$euc_large_end
fit4 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit4) # a = 55.01; b = 1.00
fun4 <- function(x) {55.01 * 2.718 ^ (-1.00* x)}

# plotting
d_prop %>% ggplot() +
  geom_point(aes(grass_total_beginning, euc_canopy_end), col = "red") +
  stat_function(fun = fun1, col = "red") +
  geom_point(aes(grass_total_beginning, euc_small_end), col = "lightblue") +
  stat_function(fun = fun2, col = "lightblue") +
  geom_point(aes(grass_total_beginning, euc_medium_end), col = "blue") +
  stat_function(fun = fun3, col = "blue") +
  geom_point(aes(grass_total_beginning, euc_large_end), col = "darkblue") +
  stat_function(fun = fun4, col = "darkblue") +
  xlab("grass cover winter 2006") +
  ylab("Euc. cover (red) & seedlings (blue) autumn 2007") +
  theme_bw() +
  ggtitle("Influence of initial grass cover on Eucalyptus growth")

# original grass cover exponentially decreases eucalyptus cover and seedling abundance


# - potential factors explaining are... and will be taken into account
