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
#View(read_csv("data/Eucalypt_Variables.csv"))
var_info <- read.csv("data/Eucalypt_Variables.csv") #BJB likes to store it :)

#### get data ####
d <- read_csv("data/Euc_data.csv")
# View(d)
head(d)
names(d)

### why not use: print(tbl_df(d), n=20) ? ;)
### because its more to write and maybe takes longer to calculate

#### exploratory graphs ####
#### is there a difference in seasons generally? ####
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
names(d)
###variabels to add
#Functional groups zusammen führen (grass +Graminoids nicht vergessen & herbs & ferns & shrubs)
#native + exotic zusammenfassen
d <- d %>%  
  mutate(aridity = (annual_precipitation / PET)*100,
        
         grass_total = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover,
         ## below: grass only single types, maybe they explain more
         # grass_total = ExoticAnnualGrass_cover,
         # grass_total = ExoticPerennialGrass_cover,
         # grass_total = NativePerennialGrass_cover,
         # grass_total = NativePerennialGraminoid_cover,
         
         herb_total = ExoticAnnualHerb_cover + ExoticPerennialHerb_cover + NativePerennialHerb_cover,
         shrub_total = ExoticShrub_cover + NativeShrub_cover,
         exotic_total = ExoticAnnualGrass_cover + ExoticShrub_cover + ExoticPerennialGrass_cover + ExoticAnnualHerb_cover + ExoticPerennialHerb_cover,
         native_total = NativeShrub_cover + NativePerennialFern_cover + NativePerennialHerb_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover) %>% # to make new variables
  rename(., Quadrat_no = `Quadrat no`,
         Landscape_position =  `Landscape position`, 
         Distance_to_Eucalypt_canopy = `Distance_to_Eucalypt_canopy(m)`,
         euc_sdlg_small = euc_sdlgs0_50cm,
         euc_sdlg_medium = `euc_sdlgs50cm-2m`,
         euc_sdlg_large = `euc_sdlgs>2m`)
names(d)
str(d)

p1 <- d %>% 
  ggplot(aes(grass_total, Euc_canopy_cover)) +
  geom_point() +
  theme_bw()

p2 <- d %>% 
  ggplot(aes(grass_total, euc_sdlg_small)) +
  geom_point() +
  theme_bw()

p3 <- d %>% 
  ggplot(aes(grass_total, euc_sdlg_medium)) +
  geom_point() +
  theme_bw()

p4 <- d %>% 
  ggplot(aes(grass_total, euc_sdlg_large)) +
  geom_point() +
  theme_bw()

p_all <- grid.arrange(p1, p2, p3, p4, ncol = 2)

#### original grass cover effect on final euc cover ####
# - get average value of cover and seedling numbers per property
d_prop_beginning <- d %>% filter(Season == "Winter 2006") %>% 
  mutate(grass_total_beginning = grass_total) %>% 
  group_by(Property) %>% summarize(grass_total_beginning = mean(grass_total_beginning, na.rm = T),
                                   euc_canopy_beginning  = mean(Euc_canopy_cover, na.rm = T),
                                   euc_small_beginning = mean(euc_sdlg_small, na.rm = T),
                                   euc_medium_beginning = mean(euc_sdlg_medium, na.rm = T), 
                                   euc_large_beginning = mean(euc_sdlg_large, na.rm = T))
d_prop_beginning

d_prop_end <- d %>% filter(Season == "Autumn 2007") %>% 
  group_by(Property) %>% summarize(euc_canopy_end = mean(Euc_canopy_cover),
                                   euc_small_end = mean(euc_sdlg_small, na.rm = T),
                                   euc_medium_end = mean(euc_sdlg_medium, na.rm = T),
                                   euc_large_end = mean(euc_sdlg_large, na.rm = T))
d_prop_end

d_prop <- inner_join(d_prop_beginning, d_prop_end, by = "Property")
as_tibble(d_prop)

# View(d_prop)

#### non-linear regression ####
# fitting for canopy
x <- d_prop$grass_total_beginning
y <- d_prop$euc_canopy_end
fit1 <- nls(y ~ a * exp(-b * x), start=list(a = 15, b = 2), algorithm="port")
summary(fit1) # 
a1 <- 10.41102
b1 <- 0.01473
fun1 <- function(x) {a1 * 2.718 ^ (-b1 * x)}

# fitting for small seedling
x <- d_prop$grass_total_beginning
y <- d_prop$euc_small_end
fit2 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit2)
a2 <- 1.96348
b2 <- 0.04053
fun2 <- function(x) {a2 * 2.718 ^ (-b2 * x)}


# fitting for medium seedling
x <- d_prop$grass_total_beginning
y <- d_prop$euc_medium_end
fit3 <- nls(y ~ a * exp(-b * x), start=list(a = 2, b = 0.01), algorithm="port")
summary(fit3) 
a3 <- 4.87654
b3 <- 0.04168
fun3 <- function(x) {a3 * 2.718 ^ (-b3 * x)}


# fitting for large seedling
x <- d_prop$grass_total_beginning
y <- d_prop$euc_large_end
fit4 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit4)
a4 <- 1.300e+01
b4 <- 2.000e+00
fun4 <- function(x) {a4 * 2.718 ^ (-b4 * x)}

#### plotting ####
# plotting
pl <- d_prop %>% ggplot() +
  geom_point(aes(grass_total_beginning, euc_canopy_end), col = "red", size = 2) +
  geom_point(aes(grass_total_beginning, euc_small_end), col = "lightblue", size = 2) +
  geom_point(aes(grass_total_beginning, euc_medium_end), col = "blue", size = 2) +
  geom_point(aes(grass_total_beginning, euc_large_end), col = "darkblue", size = 2) +
  stat_function(fun = fun1, col = "red", size = 1, linetype = 2) +
  stat_function(fun = fun2, col = "lightblue", size = 1, linetype = 2) +
  stat_function(fun = fun3, col = "blue", size = 1, linetype = 2) +
  stat_function(fun = fun4, col = "darkblue", size = 1, linetype = 2) +
  
  xlab("grass cover (%) winter 2006") +
  ylab("Euc. cover (%) & seedlings (nr.) autumn 2007") +
  theme_bw() # +
  # ggtitle("Influence of initial grass cover on Eucalyptus growth")
pl

# add legend
pl <- pl +
    annotate("text", x = 50, y = 18, label = "Euc canopy", size = 4)   + 
    annotate("text", x = 50, y = 17, label = "Euc seedl. small", size = 4)   + 
    annotate("text", x = 50, y = 16, label = "Euc seedl. medium", size = 4)   + 
    annotate("text", x = 50, y = 15, label = "Euc seedl. large", size = 4)   + 
    annotate("point", x = 60, y = 17.9, color = "red") +
    annotate("point", x = 60, y = 16.9, color = "lightblue") +
    annotate("point", x = 60, y = 15.9, color = "blue") +
    annotate("point", x = 60, y = 14.9, color = "darkblue") +
    annotate("text", x = 65, y = 18, label = "p = 0.28") +
    annotate("text", x = 65, y = 17, label = "p = 0.24") +
    annotate("text", x = 65, y = 16, label = "p = 0.17") +
    annotate("text", x = 65, y = 15, label = "p = 0.64") 
pl
  
ggsave("figs/grass-vs-euc.png", pl, width = 5, height = 4)
# original grass cover exponentially decreases eucalyptus cover and seedling abundance

unique(d$Date)
# - potential factors explaining are... and will be taken into account

#### observed vs. predicted ####
# canopy
obs1 <- d_prop$euc_canopy_end
pre1 <- a1 * exp(-b1 * d_prop$grass_total_beginning)
#plot(obs1, pre1)
cor.test(obs1, pre1, method = "pearson")

# small seedlings
obs2 <- d_prop$euc_small_end
pre2 <- a2 * exp(-b2 * d_prop$grass_total_beginning)
#plot(obs2, pre2)
cor.test(obs2, pre2, method = "pearson")

# medium seedlings
obs3 <- d_prop$euc_medium_end
pre3 <- a3 * exp(-b3 * d_prop$grass_total_beginning)
#plot(obs3, pre3)
cor.test(obs3, pre3, method = "pearson")

# large seedlings
obs4 <- d_prop$euc_large_end
pre4 <- a4 * exp(-b4 * d_prop$grass_total_beginning)
#plot(obs4, pre4)
cor.test(obs4, pre4, method = "pearson")

# grass cover as whole tends to reduce Eucalyptus cover and seedling abundance
# however, this is largely influcenced by confounders such as...

# single species should be looked at to see a clearer trend

