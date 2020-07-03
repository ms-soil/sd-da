#### Project info ####
# Same Data - Different Analyst
# Question: "How does grass cover influence Eucalyptus spp. seedling recruitment?"
# Group: BonaRes R-Group
# Participants: Marcus Schmidt, Bernd J. Berauer

#### library loading ####

library(tidyverse)
library(gridExtra) # to combine plots
library(ggpubr) # for scatter plots

#### view instructions ####
read_file("instructions")

#### view variable info ####
var_info <- read.csv("data/Eucalypt_Variables.csv") 

#### get and see data ####
d <- read_csv("data/Euc_data.csv")

head(d)
names(d)

#### Is there an Eucalyptus difference in season? ####
season_view <- function(parameter, title){
  d$Season <- factor(d$Season, levels = c('Winter 2006', 'Spring 2006', 'Autumn 2007'), ordered = TRUE)
  d %>% ggplot(aes(Season, parameter, fill = Season)) +
    geom_boxplot() +
    geom_point() +
    ggtitle(title) + theme_bw()
}
season_view(d$Euc_canopy_cover, "Euc. canopy cover")
season_view(d$euc_sdlgs0_50cm, "Euc. small seedlings")
season_view(d$`euc_sdlgs50cm-2m`, "Euc. medium seedlings")
season_view(d$`euc_sdlgs>2m`, "Euc. large seedlings")
# Result: Time matterst as only the third time (autumn 2007) shows large seedlings

#### combine grasses ####

d <- d %>%  
  mutate(aridity = (annual_precipitation / PET)*100,
         grass_total = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover,
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

#### visualize grass cover and Euc ####

euc_to_grass <- function(p, ytext){
  ggscatter(d, x = "grass_total", y = p, col = "grey",
            # add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman",
            xlab = "total grass cover (%)", ylab = ytext)
}

p1 <- euc_to_grass("Euc_canopy_cover", "Euc. canopy cover (%)")
p2 <- euc_to_grass("euc_sdlg_small",   "Euc. seedling nr. small")
p3 <- euc_to_grass("euc_sdlg_medium",  "Euc. seedling nr. medium")
p4 <- euc_to_grass("euc_sdlg_large",   "Euc. seedling nr. large")

p_all <- grid.arrange(p1, p2, p3, p4, ncol = 2)
# Result: Grass cover decreases with Euc presence

#### ORIGINAL grass cover effect on FINAL euc cover / seedling nr. per property ####
# - get average value of cover and seedling numbers per property
d_prop_beginning <- d %>% filter(Season == "Winter 2006") %>% 
  mutate(grass_total_beginning = grass_total) %>%
  mutate(grass_exotic_beginning = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover) %>%
  mutate(grass_native_beginning = NativePerennialGrass_cover + NativePerennialGraminoid_cover) %>%
  group_by(Property) %>% summarize(grass_total_beginning = mean(grass_total_beginning, na.rm = T),
                                   grass_exotic_beginning = mean(grass_exotic_beginning, na.rm = T),
                                   grass_native_beginning = mean(grass_native_beginning, na.rm = T),
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

#### non-linear regression ####
# fitting for Euc canopy cover
x <- d_prop$grass_total_beginning # plot(d_prop$grass_total_beginning, d_prop$euc_canopy_end)
y <- d_prop$euc_canopy_end
fit1 <- nls(y ~ a * exp(-b * x), start=list(a = 15, b = 2), algorithm="port")
summary(fit1) # 
a1 <- 10.41102
b1 <- 0.01473
fun1 <- function(x) {a1 * 2.718 ^ (-b1 * x)}

# fitting for small seedling nr
x <- d_prop$grass_total_beginning # plot(d_prop$grass_total_beginning, d_prop$euc_small_end)
y <- d_prop$euc_small_end
fit2 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit2)
a2 <- 1.96348
b2 <- 0.04053
fun2 <- function(x) {a2 * 2.718 ^ (-b2 * x)}


# fitting for medium seedling nr
x <- d_prop$grass_total_beginning # plot(d_prop$grass_total_beginning, d_prop$euc_medium_end)
y <- d_prop$euc_medium_end
fit3 <- nls(y ~ a * exp(-b * x), start=list(a = 2, b = 0.01), algorithm="port")
summary(fit3) 
a3 <- 4.87654
b3 <- 0.04168
fun3 <- function(x) {a3 * 2.718 ^ (-b3 * x)}


# fitting for large seedling nr # plot(d_prop$grass_total_beginning, d_prop$euc_large_end)
x <- d_prop$grass_total_beginning
y <- d_prop$euc_large_end
fit4 <- nls(y ~ a * exp(-b * x), start=list(a = 13, b = 2), algorithm="port")
summary(fit4)
a4 <- 1.300e+01
b4 <- 2.000e+00
fun4 <- function(x) {a4 * 2.718 ^ (-b4 * x)}

#### Visualizing the relationship between original grass cover and final Euc cover / seedlin nr. ####
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
  
  xlab("Grass cover (%) winter 2006") +
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

# Result: Grass cover as whole tends to reduce Eucalyptus cover and seedling abundance
# however, this is largely influcenced by confounders such as... see part 2

# single species should be looked at to see a clearer trend


#### Exploring further ####
ggplot(d_prop, aes(grass_total_beginning, euc_canopy_end)) +
  geom_point() 
ggplot(d_prop, aes(grass_exotic_beginning, euc_canopy_end)) +
  geom_point() 
ggplot(d_prop, aes(grass_native_beginning, euc_canopy_end)) +
  geom_point() 



ggplot(d_prop, aes(grass_total_beginning, euc_small_end)) +
  geom_point() 
ggplot(d_prop, aes(grass_exotic_beginning, euc_small_end)) +
  geom_point() 
ggplot(d_prop, aes(grass_native_beginning, euc_small_end)) +
  geom_point() 



ggplot(d_prop, aes(grass_total_beginning, euc_medium_end)) +
  geom_point()
ggplot(d_prop, aes(grass_exotic_beginning, euc_medium_end)) +
  geom_point()
ggplot(d_prop, aes(grass_native_beginning, euc_medium_end)) +
  geom_point()


ggplot(d_prop, aes(grass_total_beginning, euc_large_end)) +
  geom_point() 
ggplot(d_prop, aes(grass_exotic_beginning, euc_large_end)) +
  geom_point() 
ggplot(d_prop, aes(grass_native_beginning, euc_large_end)) +
  geom_point() 


# Result: the less exotic grasses the better the Euc can grow - the influence of native grasses is not consistent
# seedlings behave similar, so can be combined

ggplot(d_prop, aes(grass_exotic_beginning, euc_small_end + euc_medium_end + euc_large_end)) +
  geom_point() 


# so it is exotic grasses that consistently reduce all seedling sizes and canopy cover with no consistent influence on native grasses
# finaly we should use:


ggplot(d_prop, aes(grass_exotic_beginning, euc_small_end + euc_medium_end + euc_large_end)) +
  geom_point() 

 

#### 1st FINAL GRAPH ####
# fitting for Euc canopy cover
x <- d_prop$grass_exotic_beginning 
y <- d_prop$euc_canopy_end
fit1 <- nls(y ~ a * exp(-b * x), start=list(a = 15, b = 2), algorithm="port")
summary(fit1) # 
a1 <- 13.84565
b1 <- 0.07314
fun1 <- function(x) {a1 * 2.718 ^ (-b1 * x)}

ggplot(d_prop, aes(grass_exotic_beginning, euc_canopy_end)) +
  geom_point()

pl1 <- d_prop %>% ggplot() +
  geom_point(aes(grass_exotic_beginning, euc_canopy_end), col = "grey", size = 2) +
  stat_function(fun = fun1, col = "black", size = 1, linetype = 2) +
  theme_bw() +
  xlab("Grass cover of exotic grasses (%) Winter 2006") +
  ylab("Eucalyptus canopy cover (%) Autumn 2007") +
  scale_x_reverse()
pl1

# stats
obs1 <- d_prop$euc_canopy_end
pre1 <- a1 * exp(-b1 * d_prop$grass_exotic_beginning)
cor.test(obs1, pre1, method = "pearson")

pl1 <- pl1 +
  annotate("text", x = 35, y = 17, label = "p < 0.01, R = 0.61", size = 4) 
pl1

ggsave("figs/ex_grasses_canopy.png", pl2, width = 5, height = 4)

#### 2ND FINAL GRAPH ####
# fitting for Euc seedlings

d_prop <- d_prop %>% mutate(euc_allsize_end =  euc_small_end + euc_medium_end + euc_large_end)

x <- d_prop$grass_exotic_beginning 
y <- d_prop$euc_allsize_end
fit1 <- nls(y ~ a * exp(-b * x), start=list(a = 15, b = 2), algorithm="port")
summary(fit1) # 
a2 <- 8.7115
b2 <- 0.1642
fun1 <- function(x) {a2 * 2.718 ^ (-b2 * x)}

ggplot(d_prop, aes(grass_exotic_beginning, euc_allsize_end)) +
  geom_point()

pl2 <- d_prop %>% ggplot() +
  geom_point(aes(grass_exotic_beginning, euc_allsize_end), col = "grey", size = 2) +
  stat_function(fun = fun1, col = "black", size = 1, linetype = 2) +
  theme_bw() +
  xlab("Grass cover of exotic grasses (%) Winter 2006") +
  ylab("Eucalyptus seedlings all sizes (nr.) Autumn 2007") +
  scale_x_reverse()
pl2

# stats
obs2 <- d_prop$euc_allsize_end
pre2 <- a2 * exp(-b2 * d_prop$grass_exotic_beginning)
cor.test(obs2, pre2, method = "pearson")

pl2 <- pl2 +
  annotate("text", x = 35, y = 17, label = "p < 0.05, R = 0.48", size = 4) 
pl2

ggsave("figs/ex_grasses_seedlings.png", pl2, width = 5, height = 4)

p_together <- grid.arrange(pl1, pl2, ncol = 2)
ggsave("figs/ex_grasses_euc.png", p_together, width = 8, height = 4)


# to summarize: it is the exotic grasses which have a consistent effect on eucolyptus growth, not the native ones.
# the seedling sizes can be combines into a new measure.

# Conclusion: presence of exotic grasses reduce eucalyptus growth.

