#### Project info / libraries / instructions / variables / data####
# Same Data - Different Analyst
# Question: "How does grass cover influence Eucalyptus spp. seedling recruitment?"
# Group: BonaRes R-Group
# Participants: Marcus Schmidt, Bernd J. Berauer

# library loading #

library(tidyverse)
library(gridExtra) # to combine plots
library(ggpubr) # for scatter plots

# view instructions #
read_file("instructions")

# view variable info #
var_info <- read.csv("data/Eucalypt_Variables.csv") 

# get and see data #
d <- read_csv("data/Euc_data.csv")

head(d)
names(d)

#### Temporal trend of Eucalyptus growth ####
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

#### Combine grasses into total grass cover ####

d <- d %>%  
  mutate(grass_total = 
           ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover) %>% 
  rename(., euc_sdlg_small = euc_sdlgs0_50cm,
         euc_sdlg_medium = `euc_sdlgs50cm-2m`,
         euc_sdlg_large = `euc_sdlgs>2m`)
names(d)
str(d)

#### visualize total grass cover and Euc ####

euc_to_grass <- function(p, ytext){
  ggscatter(d, x = "grass_total", y = p, col = "grey",
            # add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "spearman",
            xlab = "total grass cover (%)", ylab = ytext) +
    scale_x_reverse()
}

p1 <- euc_to_grass("Euc_canopy_cover", "Euc. canopy cover (%)")
p2 <- euc_to_grass("euc_sdlg_small",   "Euc. seedling nr. small")
p3 <- euc_to_grass("euc_sdlg_medium",  "Euc. seedling nr. medium")
p4 <- euc_to_grass("euc_sdlg_large",   "Euc. seedling nr. large")

p_all <- grid.arrange(p1, p2, p3, p4, ncol = 2)

#### FIG 1 ####

ggsave("figs/fig_1_tot_grass_euc_growth.png", p_all, width = 7.5, height = 6)


# Result: Grass cover decreases with Euc presence

#### Generate data including temporal change ####
# get average value of cover and seedling numbers per property
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


#### Exploring total vs. exotic vs. native grassees influences ####
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



#### Fitting change in Eucalyptus canopy with exotic grasses ####
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


#### Fitting change in Eucalyptus seedlings with exotic grasses ####
# fitting for Euc seedlings

d_prop <- d_prop %>% mutate(euc_allsize_end =  euc_small_end + euc_medium_end + euc_large_end)

x <- d_prop$grass_exotic_beginning 
y <- d_prop$euc_allsize_end
fit2 <- nls(y ~ a * exp(-b * x), start=list(a = 15, b = 2), algorithm="port")
summary(fit2) # 
a2 <- 8.7115
b2 <- 0.1642
fun2 <- function(x) {a2 * 2.718 ^ (-b2 * x)}

ggplot(d_prop, aes(grass_exotic_beginning, euc_allsize_end)) +
  geom_point()

pl2 <- d_prop %>% ggplot() +
  geom_point(aes(grass_exotic_beginning, euc_allsize_end), col = "grey", size = 2) +
  stat_function(fun = fun2, col = "black", size = 1, linetype = 2) +
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

#### FIG 2 ####

p_together <- grid.arrange(pl1, pl2, ncol = 2)
ggsave("figs/fig_2_ex_grasses_euc_growth.png", p_together, width = 8, height = 4)


# to summarize: it is the exotic grasses which have a consistent effect on eucolyptus growth, not the native ones.
# the seedling sizes can be combines into a new measure.

# Conclusion: presence of exotic grasses reduce eucalyptus growth.

