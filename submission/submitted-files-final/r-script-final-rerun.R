#### PART 1 ####

#### Project info / libraries / instructions / variables / data ####
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

#### ADDED ON RERUN START ####

# library("here")
# 
# new_euc_data <- 
#   readr::read_csv(here::here("data/euc_specification_data_wide.csv"))
# 
# unique(new_euc_data$Season)
# 
# as_tibble(new_euc_data)
# names(new_euc_data)
# 
# # generate grass_exotic_beginning
# new_euc_data_2 <- new_euc_data[,c("SurveyID", "Date", "Season", "Property", "ExoticAnnualGrass_cover", "ExoticPerennialGrass_cover")] %>% 
#   mutate(grass_exotic_beginning = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover)
# new_euc_data_2



#### ADDED ON RERUN END ####

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


# reset workspace before next part 
rm(list = ls())

#### PART 2 ####

#### Stepwise regression for variable Selection ####
# - Grass-Cover influencing Eucalyptus - 
# - BonaRes R

#Load packages
library(tidyverse)
library(Hmisc)
library(plm)
library(caret)
library(MASS)
library(lmtest)
library(nlme)
library(predictmeans)
library(MuMIn)

#Create needed helper functions
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Load and view variable information
var_info <- read.csv("data/Eucalypt_Variables.csv")


#Load data
d <- read_csv("data/Euc_data.csv")

head(d)
names(d)
str(d)
###Create new variables for analysis
#Aridity P/PET - according to UNEP 1992
#summarize plant functional groups (irresepctive of native & exotic)
#summarize plants by either being native or exotic (irrespective of plant functional group)
d <- d %>%  
  mutate(aridity = (annual_precipitation / PET)*100,
         grass_total = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover,
         herb_total = ExoticAnnualHerb_cover + ExoticPerennialHerb_cover + NativePerennialHerb_cover,
         shrub_total = ExoticShrub_cover + NativeShrub_cover,
         exotic_total = ExoticAnnualGrass_cover + ExoticShrub_cover + ExoticPerennialGrass_cover + ExoticAnnualHerb_cover + ExoticPerennialHerb_cover,
         native_total = NativeShrub_cover + NativePerennialFern_cover + NativePerennialHerb_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover,
         exotic_grass = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover,
         euc_sdlg_all = euc_sdlgs0_50cm + `euc_sdlgs50cm-2m` + `euc_sdlgs>2m`) %>% # to make new variables
  rename(., Quadrat_no = `Quadrat no`,
         Landscape_position =  `Landscape position`, 
         Distance_to_Eucalypt_canopy = `Distance_to_Eucalypt_canopy(m)`,
         euc_sdlg_small = euc_sdlgs0_50cm,
         euc_sdlg_medium = `euc_sdlgs50cm-2m`,
         euc_sdlg_large = `euc_sdlgs>2m`) 

#Group columns into ecological senseful characterizations for further use
names(d)
d_pred <- names(d)[-c(1,4,5, 2,3, 6,7,33, 8,9, 24,26:28, 46)]
d_block <- names(d)[c(1,4,5)] #study set-up
d_date <- names(d)[c(2,3)] #time | date
d_geog <- names(d)[c(6,7,33)] #geographic (east northing etc)
d_fact <- names(d)[c(8,9)] #factors
d_euc <- names(d)[c(24,26:28, 46)] #eucalyptus
d_clim <- names(d)[c(29,30,31,32,37,38,39)] #climatic
d_soil <- names(d)[c(34:36)] #soil
d_veg <- names(d)[c(10:23,25,40:45)] #vegetation (includes: bareground, rock and litter cover)
d_veg_fine <- names(d)[c(10:23,25)] #raw vegetation (includes: bareground, rock and litter cover)
d_veg_coarse <- names(d)[c(40:45)] #new created, grouped vegetation


#Check for NAs in dataframe
d_cols_withNA <- d %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))
d_onlyNArows <- d %>% filter_all(any_vars(is.na(.))) 
#Create a dataframe without NAs -> Deleting all rows containing NAs (only 5 rows to remove)
d_NA <- d %>% drop_na(.)

### Check for possible correlations to remove overfitting by autocorrelation
# All (none factors!)
cor_pearson <- rcorr(as.matrix(d)[ ,10:46] , type = "pearson")
cor_pearson_flat <- flattenCorrMatrix(cormat = cor_pearson$r, pmat = cor_pearson$P)
cor_pearson_flat <- cor_pearson_flat %>%
  mutate(., cor_class = case_when(
    abs(cor) <= 0.3  ~ "negligeble",
    abs(cor) >0.3 & abs(cor) <= 0.5 ~ "low",
    abs(cor) >0.5 & abs(cor) <= 0.7 ~ "medium",
    abs(cor) >0.7 & abs(cor) <= 0.9 ~ "high",
    abs(cor) >0.9 & abs(cor) <= 1.0 ~ "very high",
  )
  ) 
cor_pearson_flat <- cor_pearson_flat[order(cor_pearson_flat$cor_class), ]
write.csv(cor_pearson_flat, "results/Correlation_Pearson_complete_24072020.csv", row.names = F)

# remove variables with R² > 0.7
summary(cor_pearson$r[upper.tri(cor_pearson$r)]) # Erzeugt verteilung der r?
cor.all <- findCorrelation(cor_pearson$r, cutoff = abs(0.7), verbose = T, exact = T, names = T) # gibt Spalten nummern den variablen mit Correlation über dem "cutoff"

# Only Predictors [NO DIFFERENCE BETWEEN PREDICTORS OR ALL IN OUTPUT]
cor_pearson_pred <- rcorr(as.matrix(d)[ ,d_pred] , type = "pearson")
cor_pearson_flat_pred <- flattenCorrMatrix(cormat = cor_pearson_pred$r, pmat = cor_pearson_pred$P)
cor_pearson_flat_pred <- cor_pearson_flat_pred %>%
  mutate(., cor_class = case_when(
    abs(cor) <= 0.3  ~ "negligeble",
    abs(cor) >0.3 & abs(cor) <= 0.5 ~ "low",
    abs(cor) >0.5 & abs(cor) <= 0.7 ~ "medium",
    abs(cor) >0.7 & abs(cor) <= 0.9 ~ "high",
    abs(cor) >0.9 & abs(cor) <= 1.0 ~ "very high",
  )
  ) 
cor_pearson_flat_pred <- cor_pearson_flat_pred[order(cor_pearson_flat_pred$cor_class), ]
write.csv(cor_pearson_flat_pred, "results/Correlation_Pearson_predictor_24072020.csv", row.names = F)


# remove variables with R² > 0.7
summary(cor_pearson_pred$r[upper.tri(cor_pearson_pred$r)])
cor.pred <- findCorrelation(cor_pearson_pred$r, cutoff = abs(0.7), verbose = T, exact = T, names = T)

### Check for linear dependencies
d_pred_matrix <- as.matrix(d_NA[ ,dput(d_pred)])
d_pred_mean <- apply(d_pred_matrix, 2, mean)
d_pred_matrix.center <- sweep(d_pred_matrix, 2, d_pred_mean)
colnames(d_pred_matrix.center)

linear.pred <- detect.lindep(d_pred_matrix.center)
linear.pred <- attr(linear.pred, which = "names")
detect.lindep(d_pred_matrix.center[ ,-c(1:10)]) #removing the fine resolution vegetation data -> no linear dependency left
detect.lindep(d_pred_matrix.center[ ,-c(26:31)]) #removing the coarse resolution vegetation data -> no linear dependency left


### Stepwise Regression to find best model parameters
# Auto-correlations have been detected in predictors
# Linear dependencies have been detected in predictors

# create a dataframe removing detected auto-correlations and linear dependencies and NAs
d_mod <- d_NA %>%
  dplyr::select(., -c(cor.pred[cor.pred!="exotic_grass"], linear.pred[1:10]))
str(d_mod)

# Create new vector of remaining predictors and blocking etc.
names(d_mod)
mod.pred <- names(d_mod)[c(10:13,15,19:27)]
mod.resp <- names(d_mod)[c(14,16:18,28)]
mod.block <- names(d_mod)[c(1,4,5)]
mod.time <- names(d_mod)[c(2,3)]

# Stepwise-Regression - GRASS
null.model_grass <- lm(exotic_grass ~ 1, data = d_mod)
full.model_grass <- lm(reformulate(paste(mod.pred[mod.pred!="exotic_grass"], collapse = "+"), "exotic_grass"), data =d_mod)

#Stepwise regression - Package "MASS"
step.mod.grass <- stepAIC(null.model_grass, 
                          scope = formula(full.model_grass),
                          trace = TRUE,
                          direction = "both")
#Look at the (allready standardized - by default setting) residuals
hist(step.mod.grass$residuals, main = "Histogram Grass")
qqnorm(step.mod.grass$residuals, main = "QQ - Grass")
qqline(step.mod.grass$residuals, col = "red")
plot(step.mod.grass$residuals ~ step.mod.grass$fitted.values, col = "blue", xlab = "fitted values", ylab = "residuals", main = "Residual - Grass")
abline(h = 0, col = "red")
bptest(step.mod.grass) #if p<0.05 variances are heteroscedastic


###
# Model with selected parameters from stepwise
###

# Linear Mixed Effect Model - GRASS
step.mod.grass$call$formula #formula of stepwise regression model -> add Season as fixed factor to it
grass.formula <- exotic_grass ~  PET + Rock_cover + Litter_cover + BareGround_cover + 
  MossLichen_cover + SRad_Jul + MrVBF + U_ppm + Season
grass.mod <- lme(grass.formula, random = (~ 1 | Property/Quadrat_no), data = d_mod)
summary(grass.mod)
hist(grass.mod$residuals, main = paste("Histogram Exotic Grass"))
residplot(grass.mod, level = 2) #visualize residuals
r.squaredGLMM(grass.mod)


grass.mod2 <- lme(grass.formula, random = (~ 1 | Property/Quadrat_no), weights = varExp(form = ~ Rock_cover + Litter_cover + BareGround_cover + MossLichen_cover),data = d_mod)
stat_grass.mod2 <- summary(grass.mod2)
hist(grass.mod2$residuals, main = paste("Histogram Exotic Grass"))
residplot(grass.mod2, level = 2) # does a resid plot externally for mixed models
r.squaredGLMM(grass.mod2)
AIC(grass.mod, grass.mod2) # Variance structure reduces AIC by ~150
grass.mod.output <- stat_grass.mod2[["tTable"]]
grass.mod.output <- round(grass.mod.output, 3)
write.csv(grass.mod.output, "results/Grass_model_output_24072020.csv")

# Residual plot per predictor
d_grass <- subset(d_mod, select = all.vars(grass.formula))
d_grass$resids_calc <- residuals(grass.mod2)
d_grass$pred_calc <- predict(grass.mod2)

d_grass %>% 
  gather(key = "iv", value = "x", -exotic_grass, -pred_calc, -resids_calc) %>%  # Get data into shape
  ggplot(aes(x = x, y = exotic_grass)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = pred_calc), alpha = .2) +
  geom_point(aes(color = resids_calc)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = pred_calc), shape = 1) +
  facet_grid(~ iv, scales = "free_x") +  # Split panels here by `iv`
  theme_bw()



#### from Bernd ####

###
# Prediction (using the newly provided test data?)
###
new_euc_data <- read_csv("data/euc_specification_data_wide.csv")
glimpse(new_euc_data)

#do exactly the same manipulations for the predict.df as was done for the model.df
d_new <- new_euc_data %>%  
  mutate(aridity = (annual_precipitation / PET)*100,
         grass_total = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover,
         herb_total = ExoticAnnualHerb_cover + ExoticPerennialHerb_cover + NativePerennialHerb_cover,
         shrub_total = ExoticShrub_cover + NativeShrub_cover,
         exotic_total = ExoticAnnualGrass_cover + ExoticShrub_cover + ExoticPerennialGrass_cover + ExoticAnnualHerb_cover + ExoticPerennialHerb_cover,
         native_total = NativeShrub_cover + NativePerennialFern_cover + NativePerennialHerb_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover,
         exotic_grass = ExoticAnnualGrass_cover + ExoticPerennialGrass_cover,
         euc_sdlg_all = euc_sdlgs0_50cm + `euc_sdlgs50cm-2m` + `euc_sdlgs>2m`) %>% # to make new variables
  rename(., Quadrat_no = `Quadrat no`,
         Landscape_position =  `Landscape position`, 
         Distance_to_Eucalypt_canopy = `Distance_to_Eucalypt_canopy(m)`,
         euc_sdlg_small = euc_sdlgs0_50cm,
         euc_sdlg_medium = `euc_sdlgs50cm-2m`,
         euc_sdlg_large = `euc_sdlgs>2m`) 

d_mod_new <- d_new %>%
  dplyr::select(., -c(cor.pred[cor.pred!="exotic_grass"], linear.pred[1:10])) %>%
  dplyr::select(., -c("total_seedlings (temp)"))
#check if both prdict.df and model.df have similar column names
all(names(d_mod) %in% names(d_mod_new)) #TRUE -> but in new_data we have the "total_seedlings (temp)" which is not in our model.df

#do the prediction
library(nlme)
predict(grass.mod, type = "link", newdata = d_mod_new, drop = TRUE)
predict.lme(grass.mod2, level = 0, newdata = d_mod_new)
grass.formula
unique(d_mod$Season)
unique(d_mod_new$Season)
str(d_mod)
str(d_mod_new)
?predict.lme

predict(grass.mod2, newdata = d_mod_new, se.fit = F, level = 0)
unique(d_mod$Season)
unique(d_mod_new$Season)
