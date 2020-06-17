###################
#Stepwise regression for variable Selection
# - Grass-Cover influencing Eucalyptus - 
# - BonaRes R
##################


#Load packages
library(tidyverse)
library(caret)
library(leaps) #Bernd needs to update his R (leaps is 3.6.3 - Bernd is 3.6.1)
library(MASS)
library(olsrr)
library(Hmisc)
library(plm)

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
var_info <- read.csv("data/Eucalypt_Variables.csv") #BJB likes to store it :)

#Load data
d <- read_csv("data/Euc_data.csv")
# View(d)
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
         native_total = NativeShrub_cover + NativePerennialFern_cover + NativePerennialHerb_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover) %>% # to make new variables
  rename(., Quadrat_no = `Quadrat no`,
         Landscape_position =  `Landscape position`, 
         Distance_to_Eucalypt_canopy = `Distance_to_Eucalypt_canopy(m)`,
         euc_sdlg_small = euc_sdlgs0_50cm,
         euc_sdlg_medium = `euc_sdlgs50cm-2m`,
         euc_sdlg_large = `euc_sdlgs>2m`) 

#Group columns into ecological senseful characterizations for further use
names(d)
d_pred <- names(d)[-c(1,4,5, 2,3, 6,7,33, 8,9, 24,26:28)]
d_block <- names(d)[c(1,4,5)] #study set-up
d_date <- names(d)[c(2,3)] #time | date
d_geog <- names(d)[c(6,7,33)] #geographic (east northing etc)
d_fact <- names(d)[c(8,9)] #factors
d_euc <- names(d)[c(24,26:28)] #eucalyptus
d_clim <- names(d)[c(29,30,31,32,37,38,39)] #climatic
d_soil <- names(d)[c(29,30,31,32,37,38,39)] #soil
d_veg <- names(d)[c(10:23,25,40:44)] #vegetation (includes: bareground, rock and litter cover)
d_veg_fine <- names(d)[c(10:23,25)] #raw vegetation (includes: bareground, rock and litter cover)
d_veg_coarse <- names(d)[c(40:44)] #new created, grouped vegetation

#In which columns are NAs and how many?
d_cols_withNA <- d %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))
d_onlyNArows <- d %>% filter_all(any_vars(is.na(.))) 
#Create a dataframe without NAs -> Deleting all rows containing NAs (only 5 rows to remove)
d_NA <- d %>% drop_na(.)

### First look at possible correlations to remove overfitting by autocorrelation
# All (none factors!)
cor_pearson <- rcorr(as.matrix(d)[ ,10:44] , type = "pearson")
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
write.csv(cor_pearson_flat, "results/Correlation_Pearson_complete_11052020.csv", row.names = F)

# where to create a cut-off in autocorrelation?
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
write.csv(cor_pearson_flat_pred, "results/Correlation_Pearson_predictor_11052020.csv", row.names = F)


# where to create a cut-off in autocorrelation?
summary(cor_pearson_pred$r[upper.tri(cor_pearson_pred$r)]) # Erzeugt verteilung der r?
cor.pred <- findCorrelation(cor_pearson_pred$r, cutoff = abs(0.7), verbose = T, exact = T, names = T) # gibt Spalten nummern den variablen mit Correlation über dem "cutoff"


### Find linear dependencies
d_pred_matrix <- as.matrix(d_NA[ ,dput(d_pred)])
d_pred_mean <- apply(d_pred_matrix, 2, mean)
d_pred_matrix.center <- sweep(d_pred_matrix, 2, d_pred_mean)
colnames(d_pred_matrix.center)

linear.pred <- detect.lindep(d_pred_matrix)
linear.pred <- attr(linear.pred, which = "names")
detect.lindep(d_pred_matrix[ ,-c(1:10)]) #removing the fine resolution vegetation data -> no linear dependency left
detect.lindep(d_pred_matrix[ ,-c(26:30)]) #removing the coarse resolution vegetation data -> no linear dependency left


### Stepwise Regression
# Auto-correlations have been detected in predictors
# Linear dependencies have been detected in predictors

# create a dataframe removing detected auto-correlations and linear dependencies and NAs
d_mod <- d_NA %>%
  dplyr::select(., -c(cor.pred, linear.pred[1:10]))
str(d_mod)

# Create new vector of remaining predictors and blocking etc.
names(d_mod)
mod.pred <- names(d_mod)[c(10:13,15,19:27)]
mod.resp <- names(d_mod)[c(14,16:18)]
mod.block <- names(d_mod)[c(1,4,5)]
mod.time <- names(d_mod)[c(2,3)]

### GRASS
null.model_grass <- lm(grass_total ~ 1, data = d_mod)
full.model_grass <- lm(reformulate(paste(mod.pred[-13], collapse = "+"), "grass_total"), data =d_mod)

#Base Package "stats"
step(full.model_grass, direction = "backward")
step(null.model_grass, direction = "forward", scope = formula(full.model_grass))
step(null.model_grass, direction = "both", scope = formula(full.model_grass))

#Package "MASS"
step.mod.grass <- stepAIC(null.model_grass, 
                    scope = formula(full.model_grass),
                    trace = TRUE,
                    direction = "both")
hist(step.mod.grass$residuals, main = "Histogram Grass")
qqnorm(step.mod.grass$residuals, main = "QQ - Grass")
qqline(step.mod.grass$residuals, col = "red")
plot(step.mod.grass$residuals ~ step.mod.grass$fitted.values, col = "blue", xlab = "fitted values", ylab = "residuals", main = "Residual - Grass")
abline(h = 0, col = "red")

step.mod.grass$coefficients
attr(step.mod.grass$terms, which = "predvars")


### ECUALYPTUS

# LOOP for eucalyptus related step.wise regression
step.res <- list()
for(i in 1:length(mod.resp)){
  #create null and full model
  null.model_tmp <- lm(noquote(paste(mod.resp[i], "~ 1")), data = d_mod)
  full.model_tmp <- lm(reformulate(paste(mod.pred, collapse = "+"), mod.resp[i]), data =d_mod)
  
  #step.wise regression
  step.mod.tmp <- stepAIC(null.model_tmp, 
                      scope = formula(full.model_tmp),
                      trace = F,
                      direction = "both")
  
  #visually test model assumptions
  hist(step.mod.tmp$residuals, main = paste("Histogram", mod.resp[i]))
  qqnorm(step.mod.tmp$residuals, main = paste("QQ", mod.resp[i]))
  qqline(step.mod.tmp$residuals, col = "red")
  
  plot(step.mod.tmp$residuals ~ step.mod.tmp$fitted.values, col = "blue", xlab = "fitted values", ylab = "residuals", main = paste("Residual", mod.resp[i]))
  abline(h = 0, col = "red")
  
  #extract needed parameters, summaires etc.
  coef <- step.mod.tmp$coefficients
  vars <- attr(step.mod.tmp$terms, which = "predvars")
  mod.sum <- summary(step.mod.tmp)
  print(mod.sum)
  assign(paste("coef", mod.resp[i], sep = ".") , value = coef)
  assign(paste("vars", mod.resp[i], sep = ".") , value = vars)
  assign(paste("summary", mod.resp[i], sep = ".") , value = mod.sum)
  
  #store results in list
  step.res[[paste(mod.resp[i])]] <- list(mod.coefficient = get(paste("coef", mod.resp[i], sep = "."), envir = .GlobalEnv),
               mod.vars = get(paste("vars", mod.resp[i], sep = "."), envir = .GlobalEnv),
               mod.summary = get(paste("summary", mod.resp[i], sep = "."), envir = .GlobalEnv),
               model.full = step.mod.tmp)
}

# Sometime the "intercept" is not significant -> the entire model is crap (at least what I know about linear models)
# https://rpubs.com/cyobero/187387 -> Nice "help file"

##########
##TOO COMPLICATED FOR ME?! Stepwise with training? I don't really get it...
#Package "caret"
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 50)
# Train the model
step.model <- train(formula(full.model_Euc_cover), data = d_mod,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)
