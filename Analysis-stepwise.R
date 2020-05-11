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

#Load and view variable information
var_info <- read.csv("data/Eucalypt_Variables.csv") #BJB likes to store it :)

#Load data
d <- read_csv("data/Euc_data.csv")
# View(d)
head(d)
names(d)

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
names(d)
str(d)

#why is it not working?
d <- d %>%
  mutate_if(. , is.character, as.factor)
str(test)
names(d)
###Stepwise regression for parameter selection

# EXHAUSTIVE LONG LIST OF PARAMETERS :/
# SurveyID + Date + Season + Property + Quadrat_no + Easting + Northing + Aspect + Landscape_position + ExoticAnnualGrass_cover + ExoticAnnualHerb_cover +  ExoticPerennialHerb_cover + ExoticPerennialGrass_cover + ExoticShrub_cover + NativePerennialFern_cover + NativePerennialGrass_cover + NativePerennialHerb_cover + NativePerennialGraminoid_cover + NativeShrub_cover + BareGround_cover + Litter_cover + MossLichen_cover + Rock_cover + Euc_canopy_cover + Distance_to_Eucalypt_canopy + euc_sdlg_small + euc_sdlg_medium + euc_sdlg_large + annual_precipitation + precipitation_warmest_quarter + precipitation_coldest_quarter + PET + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul + aridity + grass_total + herb_total + shrub_total + exotic_total + native_total

#Full Model
#removing SurveyID, Property, Quadrat no, as they are "random" or blocking factors, Easting, Nothing, Euc related (as this is the response)
full.model_Euc_cover <- lm(Euc_canopy_cover ~ Season + Aspect + Landscape_position + ExoticAnnualGrass_cover + ExoticAnnualHerb_cover +  ExoticPerennialHerb_cover + ExoticPerennialGrass_cover + ExoticShrub_cover + NativePerennialFern_cover + NativePerennialGrass_cover + NativePerennialHerb_cover + NativePerennialGraminoid_cover + NativeShrub_cover + BareGround_cover + Litter_cover + MossLichen_cover + Rock_cover + Distance_to_Eucalypt_canopy + annual_precipitation + precipitation_warmest_quarter + precipitation_coldest_quarter + PET + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul + aridity + grass_total + herb_total + shrub_total + exotic_total + native_total , data = d)
summary(full.model_Euc_cover)

#Stepwise regression model
step.model <- stepAIC(full.model_Euc_cover, 
                      direction = "both", #direction chooses the way of selection. can be "backward", "forward" or "both" 
                      trace = TRUE) #trace shows output of AIC with different parameter sets
summary(step.model)

####das unten sollte wiei iwann gehen...


# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model_Euc_cover <- train(Euc_canopy_cover ~ PET + grass_total, data = d,
                    method = "leapBackward", 
                    na.action = na.omit,
                    trControl = train.control)
names(d)
?train
step.model$results # shows the results. Typical model estimations "Güteparameter"

step.model$bestTune # gives/indicates the best model

summary(step.model$finalModel)

