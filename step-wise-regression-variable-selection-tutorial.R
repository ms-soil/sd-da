###
#Stepwise Regression - Tutorial
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
###

#Load packages
library(tidyverse)
library(caret)
library(leaps) #Bernd needs to update his R (leaps is 3.6.3 - Bernd is 3.6.1)
library(MASS)

#####
###stepwise with MASS package and the "swiss" preloaded data
####

full.model <- lm(Fertility ~., data = swiss)
summary(full.model)

#Stepwise regression model
step.model <- stepAIC(full.model, 
                      direction = "both", #direction chooses the way of selection. can be "backward", "forward" or "both" 
                      trace = TRUE) #trace shows output of AIC with different parameter sets
summary(step.model)

#####
###stepwise with leaps package
####
?regsubsets
models <- regsubsets(Fertility ~. , data = swiss,
                     nvmax = 5, #nvmax sets the maximal number of predictors allow in the model
                     method = "seqrep") #sets the method. can be "forward", "backward" or "seqrep" = sequential replacement
summary(models)


#####
###stepwise with caret package (is able to combine MASS and leaps)
####
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)

step.model$results # shows the results. Typical model estimations "Güteparameter"

step.model$bestTune # gives/indicates the best model

summary(step.model$finalModel)


names(swiss)
##CAN IT WORK WITH INTERACTIONS?
step.model <- train(Fertility ~ Agriculture + Catholic + Examination + Education + Infant.Mortality, data = swiss,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)

step.model$results # shows the results. Typical model estimations "Güteparameter"

step.model$bestTune # gives/indicates the best model

summary(step.model$finalModel)


###########
#Variable selection procedure
#https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
#######
library(olsrr)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_all_possible(model) # parameter selection comparison for all possible parameter combinations
k <- ols_step_all_possible(model)
plot(k)


ols_step_best_subset(model) # selects subset of predictors that do best (HÄH?)
j <- ols_step_best_subset(model)
plot(j)
