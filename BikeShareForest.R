install.packages("ranger")
library(tidymodels)
library(poissonreg)
library(glmnet)
library(tidyverse)
library(vroom)
library(dplyr)
library(ranger)
library(rpart)

my_mod <- rand_forest(mtry = tune(),
                      min_n= tune(),
                      trees= 500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")




my_recipe <- 
  recipe(count ~ ., data = myCleanData) |>  # Set model formula and dataset
  step_mutate(weather = recode(weather, `4` = 3)) |> #now all "4's" in weather are 3's
  step_mutate(weather = factor(weather, levels = c(1, 2, 3), 
                               labels = c(1, 2, 3))) |>  #Make something a factor
  step_mutate(season = factor(season, levels = c(1, 2, 3, 4),
                              labels = c(1, 2, 3, 4))) |>  #Make something a factor
  step_time(datetime, features = "hour") |> 
  #step_mutate(hour = hour(datetime)) |> 
  step_mutate(datetime_hour = factor(datetime_hour, levels = 0:23, 
                                     labels = 0:23)) |> 
  step_mutate(workingday = factor(workingday, levels = c(0, 1),
                                  labels = c(0, 1))) |>
  step_mutate(holiday = factor(holiday, levels = c(0, 1),
                               labels = c(0, 1))) |> 
  step_zv(all_predictors()) |>  #removes zero-variance predictors
  step_dummy(all_nominal_predictors()) |>  #make dummy variables
  step_rm(atemp, datetime) |> 
  step_normalize(all_numeric_predictors())

# ## Penalized regression model
# preg_model <- linear_reg(penalty=tune(),
#                          mixture=tune()) %>% #Set model and tuning
#   set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)


## Set up grid of tuning values
grid_of_tuning_params <- grid_regular(mtry(range = c(1, 10)),
                                      min_n(),
                                      levels = 5)
## L^2 total tuning possibilities


## Set up K-fold CV
folds <- vfold_cv(myCleanData, v = 5, repeats = 1)


## Find best tuning parameters
CV_results <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse, mae, rsq))

## Finalize workflow and predict
bestTune <- CV_results %>%
  select_best(metric = "rmse")





final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = myCleanData)

## Predict
cv_preds <- exp(predict(final_wf, new_data = testData))


# KAGGLE SUBMISSION

forest_kaggle_submission <- cv_preds %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) 


## Write out the file
vroom_write(x = forest_kaggle_submission, 
            file="./ForestPreds.csv", delim=",")

