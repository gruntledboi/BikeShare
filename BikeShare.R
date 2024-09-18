library(tidyverse)
library(tidymodels)
library(vroom)
library(dplyr)
library(skimr)
library(DataExplorer)
library(GGally)
library(ggplot2)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")

##myCleanData <- trainData %>%
##filter(whereTrue) %>% # Selects rows according to conditions
  ##select(datetime:windspeed, -casual, -registered) %>% # Keeps or throws out columns
  ##mutate(logCount = log(count)) # Create a new variable


##DATA CLEANING (just for the training data)
myCleanData <- trainData %>%
  select(-casual, -registered) |> 
  mutate(count = log(count))


# my_recipe <- recipe(logCount ~ weather + temp + atemp + humidity, data=trainData) %>% # Set model formula and dataset
#   .[weather == 4, weather := 3] |> 
#   step_mutate(var1 = factor(weather, levels = c(1, 2, 3, 4))) %>% #Make something a factor
#   step_mutate(var1 = factor(season, levels = c(1, 2, 3, 4))) %>% #Make something a factor
#   step_mutate(newVar=var1*var2) %>% #Create a new variable
#   step_poly(var, degree = 2) %>% #Create polynomial expansion of var
#   step_date(datetime, features = "dow") %>% # gets day of week
#   step_time(datetime, features = c("hour")) %>% #create time variable
#   step_dummy(all_nominal_predictors()) %>% #create dummy variables
#   step_zv(all_predictors()) %>% #removes zero-variance predictors
#   step_corr(all_predictors(), threshold=0.5) %>% # removes > than .5 corr
#   step_rm(var) %>% #removes a variables
#   step_select(var, -var2) #selects columns

my_recipe <- 
  recipe(count ~ ., data = myCleanData) |>  # Set model formula and dataset
  step_mutate(weather = recode(weather, `4` = 3)) |> #now all "4's" in weather are 3's
  step_mutate(weather = factor(weather, levels = c(1, 2, 3))) |>  #Make something a factor
  step_mutate(season = factor(season, levels = c(1, 2, 3, 4))) |>  #Make something a factor
  step_time(datetime, features = "hour") |> 
  step_mutate(datetime_hour = factor(datetime_hour)) |> 
  #step_mutate(hour = hour(datetime)) |> 
  #step_mutate(hour = factor(hour, levels = 0:23)) |> 
  step_mutate(workingday = factor(workingday, levels = c(0, 1))) |>
  step_mutate(holiday = factor(holiday, levels = c(0, 1))) |> 
  step_zv(all_predictors()) |>  #removes zero-variance predictors
  step_rm(temp, atemp)


prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet
bake(prepped_recipe, new_data = testData)



## Define a Recipe as before
bike_recipe <- my_recipe

## Define a Model
lin_model <- linear_reg() %>%
set_engine("lm") %>%
set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
add_recipe(bike_recipe) %>%
add_model(lin_model) %>%
fit(data = myCleanData)

## Run all the steps on test data
lin_preds <- exp(predict(bike_workflow, new_data = testData))




# KAGGLE SUBMISSION

eng_kaggle_submission <- lin_preds %>%
bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) 


## Write out the file
vroom_write(x=eng_kaggle_submission, file="./FeatureEngPreds.csv", delim=",")




