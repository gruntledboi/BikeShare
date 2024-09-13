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

## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() |>  #Type of model
  set_engine("lm") |>  # Engine = What R function to use
  set_mode("regression") |>  # Regression just means quantitative response
  fit(formula = count ~ weather + temp + humidity + windspeed, data=trainData)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data = testData) # Use fit to predict
bike_predictions ## Look at the output

bike_predictions <- bike_predictions |> 
  slice(1:6493)

## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

kaggle_submission <-
  kaggle_submission |> 
  slice(1:6493)

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
