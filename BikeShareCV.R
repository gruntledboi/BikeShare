library(tidymodels)
library(poissonreg) #if you want to do penalized, poisson regression
library(glmnet)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")


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
  step_rm(temp, atemp, datetime) |> 
  step_normalize(all_numeric_predictors())






## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(preg_model)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 50) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(myCleanData, v = 50, repeats = 1)

## Run the CV
CV_results <- preg_wf %>%
tune_grid(resamples = folds,
          grid = grid_of_tuning_params,
          metrics = metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")


## Finalize the Workflow & fit it
final_wf <-
preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data = myCleanData)

## Predict
cv_preds <- exp(predict(final_wf, new_data = testData))


# KAGGLE SUBMISSION

cv_kaggle_submission <- cv_preds %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) 


## Write out the file
vroom_write(x = cv_kaggle_submission, 
            file="./CVPreds.csv", delim=",")