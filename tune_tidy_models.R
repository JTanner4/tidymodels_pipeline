library(tidymodels)


# data --------------------------------------------------------------------

data(ames)


# rsample -----------------------------------------------------------------

set.seed(1401)

# Save the split information for an 80/20 split of the data
ames_split <- rsample::initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

ames_train <-  rsample::training(ames_split)
ames_test  <-  rsample::testing(ames_split)

# cv splits
cv_splits <- vfold_cv(ames_train, v = 10, strata = Sale_Price)


# recipes -----------------------------------------------------------------

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_normalize(all_numeric_predictors()) %>% #subtracts mean, dividing by sd
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) 


# tune --------------------------------------------------------------------

# parsnip model definition
rand_model <- rand_forest(trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

#dials grid
forest_grid_random   <- dials::grid_random(trees(range = c(300, 1000)), 
                                           min_n(range = c(1, 5)), 
                                           size = 6)
# tune models
rand_tune  <- tune_grid(rand_model, 
                        simple_ames, 
                        resamples = cv_splits, 
                        grid = forest_grid_random)
rand_tune

rand_tune$.metrics

#collect metrics
rand_tune %>% 
  collect_metrics()

#select the best params
rand_tune %>%
  select_best(metric = "rmse")

#prep and bake recipe
temp_train <- bake(prep(simple_ames, training = ames_train), new_data = NULL)

#fit with the best parameters
rf_fit <- rand_forest(trees = 590, min_n = 3) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  fit(Sale_Price ~ ., data = temp_train)
