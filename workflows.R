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

# dials -------------------------------------------------------------------

#dials grid
forest_grid_random   <- dials::grid_random(trees(range = c(300, 1000)), 
                                           min_n(range = c(1, 5)), 
                                           size = 6)

# parsnip -----------------------------------------------------------------

rand_model <- rand_forest(trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# workflows ---------------------------------------------------------------

base_wf <- workflow()

# Add a recipe
base_wf <- base_wf %>%
  add_recipe(simple_ames) 

# Add a model
base_wf <- base_wf %>%
  add_model(rand_model) 

# tune workflow
rand_tune_wf <- base_wf %>%
  tune_grid(
    resamples = cv_splits,
    grid = forest_grid_random,
    metrics = yardstick::metric_set(mae, rmse)
  )

# collect the metrics
rand_tune_wf %>% collect_metrics()

# plot the tuning process
autoplot(rand_tune_wf)

# numerically best models
show_best(rand_tune_wf) %>% select(-.estimator)
best_model <- select_best(rand_tune_wf, metric = "rmse")

best_model %>% select(trees, min_n)

# finalize workflow
final_wflow <- 
  base_wf %>%
  finalize_workflow(best_model %>% select(trees, min_n))

# fit on entire training set
final_fit <- final_wflow %>% fit(data = ames_train)

# make predictions
predict(final_fit, new_data = ames_test)

