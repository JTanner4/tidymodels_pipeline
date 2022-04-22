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

##adding feature engineering
area_cols <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_1", "BsmtFin_SF_2",
               "Bsmt_Unf_SF", "Total_Bsmt_SF", "First_Flr_SF", "Second_Flr_SF", "Gr_Liv_Area",
               "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch",
               "Pool_Area", "Garage_Area")

ames_rec_feat <- 
  recipe(Sale_Price ~ .,
         data = ames_train) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_mutate(total_area = rowSums(across(all_of(area_cols)))) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())  %>%
  step_select(-all_of(area_cols)) %>%
  step_nzv(all_predictors())

ames_rec_norm <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_normalize(all_numeric_predictors()) %>% #subtracts mean, dividing by sd
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) 


# workflowsets ------------------------------------------------------------

rf_specs <- rand_forest(mode = "regression", 
                        mtry = tune(), 
                        trees = tune(), 
                        min_n = tune()) %>%
  set_engine("ranger")

# Add an lm model
lm_specs <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

wf_set <- 
  workflow_set(
    preproc = list(normalized = ames_rec_norm, 
                   features   = ames_rec_feat),
    models  = list(rf   = rf_specs, 
                   lm   = lm_specs)
  )

wf_set

# how you probably want to use workflowsets

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

# Applying tune_grid to the workflow_sets
grid_results <-
  wf_set %>%
  workflow_map(
    seed = 1401,
    resamples = cv_splits,
    grid = 4,
    control = grid_ctrl
  )

grid_results

grid_results %>% collect_metrics()
grid_results %>% rank_results()

autoplot(grid_results)

# adding more control to your grid
forest_grid_entropy  <- dials::grid_max_entropy(trees(range = c(1000, 1200)), 
                                                min_n(range = c(1, 5)),
                                                mtry(range = c(1, 3)),
                                                size = 3)

wf_set

wf_set <- wf_set %>%
  option_add(grid = forest_grid_entropy, id = c("normalized_rf", "features_rf"))
wf_set$option[[1]]

#Applying tune_grid to workflow_sets

grid_results <-
  wf_set %>%
  workflow_map(
    seed = 1401,
    resamples = cv_splits,
    control = grid_ctrl,
  )
grid_results
grid_results %>% collect_metrics()

autoplot(grid_results)
grid_results %>% 
  autoplot(id = "normalized_rf")

# select the workflow id with best parameter set
best_id <- grid_results %>% rank_results() %>% filter(.metric == "rmse") %>% 
  select(model, .config, rmse = mean, rank, wflow_id) %>%
  filter(rank == 1) %>%
  select(wflow_id)

grid_results %>%
  extract_workflow_set_result(best_id$wflow_id) %>%
  select_best(metric = "rmse")

grid_results %>%
  extract_workflow(best_id$wflow_id)

# select the actual parameter set with the best rmse
best_result <- grid_results %>%
  extract_workflow_set_result(best_id$wflow_id)%>%
  select_best(metric = "rmse")

# finalize and train on the whole train split using the best parameter values
test_results <- 
  grid_results %>% 
  extract_workflow(best_id$wflow_id) %>% 
  finalize_workflow(best_result) %>%
  last_fit(split = ames_split)

test_results %>% 
  collect_predictions() %>%
  ggplot(aes_string(x = "Sale_Price", y = ".pred")) +
  geom_abline(col = "green", lty = 2) +
  geom_point(alpha = 0.5) +
  coord_obs_pred() +
  labs(x = "observed", y = "predicted")
