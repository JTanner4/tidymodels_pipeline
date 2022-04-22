#### Intro to tidymodels ####

list_packages <- c("tidymodels", "ranger", "skimr", "rpart", "DALEX", "DALEXtra", "plumber", "ggplot2")
new_packages  <- list_packages[!(list_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

# libraries ---------------------------------------------------------------

library(tidymodels)

# data --------------------------------------------------------------------

data(ames)


# rsample -----------------------------------------------------------------

set.seed(1401)

# Save the split information for an 80/20 split of the data
ames_split <- rsample::initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_split

#https://rsample.tidymodels.org/
ames_train <-  rsample::training(ames_split)
ames_test  <-  rsample::testing(ames_split)


# recipes -----------------------------------------------------------------

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_normalize(all_numeric_predictors()) %>% #subtracts mean, dividing by sd
  step_other(Neighborhood, threshold = 0.05) %>% # Infrequent values of neighborhood are simply changed to "other" if they appear less than 5% of the time
  step_dummy(all_nominal_predictors()) 


simple_ames

simple_ames <- prep(simple_ames, training = ames_train)
simple_ames

bake(simple_ames, new_data = NULL) 

# adding feature engineering
area_cols <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_1", "BsmtFin_SF_2",
               "Bsmt_Unf_SF", "Total_Bsmt_SF", "First_Flr_SF", "Second_Flr_SF", "Gr_Liv_Area",
               "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch",
               "Pool_Area", "Garage_Area")

feat_ames <- 
  recipe(Sale_Price ~ .,
         data = ames_train) %>%
  step_other(Neighborhood, threshold = 0.05) %>% 
  step_mutate(total_area = rowSums(across(all_of(area_cols)))) %>% # Ge the total area in the house
  step_dummy(all_nominal_predictors())  %>%
  step_select(-all_of(area_cols)) %>%
  step_nzv(all_predictors()) # Remove variable hat are highly sparse/unbalanced


feat_ames

feat_ames <- prep(feat_ames, training = ames_train)
feat_ames

bake(feat_ames, new_data = NULL)
bake(feat_ames, new_data = ames_test)


# parsnip -----------------------------------------------------------------
# https://www.tidymodels.org/find/parsnip/

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

# use the recipe
prepped_ames <- prep(simple_ames, training = ames_train)
temp_train   <- bake(prepped_ames, new_data = ames_train)

rf_fit <- rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  fit(Sale_Price ~ ., data = temp_train)

rf_fit

# predict test data

rf_fit %>% predict(new_data = bake(prepped_ames, new_data = ames_test))

