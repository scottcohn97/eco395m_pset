# California housing

library(tidyverse)
library(ggthemes)
library(tictoc)
library(kableExtra)
library(modelsummary)
library(skimr)
library(estimatr)
library(janitor)
library(tidymodels)
library(patchwork)
library(ggmap)
library(maps)
library(mapdata)

set.seed(395)

# funcs
read_data <- function(df) {
  #' read data from git url
  #' INPUT: data set name
  #' OUTPUT: dataframe
  full_path <- paste("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/", 
                     df, sep = "")
  df <- read_csv(full_path)
  return(df)
}

housing <- 
  read_data("CAhousing.csv") %>%
  janitor::clean_names()

skim(housing)



# cali <- ggmap(ggmap::get_stamenmap(location ='California', zoom = 6))
# ggmap(cali)

# raw map ----

states <- map_data("state")
ca_df <- subset(states, region == "california")

ca_base <- 
  ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = ca_df, 
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white")  + 
  geom_polygon(data = ca_county, 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = "gray") +
  geom_polygon(data = ca_df,
               aes(x = long, y = lat, group = group), 
               color = "black", fill = NA)

ca_base + 
  geom_point(data = housing, 
             aes(x = longitude, y = latitude, 
                 color = medianHouseValue, size = population), 
             alpha = 0.4) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_map() +
  scale_color_distiller(palette = "Paired", labels = comma) +
  labs(title = "California Housing",
       x = "Longitude", y = "Latitude",
       color = "Median House Value (in $USD)", 
       size = "Population")


# Cleaning and test/train ----
  
set.seed(395)

# Create a split object
housing_split <- initial_split(housing, prop = 0.75, strata = medianHouseValue)

# Build training data set
housing_train <- housing_split %>% training()

# Build testing data set
housing_test <- housing_split %>% testing()

# vfold
housing_vfold <- vfold_cv(housing_train, v = 10, strata = medianHouseValue)
 

# linear regression model ----

# specify a linear model
lm_model <- 
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# define a recipe - FEATURE ENG
lm_recipe <- 
  # fit on all variables
  recipe(medianHouseValue ~ ., data = housing_train) %>%
  # log price
  step_log(medianHouseValue) %>%
  # standardize
  step_range(totalBedrooms, totalRooms, population, housingMedianAge, medianIncome) %>%
  # specify tuning hyperparameters
  step_ns(longitude, deg_free = tune("long df")) %>% 
  step_ns(latitude,  deg_free = tune("lat df"))

# grid to tun long/lat
grid_vals <- seq(2, 22, by = 2)
# A regular grid:
spline_grid <- expand.grid(`long df` = grid_vals, `lat df` = grid_vals)

# which hyper param to tune
housing_param <- 
  lm_recipe %>% 
  parameters() %>% 
  update(
    `long df` = spline_degree(), 
    `lat df` = spline_degree()
  )

housing_param

# create a workflow
lm_workflow <- 
  workflow() %>% 
  # specify engine
  add_model(lm_model) %>% 
  # specify recipe
  add_recipe(lm_recipe)

tic()
lm_res <- 
  lm_workflow %>%
  tune_grid(resamples = housing_vfold, grid = spline_grid)
toc()

lm_est <- collect_metrics(lm_res)

lm_rmse_vals <- 
  lm_est %>% 
  dplyr::filter(.metric == "rmse") %>% 
  arrange(mean)

lm_final <-
  lm_rmse_vals %>%
  filter(.metric == "rmse") %>%
  filter(mean == min(mean))


lm_final_workflow <- 
  lm_workflow %>% 
  finalize_workflow(lm_final)

# fit the model
lm_fit <- 
  # use the workflow with the best model ...
  lm_final_workflow %>% 
  # ... to fit the test set
  last_fit(split = housing_split)

# Obtain performance metrics on test data
lm_fit %>% collect_metrics()
 

  
housing_train %>% 
  dplyr::select(medianHouseValue, longitude, latitude) %>% 
  tidyr::pivot_longer(cols = c(longitude, latitude), 
                      names_to = "predictor", values_to = "value") %>% 
  ggplot(aes(x = value, medianHouseValue)) + 
  geom_point(alpha = .2) + 
  geom_smooth(se = FALSE, method = lm, formula = y ~ splines::ns(x, df = 3),  col = "tomato") +
  geom_smooth(se = FALSE, method = lm, formula = y ~ splines::ns(x, df = 16)) +
  scale_y_log10() +
  theme_clean() +
  facet_wrap(~ predictor, scales = "free_x")
 

  
# Obtain test set predictions data frame
lm_results <- 
  lm_fit %>% 
  # save pred results
  collect_predictions()
 
  
    
# plot pred v actual
lm_results %>%
  ggplot(aes(x = .pred, y = medianHouseValue)) +
  geom_point(color = '#006EA1', alpha = 0.25)  +
  geom_abline(intercept = 0, slope = 1, color = 'tomato') +
  labs(title = 'Linear Regression Results - Test Set',
       x = 'Predicted Price',
       y = 'Actual Price') + 
  theme_clean()





 
# knn regression model ----

# specify a knn model
knn_model <- 
  # specify hyperparameters
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine('kknn') %>% 
  set_mode('regression') %>%
  translate()

# define a recipe
knn_recipe <- 
  # fit on all variables
  recipe(medianHouseValue ~ ., data = housing_train) %>%
  # log price
  step_log(medianHouseValue) %>%
  # standardize
  step_range(totalBedrooms, totalRooms, population, housingMedianAge, medianIncome) %>%
  # specify tuning hyperparameters
  step_ns(longitude, deg_free = tune("long df")) %>% 
  step_ns(latitude,  deg_free = tune("lat df"))

# create a workflow
knn_workflow <- 
  workflow() %>% 
  # specify engine
  add_model(knn_model) %>% 
  # specify recipe
  add_recipe(knn_recipe)

knn_param <- 
  knn_workflow %>% 
  # how to tune hyperparams
  parameters() %>% 
  update(
    `long df` = spline_degree(c(2, 18)), 
    `lat df` = spline_degree(c(2, 18)),
    neighbors = neighbors(c(3, 50)),
    weight_func = weight_func(values = c("rectangular", "inv", "gaussian", "triangular"))
  )

ctrl <- control_bayes(verbose = TRUE)

tic()
knn_search <- 
  tune_bayes(knn_workflow, resamples = housing_vfold, initial = 5, iter = 20,
             param_info = knn_param, control = ctrl)
toc()

knn_final <-
  knn_search %>%
  collect_metrics() %>% 
  dplyr::filter(.metric == "rmse") %>% 
  filter(mean == min(mean))


knn_final_workflow <- 
  knn_workflow %>% 
  finalize_workflow(knn_final)

# fit the model
knn_fit <- 
  # use the workflow with the best model ...
  knn_final_workflow %>% 
  # ... to fit the test set
  last_fit(split = housing_split)

# Obtain performance metrics on test data
knn_fit %>% collect_metrics()

# Obtain test set predictions data frame
knn_results <- 
  knn_fit %>% 
  # save pred results
  collect_predictions()
 

# plot pred v actual
knn_results %>%
  ggplot(aes(x = .pred, y = medianHouseValue)) +
  geom_point(color = '#006EA1', alpha = 0.25)  +
geom_abline(intercept = 0, slope = 1, color = 'tomato') +
  labs(title = 'KNN Regression Results - Test Set',
       x = 'Predicted Price',
       y = 'Actual Price') + 
  theme_clean() 
 
# then use this map
knn_results <- 
  knn_results %>%
    bind_cols(housing_test) %>% 
    rename(medianHouseValue_log = `medianHouseValue...4`,
           medianHouseValue = `medianHouseValue...14`) 

knn_results %>% 
  arrange(medianHouseValue_log) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = medianHouseValue_log)) + 
  geom_segment(aes(xend = id, yend = .pred), alpha = .2) +
  geom_point(aes(y = .pred), shape = 1) + 
  geom_point(color = "tomato", shape = 1, alpha = 0.5) +
  labs(x = "", y = "Logged median house value") + 
  theme_clean()

# session info -----
sessionInfo()
