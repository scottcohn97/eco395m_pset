# Greenbuildings

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
library(vip)
library(rpart.plot)
library(baguette)

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

green_build <- 
  read_data("greenbuildings.csv") %>%
  # to snake case
  janitor::clean_names() %>%
  # Revenue per sq ft
  mutate(rev_sqft = rent * leasing_rate) 

# preview data
green_build %>% 
  select(-contains("id")) %>%  # remove ID and irrelevant variables
  modify_if(is.character, as.factor) %>%  # convert character vars to factors
  skim()  %>% 
  select(-starts_with("numeric.p")) # remove quartiles

# Some plots --------------------------------------------------------------

# distro rev_sq_ft
green_build %>%
  ggplot() + 
  geom_histogram(aes(x = rev_sqft), 
                 bins = 50,
                 fill = "dodgerblue", color = "black") +
  geom_vline(aes(xintercept = mean(rev_sqft)), 
             color = "tomato", linetype = 2) +
  # few outliers
  xlim(0, 12000) +
  labs(x = "Revenue by square foot (USD)", y = "Count") + 
  theme_clean() +
  facet_wrap(. ~ green_rating,
             labeller = labeller(green_rating = c("0" = "Not rated", "1" = "Green Rated")))

# tally by group (green rating)
green_build %>%
  mutate(green_rating = if_else(green_rating == 0, "No rating", "Green rating")) %>%
  group_by(green_rating) %>%
  tally() %>%
  rename(`Green Rating` = green_rating,
         Count = n) %>% 
  kable("pipe")

# Distro of revenue by class A
green_build %>%
  mutate(class_a = if_else(class_a == 0, "Other", "Class A")) %>%
  ggplot() + 
  geom_histogram(aes(x = rev_sqft, fill = factor(class_a)), 
                 bins = 50,
                 color = "black") +
  geom_vline(aes(xintercept = mean(rev_sqft)), 
             color = "green", linetype = 2) +
  # few outliers
  xlim(0, 12000) +
  labs(x = "Revenue by square foot (USD)", y = "Count",
       caption = "Note: Green = mean") + 
  scale_fill_brewer(palette = "Set1") +
  theme_clean() +
  theme(legend.title = element_blank())

# Splits and resamples  ---------------------------------------------------

# Split into train/test split
set.seed(395)
green_split <- initial_split(green_build, strata = rev_sqft)
green_train <- training(green_split)
green_test <- testing(green_split)

# v-fold
set.seed(3951)
green_folds <- vfold_cv(green_train, v = 3, strata = rev_sqft)
green_folds

# Feature Eng -------------------------------------------------------------

green_rec <- 
  recipe(rev_sqft ~ ., green_train) %>% 
  update_role(contains("id"), new_role = "ID") %>% # declare ID variables
  step_mutate(
    green_cert = case_when(
      leed == 1 ~ "leed",
      energystar == 1 ~ "energystar",
      leed == 0 & energystar == 0 ~ "none"
    )
  ) %>%
  step_rm(c(rent, leasing_rate, leed, energystar, green_rating, total_dd_07, city_market_rent)) %>% # remove confounders
  #step_bin2factor(c(green_rating)) %>% # binary to factor
  step_nzv(all_predictors(), freq_cut = 0, unique_cut = 0) %>% # remove variables with zero variances
  step_novel(all_nominal()) %>% # prepares test data to handle previously unseen factor levels 
  step_unknown(all_nominal()) %>% # categorizes missing categorical data (NA's) as `unknown`
  step_medianimpute(all_numeric(), -all_outcomes(), -has_role("ID"))  %>% # replaces missing numeric observations with the median
  step_dummy(all_nominal(), -has_role("ID")) # dummy codes categorical variables

# Model (DT) --------------------------------------------------------------

# create tuneable dt model
tree_spec <- decision_tree(
  cost_complexity = tune(), # 0 > cp > tune()
  tree_depth = tune(),      # max tree depth
  min_n = tune()            # smallest node allowed
) %>%
  set_engine("rpart") %>%   # set tree engine
  set_mode("regression")

tree_spec

# create grids of tuning params
tree_grid <- 
  grid_regular(
    cost_complexity(), 
    tree_depth(), 
    min_n(), 
    levels = 4
    )

tree_grid

# create workflow
wflow_tree <- 
  workflow() %>% 
  add_recipe(green_rec) %>%
  add_model(tree_spec)

# try all param values on resampled datasets
doParallel::registerDoParallel()

set.seed(3452)

tree_rs <- 
  tune_grid(
    wflow_tree,
    resamples = green_folds,
    grid = tree_grid, 
    metrics = metric_set(rmse, rsq, mae)
  )

tree_rs

# evaluate model
collect_metrics(tree_rs)

autoplot(tree_rs) + theme_clean()

lowest_tree_rmse <- select_best(tree_rs, "rmse")

# # choose "best" set of params to update and finalize model
# final_tree <- finalize_model(tree_spec, lowest_tree_rmse)
# 
# final_tree # see tuned hyperparams

## Out of sample performance

# finalize workflow
final_wf <- 
  wflow_tree %>% 
  finalize_workflow(lowest_tree_rmse)

final_wf

# fit the model (two ways, same thing)
final_rs <- last_fit(final_wf, green_split)

tree_rs %>%
  show_best(metric = "rmse")

# predict 
# predict(final_rs$.workflow[[1]], green_train[146, ])         

# look at test data
collect_metrics(final_rs)[,1:3] %>% kbl(digits = 3)


# what are the most important variables
final_tree_fit %>% 
  pull_workflow_fit() %>% 
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_clean()

# look at predictions
final_rs %>%
  collect_predictions() %>%
  ggplot(aes(rev_sqft, .pred)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  theme_clean() +
  coord_fixed()

# Model (KNN) -------------------------------------------------------------

# set model with tuneable hyperparams
knn_spec <-
  nearest_neighbor(
    mode = "regression",
    neighbors = tune("K")
  ) %>%
  set_engine("kknn")

# construct workflow
wflow_knn <-
  workflow() %>% 
  add_recipe(green_rec) %>%
  add_model(knn_spec)

knn_set <-
  parameters(wflow_knn) %>%
  # try k in 1:50
  update(K = neighbors(c(1, 50)))

set.seed(3952)
knn_grid <-
  knn_set %>%
  grid_max_entropy(size = 50)

knn_grid_search <-
  tune_grid(
    wflow_knn,
    resamples = green_folds,
    grid = knn_grid
  )

# choose best model
lowest_rmse_knn <- select_best(knn_grid_search, "rmse")

best_k <- as.numeric(lowest_rmse_knn$K)

# plot rmse x K
collect_metrics(knn_grid_search) %>%
  filter(.metric == "rmse") %>% 
  ggplot() + 
  geom_vline(aes(xintercept = best_k), linetype = 2, color = "tomato") +
  geom_point(aes(x = K, y = mean), color = "midnightblue", alpha = 0.8) +
  labs(y = "RMSE") +
  theme_clean()

## Out of sample performance

# finalize workflow
final_wf_knn <- 
  wflow_knn %>% 
  finalize_workflow(lowest_rmse_knn)

final_wf_knn

# fit the model 

last_fit(
  final_wf_knn,
  green_split
  ) %>%
  collect_metrics() %>% 
  select(1:3) %>%
  kbl(digits = 3)

# Model (Penalized regression / LASSO) --------------------------------------------

lasso_spec <- 
  linear_reg(
    penalty = tune(), 
    mixture = 1
    ) %>%
  set_engine("glmnet")

# construct workflow
wflow_lasso <-
  workflow() %>% 
  add_recipe(green_rec) %>%
  add_model(lasso_spec)

lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()
set.seed(3955)
lasso_grid <- 
  tune_grid(
    wflow_lasso,
    resamples = green_folds,
    grid = lambda_grid
  )

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lowest_rmse <- 
  lasso_grid %>%
  select_best("rmse")

final_lasso <- 
  finalize_workflow(
    wflow_lasso,
    lowest_rmse
  )

final_lasso %>%
  fit(green_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme_clean()

last_fit(
  final_lasso,
  green_split
  ) %>%
  collect_metrics() %>% 
  select(1:3) %>%
  kbl(digits = 3)

# session info -----
sessionInfo()

