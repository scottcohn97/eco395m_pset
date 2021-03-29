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

skim(green_build)


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



# include rent/leasing in model ? No, right?

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

# Feature Eng -------------------------------------------------------------

# TODO feature engineering

# Only select var I wanna use...

# Model (baseline) --------------------------------------------------------

# TODO construct a baseline model -- maybe linear ? 

# Model (DT) --------------------------------------------------------------

# Split into train/test split
set.seed(395)
green_split <- initial_split(green_build, strata = rev_sqft)
green_train <- training(green_split)
green_test <- testing(green_split)

# v-fold
set.seed(3951)
green_folds <- vfold_cv(green_train, strata = rev_sqft)
green_folds

# create tuneable dt model
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_spec

# params to try for tree
tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid

# try all param values on resampled datasets

doParallel::registerDoParallel()

set.seed(3452)
tree_rs <- tune_grid(
  tree_spec,
  rev_sqft ~ .,
  resamples = green_folds,
  grid = tree_grid,
  metrics = metric_set(rmse, rsq, mae)
)

tree_rs

# evaluate model
collect_metrics(tree_rs)

autoplot(tree_rs) + theme_clean()

select_best(tree_rs, "rmse")

# choose "best" set of params to update and finalize model
final_tree <- finalize_model(tree_spec, select_best(tree_rs, "rmse"))

final_tree

# fit the model (two ways, same thing)
final_fit <- fit(final_tree, rev_sqft ~ ., green_train)      # 1
final_rs <- last_fit(final_tree, rev_sqft ~ ., green_split)  # 2

# predict 
predict(final_fit, green_train[144, ])                       # 1
predict(final_rs$.workflow[[1]], green_train[144, ])         # 2

# what are the most important variables
final_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# look at test data
collect_metrics(final_rs)

final_rs %>%
  collect_predictions() %>%
  ggplot(aes(rev_sqft, .pred)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  theme_clean() +
  coord_fixed()





# library(rpart)
# library(rpart.plot)
# rpart.plot(final_fit$fit)
