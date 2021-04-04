# Association rules for grocery purchase

source('exercise04/libs_04.R', echo = T)

# import
# groceries <- 
#   read_lines("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/groceries.txt") %>%
#   as_tibble() %>% 
#   # add id
#   mutate(id = row_number()) %>%
#   # swap rows
#   select(2,1) %>%
#   rename(basket = value) %>%
#   separate_rows(basket, sep = ",")
# 
# glimpse(groceries)

# most popular
# groceries %>% 
#   group_by(basket) %>% 
#   tally() %>% 
#   arrange(desc(n)) %>%
#   head() 

# groceries <- 
#   arules::read.transactions("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/groceries.txt",
#                             sep = "\n")

data("Groceries")

image(sample(Groceries, 100))

frequent_items <- eclat(Groceries, parameter = list(supp = 0.07, maxlen = 15)) 

summary(frequent_items)


rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen = 3)) # Min Support as 0.001, confidence as 0.8.

# remove redundant rules
subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix, diag = T)] <- NA # not working
redundant <- colSums(subset_matrix, na.rm = T) >= 1
rules_pruned <- rules[!redundant]
rules <- rules_pruned

summary(rules)
plot(rules, engine = "ggplot") + theme_clean()
plot(rules, "scatterplot", engine = "ggplot") + theme_clean()
plot(rules, "grouped", engine = "default") 
head(quality(rules)) %>% kbl(digits = 4, "pipe")

arules::itemFrequencyPlot(
  Groceries,
  topN = 20,
  col = 'dodgerblue',
  main = 'Relative Item Frequency Plot',
  type = "relative",
  ylab = "Item Frequency"
  )

plot(rules, method = "graph", control = list(type = "items"), engine = "igraph")

plot(rules, method = "paracoord", control = list(type = "items"))


#' Groceries Aisle – Milk, Eggs and Vegetables
#' Liquor Aisle – Liquor, Red/Blush Wine, Bottled Beer, Soda
#' Eateries Aisle – Herbs, Tropical Fruits, Rolls/Buns, Fruit Juices, Jams
#' Breakfast Aisle – Cereals, Yogurt, Rice, Curd

