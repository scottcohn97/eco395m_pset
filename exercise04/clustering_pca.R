# Clustering and PCA

source('exercise04/libs_04.R', echo = T)

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

# import
wine <- 
  read_data("wine.csv") %>%
  janitor::clean_names() %>%
  rename(ph = p_h) 

# preview
skim(wine)
glimpse(wine)
