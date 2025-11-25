library(tidyverse)

as_tibble(iris)

#Making dataset
df <- tibble(
  x = rnorm(50),
  y = rnorm(50)
)
df


Welch_t_test <- function(data, group, names, alpha = 0.05, bonferroni) {
  for (i in names) {
    x <- data[group == "A", i]
    y <- data[group == "B", i]
    
    
    

    
  }
}