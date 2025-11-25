#install.packages("tidyverse")
library(tidyverse)


data_frame <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10)
)

groups <- c("A", "A", "B", "A", "B", "B", "B", "A", "B", "B")


Welch_t_test <- function(data, group, names, alpha, bonferroni) {
  vars_a <- list()
  vars_b <- list()
  n <- length(data[[1]])
  print(n)
  n_variables <- length(names)
  groups <- ifelse(group=="A", 1, 0)
  groups_b <- ifelse(group=="B", 1, 0)

  for (idx in 1:n_variables) {
    variable <- data[[idx]]
    variable_a <- variable[group == "A"]
    variable_b <- variable[group == "B"]
    #print(variable)
    #print(groups)
    print(variable_a)
    print(variable_b)
  }
}

Welch_t_test(df, groups, c("x", "y"), 1, 1)