#install.packages("tidyverse")
library(tidyverse)


n <- 100
set.seed(123)
data_frame <- tibble(
  x = rnorm(n),
  y = rnorm(n),
  z = rnorm(n)
)

groups <- sample(c("A", "B"), size=n, replace=TRUE)


Welch_t_test <- function(data, group, names, alpha=0.05, bonferroni=FALSE) {
  n_variables <- length(names)

  t_vector <- numeric(n_variables)
  p_vector <- numeric(n_variables)

  for (idx in 1:n_variables) {
    variable <- data[[idx]]

    variable_a <- variable[group == "A"]
    variable_b <- variable[group == "B"]

    n_a <- length(variable_a)
    n_b <- length(variable_b)

    mean_a <- sum(variable_a)/n_a
    mean_b <- sum(variable_b)/n_b

    variance_a  <- sum((variable_a - mean_a)^2)/(n_a-1)
    variance_b  <- sum((variable_b - mean_b)^2)/(n_b-1)

    t <- (mean_a - mean_b)/sqrt((variance_a/n_a)+(variance_b/n_b))

    df <- (((variance_a/n_a)+(variance_b/n_b))^2)/(((variance_a/n_a)^2)/(n_a-1)+((variance_b/n_b)^2)/(n_b-1))

    p <- pt(t, df)

    if (bonferroni == TRUE) {
      p <- min(1, n_variables * p)
    }

    t_vector[idx] <- t
    p_vector[idx] <- p
  }

  return(list(t=t_vector, p=p_vector))
}

obj <- Welch_t_test(data_frame, groups, c("x", "y", "z"), bonferroni=TRUE)

