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
  if (!is_tibble(data)) {
    stop("Error: input data is not tibble.")
  }

  if (!"A" %in% group | !"B" %in% group) {
    stop("Error: group must be A or B.")
  }

  if (length(group) != length(data[[1]])) {
    stop("Error: group length must be equal to number of observations.")
  }

  if (length(colnames(data)) != length(names)) {
    stop("Error: number of variable names must be same as number of column names.")
  }

  if (FALSE %in% (colnames(data_frame) == names)) {
    stop("Error: variable names must be same as data column names.")
  }

  n_variables <- length(names)

  t_vector <- numeric(n_variables)
  p_vector <- numeric(n_variables)

  data[["Groups"]] <- group

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

    p <- 2 * pt(t, df)

    if (bonferroni == TRUE) {
      p <- min(1, n_variables * p)
    }

    t_vector[idx] <- t
    p_vector[idx] <- p
  }
  structure(
    list(t=t_vector, p=p_vector, names=names, groups=groups, data=data),
    class="Welch_t_test"
  )
}


print.Welch_t_test <- function(self) {
  df <- data.frame(row.names=self$names,
    names = self$names,
    p_value = self$p,
    t_value = self$t
  )
  print(as.data.frame(t(df)))
}

plot.Welch_t_test <- function(self) {
  data <- pivot_longer(
    self$data,
    cols = 1:(length(self$names)),
    names_to = "Variables",
    values_to = "Values"
  )
  ggplot(data, aes(
    x = Variables,
    y = Values,
    color = Groups
)) +
  geom_jitter(
    position = position_dodge(width = 0.1)
  ) +
  scale_color_manual(
    values = c(
      "A" = "blue",
      "B" = "red"
    )
  )
}


obj <- Welch_t_test(data_frame, groups, c("x", "y", "z"))
#res <- t.test(x ~ groups, data=data_frame, var.equal=FALSE)
print(obj)
plot(obj)


