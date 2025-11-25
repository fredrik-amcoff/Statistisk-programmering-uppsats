#install.packages("tidyverse")
library(tidyverse)


df <- tibble(
  x = rnorm(10),
  y = rnorm(10)
)

groups <- c("A", "A", "B", "A", "B", "B", "B", "A", "B", "B")

