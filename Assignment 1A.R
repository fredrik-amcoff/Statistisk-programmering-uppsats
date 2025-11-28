install.packages("tidyverse")
library(tidyverse)

#Making dataset for two variables
n <- 10 #observations

df <- tibble(
  var1 = rnorm(n),
  var2 = rnorm(n),
)
df #check the data

if(FALSE %in% (colnames(df) == c("var2", "var2"))){
  stop("Not the same")
}

group <- sample(c("A","B"), size = n, replace = TRUE) #groups


#Function
Welch_t_test <- function(data, group, names, alpha = 0.05, bonferroni = FALSE) {

  t_stat <- numeric(length(data))
  p_values <- numeric(length(data))
    
  for (i in 1:length(data)) {
    
    x <- data[[i]] [group == "A"]
    y <- data[[i]] [group == "B"]
    
    n_a <- length(x)
    n_b <- length(y)
    
    x_mean <- 1/n_a*sum(x)
    y_mean <- 1/n_b*sum(y)
    
    x_var <- (1/(n_a-1))*sum((x-x_mean)^2)
    y_var <- (1/(n_b-1))*sum((y-y_mean)^2)
    
    t <- (x_mean-y_mean)/sqrt((x_var/n_a)+(y_var/n_b))
    
    degr_free <- ((x_var/n_a)+(y_var/n_b))^2/(((x_var/n_a)^2/(n_a-1))+((y_var/n_b)^2/(n_b-1)))
    
    p_val <- 2 * pt(-abs(t), degr_free)
    
    t_stat[i] <- t
    p_values[i] <- p_val
    
  }
  
  if(bonferroni){
    p_values <- min(1, length(data) * p_values)
  }
    
    result <- list(
      variable_names = names,
      p_values = p_values,
      t_statistics = t_stat,
      data = data,
      group = group
    )
    
    class(result) <- "Welch_t_test"
    return(result)
}

obj <- Welch_t_test(df, group, names = c("Med1", "Med2"))

print(obj)

obj$data
obj$group
obj$variable_names


res <- t.test(var1 ~ group, data = df, var.equal = FALSE)
res$statistic
res$p.value 


plot.Welch_t_test <- function(x) {
  library(tidyr)
  library(ggplot2)
  
  df <- x$data
  df$group <- x$group
  
  long <- pivot_longer(df,
                       cols = -group,
                       names_to = "variable",
                       values_to = "value"
    
  )
  
  
}



####President Election
#Import data
library(readr)

county_facts <- read_csv("county_facts.csv")

general_result <- read_csv("general_result.csv")

crime_data <- read_delim("crime_data.tsv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

summary(county_facts)
summary(general_result)
summary(crime_data)


