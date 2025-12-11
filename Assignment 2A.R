# Ordinary least squares.
# Data: matrix, first column is dependent variable, rest independent
# Return: vector of estimated beta coefficient
olsFun <- function(data){
  p <- length(data[1,])  # number of variables
  n <- length(data[,1])  # number of observations
  y <- data[,1]  # dependent variable
  x <- data[,2:p]  # independent variables
  x_0 <- c(rep(1, n))  # intercept column
  x <- cbind(x_0, x)  # full x-matrix
  beta_ols <- solve(t(x) %*% x) %*% t(x) %*% y  # OLS estimator
  return(beta_ols)
}

# Weighted least squares.
# Data: matrix, first column is dependent variable, rest independent
# Lambda: vector of weights, true covariance structure
# Return: vector of estimated beta coefficient
wlsFun <- function(data, lambda){
  if (class(lambda) != "numeric") {  # control check that lambda is numeric
    stop("Error: lambda must be numeric.")
  }
  p <- length(data[1,])  # number of variables
  n <- length(data[,1])  # number of observations
  y <- data[,1]  # dependent variable
  x <- data[,2:p]  # independent variables
  x_0 <- c(rep(1, n))  # intercept column
  x_ls <- cbind(x_0, x)  # full x-matrix
  error_var_vector <- exp(x %*% as.matrix(lambda))  # n x 1 vector of error variances
  error_var_matrix <- diag(as.numeric(error_var_vector))  # n x n error covariance matrix (zero correlation)
  # WLS coefficients
  beta_wls <- solve(t(x_ls) %*% solve(error_var_matrix) %*% x_ls) %*% t(x_ls) %*% solve(error_var_matrix) %*% y
  return(beta_wls)
}

# Feasible weighted least squares.
# Data: matrix, first column is dependent variable, rest independent
# trueVar: boolean, true or erroneous form of error variance
# Return: vector of estimated beta coefficient
fwlsFun <- function(data, trueVar){
  p <- length(data[1,])  # number of variables
  n <- length(data[,1])  # number of observations
  y <- data[,1]  # dependent variable
  x <- data[,2:p]  # independent variables
  x_0 <- c(rep(1, n))  # intercept column
  x_ls <- cbind(x_0, x)  # full x-matrix
  ols <- olsFun(data)  # base regression to recieve residuals
  u_hat_squared <- (y - (x_ls %*% ols))^2  # residuals

  if (trueVar == TRUE) {
    lambda_hat <- solve(t(x_ls) %*% x_ls) %*% t(x_ls) %*% log(u_hat_squared)  # estimate of lambda
    error_var_vector <- exp(x %*% as.matrix(lambda_hat[2,]))  # n x 1 vector of error variances
  }
  else {
    lambda_hat <- solve(t(x_ls) %*% x_ls) %*% t(x_ls) %*% log(u_hat_squared)  # estimate of lambda
    error_var_vector <- 1 + x %*% as.matrix(lambda_hat[2,])  # n x 1 vector of error variances
  }

  error_var_matrix <- diag(as.numeric(error_var_vector))  # n x n error covariance matrix (zero correlation)
  # FWLS coefficients
  beta_fwls <- solve(t(x_ls) %*% solve(error_var_matrix) %*% x_ls) %*% t(x_ls) %*% solve(error_var_matrix) %*% y
  return(beta_fwls)
}

# Data generation function
# n: number of observations
# lambda: error variance weight
# return: n x 2 matrix with x- and y-values
DataFun <- function(n, lambda) {
  x <- runif(n, 0, 2)  # x-values, U(0,2)-distributed
  var_vector <- exp(x*lambda)  # vector with error variances (dependent on x and lambda)
  epsilon_vector <- rnorm(rep(0, n), sqrt(var_vector))  # error term estimates
  beta <- 2
  y <- beta*x + epsilon_vector  # calculate y-values
  output_matrix <- cbind(y, x)  # combine x and y to matrix
  return(output_matrix)
}

SimFun <- function(n, sim_reps, seed, lambda) {
  set.seed(seed)
  ols_vector <- numeric(sim_reps)
  wls_vector <- numeric(sim_reps)
  fwls_vector_true <- numeric(sim_reps)
  fwls_vector_false <- numeric(sim_reps)
  for (i in 1:sim_reps) {
    print(sprintf("Processing simulation %d", i))
    data <- DataFun(n, lambda)
    ols <- olsFun(data)
    wls <- wlsFun(data, lambda=lambda)
    fwls_true <- fwlsFun(data, trueVar=TRUE)
    fwls_false <- fwlsFun(data, trueVar=FALSE)
    ols_vector[[i]] <- ols[[2]]
    wls_vector[[i]] <- wls[[2]]
    fwls_vector_true[[i]] <- fwls_true[[2]]
    fwls_vector_false[[i]] <- fwls_false[[2]]
  }
  return(c(var(ols_vector), var(wls_vector) ,var(fwls_vector_true) , var(fwls_vector_false)))
# return(Vector of four variance estimates)
}


# Function for plotting estimated variances
# sim_reps: number of simulations
# seed: seed for reproducibility
# lambda: error variance weight
PlotSim <- function(sim_reps, seed, lambda){
  # sample isze vector
  sample_size <- c(25, 50, 100, 200, 400)
  # Placeholder vectors for estimates
  sim_vector_ols <- numeric(length(sample_size))
  sim_vector_wls <- numeric(length(sample_size))
  sim_vector_fwls_t <- numeric(length(sample_size))
  sim_vector_fwls_f <- numeric(length(sample_size))
  for (i in 1:length(sample_size)) {  # loops through sample sizes
    n <- sample_size[[i]]  # current sample size
    simulations <- SimFun(n, sim_reps, seed, lambda)  # simulation result

    # append to placeholder vectors
    sim_vector_ols[[i]] <- simulations[[1]]
    sim_vector_wls[[i]] <- simulations[[2]]
    sim_vector_fwls_t[[i]] <- simulations[[3]]
    sim_vector_fwls_f[[i]] <- simulations[[4]]
  }
  # Plot of variance estimates
    # data preparation for ggplot
    tibble(ols = sim_vector_ols,
           wls = sim_vector_wls,
           fwls_t = sim_vector_fwls_t,
           fwls_f = sim_vector_fwls_f,
           sample_size = sample_size) %>%
    pivot_longer(cols = c(ols, wls, fwls_t, fwls_f), names_to = "Estimates", values_to = "Estimate_variance") %>%
    # Create plot
    ggplot(aes(x = sample_size, y = Estimate_variance)) +
    geom_line(aes(color = Estimates)) +  # group by estimator
    scale_x_continuous(breaks = sample_size) +  # scale the x-axis
    labs( #Label names
        x = "Sample size",
        y = "Estimate variance") +
    scale_color_discrete( # Legend names
      labels = c("OLS", "WLS", "FWLS True", "FWLS False")) +
    # theme settings
    theme(panel.grid.major = element_line(colour = "gray80"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "gray93"),
          axis.ticks.y = element_line(colour = "gray93"),
          axis.ticks.x = element_line(colour = "gray93"),
          legend.position.inside = c(0.8, 0.7))
}

PlotSim(100, 125, 3)



testData <- cbind( c(0.62, 0.18, 3.92, 0.80, -5.15),
                   c(0.44, 1.49, 0.69, 0.13, 1.90))
#print(wlsFun(testData, lambda=2))
#c(olsFun(data = testData),
#wlsFun(data = testData, lambda = 2),
#fwlsFun(data = testData, trueVar = TRUE),
#fwlsFun(data = testData, trueVar = FALSE), use.names = F)
#data <- DataFun(100, 2)
print(SimFun(100, 100, 123, 2))


