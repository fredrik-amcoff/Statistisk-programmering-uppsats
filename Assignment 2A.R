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
  epsilon_vector <- numeric(n)  # placeholder vector for error terms
  for (i in seq_along(var_vector)) {  # sample N(0, exp(x_i*sigma_i))-distributed error terms
    var <- var_vector[[i]]  # variance
    epsilon <- rnorm(1, 0, sqrt(var))  # error term
    epsilon_vector[[i]] <- epsilon  # append error term to vector
  }
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



testData <- cbind( c(0.62, 0.18, 3.92, 0.80, -5.15),
                   c(0.44, 1.49, 0.69, 0.13, 1.90))
#print(wlsFun(testData, lambda=2))
#c(olsFun(data = testData),
#wlsFun(data = testData, lambda = 2),
#fwlsFun(data = testData, trueVar = TRUE),
#fwlsFun(data = testData, trueVar = FALSE), use.names = F)
#data <- DataFun(100, 2)
print(SimFun(100, 100, 123, 2))


