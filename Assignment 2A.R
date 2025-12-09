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


DataFun <- function(n, lambda) {
  x <- runif(n, 0, 2)
  var_vector <- exp(x*lambda)
  epsilon_vector <- numeric(n)
  for (i in seq_along(var_vector)) {
    var <- var_vector[[i]]
    epsilon <- rnorm(1, 0, sqrt(var))
    epsilon_vector[[i]] <- epsilon
  }
  beta <- 2
  y <- beta*x + epsilon_vector
  output_matrix <- cbind(y, x)
  return(output_matrix)


# return(an n x 2 matrix or data frame)
}


testData <- cbind( c(0.62, 0.18, 3.92, 0.80, -5.15),
                   c(0.44, 1.49, 0.69, 0.13, 1.90))
#print(wlsFun(testData, lambda=2))
#c(olsFun(data = testData),
#wlsFun(data = testData, lambda = 2),
#fwlsFun(data = testData, trueVar = TRUE),
#fwlsFun(data = testData, trueVar = FALSE), use.names = F)
data <- DataFun(100, 2)


