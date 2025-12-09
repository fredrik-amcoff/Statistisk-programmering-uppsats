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

fwlsFun <- function(data, trueVar){
  p <- length(data[1,])  # number of variables
  n <- length(data[,1])  # number of observations
  y <- data[,1]  # dependent variable
  x <- data[,2:p]  # independent variables
  x_0 <- c(rep(1, n))  # intercept column
  x_ls <- cbind(x_0, x)  # full x-matrix
  ols <- olsFun(data)
  u_hat_squared <- (y - (x_ls %*% ols))^2

  if (trueVar == TRUE) {
    lambda_hat <- solve(t(x_ls) %*% x_ls) %*% t(x_ls) %*% log(u_hat_squared)
    error_var_vector <- exp(x_ls %*% as.matrix(lambda_hat))  # n x 1 vector of error variances
  }
  else {
    lambda_hat <- solve(t(x) %*% x) %*% t(x) %*% log(u_hat_squared)
    error_var_vector <- 1 + x %*% as.matrix(lambda_hat)  # n x 1 vector of error variances
  }

  error_var_matrix <- diag(as.numeric(error_var_vector))  # n x n error covariance matrix (zero correlation)
  beta_fwls <- solve(t(x_ls) %*% solve(error_var_matrix) %*% x_ls) %*% t(x_ls) %*% solve(error_var_matrix) %*% y
  return(beta_fwls)
}

testData <- cbind( c(0.62, 0.18, 3.92, 0.80, -5.15),
                   c(0.44, 1.49, 0.69, 0.13, 1.90))
print(fwlsFun(testData, trueVar=FALSE))
#c(olsFun(data = testData),
#wlsFun(data = testData, lambda = 2),
#fwlsFun(data = testData, trueVar = TRUE),
#fwlsFun(data = testData, trueVar = FALSE), use.names = F)
test_data <- list(y=c(0.62, 0.18, 3.92, 0.80, -5.15), x=c(0.44, 1.49, 0.69, 0.13, 1.90))
ols <- lm(y ~ x, data=test_data)

