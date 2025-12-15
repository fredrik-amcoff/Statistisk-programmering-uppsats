#library(ggplot2)

filepath <- "galaxies.RData"
load(filepath)

galaxies_df <- as.data.frame(galaxies)

#ggplot(galaxies_df, aes(x = galaxies)) +
#  geom_density()


# Expectation step
# x: number of observations
# mu: 1xk vector of mean values
# sigma: 1xk vector of standard deviations
# pi: 1xk vector of mixing coefficients
# return: nxk matrix of of gamma updates
gammaUpdate <- function(x, mu, sigma, pi) {
  n <- length(x)  # number of observations
  k <- length(mu)  # number of components
  num <- pi*matrix(dnorm(x, rep(mu, n), rep(sigma, n)), n, k)  # unnormalized gamma updates
  denom <- 1/rowSums(num)  # normalization factor
  return((denom*num))
}


muUpdate <- function(x, gamma, sigma){
  n_k <- colSums(gamma)
  mu_new <- (1/n_k)*(t(x) %*% gamma)
  print(sum(gamma[,1]))
  print(n_k)
  #print(t(x) %*% gamma)
  #print(1/n_k)
  return(as.numeric(mu_new))


}
sigmaUpdate <- function(x, gamma, mu){
# your code here
}
piUpdate <- function(gamma){
# your code here
}





initialValues <- function(x, K, reps = 100){
  mu <- rnorm(K, mean(x), 5)
  sigma <- sqrt(rgamma(10, 5))
  p <- runif(K)
  p <- p/sum(p)
  currentLogLik <- loglik(x, p, mu, sigma)
  for(i in 1:reps){
    mu_temp <- rnorm(K, mean(x), 10)
    sigma_temp <- sqrt(rgamma(10, 5))
    p_temp <- runif(K)
    p_temp <- p_temp/sum(p_temp)
    tempLogLik <- loglik(x, p_temp, mu_temp, sigma_temp)
    if(tempLogLik > currentLogLik){
      mu <- mu_temp
      sigma <- sigma_temp
      p <- p_temp
      currentLogLik <- tempLogLik
    }
  }
  return(list("mu" = mu, "sigma" = sigma, "p" = p))
}

mu <- c(10, 20, 30)
sigma <- c(2, 2, 2)
probs <- c(1/3, 1/3, 1/3)
#resp <- gammaUpdate(galaxies, mu, sigma, probs)
#mu <- muUpdate(galaxies, resp, sigma)
#sigma <- sigmaUpdate(galaxies, resp, mu)
#probs <- piUpdate(resp)
#cat("mu:", mu,
#"\nsigma:", sigma,
#"\nprobs:", probs,
#"\nresp[1,]", resp[1,])


gamma <- gammaUpdate(galaxies, mu, sigma, probs)
muUpdate(galaxies, gamma, sigma)