# Negative log-likelihood function for GARCH(1,1) model
logLikGarch <- function(params, y) {
  n <- length(y)
  omega <- params[1]
  alpha1 <- params[2]
  beta1 <- params[3]
  
  # Initialize conditional variance
  sigma2 <- numeric(n)
  sigma2[1] <- var(y)
  
  # Calculate conditional variances
  for (i in 2:n) {
    sigma2[i] <- omega + alpha1 * y[i-1]^2 + beta1 * sigma2[i-1]
  }
  
  # Calculate log-likelihood
  logLik <- -sum(log(sigma2) + y^2 / sigma2)
  return(-logLik)  # Return negative log-likelihood for minimization
}

# Function to compute the conditional variances given GARCH parameters and data
ComputeHtGarch <- function(theta, y) {
  T <- length(y)
  omega <- theta[1]
  alpha1 <- theta[2]
  beta1 <- theta[3]
  
  sig2 <- numeric(T + 1)
  sig2[1] <- omega / (1 - alpha1 - beta1)  # Unconditional variance
  
  for (t in 2:(T + 1)) {
    sig2[t] <- omega + alpha1 * y[t - 1]^2 + beta1 * sig2[t - 1]
  }
  
  return(sig2)
}

# Main function for estimating GARCH(1,1) parameters and forecasting VaR
f_forecast_var <- function(y, level) {
  # Initial parameter guesses
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  # Lower bounds to ensure positivity of parameters
  LB <- c(1e-6, 1e-6, 1e-6)
  
  # Estimate parameters by minimizing the negative log-likelihood
  opt_res <- optim(theta0, logLikGarch, method = "L-BFGS-B", lower = LB, y = y)
  theta <- opt_res$par  # Extract estimated parameters
  
  # Recompute the conditional variance with estimated parameters
  sig2 <- ComputeHtGarch(theta, y)
  
  # Compute next-day ahead VaR for the Normal model
  VaR <- -qnorm(1 - level) * sqrt(sig2[length(sig2)])
  
  return(list(VaR_Forecast = VaR, ConditionalVariances = sig2, GARCH_param = theta))
}

# Example usage
# Assuming 'y' is your time series data (log-returns) and you want to forecast VaR at 95% confidence level
# y <- your_time_series_data
# level <- 0.95
# result <- f_forecast_var(y, level)
# print(result)
