library("Rsolnp")

store <- new.env()
store$objective_values <- numeric()

f_ineq_constraint <- function(theta) {
  # This should return a positive value when the constraint is met
  return(1 - (theta[2] + theta[3]))
}

f_forecast_var <- function(y, level) {
  ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
  #  OUTPUTS
  #   VaR   : [scalar] VaR forecast
  #   sig2  : [vector] (T+1 x 1) conditional variances
  #   theta : [vector] GARCH parameters
  #  NOTE
  #   o the estimation is done by maximum likelihood
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  anyNA(y)
  
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  LB     <- c(1e-6, 1e-6, 1e-6)
  
  # Stationarity condition
  A      <- 1 - theta0[3]
  b      <- 1 - theta0[2]
  UB <- c(Inf, 1 - 1e-6, 1 - 1e-6)
  
  # Run the optimization
  opt_res = solnp(
    par = theta0,
    fun = function(x)
      f_nll(x, y),
    UB = UB,
    LB = LB,
    ineqfun = f_ineq_constraint,
    ineqLB = 0,
    ineqUB = 1,
    control = list(
      trace = 0,
      maxit = 1000,
      mins = 1e-8,
      maxtime = 3600
    )
  )
  
  theta <- opt_res$par
  
  # Recompute the conditional variance
  sig2 <- ComputeHtGarch(theta, y)
  
  # Compute the next-day ahead VaR for the Normal model
  VaR <-
    qnorm(1 - level, sd = sqrt(sig2[length(sig2)])) # VaR at the 95% risk level
  
  out <- list(
    VaR_Forecast = VaR,
    ConditionalVariances = sig2,
    GARCH_param = theta
  )
  
  out
}

f_nll <- function(theta, y) {
  ### Fonction which computes the negative log likelihood value
  ### of a GARCH model with Normal errors
  #  INPUTS
  #   theta  : [vector] of parameters
  #   y      : [vector] (T x 1) of observations
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  T <- length(y)
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- ComputeHtGarch(theta, y)
  
  
  sig2 <- sig2[1:T]  # Consider the T values
  
  
  ll <-
    sum(dnorm(
      y,
      mean = 0,
      sd = sqrt(sig2),
      log = TRUE
    ))  # Compute the loglikelihood
  
  
  nll <- -ll # Output the negative value
  
  
  store$objective_values <-
    c(store$objective_values, ll)  # Store the objective function value
  
  nll
}


ComputeHtGarch <- function(theta, y) {
  T <- length(y)
  omega <- theta[1]
  alpha1 <- theta[2]
  beta1 <- theta[3]
  
  sig2 <- numeric(T + 1)
  
  sig2[1] <- omega / (1 - alpha1 - beta1)  # Unconditional variance
  sig2[1] <- var(y)
  
  for (t in 2:(T + 1)) {
    sig2[t] <- omega + alpha1 * y[t - 1] ^ 2 + beta1 * sig2[t - 1]
  }
  
  return(sig2)
}

f_ht_arch <- function(theta, y)  {
  ### Function which computes the vector of conditional variance
  #  INPUTS
  #   x0 : [vector] (3 x 1)
  #   y     : [vector] (T x 1) log-returns
  #  OUTPUTS
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]
  a1 <- theta[2]
  b1 <- theta[3]
  
  T <- length(y)
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T + 1)
  
  # Start with unconditional variances
  sig2[1] <- a0 / (1 - a1 - b1)
  
  # Compute conditional variance at each step
  for (t in 2:(T + 1)) {
    sig2[t] <- a0 + a1 * y[t - 1] ^ 2 + b1 * sig2[t - 1]
  }
  
  sig2
}
