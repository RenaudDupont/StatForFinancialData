library("here")

f_test_rugarch <- function(log_returns,level)
{
  # Load the rugarch package
  library("rugarch")
  #Y is the vector of log-returns
  # Specify the GARCH(1,1) model with normal distribution for the errors
  spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                     distribution.model = "norm")
  # Fit the GARCH model
  fit <- ugarchfit(spec = spec, data = log_returns)
  # Summary of the fitted model
  summary(fit)
  # Extract the conditional variances (sigmas)
  sigma_t <- sigma(fit)
  # Compute the VaR at the desired confidence level, e.g., 95%
  VaR <- qnorm(1 - level) * tail(sigma_t, 1)
  
  cat("VaR and Params from rugarch:\nVaR:", VaR, "\nParameters:", (coef(fit)), "\n")
  return(list(VaR = VaR, Params = coef(fit), Sigma = sigma_t))
}
