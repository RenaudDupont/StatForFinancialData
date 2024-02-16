library("here")
library("xts")
library("PerformanceAnalytics")
source(here("Code/f_forecast_var.R"))
source(here("Code/functions.R"))
source(here("Tests/UGarchLib.R"))


f_generate_datas <- function(theta, n)
{
  omega <- theta[1]
  alpha <- theta[2]
  beta <- theta[3]
  epsilon <- rnorm(n)  # Random shocks from a standard normal distribution
  sigma2 <- rep(0, n)  # Conditional variance sigma squared
  y <- rep(0, n)       # Returns y
  
  # Set initial values for sigma2 and y
  sigma2[1] <- omega / (1 - alpha - beta)
  y[1] <- sqrt(sigma2[1]) * epsilon[1]
  
  # Simulate data using the GARCH(1,1) process
  for(t in 2:n) {
    sigma2[t] <- omega + alpha * y[t-1]^2 + beta * sigma2[t-1]
    y[t] <- sqrt(sigma2[t]) * epsilon[t]
  }
  y
}
 
theta0 <- c(1, 0.1, 0.8)

data = f_generate_datas(theta0, 100000)

number_of_bins = round(10 *log(length(data)))

plot(data,type="l", min = -1, max = 1, xlab = "Time", ylab = "Data", main = "Simulated Garch(1,1) data")
# Compute the VaR forecast for the SP500 index
VaR_forecast_sp500 = f_forecast_var(data, level = 0.95)
VaR_forecast_sp500$GARCH_param


# Plot the conditional variances as a time series
plot(store$objective_values, type = 'l', xlab = "Iteration", ylab = "Objective Function Value",
     main = "Objective Function Value Over Iterations")

hist(data, main = "data daily log-returns", col = "blue", breaks = 100)
abline(v=VaR_forecast_sp500$VaR_Forecast, col="red", lwd=2 )
abline(v=quantile(data,0.05), col="black", lwd=2 )
legend("topright", legend=c("VaR from Garch", "VaR from data"), col=c("red", "black"), lty=1:1, cex=0.8)

print("VaR from data ")
quantile(data,0.05)
print("Var from Garch")
VaR_forecast_sp500$VaR_Forecast
VaR_forecast_sp500$GARCH_param

# Monte Carlo for parameters estimation :
# Number of simulations
n_sim <- 20
params <- matrix(0, nrow = n_sim, ncol = 3)
rugarch <- matrix(0, nrow = n_sim, ncol = 3)

for(i in 1:n_sim) {
  # Simulate data
  data <- f_generate_datas(theta0, 100000)
  # Estimate parameters
  # Compute VaR forecasat
  VaR_forecast <- f_forecast_var(data, level = 0.95)
  # Store the results
  params[i,] <- VaR_forecast$GARCH_param
  print(i/n_sim)
}

mc_param = c(sum(params[,1])/n_sim, sum(params[,2])/n_sim, sum(params[,3])/n_sim)
mc_param




