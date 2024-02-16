library("here")
library("xts")
library("PerformanceAnalytics")
source(here("Code/f_forecast_var.R"))
source(here("Code/functions.R"))

index_prices = get(load(here("Data/indices.rda")))
start_analysis_date <-"2005-01-01/"
index_prices = index_prices[start_analysis_date,]

par(mfrow = c(2,1), ## set 2 charts in 1 row
    cex = 0.6) ## labels are now 60% of their normal size

log_returns_sp500 = CalculateReturns(index_prices$SP500, method = "log")[-1]
log_returns_fste100 = CalculateReturns(index_prices$FTSE100, method = "log")[-1]
log_returns = CalculateReturns(index_prices)[-1]

number_of_bins = round(10 *log(length(log_returns)))


# Compute the VaR forecast for the SP500 index
VaR_forecast_sp500 = f_forecast_var(log_returns_sp500, level = 0.95)
VaR_forecast_sp500$GARCH_param


dates_vector <- index(index_prices)
xts_conditional_variances <- xts(VaR_forecast_sp500$ConditionalVariances, order.by = as.Date(dates_vector)+1)

# Plot the conditional variances as a time series
plot(sqrt(xts_conditional_variances), type="l",xlab = "Garch Standard Deviation", main="Garch Standard Deviation by date", col="blue")

plot(store$objective_values, type = 'l', xlab = "Iteration", ylab = "Objective Function Value",
     main = "Objective Function Value Over Iterations")

hist(log_returns$SP500, main = "SP500 daily log-returns", col = "blue", breaks = 100)
abline(v=VaR_forecast_sp500$VaR_Forecast, col="red", lwd=2 )
abline(v=quantile(log_returns$SP500,0.05), col="black", lwd=2 )
legend("topright", legend=c("VaR from Garch", "VaR from data"), col=c("red", "black"), lty=1:1, cex=0.8)

print("VaR from data SP500")
quantile(log_returns$SP500,0.05)
print("Var from Garch")
VaR_forecast_sp500$VaR_Forecast



f_test_rugarch <- function(log_returns,level)
{
  # Load the rugarch package
  library(rugarch)
  #Y is the vector of log-returns
  # Specify the GARCH(1,1) model with normal distribution for the errors
  spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                     distribution.model = "norm")
  # Fit the GARCH model
  fit <- ugarchfit(spec = spec, data = log_returns_sp500)
  # Summary of the fitted model
  summary(fit)
  # Extract the conditional variances (sigmas)
  sigma_t <- sigma(fit)
  # Compute the VaR at the desired confidence level, e.g., 95%
  VaR <- qnorm(1 - level) * tail(sigma_t, 1)
  
  cat("VaR and Params from rugarch:\nVaR:", VaR, "\nParameters:", (coef(fit)), "\n")
  return(list(VaR = VaR, Params = coef(fit), Sigma = sigma_t))
}

rugarch_test = f_test_rugarch(log_returns_sp500,0.95)


       