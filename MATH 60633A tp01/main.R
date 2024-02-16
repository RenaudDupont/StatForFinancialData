library("here")
library("xts")
library("PerformanceAnalytics")
source(here("Code/f_forecast_var.R"))
source(here("Code/functions.R"))
source(here("Tests/UGarchLib.R"))

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
rugarch_test = f_test_rugarch(log_returns_sp500,0.95)
cat("VaR and Params from implemented garch:\nVaR:", VaR_forecast_sp500$VaR, "\nParameters:", (VaR_forecast_sp500$GARCH_param), "\n")



hist(log_returns$SP500, main = "SP500 daily log-returns", col = "blue", breaks = 100)
abline(v=rugarch_test$VaR, col="blue", lwd=4 ))
abline(v=VaR_forecast_sp500$VaR_Forecast, col="red", lwd=2 )
abline(v=quantile(log_returns$SP500,0.05), col="black", lwd=2 )
legend("topright", legend=c("VaR from RuGarch","VaR from Implemented Garch", "VaR from data"), col=c("blue","red", "black"), lty=1:1, cex=0.8)

print("VaR from data SP500")
quantile(log_returns$SP500,0.05)
print("Var from Garch")
VaR_forecast_sp500$VaR_Forecast
print("VaR from Rugarch")  
rugarch_test$VaR







       