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

#plot(index_prices$SP500, main = "SP500 monthly price", col = "blue")
#plot(index_prices$FTSE100,main = "FTSE100 monthly price", col = "blue")

log_returns_sp500 = CalculateReturns(index_prices$SP500, method = "log")[-1]
log_returns_fste100 = CalculateReturns(index_prices$FTSE100, method = "log")[-1]
log_returns = CalculateReturns(index_prices)[-1]

number_of_bins = round(10 *log(length(log_returns)))

#hist(log_returns$SP500, main = "SP500 daily log-returns", col = "blue", breaks = 100)
#hist(log_returns$FTSE100, main = "FTSE100 daily log-returns", col = "blue", breaks = 100)

# Compute the VaR forecast for the SP500 index
VaR_forecast_sp500 = f_forecast_var(log_returns_sp500, level = 0.95)
#plot(VaR_forecast_sp500$ConditionalVariances, main = "Conditional Variances of SP500", col = "blue")

hist(log_returns$SP500, main = "SP500 daily log-returns", col = "blue", breaks = 100)
abline(v=VaR_forecast_sp500$VaR_Forecast, col="red", lwd=2)
abline(v=quantile(log_returns$SP500,0.05), col="black", lwd=2)

print("VaR from data SP500")
quantile(log_returns$SP500,0.05)
print("Var from Garch")
VaR_forecast_sp500$VaR_Forecast





       