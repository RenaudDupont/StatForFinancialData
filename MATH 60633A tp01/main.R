library("here")
library("xts")
library("PerformanceAnalytics")
source(here("Code/f_forecast_var.R"))
source(here("Code/functions.R"))
source(here("Tests/UGarchLib.R"))

# Load the data and filter the data
index_prices = get(load(here("Data/indices.rda")))
start_analysis_date <-"2005-01-01/"
index_prices = index_prices[start_analysis_date,]
level = 0.95

# Set the plot parameters
par(mfrow = c(1,1), ## set 2 charts in 1 row
    cex = 0.6) ## labels are now 60% of their normal size

# Compute the log-returns
log_returns_sp500 = CalculateReturns(index_prices$SP500, method = "log")[-1]
log_returns_fste100 = CalculateReturns(index_prices$FTSE100, method = "log")[-1]
log_returns = CalculateReturns(index_prices)[-1]

number_of_bins = round(10 *log(length(log_returns)))


# Compute the VaR forecast for the SP500 index
VaR_forecast_sp500 = f_forecast_var(log_returns_sp500, level = level)
VaR_forecast_sp500$GARCH_param

dates_vector <- index(index_prices)
xts_conditional_variances <- xts(VaR_forecast_sp500$ConditionalVariances, order.by = as.Date(dates_vector)+1)

# Plot the conditional variances as a time series
plot(sqrt(xts_conditional_variances), type="l",
     xlab = "Garch Standard Deviation", 
     main="Garch Standard Deviation by date", col="blue")

plot(store$objective_values, type = 'l', 
     xlab = "Iteration", ylab = "Objective Function Value",
     main = "Objective Function Value Over Iterations")

# Estimating the VaR and the parameters using rugarch
rugarch_test = f_test_rugarch(log_returns_sp500,0.95)
cat("VaR and Params from implemented garch:\nVaR:", VaR_forecast_sp500$VaR, "\nParameters:", 
    (VaR_forecast_sp500$GARCH_param), "\n")

# plot the histogram of the log-returns and the VaR forecast
hist(log_returns$SP500, main = "SP500 daily log-returns", col = "blue", breaks = 100)
abline(v=rugarch_test$VaR, col="blue", lwd = 4 )
abline(v=VaR_forecast_sp500$VaR_Forecast, col="red", lwd = 2 )
abline(v=quantile(log_returns$SP500,0.05), col = "black", lwd = 2 )
legend("topright", legend=c("VaR from RuGarch","VaR from Implemented Garch", "VaR from data"), 
       col = c("blue","red", "black"), lty = 1:1, cex = 0.8)

# Print the VaR forecast from the data, Garch and Rugarch
print("VaR from data SP500")
quantile(log_returns$SP500,0.05)

print("Var from Garch")
VaR_forecast_sp500$VaR_Forecast

print("VaR from Rugarch")  
rugarch_test$VaR

# **********************************************************************************************
# QUESTIONS ANSWERS ****************************************************************************
# **********************************************************************************************

# Use the first T = 1000 log-returns to estimate the VaR of each index at the 95% risk level. Which one
# is the most risky at the T + 1 horizon? 

number_of_days = 1000
VaR_forecast_sp500 = f_forecast_var(log_returns_sp500[1:number_of_days], level = level)
cat("VaR at 95% for SP500:", VaR_forecast_sp500$VaR_Forecast, "\n")

VaR_forecast_fste100 = f_forecast_var(log_returns_fste100[1:number_of_days], level = level)
cat("VaR at 95% for FSTE100:", VaR_forecast_fste100$VaR_Forecast, "\n")

most_risky = ifelse(VaR_forecast_sp500$VaR_Forecast < VaR_forecast_fste100$VaR_Forecast, "SP500", "FSTE100")
cat("The most risky index is:", most_risky, "\n")

# Compare the VaR forecast with the actual losses
f_test_rugarch(log_returns_sp500[1:number_of_days],level)
f_test_rugarch(log_returns_fste100[1:number_of_days],level)

# **********************************************************************************************

# Using a rolling window of T = 1000 days, compute and store the next-step-ahead VaR at the 95% risk
# level for the next 1000 days. 

window_size = 1000
number_of_windows = 1000
VaR_forecast_sp500 = rep(NA, number_of_windows)
VaR_forecast_fste100 = rep(NA, number_of_windows)

# We use the parallel package to parallelize the computation of the VaR forecast
library("parallel")
library("Rsolnp")

cl <- makeCluster(detectCores() - 2) # Leave 2 cores free for system processes
clusterExport(cl, list("f_forecast_var", "log_returns_sp500",
                       "log_returns_fste100",
                       "window_size",
                       "level",
                       "solnp",
                       "f_ineq_constraint",
                       "f_nll",
                       "ComputeHtGarch","store"))

# we use parLapply to parallelize the computation of the VaR forecast
VaR_forecast_sp500 <- parLapply(cl, 1:number_of_windows, function(i) {
  cat("SP500 windows ",i)
  f_forecast_var(log_returns_sp500[i:(i+window_size-1)], level = 0.95)$VaR_Forecast
})
VaR_forecast_fste100 <- parLapply(cl, 1:number_of_windows, function(i) {
  cat("FSTE100 windows ",i)
  f_forecast_var(log_returns_fste100[i:(i+window_size-1)], level = 0.95)$VaR_Forecast
})
stopCluster(cl)

# We convert the list of VaR forecasts into a time series
VaR_forecast_sp500_vector <- unlist(VaR_forecast_sp500)

dates <- index(log_returns_sp500)[window_size:(window_size + number_of_windows - 1)]
VaR_forecast_sp500_xts <- xts(VaR_forecast_sp500_vector, order.by = dates)


# We plot the VaR forecast and the log-returns
library("quantmod")
png(here("Plots/VaR_forecast_sp500.png"))
plot.xts(VaR_forecast_sp500_xts, type = 'l', col = 'blue', xlab = "Window", ylab = "VaR Forecast",
     main = "VaR Forecast for S&P 500")

returns=log_returns_sp500[window_size:(window_size+number_of_windows-1)]
daysBelowVaR = sum(returns < VaR_forecast_sp500_vector)
percentageBelowVaR = daysBelowVaR/number_of_windows *100
cat("The percentage of time that the returns are below the VaR forecast is:", percentageBelowVaR, "\n")

lines(returns, col = 'red')

addLegend("topright", on = 1, col = c("blue", "red"),
       legend.names = c("VaR Forecast", "Returns"),
       lty = 1, cex = 0.8,
       title= "Legend:", )
message = paste("The percentage of time that the returns are below the VaR forecast is:", percentageBelowVaR, "%")
mtext(message, side = 1, line = -2)
dev.off()





# **********************************************************************************************