f_compute_log_returns <- function(x) {
  ### Function which computes the log-returns of a time series
  #  INPUTS
  #   x : [vector] (T x 1) of prices
  #  OUTPUTS
  #   y : [vector] (T x 1) of log-returns
  
  # Compute the log-returns
  y <- diff(log(x))
  
  y
}

f_compute_log_returns_alterative <- function(x) {
  ### Function which computes the log-returns of a time series
  #  INPUTS
  #   x : [vector] (T x 1) of prices
  #  OUTPUTS
  #   y : [vector] (T x 1) of log-returns
  
  # Compute the log-returns
  y <- log((x/lag(x,1)))
  
  y
}