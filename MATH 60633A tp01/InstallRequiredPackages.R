packages <- c("here", "xts", "PerformanceAnalytics", "Rsolnp", "parallel", "quantmod", "rugarch") # Add required packages here
new_packages <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

lapply(packages, library, character.only = TRUE)