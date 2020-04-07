###############################################################################
## Takes a vector, and alpha confidence level and prints out lower CI, mean ####
## and upper CI for the vector.                                ####
###############################################################################
tCIMean <- function(dat, alpha) {
  
  n = length(dat)
  tValue <- qt((1 - alpha/2), df= (n - 1))
  
  upperMean <- mean(dat) + tValue * sd(dat) / sqrt(n)
  lowerMean <- mean(dat) - tValue * sd(dat) / sqrt(n) 
  
  table <- cbind("lowerCI" = lowerMean, "mean" = mean(dat), "upperCI" = upperMean)
  
  return(table)
}

