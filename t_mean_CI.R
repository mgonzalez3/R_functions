###############################################################################
## Takes a vector, and alpha confidence level and prints out lower CI, mean ####
## and upper CI for the vector.                                ####
###############################################################################
t_meanCI <- function(dat, alpha = 0.05) {

  if(!is.numeric(alpha)){
    stop("Alpha value must be a number greater than 0 and smaller than 1")
  }
  if(!alpha > 0 & !alpha < 1){
    stop("Alpha value must be a number greater than 0 and smaller than 1")
  }
  if(!is.numeric(dat) & length(dat) > 1){
    stop("A numeric vector with a length greater than one must be provided")
  }

  n = length(dat)
  tValue <- qt((1 - alpha/2), df= (n - 1))

  upperMean <- mean(dat) + tValue * sd(dat) / sqrt(n)
  lowerMean <- mean(dat) - tValue * sd(dat) / sqrt(n)

  table <- cbind("mean" = mean(dat), 
                 "lowerCI" = lowerMean, 
                 "upperCI" = upperMean,
                 "sd" = sd(dat)
                 )

  return(table)
}
