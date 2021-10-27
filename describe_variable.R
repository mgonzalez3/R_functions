
# This function is a wrapper for the tidyverse and base r descriptives
# statistics functions.

# The function takes a dta frame and character string indicating variable name.
# If the variable indicated is a numeric returns the following: arithmetic mean,
# standard deviation, normal theory lower and upper 95% confidence intervals,
# median, min and max values.

describe <- function(data, var.desc){

    stopifnot(
        is.numeric( data[[var.desc]] )
        )

    res <- data %>%
        summarize("N" = sum(!is.na(.data[[var.desc]])),
                  "mean"= mean(.data[[var.desc]], na.rm=TRUE),
                  "sd" = sd(.data[[var.desc]], na.rm=TRUE),
                  "95%CIL" = mean - qnorm(0.975) * sd/sqrt(N),
                  "95%CIU" = mean + qnorm(0.975) * sd/sqrt(N),
                  "median" = median(.data[[var.desc]], na.rm=TRUE),
                  "min" = min(.data[[var.desc]], na.rm = TRUE),
                  "max" = max(.data[[var.desc]], na.rm = TRUE),
                  .groups = "keep"
        )

    return(res)
}


