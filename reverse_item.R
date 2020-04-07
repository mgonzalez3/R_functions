# Takes a vector containg item data and reverse the values.

reverse_item <- function(item, minimum = NULL, maximum = NULL) {

    # Checks if minimum value was provided. If not min function is called to
    # determined this value
    if(length(minimum) == 0) {
        minimum <- min(item)
        warning("Minimum value was determined based on item sample data")
    }

    # Checks if minimum value was provided. If not min function is called to
    # determined this value
    if(length(maximum) == 0) {
        maximum <- max(item)
        warning("Maximum value was determined based on item sample data")
    }

    # reverses item
    reversed_item <- (maximum + minimum) - item

    return(reversed_item)
}

