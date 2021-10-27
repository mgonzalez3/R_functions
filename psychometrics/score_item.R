# Function based on the psych package score.multiple.choice fucntion. 
# This function takes a numeric or character vector of key values and compares 
# them to item responses. The function returns a matrix of 0 (Incorrect) and 
# 1 (correct)

score_item <- function(key, dat){
    #convert a data matrix or data with multiple choice responses to correct/incorrect
    if(!is.matrix(dat)) {
        if(!is.data.frame(dat)) {
        stop("data must be either a data frame or matrix!")
        } else dat <- as.matrix(dat)
    }
    
    if(length(key) == dim(dat)[2]) {
        item_scores <- t(t(dat)==items_key) #scores as true or false
        item_scores <- item_scores + 0    #converts t/f to 1/0  
    } else {stop("key must have as many elements as columns of 'data' ")}
  
    return(item_scores)
}

