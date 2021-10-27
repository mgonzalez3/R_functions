# Takes any vector or df and returns a character
remove_accents <- function(x, only_char = TRUE) {
    if(!require(stringi) && !require(purrr)){
        warning("Requires dependencies stringi and purrr needs to be loaded.
                Use library() or install.packages()")
    }

    if(is.data.frame(x) && only_char){
        # Returns data frame without coercion of variables to character
        modify_if(x, is.character, stri_trans_general,"Latin-ASCII")
    } else if(is.data.frame(x) && !only_char){
        # Returns data frame with coercion of variables to character
        modify(teacher_data, stri_trans_general,"Latin-ASCII")
    } else {
        # coerces to character
        stri_trans_general(x, "Latin-ASCII")
    }
}
