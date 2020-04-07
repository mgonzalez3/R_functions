# Function takes data frames and a character vector of the y column. Then it 
# will regress on column on y all others columns in the data frame. The 
# functions returns the value of the coefficients

all_column_lm  <- 
  function(dat, y) { # Takes the dataframe and a character vector of y columns
    # Nested function to run regression model on the columns
    column_lm <- 
      function(columns){
        reg_model <- lm(reg_y ~ columns)
        model_sum <- summary(reg_model)
        return(model_sum$coefficients[2,])
      }
    
    reg_y<- dat[, y] # creates a vector of the y column
    dat[, y] <- NULL # deletes the y column from the data frame
    
    return(apply(dat, 2, column_lm))
  }

