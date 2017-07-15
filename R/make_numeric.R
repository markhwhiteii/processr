#' Convert Dichotomous Factors to Numeric, 0 or 1, Variable
#' 
#' The functions in the `processr` package that a dichotomous variable be turned into a 
#' numeric variable. This function takes a dichotomous variable and makes
#' it a vector of 0s and 1s, where the reference value is coded zero. It returns a
#' data frame with this new variable added onto it.
#' 
#' @param var The name of the dichotomous factor variable to be converted, as a character string
#' @param ref The value of the factor that you consider the reference category, as a character string; 
#' this will be converted to zero
#' @param newvar The name of the new variable, as a character string
#' @param data The data frame
#' @return A data frame with the new variable added onto it
#' @export
make_numeric <- function(var, ref, newvar, data) {
  data[,newvar] <- ifelse(data[,var]==ref, 0, 1)
  return(data)
}
