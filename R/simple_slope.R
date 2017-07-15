#' Get the Simple Slope for the Moderator at a Given Value
#' 
#' The `model1` function will return simple slopes at -1 SD, M, and +1 SD of a continuous 
#' moderator, or at the two levels of a dichotomous moderator. If you want some other 
#' value, you can use this function, where you specify your own value at the moderator
#' that you want to see the simple slope at.
#' 
#' @param iv The name of the independent variable, as a character string
#' @param dv The name of the dependent variable, as a character string
#' @param mod The name of the moderator, as a character string
#' @param value The numeric value that you want to see the simple slope at
#' @param data The data frame with the relevant variables
#' @return The coefficient, standard error, t-statistic, and p-value of the simple slope
#' @export
simple_slope <- function(iv, dv, mod, value, data) {
  modv <- data[,mod] - value
  out <- broom::tidy(lm(data[,dv] ~ data[,iv]*modv))[2,]
  out[1,1] <- paste("when", mod, "=", round(value, 3))
  return(out)
}
