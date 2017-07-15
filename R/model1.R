#' Model 1 from the PROCESS Macro: Two-Way Interaction
#' 
#' Performs a simple two-way interaction and returns simple slopes, as per
#' Model 1 from the PROCESS Macro.
#' 
#' @param iv The name of the independent variable, as a character string
#' @param dv The name of the dependent variable, as a character string
#' @param mod The name of the moderator, as a character string. If the moderator
#' is numeric with only 0s and 1s, it will return the simple slopes at the values of
#' 0 and 1; if it is numeric otherwise, it will return the simple slopes at a standard
#' deviation below the mean, a standard deviation above the mean, and at the mean
#' @param data The data frame with the relevant variables
#' @return The coefficients, standard errors, t-statistics, and p-values of 
#' the regression coefficients table and the simple slopes
#' @export
model1 <- function(iv, dv, mod, data) {
  coefs <- broom::tidy(lm(data[,dv] ~ data[,iv]*data[,mod]))
  coefs[,1] <- c("intercept", iv, mod, "interaction")
  
  if (all(data[,mod] == 0 | data[,mod] == 1)) {
    sss <- simple_slope(iv, dv, mod, 0, data)
    sss <- rbind(sss, simple_slope(iv, dv, mod, 1, data))
  } else {
    sss <- simple_slope(iv, dv, mod, mean(data[,mod]) - sd(data[,mod]), data)
    sss <- rbind(sss, simple_slope(iv, dv, mod, mean(data[,mod]), data))
    sss <- rbind(sss, simple_slope(iv, dv, mod, mean(data[,mod]) + sd(data[,mod]), data))
  }
  out <- rbind(coefs, sss)
  rownames(out) <- 1:nrow(out)
  return(out)
}
