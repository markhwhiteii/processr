#' Model 4 from the PROCESS Macro: Simple Mediation
#' 
#' This function will perform simple mediation, using the `lavaan` package.
#' It uses maximum likelihood estimation and performs bias-corrected bootstrap 
#' resampling for the confidence intervals.
#' 
#' @param iv The name of the independent variable, as a character string
#' @param dv The name of the dependent variable, as a character string
#' @param med The name of the mediator, as a character string
#' @param data The data frame with the relevant variables
#' @param samples The number of bootstrap resamples. Defaults to 5000
#' @return Coefficients, standard errors, z-values, p-values, and confidence intervals
#' for all estimated parameters. The indirect effect will not return standard errors,
#' z-values, or p-values
#' @export
model4 <- function(iv, dv, med, data, samples=5000) {
  model <- paste0(med, " ~ a*", iv, "
                  ", dv, " ~ b*", med, " + cp*", iv, "
                  ind := a*b
                  c := cp + a*b")
  set.seed(1839)
  out <- lavaan::parameterEstimates(
    lavaan::sem(model=model, data=data, se="boot", bootstrap=samples), 
    boot.ci.type="bca.simple")
  out[7,c(6:8)] <- NA
  out <- out[c(1,2,3,7,8),c(4:10)]
  rownames(out) <- 1:nrow(out)
  return(out)
}
