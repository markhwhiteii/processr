#' Model 14 from the PROCESS Macro: Second-Stage Moderated Mediation
#' 
#' This function will perform second-stage moderated mediation, using the `lavaan` package.
#' It uses bias-corrected bootstrap resampling for the confidence intervals.
#' 
#' @param iv The name of the independent variable, as a character string.
#' @param dv The name of the dependent variable, as a character string.
#' @param med The name of the mediator, as a character string.
#' @param mod The name of the moderator, as a character string. If the moderator
#' is numeric with only 0s and 1s, it will return the simple indirect effects at the values of
#' 0 and 1; if it is numeric otherwise, it will return the simple indirect effects at a standard
#' deviation below the mean, a standard deviation above the mean, and at the mean.
#' @param data The data frame with the relevant variables.
#' @param samples The number of bootstrap resamples. Defaults to 5000.
#' @return Coefficients, standard errors, z-values, p-values, and confidence intervals
#' for all estimated parameters. The indirect effects will not return standard errors,
#' z-values, or p-values.
#' @export
model14 <- function(iv, dv, med, mod, data, samples=5000) {
  
  if (all(data[,mod] == 0 | data[,mod] == 1)) {
  
    data[,"medxmod"] <- data[,med]*data[,mod]
    model <- paste0(med, " ~ a*", iv, "
                    ", dv, " ~ b1*", med, " + b2*", mod, " + b3*medxmod + cp*", iv, "
                    imm := a*b3
                    ind_0 := a*b1
                    ind_1 := a*b1 + imm")
    set.seed(1839)
    out <- lavaan::parameterEstimates(
      lavaan::sem(model=model, data=data, se="boot", bootstrap=samples), 
      boot.ci.type="bca.simple")
    out[c(14:16),c(6:8)] <- NA
    out <- out[c(1:5, 14:16),c(4:10)]
    rownames(out) <- 1:nrow(out)
    return(out)
    
  } else {
    
    data[,"medxmod"] <- data[,med]*data[,mod]
    model <- paste0(med, " ~ a*", iv, "
                ", dv, " ~ b1*", med, " + b2*", mod, " + b3*medxmod + cp*", iv, "
                imm := a*b3
                
                ", mod, " ~ modmean*1
                ", mod, " ~~ modvar*", mod, "
                ind_lo := a*b1 + imm*(modmean-sqrt(modvar))
                ind_mn := a*b1 + imm*modmean
                ind_hi := a*b1 + imm*(modmean+sqrt(modvar))")
    set.seed(1839)
    out <- lavaan::parameterEstimates(
      lavaan::sem(model=model, data=data, se="boot", bootstrap=samples), 
      boot.ci.type="bca.simple")
    out[c(17:20),c(6:8)] <- NA
    out <- out[c(1:5, 17:20),c(4:10)]
    rownames(out) <- 1:nrow(out)
    return(out)
  }
}
