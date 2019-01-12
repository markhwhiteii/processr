#' Model 7 from the PROCESS Macro: First-Stage Moderated Mediation
#' 
#' This function will perform first-stage moderated mediation, using the `lavaan` package.
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
#' z-values, or p-values. The labels for each parameter estimate match the labels
#' provided by Hayes (2015), An Index and Test of Linear Moderated Mediation, in
#' Multivariate Behavioral Research.
#' @export
model7 <- function(iv, dv, med, mod, data, samples = 5000) {
  data[, "ivxmod"] <- data[, iv] * data[, mod]
  dichot <- all(data[, mod] == 0 | data[, mod] == 1)
  
  if (dichot) {
    model <- paste0(
      med, " ~ a1 * ", iv, " + a2 * ", mod, " + a3 * ivxmod\n", 
      dv, " ~ b * ", med, " + cp * ", iv, "\n",
      "imm := a3 * b\nind_0 := a1 * b\nind_1 := a1 * b + imm"
    )
  } else {
    model <- paste0(
      med, " ~ a1 * ", iv, " + a2 * ", mod, " + a3 * ivxmod\n", 
      dv, " ~ b*", med, " + cp * ", iv, "\n",
      "imm := a3*b\n",
      
      mod, " ~ modmean * 1\n", 
      mod, " ~~ modvar * ", mod, "\n",
      
      "ind_lo := a1 * b + imm * (modmean - sqrt(modvar))\n",
      "ind_mn := a1 * b + imm * modmean\n",
      "ind_hi := a1 * b + imm * (modmean + sqrt(modvar))"
    )
  }
  
  set.seed(1839)
  out <- lavaan::parameterEstimates(
    lavaan::sem(model = model, data = data, se = "boot", bootstrap = samples), 
    boot.ci.type = "bca.simple"
  )
    
  if (dichot) {
    out[14:16, 6:8] <- NA
    out <- out[c(1:5, 14:16), 4:10]
    rownames(out) <- 1:nrow(out)
  } else {
    out[17:20, 6:8] <- NA
    out <- out[c(1:5, 17:20), 4:10]
    rownames(out) <- 1:nrow(out)
  }
  
  return(out)
}
