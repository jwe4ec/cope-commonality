# ---------------------------------------------------------------------------- #
# Define "boot.yhat.prec()" ----
# ---------------------------------------------------------------------------- #

# Define an adaptation of "yhat" package's function "boot.yhat()" to optionally
# take a "prec" argument that can be passed to "calc.yhat()"
# - This is so that when running "booteval.yhat(prec = D)" the output will actually 
#   show results rounded to the D decimal places that the user specifies
# - This is achieved by running "booteval.yhat()" on the results from "boot(statistic 
#   = boot.yhat.prec, prec = D)" instead of from "boot(statistic = boot.yhat)"
# - Link to original "boot.yhat()":
#   https://github.com/cran/yhat/blob/3beef32b280f86b63c3a7cc4ad08d08a4ad3d4a7/R/boot.yhat.r

boot.yhat.prec <- function (data, indices, lmOut, regrout0, prec = NULL)
{
  data <- data[indices, ]
  blmOut <- lm(formula = lmOut$call$formula, data = data)
  
  if (!is.null(prec)) {
    regrout <- calc.yhat(blmOut, prec)
  } else {
    regrout <- calc.yhat(blmOut)
  }
  
  pm <- as.vector(regrout$PredictorMetrics[-nrow(regrout$PredictorMetrics), 
  ])
  apsm <- as.vector(as.matrix(regrout$APSRelatedMetrics[-nrow(regrout$APSRelatedMetrics), 
                                                        -2]))
  pdm <- as.vector(as.matrix(regrout$PairedDominanceMetrics))
  tau <- vector(length = ncol(regrout$PredictorMetrics))
  for (i in 1:length(tau)) {
    s1 <- (regrout0$PredictorMetrics[1:(nrow(regrout0$PredictorMetrics) - 
                                          1), i])
    s2 <- (regrout$PredictorMetrics[1:(nrow(regrout$PredictorMetrics) - 
                                         1), i])
    tau[i] <- cor.test(s1, s2, method = "kendall", exact = FALSE)$estimate
  }
  c(pm, pdm, apsm, tau)
}

# ---------------------------------------------------------------------------- #
# Define "beta_rsq()" ----
# ---------------------------------------------------------------------------- #

# Define function to get beta and R^2 for total effect

beta_rsq <- function(data, bootstrap_rows, formula) { 
  # Fit linear model
  
  model <- lm(
    formula = formula,
    data = data[bootstrap_rows, ]
  )
  
  # Get and name beta for total effect
  
  beta <- summary(model)$coefficients[, "Estimate"][2]
  predictor_name <- names(beta)
  names(beta) <- paste0(predictor_name, "_beta")
  
  # Get and name R^2
  
  rsq <- summary(model)$r.squared
  names(rsq) <- "rsq"
  
  # Return both statistics
  
  beta_rsq <- c(beta, rsq)
  
  return(beta_rsq)
}

# ---------------------------------------------------------------------------- #
# Define "bootstrap_beta_rsq()" ----
# ---------------------------------------------------------------------------- #

# Define function to bootstrap beta and R^2 for total effect
# - Includes option to round results to D number of decimal digits ("prec = D")

bootstrap_beta_rsq <- function(seed, data, statistic = beta_rsq, formula, R, prec = NULL) {
  # Bootstrap beta and R^2
  
  set.seed(seed)
  boot_out <- boot(data = data, statistic = statistic, formula = formula, R = R)
  
  # Get point estimates for beta and R^2
  
  est_beta <- boot_out$t0[1]
  est_rsq <- boot_out$t0[2]
  
  # Get 95% CIs for beta and R^2
  
  ci_beta <- boot.ci(boot_out, index = 1, type = "perc")$percent[4:5]
  ci_rsq <- boot.ci(boot_out, index = 2, type = "perc")$percent[4:5]
  
  # Round and return results
  
  res <- c(est_beta,
           ci_beta_ll = ci_beta[1],
           ci_beta_ul = ci_beta[2],
           est_rsq,
           ci_rsq_ll = ci_rsq[1],
           ci_rsq_ul = ci_rsq[2])
  
  if (!is.null(prec)) {
    res <- round(res, prec)
  }
  
  return(res)
}

# ---------------------------------------------------------------------------- #
# Define function to compute count and percentages for demographics ----
# ---------------------------------------------------------------------------- #

get_count_perc <- function(df, col_name) {
  df_name <- deparse(substitute(df))
  
  count   <- sum(df[[col_name]], na.rm = T)
  total_n <- nrow(df)
  perc    <- round((count / total_n) * 100, 2)
  
  count_perc <- data.frame(df_name  = df_name,
                           col_name = col_name,  
                           count    = count, 
                           total_n  = total_n, 
                           perc     = perc)
  
  return(count_perc)
}