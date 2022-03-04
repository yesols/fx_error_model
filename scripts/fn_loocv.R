# LOOCV with a modification:
# Instead of leaving one data out, it leaves a group of data points belonging to a system.
# The idea is that this will provide more realistic CV result for the nature of error model.

# This function takes as input:
#    1. lm formula to test
#    2. data frame to test (any one of the organized data tables like "bind", "fold", etc)
# and outputs a list containing:
#    1. $formula: lm formula that was tested
#    2. $means: mean coverage, and mean interval width
#    3. $df: data frame that contains predicted error, prediction interval, and 
#            test result of whether or not the actual error falls under upr pred interval

loocv <- function(f, dataset){ #inpu: lm formula, dataframe (e.g. fold or bind)
  df <- dataset[c("systems", "mut", "ddg_exp", "total", "error")]
  df$err_pred <- NA
  df$lwr <- NA
  df$upr <- NA
  df$test <- NA
  sys <- unique(dataset$systems)
  str_values <- unique(dataset$str) #vector of all existing str as produced by dssp
  for (i in 1:length(sys)){
    tset <- dataset[dataset$systems != sys[i], ]
    vset <- dataset[dataset$systems == sys[i], ]
    lm_i <- lm(formula = f, data = tset)
    lm_i$xlevels[["str"]] <- str_values #to prevent errors when some levels don't exist
    pred_i <- predict(lm_i, newdata = vset, interval = "prediction")
    for (r in 1:nrow(pred_i)){
      row_i <- rownames(pred_i)[r]
      df[rownames(df) == row_i, "err_pred"] <- pred_i[rownames(pred_i) == row_i, "fit"]
      df[rownames(df) == row_i, "lwr"] <- pred_i[rownames(pred_i) == row_i, "lwr"]
      df[rownames(df) == row_i, "upr"] <- pred_i[rownames(pred_i) == row_i, "upr"]
    }
  }
  df$test <- df$error < df$upr #not testing df$error > df$lwr as in original code
  out <- list()
  out$formula <- f
  out$means <- c(mean(df$test), mean(df$upr)) #mean coverage, mean interval width
  out$df <- df
  return(out)
}  