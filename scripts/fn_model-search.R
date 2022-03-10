# Input training dataset (non-parameter columns removed) to perform
# stepwise selection using BIC 
run_stepwise <- function(dataset){
  min_mod <- lm(error~1, data = dataset)
  full_mod <- lm(error~., data = dataset)
  selected <- step(object = min_mod,
                   scope = list(lower = min_mod, upper = full_mod),
                   direction = "both",
                   k = log(nrow(dataset)))
  return(selected)
}

# Input training dataset (non-parameter columns removed) to run regsubsets
# (from leaps package), find the model with smallest BIC, and return it
# as an lm object
run_subset <- function(dataset){
  subsets <- regsubsets(error~., data = dataset, nvmax = 15, really.big = T)
  terms <- names(coef(subsets, which.min(summary(subsets)$bic)))[-1]
  terms <- gsub("str[A-Z]*", "str", terms)
  f <- as.formula(paste("error ~ ", terms))
  lm_mod <- lm(formula = f, data = dataset)
  return(lm_mod)
}