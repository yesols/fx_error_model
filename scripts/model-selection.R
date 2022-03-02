library(leaps)


###   Read in organized data tables    ###

# Full tables:
fold <- read.csv("outputs/table_full_f.csv")
bind <- read.csv("outputs/table_full_b.csv")
fold_tr <- read.csv("outputs/tr_table_full_f.csv")
bind_tr <- read.csv("outputs/tr_table_full_b.csv")

# FoldX-only tables:
foldx <- read.csv("outputs/table_fxonly_f.csv")
bindx <- read.csv("outputs/table_fxonly_b.csv")
foldx_tr <- read.csv("outputs/tr_table_fxonly_f.csv")
bindx_tr <- read.csv("outputs/tr_table_fxonly_b.csv")




###   Stepwise model selection ###


# Full dataset

fold_min <- lm(error~1, data=fold_tr)
fold_full <- lm(error~., data=fold_tr)
mod_step_aic_f <- step(object = fold_min,
                   scope = list(lower=fold_min, upper=fold_full),
                   direction = "both")  
mod_step_bic_f <- step(object = fold_min, 
                    scope = list(lower=fold_min, upper=fold_full),
                    direction = "both",
                    k = log(nrow(fold_tr))) 

bind_min <- lm(error~1, data=bind_tr)
bind_full <- lm(error~., data=bind_tr)
mod_step_aic_b <- step(object = bind_min,
                   scope = list(lower=bind_min, upper=bind_full),
                   direction = "both") 
mod_step_bic_b <- step(object = bind_min,
                       scope = list(lower=bind_min, upper=bind_full),
                       direction = "both",
                       k = log(nrow(bind_tr))) 
# FoldX-only dataset:

foldx_min <- lm(error~1, data=foldx_tr)
foldx_full <- lm(error~., data=foldx_tr)
mod_step_aic_fx <- step(object = foldx_min, 
                    scope = list(lower=foldx_min,upper=foldx_full), 
                    direction = "both") 
mod_step_bic_fx <- step(object = foldx_min, 
                        scope = list(lower=foldx_min,upper=foldx_full),
                        direction = "both", k = log(nrow(foldx_tr)))

bindx_min <- lm(error~1, data=bindx_tr)
bindx_full <- lm(error~., data=bindx_tr)
mod_step_aic_bx <- step(object = bindx_min, 
                    scope = list(lower=bindx_min,upper=bindx_full), 
                    direction = "both") 
mod_step_bic_bx <- step(object = bindx_min, 
                     scope = list(ower=bindx_min,upper=bindx_full),
                     direction = "both", k = log(nrow(bindx_tr))) 



###    Best Subset Selection    ###


mod_subset_f <- regsubsets(error~., data = fold_tr, nvmax = 15, really.big = T)
mod_subset_b <- regsubsets(error~., data = bind_tr, nvmax = 15, really.big = T)

mod_subset_fx <- regsubsets(error~., data = foldx_tr, nvmax = 15)
mod_subset_bx <- regsubsets(error~., data = bindx_tr, nvmax = 15)

# Coefficients of the smallest bic
subset_terms_f <- coef(mod_subset_f, which.min(summary(mod_subset_f)$bic))
subset_terms_b <- coef(mod_subset_b, which.min(summary(mod_subset_b)$bic))
subset_terms_fx <- coef(mod_subset_fx, which.min(summary(mod_subset_fx)$bic))
subset_terms_bx <- coef(mod_subset_bx, which.min(summary(mod_subset_bx)$bic))



### Save models as RDS ###

mods <- list(mod_step_aic_f, mod_step_bic_f,
             mod_step_aic_b, mod_step_bic_b,
             mod_step_aic_fx, mod_step_bic_fx,
             mod_step_aic_bx, mod_step_bic_bx,
             mod_subset_f,
             mod_subset_b,
             mod_subset_fx,
             mod_subset_bx)


#saveRDS(mods, "outputs/models")







mods <- readRDS("outputs/models")

# formulas for later 
#(full best subset names won't work this way because of strE, strNONE, etc)
stepformula_f <- formula(sum_step_f$terms)
stepformula_b <- formula(sum_step_b$terms)
stepformula_fx <- formula(sum_step_fx$terms)
stepformula_bx <- formula(sum_step_bx$terms)
subsetformula_fx <- as.formula(paste("error~", 
                                     paste(names(subset_terms_fx[-1]), 
                                           collapse="+"), sep=""))
subsetformula_bx <- as.formula(paste("error~", 
                                     paste(names(subset_terms_bx[-1]), 
                                           collapse="+"), sep=""))




