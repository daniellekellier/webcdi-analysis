# Functions for modelling quantiles for Wordbank data.

fit_gcrq <- function(x) {
  mod <- try(gcrq(formula = mean ~ ps(age, monotone = 1, 
                                      lambda = 1000), 
                  tau = taus, data = x))
  
  if(inherits(mod, "try-error"))
  {
    return(NA)
  }
  
  return(mod)
}

pred_gcrq <- function(x, mods) {
  mod <- mods[[x$language[1]]]
  
  if (is.na(mod[1])) {
    return(expand.grid(age = x$age, 
                       language = x$language[1], 
                       percentile = as.character(taus*100), 
                       pred = NA))
  } else {
    preds <- predictQR_fixed(mod, 
                             newdata = x) %>%
      data.frame %>%
      mutate(age = x$age, 
             language = x$language) %>%
      gather(percentile, pred, starts_with("X")) %>%
      mutate(percentile = as.character(as.numeric(str_replace(percentile, 
                                                              "X", "")) 
                                       * 100))
    return(preds)
  }
}

fit_sex <- function(x) {
  robustbase::glmrob(cbind(production, no_production) ~ age * sex - sex,
                     family = "binomial",
                     data = x)
}


pred_sex_source <- function(x) {
  x$pred <- predict(sex_models[[x$source[1]]], 
                    newdata = x, type = "response")
  return(x)
}

fit_ses <- function(x) {
  robustbase::glmrob(cbind(production, no_production) ~ 
                       age * mom_ed  - mom_ed,
                     family = "binomial",
                     data = x)
}

pred_ses_source <- function(x) {
  x$pred <- predict(ses_models[[x$source[1]]],
                    newdata = x, type = "response")
  return(x)
}