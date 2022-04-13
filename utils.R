
icc <- function(model){
  # A small function to calculate ICC from a lme4 model. 
  # Dependencies: broom.mixed, dplyr, lme4
  
  icc <- data.frame(broom.mixed::tidy(model, effects=c("ran_pars"))) %>%
    mutate(variance = estimate^2) %>%
    mutate(icc = variance / sum(variance)) %>%
    filter(term == "sd__(Intercept)")
  return(icc$icc)
}