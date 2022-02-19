
################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
pacman::p_load("haven","dplyr","estimatr","texreg", "essurvey","ggplot2")



################################################################################


dk <- import_country(country = "Denmark", rounds = 7, ess_email = "madslangs@gmail.com") %>%
  recode_missings() %>%
  zap_labels()


temp <- dk %>%
  mutate(contact = ifelse(dfegcf == 3, 0, 1)) %>%
  mutate(
    smegbli = zap_labels(smegbli),
    smegbhw = zap_labels(smegbhw),
    smctmbe = zap_labels(smctmbe),
    smegbli = ifelse(smegbli == 2, 0, smegbli),
    smegbhw = ifelse(smegbhw == 2, 0, smegbhw),
    smctmbe = ifelse(smctmbe == 2, 0, smctmbe)) %>%
  mutate(racism = smegbli + smegbhw + smctmbe) %>% 
  filter(!is.na(imueclt)) %>%
  mutate(cultural_treat = unlist(lapply(imueclt, function(x){return(10 - x)}))) %>%
  filter(!is.na(contact)) 


p <- ggplot(data = temp,aes(y = cultural_treat, x = contact, weight = pspwght, alpha=pspwght)) +
  geom_jitter(width = 0.1, height = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") + 
  labs(y = "Cultural xenophobic threat", x = "Having friends from minority race or ethnic group") +
  scale_x_continuous(breaks=c(0,1), expand=c(0.2,0.2)) +
  scale_y_continuous(breaks=seq(0,10)) +
  theme_light() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 
p

# OLS with controls for education and age

model <- lm_robust(
  formula = cultural_treat ~ contact, 
  data=temp, 
  weight = pspwght
)
screenreg(model, include.ci = FALSE, digits = 3)



# OLS with controls for education and age

model <- lm_robust(
  formula = cultural_treat ~ contact + eisced + agea, 
  data=temp, 
  weight = pspwght
)
screenreg(model, include.ci = FALSE, digits = 3)


# OLS with perceived country homogeneity

model <- lm_robust(
  formula = cultural_treat ~ contact + eisced + agea, 
  data=temp, 
  weight = pspwght
)
screenreg(model, include.ci = FALSE, digits = 3)




