

################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
pacman::p_load("haven","dplyr","estimatr","texreg", "essurvey")




######## PLAYAROUND ############################################################



# 1
df <- import_country(country = "Denmark", rounds = 7, ess_email = "madslangs@gmail.com") %>%
  recode_missings() %>%
  filter(cntry == "DK") %>%
  filter(brncntr == 1 & mocntr == 1 & facntr == 1)


  
# 2 

df <- df %>%
  mutate(
    smegbli = zap_labels(smegbli),
    smegbhw = zap_labels(smegbhw),
    smctmbe = zap_labels(smctmbe),
    smegbli = ifelse(smegbli == 2, 0, smegbli),
    smegbhw = ifelse(smegbhw == 2, 0, smegbhw),
    smctmbe = ifelse(smctmbe == 2, 0, smctmbe),
    classic_racism_index = smegbli + smegbhw + smctmbe
  ) 



### OLS Replication, exercises 2 ###
df <- import_country(country = "Denmark", rounds = 7, ess_email = "madslangs@gmail.com") %>%
  recode_missings() %>%
  filter(brncntr == 1 & mocntr == 1 & facntr == 1)


reg_df <- df %>%
  mutate(
    outcome = case_when(
      admaimg == 1 ~ alpfpe,
      admaimg == 2 ~ alpfpne,
      admaimg == 3 ~ allbpe,
      admaimg == 4 ~ allbpne,
      TRUE ~ as.numeric(NA)),
    lowskill = ifelse(outcome >= 3, 1, 0),
    outcome = case_when(
      outcome == 4 ~ 1,
      outcome == 3 ~ 2,
      outcome == 2 ~ 3,
      outcome == 4 ~ 4
    ),
    education = edlvddk,
    income = hinctnta,
    reciprocity = imbleco
    ) 
  
model1 <- lm_robust(outcome ~ lowskill , reg_df)
screenreg(model1, digits = 3, include.ci = FALSE)

model2 <- lm_robust(outcome ~ lowskill + education*lowskill, reg_df)
screenreg(model2, digits = 3, include.ci = FALSE)
