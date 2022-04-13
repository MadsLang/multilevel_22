

################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
pacman::p_load("haven","dplyr","estimatr","texreg", "essurvey","httr", "lme4",
               "ggplot2","tidyverse","ggrepel","devtools","influence.ME", "cowplot")
#devtools::install_github("melff/iimm")
library("iimm")





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

model3 <- lm_robust(outcome ~ lowskill + education*lowskill + reciprocity, reg_df)
screenreg(model3, digits = 3, include.ci = FALSE)


# PCA 
X <- temp %>%
  select(ppltrst,pplfair,pplhlp) %>%
  remove_missing(vars = ppltrst,pplfair,pplhlp)

pca_result <- prcomp(X, scale = FALSE)

fviz_eig(pca_result, barfill="white", barcolor ="darkblue") + theme_light()



new_cols <- data.frame(get_pca_ind(pca_result)$contrib)

temp <- bind_cols(temp %>%
                    remove_missing(vars = c("ppltrst","pplfair","pplhlp")),new_cols)

list(u = "hello")

#### Multilevel

country <- read_stata("ESSMD-2014-cntry/ESSMD-2014-cntry_F1.dta") %>%
  recode_missings()

httr::set_config(config(ssl_verifypeer = 0L))

ess <- import_rounds(rounds = 7, ess_email = "madslangs@gmail.com") %>%
  recode_missings()

df <- ess %>% 
  inner_join(country, on = "cntry")


# Exercise 3a
temp <- df %>%
  mutate(
    imtcjob = zap_labels(imtcjob),
    imbleco = zap_labels(imbleco),
    rlgueim = zap_labels(rlgueim),
    imbgeco = zap_labels(imbgeco),
    imueclt = zap_labels(imueclt),
    imwbcnt = zap_labels(imwbcnt),
    # Generate additive scale 0 to 10
    xeno_ij = 10 - ((imtcjob + imbleco + rlgueim + imbgeco + imueclt + imwbcnt) / 6),
  )

m <- lmer(xeno_ij ~ (1 | cntry), data = temp)
summary(m)

0.28/(0.28+2.79)

m <- lmer(dfegcon ~ (1 | cntry), data = temp)
summary(m)

0.72/(0.72+4.08)

# Exercise 3b
temp <- df %>%
  mutate(
    ## Make variables numeric
    smegbli = zap_labels(smegbli),
    smegbhw = zap_labels(smegbhw),
    smctmbe = zap_labels(smctmbe),
    ## Recode 2 (No) to zero
    smegbli = case_when(smegbli == 2 ~ 0, TRUE ~ smegbli),
    smegbhw = case_when(smegbhw == 2 ~ 0, TRUE ~ smegbhw),
    smctmbe = case_when(smctmbe == 2 ~ 0, TRUE ~ smctmbe),
    ## Generate additive racist prejudice scale 0 to 3
    racism = smegbli + smegbhw + smctmbe,
  ) %>%
  mutate(c_gdppc_2012 = c_gdppc_2012 / 1000) %>%
  select(racism, cntry, gndr, agea, eduyrs, c_gini_2012, c_gdppc_2012) %>%
  zap_labels()


m <- lmer(racism ~ c_gini_2012 + c_gdppc_2012 +  + agea + gndr + (1 | cntry), data = temp)
summary(m)



m <- lmer(almuslv ~ (1 | cntry), data = df)
summary(m)

# Calculate ICC: Between-group-variance / (between-group variance + within-group-variance)

0.18/(0.18+0.78)*100



# Exercise 5

temp <- ess %>%
  mutate(geo_treat = case_when(
    cntry %in% c("CH","FR","IS","LU","NL","NO","PT","SI") ~ 0,
    cntry %in% c("DK","FI","HR","IE","IT","SE","SK","UA") ~ 1,
    cntry %in% c("BE","ES","LT","PL") ~ 2,
    cntry %in% c("AT","BG","CZ","DE","EE","GB","GR","LV","RO") ~ 3,
    cntry %in% c("HU") ~ 4,
    cntry %in% c("CY") ~ 5,
    cntry %in% c("RU","TR") ~ 6)) %>%
  inner_join(country, on = "cntry") %>%
  mutate(c_gdppc_2012 = c_gdppc_2012 / 1000) 


t <- temp %>% group_by(cntry) %>% summarise(x = mean(almuslv, na.rm=T), y = first(geo_treat))
p <- ggplot(t, aes(x = x, y = y, label = cntry)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_text(vjust=-0.5) + 
  theme_light() +
  labs(x = "Muslim predjudice", y = "Geopolitical treat")
p

m <- lmer(almuslv ~ geo_treat + c_gini_2012 + c_gdppc_2012 + agea + gndr + (1 | cntry), temp, weights = pspwght)
summary(m) 



# Exercise 6a
temp <- temp %>%
  mutate(
  # Educational attainment
  edu_attain = case_when(
    edulvlb < 200 ~ 1,
    edulvlb > 199 & edulvlb < 300 ~ 2,
    edulvlb > 299 & edulvlb < 400 ~ 3,
    edulvlb > 399 & edulvlb < 500 ~ 4,
    edulvlb > 499 & edulvlb < 600 ~ 5,
    edulvlb > 599 & edulvlb < 700 ~ 6,
    edulvlb > 699 & edulvlb < 801 ~ 7,
    TRUE ~ as.numeric(NA)
  ),
  # Watching TV, but not news on TV
  TV_not_news = tvtot - tvpol,
  # Immigrant friends
  dfegcf = max(dfegcf, na.rm = TRUE) - dfegcf,
  brncntr = as_factor(brncntr), 
  ctzcntr = as_factor(ctzcntr),
  rlgblg = as_factor(rlgblg),
  rlgdnm = as_factor(rlgdnm),
  uempla = as_factor(uempla),
  rlgdnm = case_when( # Add atheists to the religion variable
    rlgblg == "No" ~ "Atheist",
    rlgdnm=="Not applicable"|rlgdnm=="Refusal"|rlgdnm=="No answer" ~ as.character(NA),
    TRUE ~ as.character(rlgdnm)
  ) %>% factor(),
  gndr = as_factor(gndr),
  gndr = case_when(
    gndr == "No answer" ~ as.character(NA),
    TRUE ~ as.character(gndr)
  ) %>% factor()
  ) %>%
  inner_join(., read_dta("Schlueter.dta") %>%
               mutate(
                 z_muslim_j = (muslim_j - mean(muslim_j, na.rm= TRUE)) / sd(muslim_j, na.rm = TRUE)
               ), by = "cntry") 



Model_2 <- lmer(
  almuslv ~ gndr + agea + edu_attain + uempla + hincfel +
    dfegcf + rlgdnm + TV_not_news + muslim_j + mipex_j + 
    relstate_j + claims_j + (1 + TV_not_news | cntry), data  = temp
)

summary(Model_2)


# Predict U_01 as empirical Bayes predictions
blups <- as.data.frame(ranef(Model_2) %>% as_tibble()) %>% filter(term == "TV_not_news") %>% arrange(condval)

p <- ggplot(data = blups, aes(y = condval, x = grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-0.25, 0, 0.25, 0.5), 
                     labels = c("-0.25", expression(gamma["00"]), "0.25", "0.5")) +
  labs(y = expression(Racism["ij"]), x = "") +
  theme_minimal()
p



# Exercise 6b

z_std <- function(x) {
  return((x - mean(x)) / sd(x))
}

temp <- ess %>%
  recode_missings() %>%
  mutate(
    # Threat & contact
    symb_threat_ij = 10 - ((zap_labels(imueclt) + zap_labels(imwbcnt) + zap_labels(rlgueim)) / 3),
    real_threat_ij = 10 - ((zap_labels(imtcjob) + zap_labels(imbleco) + zap_labels(imwbcrm)) / 3),
    contact_ij = zap_labels(dfegcon), # Everyday contact
    ## Controls
    subj_income_ij = 4 - zap_labels(hincfel),
    eduyrs_ij = case_when(
      eduyrs > mean(eduyrs, na.rm = TRUE) + 3*sd(eduyrs, na.rm = TRUE) ~ 
        mean(eduyrs, na.rm = TRUE) + 3*sd(eduyrs, na.rm = TRUE),
      TRUE ~ zap_labels(eduyrs)),
    agea_ij = zap_labels(agea),
    relig_ij = zap_labels(rlgdgr),
    div_nbh_ij = zap_labels(acetalv),
    immiback_ij = case_when(
      as_factor(facntr) == "No" | as_factor(mocntr) == "No" ~ "Yes",
      as_factor(facntr) == "Yes" | as_factor(mocntr) == "Yes" ~ "No",
      TRUE ~ as.character(NA)) %>% factor(),
    conserve_ij = 6 - ((zap_labels(impsafe) + zap_labels(ipstrgv) + zap_labels(ipfrule) + zap_labels(ipmodst) + zap_labels(ipbhprp) + zap_labels(imptrad)) / 6),
    # Categorical variables
    cntry = as_factor(cntry) %>% fct_drop(),
    ctzcntr = as_factor(ctzcntr) %>% fct_drop(),
    gndr_ij = as_factor(gndr) %>% fct_drop()
  ) %>%
  filter(cntry != "IL" & agea >= 18 & ctzcntr == "Yes") %>%
  select(symb_threat_ij, real_threat_ij, contact_ij, eduyrs_ij, agea_ij, gndr_ij, 
         subj_income_ij, relig_ij, div_nbh_ij, immiback_ij, conserve_ij, cntry) %>%
  drop_na() %>%
  inner_join(read_dta("Schlueter.dta") %>%
               mutate(mipex_j = mipex_j - mean(mipex_j, na.rm = TRUE)),
             by = "cntry") %>%
  inner_join(read_dta("ESSMD-2014-cntry/ESSMD-2014-cntry_F1.dta") %>%
               select(c_gini_2012, c_gdppc_2012, c_unall_2011, cntry),
             by = "cntry") %>%
  mutate(
    symb_threat_ij = z_std(symb_threat_ij),
    contact_ij = z_std(contact_ij),
    mipex_j = z_std(mipex_j),
    c_unall_2011 = z_std(c_unall_2011)
  )



model1 <- lmer(
  #main
  symb_threat_ij ~ contact_ij + mipex_j + contact_ij*mipex_j + 
  #controls
  agea_ij + gndr_ij + eduyrs_ij + immiback_ij + subj_income_ij + relig_ij + conserve_ij + div_nbh_ij +
  # country-level cntrls
  c_gini_2012 + c_unall_2011 + 
  # random slope contact on treat
  + (1 + contact_ij | cntry),
  temp)


model2 <- lmer(
  #main
  real_threat_ij ~ contact_ij + mipex_j + contact_ij*mipex_j + 
    #controls
    agea_ij + gndr_ij + eduyrs_ij + immiback_ij + subj_income_ij + relig_ij + conserve_ij + div_nbh_ij +
    # country-level cntrls
    c_gini_2012 + c_unall_2011 + 
    # random slope contact on treat
    + (1 + contact_ij | cntry),
  temp)

screenreg(list(model1,model2), digits = 3, include.ci = FALSE)
summary(model1)

# ICC 
0.35 / (0.35 + 2.94)
# ~ 10,6 %



blups <- as.data.frame(ranef(model1) %>% as_tibble()) %>% 
  spread(term, condval) %>%
  group_by(grp) %>%
  summarise(intcpt = `(Intercept)`[which(!is.na(`(Intercept)`))[1]],
         slope = contact_ij[which(!is.na(contact_ij))[1]])

p <- ggplot(blups) + 
  geom_abline(aes(slope = slope, intercept = intcpt, color=grp)) + 
  geom_text_repel(aes(label=grp, x = 0, y = intcpt), size=3) +
  labs(x = "Contact",y = "Symbolic threat") +
  xlim(0,1) +
  ylim(-0.55, 0.55) +
  theme_minimal() %+% theme(legend.position="none")
p



# exercise 7:



model1 <- lmer(
  #main
  symb_threat_ij ~ contact_ij + mipex_j + contact_ij*mipex_j + 
    #controls
    agea_ij + gndr_ij + eduyrs_ij + immiback_ij + subj_income_ij + relig_ij + conserve_ij + div_nbh_ij +
    # country-level cntrls
    c_gini_2012 + c_unall_2011 + 
    # random intercept contact on treat
    + (1 | cntry),
  temp)

model2 <- lmer(
  #main
  symb_threat_ij ~ contact_ij + mipex_j + contact_ij*mipex_j + 
    #controls
    agea_ij + gndr_ij + eduyrs_ij + immiback_ij + subj_income_ij + relig_ij + conserve_ij + div_nbh_ij +
    # country-level cntrls
    c_gini_2012 + c_unall_2011 + 
    # random slope contact on treat
    + (1 + contact_ij | cntry),
  temp)


screenreg(list(model1,model2), include.ci=FALSE, digits=3)


blups <- as.data.frame(ranef(model2) %>% as_tibble()) %>% 
  spread(term, condval) %>%
  group_by(grp) %>%
  summarise(intcpt = `(Intercept)`[which(!is.na(`(Intercept)`))[1]],
            slope = contact_ij[which(!is.na(contact_ij))[1]])

p1 <- ggplot(blups) + 
  geom_abline(aes(slope = slope, intercept = intcpt, color=slope)) + 
  geom_text_repel(aes(label=grp, x = 0, y = intcpt), size=3) +
  labs(x = "Contact",y = "Symbolic threat") +
  xlim(-0.55, 0.55) +
  ylim(-0.55, 0.55) +
  scale_color_gradient2(midpoint=0, low="blue", mid="lightgrey",
                        high="red") +
  theme_minimal() #%+% theme(legend.position="none")
p1






# Estimate -j models
one_out_models <- influence(model = model2, group = "cntry")

# Calculate the DFBETAs and turn into tibble
DFBETAs <- dfbetas(one_out_models) %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>%
  select(rowname, contact_ij, mipex_j, contact_ij.mipex_j)
  

# Reshape to long
DFBETAs <- pivot_longer(
  DFBETAs, 
  cols = c("contact_ij", "mipex_j","contact_ij.mipex_j"), 
  names_to = "Variable")

# Plot the results
p2 <- ggplot(data = DFBETAs, 
       aes(y = value, x = Variable, label = rowname)) +
  geom_violin(color = "gray") +
  geom_text(position=position_jitter(width=0.2,height=0)) +
  geom_hline(yintercept = 2/sqrt(20), color = "#901A1E") +
  geom_hline(yintercept = -2/sqrt(20), color = "#901A1E") +
  theme_minimal() +
  labs(y = "DFBETA")
p2


ggdraw() +
  draw_plot(p1, 0, .5, 1, .5) + 
  draw_plot(p2, 0, 0, 1, .5) +
  draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 15)




