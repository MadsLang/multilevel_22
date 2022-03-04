################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
pacman::p_load("haven","dplyr","estimatr","texreg",
               "essurvey","ggplot2","stats","factoextra","rmarkdown",
               "lme4","readxl","countrycode")



################################################################################


# Read country-level data

# country <- read_stata("ESSMD-2014-cntry/ESSMD-2014-cntry_F1.dta") %>%
#   recode_missings()

# press_df <- read_excel("All_data_FIW_2013-2022.xlsx", sheet="FIW13-22", skip=1) %>%
#   mutate(cntry = countrycode(`Country/Territory`, "country.name", "iso2c")) %>%
#   filter(Edition == 2014) 


media_claims <- as.data.frame(read_sav("ESS7MCe01.spss/ESS7MCe01.sav") %>%
  mutate(issue_immigration = ifelse(Issuecode > 11 & Issuecode < 15, 1, 0)) %>%
  filter(issue_immigration == 1) %>%
  group_by(Country) %>%
  summarise(n = n(),
            mean_direction = mean(Direction, na.rm = FALSE)) %>%
  rename(cntry = Country))


# Read ESS data

ess <- import_rounds(rounds = 7, ess_email = "madslangs@gmail.com") %>%
  recode_missings() %>%
  as.data.frame() %>%
  filter(!is.na(imueclt)) %>%
  filter(!is.na(imwbcnt)) %>%
  filter(!is.na(tvtot)) %>%
  mutate(cultural_treat = unlist(lapply(imueclt, function(x){return(10 - x)}))) %>%
  mutate(general_treat = unlist(lapply(imwbcnt, function(x){return(10 - x)}))) %>%
  mutate(
    smegbli = zap_labels(smegbli),
    smegbhw = zap_labels(smegbhw),
    smctmbe = zap_labels(smctmbe),
    smegbli = ifelse(smegbli == 2, 0, smegbli),
    smegbhw = ifelse(smegbhw == 2, 0, smegbhw),
    smctmbe = ifelse(smctmbe == 2, 0, smctmbe),
    classic_racism_index = smegbli + smegbhw + smctmbe
  ) %>%
  select(cntry, tvtot, tvpol, cultural_treat, general_treat, classic_racism_index, pspwght, eisced, agea) %>%
  #inner_join(press_df, on ="cntry") %>%
  left_join(media_claims, on ="cntry")


# OLS of tvtot and cultural_treat

m <- lm(cultural_treat ~ tvtot + eisced + agea, ess, weights = pspwght)
screenreg(m)



# Plots

p <- ggplot(data = ess, aes(y = general_treat, x = tvtot, weight = pspwght, color=cntry, fill=cntry, alpha=pspwght)) +
  geom_jitter(color="black") +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) + 
  theme_light() + 
  guides(alpha = "none") +
  theme(legend.title = element_blank(),
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 
p



temp <- ess %>%
  group_by(cntry) %>%
  summarise(x = mean(tvtot, na.rm=F),
            y = mean(general_treat, na.rm=F))


p <- ggplot(temp, aes(x = x, y = y, label = cntry)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_text(vjust=-0.4, check_overlap = TRUE) +
  labs(y = "Xenophobic threat", x = "Hours of TV-watching every week") +
  theme_light()
p





# Multilevel models

m <- lmer(cultural_treat ~ tvtot + n + agea + eisced + (1 | cntry), data = ess, weights = pspwght)
summary(m)

0.47/(0.47+5.53)
