
################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
pacman::p_load("haven","dplyr","estimatr","texreg",
               "essurvey","ggplot2","stats","factoextra","rmarkdown",
               "lme4","readxl","countrycode","data.table")



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
  mutate(general_treat = unlist(lapply(imwbcnt, function(x){return(10 - x)}))) %>%
  mutate(lrscale_dummy = case_when(lrscale <= 4 ~ "Left",
                                   lrscale > 4 & lrscale <= 7 ~ "Middle",
                                   lrscale > 7 ~ "Right")) %>%
  filter(!is.na(contact)) %>%
  filter(!is.na(lrscale))





### Plots of main variables

p_data <- temp %>%
  group_by(contact) %>%
  summarise(n = n())

p <- ggplot(p_data, aes(x = contact, y = n)) + 
  geom_bar(stat = "identity", width = 0.5) +
  theme_light() +
  labs(y = "Frequency", x = "Having friends from minority race or ethnic group") +
  scale_x_continuous(breaks=c(0,1), expand=c(0.2,0.2)) +
  scale_y_continuous(expand=c(0,0), limits = c(0,900)) +
  theme(legend.position = "none",
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 
p
ggsave("plot3.png", dpi = 500)

p_data <- temp %>%
  group_by(cultural_treat) %>%
  summarise(n = n())

p <- ggplot(p_data, aes(x = cultural_treat, y = n)) + 
  geom_bar(stat = "identity", width = 0.5) +
  theme_light() +
  labs(y = "Frequency", x = "Cultural xenophobic threat") +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(expand=c(0,0), limits = c(0,300)) +
  theme(legend.position = "none",
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 
p
ggsave("plot4.png", dpi = 500)








### More


p <- ggplot(data = temp,aes(y = cultural_treat, x = contact, weight = pspwght, alpha=pspwght)) +
  geom_jitter(width = 0.1, height = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") + 
  labs(y = "Cultural xenophobic threat", x = "Having friends from minority race or ethnic group") +
  scale_x_continuous(breaks=c(0,1), expand=c(0.2,0.2)) +
  scale_y_continuous(breaks=seq(0,10)) +
  theme_light() + 
  guides(alpha = "none") +
  theme(legend.position = "none",
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 
p
ggsave("plot1.png", dpi = 500)




p <- ggplot(data = temp,aes(y = cultural_treat, x = contact, weight = pspwght, color=lrscale_dummy, fill=lrscale_dummy, alpha=pspwght)) +
  geom_jitter(width = 0.1, height = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") + 
  labs(y = "Cultural xenophobic threat", x = "Having friends from minority race or ethnic group") +
  scale_x_continuous(breaks=c(0,1), expand=c(0.2,0.2)) +
  scale_y_continuous(breaks=seq(0,10)) +
  theme_light() + 
  scale_color_manual(values=c("#FF0000","#808080","#0000FF")) +
  guides(alpha = "none") +
  theme(legend.title = element_blank(),
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) 
p
ggsave("plot2.png", dpi = 500)






# OLS 

model <- lm_robust(
  formula = cultural_treat ~ contact, 
  data=temp, 
  weight = pspwght
)
screenreg(model, include.ci = FALSE, digits = 3)



# OLS with controls for education and age

model <- lm_robust(
  formula = cultural_treat ~ contact + eisced + agea + lrscale + contact*lrscale, 
  data=temp, 
  weight = pspwght
)
screenreg(model, include.ci = FALSE, digits = 3)

htmlreg(model, file="reg_output.html", include.ci = FALSE, digits = 3)


