################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
#devtools::install_github("melff/iimm")
pacman::p_load("haven","dplyr","estimatr","texreg",
               "essurvey","ggplot2","stats","factoextra","rmarkdown",
               "lme4","readxl","countrycode","RColorBrewer","sjPlot","ggridges",
               "tidyverse","ggrepel","influence.ME", "cowplot","broom.mixed","iimm")
source("utils.R")

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6), brewer.pal(name="Accent", n = 8))



################################################################################


# Read country-level data

# country <- read_stata("ESSMD-2014-cntry/ESSMD-2014-cntry_F1.dta") %>%
#   recode_missings()

# press_df <- read_excel("All_data_FIW_2013-2022.xlsx", sheet="FIW13-22", skip=1) %>%
#   mutate(cntry = countrycode(`Country/Territory`, "country.name", "iso2c")) %>%
#   filter(Edition == 2014)



media_claims <- as.data.frame(read_sav("data/ESS7MCe01.spss/ESS7MCe01.sav")) %>%
  mutate(
    issue_immigration = ifelse(Issuecode > 11 & Issuecode < 15, 1, 0),
    direction_immigration = ifelse(Issuecode > 11 & Issuecode < 15, Direction, NA),
    issue_generalimmigration = ifelse(Issuecode == 12, 1, 0),
    direction_generalimmigration = ifelse(Issuecode == 12, Direction, NA),
    issue_economicimmigration = ifelse(Issuecode == 13, 1, 0),
    direction_economicimmigration = ifelse(Issuecode == 13, Direction, NA),
    issue_culturaldiversity = ifelse(Issuecode == 14, 1, 0),
    direction_culturaldiversity = ifelse(Issuecode == 14, Direction, NA),
  ) %>%
  group_by(Country) %>%
  summarise(
    # Total
    total_claims = n(),
    total_mean_direction = mean(Direction, na.rm = TRUE),
    
    # Immigration (sum of 12,13,14)
    n_immigration_claims = sum(issue_immigration),
    share_immigration_claims = sum(issue_immigration) / n(),
    immigration_mean_direction = mean(direction_immigration, na.rm = TRUE),
    
    # General immigration (12)
    n_generalimmigration_claims = sum(issue_generalimmigration),
    share_generalimmigration_claims = sum(issue_generalimmigration) / n(),
    generalimmigration_mean_direction = mean(direction_generalimmigration, na.rm = TRUE),
    
    # Immigrations' impact on economy (13)
    n_economiclimmigration_claims = sum(issue_economicimmigration),
    share_economicimmigration_claims = sum(issue_economicimmigration) / n(),
    economicimmigration_mean_direction = mean(direction_economicimmigration, na.rm = TRUE),
    
    # Cultural diversity (14)
    n_culturaldiversity_claims = sum(issue_culturaldiversity),
    share_culturaldiversity_claims = sum(issue_culturaldiversity) / n(),
    culturaldiversity_mean_direction = mean(direction_culturaldiversity, na.rm = TRUE),
    ) %>%
  rename(cntry = Country) %>% 
  left_join(read_excel("data/Budget-in-terms-of-GDP.xlsx") %>% rename(cntry = Country), on="cntry") %>%
  rename(public_spend_inh = Inhabitant) %>%
  # z-standardize on level 2
  mutate(
    z_share_generalimmigration_claims = std(share_generalimmigration_claims),
    z_generalimmigration_mean_direction = std(generalimmigration_mean_direction),
    z_public_spend_inh = std(public_spend_inh)
  )

  





# Read ESS data

source("credentials.R")
ess <- import_rounds(rounds = 7, ess_email = my_email) %>%
  recode_missings() %>%
  as.data.frame() %>%
  filter(!is.na(imueclt)) %>%
  filter(!is.na(imwbcnt)) %>%
  filter(!is.na(tvpol)) %>%
  filter(!is.na(tvtot)) %>%
  mutate(cultural_treat = unlist(lapply(imueclt, function(x){return(10 - x)}))) %>%
  mutate(general_treat = unlist(lapply(imwbcnt, function(x){return(10 - x)}))) %>%
  mutate(xeno = cultural_treat + general_treat) %>%
  mutate(tvdif = tvtot - tvpol) %>%
  mutate(unemployed = case_when(uempla == 1 ~ 1,
                                uempli == 1 ~ 1,
                                TRUE ~ 0)) %>%
  select(cntry, tvtot, tvpol, tvdif, cultural_treat, general_treat,pspwght, eisced, agea, xeno, unemployed) %>%
  # z-standardize
  mutate(
    z_xeno = std(xeno),
    z_tvtot = std(tvtot),
    z_tvpol = std(tvpol),
    z_tvdif = std(tvdif),
    z_eisced = std(eisced),
    z_agea = std(agea)
  ) %>%
  left_join(media_claims, on ="cntry") %>%
  filter(!is.na(z_share_generalimmigration_claims)) %>%
  filter(!is.na(z_generalimmigration_mean_direction)) %>%
  filter(!is.na(z_public_spend_inh)) %>%
  filter(!is.na(agea)) %>%
  filter(!is.na(eisced)) %>%
  rename(Country=cntry)




################################################################################
#
#   Plots
#
################################################################################


# Outcome: General xenophobic treat
temp <- ess %>%
  group_by(xeno) %>%
  summarise(n = n())

p <- ggplot(temp, aes(x = xeno, y = n)) + 
  geom_bar(stat = "identity", fill = mycolors[1]) +
  scale_y_continuous(expand=c(0,0), limits = c(0,6000)) +
  labs(y = "Frequency", x = "Xenophobic treat") +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10)) + 
  theme_light() 
p
ggsave("outputs/outcome.png", dpi= 600, width = 7, height = 5)


# Outcome: 
temp <- ess %>%
  group_by(Country) %>%
  summarise(m = mean(xeno),
            s = sd(xeno)) %>%
  arrange(m)

p <- ggplot(ess, aes(x = factor(Country, levels=temp$Country), y = xeno)) +
  geom_jitter(color="grey", width = 0.2, alpha=0.5) +
  geom_point(data=temp, aes(x=Country, y=m)) +
  geom_linerange(data=temp, aes(x = Country, y=m, ymin=m-s, ymax=m+s)) +
  labs(y = "Xenophobic treat", x = "Country") +
  theme_light()
p
ggsave("outputs/outcome_variation.png", dpi= 600, width = 7, height = 5)




# TVPOL

p <- ggplot(data = ess, aes(y = xeno, x = tvpol, weight = pspwght, color=Country, fill=Country, alpha=pspwght)) +
  geom_jitter(color="grey") +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) + 
  theme_light() + 
  guides(alpha = "none") +
  labs(y = "Xenophobia", x = "News consumption") +
  theme(legend.title = element_blank(),
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = mycolors)
p
ggsave("outputs/scatterplot.png", dpi= 600, width = 7, height = 5)


# TVDIF
p <- ggplot(data = ess, aes(y = xeno, x = tvdif, weight = pspwght, color=Country, fill=cntry, alpha=pspwght)) +
  geom_jitter(color="grey") +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) + 
  theme_light() + 
  guides(alpha = "none") +
  labs(y = "Xeno", x = "TV watching, news/politics/current affairs") +
  theme(legend.title = element_blank(),
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = mycolors)
p



# Media salience

temp <- ess %>%
  group_by(Country) %>%
  summarise(x = first(share_generalimmigration_claims),
            y = mean(xeno, na.rm=F))


p <- ggplot(temp, aes(x = x, y = y, label = Country)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_text(vjust=-0.4, check_overlap = TRUE) +
  labs(y = "Xenophobia", x = "Media salience of immigration (% of claims)") +
  theme_light()
p
ggsave("outputs/media_salience.png", dpi= 600, width = 7, height = 5)


# Media sentiment

temp <- ess %>%
  group_by(Country) %>%
  summarise(x = first(generalimmigration_mean_direction),
            y = mean(xeno, na.rm=F))


p <- ggplot(temp, aes(x = x, y = y, label = Country)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_text(vjust=-0.4, check_overlap = TRUE) +
  labs(y = "Xenophobia", x = "Media sentiment on immigration") +
  theme_light()
p
ggsave("outputs/media_sentiment.png", dpi= 600, width = 7, height = 5)

# Public spending

temp <- ess %>%
  group_by(Country) %>%
  summarise(x = first(public_spend_inh),
            y = mean(xeno, na.rm=F)) 


p <- ggplot(temp, aes(x = x, y = y, label = Country)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_text(vjust=-0.4, check_overlap = TRUE) +
  labs(y = "Xenophobia", x = "Spending on public broadcasting") +
  theme_light()
p
ggsave("outputs/media_spending.png", dpi= 600, width = 7, height = 5)




################################################################################
#
#   Multilevel modelling
#
################################################################################

# Model 1


m11 <- lmer(
    #outcome
    z_xeno ~ 
    # main level 1 predictor
    z_tvpol + 
    # controls
    z_agea + 
    z_eisced + 
    #unemployed +
    # random intercept + random slope
    (1 | Country),
  data = ess, 
  weights = pspwght
)
# get correct p-values using Satterthaite method
m11_t <- lmer_t(m11, method = "Satterthwaite")
m11_out <- texreg::extract(m11)
m11_out@pvalues <- summary(m11_t)$coef[,7]

m12 <- lmer(
  #outcome
  z_xeno ~ 
    # main level 1 predictor
    z_tvpol + 
    # controls
    z_agea + 
    z_eisced + 
    #unemployed +
    # random intercept + random slope
    (1 + z_tvpol | Country),
  data = ess, 
  weights = pspwght
)
# get correct p-values using Satterthaite method
m12_t <- lmer_t(m12, method = "Satterthwaite")
m12_out <- texreg::extract(m12)
m12_out@pvalues <- summary(m12_t)$coef[,7]



# write model outputs to beautiful table! 
htmlreg(
  list(m11_out,m12_out), 
  file="outputs/reg_output1.html", 
  include.ci = FALSE, 
  custom.coef.names = c(
    "Intercept",
    "News consumption",
    "Age",
    "Education"
  ),
  custom.model.names = c(
    "Model 1a (Random Intercept, Fixed Slope)",
    "Model 1b (Random Intercept, Random Slope)"
  ),
  groups = c(
    "Intercept" = list(1:1),
    "Individual level predictor" = list(2:2),
    "Invididual level controls" = list(3:4)
  ),
  digits = 3,
  caption = "",
  custom.gof.rows = list("ICC" = c(
    paste0(round(icc(m11)*100, digits=2),"%"), 
    paste0(round(icc(m12)*100, digits=2),"%")
  )
  )
)





# Model 2

m2 <- lmer(
    #outcome
    z_xeno ~ 
    # level 1 predictor
    z_tvpol + 
    # controls
    z_agea + 
    z_eisced + 
    #z_unemployed +
    # level 2 predictors
    z_share_generalimmigration_claims + 
    z_generalimmigration_mean_direction + 
    z_public_spend_inh +
    # random intercept + random slope
    (1 + z_tvpol | Country),
  data = ess, 
  weights = pspwght
)
# get correct p-values using Satterthaite method
m2_t <- lmer_t(m2, method = "Satterthwaite")
m2_out <- texreg::extract(m2)
m2_out@pvalues <- summary(m2_t)$coef[,7]


# write model outputs to beautiful table! 
htmlreg(
  list(m2_out), 
  file="outputs/reg_output2.html", 
  include.ci = FALSE, 
  custom.coef.names = c(
    "Intercept",
    "News consumption",
    "Age",
    "Education",
    "Media salience",
    "Media sentiment",
    "Public broadcast spending"
  ),
  custom.model.names = c(
    "Model 2"
  ),
  groups = c(
    "Intercept" = list(1:1),
    "Individual level predictor" = list(2:2),
    "Invididual level controls" = list(3:4),
    "Country level predictors" = list(5:7)
  ),
  digits = 3,
  caption = "",
  custom.gof.rows = list("ICC" = c(
    paste0(round(icc(m2)*100, digits=2),"%")
  )
  )
)



# Model 3

m3 <- lmer(
  #outcome
  z_xeno ~
  # level 1 predictor
  z_tvpol + 
  # controls
  z_agea + 
  z_eisced + 
  #z_unemployed +
  # level 2 predictors
  z_share_generalimmigration_claims + 
  z_generalimmigration_mean_direction + 
  z_public_spend_inh + 
  # cross-level-interactions
  z_tvpol*z_share_generalimmigration_claims +
  #z_tvpol*z_generalimmigration_mean_direction + 
  #z_tvpol*z_public_spend_inh + 
  # random intercept + random slope
  (1 + z_tvpol | Country),
  data = ess, 
  weights = pspwght
)
# get correct p-values using Satterthaite method
m3_t <- lmer_t(m3, method = "Satterthwaite")
m3_out <- texreg::extract(m3)
m3_out@pvalues <- summary(m3_t)$coef[,7]

# Model 4

m4 <- lmer(
  #outcome
  z_xeno ~
    # level 1 predictor
    z_tvpol + 
    # controls
    z_agea + 
    z_eisced + 
    #z_unemployed +
    # level 2 predictors
    z_share_generalimmigration_claims + 
    z_generalimmigration_mean_direction + 
    z_public_spend_inh + 
    # cross-level-interactions
    #z_tvpol*z_share_generalimmigration_claims +
    z_tvpol*z_generalimmigration_mean_direction + 
    #z_tvpol*z_public_spend_inh + 
    # random intercept + random slope
    (1 + z_tvpol | Country),
  data = ess, 
  weights = pspwght
)
# get correct p-values using Satterthaite method
m4_t <- lmer_t(m4, method = "Satterthwaite")
m4_out <- texreg::extract(m4)
m4_out@pvalues <- summary(m4_t)$coef[,7]

# Model 5

m5 <- lmer(
  #outcome
  z_xeno ~
    # level 1 predictor
    z_tvpol + 
    # controls
    z_agea + 
    z_eisced + 
    #z_unemployed +
    # level 2 predictors
    z_share_generalimmigration_claims + 
    z_generalimmigration_mean_direction + 
    z_public_spend_inh + 
    # cross-level-interactions
    #z_tvpol*z_share_generalimmigration_claims +
    #z_tvpol*z_generalimmigration_mean_direction + 
    z_tvpol*z_public_spend_inh + 
    # random intercept + random slope
    (1 + z_tvpol | Country),
  data = ess, 
  weights = pspwght
)
# get correct p-values using Satterthaite method
m5_t <- lmer_t(m5, method = "Satterthwaite")
m5_out <- texreg::extract(m5)
m5_out@pvalues <- summary(m5_t)$coef[,7]

# write model outputs to beautiful table! 
htmlreg(
  list(m3_out,m4_out,m5_out), 
  file="outputs/reg_output3.html", 
  include.ci = FALSE, 
  custom.coef.names = c(
    "Intercept",
    "News consumption",
    "Age",
    "Education",
    "Media salience",
    "Media sentiment",
    "Public broadcast spending",
    "TV watching:Media salience",
    "TV watching:Media sentiment",
    "TV watching:Public broadcast spending"
  ),
  custom.model.names = c(
    "Model 3",
    "Model 4",
    "Model 5"
  ),
  groups = c(
    "Intercept" = list(1:1),
    "Individual level predictor" = list(2:2),
    "Invididual level controls" = list(3:4),
    "Country level predictors" = list(5:7),
    "Cross-level interactions" = list(8:10)
  ),
  digits = 3,
  caption = "",
  custom.gof.rows = list("ICC" = c(
    paste0(round(icc(m3)*100, digits=2),"%"),
    paste0(round(icc(m4)*100, digits=2),"%"),
    paste0(round(icc(m5)*100, digits=2),"%")
    )
  )
)








### Residual plot ###

temp <- ess %>%
  mutate(preds = predict(m3)) %>%
  mutate(res = z_xeno - preds)

p <- ggplot(temp, aes(x=res, y = Country)) +
  geom_density_ridges() +
  scale_color_manual(values = mycolors) +
  theme_light()
p
ggsave("outputs/m3_residuals.png", dpi= 600, width = 7, height = 5)



################################################################################
#
#   Post-estimation stuff
#
################################################################################

### BLUPS

blups <- as.data.frame(ranef(m1) %>% as_tibble()) %>% 
  spread(term, condval) %>%
  group_by(grp) %>%
  summarise(intcpt = `(Intercept)`[which(!is.na(`(Intercept)`))[1]],
            slope = z_tvpol[which(!is.na(z_tvpol))[1]])

p1 <- ggplot(blups) + 
  geom_abline(aes(slope = slope, intercept = intcpt, color=slope)) + 
  geom_text_repel(aes(label=grp, x = 0, y = intcpt), size=3) +
  labs(x = "TV watching",y = "Xenophobia") +
  #xlim(-0.55, 0.55) +
  #ylim(-0.55, 0.55) +
  scale_color_gradient2(midpoint=0, low="blue", mid="lightgrey",
                        high="red") +
  theme_minimal() #%+% theme(legend.position="none")
p1


### DFBETAS

# Estimate -j models
one_out_models <- influence(model = m3, group = "Country")

# Calculate the DFBETAs and turn into tibble
DFBETAs <- dfbetas(one_out_models) %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>%
  select(rowname, z_tvtot,z_share_generalimmigration_claims, z_generalimmigration_mean_direction, z_trust_in_tv)


# Reshape to long
DFBETAs <- pivot_longer(
  DFBETAs, 
  cols = c("z_tvtot", "z_share_generalimmigration_claims","z_generalimmigration_mean_direction","z_trust_in_tv"), 
  names_to = "variable") %>%
  mutate(variable = case_when(variable == "z_tvtot" ~ "TV watching",
                              variable == "z_share_generalimmigration_claims" ~ "Media \n Salience",
                              variable == "z_generalimmigration_mean_direction" ~ "Media \nsentiment",
                              variable =="z_trust_in_tv" ~ "Media trust"))

# Plot the results
p2 <- ggplot(data = DFBETAs, 
             aes(y = value, x = variable, label = rowname)) +
  geom_violin(fill = "gray", color="gray", alpha=0.5) +
  geom_point(position=position_jitter(width=0.1,height=0), size=1, color="black", alpha=0.5) +
  geom_text_repel(size=3, arrow=arrow(angle = 30, length = unit(0.03, "inches"),
                                                                                  ends = "last", type = "open")) +
  geom_hline(yintercept = 2/sqrt(16), color = "#901A1E", linetype='dashed', alpha=0.5) +
  geom_hline(yintercept = -2/sqrt(16), color = "#901A1E", linetype='dashed', alpha=0.5) +
  theme_minimal() +
  labs(y = "DFBETA")
p2
ggsave("outputs/dfbetas.png", dpi= 600, width = 8, height = 5)




### combined plot of BLUPS and DFBETAS

ggdraw() +
  draw_plot(p1, 0, .5, 1, .5) + 
  draw_plot(p2, 0, 0, 1, .5) +
  draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 15)


