################## CONFIG ######################################################

rm(list = ls())
options(scipen=999)
pacman::p_load("haven","dplyr","estimatr","texreg",
               "essurvey","ggplot2","stats","factoextra","rmarkdown",
               "lme4","readxl","countrycode","RColorBrewer","sjPlot", 
               "tidyverse","ggrepel","influence.ME", "cowplot","broom.mixed")
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
  rename(cntry = Country)





# Read ESS data

ess <- import_rounds(rounds = 7, ess_email = "madslangs@gmail.com") %>%
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
  mutate(
    smegbli = zap_labels(smegbli),
    smegbhw = zap_labels(smegbhw),
    smctmbe = zap_labels(smctmbe),
    smegbli = ifelse(smegbli == 2, 0, smegbli),
    smegbhw = ifelse(smegbhw == 2, 0, smegbhw),
    smctmbe = ifelse(smctmbe == 2, 0, smctmbe),
    classic_racism_index = smegbli + smegbhw + smctmbe
  ) %>%
  select(cntry, tvtot, tvpol, tvdif, cultural_treat, general_treat, classic_racism_index, pspwght, eisced, agea, xeno) %>%
  #inner_join(press_df, on ="cntry") %>%
  left_join(media_claims, on ="cntry") %>%
  filter(!is.na(share_generalimmigration_claims)) %>%
  filter(!is.na( + generalimmigration_mean_direction))





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




# TVPOL

p <- ggplot(data = ess, aes(y = xeno, x = tvpol, weight = pspwght, color=cntry, fill=cntry, alpha=pspwght)) +
  geom_jitter(color="grey") +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) + 
  theme_light() + 
  guides(alpha = "none") +
  labs(y = "Xenophobic threat", x = "TV watching, news/politics/current affairs") +
  theme(legend.title = element_blank(),
        text=element_text(family="serif"),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = mycolors)
p
ggsave("outputs/scatterplot.png", dpi= 600, width = 7, height = 5)


# TVDIF
p <- ggplot(data = ess, aes(y = xeno, x = tvdif, weight = pspwght, color=cntry, fill=cntry, alpha=pspwght)) +
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
  group_by(cntry) %>%
  summarise(x = first(share_generalimmigration_claims),
            y = mean(xeno, na.rm=F))


p <- ggplot(temp, aes(x = x, y = y, label = cntry)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_text(vjust=-0.4, check_overlap = TRUE) +
  labs(y = "Xenophobic threat", x = "Media salience of immigration (% of claims)") +
  theme_light()
p
ggsave("outputs/media_salience.png", dpi= 600, width = 7, height = 5)


# Media sentiment

temp <- ess %>%
  group_by(cntry) %>%
  summarise(x = first(generalimmigration_mean_direction),
            y = mean(xeno, na.rm=F))


p <- ggplot(temp, aes(x = x, y = y, label = cntry)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  geom_text(vjust=-0.4, check_overlap = TRUE) +
  labs(y = "Xenophobic threat", x = "Media sentiment on immigration") +
  theme_light()
p
ggsave("outputs/media_sentiment.png", dpi= 600, width = 7, height = 5)



################################################################################
#
#   Multilevel modelling
#
################################################################################

# Model 1

m1 <- lmer(
    #outcome
    xeno ~ 
    # main level 1 predictor
    tvtot + 
    # random intercept
    (1 | cntry), 
  data = ess, 
  weights = pspwght
)


# Model 2

m2 <- lmer(
    #outcome
    xeno ~ 
    # level 1 predictor
    tvpol + 
    # level 2 predictors
    share_generalimmigration_claims + 
    generalimmigration_mean_direction + 
    # random intercept
    (1 | cntry), 
  data = ess, 
  weights = pspwght
)

# Model 3

m3 <- lmer(
  #outcome
  xeno ~
  # level 1 predictor
  tvpol + 
  # level 2 predictors
  share_generalimmigration_claims + 
  generalimmigration_mean_direction + 
  # random intercept + random slope
  (1 + tvpol | cntry),
  data = ess, 
  weights = pspwght
)


# write model outputs to beautiful table! 
htmlreg(
  list(m1,m2,m3), 
  file="outputs/reg_output.html", 
  include.ci = FALSE, 
  digits = 3,
  custom.gof.rows = list("ICC" = c(
    paste0(round(icc(m1)*100, digits=2),"%"), 
    paste0(round(icc(m2)*100, digits=2),"%"), 
    paste0(round(icc(m3)*100, digits=2),"%")
    )
  )
)




################################################################################
#
#   Post-estimation stuff
#
################################################################################

### BLUPS

blups <- as.data.frame(ranef(m3) %>% as_tibble()) %>% 
  spread(term, condval) %>%
  group_by(grp) %>%
  summarise(intcpt = `(Intercept)`[which(!is.na(`(Intercept)`))[1]],
            slope = tvpol[which(!is.na(tvpol))[1]])

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
one_out_models <- influence(model = m3, group = "cntry")

# Calculate the DFBETAs and turn into tibble
DFBETAs <- dfbetas(one_out_models) %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>%
  select(rowname, tvpol,share_generalimmigration_claims, generalimmigration_mean_direction)


# Reshape to long
DFBETAs <- pivot_longer(
  DFBETAs, 
  cols = c("tvpol", "share_generalimmigration_claims","generalimmigration_mean_direction"), 
  names_to = "Variable")

# Plot the results
p2 <- ggplot(data = DFBETAs, 
             aes(y = value, x = Variable, label = rowname)) +
  geom_violin(fill = "gray", color="gray", alpha=0.5) +
  geom_point(position=position_jitter(width=0.1,height=0), size=1, color="black", alpha=0.5) +
  geom_text_repel(size=3, arrow=arrow(angle = 30, length = unit(0.03, "inches"),
                                                                                  ends = "last", type = "open")) +
  geom_hline(yintercept = 2/sqrt(16), color = "#901A1E", linetype='dashed', alpha=0.5) +
  geom_hline(yintercept = -2/sqrt(16), color = "#901A1E", linetype='dashed', alpha=0.5) +
  theme_minimal() +
  labs(y = "DFBETA")
p2



### combined plot of BLUPS and DFBETAS

ggdraw() +
  draw_plot(p1, 0, .5, 1, .5) + 
  draw_plot(p2, 0, 0, 1, .5) +
  draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 15)


