https://docs.google.com/spreadsheets/d/e/2PACX-1vTCOl-dFppqUz0s_p5lsszbF4KWUHJQIc5obS-Z9NCBlYI-dSRvX91EzjkFJrtpNT8zZ8huz5wxUdsZ/pub?gid=479539032&single=true&output=csv


#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of woody cover
# Paper:
# browseURL("https://docs.google.com/spreadsheets/d/1zn00-OphZMeOzZRc1f1nk5uW1dSOrLFVyifgMQ0jgo4/edit?gid=691676984#gid=691676984")

# restore libraries
install.packages("lavaan")

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)



# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCOl-dFppqUz0s_p5lsszbF4KWUHJQIc5obS-Z9NCBlYI-dSRvX91EzjkFJrtpNT8zZ8huz5wxUdsZ/pub?gid=479539032&single=true&output=csv")

# read the data from the google docs link:

SEMdata <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCOl-dFppqUz0s_p5lsszbF4KWUHJQIc5obS-Z9NCBlYI-dSRvX91EzjkFJrtpNT8zZ8huz5wxUdsZ/pub?gid=479539032&single=true&output=csv")
names(SEMdata)
# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% select(woody,rainfall,burnfreq, elevation, cec,
                                            dist2river, nitrogen),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(woody,rainfall,burnfreq, elevation, cec,
                                               dist2river, nitrogen),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
lmSEM_std <- lm(woody~rainfall + burnfreq + elevation + cec + dist2river, nitrogen, data=SEMdatastd)
summary(lmSEM_std)
plot(lmSEM_std)
# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model <- "woody~rainfall + burnfreq + elevation + cec + dist2river + nitrogen
                cec~burnfreq + rainfall + elevation
                nitrogen~rainfall + burnfreq
                burnfreq~rainfall + elevation
                rainfall~elevation
                "
woody_model
woody_fit<- lavaan::sem(woody_model, data=SEMdatastd)
# show the model results
summary(woody_fit, standardized=T, fit.measures=T, rsquare=T)
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR


# visualise the model


# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content
psych::pairs.panels(Anderson2007 %>% select(BIOMASS,RES_LHU,FIRE_FRQ,NMS,
                                            LF_P),
                    stars = T, ellipses = F)
psych::pairs.panels(Anderson2007std %>% select(BIOMASS,RES_LHU,FIRE_FRQ,NMS,
                                               LF_P),
                    stars = T, ellipses = F)


Leaf_P_model <- "LF_P~BIOMASS + RES_LHU + FIRE_FRQ + NMS
                BIOMASS~FIRE_FRQ + RES_LHU
                NMS~FIRE_FRQ + RES_LHU"
Leaf_P_model
Leaf_P_fit<- lavaan::sem(Leaf_P_model, data=Anderson2007std)
# show the model results
summary(Leaf_P_fit, standardized=T, fit.measures=T, rsquare=T)
