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


# Piecewise SEM
install.packages("piecewiseSEM")
library(piecewiseSEM)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCOl-dFppqUz0s_p5lsszbF4KWUHJQIc5obS-Z9NCBlYI-dSRvX91EzjkFJrtpNT8zZ8huz5wxUdsZ/pub?gid=479539032&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))


psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models
# I started from this initially hypothesized causal scheme, my model 1)
browseURL("https://docs.google.com/presentation/d/1PB8rhbswyPew-FYULsw1pIl8Jyb1FFElKPf34DZrEY8/edit?usp=sharing")

# Model 1: woody predicted by burnfreq and rainfall
model_woody <- lm(woody ~  cec +burnfreq + nitrogen, 
                  data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=burnfreq,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p1
p2<-ggplot(data=pointdata,aes(x=cec,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2

# Model_burnfreq: burning frequency predicted by Core Protected Areas and Rainfall
model_burnfreq_init <- glm(burnfreq ~ CorProtAr + rainfall +elevation, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
model_burnfreq <- MASS::glm.nb(burnfreq ~ CorProtAr + rainfall +elevation, 
                               data = pointdata)
summary(model_burnfreq)

p3<-ggplot(data=pointdata,aes(y=burnfreq,x=CorProtAr))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p3
p4<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p4

# model_cec: predicted by rainfall

model_cec <- lm(cec ~ rainfall + CorProtAr +elevation, 
                data = pointdata)
summary(model_cec)

p5<-ggplot(data=pointdata,aes(y=cec,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5

p6<-ggplot(data=pointdata,aes(y=cec,x=CorProtAr))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p6


# model_CorProtAra:  predicted by elevation
model_CorProtAr <-glm(CorProtAr~elevation,
                      family=binomial,
                      data=pointdata)
summary(model_CorProtAr)
p7<-ggplot(data=pointdata,aes(y=CorProtAr,x=elevation))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p7

# model_rainfall: rainfall predicted by elevation
model_rainfall <- lm(rainfall ~ elevation, 
                     data = pointdata)
summary(model_rainfall)

p8<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,  #y~poly(x,2)
              se=T)
p8

model_N <- lm(nitrogen ~ rainfall + CorProtAr +burnfreq + elevation, 
                data = pointdata)
summary(model_N)

p9<-ggplot(data=pointdata,aes(y=nitrogen,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p9
# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+p8+p9+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_cec,
                                 model_CorProtAr,
                                 model_rainfall,
                                 model_N)

# Summarize the SEM results
summary(psem_model)

install.packages("lavaanPlot")
library(lavaanPlot)

library(piecewiseSEM)
plot(psem_model)

lavaanPlot(model = psem_model, 
           node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,  # Display coefficients on edges
           stand = TRUE)  # Standardized coefficients
# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate d-Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony


