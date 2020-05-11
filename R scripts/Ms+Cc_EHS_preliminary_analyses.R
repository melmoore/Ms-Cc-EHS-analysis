#Ms+Cc Early heat shock experiment--PRELIMINARY ANALYSES

#load libraries
library(scales)
library(Rmisc)
library(readr)
library(nlme)
library(lme4)
library(lmerTest)
library(ggplot2)
library(car)
library(tidyr)
library(mgcv)
library(dplyr)
library(viridis)
library(cowplot)


#Load data

ehs <- read_csv("data/Ms+Cc_EHS_incomplete_clean.csv", 
                col_types = cols(hs.num = col_factor(levels = c("0", "1", "2", "3", "4")), 
                                 hs.temp = col_factor(levels = c("0", "40", "42"))))

View(ehs)


ehs_lng <- read_csv("data/Ms+Cc_EHS_incomplete_clean_long.csv", 
                     col_types = cols(hs.num = col_factor(levels = c("0", "1", "2", "3", "4")), 
                                      hs.temp = col_factor(levels = c("0", "40", "42")), 
                                      instar = col_factor(levels = c("3", "4", "5", "end.all"))))
View(ehs_lng)



#--------------------------

#preliminary analysis to determine if the incidence of wanderers differed between treatments. 

#make a binary column indicating if the individual wandered or not (had emergence or was WOWE)
ehs$bin_wand <- ifelse(ehs$class=="wand", 1, 0)


#doesn't seem to like that my factor levels are not fully crossed or nested (I think....)
#my controls are coded as hs.temp=0 and hs.num=0, while all my experimental treatments are fully crossed
#not exactly sure how to deal with this (ask Joel or James), so removing control group for now to do this
#quick and dirty analysis
ehs_nc <- subset(ehs, hs.num!=0 & hs.temp!=0)
ehs_nc$hs.temp <- factor(ehs_nc$hs.temp, levels=c(40, 42))
ehs_nc$hs.num <- factor(ehs_nc$hs.num, levels = c(1, 2, 3, 4))


#binomial glm with bin_wand as response variable, hs.num and hs.temp as predictor variables
wand_mod <- glm(bin_wand ~ hs.num * hs.temp, 
                family = binomial,
                data = ehs_nc,
                na.action = na.omit)

summary(wand_mod)


wand_mod_hsn <- glm(bin_wand ~ hs.num, 
                    family = binomial,
                    data = ehs_nc,
                    na.action = na.omit)


wand_mod_hst <- glm(bin_wand ~ hs.temp, 
                    family = binomial,
                    data = ehs_nc,
                    na.action = na.omit)


wand_mod_int <- glm(bin_wand ~ hs.num:hs.temp, 
                    family = binomial,
                    data = ehs_nc,
                    na.action = na.omit)

anova(wand_mod, wand_mod_hsn, wand_mod_hst, wand_mod_int, test="Chisq")
AIC(wand_mod, wand_mod_hsn, wand_mod_hst, wand_mod_int)

#seems like incident of wanderers doesn't differ between treatments--unsure if it differs between 
#hs and not hs


#create column "hs", that indicates whether an individual was heatshocked or not, try analyzing
#incidence of wanderers with that
ehs$hs <- ifelse(ehs$hs.num==0, 0, 1)

wand_mod2 <- glm(bin_wand ~ hs, 
                 family = binomial,
                 data = ehs,
                 na.action = na.omit)

summary(wand_mod2) #seems to indicate that there are more wanderers in heat shocked treatments. 


#----------------------

#removing wanderers and running a glm on wowe vs emergence. not including controls, since I don't know
#how to get the model to accept my non-crossed design

ehs_nc <- subset(ehs_nc, class!="wand")

#create binary class column: 0==wowe, 1==em
ehs_nc$bin_class <- ifelse(ehs_nc$class=="mongo", 0, 1)

class_mod <- glm(bin_class ~ hs.num * hs.temp,
                 family = binomial,
                 data = ehs_nc,
                 na.action = na.omit)

summary(class_mod)


class_mod_hsn <- glm(bin_class ~ hs.num,
                     family = binomial,
                     data = ehs_nc,
                     na.action = na.omit)


class_mod_hst <- glm(bin_class ~ hs.temp,
                     family = binomial,
                     data = ehs_nc,
                     na.action = na.omit)


class_mod_int <- glm(bin_class ~ hs.num:hs.temp,
                     family = binomial,
                     data = ehs_nc,
                     na.action = na.omit)


anova(class_mod, class_mod_hsn, class_mod_hst, class_mod_int, test="Chisq")















