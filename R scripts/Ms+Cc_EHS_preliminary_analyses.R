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
library(ResourceSelection)


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


hoslem.test(mtcars$vs, fitted(model))

#Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(ehs_nc$bin_class, fitted(class_mod))


#--------------------

#plotting binary class data for each hs number and temperature
bin_class_plot <- ggplot(ehs_nc, aes(x=as.numeric(hs.num), y=bin_class, color=hs.temp))
bin_class_plot + stat_smooth(method="glm", method.args=list(family="binomial"), formula=y~x, 
                             size=2
) + geom_point(position=position_jitter(height=0.03, width=0.3)
) + scale_y_continuous(breaks=c(0, 1), labels = c("WOWE", "em"))



#seeing if I can plot with controls as well--doesn't really work well, as cons don't have hs.num values other 
#than 0

#remove wanderers
ehs_nw <- subset(ehs, class!="wand")

#create a binary class column
ehs_nw$bin_class <- ifelse(ehs_nw$class=="mongo", 0, 1)

#plot with controls
bincl_wcon_plot <- ggplot(ehs_nw, aes(x=as.numeric(hs.num), y=bin_class, color=hs.temp))
bincl_wcon_plot + stat_smooth(method="glm", method.args=list(family="binomial"), formula=y~x, 
                             size=2
) + geom_point(position=position_jitter(height=0.03, width=0.3)
) + scale_y_continuous(breaks=c(0, 1), labels = c("WOWE", "em"))


#---------------------------

#analyze wasp survival across treatments

#create a tot.died column
ehs_nw$tot.died <- ehs_nw$load - ehs_nw$num.ecl

#subset to only those that have been dissected
ehs_nw$num.unem[is.na(ehs_nw$num.unem)] <- 0

ehs_nw$keep <- ifelse(ehs_nw$class=="em" & ehs_nw$num.unem > 0, 1, 
                      ifelse(ehs_nw$class=="mongo", 1, 0))

ehs_diss <- subset(ehs_nw, keep==1)


#binomial glm of wasp survival--attempting to have control group included--doesn't work with controls
wsurvecl_wcon_mod <- glm(cbind(num.ecl, tot.died) ~ hs.num * hs.temp * load,
                         family = binomial,
                         data = ehs_diss,
                         na.action = na.omit)

summary(wsurvecl_wcon_mod)



#subset out controls
ehs_dnc <- subset(ehs_diss, hs.num!=0)

#looks like there were no hosts with emergence in 42.3, so there is a singularity in that term
#ask James if the estimates from the rest of the model are ok?
wsurvecl_ncon_mod <- glm(cbind(num.ecl, tot.died) ~ hs.num * hs.temp,
                         family = binomial,
                         data = ehs_dnc,
                         na.action = na.omit)

summary(wsurvecl_ncon_mod)


wsurvecl_ncon_mod_hsn <- glm(cbind(num.ecl, tot.died) ~ hs.num,
                             family = binomial,
                             data = ehs_dnc,
                             na.action = na.omit)

wsurvecl_ncon_mod_hst <- glm(cbind(num.ecl, tot.died) ~ hs.temp,
                             family = binomial,
                             data = ehs_dnc,
                             na.action = na.omit)


wsurvecl_ncon_mod_int <- glm(cbind(num.ecl, tot.died) ~ hs.num:hs.temp,
                             family = binomial,
                             data = ehs_dnc,
                             na.action = na.omit)

anova(wsurvecl_ncon_mod, wsurvecl_ncon_mod_hsn, wsurvecl_ncon_mod_hst, wsurvecl_ncon_mod_int, test="Chisq")



#rough analysis of wasp survival between controls with no heat shock and all heat shock treatments
wsurvecl_hs_mod <- glm(cbind(num.ecl, tot.died) ~ hs,
                       family = binomial,
                       data = ehs_diss,
                       na.action = na.omit)

summary(wsurvecl_hs_mod)


#-----------------------

#create binary wasp survival column
ehs_dnc$num.ecl[is.na(ehs_dnc$num.ecl)] <- 0
ehs_dnc$bin_surv <- ifelse(ehs_dnc$class=="em" & ehs_dnc$num.ecl > 0, 1, 0)
ehs_dnc$num.ecl[ehs_dnc$num.ecl==0] <- NA


#plotting binomial of wasp survival (or attempting to)
#points are proportion of surviving wasps, lines are binomial trends of wasp survival
#not sure if this is quite appropriate
ecl_surv_plot <- ggplot(ehs_dnc, aes(x=as.numeric(hs.num), y=tot.surv, color=hs.temp))
ecl_surv_plot + geom_point(position=position_jitter(height=0.03, width=0.3),
                           size=4
) + stat_smooth(data = ehs_dnc, aes(y=bin_surv),
                method="glm", method.args=list(family="binomial"), 
                formula=y~x, size=2)









