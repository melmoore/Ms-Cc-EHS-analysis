# EHS__Picking random subset of mongos from each treatment for dissection

#load libraries

library(readr)
library(ggplot2)
library(tidyr)
library(Rmisc)
library(dplyr)
library(viridis)
library(cowplot)
library(extrafont)



#-------------------

#Load data

ehs <- read_csv("data/Ms+Cc_EHS_incomplete_clean.csv", 
                col_types = cols(hs.num = col_factor(levels = c("0", "1", "2", "3", "4")), 
                                 hs.temp = col_factor(levels = c("0", "40", "42"))))

View(ehs)



#--------------------

# subset to only mongos

mongo <- subset(ehs, class=="mongo")
View(mongo)


#select a random sample of 6 from each hs temp and hs num treatments (with set.seed, should be reproducible)
set.seed(1)
test <- mongo %>% group_by(hs.temp, hs.num) %>% sample_n(6)

View(test)


#save as a csv for referral outside of R
write.csv(test, "EHS_mongos_for_diss.csv", row.names = FALSE)


