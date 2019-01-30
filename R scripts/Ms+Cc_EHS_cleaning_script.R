#Ms + Cc Early heat shock expt--CLEANING SCRIPT


#load libraries
library(readr)
library(dplyr)
library(tidyr)



#---------------------------------

#import data

ehs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Early Ms+Cc heat shock/Ms+Cc-EHS-analysis/data/Ms+Cc_EHS_incomp_data_4-26-18.csv")
View(ehs)



#-----------------------------

#removing empty rows at bottom of data sheet

ehs$id[is.na(ehs$id)]<-0

ehs<-subset(ehs, id>0)


#------------------------------

#Transform date columns into julian date

##Converts x into julian date
j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}


#Takes all columns that have "date." in the name, and converts contents to Julian day using j.date function. Renames columns (adds a 
##j to end of column name), and binds the out put julian day columns to the original data set

lapj.date<-function(df){
  date.j<-lapply(df[,grep("date.",colnames(df))],j.date)
  date.j<-as.data.frame(date.j)
  colnames(date.j)<-paste(colnames(date.j), "j", sep = ".")
  output.df<-cbind(df,date.j)
  output.df
}


ehs<-lapj.date(ehs)

ehs$date.died.j[is.na(ehs$date.died.j)]<-0
ehs$died<-ifelse(ehs$date.died.j==0, "0", "1")


#-------------------------

#removing culled individuals

ehs<-ehs[grep("culled",ehs$gen.notes,invert = TRUE),]


#------------------------

#calculating % host survival for each treatment

surv<-ehs %>% count(hs.temp,hs.num)

died<-ehs %>% count(hs.temp,hs.num,died)

died<-subset(died,died=="1")
died.n<-died[,4]


surv<-surv %>% mutate(died.n = died$n)
surv<-dplyr::rename(surv, tot.n=n)


surv<-surv %>% mutate(surv.prop = 1-(died.n/tot.n))


write.csv(surv, "treat_surv_table.csv",row.names = FALSE)


#----------------------------

#Excluding those that died for cleaned data for analysis

  
ehs<-filter(ehs,died!=1)



#--------------------------------

#Converting time to decimal time

#renaming tths.ovp1 to time.ths.ovp1 so that it will be included in the dec script

ehs<-dplyr::rename(ehs, time.ths1.ovp=tths1.ovp)


#Function that turns turns time (x) into a character, splits it at the :, and adds it together to get decimal time
dec.time<-function(x) {
  x<-as.character(x)
  sapply(strsplit(x,":"),function(x){
    x <- as.numeric(x)
    y<-x[1]+x[2]/60
    
  })
}


#Function that applies the dec.time function to every column with "time." in the name, and adds decminal time columns to 
  ##dataframe
dec.time.col<-function(df){
  dct<-lapply(df[,grep("time.",colnames(df))],dec.time)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}

ehs<-dec.time.col(ehs)


#-------------------------

#Creating 2 columns that have the # of hours and days the caterpillar was in the heatshock chamber

ehs<-ehs %>% mutate(hr.hs = (((date.out.hs.j-date.in.hs.j)*24)+(time.out.hs.dec-time.in.hs.dec))) %>%
             mutate(day.hs=(hr.hs/24))




#-------------------------


#Calculating development time for caterpillars

ehs<-ehs %>% mutate(tt3 = date.3.j-date.hatch.j) %>%
             mutate(inst3 = date.4.j-date.3.j) %>%
             mutate(tt4 = date.4.j-date.hatch.j) %>%
             mutate(inst4 = date.5.j-date.4.j) %>%
             mutate(tt5 = date.5.j-date.hatch.j) %>%
             mutate(ttem = date.em.j-date.hatch.j) %>%
             mutate(ttend = date.end.j-date.hatch.j) %>%
             mutate(ttwand = date.wand.j-date.hatch.j)


#Calculating development time for wasps


ehs<-ehs %>% mutate(ttem.w = date.em.j-date.ovp.j) %>%
              mutate(ttecl = date.ecl.j-date.ovp.j) %>%
              mutate(dt.coc = date.ecl.j-date.em.j)


#Once I have load data, calculate % survival at each wasp stage



#--------------------------------


#Creating a long format data set for mass and age 

  #Need to make a day hatch dummy column with 0s 

ehs$mass.end.all<-coalesce(ehs$mass.48em, ehs$mass.end, ehs$mass.wand)


#Making a column "class"--either em, sm.mongo, mongo or wand

ehs$wand[is.na(ehs$wand)]<-0

ehs$date.em.j[is.na(ehs$date.em.j)]<-0
ehs$date.end.j[is.na(ehs$date.end.j)]<-0

#making mongos just 1 class, not separating by size atm

ehs$class<-ifelse(ehs$date.em.j>0,"em",
                  ifelse(ehs$date.end.j>0, "mongo", 
                         ifelse(ehs$wand==1, "wand", 0)))




ehs.long<-ehs %>% gather(instar, mass, mass.3, mass.4, mass.5, mass.end.all)
ehs.long$instar<-gsub("mass.", "", ehs.long$instar)


ehs$ttend.all<-coalesce(ehs$ttem, ehs$ttend, ehs$ttwand)

age.long<-ehs %>% gather(instar, day.age, tt3, tt4, tt5, ttend.all)
age.long$instar<-gsub("tt", "", age.long$instar)

age.long<-select(age.long, id, hs.temp, hs.num, instar, day.age)

ehs.long<-merge(ehs.long, age.long, by=c("id", "hs.temp", "hs.num", "instar"))


write.csv(ehs, "Ms+Cc_EHS_incomplete_clean.csv",row.names = FALSE)
write.csv(ehs.long, "Ms+Cc_EHS_incomplete_clean_long.csv", row.names = FALSE)

