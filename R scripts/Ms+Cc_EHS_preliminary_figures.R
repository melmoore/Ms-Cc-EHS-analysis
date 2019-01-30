#Ms+Cc Early heat shock experiment--PRELIMINARY FIGURES



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

ehs <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Early Ms+Cc heat shock/Ms+Cc-EHS-analysis/data/Ms+Cc_EHS_incomplete_clean.csv")
View(ehs)

ehs.long <- read_csv("~/Manduca expts/Spring+Summer+Fall 2018/Early Ms+Cc heat shock/Ms+Cc-EHS-analysis/data/Ms+Cc_EHS_incomplete_clean_long.csv")
View(ehs.long)





#--------------------

#Looking at mongo.age and mongo.mass at the different heat shock treatments



#Plotting mass.end vs hs.num


ehs$hs.num<-as.factor(ehs$hs.num)

mongo.plot<-ggplot(ehs, aes(x=hs.num,y=mass.end.all,color=hs.num))
mongo.plot+geom_jitter(aes(shape=class)
         )+facet_wrap(~hs.temp)



#Doesn't seem to be any correlation between time between ovp and first heat shock affecting mass at end
mongo.time.plot<-ggplot(ehs,aes(x=time.ths1.ovp.dec,y=mass.end.all, group=class, color=class))
mongo.time.plot+geom_point(size=3
              )+facet_wrap(~hs.temp)


mongo.time.plot2<-ggplot(ehs,aes(x=time.ovp.dec,y=mass.end.all, group=class, color=class))
mongo.time.plot2+geom_point(size=3
)+facet_wrap(~hs.temp)


#Plotting % of treatment that falls into each class--that survived, have removed ind that died


n.table<-table(ehs$hs.num, ehs$hs.temp, ehs$class)
tot.table<-table(ehs$hs.num, ehs$hs.temp)

n.table<-data.frame(n.table)
tot.table<-data.frame(tot.table)

n.table<-dplyr::rename(n.table, hs.num=Var1, hs.temp=Var2, class=Var3)
tot.table<-dplyr::rename(tot.table, hs.num=Var1, hs.temp=Var2)


n.table$keep<-ifelse(n.table$hs.num==0 & n.table$hs.temp==0, 1,
                     ifelse(n.table$hs.temp==40 & n.table$hs.num!=0, 1,
                            ifelse(n.table$hs.temp==42 & n.table$hs.num!=0, 1, 0)))

n.table<-subset(n.table, keep==1)


tot.table$keep<-ifelse(tot.table$hs.num==0 & tot.table$hs.temp==0, 1,
                       ifelse(tot.table$hs.temp==40 & tot.table$hs.num!=0, 1,
                              ifelse(tot.table$hs.temp==42 & tot.table$hs.num!=0, 1, 0)))


tot.table<-subset(tot.table, keep==1)


tot.table[which(tot.table$hs.num==0 & tot.table$hs.temp==0),3]

n.table$tot<-ifelse(n.table$hs.num==0 & n.table$hs.temp==0, tot.table[which(tot.table$hs.num==0 & tot.table$hs.temp==0),3],
             ifelse(n.table$hs.num==1 & n.table$hs.temp==40, tot.table[which(tot.table$hs.num==1 & tot.table$hs.temp==40),3],
             ifelse(n.table$hs.num==2 & n.table$hs.temp==40, tot.table[which(tot.table$hs.num==2 & tot.table$hs.temp==40),3],
             ifelse(n.table$hs.num==3 & n.table$hs.temp==40, tot.table[which(tot.table$hs.num==3 & tot.table$hs.temp==40),3],
             ifelse(n.table$hs.num==4 & n.table$hs.temp==40, tot.table[which(tot.table$hs.num==4 & tot.table$hs.temp==40),3],
             ifelse(n.table$hs.num==1 & n.table$hs.temp==42, tot.table[which(tot.table$hs.num==1 & tot.table$hs.temp==42),3],
             ifelse(n.table$hs.num==2 & n.table$hs.temp==42, tot.table[which(tot.table$hs.num==2 & tot.table$hs.temp==42),3],
             ifelse(n.table$hs.num==3 & n.table$hs.temp==42, tot.table[which(tot.table$hs.num==3 & tot.table$hs.temp==42),3],
             ifelse(n.table$hs.num==4 & n.table$hs.temp==42, tot.table[which(tot.table$hs.num==4 & tot.table$hs.temp==42),3],0)))))))))


n.table<-mutate(n.table, prop = Freq/tot)

#making 0.0 data correspond to both 40 and 42 hs temp for plotting (put control data next to expt data on plot)

con40<-subset(n.table, hs.temp==0)
con40$hs.temp<-40

con42<-subset(n.table, hs.temp==0)
con42$hs.temp<-42


nt.con<-rbind(n.table, con40, con42)

nt.con<-subset(nt.con, hs.temp!=0)

nt.con$dat.typ<-ifelse(nt.con$hs.num!=0, "expt", "con")


#Plotting perc.class for each treatment

class.plot<-ggplot(nt.con,aes(x=hs.num,y=prop,fill=class, alpha=dat.typ))
class.plot+geom_bar(position="fill",stat="identity"
         )+scale_fill_manual(values=c("#95D840", "#1F9F88", "#440D54"),
                      breaks=c("em", "mongo", "wand"),
                      labels=c("Emergence", "Mongo", "Wandering"),
                      name="Outcome"
         )+scale_alpha_manual(values=c(.5, 1),
                              breaks=c("con", "expt"),
                              guide="none"
         )+labs(x="Days in Heat shock", y="Proportion"
         )+facet_wrap(~hs.temp
         )+theme(text = element_text(family=("Cambria")),
                 strip.background = element_rect(colour="black",linetype = "solid",fill="white",
                                                 size = 1),
                 strip.text = element_text(size=30),
                 axis.line.x=element_line(colour = 'black', size = 1),
                 axis.line.y=element_line(colour = 'black', size = 1),
                 axis.ticks = element_line(colour = 'black', size = 1),
                 axis.ticks.length = unit(2, "mm"),
                 axis.text.x = element_text(size = 26),
                 axis.text.y = element_text(size = 26),
                 axis.title.x = element_text(size = 30),
                 axis.title.y = element_text(size = 30),
                 legend.key.width=unit(15,"mm"),
                 legend.key.height = unit(10,"mm"),
                 legend.text=element_text(size=18),
                 legend.title=element_text(size=20),
                 legend.background = element_rect(color="black",linetype="solid",size=1))





#plotting mass vs age for each instar

#Need to make a column that differentiates those with wasp em and those that are mongos
  ##put this in cleaning script!!

ehs.long$wand[is.na(ehs.long$wand)]<-0

ehs.long$date.em.j[is.na(ehs.long$date.em.j)]<-0
ehs.long$date.end.j[is.na(ehs.long$date.end.j)]<-0

ehs.long$class<-ifelse(ehs.long$date.em.j>0,"em",
                       ifelse(ehs.long$mongo.mass==1, "mongo", "sm.mongo"))

ehs.long$class2<-ifelse(ehs.long$wand==1, "wand",0)

ehs.long$class2[ehs.long$class2==0]<-NA

ehs.long$class<-coalesce(ehs.long$class2, ehs.long$class)

ehs.long<-subset(ehs.long, wand!="1")


#Subsetting ehs.long to exclude those that are still in expt

#ehs.lf<-subset(ehs.long,date.3.j<=53)
#ehs.lf<-subset(ehs.lf, id!=212)
#ehs.lf<-subset(ehs.lf, id!=69)


mass.sum<-summarySE(ehs.long, measurevar = "mass",
                    groupvars = c("hs.temp","hs.num","instar","class"),
                    na.rm=TRUE)
mass.sum



age.sum<-summarySE(ehs.long, measurevar = "day.age",
                   groupvars = c("hs.temp","hs.num","instar","class"),
                   na.rm = TRUE)
age.sum


mass.sum$day.age<-age.sum[,6]
mass.sum$dage.se<-age.sum[,8]


mass.sum$instar<-as.factor(mass.sum$instar)
mass.sum$hs.num<-as.factor(mass.sum$hs.num)

mass.sum.0<-subset(mass.sum, hs.temp==0)
mass.sum.40<-subset(mass.sum, hs.temp==40)
mass.sum.42<-subset(mass.sum, hs.temp==42)

mage0.plot<-ggplot(mass.sum.0,aes(x=day.age,y=mass,group=interaction(hs.num,class),color=hs.num))
mage0.plot+geom_point(size=2
         )+geom_line(aes(linetype=class),
                     size=1.2)

mage40.plot<-ggplot(mass.sum.40,aes(x=day.age,y=mass,group=interaction(hs.num,class),color=hs.num))
mage40.plot+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2
)+facet_wrap(~class)


mage42.plot<-ggplot(mass.sum.42,aes(x=day.age,y=mass,group=interaction(hs.num,class),color=hs.num))
mage42.plot+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2
)+facet_wrap(~class)



mass.sum.em<-subset(mass.sum, class=="em")
mass.sum.sm<-subset(mass.sum, class=="sm.mongo")
mass.sum.mongo<-subset(mass.sum, class=="mongo")


mem.plot<-ggplot(mass.sum.em, aes(x=day.age,y=mass,group=hs.num,color=hs.num))
mem.plot+geom_point(aes(size=N)
       )+geom_line(size=1.2
       )+facet_wrap(~hs.temp)


msm.plot<-ggplot(mass.sum.sm, aes(x=day.age,y=mass,group=hs.num,color=hs.num))
msm.plot+geom_point(aes(size=N)
)+geom_line(size=1.2
)+facet_wrap(~hs.temp)


mmong.plot<-ggplot(mass.sum.mongo, aes(x=day.age,y=mass,group=hs.num,color=hs.num))
mmong.plot+geom_point(aes(size=N)
)+geom_line(size=1.2
)+facet_wrap(~hs.temp)


ehs.lmall<-subset(ehs.lf, class=="sm.mongo" | class=="mongo")




#Creating plot of log.mass by age


ehs.long$log.mass<-log(ehs.long$mass)



lm.sum<-summarySE(ehs.long, measurevar = "log.mass",
                    groupvars = c("hs.temp","hs.num","instar","class"),
                    na.rm=TRUE)
lm.sum


lm.sum$day.age<-age.sum[,6]
lm.sum$dage.se<-age.sum[,8]


lm.sum$instar<-as.factor(lm.sum$instar)
lm.sum$hs.num<-as.factor(lm.sum$hs.num)
lm.sum$hs.temp<-as.factor(lm.sum$hs.temp)

lmage.plot<-ggplot(lm.sum,aes(x=day.age,y=log.mass,group=interaction(hs.num,class),color=hs.num))
lmage.plot+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2
)+facet_wrap(~hs.temp)



#best format so far, but may want to plot each hs.temp and hs.num combination separately, then put together in  better order
lmage.plot2<-ggplot(lm.sum,aes(x=day.age,y=log.mass,group=class,color=class))
lmage.plot2+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2
)+facet_wrap(~hs.num*hs.temp)


#Plotting log.mass by day.age separately for hs.temps

lm.sum40<-subset(lm.sum, hs.temp==40)
lm.sum42<-subset(lm.sum, hs.temp==42)
lm.sum0<-subset(lm.sum, hs.temp==0)


lmage.plot40<-ggplot(lm.sum40,aes(x=day.age,y=log.mass,group=class,color=class))
lmage.plot40+geom_point(size=3
)+geom_errorbar(aes(ymin=log.mass-se,ymax=log.mass+se),
                width=.5,size=1
)+geom_errorbarh(aes(xmin=day.age-dage.se,xmax=day.age+dage.se),
                 height=.5,size=1
)+geom_line(aes(linetype=class),
            size=1.2
)+scale_color_manual(values=c("#000000","red","#E69F00"),
                     breaks=c("em","sm.mongo","mongo"),
                     labels=c("Wasp emergence","Mongo < 14g","Mongo > 14g"),
                     name="Fate"
)+scale_linetype_manual(values=c("solid","dashed","dotted"),
                        breaks=c("em","sm.mongo","mongo"),
                        labels=c("Wasp emergence","Mongo < 14g","Mongo > 14g"),
                        name="Fate"
)+facet_wrap(~hs.num)



lmage.plot42<-ggplot(lm.sum42,aes(x=day.age,y=log.mass,group=class,color=class))
lmage.plot42+geom_point(size=3
)+geom_errorbar(aes(ymin=log.mass-se,ymax=log.mass+se),
                width=.5,size=1
)+geom_errorbarh(aes(xmin=day.age-dage.se,xmax=day.age+dage.se),
                 height=.5,size=1
)+geom_line(aes(linetype=class),
            size=1.2
)+scale_color_manual(values=c("#000000","red","#E69F00"),
                     breaks=c("em","sm.mongo","mongo"),
                     labels=c("Wasp emergence","Mongo < 14g","Mongo > 14g"),
                     name="Fate"
)+scale_linetype_manual(values=c("solid","dashed","dotted"),
                        breaks=c("em","sm.mongo","mongo"),
                        labels=c("Wasp emergence","Mongo < 14g","Mongo > 14g"),
                        name="Fate"
)+facet_wrap(~hs.num)




lmage.plot0<-ggplot(lm.sum0,aes(x=day.age,y=log.mass,group=class,color=class))
lmage.plot0+geom_point(size=3
)+geom_errorbar(aes(ymin=log.mass-se,ymax=log.mass+se),
                width=.5,size=1
)+geom_errorbarh(aes(xmin=day.age-dage.se,xmax=day.age+dage.se),
                 height=.5,size=1
)+geom_line(aes(linetype=class),
            size=1.2
)+scale_color_manual(values=c("#000000","red","#E69F00"),
                     breaks=c("em","sm.mongo","mongo"),
                     labels=c("Wasp emergence","Mongo < 14g","Mongo > 14g"),
                     name="Fate"
)+scale_linetype_manual(values=c("solid","dashed","dotted"),
                        breaks=c("em","sm.mongo","mongo"),
                        labels=c("Wasp emergence","Mongo < 14g","Mongo > 14g"),
                        name="Fate")




lmage0.plot<-ggplot(lm.sum0,aes(x=day.age,y=log.mass,group=interaction(hs.num,class),color=hs.num))
lmage0.plot+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2)



lmage40.plot<-ggplot(lm.sum40,aes(x=day.age,y=log.mass,group=interaction(hs.num,class),color=hs.num))
lmage40.plot+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2
)+facet_wrap(~class)


lmage42.plot<-ggplot(lm.sum42,aes(x=day.age,y=log.mass,group=interaction(hs.num,class),color=hs.num))
lmage42.plot+geom_point(size=2
)+geom_line(aes(linetype=class),
            size=1.2
)+facet_wrap(~class)



#------------------------


#Creating a column that combines mongos and sm.mongos for visualization purposes

ehs.lf$cl.comb<-gsub("sm.", "", ehs.lf$class)




#Plotting raw data for each caterpillar--log mass by age
  ##separating out by hs.temp

ehs.lf40<-subset(ehs.lf,hs.temp==40)
ehs.lf42<-subset(ehs.lf, hs.temp==42)
ehs.lf0<-subset(ehs.lf, hs.temp==0)


#With sm.mongos and mongos separated

#hs.temp==40
rlmage.plot40<- ggplot(ehs.lf40, aes(x=day.age, y=log.mass, group=interaction(id, hs.num), color=class))
rlmage.plot40+geom_line(aes(linetype=class)
          )+facet_wrap(~hs.num)
            

#hs.temp==42
rlmage.plot42<- ggplot(ehs.lf42, aes(x=day.age, y=log.mass, group=interaction(id, hs.num), color=class))
rlmage.plot42+geom_line(aes(linetype=class)
          )+facet_wrap(~hs.num)


#hs.temp==0
rlmage.plot0<- ggplot(ehs.lf0, aes(x=day.age, y=log.mass, group=interaction(id, hs.num), color=class))
rlmage.plot0+geom_line(aes(linetype=class))




#With sm.mongos and mongos combined

#hs.temp==40
rlmage.plot<- ggplot(ehs.lf40, aes(x=day.age, y=log.mass, group=interaction(id, hs.num), color=cl.comb))
rlmage.plot+geom_line(aes(linetype=cl.comb)
          )+facet_wrap(~hs.num)


#hs.temp==42
rlmage.plot<- ggplot(ehs.lf42, aes(x=day.age, y=log.mass, group=interaction(id, hs.num), color=cl.comb))
rlmage.plot+geom_line(aes(linetype=cl.comb)
          )+facet_wrap(~hs.num)


#hs.temp==0
rlmage.plot<- ggplot(ehs.lf0, aes(x=day.age, y=log.mass, group=interaction(id, hs.num), color=cl.comb))
rlmage.plot+geom_line(aes(linetype=cl.comb)
          )+facet_wrap(~hs.num)






#-----------------------------

#plotting mass.end by time in heat shock chamber 


tinhs.plot<-ggplot(ehs.fin,aes(x=day.hs,y=mass.end,color=class))
tinhs.plot+geom_point(
         )+facet_wrap(~hs.temp)



#Plotting mass.end by num ovp

numovp.plot<-ggplot(ehs.fin,aes(x=num.ovp,y=mass.end,color=class))
numovp.plot+geom_jitter(
)+facet_wrap(~hs.temp)



#-----------


#Plotting mass end by mass at 3rd

mass3end.plot<-ggplot(ehs,aes(x=mass.3,y=mass.end.all,color=class))
mass3end.plot+geom_point(size=3
            )+facet_wrap(~hs.temp)




#-------------


#Plotting a distribution of masses at end for mongos

#subsetting mongos
ehs.m<-subset(ehs, class!="em" & class!="wand")


#histogram
mhist.plot<-ggplot(ehs.m, aes(x=mass.end))
mhist.plot+geom_histogram(binwidth = 500
         )+facet_wrap(~hs.temp)



#frequency dist plot--with hs.num clumped within hs.temp
mdist.plot<-ggplot(ehs.m, aes(x=mass.end))
mdist.plot+geom_density(adjust=1/4,size=1.5
         )+facet_wrap(~hs.temp)



#Frequency dist plot--with hs.num plotted separately

mdist.plot2<-ggplot(ehs.m, aes(x=mass.end, fill=hs.num))
mdist.plot2+geom_density(
          )+scale_fill_viridis(discrete = TRUE, alpha=.7
          )+facet_wrap(~hs.temp)




#Frequency dist plot--with hs.num plotted separately--"fill" option, which produces a conditional density estimate

mdist.plot3<-ggplot(ehs.m, aes(x=mass.end, fill=hs.num))
mdist.plot3+geom_density(position="fill"
)+scale_fill_viridis(discrete = TRUE, alpha=.7
)+facet_wrap(~hs.temp)




#Frequency dist plot--with hs.num plotted separately--at molt to 5th

mdist.plot4<-ggplot(ehs.m, aes(x=mass.5, fill=hs.num))
mdist.plot4+geom_density(adjust=1/2
)+scale_fill_viridis(discrete = TRUE, alpha=.7
)+facet_wrap(~hs.temp)






#Frequency dist plot--with hs.num plotted separately--at molt to 4th

mdist.plot4<-ggplot(ehs.m, aes(x=mass.4, fill=hs.num))
mdist.plot4+geom_density(adjust=1/2
)+scale_fill_viridis(discrete = TRUE, alpha=.7
)+facet_wrap(~hs.temp)


#subsetting by temperature

ehs.m40<-subset(ehs.m,hs.temp=="40")
ehs.m42<-subset(ehs.m,hs.temp=="42")


#try mass by age plot, with raw points and violin plots
  #do within each instar, hs.num on x axis, mass on y--violin plot on top of raw points
  #both temps

madend.plot<-ggplot(ehs.m, aes(x=hs.num, y=mass.end, color=hs.num))
madend.plot<-madend.plot+geom_violin(aes(fill=hs.num),
                     alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3
)+facet_wrap(~hs.temp)



#plotting hs.temps separately

madend40.plot<-ggplot(ehs.m40, aes(x=hs.num, y=mass.end, color=hs.num))
madend40.plot<-madend40.plot+geom_violin(aes(fill=hs.num),
                                     alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3)


madend42.plot<-ggplot(ehs.m42, aes(x=hs.num, y=mass.end, color=hs.num))
madend42.plot<-madend42.plot+geom_violin(aes(fill=hs.num),
                                         alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3)


#Point and violin plot of mass at 5th--both hs temps

mad5.plot<-ggplot(ehs.m, aes(x=hs.num, y=mass.5, color=hs.num))
mad5.plot<-mad5.plot+geom_violin(aes(fill=hs.num),
                        alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3
)+facet_wrap(~hs.temp)



#plotting hs.temp separately

mad540.plot<-ggplot(ehs.m40, aes(x=hs.num, y=mass.5, color=hs.num))
mad540.plot<-mad540.plot+geom_violin(aes(fill=hs.num),
                                 alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3)


mad542.plot<-ggplot(ehs.m42, aes(x=hs.num, y=mass.5, color=hs.num))
mad542.plot<-mad542.plot+geom_violin(aes(fill=hs.num),
                                     alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3)



#Point and violin plot of mass at 4th

mad4.plot<-ggplot(ehs.m, aes(x=hs.num, y=mass.4, color=hs.num))
mad4.plot<-mad4.plot+geom_violin(aes(fill=hs.num),
                      alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3
)+facet_wrap(~hs.temp)



#Point and violin plot of mass at 3rd

mad3.plot<-ggplot(ehs.m, aes(x=hs.num, y=mass.3, color=hs.num))
mad3.plot<-mad3.plot+geom_violin(aes(fill=hs.num),
                      alpha=.4
)+scale_fill_manual(values=c("#471265", "#3A78AB", "#23A99B", "#96D83E"),
                    breaks=c(1,2,3,4),
                    name="Days in Heat Shock"
)+scale_color_manual(values=c("#471265", "#3A78AB", "#23A99B", "#83C628"),
                     breaks=c(1,2,3,4),
                     name="Days in Heat Shock"
)+geom_jitter(size=3
)+facet_wrap(~hs.temp)





#combining into one plot using cowplot
  ##need to work on a way to format this better

madall.plot<-plot_grid(mad3.plot,mad4.plot,mad5.plot,madend.plot,
                       labels=c("Mass at 3rd", "Mass at 4th", "Mass at 5th","Mass at End"),
                       align="v", ncol=1, nrow=4)

madall.plot


#subset ehs.long to only mongos

ehs.lfm<-subset(ehs.lf, class!="em" & class!="wand")






#might be good to do multinomial
#can do random effects multinomials, but not in lme4 or nlme
#Can do in rstanarm in bayesian methods

#use multinomial to assess affect of heat shock on final "fate", use a different analysis to look at effect of
#heat shock on mass--regular lme?

#creating point and violin plot of all classes for end mass

#creating combined column with mass 48em, mass at end and mass at wandering--need to extract wand mass

fatedist.plot<-ggplot(ehs,aes(x=hs.num, y=mass.end.all, group=interaction(hs.num, class), color=class))
fatedist.plot+geom_jitter(
)+geom_violin(aes(fill=class),
              alpha=.4
)+facet_wrap(~hs.temp)


#making the 0.0 be included in the 40 and 42s, for plotting purposes

ehs$dat.type<-ifelse(ehs$hs.temp!=0, "expt", "con")

ehs.con40<-subset(ehs, hs.temp==0)
ehs.con40$hs.temp<-40

ehs.con42<-subset(ehs, hs.temp==0)
ehs.con42$hs.temp<-42

ehs.con<-subset(ehs, hs.temp!=0)
ehs.con<-rbind(ehs.con, ehs.con40, ehs.con42)


#plot with points and box plot for each class


fatebox.plot<-ggplot(ehs.con,aes(x=hs.num, y=mass.end.all, group=interaction(hs.num, class)))
fatebox.plot+geom_jitter(aes(color=class),
                         size=3, width=0.25, height=0
)+geom_boxplot(aes(fill=class),
               alpha=.6, outlier.color = NA,
               lwd=1.2
)+scale_colour_manual(values=c("#95D840", "#1F9F88", "#440D54"),
                      breaks=c("em", "mongo", "wand"),
                      labels=c("Emergence", "Mongo", "Wandering"),
                      name="Outcome", guide="none"
)+scale_fill_manual(values=c("#95D840", "#1F9F88", "#440D54"),
                    breaks=c("em", "mongo", "wand"),
                    labels=c("Emergence", "Mongo", "Wandering"),
                    name="Outcome", guide="none"
)+labs(x="Days in Heat shock", y="Mass [mg]"
)+facet_wrap(~hs.temp
)+theme(text = element_text(family=("Cambria")),
        strip.background = element_rect(colour="black",linetype = "solid",fill="white",
                                        size = 1),
        strip.text = element_text(size=35),
        axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size = 32),
        legend.key.width=unit(15,"mm"),
        legend.key.height = unit(10,"mm"),
        legend.text=element_text(size=28),
        legend.title=element_text(size=30),
        legend.background = element_rect(color="black",linetype="solid",size=1))





#---------------------

#Calculating development time--planning for early, post hatch and maybe late hs experiment

tt4.sum<-summarySE(ehs, measurevar = "tt4",
                   groupvars = c("hs.temp", "hs.num", "class"),
                   na.rm = TRUE)
tt4.sum


inst4.sum<-summarySE(ehs, measurevar = "inst4",
                     groupvars = c("hs.temp", "hs.num", "class"),
                     na.rm = TRUE)
inst4.sum


tt5.sum<-summarySE(ehs, measurevar = "tt5",
                   groupvars = c("hs.temp", "hs.num", "class"),
                   na.rm = TRUE)


ttem.sum<-summarySE(ehs, measurevar = "ttem",
                    groupvars = c("hs.temp", "hs.num", "class"),
                    na.rm = TRUE)
ttem.sum



#----------------------

#Plotting wasp survival (by numbers, don't have enough dissected)


ehs$load[is.na(ehs$load)]<-0
ehs$num.ecl[is.na(ehs$num.ecl)]<-0

ehs$hs.num<-as.factor(ehs$hs.num)
ehs$hs.temp<-as.factor(ehs$hs.temp)

numecl.plot<-ggplot(ehs, aes(x=load, y=num.ecl, group=hs.num, color=hs.num))
numecl.plot+geom_point(
)+geom_smooth(method=lm
)+facet_wrap(~hs.temp)


#mean ps.ecl for each hs.num and hs.temp treatment

numecl.sum<-summarySE(ehs, measurevar = "num.ecl",
                     groupvars = c("hs.temp", "hs.num"),
                     na.rm=TRUE)
numecl.sum


numecl.sum.plot<-ggplot(numecl.sum, aes(x=hs.num, y=num.ecl, group=hs.temp, color=hs.temp))
numecl.sum.plot+geom_point(aes(shape=hs.temp),
                           size=7
)+geom_line(size=2
)+geom_errorbar(aes(ymin=num.ecl-se, ymax=num.ecl+se),
                width=.4, size=1.2
)+scale_color_manual(values=c("orange", "red", "black"),
                     name="Heat Shock Temp",
                     label=c("Control", "40", "42")
)+scale_shape_manual(values=c(16, 15, 17),
                     name="Heat Shock Temp",
                     label=c("Control", "40", "42")
)+labs(x="Days in Heat Shock", y="Survival to Eclosion (#)"                     
)+theme(text = element_text(family=("Cambria"),face = "bold"),
        strip.background = element_rect(colour="black",linetype = "solid",fill="white",
                                        size = 3),
        strip.text = element_text(size=30),
        axis.line.x=element_line(colour = 'black', size = 2),
        axis.line.y=element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = 'black', size = 2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 26, face = "bold"),
        axis.text.y = element_text(size = 26, face = "bold"),
        axis.title.x = element_text(size = 30, face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 30, face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.key.width=unit(15,"mm"),
        legend.key.height = unit(10,"mm"),
        legend.text=element_text(size=18, face = "bold"),
        legend.title=element_text(size=20, face = "bold"),
        legend.background = element_rect(color="black",linetype="solid",size=2),
        legend.position = c(0.7, 0.8))

