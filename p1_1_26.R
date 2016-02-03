setwd("C:/Original/good stat/2016 spring/Applied data science/project 1")
getwd()
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  colsToKeep <- c("MSP", "ST","AGEP","SEX","MARHT","MARHYP","OC")
  popDataA <- fread("ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("ss13pusb.csv", select=colsToKeep )
  populData <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(populData, file="populData.RData")
}else{
  load("populData.RData")
} 

ST.name=read.csv("statename.csv",header =F) 

##add variables including statename,state abbreviation, region
region_name=c("Northeast","Midwest","South","West")
populData<- tbl_df(populData)
populData = mutate(populData,STname=ST.name[ST,2],
                 STabbr = ST.name[ST,3],
                 Region = ST.name[ST,4])

##delete people who are less than 15 years old
dm<-populData[!is.na(populData$MSP),]
dm<-filter(dm,Region %in%  c(1,2,3,4)) %>%
    group_by(Region) 

dm$Region<-region_name[dm$Region]


##count people who are allowed to marry in each region
People<-summarise(dm,count=n())

##count married percent
Married<-dm %>%
         filter(MSP==1|MSP==2) %>%
         group_by(Region) %>%
         summarise(count=n())%>%
         mutate(Percet = count/People$count*100)
levels(Married$Region) <- c("Northeast","Midwest","South","West")
##plot
ggplot(Married, aes(x = Region, y =Percet,fill = factor(Region),color=factor(Region))) +   
   geom_bar(position = "dodge", stat="identity",width = 0.75)+
   geom_text(aes(label = round(Percet,2)), vjust = -0.5, colour = "black")+
   ylab("Married People Percent") + 
   xlab("Region") + ggtitle("Comparing Married People Percent between Regions") +
   theme(axis.text.x = element_text(angle = 30, hjust = 1))

female<-filter(dm,SEX==2)%>%
  group_by(Region)

num_female<-summarise(female,count=n())

f_ma<-female %>%
  group_by(MSP,Region) %>%
  summarise(count=n()) %>%
  mutate(Percet =count/num_female$count*100)

MSP_name<-c("Now married, spouse present","Now married, spouse absent","Widowed","Divorced","Separated","Never married")
f_ma$MSP<-MSP_name[f_ma$MSP]

##plot
ggplot(f_ma, aes(x=MSP,y=Percet,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity")+
  xlab("Marital Status")+
  ylab("Percent") + ggtitle("Comparing Marriage Status between Regions (Female)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

male<-filter(dm,SEX==1)%>%
  group_by(Region)

num_male<-summarise(male,count=n())

m_ma<-male %>%
  group_by(MSP,Region) %>%
  summarise(count=n()) %>%
  mutate(Percet =count/num_male$count*100)

m_ma$MSP<-MSP_name[m_ma$MSP]

##plot
ggplot(m_ma, aes(x=MSP,y=Percet,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity")+
  xlab("Marital Status") +
  ylab("Percent") + ggtitle("Comparing Marriage Status between Regions (Male)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##age distribution
ggplot(female, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("Female's Age Distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(male, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("Male's Age Distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##never-married age distribution

unma_female<-filter(dm,MSP==6 & SEX==2)%>%
        group_by(Region)

ggplot(unma_female, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("Single Female's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

unma_male<-filter(dm,MSP==6 & SEX==1)%>%
    group_by(Region)

ggplot(unma_male, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) +   
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("Single Male's Age Distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ma_female<-filter(dm,(MSP %in%  c(1,2,3,4,5)) & SEX==2)%>%
  group_by(Region)

ggplot(ma_female, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("Married Female's Age Distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ma_male<-filter(dm,(MSP %in%  c(1,2,3,4,5)) & SEX==1)%>%
  group_by(Region)

ggplot(ma_male, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) +   
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("Married Male's Age Distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##first-marriage age
f_m_f<-na.omit(female) %>%
       filter(MARHT==1) %>%
       group_by(Region)
f_m_a<-mutate(f_m_f,Married_age=AGEP-(2013-MARHYP))
ggplot(f_m_a, aes(Married_age,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("The Distribution of Age at First-marriage (Female)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f_m_m<-na.omit(male) %>%
  filter(MARHT==1) %>%
  group_by(Region)
f_m_ma<-mutate(f_m_m,Married_age=AGEP-(2013-MARHYP))
ggplot(f_m_ma, aes(Married_age,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("Density") + 
  xlab("Age") + ggtitle("The Distribution of Age at First-marriage (Male)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##first-marriage fail percent
ma_female_num<-summarise(ma_female,count=n())

f_m_f_f<-na.omit(ma_female) %>%
  filter((MARHT==1 & MSP==4)|(MARHT>1)) %>%
  group_by(Region) %>%
  summarise(count=n()) %>%
  mutate(percent=count/ma_female_num$count*100)

ggplot(f_m_f_f, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("First-marriage Failure Percent") + 
  xlab("Region") + ggtitle("Comparing First-marriage Failure Percent for Female between regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ma_male_num<-summarise(ma_male,count=n())

f_m_f_m<-na.omit(ma_male) %>%
  filter((MARHT==1 & MSP==4)|(MARHT>1)) %>%
  group_by(Region) %>%
  summarise(count=n()) %>%
  mutate(percent=count/ma_male_num$count*100)

ggplot(f_m_f_m, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("First-marriage Failure Percent") + 
  xlab("Region") + ggtitle("Comparing First-marriage Failure Percent for Male between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## num of times married
t_m_f<-group_by(ma_female,MARHT,Region) %>%
       summarise(count=n()) %>%
       mutate(percent=count/ma_female_num$count*100)

ggplot(t_m_f, aes(x=factor(MARHT),y=percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity")+
  xlab("Number of Times Married") + ggtitle("Comparing Number of Times Married Percent between Regions (Female)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

t_m_m<-group_by(ma_male,MARHT,Region) %>%
  summarise(count=n()) %>%
  mutate(percent=count/ma_male_num$count*100)

ggplot(t_m_m, aes(x=MARHT,y=percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity")+
  xlab("Number of Times Married") + ggtitle("Comparing Number of Times Married Percent between Regions (Male)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##own children (no difference for both male and female)
oc_female<-filter(female,OC==1) %>%
         summarise(count=n())%>%
         mutate(percent=count/num_female$count*100)

ggplot(oc_female, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("Percent") + 
  xlab("Region") + ggtitle("Comparing Mom Percent between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

oc_male<-filter(male,OC==1) %>%
  summarise(count=n())%>%
  mutate(percent=count/num_male$count*100)

ggplot(oc_male, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("Percent") + 
  xlab("Region") + ggtitle("Comparing Dad Percent between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##never-married people with own children
unma_female_num<-summarise(unma_female,count=n())
unmarried_mom<-filter(unma_female,(OC==1)) %>%
              summarise(count=n())%>%
              mutate(percent=count/unma_female_num$count*100)

ggplot(unmarried_mom, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("Percent") + 
  xlab("Region") + ggtitle("Comparing Female with Illegitimate Children Percent between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

unma_male_num<-summarise(unma_male,count=n())
unmarried_dad<-filter(unma_male,(OC==1)) %>%
  summarise(count=n())%>%
  mutate(percent=count/unma_male_num$count*100)

ggplot(unmarried_dad, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("Percent") + 
  xlab("Region") + ggtitle("Comparing Male with Illegitimate Children Percent between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  colsToKeep <- c( "ST","HHT","NOC","SSMC")
  hhDataA <- fread("ss13husa.csv", select=colsToKeep )  
  hhDataB <- fread("ss13husb.csv", select=colsToKeep )
  hhData <- rbind(hhDataA, hhDataB)
  rm(hhDataA, hhDataB)
  save(hhData, file="hhData.RData")
}else{
  load("hhData.RData")
} 

ST.name=read.csv("statename.csv",header =F) 

##add variables including statename,state abbreviation, region
region_name=c("Northeast","Midwest","South","West")
hhData<- tbl_df(hhData)
hhData = mutate(hhData,STname=ST.name[ST,2],
                   STabbr = ST.name[ST,3],
                   Region = ST.name[ST,4])

##delete households that are GQ and vacant
dh<-na.omit(hhData)
dh<-filter(dh,Region %in%  c(1,2,3,4)) %>%
  group_by(Region) 

dh$Region<-region_name[dh$Region]

#same-sex marriage percent

Married_couple<-filter(dh,HHT==1) %>%
              group_by(Region)

num_mac<-summarise(Married_couple,count=n())

samesex_ma_couple<-filter(dh,SSMC==2) %>%
                 summarise(count=n()) %>%
                 mutate(percent=count/num_mac$count*100)

ggplot(samesex_ma_couple, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("Same-sex Marriage Percent") + 
  xlab("Region") + ggtitle("comparing Same-sex Marriage Percent between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))            

#  samesex_ma_couple1<-filter(dh,(SSMC==2)|(SSMC==1)) %>%
#   summarise(count=n()) %>%
#   mutate(percent=count/num_mac$count*100)
# 
# ggplot(samesex_ma_couple1, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
#   geom_bar(position = "dodge", stat="identity",width = 0.75)+
#   geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
#   ylab("Same-sex marriage percent per married couple") + 
#   xlab("Region") + ggtitle("Same-sex marriage percent between regions") +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))   

#number of children(need more consideration)
# 
# family_hh<-filter(dh,HHT %in% c(1,2,3)) %>%
#            group_by(Region)
# 
# ggplot(family_hh, aes(NOC,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
#   ##scale_x_continuous(breaks=seq(0,20,5))+
#   geom_density()+
#   ylab("density") + 
#   xlab("age") + ggtitle("number of own children per family distribution") +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))

#single-parent percent
single_family<-filter(dh,HHT %in% c(2,3)) %>%
  group_by(Region)
num_sf<-summarise(single_family,count=n())
single_parent<-filter(single_family,NOC>0) %>%
              summarise(count=n()) %>%
              mutate(percent=count/num_sf$count*100)

ggplot(single_parent, aes(x = Region, y =percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity",width = 0.75)+
  geom_text(aes(label = round(percent,2)), vjust = -0.5, colour = "black")+
  ylab("Single Parent Percent") + 
  xlab("Region") + ggtitle("Comparing Single Parent Percent between Regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

age<-dm$AGEP
mstatus<-factor(dm$MSP)
cstatus<-factor(dm$OC)
levels(mstatus)<-c("Married","Married", "Married","Married","Married","Single")


plot(mstatus~age)
plot(cstatus~age)
lm1<-glm(mstatus~age, family=binomial(link="logit"))
lm2<-glm(cstatus~age, family=binomial(link="logit"))
summary(lm1)
summary(lm2)
dd<-data.frame(xx=age,yy=cstatus)
dd<-dd[dd$xx>20,]
plot(yy~xx,data=dd)
plot(xx~yy,data=dd)
# mpop<-na.omit(dm) %>%
#      mutate(mf=0)
# mage<-mpop$AGEP
# mpop[(mpop$MARHT==1 & mpop$MSP==4)|(mpop$MARHT>1),"mf"]=1 
# mpop$mf<-factor(mpop$mf)
# mfr<-mpop$mf
# levels(mfr)<-c("unfail","fail")
# plot(mfr~mage)


