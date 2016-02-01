setwd("C:/Original/good stat/2016 spring/Applied data science/project 1")
getwd()
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  colsToKeep <- c("MSP", "ST","AGEP","SEX","MARHT","MARHYP")
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
  ylab("Married Percent") + 
  xlab("Region") + ggtitle("Comparing married percent between regions") +
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
  xlab("Marital Status") + ggtitle("Comparing married percent between regions(female") +
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
  xlab("Marital Status") + ggtitle("Comparing married percent between regions(male") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##age distribution
ggplot(female, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("female's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(male, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("male's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##never-married age distribution

unma_female<-filter(dm,MSP==6 & SEX==2)%>%
        group_by(Region)

ggplot(unma_female, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("never-married female's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

unma_male<-filter(dm,MSP==6 & SEX==1)%>%
    group_by(Region)

ggplot(unma_male, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) +   
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("never-married male's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ma_female<-filter(dm,(MSP %in%  c(1,2,3,4,5)) & SEX==2)%>%
  group_by(Region)

ggplot(ma_female, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("once-married female's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ma_male<-filter(dm,(MSP %in%  c(1,2,3,4,5)) & SEX==1)%>%
  group_by(Region)

ggplot(ma_male, aes(AGEP,color=factor(Region),fill=factor(Region),alpha=0.001)) +   
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("once-married male's age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##first-marriage age
f_m_f<-na.omit(female) %>%
       filter(MARHT==1) %>%
       group_by(Region)
f_m_a<-mutate(f_m_f,Married_age=AGEP-(2013-MARHYP))
ggplot(f_m_a, aes(Married_age,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("1-marriage female's first married age distribution") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

f_m_m<-na.omit(male) %>%
  filter(MARHT==1) %>%
  group_by(Region)
f_m_ma<-mutate(f_m_m,Married_age=AGEP-(2013-MARHYP))
ggplot(f_m_ma, aes(Married_age,color=factor(Region),fill=factor(Region),alpha=0.001)) + 
  scale_x_continuous(breaks=seq(0,100,5))+
  geom_density()+
  ylab("density") + 
  xlab("age") + ggtitle("1-marriage male's first married age distribution") +
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
  ylab("First-marriage failing Percent") + 
  xlab("Region") + ggtitle("Comparing First-marriage failing Percent for female between regions") +
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
  ylab("First-marriage failing Percent") + 
  xlab("Region") + ggtitle("Comparing First-marriage failing Percent for female between regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## num of times married
t_m_f<-group_by(ma_female,MARHT,Region) %>%
       summarise(count=n()) %>%
       mutate(percent=count/ma_female_num$count*100)

ggplot(t_m_f, aes(x=MARHT,y=percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity")+
  xlab("number of times married") + ggtitle("Comparing number of times married percent between regions(female)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

t_m_m<-group_by(ma_male,MARHT,Region) %>%
  summarise(count=n()) %>%
  mutate(percent=count/ma_male_num$count*100)

ggplot(t_m_m, aes(x=MARHT,y=percent,fill = factor(Region),color=factor(Region))) +   
  geom_bar(position = "dodge", stat="identity")+
  xlab("number of times married") + ggtitle("Comparing number of times married percent between regions(male)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##own children

