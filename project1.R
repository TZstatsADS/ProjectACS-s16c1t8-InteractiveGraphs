setwd("/Users/MengyingLiu/Documents/STAT4249")
library("data.table")
library("dplyr")
library("plyr")
library("ggplot2")
colkeep<-c("ST","INDP","COW","OCCP")
dataa<- fread("/Users/MengyingLiu/Documents/STAT4249/ss13pusa.csv", select=colkeep )  
datab<- fread("/Users/MengyingLiu/Documents/STAT4249/ss13pusb.csv", select=colkeep )  
popdata<-na.omit(rbind(dataa,datab))

          
#add related name
ST.name =read.csv("statename.csv",header = FALSE)
INDP.name = read.csv("indpname.csv",header = TRUE)
COW.name = read.csv("cowname.csv",header = TRUE)
popdata$INDP<-as.factor(popdata$INDP)
popdata$COW<-as.factor(popdata$COW)
popdata<-merge(popdata,INDP.name,by.x = "INDP", by.y = "num")
popdata<-merge(popdata,COW.name,by.x = "COW", by.y = "num")
popdata = mutate(popdata,STname=ST.name[ST,2],
                 STabbr = ST.name[ST,3],
                 Reigion = ST.name[ST,4])

#analyze cow
data<-popdata%>%
      filter(Reigion %in% c(1,2))%>%
      group_by(Reigion)

pop<-table(data$Reigion)

cowcount<-xtabs(~COW+Reigion,data)%>%
  prop.table(margin = 2)*100

cowcount<-ddply(data,.(data$Reigion,data$COWname),nrow)
names(cowcount)<-c("Reigion","Cow","Freq")
cowcount$Freq<-cowcount$Freq/pop[cowcount$Reigion]*100
cowcount<-cowcount[with(cowcount,order(-Freq)),]
ggplot(cowcount, aes(x = factor(Cow), y = Freq,fill = factor(Reigion),color=factor(Reigion))) +   
  geom_bar(position = "dodge", stat="identity")+
  ylab("Percentage") + 
  xlab("Class of Workers") + ggtitle("Comparing class of workers bwteen the westcoast and eastcoast") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


levels(data$COW) <- c("Private for-profit", "Private non-profit", "Local government",
                      "State government", "Federal government", "Self-employed without corporation",
                      "Self-employed incorpoarated", "Working without pay(family business/farm)", "Unemployed")
cowvar<-factor(counts$COW,)