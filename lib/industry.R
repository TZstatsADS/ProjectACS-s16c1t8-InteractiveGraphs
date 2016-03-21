setwd("/Users/MengyingLiu/Documents/STAT4249")
library("data.table")
library("dplyr")
library("plyr")
library("ggplot2")

colkeep<-c("ST","INDP","COW","OCCP","POBP","POWSP")
dataa<- fread("/Users/MengyingLiu/Documents/STAT4249/ss13pusa.csv", select=colkeep )  
datab<- fread("/Users/MengyingLiu/Documents/STAT4249/ss13pusb.csv", select=colkeep )  
popdata<-na.omit(rbind(dataa,datab))

          
#add related name
ST.name =read.csv("statename.csv",header = FALSE)
INDP.name = read.csv("indpname.csv",header = TRUE)
COW.name = read.csv("cowname.csv",header = TRUE)
Region.name = read.csv("Regionname.csv",header = TRUE)
popdata$INDP<-as.factor(popdata$INDP)
popdata$COW<-as.factor(popdata$COW)


popdata<-merge(popdata,INDP.name,by.x = "INDP", by.y = "num")
popdata<-merge(popdata,COW.name,by.x = "COW", by.y = "num")
popdata = mutate(popdata,STname=ST.name[ST,2],
                 STabbr = ST.name[ST,3],
                 Region = ST.name[ST,4])
popdata<-merge(popdata,Region.name,by.x = "Region",by.y ="Region")

#MOVE

popdata = mutate(popdata,
                 BirthRegion = ST.name[POBP,4])
popdata = mutate(popdata,
                 WorkRegion = ST.name[POWSP,4])

movecount<-xtabs(~BirthRegion+ WorkRegion,popdata);movecount

movecount<-as.matrix.data.frame(movecount)

#the cord diagram of people flow 
library("dplyr")
#flow matrix
df1<-structure(list(order = rep(1:5),
                rgb =c("210,150,12","125,175,0","255,219,0","100,146,125","73,255,233"),
                region = c("other","northeast","midwest","south","west")),
               .Names= c("order","rgb","region"),row.names=c(NA,-5L),class="data.frame")

m <- as.matrix.data.frame(movecount)
dimnames(m) <- list(orig = df1$region, work = df1$region)
m[m<=quantile(m,0.01)]<-0
#sort regions and create colours
library("tidyr")
df1 <- df1 %>% separate(rgb, c("r","g","b")) %>% mutate(col = rgb(r, g, b, max=255), 
                                                        max = rowSums(m)+colSums(m))
#plot using chordDiagram
library("circlize")
circos.clear()
par(mar = rep(0, 4), cex=0.9)
circos.par(start.degree = 90, gap.degree = 3)
chordDiagram(x = m, directional = 1, order = df1$region, grid.col = df1$col, 
             transparency = 0.25,preAllocateTracks = list(track.height = 0.1))


#analyze cow
data<-popdata%>%
      #filter(ST %in% c(6,36))%>%
      group_by(Region)
pop<-table(data$Region)
pop
cowcount<-ddply(data,.(data$Region,data$COWname,data$Regionname),nrow)
names(cowcount)<-c("Region","Cow","Regionname","Freq")
cowcount$Freq<- cowcount$Freq / pop[cowcount$Region]*100
cowcount<-cowcount[with(cowcount,order(-Freq)),]
ggplot(cowcount, aes(x = Cow, y = Freq,fill = factor(Regionname),color=factor(Regionname))) +   
  geom_bar(position = "dodge", stat="identity")+
  ylab("Percentage") + 
  xlab("Class of Workers") + ggtitle("Comparing class of workers among different regions") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

data1<-popdata%>%
    filter(ST %in% c(6,36))%>%
    group_by(Region)
pop1<-table(data1$Region)
pop1
cowcount1<-ddply(data1,.(data1$Region,data1$COWname,data1$STname),nrow)
names(cowcount1)<-c("Region","Cow","STname","Freq")
cowcount1$Freq<- cowcount1$Freq / pop1[cowcount1$Region/3+2/3]*100
cowcount1<-cowcount1[with(cowcount1,order(-Freq)),]
ggplot(cowcount1, aes(x = Cow, y = Freq,fill = factor(STname),color=factor(STname))) +   
  geom_bar(position = "dodge", stat="identity")+
  ylab("Percentage") + 
  xlab("Class of Workers") + ggtitle("Comparing class of workers between the California and New York") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#Analyze Industry
data1<-popdata%>%
      group_by(Regionname)
pop1<-table(data1$Region);pop1
indpcount<-ddply(data1,.(data1$Region,data1$Regionname,data1$INDPname),nrow)
names(indpcount)<-c("Region","Regionname","Indpname","Freq")
indpcount$Freq<-indpcount$Freq/pop1[indpcount$Region]*100
indpcount<-indpcount[with(indpcount,order(-Freq)),]
indp1<-indpcount[indpcount$Region ==1,]
indp2<-indpcount[indpcount$Region ==2,]
indp3<-indpcount[indpcount$Region ==3,]
indp4<-indpcount[indpcount$Region ==4,]
indpresult<-rbind(indp1[1:10,],indp2[1:10,],indp3[1:10,],indp4[1:10,])
indpresult$Freq<-round(indpresult$Freq,digits= 2)


a <- list(
  title = "\n\n\n\nIndustry\n",
  showticklabels = FALSE,
  tickangle = 45,
  size = 12
)

b<-list(title = "Freq")



dev.new(width=5, height=4)
library(plotly)
plot_ly(indpresult, x = factor(Indpname),y = Freq, size = Freq,mode = "markers",
        text = paste("Industry: ", Indpname),color = factor(Regionname)) %>%
  layout(xaxis = a, yaxis = b, showlegend = TRUE)

