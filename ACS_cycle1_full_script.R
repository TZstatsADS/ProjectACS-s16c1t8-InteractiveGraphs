
library("data.table")
library("dplyr")
library("plyr")
library("ggplot2")
library("tidyr")
library("circlize")
library("plotly")
library("readr")
library("gsub")
library("choroplethr")


#migration&industry
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
#flow matrix
df1<-structure(list(order = rep(1:5),
                    rgb =c("210,150,12","125,175,0","255,219,0","100,146,125","73,255,233"),
                    region = c("other","northeast","midwest","south","west")),
               .Names= c("order","rgb","region"),row.names=c(NA,-5L),class="data.frame")

m <- as.matrix.data.frame(movecount)
dimnames(m) <- list(orig = df1$region, work = df1$region)
m[m<=quantile(m,0.01)]<-0
#sort regions and create colours

df1 <- df1 %>% separate(rgb, c("r","g","b")) %>% mutate(col = rgb(r, g, b, max=255), 
                                                        max = rowSums(m)+colSums(m))
#plot using chordDiagram

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

plot_ly(indpresult, x = factor(Indpname),y = Freq, size = Freq,mode = "markers",
        text = paste("Industry: ", Indpname),color = factor(Regionname)) %>%
  layout(xaxis = a, yaxis = b, showlegend = TRUE)


rm(list=ls())

a<-read.csv('pusa1.csv')
all<-a

txt<- 'pusa1.csv'
txtsave<-'a1'

for (i in 2:11){
  filename<-gsub('1', i,txt,perl=TRUE)
  savename<-gsub('1',i,txtsave,perl=TRUE)
  assign(savename,read.csv(filename))
  
}

print


#Income analysis


colkeep<-c('ST','MIGSP','AGEP')
dataa<- fread("pusa.csv", select=colkeep )  
datab<- fread("pusb.csv", select=colkeep )

popdata<-na.omit(rbind(dataa,datab))

people <- popdata %>%
  na.omit() %>%
  filter(MIGSP %in% c(6,36))%>%
  group_by(ST) #group by state

c_ny=0
c_ca=0
for (i in 1:52){
  a=0
  b_ny=0
  b_ca=0
  
  a<- people %>%
    filter(ST %in% c(i))
  b_ny<-a %>%
    filter(MIGSP %in% c(36))
  b_ca<-a %>%
    filter(MIGSP %in% c(6))
  c_ny[i]=nrow(b_ny)
  c_ca[i]=nrow(b_ca)
}

result=data.frame(c(1:52),c_ca,c_ny)
write.csv(result,'migration.csv')
rest<-read.csv('rest.csv')
number<-read.csv('number.csv')
a<-c(0,0)
b=0
for (k in c(1:51)){
  
  kk=result[k,2]/(result[k,3]+result[k,2])
  n=nrow(rest%>%filter(X.4 %in%number[k,1]))
  
  
  if (n!=0){
    n_ca=floor(kk*n)
    n_ny=n-n_ca
    for (m in c(1:n_ca)){
      a<-rbind(a,c(0,1))
    }
    for (t in c(1:n_ny)){
      a<-rbind(a,c(1,0))
    }
  }
}
write.csv(a,'finally.csv')


for (k in c(1:52)){
  k=result[k,2]/result[k,3]
  rest%>%filter(X.4 %in%c(i))
  
  
}

data<-peopleWAGP%>%
  filter(X.4 %in% c(i))%>%
  group_by(ST)










#define state code
stateCode = "ST,State
1,Alabama/AL
2,Alaska/AK
4,Arizona/AZ
5,Arkansas/AR
6,California/CA
8,Colorado/CO
9,Connecticut/CT
10,Delaware/DE
11,DistrictofColumbia/DC
12,Florida/FL
13,Georgia/GA
15,Hawaii/HI
16,Idaho/ID
17,Illinois/IL
18,Indiana/IN
19,Iowa/IA
20,Kansas/KS
21,Kentucky/KY
22,Louisiana/LA
23,Maine/ME
24,Maryland/MD
25,Massachusetts/MA
26,Michigan/MI
27,Minnesota/MN
28,Mississippi/MS
29,Missouri/MO
30,Montana/MT
31,Nebraska/NE
32,Nevada/NV
33,NewHampshire/NH
34,NewJersey/NJ
35,NewMexico/NM
36,NewYork/NY
37,NorthCarolina/NC
38,NorthDakota/ND
39,Ohio/OH
40,Oklahoma/OK
41,Oregon/OR
42,Pennsylvania/PA
44,RhodeIsland/RI
45,SouthCarolina/SC
46,SouthDakota/SD
47,Tennessee/TN
48,Texas/TX
49,Utah/UT
50,Vermont/VT
51,Virginia/VA
53,Washington/WA
54,WestVirginia/WV
55,Wisconsin/WI
56,Wyoming/WY
72,PuertoRico/PR"
statecode <- fread(stateCode)
write.csv(statecode,'statecode.csv')

peopleWAGP <- left_join(people , statecode, by.x='ST')

#select state
data<-peopleWAGP%>%
  filter(ST %in% c(6,36))%>%
  group_by(ST)
#select age
data<-data%>%
  filter(AGEP %in% c(20:30))


plotdata_ny<-data%>%
  filter(ST %in% c(36))

df_ny <- setNames(data.frame(plotdata_ny), c("ST", "ADJINC", "AGEP",'WAGP','WKHP','State'))

d_ny <- df_ny[sample(nrow(df_ny), 1000), ]
plot_ly(df_ny, x = AGEP, y = WKHP, text = paste("INCOME ", WAGP),
        mode = "markers", color = WAGP, size = WAGP)



plotdata_ca<-data%>%
  filter(ST %in% c(6))

df_ca <- setNames(data.frame(plotdata_ca), c("ST", "ADJINC", "AGEP",'WAGP','WKHP','State'))


d_ca <- df_ca[sample(nrow(df_ca), 1000), ]
plot_ly(d_ca, x = AGEP, y = WKHP, text = paste("INCOME ", WAGP),
        mode = "markers", color = WAGP, size = WAGP)


#Income analysis



colkeep<-c('ST','ADJINC',"OIP","SEMP",'WAGP','PINCP','PAP','RETP','SEX','AGEP','INDP','SSIP')
dataa<- fread("pusa.csv", select=colkeep )
datab<- fread("pusb.csv", select=colkeep )
test<- fread("pusb.csv", select='PWGTP' )
popdata<-na.omit(rbind(dataa,datab))

people <- popdata %>%
  na.omit() %>%
  filter(popdata,WAGP!=000000) %>% #exclude no salary person
  filter(popdata,WAGP!='bbbbbb') %>% # exclude N/A
  group_by(ST) #group by state


#define state code
stateCode = "ST,State
1,Alabama/AL
2,Alaska/AK
4,Arizona/AZ
5,Arkansas/AR
6,California/CA
8,Colorado/CO
9,Connecticut/CT
10,Delaware/DE
11,DistrictofColumbia/DC
12,Florida/FL
13,Georgia/GA
15,Hawaii/HI
16,Idaho/ID
17,Illinois/IL
18,Indiana/IN
19,Iowa/IA
20,Kansas/KS
21,Kentucky/KY
22,Louisiana/LA
23,Maine/ME
24,Maryland/MD
25,Massachusetts/MA
26,Michigan/MI
27,Minnesota/MN
28,Mississippi/MS
29,Missouri/MO
30,Montana/MT
31,Nebraska/NE
32,Nevada/NV
33,NewHampshire/NH
34,NewJersey/NJ
35,NewMexico/NM
36,NewYork/NY
37,NorthCarolina/NC
38,NorthDakota/ND
39,Ohio/OH
40,Oklahoma/OK
41,Oregon/OR
42,Pennsylvania/PA
44,RhodeIsland/RI
45,SouthCarolina/SC
46,SouthDakota/SD
47,Tennessee/TN
48,Texas/TX
49,Utah/UT
50,Vermont/VT
51,Virginia/VA
53,Washington/WA
54,WestVirginia/WV
55,Wisconsin/WI
56,Wyoming/WY
72,PuertoRico/PR"
statecode <- fread(stateCode)


peopleWAGP <- left_join(people , statecode, by.x='ST')




data<-popdata%>%
  filter(region %in% c(1:52))%>%
  group_by(region)

pop<-table(data$region)

a=0
a[c(1:56)]=0


for (i in c(1:56)){
  a[i]=mean(filter(peopleWAGP,ST==i)$WAGP)
}


write.csv(a,'WAGP.csv')

df <- read.csv("2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "WAGP", WAGP ))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_ly(df, z = WAGP, text = hover, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = WAGP, colors = 'Blues',
        marker = list(line = l), colorbar = list(title = "USD")) %>%
  layout(title = 'Everage Income for Every State', geo = g)



#Income analysis



colkeep<-c('ST','ADJINC','WAGP','WKHP','AGEP')
dataa<- fread("pusa.csv", select=colkeep )
datab<- fread("pusb.csv", select=colkeep )

popdata<-na.omit(rbind(dataa,datab))

people <- popdata %>%
  na.omit() %>%
  filter(popdata,WAGP!=000000) %>% #exclude no salary person
  filter(popdata,WAGP!='bbbbbb') %>% # exclude N/A
  group_by(ST) #group by state


#define state code
stateCode = "ST,State
1,Alabama/AL
2,Alaska/AK
4,Arizona/AZ
5,Arkansas/AR
6,California/CA
8,Colorado/CO
9,Connecticut/CT
10,Delaware/DE
11,DistrictofColumbia/DC
12,Florida/FL
13,Georgia/GA
15,Hawaii/HI
16,Idaho/ID
17,Illinois/IL
18,Indiana/IN
19,Iowa/IA
20,Kansas/KS
21,Kentucky/KY
22,Louisiana/LA
23,Maine/ME
24,Maryland/MD
25,Massachusetts/MA
26,Michigan/MI
27,Minnesota/MN
28,Mississippi/MS
29,Missouri/MO
30,Montana/MT
31,Nebraska/NE
32,Nevada/NV
33,NewHampshire/NH
34,NewJersey/NJ
35,NewMexico/NM
36,NewYork/NY
37,NorthCarolina/NC
38,NorthDakota/ND
39,Ohio/OH
40,Oklahoma/OK
41,Oregon/OR
42,Pennsylvania/PA
44,RhodeIsland/RI
45,SouthCarolina/SC
46,SouthDakota/SD
47,Tennessee/TN
48,Texas/TX
49,Utah/UT
50,Vermont/VT
51,Virginia/VA
53,Washington/WA
54,WestVirginia/WV
55,Wisconsin/WI
56,Wyoming/WY
72,PuertoRico/PR"
statecode <- fread(stateCode)

peopleWAGP <- left_join(people , statecode, by.x='ST')

#select state
data<-peopleWAGP%>%
  filter(ST %in% c(6,36))%>%
  group_by(ST)
#select age
data<-data%>%
  filter(AGEP %in% c(20:30))


plotdata_ny<-data%>%
  filter(ST %in% c(36))

df_ny <- setNames(data.frame(plotdata_ny), c("ST", "ADJINC", "AGEP",'WAGP','WKHP','State'))

d_ny <- df_ny[sample(nrow(df_ny), 1000), ]
plot_ly(df_ny, x = AGEP, y = WKHP, text = paste("INCOME ", WAGP),
        mode = "markers", color = WAGP, size = WAGP)



plotdata_ca<-data%>%
  filter(ST %in% c(6))

df_ca <- setNames(data.frame(plotdata_ca), c("ST", "ADJINC", "AGEP",'WAGP','WKHP','State'))


d_ca <- df_ca[sample(nrow(df_ca), 1000), ]
plot_ly(d_ca, x = AGEP, y = WKHP, text = paste("INCOME ", WAGP),
        mode = "markers", color = WAGP, size = WAGP)


#work

#colsToKeep = c("ST", "JWMNP", "JWTR", "WKHP", "WKW", "JWAP", "JWDP", "PWGTP")
#pusa <- fread("D:/CU/4249_Data/Project_1/ss13pusa.csv", select = colsToKeep)
#pusb <- fread("D:/CU/4249_Data/Project_1/ss13pusb.csv", select = colsToKeep)
#WorkData <- rbind(pusa, pusb)
#rm(pusa, pusb)
#save(WorkData, file = "WorkData.RData")

load("WorkData.RData")

ST.name =read.csv("statename.csv",header = FALSE)
ST.name[,4] <- ifelse(ST.name[,4] == 1, "Northeast", ST.name[,4])
ST.name[,4] <- ifelse(ST.name[,4] == 2, "Middle", ST.name[,4])
ST.name[,4] <- ifelse(ST.name[,4] == 3, "South", ST.name[,4])
ST.name[,4] <- ifelse(ST.name[,4] == 4, "West", ST.name[,4])
Work <- mutate(WorkData, Region = ST.name[ST,4]) %>%
  na.omit() %>%
  group_by(Region)

#Means of transportation
Means0 <- c(0, "Car", "Bus", "Streetcar", "Subway", "Railroad", "Ferryboat",
            "Taxicab", "Motorcycle", "Bicycle", "Walked", "Work at home", "other")
ggplot(Work, aes(JWTR, group = Region)) + 
  geom_bar(aes(colour = Region, fill = Region), alpha = 0.7) +
  xlab("Means") + ylab("Count") + ggtitle("Means of transportation") + 
  scale_x_continuous(breaks = seq(0, 12, 1), labels = Means0)

Means1 <- Means0[c(1:12)]
Transport1 <- select(Work, JWTR, Region) %>%
  filter(JWTR != 1)
ggplot(Transport1, aes(JWTR, group = Region)) + 
  geom_bar(aes(colour = Region, fill = Region), alpha = 0.7) +
  xlab("Means") + ylab("Count") + ggtitle("Means of transportation(Remove car)") + 
  scale_x_continuous(breaks = seq(0, 11, 1), labels = Means1)

Transport2 <- select(Work, ST, JWTR) %>%
  filter(ST %in% c(6, 36))
ggplot(Transport2, aes(JWTR, group = ST)) + 
  geom_bar(aes(colour = Region, fill = Region), alpha = 0.7) +
  xlab("Means") + ylab("Count") + ggtitle("Means of transportation(NY and CA)") + 
  scale_x_continuous(breaks = seq(0, 12, 1), labels = Means0)


#marriage
#setwd("C:/Original/good stat/2016 spring/Applied data science/project 1")
#getwd()



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




