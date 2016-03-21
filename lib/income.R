
rm(list=ls())
install.packages('readr')
library(readr)
install.packages('gsub')
library(gsub)

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

library("data.table")
library("dplyr")
library("plyr")
library("ggplot2")
library('plotly')

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

library("data.table")
library("dplyr")
library("plyr")
library("ggplot2")
library(plotly)

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

library("data.table")
library("dplyr")
library("plyr")
library("ggplot2")
library('plotly')

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



