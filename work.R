library("dplyr")
library("ggplot2")

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



