library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

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

#Travel time to work
ggplot(Work, aes(JWMNP, group = Region)) + 
  geom_bar(binwidth = 10, aes(colour = Region, fill = Region), alpha = 0.7) +
  xlab("Time") + ylab("Count") + ggtitle("Travel time to work")

#Hours per week
ggplot(Work, aes(WKHP, group = Region)) + 
  geom_bar(binwidth = 10, aes(colour = Region, fill = Region), alpha = 0.7) +
  xlab("Hours") + ylab("Count") + ggtitle("Hours per week")

#Weeks worked
ggplot(Work, aes(WKHP, group = Region)) + 
  geom_bar(binwidth = 10, aes(colour = Region, fill = Region), alpha = 0.7) +
  xlab("Weeks") + ylab("Count") + ggtitle("Weeks worked")

#Time of arrival
data <- as.data.frame(prop.table(table(Work$JWAP, Work$Region)))
data$margin <- prop.table(table(Work$JWAP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(Work$JWAP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Time", y = "Frequency", title = "Time of arrival") +
  scale_x_continuous(breaks = seq(0, 1, 1/12), 
          labels=c("0", "2", "4", "6", "8", "10", "12", "14", "16", "18", "20", "22", "24"))

#Time of departure
data <- as.data.frame(prop.table(table(Work$JWDP, Work$Region)))
data$margin <- prop.table(table(Work$JWDP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(Work$JWDP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Time", y = "Frequency", title = "Time of departure") +
  scale_x_continuous(breaks = seq(0, 1, 1/12), 
         labels=c("0", "2", "4", "6", "8", "10", "12", "14", "16", "18", "20", "22", "24"))

#Travel time to work
RegionNames <- c("Northeast", "Middle", "South", "West")
Table <- matrix(0, 18, 4)
colnames(Table) <- RegionNames
for (i in 1:4){
  temp <- select(Work, JWMNP, Region) %>%
          filter(Region == RegionNames[i])
  h0 <- hist(temp$JWMNP, breaks = seq(0,180,10), plot = FALSE)
  Table[,i] <- h0$counts
}
Table <- as.table(Table)

data <- as.data.frame(prop.table(Table))
h0 <- hist(Work$JWMNP, breaks = seq(0,180,10), plot = FALSE)
data$margin <- rep(h0$density, 4)
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:17]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Time", y = "Frequency", title = "Travel time to work") +
  scale_x_continuous(breaks = seq(0, 1, 1/8), 
              labels=c("0", "3", "6", "9", "12", "15", "18", "21", "24"))



