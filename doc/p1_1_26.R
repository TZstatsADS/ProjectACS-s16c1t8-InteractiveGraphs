setwd("C:/Original/good stat/2016 spring/Applied data science/project 1")
getwd()
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  colsToKeep <- c("FARP", "ST")
  popDataA <- fread("ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("ss13pusb.csv", select=colsToKeep )
  populData <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(populData, file="populData.RData")
}else{
  load("populData.RData")
} 

populData <- tbl_df(populData) 
ds <-  populData %>%  
  na.omit() %>%
  filter(ST %in%  c(6,41,53,9,23,25,33,34,36,42,44,50)) %>%
  group_by(ST) 
rm(populData)

