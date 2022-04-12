
rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/SART")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/SART")

library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(Hmisc)
library(corrplot)
library(data.table)
library(ggplot2)
library(reshape)
library(ggpubr)
library(lubridate)
library(igraph)
library(qgraph)

#read in data
games <- read_csv('SART_games_110422.csv') 
numbers <- read_csv('SART_numbers_110422.csv') 
questions <- read_csv('SART_questions_110422.csv') 

################################## Inspecting games ###################################
length(unique(games$userID)) #72
length(games$gameSessionID) #3771

games_summary <- ddply(games, .(userID), plyr::summarise,
                       nGames = length(unique(gameSessionID)))
                       
################################# Inspecting numbers ##################################
length(unique(numbers$userID)) #71

numbers_summary <- ddply(numbers, .(userID), plyr::summarise,
                         nGames = length(unique(gameSessionID)),
                         nTrials = length(correct),
                         proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                         meanRT = round(mean(responseTime),2))

max(numbers_summary$nTrials) #21493
mean(numbers_summary$nTrials) #1612
#View(numbers[which(numbers$userID=="148649783"),])                      

################################ Inspecting questions ##################################
length(unique(questions$userID)) #62

questions_summary <- ddply(questions, .(userID), plyr::summarise,
                           nGames = length(unique(gameSessionID)),
                           nQuestions = length(questionID))

unique(questions$answer)
questions_summary2 <- ddply(questions, .(questionID), plyr::summarise,
                            proportionAnswer0 = round(length(questionID[which(answer==0)])/length(questionID),2),
                            proportionAnswer1 = round(length(questionID[which(answer==1)])/length(questionID),2),
                            proportionAnswer2 = round(length(questionID[which(answer==2)])/length(questionID),2),
                            proportionAnswer3 = round(length(questionID[which(answer==3)])/length(questionID),2),
                            proportionAnswer4 = round(length(questionID[which(answer==4)])/length(questionID),2),
                            proportionAnswer5 = round(length(questionID[which(answer==5)])/length(questionID),2))

################################









