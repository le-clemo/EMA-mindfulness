rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)           # Simplify R code
library(huge)
library(languageR)
library(Hmisc)

#read in data
data <- read.csv('ESM/mindcog_v202207/preprocessed_data.csv') 

#Pick response rate cut-off value
cutOff <- 0.5

responses_block <- ddply(data, .(subject), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))

pp <- unique(responses_block[which(responses_block$responseRate >= cutOff),]$subject)
data <- data[which(data$subject %in% pp),]
data <- data[which(data$dayBeepNum < 11),] #three subjects had extra beeps on a day. Since this messes with the detrending code I just removed
#the extra beeps (<15 entries)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
################################################### Hierarchical Clustering #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(data)[colnames(data) == 'meanNA'] <- 'NegativeAffect'
colnames(data)[colnames(data) == 'meanPA'] <- 'PositiveAffect'
colnames(data)[colnames(data) == 'negMax'] <- 'EventUnpleasantness'
colnames(data)[colnames(data) == 'posMax'] <- 'EventPleasantness'
colnames(data)[colnames(data) == 'ruminating'] <- 'Rumination'
colnames(data)[colnames(data) == 'stickiness'] <- 'Stickiness'
colnames(data)[colnames(data) == 'energetic'] <- 'Energy'
colnames(data)[colnames(data) == 'wakeful'] <- 'Wakefulness'
colnames(data)[colnames(data) == 'satisfied'] <- 'Satisfaction'
colnames(data)[colnames(data) == 'down'] <- 'Sadness'
colnames(data)[colnames(data) == 'anxious'] <- 'Anxiety'
colnames(data)[colnames(data) == 'restless'] <- 'Restlessness'
colnames(data)[colnames(data) == 'irritated'] <- 'Irritation'
colnames(data)[colnames(data) == 'distracted'] <- 'Distraction'
colnames(data)[colnames(data) == 'stressed'] <- 'Stress'
colnames(data)[colnames(data) == 'listless'] <- 'Listlessness'
colnames(data)[colnames(data) == 'thoughtsPleasant'] <- 'ThoughtPleasantness'
colnames(data)[colnames(data) == 'negIntensity'] <- 'NegativeEventIntensity'
colnames(data)[colnames(data) == 'posIntensity'] <- 'PositiveEventIntensity'
colnames(data)[colnames(data) == 'sleepQuality'] <- 'SleepQuality'
colnames(data)[colnames(data) == 'sleepLatency'] <- 'SleepLatency'
colnames(data)[colnames(data) == 'sleepDuration'] <- 'SleepDuration'
colnames(data)[colnames(data) == 'restednessWakeup'] <- 'Restedness'
colnames(data)[colnames(data) == 'restOfDayPos'] <- 'OutlookPositivity'


metricCols <- c('Rumination', 'Stickiness', 'NegativeAffect', 'PositiveAffect',
                'Wakefulness', 'Sadness', 'Satisfaction',
                'Irritation', 'Energy', 'Restlessness', 'Anxiety', 'Stress',
                'Listlessness', 'ThoughtPleasantness', 'Distraction', 'OutlookPositivity',
                'EventPleasantness', 'PositiveEventIntensity',
                'EventUnpleasantness', 'NegativeEventIntensity',
                "SleepQuality", "Restedness")
#we run into convergence issues when we include too many nodes. Especially when trying to explore the networks of
#group + intervention + phase...
#for that reason we perform hierarchical clustering to find highly correlated variables
#we have already done this for the mixed effects models. However, because we use much smaller parts of the data per
#network, we need to simplify even more for network analysis.

dat_clust <- copy(data)
dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
dat_clust <- dat_clust[,metricCols]
dat_clust <- dat_clust[complete.cases(dat_clust), ]

# goldbricker(dat_clust, p=.01, threshold = 0.5) #doesn't suggest reductions...

#only excluding sumNA and sumPA
collin.fnc(dat_clust[,-c(3, 4)])$cnumber #~29.06 --> problematic collinearity
varCors <- round(varclus(as.matrix(dat_clust[,-c(3, 4)]))$sim, 2)

pdf(width = 10, height = 6,
    file = "Figures/hierarchicalClustering.pdf")
plot(varclus(as.matrix(dat_clust[,-c(3, 4)])))
dev.off()

#for the mixed-effects models we only kept the following
collin.fnc(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])$cnumber #~16.3
plot(varclus(as.matrix(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])))

#for the network models, we ideally want to keep more of the PA and NA variables, not just their aggregate
collin.fnc(dat_clust[,-c(2, 3, 4, 12, 13, 14, 16, 18, 20, 21, 22, 23, 24)])$cnumber #~23.4
plot(varclus(as.matrix(dat_clust[,-c(2, 3, 4, 12, 13, 14, 16, 18, 20, 21, 22, 23, 24)])))
