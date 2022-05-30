rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")
library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(itsadug)
library(mgcv)
library(tidyr)           # Simplify R code
library(car)
library(MASS)

#read in data
data <- read.csv('preprocessed_data.csv') 



#same for sleepQuality
for(id in unique(data$subject)){
  respondent_rows <- which(data$subject == id)
  current_day <- 0
  for(row in respondent_rows){
    if((data$assessmentDay[row] != current_day) & (!is.na(data$sleepQuality[row]))){
      sleep_quality <- data$sleepQuality[row]
      current_day <- data$assessmentDay[row]
    } else if((data$assessmentDay[row] == current_day) & (is.na(data$sleepQuality[row]))){
      data$sleepQuality[row] <- sleep_quality
    }
  }
}

#same for sleepDuration
for(id in unique(data$subject)){
  respondent_rows <- which(data$subject == id)
  current_day <- 0
  for(row in respondent_rows){
    if((data$assessmentDay[row] != current_day) & (!is.na(data$sleepDuration[row]))){
      sleep_duration <- data$sleepDuration[row]
      current_day <- data$assessmentDay[row]
    } else if((data$assessmentDay[row] == current_day) & (is.na(data$sleepDuration[row]))){
      data$sleepDuration[row] <- sleep_duration
    }
  }
}
#aaaaaaand for sleepLatency
for(id in unique(data$subject)){
  respondent_rows <- which(data$subject == id)
  current_day <- 0
  for(row in respondent_rows){
    if((data$assessmentDay[row] != current_day) & (!is.na(data$sleepLatency[row]))){
      sleep_latency <- data$sleepLatency[row]
      current_day <- data$assessmentDay[row]
    } else if((data$assessmentDay[row] == current_day) & (is.na(data$sleepLatency[row]))){
      data$sleepLatency[row] <- sleep_latency
    }
  }
}

#aaaaaaand for restednessWakeup
for(id in unique(data$subject)){
  respondent_rows <- which(data$subject == id)
  current_day <- 0
  for(row in respondent_rows){
    if((data$assessmentDay[row] != current_day) & (!is.na(data$restednessWakeup[row]))){
      restedness <- data$restednessWakeup[row]
      current_day <- data$assessmentDay[row]
    } else if((data$assessmentDay[row] == current_day) & (is.na(data$restednessWakeup[row]))){
      data$restednessWakeup[row] <- restedness
    }
  }
}

data$group <- factor(data$group, levels = c("controls", "remitted"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))
data$grInt <- as.factor(paste(data$group, data$intervention, sep = "."))
data$blockPhase <- as.factor(paste(data$phase, data$block, sep = "."))
data$grIntPhase <- as.factor(interaction(data$group, data$intervention, data$phase, drop = TRUE))
data$subjB <- interaction(data$subject, data$block, drop = TRUE)

met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

sc_data <- copy(data)
sc_data[met.vars] <- scale(sc_data[met.vars])



#number of participants so far
length(unique(sc_data$subjB)) #62 subjB (same subject, different block --> viewed as separate)
responses_block <- ddply(sc_data, .(subjB), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))


meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.6%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subjB)) #36
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)) #45
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subjB)) #53

#removing participants with a response rate lower than 60%
pp <- unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)
sc_data <- sc_data[which(sc_data$subjB %in% pp),]



############################# Predicting rumination ######################################
#checking out its distribution
hist(data$ruminating, breaks = 20)

qqnorm(data$ruminating)
qqline(data$ruminating)

#first simple gams
if(FALSE){
  m1 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data)
  
  summary_m1 <- summary(m1)
  
  #refitting with ML for FE comparison
  ml1 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data, method = "ML")
  
  save(m1, ml1, summary_m1, file="m1.rda", compress="xz")
} else {
  load("m1.rda")
}

plot_smooth(m1, view="blockBeepNum", plot_all = "group")

plot_diff(m1, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(m1))
qqline(resid(m1)) #residuals are not super Gaussian

plot(fitted(m1), resid(m1))
abline(h=0) #there is also obvious structure in the residuals



#using a scaled t-distribution
if(FALSE){
  s1 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data, family = "scat")
  
  summary_s1 <- summary(s1)
  
  save(s1, summary_s1, file="s1.rda", compress="xz")
} else {
  load("s1.rda")
}


plot_smooth(s1, view="blockBeepNum", plot_all = "group")

plot_diff(s1, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(s1))
qqline(resid(s1))#the residuals are no more Gaussian than with the normal distribution


# PLOT 2:
plot(fitted(s1), resid(s1))
abline(h=0) #structure is even worse --> stick with normal dist for now


#Extending the model (with normal distribution)
#Negative Affect
if(FALSE){
  m2 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data)
  
  summary_m2 <- summary(m2)
  
  ml2 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data, method = "ML")
  
  save(m2, ml2, summary_m2, file="m2.rda", compress="xz")
} else {
  load("m2.rda")
}

qqnorm(resid(m2))
qqline(resid(m2))

# PLOT 2:
plot(fitted(m2), resid(m2))
abline(h=0) 


compareML(ml1, ml2) #significant --> ml2 better


#Positive Affect
if(FALSE){
  ml3 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) +
               s(blockBeepNum, by = subjB, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  save(ml3, file="ml3.rda", compress="xz")
} else {
  load("ml3.rda")
}


compareML(ml2, ml3) #significant --> ml3 is better


#Sleep Quality
if(FALSE){
  
  m4 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) + s(sleepQuality) +
               s(blockBeepNum, by = subjB, bs="fs", m=1),
             data=sc_data)
  
  summary_m4 <- summary(m4)
  
  ml4 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) + s(sleepQuality) +
               s(blockBeepNum, by = subjB, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  save(m4, summary_m4, ml4, file="m4.rda", compress="xz")
} else {
  load("m4.rda")
}


compareML(ml3, ml4) #significant --> ml4 is better

qqnorm(resid(m4))
qqline(resid(m4))

# PLOT 2:
plot(fitted(m4), resid(m4))
abline(h=0) 

















#################################################################################