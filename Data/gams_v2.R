rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data") #/ESM/mindcog_v202204

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
data <- read.csv('merged_data.csv') 



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
data$subject <- as.factor(data$subject)
data$subjB <- interaction(data$subject, data$block, drop = TRUE)

#creating rumination measures minus baseline
data$ruminating_minBase <- NA
for(id in unique(data$subject)){
  for(b in 1:2){
    pre_rows <- which((data$subject == id) & (data$phase=="pre") & (data$block==b))
    s_rows <- which((data$subject == id) & (data$block==b))
    baselineMean <- mean(data$ruminating[pre_rows], na.rm=TRUE)
    
    if(is.na(baselineMean)){
      baselineMean <- 0
    }
    
    data$ruminating_minBase[s_rows] <- round(data$ruminating[s_rows] - baselineMean, 2)
  }
}


met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
              "ruminating_minBase")

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

############################################## ruminating minus baseline average models ######################################


if(FALSE){
  r0 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r0 <- summary(r0)
  
  #refitting with ML for FE comparison
  r_ml0 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r0, r_ml0, summary_r0, file="models/r0.rda", compress="xz")
} else {
  load("models/r0.rda")
}


if(FALSE){
  r1 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r1 <- summary(r1)
  
  #refitting with ML for FE comparison
  r_ml1 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  save(r1, r_ml1, summary_r1, file="models/r1.rda", compress="xz")
} else {
  load("models/r1.rda")
}


compareML(r_ml0, r_ml1) #significant --> r1 preferred

plot_smooth(r1, view="blockBeepNum", plot_all = "group")

plot_diff(r1, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(r1))
qqline(resid(r1)) #residuals are not super Gaussian

plot(fitted(r1), resid(r1), col="blue")
#points(fitted(r0), resid(r0), col="red")
abline(h=0) #there is also obvious structure in the residuals



#Extending the model (with normal distribution)
#Negative Affect
if(FALSE){
  r2 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r2 <- summary(r2)
  
  r_ml2 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  save(r2, r_ml2, summary_r2, file="models/r2.rda", compress="xz")
} else {
  load("models/r2.rda")
}

compareML(r_ml1, r_ml2)

plot_smooth(r2, view="blockBeepNum", plot_all = "group")

plot_diff(r2, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(r2))
qqline(resid(r2)) #residuals are not super Gaussian

plot(fitted(r2), resid(r2), col="blue")
points(fitted(r1), resid(r1), col = "red")
abline(h=0) #there is also obvious structure in the residuals



#Positive Affect
if(FALSE){
  r3 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data)
  
  summary_r3 <- summary(r3)
  
  rl3 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  
  save(r3, rl3, file="models/r3.rda", compress="xz")
} else {
  load("models/r3.rda")
}

compareML(r_ml2, rl3) #significant --> r3 preferred

plot_smooth(r3, view="blockBeepNum", plot_all = "group")

plot_diff(r3, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(r3))
qqline(resid(r3)) #residuals are not super Gaussian

plot(fitted(r3), resid(r3))
abline(h=0) #there is also obvious structure in the residuals


#Sleep Quality
if(FALSE){
  r4 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r4 <- summary(r4)
  
  r_ml4 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) + s(sleepQuality) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  save(r4, summary_r4, r_ml4, file="models/r4.rda", compress="xz")
} else {
  load("models/r4.rda")
}

plot_smooth(r4, view="blockBeepNum", plot_all = "group")

plot_diff(r4, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

compareML(rl3, r_ml4) #significant --> r4 preferred


#adding negIntensity
if(FALSE){
  r5 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) + s(negIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r5 <- summary(r5)
  
  r_ml5 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
                 s(sumNA) + s(sumPA) + s(sleepQuality) + s(negIntensity) +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r5, summary_r5, r_ml5, file="models/r5.rda", compress="xz")
} else {
  load("models/r5.rda")
}

plot_smooth(r5, view="blockBeepNum", plot_all = "group")

plot_diff(r5, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

compareML(r_ml4, r_ml5) #significant --> r5 preferred

#adding posIntensity
if(FALSE){
  
  r6 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) + s(negIntensity) + s(posIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r6 <- summary(r6)
  
  r_ml6 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
                 s(sumNA) + s(sumPA) + s(sleepQuality) + s(negIntensity) + s(posIntensity) +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r6, summary_r6, r_ml6, file="models/r6.rda", compress="xz")
} else {
  load("models/r6.rda")
}

plot_smooth(r6, view="blockBeepNum", plot_all = "group")

plot_diff(r6, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

plot(fitted(r5), resid(r5), col = "blue")
points(fitted(r6), resid(r6), col = "red")
abline(h=0)

compareML(r_ml5, r_ml6) #significant --> r6 preferred

#adding interaction betwwen PA and NA
if(FALSE){
  r7 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + s(negIntensity) + s(posIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r7 <- summary(r7)
  
  r_ml7 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
                 s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + s(negIntensity) + s(posIntensity) +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r7, summary_r7, r_ml7, file="models/r7.rda", compress="xz")
} else {
  load("models/r7.rda")
}

plot_smooth(r7, view="blockBeepNum", plot_all = "group")

plot_diff(r7, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

compareML(r_ml6, r_ml7) #significant --> r7 preferred

#adding interaction betwwen neg- and posIntensity
if(FALSE){
  r8 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r8 <- summary(r8)
  
  r_ml8 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
                 s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                 s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r8, summary_r8, r_ml8, file="models/r8.rda", compress="xz")
} else {
  load("models/r8.rda")
}

plot_smooth(r8, view="blockBeepNum", plot_all = "group")

plot_diff(r8, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

compareML(r_ml7, r_ml8) #significant

plot(fitted(r7), resid(r7), col = "blue")
points(fitted(r8), resid(r8), col = "red")
abline(h=0)

#adding stickiness
if(FALSE){
  r9 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r9 <- summary(r9)
  
  r_ml9 <- gam(ruminating_minBase ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
                 s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                 s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r9, summary_r9, r_ml9, file="models/r9.rda", compress="xz")
} else {
  load("models/r9.rda")
}

compareML(r_ml8, r_ml9)

plot_smooth(r8, view="blockBeepNum", plot_all = "group")
plot_smooth(r9, view="blockBeepNum", plot_all = "group")

plot_diff(r9, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

r9_pred <- predict(r9)

mean(r9_pred)
mean(sc_data$ruminating_minBase, na.rm = TRUE)
























############################################ Models with unchanged ruminating ############################################
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
  
  save(m1, ml1, summary_m1, file="models/m1.rda", compress="xz")
} else {
  load("models/m1.rda")
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
  
  save(s1, summary_s1, file="models/s1.rda", compress="xz")
} else {
  load("models/s1.rda")
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
  
  save(m2, ml2, summary_m2, file="models/m2.rda", compress="xz")
} else {
  load("models/m2.rda")
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
  
  save(ml3, file="models/ml3.rda", compress="xz")
} else {
  load("models/ml3.rda")
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
  
  save(m4, summary_m4, ml4, file="models/m4.rda", compress="xz")
} else {
  load("models/m4.rda")
}


compareML(ml3, ml4) #significant --> ml4 is better

qqnorm(resid(m4))
qqline(resid(m4))

# PLOT 2:
plot(fitted(m4), resid(m4))
abline(h=0) 

















#################################################################################