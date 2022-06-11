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

#and for sleepLatency
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


#add day of the week
data$weekday <- weekdays(strptime(data$mindcog_db_open_from, "%Y-%m-%d %H:%M:%S"))
data$weekday <- as.factor(data$weekday)


#creating variables minus baseline means per subject
met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

#in addition we create a new list which includes both the changed and unchanged met.vars for scaling later on
scale.vars <- c(rep(NA, length(met.vars)*2))
i = 0
for(v in met.vars){
  new_var <- paste(v, "_diff", sep = "")
  data[[new_var]] <- NA
  i = i+1
  scale.vars[[i]] <- v
  i = i+1
  scale.vars[[i]] <- new_var
  
  for(id in unique(data$subject)){
    for(b in 1:2){
      pre_rows <- which((data$subject == id) & (data$phase=="pre") & (data$block==b))
      s_rows <- which((data$subject == id) & (data$block==b))
      baselineMean <- mean(data[[v]][pre_rows], na.rm=TRUE)
      
      if(is.na(baselineMean)){
        baselineMean <- 0
      }
      
      data[[new_var]][s_rows] <- round(data[[v]][s_rows] - baselineMean, 2)
    }
  }  
}

#creating a scaled version of data
sc_data <- copy(data)
sc_data[scale.vars] <- scale(sc_data[scale.vars])


#number of participants so far
length(unique(sc_data$subjB)) #66 subjB (same subject, different block --> viewed as separate)
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

responses_subject <- ddply(sc_data, .(subject), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))

meanResponseRate_subject <- mean(responses_subject$responseRate) #the mean response rate is ~66.9%
length(unique(responses_subject[which(responses_subject$responseRate >= meanResponseRate_block),]$subject)) #20
length(unique(responses_subject[which(responses_subject$responseRate >= 0.6),]$subject)) #26
length(unique(responses_subject[which(responses_subject$responseRate >= 0.5),]$subject)) #33




#removing participants with a response rate lower than 60%
pp <- unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)
sc_data <- sc_data[which(sc_data$subjB %in% pp),]
#sc_data <- sc_data[which(is.na(sc_data$mindcog_db_non_response)),]



############################# Predicting rumination ######################################
#checking out its distribution
hist(data$ruminating, breaks = 20)
qqnorm(data$ruminating)
qqline(data$ruminating)

#also the change scores
hist(data$ruminating_diff, breaks = 20)
qqnorm(data$ruminating_diff)
qqline(data$ruminating_diff) #change scores are more normally distributed

############################################## ruminating minus baseline average models ######################################


if(FALSE){
  r0 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r0 <- summary(r0)
  
  #refitting with ML for FE comparison
  r_ml0 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                 s(blockBeepNum, by = subject, bs="fs", m=1),
               data=sc_data, method = "ML")
  
  save(r0, r_ml0, summary_r0, file="models/r0.rda", compress="xz")
} else {
  load("models/r0.rda")
}


if(FALSE){
  r1 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r1 <- summary(r1)
  
  #refitting with ML for FE comparison
  r_ml1 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r2 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r2 <- summary(r2)
  
  r_ml2 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r3 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data)
  
  summary_r3 <- summary(r3)
  
  rl3 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r4 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r4 <- summary(r4)
  
  r_ml4 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r5 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) + s(negIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r5 <- summary(r5)
  
  r_ml5 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  
  r6 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) + s(negIntensity) + s(posIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r6 <- summary(r6)
  
  r_ml6 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r7 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + s(negIntensity) + s(posIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r7 <- summary(r7)
  
  r_ml7 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r8 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r8 <- summary(r8)
  
  r_ml8 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
  r9 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_r9 <- summary(r9)
  
  r_ml9 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
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
mean(sc_data$ruminating_diff, na.rm = TRUE)












if(FALSE){
  rMax <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence * thoughtsTime +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(listless) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_rMax <- summary(rMax)
  
  rMax_ML <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence * thoughtsTime +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(listless) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(rMax, summary_rMax, rMax_ML, file="models/rMax.rda", compress="xz")
} else {
  load("models/rMax.rda")
}
 #rMax leads to a matrix that "is either rank-deficient or indefinite" --> likely too complex for the amount of (very noisy) data we have


# rMax_weekday <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence * thoughtsTime *
#                       weekday +
#                  s(blockBeepNum, by=group) +
#                  s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
#                  s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
#                  s(listless) +
#                  s(blockBeepNum, by = subject, bs="fs", m=1),
#                data=sc_data, method = "ML")

#minus interaction thoughtsTime:phase
if(FALSE){
  bw1 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence + thoughtsTime +
               thoughtsTime:group + thoughtsTime:intervention +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(listless) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  
  save(bw1, file="models/bw1.rda", compress="xz")
} else {
  load("models/bw1.rda")
}

compareML(bw1, rMax_ML) #significant --> rMax preferred

#minus interaction thoughtsTime:intervention
if(FALSE){
  bw2 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence + thoughtsTime +
               thoughtsTime:group + thoughtsTime:phase +
               s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
               s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
               s(listless) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  
  save(bw1, file="models/bw2.rda", compress="xz")
} else {
  load("models/bw2.rda")
}

compareML(bw2, rMax_ML)


#minus interaction thoughtsTime:intervention
if(FALSE){
  bw2 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence + thoughtsTime +
               thoughtsTime:group + thoughtsTime:phase +
               s(blockBeepNum, by=group) +
               s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
               s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
               s(listless) +
               s(blockBeepNum, by = subject, bs="fs", m=1),
             data=sc_data, method = "ML")
  
  
  save(bw2, file="models/bw2.rda", compress="xz")
} else {
  load("models/bw2.rda")
}

compareML(bw2, rMax_ML)



#rMax but minus the "thoughtsTime" predictor
if(FALSE){
  g1 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(listless) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data)
  
  summary_g1 <- summary(g1)
  
  gML1 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence +
                        s(blockBeepNum, by=group) +
                        s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                        s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                        s(listless) +
                        s(blockBeepNum, by = subject, bs="fs", m=1),
                      data=sc_data, method = "ML")
  
  save(g1, summary_g1, gML1, file="models/g1.rda", compress="xz")
} else {
  load("models/g1.rda")
}

compareML(rMax_ML, gML1)


plot_smooth(g1, view="blockBeepNum", plot_all = "group")

plot(fitted(g1), resid(g1))
points(fitted(r9), resid(r9), col = "red")
abline(h=0)

# minus listless
if(FALSE){
  g2 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_g2 <- summary(g2)
  
  gML2 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g2, summary_g2, gML2, file="models/g2.rda", compress="xz")
} else {
  load("models/g2.rda")
}

plot(fitted(g1), resid(g1))
points(fitted(g2), resid(g2), col= "red")
abline(h=0)

plot_smooth(g1, view="blockBeepNum", plot_all="group")

compareML(gML1, gML2) #significant --> g1 is preferred


# minus distracted
if(FALSE){
  g3 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_g3 <- summary(g3)
  
  gML3 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(negIntensity) + s(posIntensity) + ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g3, summary_g3, gML3, file="models/g3.rda", compress="xz")
} else {
  load("models/g3.rda")
}

compareML(gML1, gML3) #significant --> g1 preferred


# minus interaction(posIntensity, negIntensity)
if(FALSE){
  g4 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_g4 <- summary(g4)
  
  gML4 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g4, summary_g4, gML4, file="models/g4.rda", compress="xz")
} else {
  load("models/g4.rda")
}

compareML(gML1, gML4) #significant --> g1 preferred

plot_smooth(g4, view="blockBeepNum", plot_all = "group")


# minus interaction(sumNA, sumPA)
if(FALSE){
  g5 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_g5 <- summary(g5)
  
  gML5 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + s(sleepQuality) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g5, summary_g5, gML5, file="models/g5.rda", compress="xz")
} else {
  load("models/g5.rda")
}

compareML(gML1, gML5) #not significant --> g5 preferred
plot_smooth(g5, view="blockBeepNum", plot_all="group")


# minus sleepQuality
if(FALSE){
  g6 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) +
              s(listless) +
              s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_g6 <- summary(g7)
  
  gML6 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g6, summary_g6, gML6, file="models/g6.rda", compress="xz")
} else {
  load("models/g6.rda")
}

compareML(gML5, gML6) #significant --> g5 preferred
plot_smooth(g5, view="blockBeepNum", plot_all="group")


# minus stickiness
if(FALSE){
  g7 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(blockBeepNum, by=group) +
              s(sumNA) + s(sumPA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1),
            data=sc_data)
  
  summary_g7 <- summary(g7)
  
  gML7 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(blockBeepNum, by=group) +
                s(sumNA) + s(sumPA) + s(sleepQuality) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g7, summary_g7, gML7, file="models/g7.rda", compress="xz")
} else {
  load("models/g7.rda")
}

compareML(gML5, gML7) #not significant --> g5 preferred


# minus interaction (blockBeepNum by group)
if(FALSE){
  g8 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(sumNA) + s(sumPA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1)
            data=sc_data)
  
  summary_g8 <- summary(g8)
  
  gML8 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(sumNA) + s(sumPA) + s(sleepQuality) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data=sc_data, method = "ML")
  
  save(g8, summary_g8, gML8, file="models/g8.rda", compress="xz")
} else {
  load("models/g8.rda")
}

compareML(gML5, gML8) #significant --> g8 preferred

plot_smooth(g8, view="blockBeepNum", plot_all= "group")


# minus sumPA
if(FALSE){
  g9 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(sumNA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data=sc_data)
  
  summary_g9 <- summary(g9)
  
  gML9 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(sumNA) + s(sleepQuality) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
              data=sc_data, method = "ML")
  
  save(g9, summary_g9, gML9, file="models/g9.rda", compress="xz")
} else {
  load("models/g9.rda")
}

compareML(gML8, gML9) #significant --> g8 preferred


# minus sumNA
if(FALSE){
  g10 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
              s(sumPA) + s(sleepQuality) +
              s(listless) +
              s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
              s(blockBeepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data=sc_data)
  
  summary_g10 <- summary(g10)
  
  gML10 <- gam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase +
                s(sumPA) + s(sleepQuality) +
                s(listless) +
                s(negIntensity) + s(posIntensity)+ ti(negIntensity, posIntensity) + s(stickiness) + s(distracted) +
                s(blockBeepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
              data=sc_data, method = "ML")
  
  save(g10, summary_g10, gML10, file="models/g10.rda", compress="xz")
} else {
  load("models/g10.rda")
}

compareML(gML8, gML10) #significant --> g8 preferred

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