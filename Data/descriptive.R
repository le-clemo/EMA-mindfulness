
#################################### Set up ####################################
rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202202-2")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202202-2")

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
data <- read_csv('preprocessed_data.csv') 

################################# response-related measures #####################################
# #group by id and count the number on nonresponses
# participant_responses <- ddply(data, .(subject), plyr::summarise,
#                                numBeeped = length(mindcog_db_open_from),
#                                noResponse = length(unique(mindcog_db_non_response)),
#                                response = numBeeped - noResponse,
#                                responseRate = round(response/numBeeped,2))


#recreacting with assessment days per participant
participant_responses <- ddply(data, .(subject), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay))

#recreacting with assessment days per group
group_responses <- ddply(data, .(group), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay)) #6% higher response rate in controls

#the mean response rate is ~66%
meanResponseRate <- mean(participant_responses$responseRate)
#sd of 20.48
sdResponseRate <- sd(participant_responses$responseRate)


################################### Initial analyses - raw values ####################################

#number of respondents (i.e., participants?) so far
length(unique(data$subject)) #35 associated with a group

#summary(data)

#calculating statistics per group (remitted vs controls)
grp_avgs <- ddply(data, .(group), plyr::summarize,
                 n_Subj = length(unique(subject)),
                 db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                 db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                 db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                 db9_sad_avg = mean(sad, na.rm = TRUE),
                 db9_sad_sd = sd(sad, na.rm = TRUE),
                 db10_satisfied_avg = mean(satisfied, na.rm = TRUE),
                 db10_satisfied_sd = sd(satisfied, na.rm = TRUE),
                 db11_irritated_avg = mean(irritated, na.rm = TRUE),
                 db12_energetic_avg = mean(energetic, na.rm = TRUE),
                 db13_restless_avg = mean(restless, na.rm = TRUE),
                 db14_stressed_avg = mean(stressed, na.rm = TRUE),
                 db15_anxious_avg = mean(anxious, na.rm = TRUE),
                 db16_listless_avg = mean(listless, na.rm = TRUE),
                 db18_worrying_avg = mean(worried, na.rm = TRUE),
                 db19_stickiness_avg = mean(stickiness, na.rm = TRUE),
                 db20_easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                 db24_distracted_avg = mean(distracted, na.rm = TRUE),
                 db25_restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                 db27_companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                 db28_solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                 db29_enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                 db30_intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                 db31_unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                 db32_intensityNeg_avg = mean(negIntensity, na.rm = TRUE),
                 response_delay_avg = round(mean(response_delay, na.rm = TRUE), 2),
                 response_duration_avg = round(mean(response_duration, na.rm = TRUE), 2))

#Metric columns
metricCols <- c('wakeful', 'sad', 'satisfied', 'irritated', 'energetic', 'restless',
                'stressed', 'anxious', 'listless', 'worried', 'stickiness', 'thoughtsPleasant', 'distracted',
                'restOfDayPos', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

#for some strange dplyr-ralted reason I need to do this to get melt() to work
data <- as.data.frame(data)

#boxplot comparisons
meltData1 <- melt(data[, c("group", metricCols[1:9])])
#boxplot(data=meltData, value~variable)

p1 <- ggplot(meltData1, aes(factor(variable), value, fill = group)) 
p1 + geom_boxplot() + facet_wrap(~variable, scale="free")


meltData2 <- melt(data[, c("group", metricCols[10:18])])
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = group)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")

#per group and intervention
int_avgs <- ddply(data, .(group, intervention), plyr::summarize,
                  n_Subj = length(unique(id)),
                  db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                  db9_sad_avg = mean(sad, na.rm = TRUE),
                  db9_sad_sd = sd(sad, na.rm = TRUE),
                  db10_satisfied_avg = mean(satisfied, na.rm = TRUE),
                  db10_satisfied_sd = sd(satisfied, na.rm = TRUE),
                  db11_irritated_avg = mean(irritated, na.rm = TRUE),
                  db12_energetic_avg = mean(energetic, na.rm = TRUE),
                  db13_restless_avg = mean(restless, na.rm = TRUE),
                  db14_stressed_avg = mean(stressed, na.rm = TRUE),
                  db15_anxious_avg = mean(anxious, na.rm = TRUE),
                  db16_listless_avg = mean(listless, na.rm = TRUE),
                  db18_worrying_avg = mean(worried, na.rm = TRUE),
                  db19_stickiness_avg = mean(stickiness, na.rm = TRUE),
                  db20_easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                  db24_distracted_avg = mean(distracted, na.rm = TRUE),
                  db25_restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                  db27_companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                  db28_solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                  db29_enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                  db30_intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                  db31_unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                  db32_intensityNeg_avg = mean(negIntensity, na.rm = TRUE))

#boxplot comparisons
meltData1 <- melt(data[, c("group", "intervention", metricCols[1:9])])
meltData1 <- meltData1 %>% drop_na(intervention)
meltData1$grInt <- sprintf("%s.%s", as.character(meltData1$group), meltData1$intervention)
#boxplot(data=meltData, value~variable)


p1 <- ggplot(meltData1, aes(factor(variable), value, fill = grInt)) #interaction = intervention)) 
p1 + geom_boxplot() + facet_wrap(~variable, scale="free")

meltData2 <- melt(data[, c("group", "intervention", metricCols[10:18])])
meltData2 <- meltData2 %>% drop_na(intervention)
meltData2$grInt <- sprintf("%s.%s", as.character(meltData2$group), meltData2$intervention)
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = grInt)) #interaction = intervention)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")

#per group, intervention, and phase
phase_avgs <- ddply(data, .(group, intervention, phase), plyr::summarize,
                  n_Subj = length(unique(id)),
                  db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                  db9_sad_avg = mean(sad, na.rm = TRUE),
                  db9_sad_sd = sd(sad, na.rm = TRUE),
                  db10_satisfied_avg = mean(satisfied, na.rm = TRUE),
                  db10_satisfied_sd = sd(satisfied, na.rm = TRUE),
                  db11_irritated_avg = mean(irritated, na.rm = TRUE),
                  db12_energetic_avg = mean(energetic, na.rm = TRUE),
                  db13_restless_avg = mean(restless, na.rm = TRUE),
                  db14_stressed_avg = mean(stressed, na.rm = TRUE),
                  db15_anxious_avg = mean(anxious, na.rm = TRUE),
                  db16_listless_avg = mean(listless, na.rm = TRUE),
                  db18_worrying_avg = mean(worried, na.rm = TRUE),
                  db19_stickiness_avg = mean(stickiness, na.rm = TRUE),
                  db20_easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                  db24_distracted_avg = mean(distracted, na.rm = TRUE),
                  db25_restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                  db27_companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                  db28_solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                  db29_enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                  db30_intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                  db31_unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                  db32_intensityNeg_avg = mean(negIntensity, na.rm = TRUE))


for(g in c("controls", "remitted")){
  #boxplot comparisons
  meltData3 <- melt(data[data$group == g, c("intervention", "phase", metricCols[1:9])])
  meltData3 <- meltData3 %>% drop_na(intervention)
  meltData3 <- meltData3 %>% drop_na(phase)
  meltData3$interventionPhase <- sprintf("%s.%s", as.character(meltData3$intervention), meltData3$phase)
  meltData3$interventionPhase <- factor(meltData3$interventionPhase,
                                        levels = c("fantasizing.pre", "fantasizing.peri", "mindfulness.pre",
                                                   "mindfulness.peri"))
  meltData3 <- meltData3 %>% drop_na(interventionPhase)
  
  p1 <- ggplot(meltData3, aes(factor(variable), value, fill = interventionPhase)) #interaction = intervention)) 
  p1 <- p1 + geom_boxplot() + facet_wrap(~variable, scale="free") +
    ggtitle(g)
  print(p1)
  
  meltData4 <- melt(data[data$group == g, c("intervention", "phase", metricCols[10:18])])
  meltData4 <- meltData4 %>% drop_na(intervention)
  meltData4 <- meltData4 %>% drop_na(phase)
  meltData4$interventionPhase <- sprintf("%s.%s", as.character(meltData4$intervention), meltData4$phase)
  meltData4$interventionPhase <- factor(meltData4$interventionPhase,
                                        levels = c("fantasizing.pre", "fantasizing.peri", "mindfulness.pre",
                                                   "mindfulness.peri"))
  meltData4 <- meltData4 %>% drop_na(interventionPhase)
  
  #boxplot(data=meltData, value~variable)
  
  p2 <- ggplot(meltData4, aes(factor(variable), value, fill = interventionPhase)) #interaction = intervention)) 
  p2 <- p2 + geom_boxplot() + facet_wrap(~variable, scale="free") +
    ggtitle(g)
  print(p2)
}

#################################### naive time series plots ####################################
#developments over time
time_avgs <- ddply(data, .(group, intervention, beepNum), plyr::summarize,
                  #n_Subj = length(unique(id)),
                  sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  wakeful_avg = mean(wakeful, na.rm = TRUE),
                  sad_avg = mean(sad, na.rm = TRUE),
                  sad_sd = sd(sad, na.rm = TRUE),
                  satisfied_avg = mean(satisfied, na.rm = TRUE),
                  satisfied_sd = sd(satisfied, na.rm = TRUE),
                  irritated_avg = mean(irritated, na.rm = TRUE),
                  energetic_avg = mean(energetic, na.rm = TRUE),
                  restless_avg = mean(restless, na.rm = TRUE),
                  stressed_avg = mean(stressed, na.rm = TRUE),
                  anxious_avg = mean(anxious, na.rm = TRUE),
                  listless_avg = mean(listless, na.rm = TRUE),
                  worrying_avg = mean(worried, na.rm = TRUE),
                  stickiness_avg = mean(stickiness, na.rm = TRUE),
                  easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                  distracted_avg = mean(distracted, na.rm = TRUE),
                  restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                  companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                  solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                  enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                  intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                  unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                  intensityNeg_avg = mean(negIntensity, na.rm = TRUE))

time_avgs$grInt <- paste(time_avgs$group, time_avgs$intervention, sep = "-")
time_avgs <- drop_na(time_avgs, intervention)

dependent_vars = c("sleep_avg", "wakeful_avg", "sad_avg", "satisfied_avg", "irritated_avg",
                   "energetic_avg", "restless_avg", "stressed_avg", "anxious_avg", "listless_avg",
                   "worrying_avg", "stickiness_avg", "easeThoughts_avg", "distracted_avg", "restOfDayPos_avg",
                   "companyPos_avg", "solitudePos_avg", "enjoyabilityMax_avg", "intensityPos_avg",
                   "unpleasantMax_avg", "intensityNeg_avg")

for(var in dependent_vars){
  meltData <- melt(drop_na(time_avgs, grInt), id = c( "beepNum", "grInt"), measure.vars = var)
  
  p1 <- ggplot(meltData, aes(beepNum, value, color = factor(grInt), group = variable)) +
          geom_point() +
          facet_grid("grInt") +
          geom_smooth(method='lm', formula = "y ~ x") +
          labs(y = var, x = "Assessment Number") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
                #strip.background = element_rect(fill = "lightblue"),
                legend.position="None")
  print(p1)
}

#developments over time per phase
phase_avgs <- ddply(data, .(group, intervention, block, phase, blockBeepNum), plyr::summarize,
                   #n_Subj = length(unique(id)),
                   sleep_avg = mean(sleepQuality, na.rm = TRUE),
                   sleep_sd = sd(sleepQuality, na.rm = TRUE),
                   wakeful_avg = mean(wakeful, na.rm = TRUE),
                   sad_avg = mean(sad, na.rm = TRUE),
                   sad_sd = sd(sad, na.rm = TRUE),
                   satisfied_avg = mean(satisfied, na.rm = TRUE),
                   satisfied_sd = sd(satisfied, na.rm = TRUE),
                   irritated_avg = mean(irritated, na.rm = TRUE),
                   energetic_avg = mean(energetic, na.rm = TRUE),
                   restless_avg = mean(restless, na.rm = TRUE),
                   stressed_avg = mean(stressed, na.rm = TRUE),
                   anxious_avg = mean(anxious, na.rm = TRUE),
                   listless_avg = mean(listless, na.rm = TRUE),
                   worrying_avg = mean(worried, na.rm = TRUE),
                   stickiness_avg = mean(stickiness, na.rm = TRUE),
                   easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                   distracted_avg = mean(distracted, na.rm = TRUE),
                   restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                   companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                   solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                   enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                   intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                   unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                   intensityNeg_avg = mean(negIntensity, na.rm = TRUE))

phase_avgs$grInt <- paste(phase_avgs$group, phase_avgs$intervention, sep = "-")
phase_avgs <- drop_na(phase_avgs, intervention)
phase_avgs <- drop_na(phase_avgs, phase)

dependent_vars = c("sleep_avg", "wakeful_avg", "sad_avg", "satisfied_avg", "irritated_avg",
                   "energetic_avg", "restless_avg", "stressed_avg", "anxious_avg", "listless_avg",
                   "worrying_avg", "stickiness_avg", "easeThoughts_avg", "distracted_avg",
                   "intensityPos_avg", "intensityNeg_avg")

for(var in dependent_vars){
  meltData <- melt(drop_na(phase_avgs, grInt),
                   id = c("block", "phase", "blockBeepNum", "grInt"), measure.vars = var)
  
  p1 <- ggplot(meltData, aes(blockBeepNum, value, color = factor(grInt), group = variable)) +
    geom_point() +
    facet_grid("grInt") +
    geom_smooth(method='lm', formula = "y ~ x") +
    labs(y = var, x = "Assessment Number") +
    theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
          #strip.background = element_rect(fill = "lightblue"),
          legend.position="None") #+
    #geom_vline(xintercept = max(phase_avgs[phase_avgs$phase == "pre1",]$phaseBeepNum)) +
    #geom_vline(xintercept = max(phase_avgs[phase_avgs$phase == "pre2",]$phaseBeepNum))
  
  print(p1)
  
}

#################################### Correlation Matrix ####################################

#creating a correlation matrix with numeric data
corrMat <- as.matrix(data[, metricCols])

#calculate the correlations
res <- rcorr(corrMat, type = c("pearson"))

corrplot(res$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "All participants")


#separately for remitted and controls
corrMatRm <- as.matrix(data[data$group == "remitted", metricCols])
corrMatCont <- as.matrix(data[data$group == "controls", metricCols])

#calculate the correlations
resRm <- rcorr(corrMatRm, type = c("pearson"))
resCont <- rcorr(corrMatCont, type = c("pearson"))


corrplot(resRm$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "Remitted")

corrplot(resCont$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "Controls")

par(mfrow = c(1,2))
corrplot(resRm$r, method = "number", order = 'alphabet', main = "Remitted")
corrplot(resCont$r, method = "number", order = 'alphabet', main = "Controls")

par(mfrow = c(1,1))

#################################### Individual plots ####################################

dependent_var <- "stickiness"

remittedIDs <-  unique(data[data$group == "remitted",]$id) #get a list of all unique IDs
set.seed(125)
remittedSample <- sample(remittedIDs, 3)

contIDs <-  unique(data[data$group == "controls",]$id) #get a list of all unique IDs
contSample <- sample(contIDs, 3)

randData1 <- data[data$id %in% remittedSample,]
randData2 <- data[data$id %in% contSample,]

meltData1 <- melt(randData1, id = c("id", "assessmentDay", "beepNum"), measure.vars = dependent_var)
meltData2 <- melt(randData2, id = c("id", "assessmentDay", "beepNum"), measure.vars = dependent_var)

#for x: use beepNum for time or another variable to see relationship
p1 <- ggplot(meltData1,aes(x=beepNum,y=value,colour=factor(id), group = variable)) +
  geom_line() + geom_smooth(method='lm')
p2 <- ggplot(meltData2,aes(x=beepNum,y=value,colour=factor(id), group = variable)) +
  geom_line() + geom_smooth(method='lm')

#to split one plot into a grid of multiple plots
p1 <- p1 + facet_grid(rows = vars(factor(id))) + xlab("Assessment Number") + ylab(dependent_var)
p2 <- p2 + facet_grid(rows = vars(factor(id))) + xlab("Assessment Number") + ylab(dependent_var)

p1 <- p1 + theme(legend.position="None")
p2 <- p2 + theme(legend.position="None") #+ geom_vline(xintercept = assessmentDay, linetype = "dashed", color = "red")

figure <- ggarrange(p1, p2,
                    labels = c("Remitted", "Controls"),
                    ncol = 2, nrow = 1)#, common.legend = TRUE)
figure


#test <- subset(data[data$id == 3602171,], select = c(id, mindcog_db_protocol, assessmentDay, beepNum, mindcog_db_open_from))


#################################### Exploratory rumination analysis ##################################

pc_time <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                  N = length(group[which(!is.na(thoughtsTime))]),
                  past = round(length(group[which(thoughtsTime == 1)])/N, 2),
                  present = round(length(group[which(thoughtsTime == 2)])/N, 2),
                  future = round(length(group[which(thoughtsTime == 3)])/N, 2))

pc_time <- drop_na(pc_time, intervention)

pc_val <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                 N = length(group[which(!is.na(thoughtsValence))]),
                 negative = round(length(group[which(thoughtsValence == 1)])/N, 2),
                 neutral = round(length(group[which(thoughtsValence == 2)])/N, 2),
                 positive = round(length(group[which(thoughtsValence == 3)])/N, 2))

pc_val <- drop_na(pc_val, intervention)


pc_object <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                   N = length(group[which(!is.na(thoughtsObject))]),
                   self = round(length(group[which(thoughtsObject == 1)])/N, 2),
                   somebody = round(length(group[which(thoughtsObject == 2)])/N, 2),
                   neither = round(length(group[which(thoughtsObject == 3)])/N, 2))

pc_object <- drop_na(pc_object, intervention)


pc_thinkingOf <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                   N = length(group[which(!is.na(thinkingOf))]),
                   currentActivity = round(length(group[which(thinkingOf == 1)])/N, 2),
                   externalStimuli = round(length(group[which(thinkingOf == 2)])/N, 2),
                   currentFeelings = round(length(group[which(thinkingOf == 3)])/N, 2),
                   personalConcerns = round(length(group[which(thinkingOf == 4)])/N, 2),
                   daydreaming = round(length(group[which(thinkingOf == 5)])/N, 2),
                   other = round(length(group[which(thinkingOf == 6)])/N, 2))

pc_thinkingOf <- drop_na(pc_thinkingOf, intervention)

#merge(pc_time, pc_val, pc_object, pc_thinkingOf, by = c(group, intervention, phase, block))

pc_summary <- Reduce(function(x, y) merge(x, y, all=TRUE), list(pc_time, pc_val, pc_object, pc_thinkingOf))



################################### Change scores ######################################
cols <- c('wakeful', 'sad', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
          'listless', 'thinkingOf', 'worried', 'stickiness', 'thoughtsPleasant', 'thoughtsTime',
          'thoughtsValence', 'thoughtsObject', 'distracted', 'restOfDayPos', 'aloneCompany',
          'companyPleasant', 'alonePleasant', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

changeCols <- c()
for(col in metricCols) {
  changeCols <- c(changeCols, paste(col, "change", sep = "_"))
}

grp_avg_cs<- ddply(data, .(group), plyr::summarize,
                  n_Subj = length(unique(subject)),
                  # sleep_avg = mean(sleepQuality_change, na.rm = TRUE),
                  # sleep_sd = sd(sleepQuality_change, na.rm = TRUE),
                  wakeful_avg = mean(wakeful_change, na.rm = TRUE),
                  sad_avg = mean(sad_change, na.rm = TRUE),
                  sad_sd = sd(sad_change, na.rm = TRUE),
                  satisfied_avg = mean(satisfied_change, na.rm = TRUE),
                  satisfied_sd = sd(satisfied_change, na.rm = TRUE),
                  irritated_avg = mean(irritated_change, na.rm = TRUE),
                  energetic_avg = mean(energetic_change, na.rm = TRUE),
                  restless_avg = mean(restless_change, na.rm = TRUE),
                  stressed_avg = mean(stressed_change, na.rm = TRUE),
                  anxious_avg = mean(anxious_change, na.rm = TRUE),
                  listless_avg = mean(listless_change, na.rm = TRUE),
                  worrying_avg = mean(worried_change, na.rm = TRUE),
                  stickiness_avg = mean(stickiness_change, na.rm = TRUE),
                  easeThoughts_avg = mean(thoughtsPleasant_change, na.rm = TRUE),
                  distracted_avg = mean(distracted_change, na.rm = TRUE),
                  restOfDayPos_avg = mean(restOfDayPos_change, na.rm = TRUE),
                  companyPos_avg = mean(companyPleasant_change, na.rm = TRUE),
                  solitudePos_avg = mean(alonePleasant_change, na.rm = TRUE),
                  enjoyabilityMax_avg = mean(posMax_change, na.rm = TRUE),
                  intensityPos_avg = mean(posIntensity_change, na.rm = TRUE),
                  unpleasantMax_avg = mean(negMax_change, na.rm = TRUE),
                  intensityNeg_avg = mean(negIntensity_change, na.rm = TRUE))


#per group, intervention, and phase
phase_avg_cs<- ddply(data, .(group, intervention, phase), plyr::summarize,
                   n_Subj = length(unique(subject)),
                   # sleep_avg = mean(sleepQuality_change, na.rm = TRUE),
                   # sleep_sd = sd(sleepQuality_change, na.rm = TRUE),
                   wakeful_avg = mean(wakeful_change, na.rm = TRUE),
                   sad_avg = mean(sad_change, na.rm = TRUE),
                   sad_sd = sd(sad_change, na.rm = TRUE),
                   satisfied_avg = mean(satisfied_change, na.rm = TRUE),
                   satisfied_sd = sd(satisfied_change, na.rm = TRUE),
                   irritated_avg = mean(irritated_change, na.rm = TRUE),
                   energetic_avg = mean(energetic_change, na.rm = TRUE),
                   restless_avg = mean(restless_change, na.rm = TRUE),
                   stressed_avg = mean(stressed_change, na.rm = TRUE),
                   anxious_avg = mean(anxious_change, na.rm = TRUE),
                   listless_avg = mean(listless_change, na.rm = TRUE),
                   worrying_avg = mean(worried_change, na.rm = TRUE),
                   stickiness_avg = mean(stickiness_change, na.rm = TRUE),
                   easeThoughts_avg = mean(thoughtsPleasant_change, na.rm = TRUE),
                   distracted_avg = mean(distracted_change, na.rm = TRUE),
                   restOfDayPos_avg = mean(restOfDayPos_change, na.rm = TRUE),
                   companyPos_avg = mean(companyPleasant_change, na.rm = TRUE),
                   solitudePos_avg = mean(alonePleasant_change, na.rm = TRUE),
                   enjoyabilityMax_avg = mean(posMax_change, na.rm = TRUE),
                   intensityPos_avg = mean(posIntensity_change, na.rm = TRUE),
                   unpleasantMax_avg = mean(negMax_change, na.rm = TRUE),
                   intensityNeg_avg = mean(negIntensity_change, na.rm = TRUE))


for(g in c("controls", "remitted")){
  #boxplot comparisons
  meltData3 <- melt(data[data$group == g, c("intervention", "phase", changeCols[1:9])])
  meltData3 <- meltData3 %>% drop_na(intervention)
  meltData3 <- meltData3 %>% drop_na(phase)
  meltData3$interventionPhase <- sprintf("%s.%s", as.character(meltData3$intervention), meltData3$phase)
  meltData3$interventionPhase <- factor(meltData3$interventionPhase,
                                        levels = c("fantasizing.pre", "fantasizing.peri", "mindfulness.pre",
                                                   "mindfulness.peri"))
  meltData3 <- meltData3 %>% drop_na(interventionPhase)
  
  p1 <- ggplot(meltData3, aes(factor(variable), value, fill = interventionPhase)) #interaction = intervention)) 
  p1 <- p1 + geom_boxplot() + facet_wrap(~variable, scale="free") +
    ggtitle(g)
  print(p1)
  
  meltData4 <- melt(data[data$group == g, c("intervention", "phase", changeCols[10:18])])
  meltData4 <- meltData4 %>% drop_na(intervention)
  meltData4 <- meltData4 %>% drop_na(phase)
  meltData4$interventionPhase <- sprintf("%s.%s", as.character(meltData4$intervention), meltData4$phase)
  meltData4$interventionPhase <- factor(meltData4$interventionPhase,
                                        levels = c("fantasizing.pre", "fantasizing.peri", "mindfulness.pre",
                                                   "mindfulness.peri"))
  meltData4 <- meltData4 %>% drop_na(interventionPhase)
  
  #boxplot(data=meltData, value~variable)
  
  p2 <- ggplot(meltData4, aes(factor(variable), value, fill = interventionPhase)) #interaction = intervention)) 
  p2 <- p2 + geom_boxplot() + facet_wrap(~variable, scale="free") +
    ggtitle(g)
  print(p2)
}



