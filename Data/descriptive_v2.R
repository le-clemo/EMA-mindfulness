
#################################### Set up ####################################
rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

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
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(broom)
library(effectsize)
library(languageR)

#read in data
data <- read.csv('preprocessed_data.csv') 

data$group <- factor(data$group, levels = c("controls", "remitted"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))
data$subjB <- interaction(data$subject, data$block, drop = TRUE) #unique identifier for subject per block


unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)
falseDays <- which(data$phaseAssessmentDay==0)
data <- data[-falseDays,]


################################# response-related measures #####################################
# #group by id and count the number of nonresponses
participant_responses <- ddply(data, .(subject), plyr::summarise,
                               numCompleted = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numCompleted - noResponse,
                               responseRate = round(response/numCompleted,2),
                               numDays = max(assessmentDay))

#number of participants so far
length(unique(data$subject)) #39 associated with a group

#the mean response rate is ~67%
meanResponseRate <- mean(participant_responses$responseRate)
#sd of 18.63
sdResponseRate <- sd(participant_responses$responseRate)


# View(subset(data[which(data$phaseAssessmentDay>7),],
#             select=c("group", "intervention", "id", "phase", "block",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date")))




#numDays = max(phaseAssessmentDay))

group_responses <- ddply(data, .(group), plyr::summarise,
                                nSubj = length(unique(subject)),
                                numCompleted = length(mindcog_db_open_from),
                                noResponse = length(unique(mindcog_db_non_response)),
                                response = numCompleted - noResponse,
                                responseRate = round(response/numCompleted,2))

group_responses_baseline <- ddply(data[which(data$phase=="pre"),], .(group), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2))

#recreacting with assessment days per group
intervention_responses <- ddply(data, .(group, intervention), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2))

responses_by_phase <- ddply(data, .(group, intervention, phase), plyr::summarise,
                            numCompleted = length(mindcog_db_open_from),
                            noResponse = length(unique(mindcog_db_non_response)),
                            response = numCompleted - noResponse,
                            responseRate = round(response/numCompleted,2))


#Chi-squared tests
#group difference?
chisq.test(group_responses[,c("noResponse", "response")]) #significant difference
#x-sq = 12.5, p = 0.0004

chisq.test(group_responses_baseline[,c("noResponse", "response")]) #significant difference
#X-squared = 7.0587, df = 1, p-value = 0.007888

#difference by phase (groupXintervention)?
groups <- c("controls", "remitted")
interventions <- c("fantasizing", "mindfulness")
for(g in groups){
  for(int in interventions){
    #print(paste(g, int, sep = " + "))
    responses <- responses_by_phase[which((responses_by_phase$group==g) & (responses_by_phase$intervention==int)),]
    #print(chisq.test(responses[, c("noResponse", "response")]))
  }
} #response rates are always worse in peri


#to remove participants below a certain threshold (response rate)
responses_block <- ddply(data, .(subjB), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))

pp <- unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)
cut_data <- data[which(data$subjB %in% pp),]


################################################ Missed assessments ##################################################

missedAssessments <- ddply(data, .(group, intervention, phase, blockAssessmentDay), plyr::summarize,
                           nBeeps = length(subjB),
                           nMissed =  length(unique(mindcog_db_non_response)),
                           propMissed = round(nMissed/nBeeps, 2))


ggplot(missedAssessments, aes(x=blockAssessmentDay, y=propMissed, color=interaction(group, intervention))) +
  geom_line() + geom_point() + geom_vline(xintercept = 7.5, lty = "dashed")


########################################### T.tests ##########################################################

met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
               'sumPA', 'wakeful', 'satisfied', 'energetic',
                'stressed', 'listless',  'distracted',
                'thoughtsPleasant', 'restOfDayPos',
                'posMax', 'posIntensity', 'negMax', 'negIntensity',
               "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

cat.vars <- c( "thinkingOf", "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany")

for(g in groups){
  for(int in interventions){
    for(v in met.vars){
      print('')
      print('##########################################################')
      print(paste(g, int, v, sep = " + "))
      pre <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="pre")),][[v]]
      peri <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="peri")),][[v]]
      
      test <- t.test(peri, pre)
      if(test$p.value<0.05){
        print(test)
      } else {
        print("No significant difference")
      }
    }
  }
}



######################################### ANOVAs ##########################################################################

# for(v in c("ruminating", "sumNA", "sumPA", "companyPleasant", "alonePleasant", "negMax", "negIntensity",
#            "posMax", "posIntensity", "stickiness", "anxious", "sleepQuality")){
#   print("###############################################################")
#   print(v)
#   one.way <- aov(data[[v]] ~ group * intervention, data = data)
#   print(eta_squared(one.way))
#   #print(summary(one.way))
#   print(TukeyHSD(one.way, "group"))
# }

for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){
    dat <- data[which((data$group==g) & (data$intervention==int)),]
    for(v in met.vars){
      print('')
      print('##########################################################')
      print(paste(g, int, v, sep = " + "))
      # one.way <- aov(dat[[v]] ~ phase, data = dat)
      # anova <- TukeyHSD(one.way)
      test <- var.test(dat[[v]] ~ phase, data = dat)
      if(test$p.value<0.05){
        print("pre")
        print(var(dat[which(dat$phase=="pre"),][[v]], na.rm=TRUE))
        print("peri")
        print(var(dat[which(dat$phase=="peri"),][[v]], na.rm=TRUE))
        print(test)
        # print(anova)
        # print(eta_squared(one.way))
      } else {
        print("No significant difference")
        # print(anova)
      }
    }
  }
}



##################################### Daily avg per individual ########################################
melt.dat <- melt(data, id.vars=c("subject", "group", "assessmentDay"),
                 measure.vars = c("ruminating", "sumPA", "sumNA"), na.rm = TRUE)
melt.dat <- aggregate(melt.dat$value, by=list(subject=melt.dat$subject, assessmentDay=melt.dat$assessmentDay,
                                              group=melt.dat$group, variable=melt.dat$variable), FUN=mean)

ggplot(melt.dat[which((melt.dat$variable=="ruminating") & (melt.dat$assessmentDay<=7)),],
       aes(x=assessmentDay, y=x, group=subject, color=group, fill = group)) + theme_bw() +
  geom_line(alpha = 0.3) +#+ stat_summary(fun = mean, na.rm = TRUE, geom ='line', group="group", lwd = 1) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", lwd = 3, group = "") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line", group = "group") + ylab("Rumination")



melt.dat <- melt(data, id.vars=c("subjB", "group", "intervention", "phase", "blockAssessmentDay"),
                 measure.vars = c("ruminating", "sumPA", "sumNA"), na.rm = TRUE)
melt.dat <- aggregate(melt.dat$value, by=list(subject=melt.dat$subjB, blockAssessmentDay=melt.dat$blockAssessmentDay,
                                              group=melt.dat$group, intervention=melt.dat$intervention,
                                              phase=melt.dat$phase, variable=melt.dat$variable), FUN=mean)

ggplot(melt.dat[which(melt.dat$variable=="ruminating"),],
       aes(x=blockAssessmentDay, y=x, group=subject, color=interaction(group, intervention))) + theme_bw() +
  geom_line(alpha = 0.3) +#+ stat_summary(fun = mean, na.rm = TRUE, geom ='line', group="group", lwd = 1) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", lwd = 3, group = "") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line", group = "group") + ylab("Rumination")















