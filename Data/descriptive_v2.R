
#################################### Set up ####################################
rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202205")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202205")

library(readxl)
library(ggh4x)
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

data$group <- factor(data$group, levels = c("remitted", "controls"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))
data$subjB <- interaction(data$subject, data$block, drop = TRUE) #unique identifier for subject per block


# unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)
falseDays <- which(data$phaseAssessmentDay==0)
data <- data[-falseDays,]

################################# response-related measures #####################################
# #group by id and count the number of nonresponses
participant_responses <- ddply(data, .(subject, group), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay))

#number of participants so far
length(unique(data$subject)) #39 associated with a group

#number of responses
sum(participant_responses$response) #~6000+

#the mean response rate is ~67%
meanResponseRate <- mean(participant_responses$responseRate)
#sd of 18.29
sdResponseRate <- sd(participant_responses$responseRate)

sdResponseRateCon <- round(sd(participant_responses[which(participant_responses$group == "controls"),]$responseRate), 2)
sdResponseRateRem <- round(sd(participant_responses[which(participant_responses$group == "remitted"),]$responseRate), 2)

quantile(participant_responses$responseRate, probs = c(.1, .9))
quantile(participant_responses[which(participant_responses$group == "controls"),]$responseRate, probs = c(.1, .9))
quantile(participant_responses[which(participant_responses$group == "remitted"),]$responseRate, probs = c(.1, .9))

# View(subset(data[which(data$phaseAssessmentDay>7),],
#             select=c("group", "intervention", "id", "phase", "block",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date")))




#numDays = max(phaseAssessmentDay))

group_responses <- ddply(data, .(group), plyr::summarise,
                                nSubj = length(unique(subject)),
                                numBeeps = length(mindcog_db_open_from),
                                noResponse = length(unique(mindcog_db_non_response)),
                                response = numBeeps - noResponse,
                                responseRate = round(response/numBeeps,2),
                                SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))

group_responses_baseline <- ddply(data[which(data$phase=="pre"),], .(group), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2),
                         SDResponseRate = round(sd(response/numBeeps, na.rm = TRUE), 2))

#recreacting with assessment days per group, intervention, block, phase
full_split <- ddply(data, .(group, intervention, block, phase), plyr::summarise,
                                nSubj = length(unique(subject)),
                                numBeeps = length(mindcog_db_open_from),
                                noResponse = length(unique(mindcog_db_non_response)),
                                response = numBeeps - noResponse,
                                responseRate = round(response/numBeeps,2))

#recreacting with assessment days per group
intervention_responses <- ddply(data, .(group, intervention), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2))

responses_by_phase <- ddply(data, .(group, intervention, phase), plyr::summarise,
                            nSubj = length(unique(subject)),
                            numBeeps = length(mindcog_db_open_from),
                            noResponse = length(unique(mindcog_db_non_response)),
                            response = numBeeps - noResponse,
                            responseRate = round(response/numBeeps,2))

#group by phase
groupXphase <- ddply(data, .(group, phase), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2),
                         SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))

responses_block <- ddply(data, .(block), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2),
                         SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))

responses_phase <- ddply(data, .(phase), plyr::summarise,
                     nSubj = length(unique(subject)),
                     numBeeps = length(mindcog_db_open_from),
                     noResponse = length(unique(mindcog_db_non_response)),
                     response = numBeeps - noResponse,
                     responseRate = round(response/numBeeps,2),
                     SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))


#Chi-squared tests
#group difference?
chisq.test(group_responses[,c("noResponse", "response")]) #significant difference
#x-sq = 12.5, p = 0.0004

chisq.test(group_responses_baseline[,c("noResponse", "response")]) #significant difference
#X-squared = 7.0587, df = 1, p-value = 0.007888

#general difference between blocks
chisq.test(responses_block[, c("noResponse", "response")])

#general difference between phases
chisq.test(responses_phase[, c("noResponse", "response")])

#difference by phase (groupXintervention)?
groups <- c("controls", "remitted")
interventions <- c("fantasizing", "mindfulness")
for(g in groups){
  for(int in interventions){
    print(paste(g, int, sep = " + "))
    responses <- responses_by_phase[which((responses_by_phase$group==g) & (responses_by_phase$intervention==int)),]
    print(chisq.test(responses[, c("noResponse", "response")]))
  }
} #response rates are always worse in peri


######################################## Removing low-response subjects ##############################################

responses_block <- ddply(data, .(subject, group), plyr::summarise,
                         numBeeped = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeped - noResponse,
                         responseRate = round(response/numBeeped,2),
                         numDays = max(assessmentDay))

data$subjB <- interaction(data$subject, data$block, drop = TRUE)

meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.6%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subject)) #20
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subject)) #26
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subject)) #33
length(unique(responses_block[which(responses_block$group == "remitted"),]$subject)) #16
length(unique(responses_block[which(responses_block$group == "controls"),]$subject)) #23

#removing participants with a response rate lower than 50%
pp <- unique(responses_block[which(responses_block$responseRate >= 0.5),]$subject)
data <- data[which(data$subject %in% pp),]

length(unique(data[which(data$group == "remitted"),]$subject)) #12
length(unique(data[which(data$group == "controls"),]$subject)) #21

################################################ Missed assessments ##################################################

missedAssessments <- ddply(data, .(group, intervention, phase, blockAssessmentDay), plyr::summarize,
                           nBeeps = length(subjB),
                           nMissed =  length(unique(mindcog_db_non_response)),
                           propMissed = round(nMissed/nBeeps, 2))


p <- ggplot(missedAssessments, aes(x=blockAssessmentDay, y=propMissed, color=interaction(group, intervention))) +
  geom_line() + geom_point() + geom_vline(xintercept = 7.5, lty = "dashed") +
  ylab("Proportion of assessments missed") + xlab("Block assessment day") 
p$labels$colour <- "Group"
p

########################################### T.tests ##########################################################

met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'stressed', 'listless',  'distracted',
              'restOfDayPos',
              'posMax', 'negMax', 'negIntensity')


meltDat <- melt(data, id.vars = c("group", "intervention", "phase"), measure.vars = met.vars)
avgByPhase <- ddply(meltDat, ~group + intervention + phase + variable, plyr::summarize, mean=mean(value, na.rm = TRUE))


met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos', 'companyPleasant', 'alonePleasant',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

cat.vars <- c( "thinkingOf", "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany")

# t-tests to compare baselevel means between groups

                  
for(v in met.vars){
  print('')
  print('##########################################################')
  print(v)
  
  preCon <- subset(data[which((data$group=="controls") & (data$block==1) & (data$phase=="pre")),], select = c("subject", v))
  colnames(preCon) <-  c("subject", "variable")
  
  preRem <- subset(data[which((data$group=="remitted") & (data$block==1) & (data$phase=="pre")),], select = c("subject", v))
  colnames(preRem) <-  c("subject", "variable")
  
  meansCon <- ddply(preCon, .(subject), plyr::summarise,
                    controls = mean(variable, na.rm=T))
  
  meansRem <- ddply(preRem, .(subject), plyr::summarise,
                    remitted = mean(variable, na.rm=T))
                    
  
  # pre <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="pre")),][[v]]
  # peri <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="peri")),][[v]]
  
  test <- t.test(meansCon$controls, meansRem$remitted)
  print(test)
  if(test$p.value>0.05){
    print("No significant difference")
    # print(mean(peri, na.rm = TRUE))
    #print(mean(pre, na.rm = TRUE))
  }
}



# paired t-test between pre and peri means (by group and intervention)
meansDF <- data.table(data)[, c(lapply(.SD[, met.vars, with = FALSE], mean, na.rm=TRUE)), by = c("subject", "group", "intervention", "phase")]

for(g in groups){
  for(int in interventions){
    for(v in met.vars){
      print('')
      print('##########################################################')
      print(paste(g, int, v, sep = " + "))
      
      subDat <- subset(meansDF[which((meansDF$group==g) & (meansDF$intervention==int)),], select = c("subject", "group", "intervention", "phase", v))
      subjects <- unique(subDat$subject)
      
      pre <- c(rep(NA, length(subjects)))
      peri <- c(rep(NA, length(subjects)))
      
      i <- 1
      for(s in subjects){
        subjectMeans <- subDat[which((subDat$subject==s)),]
        v1 <- subjectMeans[which(subjectMeans$phase == "pre"),][[v]]
        v2 <- subjectMeans[which(subjectMeans$phase == "peri"),][[v]]
        
        if(length(v1) != 0){
          pre[[i]] <- v1
        } else {
          pre[[i]] <- NA
        }
        
        if(length(v2) != 0){
          peri[[i]] <- v2
        } else {
          peri[[i]] <- NA
        }
        
        i <- i + 1
      }
      
      
      testFrame <- (data.frame(subject = subjects, pre = pre, peri = peri))

      # pre <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="pre")),][[v]]
      # peri <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="peri")),][[v]]
      
      test <- with(testFrame, t.test(pre, peri, paired = TRUE))
      if(test$p.value<0.05){
        print(test)
       # print(mean(peri, na.rm = TRUE))
        #print(mean(pre, na.rm = TRUE))
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


for(g in groups){
  for(int in interventions){
    for(v in met.vars){
      print('')
      print('##########################################################')
      print(paste(g, int, v, sep = " + "))
      
      subDat <- subset(meansDF[which((meansDF$group==g) & (meansDF$intervention==int)),], select = c("subject", "group", "intervention", "phase", v))
      subjects <- unique(subDat$subject)
      
      pre <- c(rep(NA, length(subjects)))
      peri <- c(rep(NA, length(subjects)))
      
      i <- 1
      for(s in subjects){
        subjectMeans <- subDat[which((subDat$subject==s)),]
        v1 <- subjectMeans[which(subjectMeans$phase == "pre"),][[v]]
        v2 <- subjectMeans[which(subjectMeans$phase == "peri"),][[v]]
        
        if(length(v1) != 0){
          pre[[i]] <- v1
        } else {
          pre[[i]] <- NA
        }
        
        if(length(v2) != 0){
          peri[[i]] <- v2
        } else {
          peri[[i]] <- NA
        }
        
        i <- i + 1
      }
      
      
      testFrame <- (data.frame(subject = subjects, pre = pre, peri = peri))
      
      # pre <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="pre")),][[v]]
      # peri <- data[which((data$group==g) & (data$intervention==int) & (data$phase=="peri")),][[v]]
      
      test <- with(testFrame, var.test(pre, peri))
      if(test$p.value<0.05){
        print(test)
        # print(mean(peri, na.rm = TRUE))
        #print(mean(pre, na.rm = TRUE))
      } else {
        print("No significant difference")
      }
    }
  }
}




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

# Histograms and density lines
par(mfrow=c(2, 2))
for(g in c("controls", "remitted")){
  for(col in met.vars) {
    for(int in c("fantasizing", "mindfulness")){
      for(p in c("pre", "peri")){
        hist(data[which((data$group==g) & (data$intervention==int) & (data$phase==p)),col], xlim=c(0, 310), breaks=seq(0, 310, 10),
             main=paste(g,int,p, sep = " | "), probability=TRUE, col="gray", border="white", xlab = col)
        d <- density(data[which((data$group==g) & (data$intervention==int)),col], na.rm = TRUE)
        lines(d, col="red")
      }
      
    }
  }
}
par(mfrow=c(1,1))



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



melt.dat <- melt(data, id.vars=c("subjB", "group", "intervention", "phase", "block", "phaseAssessmentDay"),
                 measure.vars = c("ruminating", "sumPA", "sumNA"), na.rm = TRUE)
melt.dat <- aggregate(melt.dat$value, by=list(subject=melt.dat$subjB, phaseAssessmentDay=melt.dat$phaseAssessmentDay,
                                              group=melt.dat$group, intervention=melt.dat$intervention, block=melt.dat$block,
                                              phase=melt.dat$phase, variable=melt.dat$variable), FUN=mean)


#same for baseline2 (pre2)
ggplot(melt.dat[which((melt.dat$variable=="ruminating") & (melt.dat$phase=="pre") & (melt.dat$block==2)),],
       aes(x=phaseAssessmentDay, y=x, group=subject, color=group, fill = group)) + theme_bw() +
  geom_line(alpha = 0.3) +#+ stat_summary(fun = mean, na.rm = TRUE, geom ='line', group="group", lwd = 1) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", lwd = 3, group = "") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line", group = "group") + ylab("Rumination")

#looking at group X intervention
# ggplot(melt.dat[which(melt.dat$variable=="ruminating"),],
#        aes(x=blockAssessmentDay, y=x, group=subject, color=interaction(group, intervention))) + theme_bw() +
#   geom_line(alpha = 0.3) +#+ stat_summary(fun = mean, na.rm = TRUE, geom ='line', group="group", lwd = 1) +
#   stat_summary(fun = mean, na.rm = TRUE, geom = "point", lwd = 3, group = "") +
#   stat_summary(fun = mean, na.rm = TRUE, geom = "line", group = "group") + ylab("Rumination")




#################################################### random subject scores ###############################################
set.seed(1)
control_subj <- unique(data[which(data$group == "controls"),]$subject)
remitted_subj <- unique(data[which(data$group == "remitted"),]$subject)

rand.cs <- sample(control_subj, 6)
rand.rm <- sample(remitted_subj, 6)
rand.subj <- append(rand.cs, rand.rm)

subCon <- data[which((data$subject %in% rand.cs) & (data$block==1) & (data$phase == "pre")),] 
subRem <- data[which((data$subject %in% rand.rm) & (data$block==1) & (data$phase == "pre")),]

idVars <- c("subject", "group", "intervention", "phase", "beepNum")

meltCon <- melt(subCon[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, beepNum, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, beepNum, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltCon <- meltCon[!duplicated(meltCon[c(1,8)]), ]
meltRem <- meltRem[!duplicated(meltRem[c(1,8)]), ]

#individual rumination plots
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  xlab('Beep Number') + ylab("Rumination") + #geom_vline(xintercept = 70, lty = "dashed")) +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank())

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  xlab('Beep Number') + ylab("Rumination") + #geom_vline(xintercept = 70, lty = "dashed") +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination")


#individual NA plots
p1 <- ggplot(data = meltCon[which(meltCon$variable=="sumNA"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,300) +
  xlab('Beep Number') + #geom_vline(xintercept = 70, lty = "dashed") +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank())

p2 <- ggplot(data = meltRem[which(meltRem$variable=="sumNA"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,300) +
  xlab('Beep Number') + #geom_vline(xintercept = 70, lty = "dashed") +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Negative Affect")


#individual PA plots
p1 <- ggplot(data = meltCon[which(meltCon$variable=="sumPA"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,300) +
  xlab('Beep Number') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank())

p2 <- ggplot(data = meltRem[which(meltRem$variable=="sumPA"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,300) +
  xlab('Beep Number') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Positive Affect")


###################################### variable as a function of most unpleasant previous event ###################################################
idVars <- c("subject", "group", "intervention", "phase", "negMax")

meltCon <- melt(subCon[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, negMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, negMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

#rumination
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,100) + xlim(0,100) +
  xlab('Unpleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,100) + xlim(0,100) +
  xlab('Unpleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination")


#positive affect
p1 <- ggplot(data = meltCon[which(meltCon$variable=="sumPA"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,300) + xlim(0,100) +
  xlab('Unpleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="sumPA"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,300) + xlim(0,100) +
  xlab('Unpleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Positive Affect")


#negative affect
p1 <- ggplot(data = meltCon[which(meltCon$variable=="sumNA"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,300) + xlim(0,100) +
  xlab('Unpleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="sumNA"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,300) + xlim(0,100) +
  xlab('Unpleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Negative Affect")



###################################### variable as a function of most pleasant previous event ###################################################
idVars <- c("subject", "group", "intervention", "phase", "posMax")

meltCon <- melt(subCon[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "sumNA", "sumPA", "posMax"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, posMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "sumNA", "sumPA", "posMax"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, posMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

#rumination
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=posMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,100) + xlim(0,100) +
  xlab('Pleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=posMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,100) + xlim(0,100) +
  xlab('Pleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination")

#positive affect
p1 <- ggplot(data = meltCon[which(meltCon$variable=="sumPA"),],
             aes(x=posMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,300) + xlim(0,100) +
  xlab('Pleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="sumPA"),],
             aes(x=posMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,300) + xlim(0,100) +
  xlab('Pleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Positive Affect")


#negative affect
p1 <- ggplot(data = meltCon[which(meltCon$variable=="sumNA"),],
             aes(x=posMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,300) + xlim(0,100) +
  xlab('Pleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="sumNA"),],
             aes(x=posMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,300) + xlim(0,100) +
  xlab('Pleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Negative Affect")

################################################ Baseline boxplots #####################################################################

### boxplot comparisons with all data points separately


group.colors = c(remitted = "#F8766D", controls = "#619CFF")

meltData1 <- melt(data[, c("group", met.vars[1:2])])
#boxplot(data=meltData, value~variable)

p1 <- ggplot(meltData1, aes(factor(variable), value, fill = group)) 
p1 + geom_boxplot() + facet_wrap(~variable, scale="free") + scale_fill_manual(values=group.colors)


meltData2 <- melt(data[, c("group", "down", "irritated", "restless", "anxious")])
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = group)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")


meltData3 <- melt(data[, c("group", "wakeful", "satisfied", "energetic")])
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData3, aes(factor(variable), value, fill = group)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")


### boxplots of daily avg
idVars <- c("subject", "group", "intervention", "phase", "assessmentDay")

meltDat <-  within( melt(data[, c(idVars, "ruminating", "sumNA", "sumPA", "negMax", "posMax",
                                  "down", "irritated", "restless", "anxious",
                                  "satisfied", "wakeful", "energetic"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, assessmentDay, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltDat <- meltDat[!duplicated(meltDat[c(1,5,8)]), ]

ggplot(data = meltDat[which(meltDat$variable=="ruminating"),],
             aes(x= phase, y=Mean, fill = group)) +
  geom_boxplot() + ylim(0,100) +
  ylab("Rumination score") +
  facet_grid(factor(intervention)~factor(group), scale = "free") + theme(legend.position="none", axis.title.x=element_blank(),
                                                     axis.ticks.x=element_blank())  + scale_fill_manual(values=group.colors)
#PANA
p1 <- ggplot(data = meltDat[which(meltDat$variable=="sumNA"),],
             aes(x= phase, y=Mean, fill = group)) +
  geom_boxplot() + ylim(0,300) +
  ylab("Negative Affect") +
  facet_grid(factor(intervention)~factor(group), scale = "free") + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                                         axis.ticks.x=element_blank()) + scale_fill_manual(values=group.colors)
p2 <- ggplot(data = meltDat[which(meltDat$variable=="sumPA"),],
       aes(x= phase, y=Mean, fill = group)) +
  geom_boxplot() + ylim(0,300) +
  ylab("Positive Affect") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", axis.title.x=element_blank(), axis.ticks.x=element_blank(), strip.text.x = element_blank()) +
  scale_fill_manual(values=group.colors)

grid.arrange(p1, p2, ncol=1, nrow = 2)

#event (un)pleasantness
p1 <- ggplot(data = meltDat[which(meltDat$variable=="negMax"),],
       aes(x= phase, y=Mean, fill = group)) +
  geom_boxplot() + ylim(0,100) +
  ylab("Unpleasantness of Events") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=group.colors)

p2 <- ggplot(data = meltDat[which(meltDat$variable=="posMax"),],
       aes(x= phase, y=Mean, fill = group)) +
  geom_boxplot() + ylim(0,100) +
  ylab("Pleasantness of Events") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", axis.title.x=element_blank(),  axis.ticks.x=element_blank(), strip.text.x = element_blank()) +
  scale_fill_manual(values=group.colors)

grid.arrange(p1, p2, ncol=1, nrow = 2)

#NA split
phase.colors = c(pre = "#F8766D", peri = "#619CFF")
ggplot(data = meltDat[which(meltDat$variable %in% c("down", "irritated", "restless", "anxious")),],
       aes(x= variable, y=Mean, fill = phase)) +
  geom_boxplot() + ylim(0,100) +
  ylab("Score") +
  facet_grid(factor(intervention)~factor(group), scale = "free") + theme(legend.position="none", axis.title.x=element_blank(),
                                                                         axis.ticks.x=element_blank()) + scale_fill_manual(values=phase.colors)
#PA split
ggplot(data = meltDat[which(meltDat$variable %in% c("satisfied", "wakeful", "energetic")),],
       aes(x= variable, y=Mean, fill = phase)) +
  geom_boxplot() + ylim(0,100) +
  ylab("Score") +
  facet_grid(factor(intervention)~factor(group), scale = "free") + theme(legend.position="none", axis.title.x=element_blank(),
                                                                         axis.ticks.x=element_blank()) + scale_fill_manual(values=phase.colors)



### boxplots of rumination as 
idVars <- c("subject", "group", "intervention", "phase", "assessmentDay")

meltDat <-  within( melt(data[, c(idVars, "ruminating", "sumNA", "sumPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, assessmentDay, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltDat <- meltDat[!duplicated(meltDat[c(1,5,8)]), ]












