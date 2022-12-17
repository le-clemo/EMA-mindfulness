
#################################### Set up ####################################
rm(list = ls()) #clean all up


source("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/common_plot_theme.R")
setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202207")

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
library(ggthemes)
library(ggpattern)
library(Rmisc)
library(tikzDevice)

#read in data
data <- read.csv('preprocessed_data.csv') 

#Pick response rate cut-off value
cutOff <- 0.5

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
#x-sq = 5.92, p = 0.01496

chisq.test(group_responses_baseline[,c("noResponse", "response")]) #significant difference
#X-squared = 7.01, df = 1, p-value = 0.008106

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
pp <- unique(responses_block[which(responses_block$responseRate >= cutOff),]$subject)
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


met.vars <- c('ruminating', 'stickiness', 'sumNA', 'meanNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'meanPA', 'wakeful', 'satisfied', 'energetic',
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


dat <- data[which((data$phase=="pre")),]
for(v in met.vars){
  print('')
  print('##########################################################')
  print(paste(g, v, sep = " + "))
  # one.way <- aov(dat[[v]] ~ phase, data = dat)
  # anova <- TukeyHSD(one.way)
  test <- var.test(dat[[v]] ~ group, data = dat)
  if(test$p.value<0.05){
    print("controls")
    print(var(dat[which(dat$group=="controls"),][[v]], na.rm=TRUE))
    print("remitted")
    print(var(dat[which(dat$group=="remitted"),][[v]], na.rm=TRUE))
    print(test)
    # print(anova)
    # print(eta_squared(one.way))
  } else {
    print("No significant difference")
    # print(anova)
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
                 measure.vars = c("ruminating", "meanPA", "meanNA"), na.rm = TRUE)
melt.dat <- aggregate(melt.dat$value, by=list(subject=melt.dat$subject, assessmentDay=melt.dat$assessmentDay,
                                              group=melt.dat$group, variable=melt.dat$variable), FUN=mean)

ggplot(melt.dat[which((melt.dat$variable=="ruminating") & (melt.dat$assessmentDay<=7)),],
       aes(x=assessmentDay, y=x, group=subject, color=group, fill = group)) + theme_bw() +
  geom_line(alpha = 0.3) +#+ stat_summary(fun = mean, na.rm = TRUE, geom ='line', group="group", lwd = 1) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", lwd = 3, group = "") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line", group = "group") + ylab("Rumination")



melt.dat <- melt(data, id.vars=c("subjB", "group", "intervention", "phase", "block", "phaseAssessmentDay"),
                 measure.vars = c("ruminating", "meanPA", "meanNA"), na.rm = TRUE)
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
data$group <- factor(data$group, levels = c("remitted", "controls"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))

set.seed(1)
control_subj <- unique(data[which(data$group == "controls"),]$subject)
remitted_subj <- unique(data[which(data$group == "remitted"),]$subject)

rand.cs <- sample(control_subj, 6)
rand.rm <- sample(remitted_subj, 6)
rand.subj <- append(rand.cs, rand.rm)

subCon <- data[which((data$subject %in% rand.cs) &
                       (data$block==1) &
                       (data$phase == "pre") & 
                       (data$phaseBeepNum <= 70)),] 
subRem <- data[which((data$subject %in% rand.rm) &
                       (data$block==1) &
                       (data$phase == "pre") &
                       (data$phaseBeepNum <= 70)),]



idVars <- c("subject", "group", "intervention", "phase", "beepNum")

meltCon <- melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, beepNum, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, beepNum, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltCon <- meltCon[!duplicated(meltCon[c(1,8)]), ]
meltRem <- meltRem[!duplicated(meltRem[c(1,8)]), ]

levels(meltCon$group) <- c("rMDD", "HC")
levels(meltRem$group) <- c("rMDD", "HC")

levels(meltCon$intervention) <- c("Mindfulness", "Fantasizing")
levels(meltRem$intervention) <- c("Mindfulness", "Fantasizing")

# Reduce the opacity of the grid lines: Default is 255
# col_grid <- rgb(255, 255, 255, 60, maxColorValue = 255)

pdf(width = 10, height = 5.5,
    file = "individual_rum_plots.pdf")  #bg = "#D5E4EB"
#individual rumination plots
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() + 
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61), labels = c(1:7))

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61))
  # scale_y_continuous(minor_breaks = seq(0,100,25))

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination", bottom = "Assessment Day")

dev.off()


#individual NA plots
pdf(width = 10, height = 5.5,
    file = "individual_NA_plots.pdf")  #bg = "#D5E4EB"
p1 <- ggplot(data = meltCon[which(meltCon$variable=="meanNA"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() + 
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61), labels = c(1:7))

p2 <- ggplot(data = meltRem[which(meltRem$variable=="meanNA"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61))
# scale_y_continuous(minor_breaks = seq(0,100,25))

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Negative Affect", bottom = "Assessment Day")

dev.off()


#individual PA plots
pdf(width = 10, height = 5.5,
    file = "individual_PA_plots.pdf")  #bg = "#D5E4EB"
p1 <- ggplot(data = meltCon[which(meltCon$variable=="meanPA"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() + 
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61), labels = c(1:7))

p2 <- ggplot(data = meltRem[which(meltRem$variable=="meanPA"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61))
# scale_y_continuous(minor_breaks = seq(0,100,25))

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Positive Affect", bottom = "Assessment Day")

dev.off()


###################################### variable as a function of most unpleasant previous event ###################################################
idVars <- c("subject", "group", "intervention", "phase", "negMax")

meltCon <- melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, negMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, negMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

levels(meltCon$group) <- c("rMDD", "HC")
levels(meltRem$group) <- c("rMDD", "HC")

#rumination
pdf(width = 10, height = 5.5,
    file = "individual_rum_by_negMax.pdf")  #bg = "#D5E4EB"
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,100) + xlim(0,100) +
  multi_plot_theme() +
  facet_grid(group~factor(subject), scale = "free") +
  geom_smooth(method = "lm", se = FALSE, col = "black")

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,100) + xlim(0,100) +
  multi_plot_theme() +
  facet_grid(group~factor(subject), scale = "free") +
  geom_smooth(method = "lm", se = FALSE, col = "black")

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination",
             bottom = "Event Unpleasantness")

dev.off()


#positive affect
p1 <- ggplot(data = meltCon[which(meltCon$variable=="meanPA"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,100) + xlim(0,100) +
  xlab('Unpleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="meanPA"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,100) + xlim(0,100) +
  xlab('Unpleasant of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none",
                                                            axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Positive Affect")


#negative affect
p1 <- ggplot(data = meltCon[which(meltCon$variable=="meanNA"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,100) + xlim(0,100) +
  xlab('Unpleasantness of Event') +
  facet_grid(group~factor(subject), scale = "free") + theme(legend.position="none", axis.title.y=element_blank()) +
  geom_smooth(method = "lm", se = FALSE, col = "black") 

p2 <- ggplot(data = meltRem[which(meltRem$variable=="meanNA"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,100) + xlim(0,100) +
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


group.colors = c(rMDD = "#F8766D", HC = "#619CFF")

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

meltDat <-  within( melt(data[, c(idVars, "ruminating", "meanNA", "meanPA", "negMax", "posMax",
                                  "down", "irritated", "restless", "anxious",
                                  "satisfied", "wakeful", "energetic"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, assessmentDay, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltDat <- meltDat[!duplicated(meltDat[c(1,5,8)]), ]
meltDat$group <- factor(meltDat$group, levels = c("remitted", "controls"))
meltDat$intervention <- factor(meltDat$intervention, levels = c("mindfulness", "fantasizing"))

levels(meltDat$group) <- c("rMDD", "HC")
levels(meltDat$intervention) <- c("Mindfulness", "Fantasizing")

pdf(width = 8, height = 6,
    file = "daily_avg_rum_boxplots.pdf")
ggplot(data = meltDat[which(meltDat$variable=="ruminating"),],
             aes(x= factor(phase, levels = c("pre", "peri")), y=Mean,
                 fill = group)) +
  geom_boxplot() + ylim(0,100) +
  single_plot_theme() +
  ylab("Rumination") + 
  xlab("Phase") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  scale_fill_manual(values=group.colors)

dev.off()

#NA
pdf(width = 8, height = 6,
    file = "daily_avg_NA_boxplots.pdf")
ggplot(data = meltDat[which(meltDat$variable=="meanNA"),],
       aes(x= factor(phase, levels = c("pre", "peri")), y=Mean,
           fill = group)) +
  geom_boxplot() + ylim(0,100) +
  single_plot_theme() +
  ylab("Negative Affect") +
  xlab("Phase") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  # axis.ticks.x=element_blank()) +
  scale_fill_manual(values=group.colors)

dev.off()


#PA
pdf(width = 8, height = 6,
    file = "daily_avg_PA_boxplots.pdf")
ggplot(data = meltDat[which(meltDat$variable=="meanPA"),],
       aes(x= factor(phase, levels = c("pre", "peri")), y=Mean,
           fill = group)) +
  geom_boxplot() + ylim(0,100) +
  single_plot_theme() +
  ylab("Positive Affect") +
  xlab("Phase") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  # axis.ticks.x=element_blank()) +
  scale_fill_manual(values=group.colors)

dev.off()
# p1 <- ggplot(data = meltDat[which(meltDat$variable=="meanNA"),],
#              aes(x= phase, y=Mean, fill = group)) +
#   geom_boxplot() + ylim(0,100) +
#   ylab("Negative Affect") +
#   facet_grid(factor(intervention)~factor(group), scale = "free") +
#   single_plot_theme() +
#   theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank()) +
#   scale_fill_manual(values=group.colors)
# p2 <- ggplot(data = meltDat[which(meltDat$variable=="meanPA"),],
#        aes(x= phase, y=Mean, fill = group)) +
#   geom_boxplot() + ylim(0,100) +
#   ylab("Positive Affect") +
#   facet_grid(factor(intervention)~factor(group), scale = "free") +
#   single_plot_theme( ) +
#   theme(legend.position="none", axis.title.x=element_blank(), axis.ticks.x=element_blank(), strip.text.x = element_blank()) +
#   scale_fill_manual(values=group.colors)
# 
# grid.arrange(p1, p2, ncol=1, nrow = 2)
# 
# dev.off()

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


#####################################################################################################################
colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep("plain", length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- "bold"                                  # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

# standard_error <- function(x) sd(x, na.rm=T) / sqrt(length(x))


idVars <- c("group", "intervention") # "subject"

# colnames(data)[colnames(data) == 'meanNA'] <- 'Negative Affect *'
# colnames(data)[colnames(data) == 'meanPA'] <- 'Positive Affect'
# colnames(data)[colnames(data) == 'ruminating'] <- 'Rumination *'

baselineDat <- data[which(data$phase=="pre"),]

meltDat <-  within( melt(baselineDat[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
                                    variable<- gsub("\\_.*","",variable)
                                    Mean<- ave(value, group, variable, FUN=function(x) mean(x,na.rm=T))
                                    SD <- ave(value, group, variable, FUN=function(x) sd(x,na.rm=T))}) #subject


meltDat <- meltDat[!duplicated(meltDat[c(1,3,5)]), ]

meltDat[which(meltDat$variable=="ruminating"),]$variable <- "Rumination *"
meltDat[which(meltDat$variable=="meanNA"),]$variable <- "Negative Affect *"
meltDat[which(meltDat$variable=="meanPA"),]$variable <- "Positive Affect"

meltDat$group <- factor(meltDat$group)
meltDat$intervention <- factor(meltDat$intervention, levels = c("mindfulness", "fantasizing"))

levels(meltDat$group) <- c("HC", "rMDD")
levels(meltDat$intervention) <- c("Mindfulness", "Fantasizing")

se_rum_c <- meltDat[which((meltDat$group=="HC") & (meltDat$variable=="Rumination *")),]$SD /
  sqrt(length(unique(baselineDat[which((baselineDat$group=="controls")),]$subject)))
se_rum_r <- meltDat[which((meltDat$group=="rMDD") & (meltDat$variable=="Rumination *")),]$SD /
  sqrt(length(unique(baselineDat[which((baselineDat$group=="remitted")),]$subject)))
se_na_c <- meltDat[which((meltDat$group=="HC") & (meltDat$variable=="Negative Affect *")),]$SD /
  sqrt(length(unique(baselineDat[which((baselineDat$group=="controls")),]$subject)))
se_na_r <- meltDat[which((meltDat$group=="rMDD") & (meltDat$variable=="Negative Affect *")),]$SD /
  sqrt(length(unique(baselineDat[which((baselineDat$group=="remitted")),]$subject)))
se_pa_c <- meltDat[which((meltDat$group=="HC") & (meltDat$variable=="Positive Affect")),]$SD /
  sqrt(length(unique(baselineDat[which((baselineDat$group=="controls")),]$subject)))
se_pa_r <- meltDat[which((meltDat$group=="rMDD") & (meltDat$variable=="Positive Affect")),]$SD /
  sqrt(length(unique(baselineDat[which((baselineDat$group=="remitted")),]$subject)))

meltDat$SE <- c(se_rum_c, se_rum_r, se_na_c, se_na_r, se_pa_c, se_pa_r)


pdf(width = 10, height = 3.75,
    file = "rq1_data.pdf")
p<- ggplot(meltDat, aes(x=factor(variable, levels=c("Positive Affect", "Negative Affect *", "Rumination *")), y=Mean, fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + ylim(0,75) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2,
                position=position_dodge(.9)) +
  coord_flip()

#  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
#position=position_dodge(.9))  + 

# Finished bar plot
p+labs(x="", y = "")+
  single_plot_theme() +
  theme(legend.position=c(0.8,.9),
        legend.background = element_rect("#D5E4EB"),
        legend.direction = "vertical",
        axis.text.y = element_text(face = colorado(meltDat$variable, c("Rumination *", "Positive Affect")))) +
  scale_fill_manual(labels = c("HC", "rMDD"), values=c('#619CFF', '#F8766D'))
# guides(fill=guide_legend(title="Group"))

dev.off()



#######################################################################################################################
met.vars <- c("ruminating", "meanNA", "meanPA")

#in addition we create a new list which includes both the changed and unchanged met.vars for scaling later on
scale.vars <- c(rep(NA, length(met.vars)*3))
i = 0
for(v in met.vars){
  new_var <- paste(v, "_diff", sep = "")
  data[[new_var]] <- NA
  
  gam_var <- paste(v, "_gam", sep = "")
  data[[gam_var]] <- NA
  i = i+1
  scale.vars[[i]] <- v
  i = i+1
  scale.vars[[i]] <- new_var
  i = i+1
  scale.vars[[i]] <- gam_var
  
  for(id in unique(data$subject)){
    for(b in 1:2){
      pre_rows <- which((data$subject == id) & (data$phase=="pre") & (data$block==b))
      peri_rows <- which((data$subject == id) & (data$phase=="peri") & (data$block==b))
      s_rows <- which((data$subject == id) & (data$block==b))
      baselineMean <- mean(data[[v]][pre_rows], na.rm=TRUE)
      
      if(is.na(baselineMean)){
        baselineMean <- 0
      }
      
      data[[new_var]][s_rows] <- round(data[[v]][s_rows] - baselineMean, 2)
      
      data[[gam_var]][pre_rows] <- NA
      data[[gam_var]][peri_rows] <- round(data[[v]][peri_rows] - baselineMean, 2)
    }
  }  
}


meltDat <-  within( melt(data[which(data$phase=="peri"),][, c(idVars, "ruminating_gam", "meanNA_gam", "meanPA_gam"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, group, intervention, variable, FUN=function(x) mean(x,na.rm=T))
  SD <- ave(value, group, intervention, variable, FUN=function(x) sd(x,na.rm=T))}) #subject


meltDat <- meltDat[!duplicated(meltDat[c(1,2,3,6)]), ]
meltDat$group <- factor(meltDat$group)
meltDat$intervention <- factor(meltDat$intervention)

rq2_split <- read.csv("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/rq2_split.csv")
rq2_split[which(rq2_split$group=="remitted"),]$group <- "rMDD"
rq2_split[which(rq2_split$group=="controls"),]$group <- "HC"
rq2_split[which(rq2_split$intervention=="fantasizing"),]$intervention <- "Fantasizing"
rq2_split[which(rq2_split$intervention=="mindfulness"),]$intervention <- "Mindfulness"
names(rq2_split)[names(rq2_split) == 'node'] <- 'variable'

meltDat$SE <- rq2_split$se

se_list <- c(rep(NA,12))
j <- 1
for(var in c("ruminating", "meanNA", "meanPA")){
  for(g in c("controls", "remitted")){
    for(i in c("fantasizing", "mindfulness")){
      se_list[j] <- meltDat[which((meltDat$variable==var) & (meltDat$group==g) & (meltDat$intervention==i)),]$SD /
        sqrt(length(unique(data[which((data$group==g) & (data$phase=="peri")),]$subject)))
      
      j <- j + 1
    }
  }
}

levels(meltDat$group) <- c("HC", "rMDD")
levels(meltDat$intervention) <- c("Fantasizing", "Mindfulness")
meltDat[which(meltDat$variable=="ruminating"),]$variable <- "Rumination"
meltDat[which(meltDat$variable=="meanNA"),]$variable <- "Negative Affect"
meltDat[which(meltDat$variable=="meanPA"),]$variable <- "Positive Affect"


meltDat <- merge(meltDat, rq2_split[, c("se", "group", "intervention", "variable")], by = c("variable", "group", "intervention"))

meltDat$SE <- se_list

pdf(width = 10, height = 3.75,
    file = "rq2_gam_split_results.pdf")
p<- ggplot(meltDat, aes(x=factor(variable, levels=c("Positive Affect", "Negative Affect", "Rumination")),
                              y=Mean,
                              fill=group, pattern = intervention)) + 
  geom_bar_pattern(stat = "identity",
                   position = position_dodge(), #preserve = "single"
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.2)+
  ylim(-10,10) +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.2,
                position=position_dodge(.9))+
  coord_flip()

# Finished bar plot
p+labs(x="", y = "")+
  single_plot_theme() +
  theme(legend.position=c(0.8,.9),
        legend.background = element_rect("#D5E4EB"),
        legend.direction = "horizontal") +
  # guides(fill=guide_legend(title="Group X Intervention")) +
  scale_fill_manual(labels = c("HC", "rMDD"),
                    values=c('#619CFF', '#F8766D')) +
  scale_pattern_manual(labels = c("Fantasizing", "Mindfulness"), values = c(Fantasizing = "stripe", Mindfulness = "none")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

dev.off()

#########################################################################################################################
idVars = c("subject", "group", "intervention", "assessmentDay")
meltDat <-  within(melt(baselineDat[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, subject, group, assessmentDay, variable, FUN=function(x) mean(x,na.rm=T))}) #subject

meltDat[which(meltDat$variable=="ruminating"),]$variable <- "Rumination *"
meltDat[which(meltDat$variable=="meanNA"),]$variable <- "Negative Affect *"
meltDat[which(meltDat$variable=="meanPA"),]$variable <- "Positive Affect"

pdf(width = 10, height = 3.75,
    file = "rq1_data.pdf")
p<- ggplot(meltDat, aes(x=factor(variable, levels=c("Positive Affect", "Negative Affect *", "Rumination *")), y=Mean, fill=group)) + 
  geom_boxplot() + ylim(0,100) +
  coord_flip()

#  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
#position=position_dodge(.9))  + 

# Finished bar plot
p+labs(x="", y = "")+
  single_plot_theme() +
  theme(legend.position=c(0.8,.9),
        legend.background = element_rect("#D5E4EB"),
        legend.direction = "vertical") +
  # guides(fill=guide_legend(title="Group X Intervention")) +
  scale_fill_manual(labels = c("HC", "rMDD"),
                    values=c('#619CFF', '#F8766D')) +
  scale_pattern_manual(labels = c("Fantasizing", "Mindfulness"), values = c(Fantasizing = "stripe", Mindfulness = "none")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

dev.off()



#########################################################################################################################
idVars = c("subject", "group", "intervention", "assessmentDay")
meltDat <-  within(melt(data[which(data$phase=="peri"),][, c(idVars, "ruminating_gam", "meanNA_gam", "meanPA_gam"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, subject, group, intervention, assessmentDay, variable, FUN=function(x) mean(x,na.rm=T))}) #subject

meltDat[which(meltDat$variable=="ruminating"),]$variable <- "Rumination"
meltDat[which(meltDat$variable=="meanNA"),]$variable <- "Negative Affect"
meltDat[which(meltDat$variable=="meanPA"),]$variable <- "Positive Affect"

pdf(width = 10, height = 3.75,
    file = "rq2_data.pdf")
p<- ggplot(meltDat, aes(x=factor(variable, levels=c("Positive Affect", "Negative Affect", "Rumination")),
                        y=Mean, fill=group, linetype = factor(intervention))) + 
  geom_boxplot() + ylim(-50,50)
  # geom_bar_pattern(stat = "identity",
  #                  position = position_dodge(), #preserve = "single"
  #                  color = "black", 
  #                  pattern_fill = "black",
  #                  pattern_angle = 45,
  #                  pattern_density = 0.1,
  #                  pattern_spacing = 0.025,
  #                  pattern_key_scale_factor = 0.2)+
  # coord_flip()

#  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
#position=position_dodge(.9))  + 

# Finished bar plot
p+labs(x="", y = "")+
  single_plot_theme() +
  theme(legend.position=c(0.8,.9),
        legend.background = element_rect("#D5E4EB")) +
        # legend.direction = "vertical") +
        # axis.text.y = element_text(face = colorado(meltDat$variable, c("Rumination *", "Positive Affect")))) +
  scale_fill_manual(labels = c("HC", "rMDD"), values=c('#619CFF', '#F8766D')) +
  scale_linetype_manual(name = "Intervention", values = c("solid", "dotted"))
# guides(fill=guide_legend(title="Group"))

dev.off()


################################# using Cousineau-Morey method ################################################
# library(superb)
# View(data %>%
#   pivot_wider(id_cols = c(subject, group, intervention, phase),
#               names_from = phaseBeepNum,
#               values_from = ruminating))


# set up a datatable
cmDat <- setDT(data)
# subj-specific pace
cmDat[ , subj_rum := mean(ruminating, na.rm=T), by = .(subject)]
# general mean across conditions
cmDat[ , overall_rum := mean(ruminating, na.rm=T)]
# pace controlled for subjective pace
cmDat[ , rum_within := ruminating - subj_rum + overall_rum]
# sanity check
cmDat[ , check := mean(rum_within, na.rm=T), by = .(subject)]

# subj-specific pace
cmDat[ , subj_rum_gam := mean(ruminating_gam, na.rm=T), by = .(subject)]
# general mean across conditions
cmDat[ , overall_rum_gam := mean(ruminating_gam, na.rm=T)]
# pace controlled for subjective pace
cmDat[ , rum_within_gam := ruminating_gam - subj_rum_gam + overall_rum_gam]
# sanity check
cmDat[ , check := mean(rum_within_gam, na.rm=T), by = .(subject)]


View(subset(cmDat, select = c("subject", "group", "intervention", "phase", "ruminating", "subj_rum", "overall_rum", "rum_within", "check")))

muh_grob <- grid::rectGrob(
  x=1:2, y=0, gp=gpar(
    colour="#D5E4EB",fill="#D5E4EB"))


group.colors = c(HC = "#619CFF", rMDD = "#F8766D")

SE_rum1 <- summarySE(data[which((data$block==1)),], measurevar = "ruminating", groupvars = c("group", "phase"), 
                   na.rm = T)

SE_rum1$group <- factor(SE_rum1$group, levels = c("controls", "remitted"))
levels(SE_rum1$group) <- c("HC", "rMDD")
SE_rum1$phase <- factor(SE_rum1$phase, levels = c("pre", "peri"))
levels(SE_rum1$phase) <- c("Pre", "Peri")

p1 <- ggplot(SE_rum1[which(SE_rum1$phase=="Pre"),], aes(y =  ruminating, x=group, group=1), color = group) +
  geom_line(color = c("black"), size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = ruminating-se, ymax = ruminating+se),width = 0.5, color = rev(group.colors), size=0.5) +
  geom_point(size = 3, color = rev(group.colors))+
  ylim(5,25) +
  single_plot_theme() +
  ylab("Rumination") + 
  # facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        # axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  # coord_cartesian(clip='off') +
  scale_fill_manual(values=rev(group.colors)) +
  geom_text(
    mapping = aes(x = 1.5, y = 13.4, label = "*"), size = 10
  )
  # scale_x_discrete(position = "top")
  # annotation_custom(
  #   grob=muh_grob, xmin = -0.2, xmax = 1, ymin = 29, ymax=33
  # )


SE_na1 <- summarySE(data[which((data$block==1)),], measurevar = "meanNA", groupvars = c("group", "phase"), 
                     na.rm = T)

SE_na1$group <- factor(SE_na1$group, levels = c("controls", "remitted"))
levels(SE_na1$group) <- c("HC", "rMDD")
SE_na1$phase <- factor(SE_na1$phase, levels = c("pre", "peri"))
levels(SE_na1$phase) <- c("Pre", "Peri")

p2 <- ggplot(SE_na1[which(SE_na1$phase=="Pre"),], aes(y =  meanNA, x=group, group=1), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanNA-se, ymax = meanNA+se),width = 0.5, color = rev(group.colors), size=0.5) +
  geom_point(size = 3, color = rev(group.colors))+
  ylim(5,25) +
  single_plot_theme() +
  ylab("Negative Affect") + 
  # facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_blank(),
        axis.title.x=element_blank()) +
  scale_fill_manual(values=rev(group.colors)) +
  geom_text(
    mapping = aes(x = 1.5, y = 11.2, label = "*"), size = 10
  )


SE_pa1 <- summarySE(data[which((data$block==1)),], measurevar = "meanPA", groupvars = c("group", "phase"), 
                    na.rm = T)

SE_pa1$group <- factor(SE_pa1$group, levels = c("controls", "remitted"))
levels(SE_pa1$group) <- c("HC", "rMDD")
SE_pa1$phase <- factor(SE_pa1$phase, levels = c("pre", "peri"))
levels(SE_pa1$phase) <- c("Pre", "Peri")

p3 <- ggplot(SE_pa1[which(SE_pa1$phase=="Pre"),], aes(y =  meanPA, x=group, group=1), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanPA-se, ymax = meanPA+se),width = 0.5, color = rev(group.colors), size=0.5) +
  geom_point(size = 3, color = rev(group.colors))+
  ylim(50,70) +
  single_plot_theme() +
  ylab("Positive Affect") + 
  xlab("Group") +
  # facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  scale_fill_manual(values=rev(group.colors))

rq1_plot <- grid.arrange(p1, p2, p3, nrow=3)

ggsave(rq1_plot, file="rq1_group_diff.pdf", width = 4, height = 10)



############################################# RQ 2 ###############################################################
# calculate SE by taking repeated measures into account
group.colors <- c("#619CFF", "#F8766D")
group.colors2 <- c("#619CFF", "#619CFF", "#F8766D", "#F8766D")
peri_dat <- data[which(data$phase=="peri"),]

source("summarySEwithin2.R")

SE_rum2 <- summarySEwithin2(peri_dat, measurevar = "ruminating_gam", betweenvars = c("group"),
                     withinvars = c("intervention"), idvar="subject", na.rm = T)

SE_rum2$group <- factor(SE_rum2$group)
levels(SE_rum2$group) <- c("HC", "rMDD")
SE_rum2$intervention <- factor(SE_rum2$intervention)
levels(SE_rum2$intervention) <- c("Fantasizing", "Mindfulness")


dat_text <- data.frame(
  label = c("*", "*", "*"),
  group   = c("HC", "HC", "HC"),
  x     = c(1, 1.5, 2),
  y     = c(2.5, -0.15, -3.05)
)

p1 <- ggplot(SE_rum2, aes(y =  ruminating_gam, x=intervention, group=1,), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = ruminating_gam-ci, ymax = ruminating_gam+ci),width = 0.5, size=0.5, color = group.colors2) +
  geom_point(size = 3, color = group.colors2)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) +
  # annotate("text", x = 4, y=2, label = "ship") +
  ylim(-5,5) +
  single_plot_theme() +
  ylab("Rumination") + 
  facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_manual(values=group.colors) +
  geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label), size = 10
)

##na
SE_na2 <- summarySEwithin2(peri_dat, measurevar = "meanNA_gam", betweenvars = c("group"),
                            withinvars = c("intervention"), idvar="subject", na.rm = T)

SE_na2$group <- factor(SE_na2$group)
levels(SE_na2$group) <- c("HC", "rMDD")
SE_na2$intervention <- factor(SE_na2$intervention)
levels(SE_na2$intervention) <- c("Fantasizing", "Mindfulness")

dat_text <- data.frame(
  label = c("*", "*", "*", "*", "*", "*"),
  group   = c("HC", "HC", "HC", "rMDD", "rMDD", "rMDD"),
  x     = c(1, 1.5, 2, 1, 1.5, 2),
  y     = c(0.25, -0.7, -1.6, 0.25, -0.6, -1.4)
)

p2 <- ggplot(SE_na2, aes(y =  meanNA_gam, x=intervention, group=1,), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanNA_gam-se, ymax = meanNA_gam+se),width = 0.5, size=0.5, color = group.colors2) +
  geom_point(size = 3, color = group.colors2)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) +
  ylim(-5,5) +
  single_plot_theme() +
  ylab("Negative Affect") + 
  facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none",
        strip.text = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank()) +
  scale_fill_manual(values=group.colors) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label), size = 10
  )

##pa
SE_pa2 <- summarySEwithin2(peri_dat, measurevar = "meanPA_gam", betweenvars = c("group"),
                           withinvars = c("intervention"), idvar="subject", na.rm = T)

SE_pa2$group <- factor(SE_pa2$group)
levels(SE_pa2$group) <- c("HC", "rMDD")
SE_pa2$intervention <- factor(SE_pa2$intervention)
levels(SE_pa2$intervention) <- c("Fantasizing", "Mindfulness")

dat_text <- data.frame(
  label = c("*", "*", "*", "*"),
  group   = c("HC", "HC", "rMDD", "rMDD"),
  x     = c(1.5, 2, 1.5, 2),
  y     = c(0.3, 1.2, -1.7, -3.4)
)

p3 <- ggplot(SE_pa2, aes(y =  meanPA_gam, x=intervention, group=1,), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanPA_gam-se, ymax = meanPA_gam+se),width = 0.5, size=0.5, color = group.colors2) +
  geom_point(size = 3, color = group.colors2)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) +
  ylim(-5,5) +
  xlab("Intervention") +
  single_plot_theme() +
  ylab("Positive Affect") + 
  facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none",
        strip.text.x = element_blank(),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=8),
        strip.background = element_blank()) +
  scale_fill_manual(values=group.colors) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label), size = 10
  )

rq2_plot <- grid.arrange(p1, p2, p3, nrow=3)

ggsave(rq2_plot, file="rq2.pdf", width = 4, height = 10)





