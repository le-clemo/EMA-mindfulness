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


###########################################################################################################
######################################### Some more data prep #############################################
###########################################################################################################

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
data$thoughtsTime <- factor(data$thoughtsTime, levels = c("past", "present", "future"))
data$thoughtsValence <- factor(data$thoughtsValence, levels = c("negative", "neutral", "positive"))
data$aloneCompany <- factor(data$aloneCompany, levels = c("alone", "in company"))


#add day of the week
data$weekday <- weekdays(strptime(data$mindcog_db_open_from, "%Y-%m-%d %H:%M:%S"))
data$weekday <- as.factor(data$weekday)


###########################################################################################################
############################################ Scaling Data #################################################
###########################################################################################################
#creating variables minus baseline means per subject
met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

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

# View(subset(data[which(data$subject=="s37"),],
#             select = c("subject", "phase", "block", "ruminating", "ruminating_gam", "ruminating_diff", "beepNum")))

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
#sc_data <- sc_data[which(sc_data$blockBeepNum <= 140),]
#sc_data <- sc_data[which(is.na(sc_data$mindcog_db_non_response)),]


###########################################################################################################
######################################## Predicting rumination ############################################
###########################################################################################################
#checking out its distribution
hist(data$stickiness, breaks = 20)
qqnorm(data$stickiness)
qqline(data$stickiness)

#also the change scores
hist(data$stickiness_gam, breaks = 20)
qqnorm(data$stickiness_gam)
qqline(data$stickiness_gam) #change scores are more normally distributed


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++ Creating models with gam-variables +++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#gam varialbes are NA for entire pre-phase (while diff-variables are actual - baselineMean for all data)

avg2 <- ddply(sc_data, c("group", "intervention", "phaseBeepNum", "stickiness_gam"), summarise,
              N    = length(stickiness_gam),
              sd   = sd(stickiness_gam),
              se   = sd / sqrt(N))
plot2 <- ggplot(avg2, aes(y=stickiness_gam, x=phaseBeepNum, color=intervention)) +
  geom_point(position = position_jitter(w=0.1,h=0))+ facet_grid(.~group) +
  ylab("Stickiness Difference peri-pre")+ xlab("Phase Assessment Number") + labs(color = "Intervention") +
  geom_hline(yintercept=0) + geom_smooth()  #+ scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7"))

plot2


#Maximum model with ML
sticky.max <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention +
                    s(companyPleasant_gam) +
                    s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                    s(ruminating_gam) + s(sumNA_gam) +
                    s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                    s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                    s(phaseBeepNum, by = subject, bs="fs", m=1),
                  data = sc_data, method = "ML")

#Maximum model with REML
sticky.max.reml <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence * thoughtsTime +
                      aloneCompany + s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                      s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                      s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                      s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                      s(phaseBeepNum, by = subject, bs="fs", m=1),
                      data = sc_data)

summary.sticky.max.reml <- summary(sticky.max.reml)

save(sticky.max, sticky.max.reml, summary.sticky.max.reml, file = "models_stickiness/sticky_max.rda")



#removing aloneCompany
sticky1 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence * thoughtsTime +
                 s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky1, file = "models_stickiness/sticky1_gam.rda")

compareML(sticky.max, sticky1) #no significant difference --> sticky1 preferred


#removing s(phaseBeepNum, by=intervention)
sticky2 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence * thoughtsTime +
                 s(phaseBeepNum, by=group) +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky2, file = "models_stickiness/sticky2_gam.rda")

compareML(sticky1, sticky2) #no significant difference --> sticky2 preferred  



#removing s(phaseBeepNum, by=group)
sticky3 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence * thoughtsTime +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky3, file = "models_stickiness/sticky3_gam.rda")

compareML(sticky2, sticky3) #no significant difference --> sticky3 preferred 


#removing thoughtsTime:intervention
sticky4 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                 thoughtsTime:group + thoughtsTime:thoughtsValence +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky4, file = "models_stickiness/sticky4_gam.rda")

compareML(sticky3, sticky4) #no significant difference --> sticky4 preferred 


#removing thoughtsTime:thoughtsValence
sticky5 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                 thoughtsTime:group +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky5, file = "models_stickiness/sticky5_gam.rda")

compareML(sticky4, sticky5) #no significant difference --> sticky5 preferred 


#removing thoughtsTime:group
sticky6 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky6, file = "models_stickiness/sticky6_gam.rda")

compareML(sticky5, sticky6) #significant difference --> sticky5 preferred 


#removing thoughtsValence:intervention
sticky7 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention + thoughtsValence + thoughtsTime +
                 thoughtsTime:group + thoughtsValence:group + thoughtsValence:thoughtsTime +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky7, file = "models_stickiness/sticky7_gam.rda")

compareML(sticky5, sticky7) #significant difference --> sticky5 preferred


#removing thoughtsValence:thoughtsTime
sticky8 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention + thoughtsValence + thoughtsTime +
                 thoughtsTime:group + thoughtsValence:group + thoughtsValence:intervention +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky8, file = "models_stickiness/sticky8_gam.rda")

compareML(sticky5, sticky8) #significant difference --> sticky5 preferred


#removing thoughtsValence:group
sticky9 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention + thoughtsValence + thoughtsTime +
                 thoughtsTime:group + thoughtsValence:thoughtsTime + thoughtsValence:intervention +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky9, file = "models_stickiness/sticky9_gam.rda")

compareML(sticky5, sticky9) #significant difference --> sticky5 preferred



#removing ti(negIntensity_gam, posIntensity_gam)
sticky10 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                 thoughtsTime:group + thoughtsValence:thoughtsTime +
                 s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

save(sticky10, file = "models_stickiness/sticky10_gam.rda")

compareML(sticky5, sticky10) #no significant difference --> sticky10 preferred



#removing s(sleepQuality_gam)
sticky11 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                  s(negIntensity_gam) + s(posIntensity_gam) +
                  s(distracted_gam) + s(listless_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky11, file = "models_stickiness/sticky11_gam.rda")

compareML(sticky10, sticky11) #significant difference --> sticky10 preferred


#removing s(listless_gam)
sticky12 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                  s(negIntensity_gam) + s(posIntensity_gam) +
                  s(distracted_gam) + s(sleepQuality_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky12, file = "models_stickiness/sticky12_gam.rda")

compareML(sticky10, sticky12) #no significant difference --> sticky12 preferred


#removing s(distracted_gam)
sticky13 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                  s(negIntensity_gam) + s(posIntensity_gam) +
                  s(sleepQuality_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky13, file = "models_stickiness/sticky13_gam.rda")

compareML(sticky12, sticky13) #significant difference --> sticky12 preferred



#removing ti(sumNA_gam, sumPA_gam)
sticky14 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) +
                  s(negIntensity_gam) + s(posIntensity_gam) +
                  s(distracted_gam) + s(sleepQuality_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky14, file = "models_stickiness/sticky14_gam.rda")

compareML(sticky12, sticky14) #no significant difference --> sticky14 preferred


#removing  s(posIntensity_gam)
sticky15 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) +
                  s(negIntensity_gam) +
                  s(distracted_gam) + s(sleepQuality_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky15, file = "models_stickiness/sticky15_gam.rda")

compareML(sticky14, sticky15) #no significant difference --> sticky15 preferred


#removing s(negIntensity_gam)
sticky16 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) +
                  s(distracted_gam) + s(sleepQuality_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky16, file = "models_stickiness/sticky16_gam.rda")

compareML(sticky15, sticky16) #significant difference --> sticky15 preferred


#removing s(ruminating_gam)
sticky17 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                  thoughtsTime:group + thoughtsValence:thoughtsTime +
                  s(sumNA_gam) + s(sumPA_gam) +
                  s(negIntensity_gam) +
                  s(distracted_gam) + s(sleepQuality_gam) +
                  s(phaseBeepNum, by = subject, bs="fs", m=1),
                data = sc_data, method = "ML")

save(sticky17, file = "models_stickiness/sticky17_gam.rda")

compareML(sticky15, sticky17) #nsignificant difference --> sticky15 preferred

#sticky15 is the winner

sticky.m1 <- bam(stickiness_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence + thoughtsTime +
                   thoughtsTime:group + thoughtsValence:thoughtsTime +
                   s(ruminating_gam) + s(sumNA_gam) + s(sumPA_gam) +
                   s(negIntensity_gam) +
                   s(distracted_gam) + s(sleepQuality_gam) +
                   s(phaseBeepNum, by = subject, bs="fs", m=1),
                 data = sc_data)

summary_sticky.m1 <- summary(sticky.m1)

save(sticky.m1, summary_sticky.m1, file="models_stickiness/sticky.m1.rda")

plot_smooth(sticky.m1, view="phaseBeepNum", rug=F, plot_all="intervention")
plot_smooth(sticky.m1, view="phaseBeepNum", rug=F, plot_all="group")

# check autocorrelation
model1.acf <- acf_resid(sticky.m1)

#check model fit
gam.check(sticky.m1)

#plot the model based predicted differences between interventions (summed effects)
plot_parametric(sticky.m1, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(sticky.m1, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")))

# plot differences
plot_diff(sticky.m1, view="phaseBeepNum", comp=list("intervention"=c("mindfulness", "fantasizing")), main="mindfulness vs fantasizing")
plot_diff(sticky.m1, view="phaseBeepNum", comp=list( group=c("remitted", "remitted"), intervention=c("mindfulness", "fantasizing")))
#,main="mindfulness vs fantasizing")
plot_diff(sticky.m1, view="phaseBeepNum", comp=list( group=c("controls", "controls"), intervention=c("mindfulness", "fantasizing")))


# intercept model to compare with winner model
sticky.int <- bam(stickiness_gam ~ 1 + s(phaseBeepNum, by=subject, bs="fs", m=1), data=sc_data, method="ML")
summary(sticky.int)

compareML(sticky.int, sticky15) #g17 clearly outperforms the intercept model
