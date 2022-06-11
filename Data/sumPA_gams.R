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
              'thoughtsPleasant', 'restOfDayPos', 'companyPleasant', 'alonePleasant',
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
hist(data$sumPA, breaks = 20)
qqnorm(data$sumPA)
qqline(data$sumPA)

#also the change scores
hist(data$sumPA_gam, breaks = 20)
qqnorm(data$sumPA_gam)
qqline(data$sumPA_gam) #change scores are more normally distributed


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++ Creating models with gam-variables +++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#gam varialbes are NA for entire pre-phase (while diff-variables are actual - baselineMean for all data)

avg2 <- ddply(sc_data, c("group", "intervention", "phaseBeepNum", "sumPA_gam"), summarise,
              N    = length(sumPA_gam),
              sd   = sd(sumPA_gam),
              se   = sd / sqrt(N))
plot2 <- ggplot(avg2, aes(y=sumPA_gam, x=phaseBeepNum, color=intervention)) +
  geom_point(position = position_jitter(w=0.1,h=0))+ facet_grid(.~group) +
  ylab("Positive Affect Difference peri-pre")+ xlab("Phase Assessment Number") + labs(color = "Intervention") +
  geom_hline(yintercept=0) + geom_smooth()  #+ scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7"))

plot2


#Maximum model with ML
pa.max <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
                s(companyPleasant_gam) +
                s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                s(ruminating_gam) + s(sumNA_gam) +
                s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                s(phaseBeepNum, by = subject, bs="fs", m=1),
              data = sc_data, method = "ML")


pa.max1 <- bam(sumPA_gam ~ s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                s(companyPleasant_gam) +
                s(ruminating_gam) + s(sumNA_gam) +
                s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                s(phaseBeepNum, by = subject, bs="fs", m=1),
              data = sc_data, method = "ML")

pa.max2 <- bam(sumPA_gam ~ s(phaseBeepNum) + s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                 s(companyPleasant_gam) +
                 s(ruminating_gam) + s(sumNA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")

compareML(pa.max, pa.max1)
compareML(pa.max, pa.max2)
compareML(pa.max1, pa.max2) #pa.max1 is slightly preferred

#Maximum model with REML
pa.max.reml <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
                     s(companyPleasant_gam) +
                     s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                     s(ruminating_gam) + s(sumNA_gam) +
                     s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                     s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                     s(phaseBeepNum, by = subject, bs="fs", m=1),
                   data = sc_data)

summary.pa.max <- summary(pa.max)

summary.pa.max.reml <- summary(pa.max.reml)

pa.max1.reml <- bam(sumPA_gam ~ s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                 s(companyPleasant_gam) +
                 s(ruminating_gam) + s(sumNA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data)
summary(pa.max1)

save(pa.max1, pa.max.reml1, file = "models_pa/pa_Max.rda")



#removing s(phaseBeepNum, by=interaction(intervention))
pa2 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(ruminating_gam) + s(sumNA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(pa2, file = "models_pa/pa2_gam.rda")

compareML(pa.max1, pa2) #pa1 preferred


#removing s(companyPleasant_gam)
pa2a <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
             s(ruminating_gam) + s(sumNA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

compareML(pa2, pa2a) #pa2 preferred

#removing ti(negIntensity_gam, posIntensity_gam)
pa3 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa3, file = "models_pa/pa3_gam.rda")

compareML(pa2, pa3) #no significant difference --> pa3 preferred


#removing  s(posIntensity_gam)
pa4 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(negIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa4, file = "models_pa/pa4_gam.rda")

compareML(pa3, pa4) #significant difference --> pa3 preferred


#removing s(negIntensity_gam)
pa5 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(posIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa5, file = "models_pa/pa5_gam.rda")

compareML(pa3, pa5) #significant difference --> pa3 preferred


#removing s(sleepQuality_gam)
pa6 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(listless_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa6, file = "models_pa/pa6_gam.rda")

compareML(pa3, pa6) #significant difference --> pa3 preferred



#removing s(listless_gam)
pa7 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa7, file = "models_pa/pa7_gam.rda")

compareML(pa3, pa7) #significant difference --> pa3 preferred


#removing s(distracted_gam)
pa8 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) +
              s(stickiness_gam) + s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa8, file = "models_pa/pa8_gam.rda")

compareML(pa3, pa8) #no significant difference --> pa8 preferred


#removing s(stickiness_gam)
pa9 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) + s(sumNA_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) +
              s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa9, file = "models_pa/pa9_gam.rda")

compareML(pa8, pa9) #no significant difference --> pa9 preferred


#removing s(ruminating_gam)
pa10 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) +
             s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(pa10, file = "models_pa/pa10_gam.rda")

compareML(pa9, pa10) #significant difference --> pa9 preferred


#removing s(sumNA_gam)
pa11 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(ruminating_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) +
              s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data, method = "ML")

save(pa11, file = "models_pa/pa11_gam.rda")

compareML(pa9, pa11) #significant difference --> pa9 preferred

#removing  s(companyPleasant_gam)
pa12 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
             s(ruminating_gam) + s(sumNA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) +
             s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(pa12, file = "models_pa/pa12_gam.rda")

compareML(pa9, pa12) #significant difference --> pa9 preferred

#pa9 is the winner

pa.m1 <- bam(sumPA_gam ~ s(phaseBeepNum) + group * intervention +
               s(companyPleasant_gam) +
               s(ruminating_gam) + s(sumNA_gam) +
               s(negIntensity_gam) + s(posIntensity_gam) +
               s(listless_gam) + s(sleepQuality_gam) +
               s(phaseBeepNum, by = subject, bs="fs", m=1),
             data = sc_data)

summary_pa.m1 <- summary(pa.m1)

save(pa.m1, summary_pa.m1, file="models_na/pa.m1.rda")

plot_smooth(pa.m1, view="phaseBeepNum", rug=F, plot_all="intervention")
plot_smooth(pa.m1, view="phaseBeepNum", rug=F, plot_all="group")

# check autocorrelation
model1.acf <- acf_resid(pa.m1)

#check model fit
gam.check(pa.m1)

#plot the model based predicted differences between interventions (summed effects)
plot_parametric(pa.m1, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(pa.m1, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")))

# plot differences
plot_diff(pa.m1, view="phaseBeepNum", comp=list("intervention"=c("mindfulness", "fantasizing")), main="mindfulness vs fantasizing")
plot_diff(pa.m1, view="phaseBeepNum", comp=list( group=c("remitted", "remitted"), intervention=c("mindfulness", "fantasizing")))
#,main="mindfulness vs fantasizing")
plot_diff(pa.m1, view="phaseBeepNum", comp=list( group=c("controls", "controls"), intervention=c("mindfulness", "fantasizing")))


# intercept model to compare with winner model
pa.int <- bam(sumPA_gam ~ 1 + s(phaseBeepNum, by=subject, bs="fs", m=1), data=sc_data, method="ML")
summary(pa.int)

compareML(pa.int, pa9) #p9 clearly outperforms the intercept model
