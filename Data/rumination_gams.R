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

R.version.string

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
hist(data$ruminating, breaks = 20)
qqnorm(data$ruminating)
qqline(data$ruminating)

#also the change scores
hist(data$ruminating_diff, breaks = 20)
qqnorm(data$ruminating_diff)
qqline(data$ruminating_diff) #change scores are more normally distributed


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++ Creating models with gam-variables +++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#gam varialbes are NA for entire pre-phase (while diff-variables are actual - baselineMean for all data)

avg2 <- ddply(sc_data, c("group", "intervention", "phaseBeepNum", "ruminating_gam"), summarise,
              N    = length(ruminating_gam),
              sd   = sd(ruminating_gam),
              se   = sd / sqrt(N))
plot2 <- ggplot(avg2, aes(y=ruminating_gam, x=phaseBeepNum, color=intervention)) +
  geom_point(position = position_jitter(w=0.1,h=0))+ facet_grid(.~group) +
  ylab("Rumination Difference peri-pre")+ xlab("Phase Assessment Number") + labs(color = "Intervention") +
  geom_hline(yintercept=0) + geom_smooth()  #+ scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7"))

plot2


#Maximum model with ML
gam.max <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence * thoughtsTime +
                   aloneCompany +  s(alonePleasant) + s(companyPleasant_gam) +
                   s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                   s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                   s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                   s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                   s(phaseBeepNum, by = subject, bs="fs", m=1),
                   data = sc_data, method = "ML")
#not enought non-NA data

gam.max <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence * thoughtsTime +
                 s(alonePleasant) + s(companyPleasant_gam) +
                 s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                 s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")
#not enought non-NA data

gam.max <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence +
                 s(alonePleasant) + s(companyPleasant_gam) +
                 s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                 s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")
#not enought non-NA data

gam.max <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
                 s(alonePleasant) + s(companyPleasant_gam) +
                 s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                 s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")
#not enought non-NA data

gam.max <- bam(ruminating_gam ~ s(phaseBeepNum) + s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                 s(companyPleasant_gam) +
                 s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                 s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                 s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                 s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                 s(phaseBeepNum, by = subject, bs="fs", m=1),
               data = sc_data, method = "ML")


#Maximum model with REML
gam.max.reml <- bam(ruminating_gam ~ s(phaseBeepNum) + s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                      s(companyPleasant_gam) +
                      s(phaseBeepNum, by=group) + s(phaseBeepNum, by=intervention) +
                      s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
                      s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
                      s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
                      s(phaseBeepNum, by = subject, bs="fs", m=1),
                    data = sc_data)

summary.gam.max <- summary(gam.max)

summary.gam.max.reml <- summary(gam.max.reml)

save(gam.max, gam.max.reml, summary.gam.max, summary.gam.max.reml, file = "models_rumination/gam_Max.rda")


#removing s(phaseBeepNum, by=intervention)
g0 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
            s(companyPleasant_gam) +
            s(phaseBeepNum, by=group) + s(phaseBeepNum, by = intervention) +
            s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
            s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
            s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
            s(phaseBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(g0, file = "models_rumination/g0_gam.rda")

compareML(gam.max, g0) # g0 preferred

#removing s(phaseBeepNum, by=intervention)
g1 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
            s(companyPleasant_gam) +
            s(phaseBeepNum, by=group) +
            s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
            s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
            s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
            s(phaseBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(g1, file = "models_rumination/g1_gam.rda")

compareML(gam.max, g1) # g1 preferred


#removing s(phaseBeepNum, by=group)
g1a <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
            s(companyPleasant_gam) +
            s(phaseBeepNum, by=intervention) +
            s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
            s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
            s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
            s(phaseBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(g1a, file = "models_rumination/g1a_gam.rda")

compareML(gam.max, g1a) #g1a preferred


#removing both s(phaseBeepNum, by=group) and s(phaseBeepNum, by=intervention)
g2 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
            s(companyPleasant_gam) +
            s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
            s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
            s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
            s(phaseBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(g2, file = "models_rumination/g2_gam.rda")

compareML(g1, g2) #no significant difference --> g2 preferred
compareML(g1a, g2) #no significant difference --> g2 preferred


#removing ti(negIntensity_diff, posIntensity_diff)
g3 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g3, file = "models_rumination/g3_gam.rda")

compareML(g2, g3) #significant difference --> g2 preferred


#removing ti(sumNA_gam, sumPA_gam)
g4 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) + s(sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g4, file = "models_rumination/g4_gam.rda")

compareML(g2, g4) #significant difference --> g2 preferred


#removing s(sleepQuality_gam)
g5 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(listless_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g5, file = "models_rumination/g5_gam.rda")

compareML(g2, g5) #significant difference --> g2 preferred


#removing s(listless_gam)
g6 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g6, file = "models_rumination/g6_gam.rda")

compareML(g2, g6) #significant difference --> g2 preferred


#removing s(distracted_gam)
g7 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g7, file = "models_rumination/g7_gam.rda")

compareML(g2, g7) #significant difference --> g2 preferred



#removing s(stickiness_gam)
g8 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(companyPleasant_gam) +
             s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g8, file = "models_rumination/g8_gam.rda")

compareML(g2, g8) #significant difference --> g2 preferred


#removing s(companyPleasant_gam)
g9 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
             s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
             s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
             s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(g9, file = "models_rumination/g9_gam.rda")

compareML(g2, g9) #significant difference --> g2 preferred

#g2 is the winner

gam1 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention +
              s(companyPleasant_gam) +
              s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
              s(negIntensity_gam) + s(posIntensity_gam) + ti(negIntensity_gam, posIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data)

summary_gam1 <- summary(gam1)

save(gam1, summary_gam1, file="gam1.rda")

plot_smooth(gam1, view="phaseBeepNum", rug=F, plot_all="intervention", main="Change in rumination peri-intervention")
plot_smooth(gam1, view="phaseBeepNum", rug=F, plot_all="group")

# check autocorrelation
model1.acf <- acf_resid(gam1)

#check model fit
gam.check(gam1)

#plot the model based predicted differences between interventions (summed effects)
plot_parametric(gam1, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(gam1, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")))

# plot differences
plot_diff(gam1, view="phaseBeepNum", comp=list("intervention"=c("mindfulness", "fantasizing")), main="mindfulness vs fantasizing")
plot_diff(gam1, view="phaseBeepNum", comp=list( group=c("remitted", "remitted"), intervention=c("mindfulness", "fantasizing")))
#,main="mindfulness vs fantasizing")
plot_diff(gam1, view="phaseBeepNum", comp=list( group=c("controls", "controls"), intervention=c("mindfulness", "fantasizing")))



# intercept model to compare with winner model
rum.int <- bam(ruminating_gam ~ 1 + s(phaseBeepNum, by=subject, bs="fs", m=1), data=sc_data, method="ML")
summary(rum.int)

compareML(rum.int, g2) #g2 clearly outperforms the intercept model


########################### Recreating model with thoughtsTime split ################################################
sc_data$thoughtsPast <- 0
sc_data$thoughtsPresent <- 0
sc_data$thoughtsFuture <- 0

sc_data[which(sc_data$thoughtsTime == 1),]$thoughtsPast <- 1
sc_data[which(sc_data$thoughtsTime == 2),]$thoughtsPresent <- 1
sc_data[which(sc_data$thoughtsTime == 3),]$thoughtsFuture <- 1
sc_data[which(is.na(sc_data$thoughtsTime)),]$thoughtsPast <- NA
sc_data[which(is.na(sc_data$thoughtsTime)),]$thoughtsPresent <- NA
sc_data[which(is.na(sc_data$thoughtsTime)),]$thoughtsFuture <- NA


gam2 <- bam(ruminating_gam ~ s(phaseBeepNum) + group * intervention * thoughtsValence +
              thoughtsPast + thoughtsPast:group:intervention:thoughtsValence + thoughtsPast:group +
              thoughtsPast:intervention + thoughtsPast:thoughtsValence +
              thoughtsPresent + thoughtsPresent:group:intervention:thoughtsValence + thoughtsPresent:group +
              thoughtsPresent:intervention + thoughtsPresent:thoughtsValence +
              thoughtsFuture + thoughtsFuture:group:intervention:thoughtsValence + thoughtsFuture:group +
              thoughtsFuture:intervention + thoughtsFuture:thoughtsValence +
              s(sumNA_gam) + s(sumPA_gam) + ti(sumNA_gam, sumPA_gam) +
              s(posIntensity_gam) +
              s(stickiness_gam) + s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_data)

summary(gam2)













###########################################################################################################
########################################## GAMS with change scores ########################################
###########################################################################################################
#Minimum model
model.min <- bam(ruminating_diff ~ s(blockBeepNum, by = intervention, k = 6) + group * intervention * phase +
                   s(blockBeepNum, by = subject, bs="fs", m=1),
                 data=sc_data, method = "ML")

summary(model.min)
plot_smooth(model.min, view="blockBeepNum", rug=F, plot_all="intervention")
plot_smooth(model.min, view="blockBeepNum", rug=F, plot_all="group")

# check autocorrelation
model.min.acf <- acf_resid(model.min)

#check model fit
gam.check(model.min)

#plot the model based predicted differences between interventions (summed effects)
plot_parametric(model.min, pred=list(intervention=c("mindfulness", "fantasizing")))

# plot differences
plot_diff(model.min, view="blockBeepNum", comp=list("intervention"=c("mindfulness", "fantasizing")), main="mindfulness vs fantasizing")


# #I adapted this loop from some online resource (stackoverflow), but can't find the exact link anymore
# ks <- c(3:15) 
# nn<-1
# aic<-0
# bic<-0
# kt<-""
# for(k in ks){
#   fit <- bam(ruminating_diff ~ s(blockBeepNum, by = intervention, k = 6) + group * intervention * phase +
#               s(blockBeepNum, by = subject, bs="fs", m=1, k = k),
#             data=sc_data, method = "ML")
#   
#   aic[nn]<-AIC(fit)
#   kt[nn]<-k
#   nn<-nn+1
# }
# 
# result<-data.frame(kt, aic)
# #head(result)
# result[which.min(result$aic),]$kt
# best_k <- as.numeric(result[which.min(result$aic),]$kt) #k=15 is best according to AIC
# #but because it is much more computationally expensive, I would opt for a somewhat lower k

#in the end it seems like the best course of action is to go with the default settings.
model.min <- bam(ruminating_diff ~ s(blockBeepNum, by = intervention) + group * intervention * phase +
                   s(blockBeepNum, by = subject, bs="fs", m=1),
                 data=sc_data, method = "ML")

gam.check(model.min)

avg2 <- ddply(sc_data, c("group", "intervention", "blockBeepNum", "ruminating_diff"), summarise,
              N    = length(ruminating_diff),
              sd   = sd(ruminating_diff),
              se   = sd / sqrt(N))
plot2 <- ggplot(avg2, aes(y=ruminating_diff, x=blockBeepNum, color=intervention)) +
  geom_point(position = position_jitter(w=0.1,h=0))+ facet_grid(.~group) +
  ylab("Rumination Difference peri-pre")+ xlab("Assessment Number") + labs(color = "Intervention") +
  geom_hline(yintercept=0) + geom_smooth()  #+ scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7"))

plot2



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++  Creating models with diff-variables ++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###########################################################################################################
###################################### Determining RE-structure ###########################################
###########################################################################################################
model.min.reml <- bam(ruminating_diff ~ s(blockBeepNum, by = intervention) + group * intervention * phase +
                        s(blockBeepNum, by = subject, bs="fs", m=1),
                      data=sc_data)

summary(model.min.reml) #significant factor smooths --> keep RE-structure as is

###########################################################################################################
##################################### Backwards-fitting for FE-structure ##################################
###########################################################################################################

#Maximum model with ML
model.max <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence * thoughtsTime +
                   aloneCompany + s(blockBeepNum, by=group) +
                   s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
                   s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
                   s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
                   s(blockBeepNum, by = subject, bs="fs", m=1),
                 data = sc_data, method = "ML")

#Maximum model with REML
model.max.reml <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence * thoughtsTime +
                        aloneCompany + s(blockBeepNum, by=group) +
                        s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
                        s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
                        s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
                        s(blockBeepNum, by = subject, bs="fs", m=1),
                      data = sc_data)

summary_max <- summary(model.max)

summary_max_reml <- summary(model.max.reml)

save(model.max, model.max.reml, summary_max, summary_max_reml, file = "modelMax.rda")

#removing aloneCompany
m1 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence * thoughtsTime +
            s(blockBeepNum, by=group) +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m1, file = "models_rumination/m1_diff.rda")

compareML(model.max, m1) #only small difference in AIC --> m1 preferred


#adding s(blockBeepNum, by = intervention)
m1b <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(blockBeepNum, by=group) + s(blockBeepNum, by = intervention) +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
             s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m1b, file = "models_rumination/m1b_diff.rda")

compareML(m1b, m1) #significant difference --> m1 preferred


#removing s(blockBeepNum, by = group)
m1c <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
             s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m1c, file = "models_rumination/m1c_diff.rda")

compareML(m1c, m1) #significant difference --> m1c preferred


#removing interaction thoughtsTime:phase
m2 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence + thoughtsTime +
            thoughtsTime:group + thoughtsTime:intervention +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m2, file = "models_rumination/m2_diff.rda")

compareML(m2, m1c) #significant difference --> m1c preferred


#removing interaction thoughtsTime:intervention
m3 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence + thoughtsTime +
            thoughtsTime:group + thoughtsTime:phase +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m3, file = "models_rumination/m3_diff.rda")

compareML(m3, m1c) #significant difference --> m1c preferred


#removing interaction thoughtsTime:group
m4 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsValence + thoughtsTime +
            thoughtsTime:intervention + thoughtsTime:phase +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m4, file = "models_rumination/m4_diff.rda")

compareML(m4, m1c) #significant difference --> m1c preferred


#removing interaction thoughtsValence:phase
m5 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime + thoughtsValence +
            thoughtsValence:group + thoughtsValence:intervention +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m5, file = "models_rumination/m5_diff.rda")

compareML(m5, m1c) #significant difference --> m1c preferred

#removing interaction thoughtsValence:intervention
m6 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime + thoughtsValence +
            thoughtsValence:group + thoughtsValence:phase +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m6, file = "models_rumination/m6_diff.rda")

compareML(m6, m1c) #significant difference --> m1 preferred


#removing interaction thoughtsValence:group
m7 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime + thoughtsValence +
            thoughtsValence:intervention + thoughtsValence:phase +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) + ti(negIntensity_diff, posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m7, file = "models_rumination/m7_diff.rda")

compareML(m7, m1c) #significant difference --> m1 preferred


#removing ti(negIntensity_diff, posIntensity_diff)
m8 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
            s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m8, file = "models_rumination/m8_diff.rda")

compareML(m8, m1c) #no significant difference --> m8 preferred


#removing ti(sumNA_diff, sumPA_diff)
m9 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
            s(sumNA_diff) + s(sumPA_diff) +
            s(negIntensity_diff) + s(posIntensity_diff) +
            s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
            s(blockBeepNum, by = subject, bs="fs", m=1),
          data = sc_data, method = "ML")

save(m9, file = "models_rumination/m9_diff.rda")

compareML(m9, m8) #significant difference --> m8 preferred


#removing s(sleepQuality)
m10 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) + s(posIntensity_diff) +
             s(stickiness_diff) + s(distracted_diff) + s(listless_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m10, file = "models_rumination/m10_diff.rda")

compareML(m8, m10) #significant difference --> m8 preferred


#removing s(listless_diff)
m11 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) + s(posIntensity_diff) +
             s(stickiness_diff) + s(distracted_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m11, file = "models_rumination/m11_diff.rda")

compareML(m8, m11) #significant difference --> m8 preferred


#removing s(distracted_diff)
m12 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) + s(posIntensity_diff) +
             s(stickiness_diff) + s(listless_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m12, file = "models_rumination/m12_diff.rda")

compareML(m8, m12) #significant difference --> m8 preferred


#removing s(stickiness_diff)
m13 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) + s(posIntensity_diff) +
             s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m13, file = "models_rumination/m13_diff.rda")

compareML(m8, m13) #significant difference --> m8 preferred


#removing s(posIntensity_diff) 
m14 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(negIntensity_diff) +
             s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m14, file = "models_rumination/m14_diff.rda")

compareML(m8, m14) #no significant difference --> m14 preferred


#removing s(negIntensity_diff) 
m15 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
             s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
             s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
             s(blockBeepNum, by = subject, bs="fs", m=1),
           data = sc_data, method = "ML")

save(m15, file = "models_rumination/m15_diff.rda")

compareML(m15, m14) #significant difference --> m14 preferred


#m14 is the winning model
#refitting m14 with REML
model1 <- bam(ruminating_diff ~ s(blockBeepNum) + group * intervention * phase * thoughtsTime * thoughtsValence +
                s(sumNA_diff) + s(sumPA_diff) + ti(sumNA_diff, sumPA_diff) +
                s(negIntensity_diff) +
                s(stickiness_diff) + s(distracted_diff) + s(listless_diff) + s(sleepQuality_diff) +
                s(blockBeepNum, by = subject, bs="fs", m=1),
              data = sc_data)

summary_model1 <- summary(model1)

save(model1, summary_model1, file = "model1.rda")

plot_smooth(model1, view="blockBeepNum", rug=F, plot_all="intervention")
plot_smooth(model1, view="blockBeepNum", rug=F, plot_all="group")

# check autocorrelation
model1.acf <- acf_resid(model1)

#check model fit
gam.check(model1)

#plot the model based predicted differences between interventions (summed effects)
plot_parametric(model1, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(model1, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")))

# plot differences
plot_diff(model1, view="blockBeepNum", comp=list("intervention"=c("mindfulness", "fantasizing")), main="mindfulness vs fantasizing")
plot_diff(model1, view="blockBeepNum", comp=list( group=c("remitted", "remitted"), intervention=c("mindfulness", "fantasizing")))
#,main="mindfulness vs fantasizing")
plot_diff(model1, view="blockBeepNum", comp=list( group=c("controls", "controls"), intervention=c("mindfulness", "fantasizing")))













