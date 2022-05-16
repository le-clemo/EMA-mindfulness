rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

library(ggplot2)
library(data.table)
library(lme4)
library(mgcv)
library(itsadug)
library(dplyr)
library(plyr)
library(lmerTest) #to more quickly be able to see significance
library(plotfunctions)
library(gratia)
library(rms)

R.version.string
packageVersion("mgcv")
packageVersion("itsadug")

#load preprocessed ESM data
data <- read.csv('ESM/mindcog_v202204/preprocessed_data.csv') 

#set factors
data$group <- factor(data$group, levels = c("controls", "remitted"))
data$subject <- as.factor(data$subject)
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))
data$phase = relevel(data$phase, ref="pre")
data$grInt <- as.factor(paste(data$group, data$intervention, sep = "."))
data$grIntPhase <- as.factor(paste(data$grInt, data$phase, sep = "."))
data$grIntPhase = relevel(data$grIntPhase, ref="controls.fantasizing.pre")
data$blockPhase <- as.factor(paste(data$phase, data$block, sep = "."))
data$blockPhase = relevel(data$blockPhase, ref="pre.1")
data$thinkingOf <- as.factor(data$thinkingOf)
data$thoughtsValence <- as.factor(data$thoughtsValence)
data$thoughtsTime <- as.factor(data$thoughtsTime)
data$thoughtsObject <- as.factor(data$thoughtsObject)
data$aloneCompany <- as.factor(data$aloneCompany)
data$sleepScore <- data$sleepQuality + data$restednessWakeup

#set sleepScore for every assessment per day
for(id in unique(data$subject)){
  respondent_rows <- which(data$subject == id)
  current_day <- 0
  for(row in respondent_rows){
    if((data$assessmentDay[row] != current_day) & (!is.na(data$sleepScore[row]))){
      sleep_score <- data$sleepScore[row]
      current_day <- data$assessmentDay[row]
    } else if((data$assessmentDay[row] == current_day) & (is.na(data$sleepScore[row]))){
      data$sleepScore[row] <- sleep_score
    }
  }
}
#same for sleepQuality
for(id in unique(data$subject)){
  respondent_rows <- which(data$subject == id)
  current_day <- 0
  for(row in respondent_rows){
    if((data$assessmentDay[row] != current_day) & (!is.na(data$sleepScore[row]))){
      sleep_score <- data$sleepScore[row]
      current_day <- data$assessmentDay[row]
    } else if((data$assessmentDay[row] == current_day) & (is.na(data$sleepScore[row]))){
      data$sleepScore[row] <- sleep_score
    }
  }
}

#ruminating
hist(data$ruminating)
qqnorm(data$ruminating)
qqline(data$ruminating)


#sumNA
hist(data$sumNA)
qqnorm(data$sumNA)
qqline(data$sumNA)


#create a scaled version of data
pvars <- c( "ruminating_lag1", "sumNA", "stressed", "listless", "stickiness",# Scaling numeric parameters
            "sumPA", "distracted", "companyPleasant", "alonePleasant", "posMax", "posIntensity", "negMax",
            "negIntensity", "restOfDayPos", "sleepScore", "sleepQuality")

sc_data <- copy(data)
sc_data[pvars] <- lapply(data[pvars],scale)

sc_data$subjB <- interaction(sc_data$subject, sc_data$block, drop = TRUE)
sc_data$subjB <- as.factor(sc_data$subjB)

sc_data$subjPh <- interaction(sc_data$subject, sc_data$phase, sc_data$block, drop = TRUE)
sc_data$subjPh <- as.factor(sc_data$subjPh)



#number of participants so far
length(unique(sc_data$subject)) #39 associated with a group
participant_responses <- ddply(sc_data, .(subject), plyr::summarise,
                               numCompleted = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numCompleted - noResponse,
                               responseRate = round(response/numCompleted,2),
                               numDays = max(assessmentDay))

meanResponseRate <- mean(participant_responses$responseRate) #the mean response rate is ~68%
length(unique(participant_responses[which(participant_responses$responseRate >= meanResponseRate),]$subject)) #21
length(unique(participant_responses[which(participant_responses$responseRate >= 0.6),]$subject)) #26
length(unique(participant_responses[which(participant_responses$responseRate >= 0.5),]$subject)) #33
pp <- unique(participant_responses[which(participant_responses$responseRate >= 0.6),]$subject)
sc_data <- sc_data[which(sc_data$subject %in% pp),]



#number of participants so far
length(unique(sc_data$subjB)) #62 subjB (same subject, different block --> viewed as separate)
responses_block <- ddply(sc_data, .(subjB), plyr::summarise,
                               numCompleted = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numCompleted - noResponse,
                               responseRate = round(response/numCompleted,2),
                               numDays = max(assessmentDay))


meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.5%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subjB)) #34
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)) #43
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subjB)) #51

#removing participants with a response rate lower than 60%
# pp <- unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)
# sc_data <- sc_data[which(sc_data$subjB %in% pp),]


################################ blocks combined #############################
# t2 <- bam(ruminating_lag1 ~ blockAssessmentDay * grInt * phase * sumNA * sumPA * stressed * listless * thinkingOf *
#             stickiness * thoughtsPleasant * thoughtsTime * thoughtsValence * thoughtsObject * distracted *
#             restOfDayPos * aloneCompany * companyPleasant * alonePleasant * posMax * posIntensity *
#             negMax * negIntensity + s(blockAssessmentDay, by = subjB, bs = 'fs', m = 1) +
#             s(blockAssessmentDay, by = grInt), data = sc_data)


m1 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsObject * thoughtsTime * thoughtsValence * aloneCompany +
            # s(beepNum, by = group) + s(beepNum, by = thoughtsObject) + s(beepNum, by = thoughtsTime) + s(beepNum, by=thoughtsValence) +
            # s(beepNum, by = aloneCompany) +
            s(beepNum, by = blockPhase) +
            s(beepNum, by = intervention) + s(sumNA) + s(sumPA) + ti(sumNA, sumPA) +
            s(stressed) + s(distracted) + s(restOfDayPos) +
            # s(sumNA, by=group) + s(stressed, by=group) + s(stressed, by = group) +
            # s(sumNA, by=intervention) + s(sumPA, by=group) + s(sumPA, by=intervention) +
            # s(posMax) + s(posIntensity) + s(negMax) + s(negIntensity) + 
            # ti(posMax, posIntensity) + ti(negMax, negIntensity) +
            # s(stickiness) + 
            s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +# + s(intervention, by=subject, bs="fs", m=1) +
            s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, family = "scat")

summary(m1)

fastbw(m1) #give it a try...

plot_smooth(m1, view="beepNum", 
            plot_all="group")

plot_diff(m1, view="beepNum", 
          comp=list(group=c("remitted", "controls")))

#to see differences in groups
m1b <- bam(sumNA_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence *
             thoughtsObject * aloneCompany + s(blockBeepNum, by = group) + s(blockBeepNum, by = blockPhase) + 
             s(blockBeepNum, by = intervention) + s(stickiness) + s(stressed) + s(distracted) + s(listless) +
             ti(stickiness, stressed) + ti(ruminating, stickiness) + ti(ruminating, stressed) +
             s(ruminating) + s(sumPA) + ti(ruminating, sumPA) +
             s(blockBeepNum, by = subjB, bs="fs", m=1) + s(group, bs="re") + s(blockPhase, bs="re"),
          data = sc_data)

summary(m1b)

plot_smooth(m1b, view="blockBeepNum", 
            plot_all="group")
abline(v=70, lty = "dashed")

plot_diff(m1b, view="blockBeepNum", 
          comp=list(group=c("remitted", "controls")))
abline(v=70, lty = "dashed")

acf_resid(m1b)
#abline(v=150)
#draw(t2)

#View(subset(data[which(data$subject == "s81"),], select = c("subject", "block", "phase", "beepNum", "phaseBeepNum", "blockBeepNum")))


############################################# try with half day means #################################################################
# 
# sc_melt <- melt(sc_data, id.vars = c("subject", "subjB", "group", "intervention", "phase", "block", "halfDay"),
#                 measure.vars = c("ruminating_lag1", "sumNA", "stressed", "listless", "stickiness",# Scaling numeric parameters
#             "sumPA", "distracted", "companyPleasant", "alonePleasant", "posMax", "posIntensity", "negMax",
#             "negIntensity", "restOfDayPos", "sleepScore", "ruminating", "sumNA_lag1"), na.rm=TRUE)


sc_melt <- with(sc_data, aggregate(list(ruminating=ruminating, ruminating_lag1=ruminating_lag1, sumNA=sumNA, sumNA_lag1=sumNA_lag1,
                                        sumPA=sumPA, stressed=stressed, listless=listless, stickiness=stickiness, distracted=distracted,
                                        companyPleasant=companyPleasant, alonePleasant=alonePleasant, posMax=posMax, posIntensity=posIntensity, negMax=negMax,
                                        negIntensity=negIntensity, sleepScore=sleepScore, sleepQuality=sleepQuality, restOfDayPos=restOfDayPos),
                                   by = list(subject=subject, subjB=subjB, group=group, intervention=intervention, grInt=grInt, phase=phase,
                                             block=block, blockPhase=blockPhase, grIntPhase=grIntPhase, halfDay=halfDay),
                                   FUN = function(x) {mon.mean = mean(x, na.rm=TRUE)}))

sc_melt <- sc_melt[!is.na(sc_melt$ruminating),]
sc_melt <- sc_melt[!is.na(sc_melt$ruminating_lag1),]


sc_melt[which(is.na(sc_melt$sleepQuality)),]$sleepQuality <- 0

######################################### some visualizations ###############################################################

# 
# plt_labs <- labs(y = 'Rumination',
#                  x = 'Half days',
#                  colour = 'Group.Intervention')
# ggplot(sc_melt, aes(x = blockHalfDay, y = ruminating,
#                     group = subjB, colour = grInt)) +
#   geom_line() + geom_vline(xintercept=14) +
#   facet_wrap(~ grInt, ncol = 2) +
#   plt_labs

##############################################################################################################################


#Note: Jacolien van Rij-Tange says to stick to your hypotheses when including interaction terms (ti, te)
#So don't just try to include every possible interaction!

#to see differences in groups
t1 <- bam(ruminating ~ group * intervention * blockPhase + s(halfDay, by = group) + s(halfDay, by = blockPhase) +
            s(halfDay, by = intervention) + s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(stressed) + s(distracted) +
            s(sumNA, by=group) + s(stressed, by=group) + s(companyPleasant, by = group) + s(stressed, by = group) +
            s(sumPA, by=group) + s(sumPA, by=intervention) +
             s(halfDay, by = intervention) + s(posMax) + s(posIntensity) + s(negMax) + s(negIntensity) +
            ti(posMax, posIntensity) + ti(negMax, negIntensity) +
            s(stickiness) + s(companyPleasant) +
             s(halfDay, by = subject, bs="fs", m=1) + s(group, bs="re") + s(blockPhase, bs="re") + s(intervention, bs="re"),
           data = sc_melt, family = "scat")

summary(t1)

plot_smooth(t1, view="halfDay", 
            plot_all="group")
abline(v=28, lty="dashed")

plot_diff(t1, view="halfDay", 
          comp=list(group=c("remitted", "controls")))
abline(v=28, lty="dashed")

acf_resid(t1)


t2 <- bam(ruminating ~ grInt * blockPhase + s(halfDay, by = grInt) + s(halfDay, by = blockPhase) +
            s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(stressed) + s(distracted) +
            s(sumNA, by=grInt) + s(stressed, by=grInt) + s(companyPleasant, by = grInt) + s(stressed, by = grInt) +
            s(sumPA, by=grInt) + s(posMax) + s(posIntensity) + s(negMax) + s(negIntensity) +
            ti(posMax, posIntensity) + ti(negMax, negIntensity) +
            s(stickiness) + s(companyPleasant) +
            s(halfDay, by = subject, bs="fs", m=1) + s(grInt, bs="re") + s(blockPhase, bs="re"),
          data = sc_melt)

summary(t2)

plot_smooth(t2, view="halfDay", 
            plot_all="grInt")
abline(v=28, lty="dashed")

plot_diff(t2, view="halfDay", 
          comp=list(grInt=c("remitted", "controls")))
abline(v=28, lty="dashed")

plot_diff(t2, view="halfDay", 
          comp=list(grInt=c("remitted.mindfulness", "remitted.fantasizing")))
abline(v=28, lty="dashed")

plot_diff(t2, view="halfDay", 
          comp=list(grInt=c("controls.mindfulness", "controls.fantasizing")))
abline(v=28, lty="dashed")






################################################################################################################################



if(FALSE){
  t2 <- gam(ruminating ~ grInt * blockPhase + s(halfDay, by = grInt) + s(halfDay, by = blockPhase) + 
              s(stickiness) + s(stressed) + s(listless) +
              ti(sumNA, stickiness) + ti(sumNA, stressed) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) +
              s(halfDay, by = subjB, bs="fs", m=1) + s(grInt, bs="re") + s(blockPhase, bs="re"),
            data = sc_melt)
  
  summaryT2 <- summary(t2)
  
  save(t2, summaryT2, file="ruminating_grInt_blockPhase_halfDay.rda", compress="xz")
} else{
  load("ruminating_grInt_blockPhase_halfDay.rda")
}
#combining group and intervention


plot_smooth(t2, view="halfDay", 
            plot_all="grInt")
abline(v=14, lty="dashed")

plot_diff(t2, view="halfDay", 
          comp=list(grInt=c("remitted.mindfulness", "remitted.fantasizing")))
abline(v=14, lty="dashed")

acf_resid(t2)




phase_melt <- with(sc_data, aggregate(list(ruminating=ruminating, ruminating_lag1=ruminating_lag1, sumNA=sumNA, sumNA_lag1=sumNA_lag1,
                                        sumPA=sumPA, stressed=stressed, listless=listless, stickiness=stickiness, distracted=distracted,
                                        companyPleasant=companyPleasant, posMax=posMax, posIntensity=posIntensity, negMax=negMax,
                                        negIntensity=negIntensity, sleepScore=sleepScore, sleepQuality=sleepQuality, restOfDayPos=restOfDayPos),
                                   by = list(subject=subject, subjPh=subjPh, group=group, intervention=intervention, grInt=grInt, phase=phase,
                                             block=block, grIntPhase=grIntPhase, phaseHalfDay=phaseHalfDay),
                                   FUN = function(x) {mon.mean = mean(x, na.rm=TRUE)}))

phase_melt <- phase_melt[!is.na(phase_melt$ruminating),]
phase_melt <- phase_melt[!is.na(phase_melt$ruminating_lag1),]


if(FALSE){
  t3 <- bam(ruminating ~ grIntPhase + s(phaseHalfDay, by = grIntPhase) + 
              s(stickiness) + s(stressed) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) +
              s(phaseHalfDay, by = subjPh, bs="fs", m=1) + s(grIntPhase, bs="re"),
            data = phase_melt)
  
  summaryT3 <- summary(t3)
  
  save(t3, summaryT3, file="ruminating_grIntPhase_phasehalfDay.rda", compress="xz")
} else{
  load("ruminating_grInt_blockPhase_halfDay.rda")
}


plot_smooth(t3, view="phaseHalfDay", 
            plot_all="grIntPhase")

plot_diff(t3, view="phaseHalfDay", 
          comp=list(grIntPhase=c("remitted.fantasizing.peri", "remitted.fantasizing.pre")))

acf_resid(t3)




                                                             

