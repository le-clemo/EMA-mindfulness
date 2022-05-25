rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

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
data <- read.csv('preprocessed_data.csv') 



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
data$subjB <- interaction(data$subject, data$block, drop = TRUE)

met.vars <- c('ruminating', 'stickiness', 'sumNA',  'down', 'irritated', 'restless', 'anxious',
              'sumPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

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


##################################### Distribution ###############################################
qqnorm(data$ruminating)
qqline(data$ruminating)


qqp(data$ruminating, "norm")
qqp(data$ruminating, "lnorm")

t.dist <- fitdistr(test$ruminating, "t")
qqp(test$ruminating, "t", df=1)


nbinom <- fitdistr(test$ruminating, "Negative Binomial")
qqp(test$ruminating, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

gamma <- fitdistr(test$ruminating, "gamma")
qqp(test$ruminating, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])


test <- data[which(!is.na(data$ruminating)),]
test$ruminating <- test$ruminating + 1

g1 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
            s(blockBeepNum, by = subjB, bs="fs", m=1),
          data=test, family = "Gamma")
################################### simple model #################################################

if(FALSE){
  s1 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data, family = "scat")
  
  summary_s1 <- summary(s1)
  
  save(s1, summary_s1, file="s1.rda", compress="xz")
} else {
  load("s1.rda")
}


plot_smooth(s1, view="blockBeepNum", plot_all = "group")

plot_diff(s1, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(s1))
qqline(resid(s1))

resid_s1 <- resid(s1)
match(c(min(resid_s1),max(resid_s1)), resid_s1) #1980, 621


x <- resid(s1)
h <- hist(x, col = "red", breaks = 40)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

# PLOT 2:
plot(fitted(s1), resid(s1))
abline(h=0)

acf_resid(s1)





if(FALSE){
  s1b <- gam(ruminating ~ s(blockBeepNum) + grIntPhase + s(blockBeepNum, by=grIntPhase) +
               s(blockBeepNum, by = subjB, bs="fs", m=1),
             data=sc_data, family = "scat")
  
  summary_s1b <- summary(s1b)
  
  save(s1b, summary_s1b, file="s1b.rda", compress="xz")
} else {
  load("s1b.rda")
}


plot_smooth(s1b, view="blockBeepNum", 
            plot_all="grIntPhase")

for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){
    g1 <- paste(g, int, "peri", sep = ".")
    g2 <- paste(g, int, "pre", sep = ".")
    print(plot_diff(s1b, view="blockBeepNum", 
                    comp=list(grIntPhase=c(g1, g2))))
  }
}




if(FALSE){
  m1 <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
              s(blockBeepNum, by = subjB, bs="fs", m=1),
            data=sc_data)
  
  summary_m1 <- summary(m1)
  
  save(m1, summary_m1, file="m1.rda", compress="xz")
} else {
  load("m1.rda")
}

plot_smooth(m1, view="blockBeepNum", plot_all = "group")

plot_diff(m1, view = "blockBeepNum", comp = list(group = c("controls", "remitted")))

qqnorm(resid(m1))
qqline(resid(m1))

plot(fitted(m1), resid(m1))
abline(h=0)



if(FALSE){
  
  m1b <- gam(ruminating ~ s(blockBeepNum) + grIntPhase + s(blockBeepNum, by=grIntPhase) +
               s(blockBeepNum, by = subjB, bs="fs", m=1),
             data=sc_data)
  
  summary_m1b <- summary(m1b)
  
  save(m1b, summary_m1b, file="m1b.rda", compress="xz")
} else {
  load("m1b.rda")
}


plot_smooth(m1b, view="blockBeepNum", 
            plot_all="grIntPhase")

for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){
    g1 <- paste(g, int, "peri", sep = ".")
    g2 <- paste(g, int, "pre", sep = ".")
    print(plot_diff(m1b, view="blockBeepNum", 
                    comp=list(grIntPhase=c(g1, g2))))
  }
}








# m1c <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
#             s(blockBeepNum, by = subjB, bs="fs", m=1),
#           data=data)
# 
# 
# summary_m1c <- summary(m1c)
# 
# save(m1c, summary_m1c, file="m1c_unscaled.rda", compress="xz")
# 
# 
# plot(fitted(m1c), resid(m1c))
# abline(h=0)
# 
# 
# #excluding non responses
# m1d <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase + s(blockBeepNum, by=group) +
#              s(blockBeepNum, by = subjB, bs="fs", m=1),
#            data=data[which(is.na(data$mindcog_db_non_response)),])
# 
# 
# summary_m1d <- summary(m1d)
# 
# save(m1d, summary_m1d, file="m1d_unscaled_exclNonResp.rda", compress="xz")
# 
# 
# plot(fitted(m1d), resid(m1d))
# abline(h=0)



################################ blocks combined #############################
# t2 <- bam(ruminating_lag1 ~ blockAssessmentDay * grInt * phase * sumNA * sumPA * stressed * listless * thinkingOf *
#             stickiness * thoughtsPleasant * thoughtsTime * thoughtsValence * thoughtsObject * distracted *
#             restOfDayPos * aloneCompany * companyPleasant * alonePleasant * posMax * posIntensity *
#             negMax * negIntensity + s(blockAssessmentDay, by = subjB, bs = 'fs', m = 1) +
#             s(blockAssessmentDay, by = grInt), data = sc_data)


if(FALSE){
  m1 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
              aloneCompany_lag1 +
              s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
              s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
              s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
              s(distracted_lag1) + s(stickiness_lag1) + s(listless_lag1) + s(negMax_lag1) + s(posMax_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data)
  
  summarym1 <- summary(m1)
  
  save(m1, summarym1, file="ruminating_comprehensive_model1.rda", compress="xz")
} else{
  load("ruminating_comprehensive_model1.rda")
}


if(FALSE){
  m1b <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
              aloneCompany_lag1 +
              s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
              s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
              s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
              s(distracted_lag1) + s(stickiness_lag1) + s(listless_lag1) + s(negMax_lag1) + s(posMax_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re"),
            data = sc_data)
  
  summarym1b <- summary(m1b)
  
  save(m1b, summarym1b, file="ruminating_comprehensive_model1b.rda", compress="xz")
} else{
  load("ruminating_comprehensive_model1.rda")
}

if(FALSE){
  m1c <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
              aloneCompany_lag1 +
              s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
              s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
              s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
              s(distracted_lag1) + s(stickiness_lag1) + s(listless_lag1) + s(negMax_lag1) + s(posMax_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data = sc_data)
  
  summarym1c <- summary(m1c)
  
  save(m1c, summarym1c, file="ruminating_comprehensive_model1.rda", compress="xz")
} else{
  load("ruminating_comprehensive_model1.rda")
}



#fastbw(m1) #give it a try...

plot_smooth(m1, view="beepNum", 
            plot_all="group")

plot_diff(m1, view="beepNum", 
          comp=list(group=c("remitted", "controls")))




if(FALSE){
  m1c_grIntPhase <- bam(ruminating ~ grIntPhase * block * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
                          aloneCompany_lag1 +
                          s(beepNum, by = grIntPhase) +
                          s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
                          s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
                          s(distracted_lag1) + s(stickiness_lag1) + s(listless_lag1) + s(negMax_lag1) + s(posMax_lag1) +
                          s(beepNum, by = subject, bs="fs", m=1) + s(grIntPhase, bs="re"),
                        data = sc_data)
  
  summarym1c_grIntPhase <- summary(m1c_grIntPhase)
  
  save(m1c_grIntPhase, summarym1c_grIntPhase, file="ruminating_comprehensive_m1c_grIntPhase.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m1c_grIntPhase.rda")
}


#fastbw(m1) #give it a try...

plot_smooth(m1c_grIntPhase, view="beepNum", 
            plot_all="grIntPhase")

for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){
    g1 <- paste(g, int, "peri", sep = ".")
    g2 <- paste(g, int, "pre", sep = ".")
    print(plot_diff(m1c_grIntPhase, view="beepNum", 
              comp=list(grIntPhase=c(g1, g2))))
  }
}




if(FALSE){ #fitted with ML for FE comparison
  m1c <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(distracted_lag1) + s(stickiness_lag1) + s(listless_lag1) + s(negMax_lag1) + s(posMax_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  summarym1c <- summary(m1c)
  
  save(m1c, summarym1c, file ="ruminating_comprehensive_m1c.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m1c.rda")
}


if(FALSE){ #without "listless"
  m2b <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
              aloneCompany_lag1 +
              s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
              s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
              s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) + s(distracted_lag1) +
              s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2b, file="ruminating_comprehensive_m2b-listless.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2b-listless.rda")
}

compareML(m1c, m2b) # significant



if(FALSE){ #without "distracted"
  m2 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
              aloneCompany_lag1 +
              s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
              s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) + s(beepNum, by = thoughtsObject_lag1) +
              s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) + s(listless_lag1) +
              s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2, file="ruminating_comprehensive_m2-distracted.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2-distracted.rda")
}

compareML(m2, m2b) #not significant


if(FALSE){ #without "s(beepNum, by = thoughtsObject)"
  m2c <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(beepNum, by = thoughtsTime_lag1) + s(beepNum, by = thoughtsValence_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2c, file="ruminating_comprehensive_m2c-beepNum_by_thoughtsObject.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2c-beepNum_by_thoughtsObject.rda")
}

compareML(m2c, m2) #not significant



if(FALSE){ #without "s(beepNum, by = thoughtsValence)"
  m2d <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(beepNum, by = thoughtsTime_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2d, file="ruminating_comprehensive_m2d-beepNum_by_thoughtsValence.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2d-beepNum_by_thoughtsValence.rda")
}

compareML(m2c, m2d) #not significant



if(FALSE){ #without "s(beepNum, by = thoughtsTime)"
  m2e <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 *
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2e, file="ruminating_comprehensive_m2e-beepNum_by_thoughtsTime.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2e-beepNum_by_thoughtsTime.rda")
}

compareML(m2e, m2d) #not significant


if(FALSE){ #without "s(beepNum, by = thoughtsTime)"
  m2f <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 * thoughtsObject_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2f, file="ruminating_comprehensive_m2f-aloneCompanyInteractions.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2f-aloneCompanyInteractions.rda")
}

compareML(m2e, m2f) #not significant


if(FALSE){ #without "thoughtsObject interactions"
  m2g <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsValence_lag1 + thoughtsObject_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2g, file="ruminating_comprehensive_m2g-thoughtsObjectInteractions.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2f-aloneCompanyInteractions.rda")
}

compareML(m2g, m2f) #significant


if(FALSE){ #without thoughtsValence interactions
  m2h <- bam(ruminating ~ group * intervention * blockPhase * thoughtsTime_lag1 * thoughtsObject_lag1 + thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2h, file="ruminating_comprehensive_m2h-thoughtsValenceInteractions.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m2h-thoughtsValenceInteractions.rda")
}

compareML(m2h, m2f) # significant



if(FALSE){ #without thoughtsValence interactions
  m2i <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsObject_lag1 + thoughtsTime_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2i, file="m2i.rda", compress="xz")
} else{
  load("m2i.rda")
}

compareML(m2i, m2f) # significant


if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2j <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsObject_lag1 + thoughtsTime_lag1 +
               thoughtsTime_lag1:group + thoughtsTime_lag1:intervention + thoughtsTime_lag1:blockPhase + thoughtsTime_lag1:thoughtsValence_lag1 +
              aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2j, file="m2j.rda", compress="xz")
} else{
  load("m2j.rda")
}

compareML(m2j, m2f) # significant

if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2k <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsObject_lag1 + thoughtsTime_lag1 +
               thoughtsTime_lag1:group + thoughtsTime_lag1:intervention + thoughtsTime_lag1:blockPhase + thoughtsTime_lag1:thoughtsObject_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2k, file="m2k.rda", compress="xz")
} else{
  load("m2k.rda")
}

compareML(m2k, m2f) # significant


if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2l <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsObject_lag1 + thoughtsTime_lag1 +
               thoughtsTime_lag1:group + thoughtsTime_lag1:intervention + thoughtsTime_lag1:thoughtsValence_lag1 + thoughtsTime_lag1:thoughtsObject_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2l, file="m2l.rda", compress="xz")
} else{
  load("m2l.rda")
}

compareML(m2l, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2m <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsObject_lag1 + thoughtsTime_lag1 +
               thoughtsTime_lag1:group + thoughtsTime_lag1:blockPhase + thoughtsTime_lag1:thoughtsValence_lag1 + thoughtsTime_lag1:thoughtsObject_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2m, file="m2m.rda", compress="xz")
} else{
  load("m2m.rda")
}

compareML(m2l, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2n <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsObject_lag1 + thoughtsTime_lag1 +
               thoughtsTime_lag1:intervention + thoughtsTime_lag1:blockPhase + thoughtsTime_lag1:thoughtsValence_lag1 + thoughtsTime_lag1:thoughtsObject_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2n, file="m2n.rda", compress="xz")
} else{
  load("m2m.rda")
}

compareML(m2n, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2o <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsTime_lag1 + thoughtsObject_lag1 +
              thoughtsObject_lag1:group + thoughtsObject_lag1:intervention + thoughtsObject_lag1:blockPhase +
               thoughtsObject_lag1:thoughtsTime_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2o, file="m2o.rda", compress="xz")
} else{
  load("m2o.rda")
}

compareML(m2o, m2f) # significant


if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2p <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsTime_lag1 + thoughtsObject_lag1 +
               thoughtsObject_lag1:group + thoughtsObject_lag1:intervention + thoughtsObject_lag1:blockPhase +
               thoughtsObject_lag1:thoughtsValence_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2p, file="m2p.rda", compress="xz")
} else{
  load("m2p.rda")
}

compareML(m2p, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2q <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsTime_lag1 + thoughtsObject_lag1 +
               thoughtsObject_lag1:group + thoughtsObject_lag1:intervention + thoughtsObject_lag1:thoughtsTime_lag1 +
               thoughtsObject_lag1:thoughtsValence_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2q, file="m2q.rda", compress="xz")
} else{
  load("m2q.rda")
}

compareML(m2p, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2r <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsTime_lag1 + thoughtsObject_lag1 +
               thoughtsObject_lag1:group + thoughtsObject_lag1:blockPhase + thoughtsObject_lag1:thoughtsTime_lag1 +
               thoughtsObject_lag1:thoughtsValence_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2r, file="m2r.rda", compress="xz")
} else{
  load("m2r.rda")
}

compareML(m2r, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2s <- bam(ruminating ~ group * intervention * blockPhase * thoughtsValence_lag1 * thoughtsTime_lag1 + thoughtsObject_lag1 +
               thoughtsObject_lag1:intervention + thoughtsObject_lag1:blockPhase + thoughtsObject_lag1:thoughtsTime_lag1 +
               thoughtsObject_lag1:thoughtsValence_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2s, file="m2s.rda", compress="xz")
} else{
  load("m2s.rda")
}

compareML(m2s, m2f) # significant



if(FALSE){ #without thoughtsTime:ThoughtsObject
  m2t <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 + thoughtsValence_lag1 +
               thoughtsValence_lag1:intervention + thoughtsValence_lag1:blockPhase + thoughtsValence_lag1:thoughtsTime_lag1 +
               thoughtsValence_lag1:thoughtsObject_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2t, file="m2t.rda", compress="xz")
} else{
  load("m2t.rda")
}

compareML(m2t, m2f) # significant


if(FALSE){ #
  m2u <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 + thoughtsValence_lag1 +
               thoughtsValence_lag1:group + thoughtsValence_lag1:blockPhase + thoughtsValence_lag1:thoughtsTime_lag1 +
               thoughtsValence_lag1:thoughtsObject_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2u, file="m2u.rda", compress="xz")
} else{
  load("m2u.rda")
}

compareML(m2u, m2f) # significant


if(FALSE){ #
  m2v <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 + thoughtsValence_lag1 +
               thoughtsValence_lag1:group + thoughtsValence_lag1:intervention + thoughtsValence_lag1:thoughtsTime_lag1 +
               thoughtsValence_lag1:thoughtsObject_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2v, file="m2v.rda", compress="xz")
} else{
  load("m2v.rda")
}

compareML(m2v, m2f) # significant


if(FALSE){ #
  m2x <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 + thoughtsValence_lag1 +
               thoughtsValence_lag1:group + thoughtsValence_lag1:intervention + thoughtsValence_lag1:blockPhase +
               thoughtsValence_lag1:thoughtsObject_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant



if(FALSE){ #
  m2x <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 + thoughtsValence_lag1 +
               thoughtsValence_lag1:group + thoughtsValence_lag1:intervention + thoughtsValence_lag1:blockPhase +
               thoughtsValence_lag1:thoughtsTime_lag1 +  aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant



if(FALSE){ #
  m2x <- bam(ruminating ~ group * intervention * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 + blockPhase +
               aloneCompany_lag1 + blockPhase:group + blockPhase:intervention +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant


if(FALSE){ #
  m2x <- bam(ruminating ~ group * intervention * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 + blockPhase +
               aloneCompany_lag1 + blockPhase:group + blockPhase:intervention + blockPhase:thoughtsTime_lag1 + blockPhase:thoughtsValence_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant


if(FALSE){ #
  m2x <- bam(ruminating ~ group * intervention * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 + blockPhase +
               aloneCompany_lag1 + blockPhase:group + blockPhase:intervention + blockPhase:thoughtsTime_lag1 + blockPhase:thoughtsValence_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant


if(FALSE){ #
  m2x <- bam(ruminating ~ group * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 + intervention +
               aloneCompany_lag1 + intervention:group + intervention:blockPhase +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant




if(FALSE){ #
  m2x <- bam(ruminating ~ group * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 + intervention +
               aloneCompany_lag1 + intervention:group + intervention:blockPhase + intervention:thoughtsValence_lag1 + intervention:thoughtsObject_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant



if(FALSE){ #
  m2x <- bam(ruminating ~ intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               group + group:intervention + group:blockPhase + group:thoughtsObject_lag1 + group:thoughtsTime_lag1 +
               aloneCompany_lag1 + intervention:group + intervention:blockPhase +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant



if(FALSE){ #
  m2x <- bam(ruminating ~ intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               group + group:intervention + group:blockPhase + group:thoughtsObject_lag1 + group:thoughtsValence_lag1 +
               aloneCompany_lag1 + intervention:group + intervention:blockPhase +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) + ti(sumNA_lag1, sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m2x, file="m2x.rda", compress="xz")
} else{
  load("m2x.rda")
}

compareML(m2x, m2f) # significant




if(FALSE){ #all categoricalXcategorical interactions are significant! Let's move on to cont x cont
  m3 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA_lag1) + s(sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3, file="m3.rda", compress="xz")
} else{
  load("m3.rda")
}

compareML(m3, m2f) #not significant


if(FALSE){ #all cont x cont --> on to cont X cat
  m3a <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
              aloneCompany_lag1 +
              s(beepNum, by = group) + s(beepNum, by = intervention) +
              s(sumNA_lag1) + s(sumPA_lag1) +
              s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3a, file="m3a.rda", compress="xz")
} else{
  load("m3a.rda")
}

compareML(m3, m3a) #not significant


if(FALSE){ #all cont x cont --> on to cont X cat
  m3b <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(beepNum, by = group) +
               s(sumNA_lag1) + s(sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3b, file="m3b.rda", compress="xz")
} else{
  load("m3b.rda")
}

compareML(m3a, m3b) #not significant



if(FALSE){ #all cont x cont --> on to cont X cat
  m3c <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(sumNA_lag1) + s(sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3c, file="m3c.rda", compress="xz")
} else{
  load("m3c.rda")
}

compareML(m3c, m3b) #not significant


if(FALSE){ #-stickiness
  m3d <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(sumNA_lag1) + s(sumPA_lag1) +
               s(negMax_lag1) + s(posMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3d, file="m3d.rda", compress="xz")
} else{
  load("m3d.rda")
}

compareML(m3c, m3d) #significant


if(FALSE){ #-posMax
  m3e <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(sumNA_lag1) + s(sumPA_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3c, file="m3c.rda", compress="xz")
} else{
  load("m3c.rda")
}

compareML(m3c, m3e) #significant


if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m3f <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + s(sleepQuality) + s(sleepDuration) + s(sleepLatency) +
               s(restednessWakeup) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3f, file="m3f.rda", compress="xz")
} else{
  load("m3f.rda")
}

compareML(m3c, m3f) #significant


if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m3g <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + s(sleepQuality) + s(sleepDuration) + s(sleepLatency) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3g, file="m3g.rda", compress="xz")
} else{
  load("m3g.rda")
}

compareML(m3g, m3f) #not significant


if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m3h <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + s(sleepQuality) + s(sleepDuration) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3h, file="m3h.rda", compress="xz")
} else{
  load("m3h.rda")
}

compareML(m3h, m3g) #significant



if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m3i <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + s(sleepQuality) + s(sleepLatency) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3i, file="m3i.rda", compress="xz")
} else{
  load("m3i.rda")
}

compareML(m3g, m3i) #not significant --> m31 is the less complex one --> m3i is the winner


if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m3i2 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + s(sleepLatency) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3i2, file="m3i2.rda", compress="xz")
} else{
  load("m3i2.rda")
}

compareML(m3i2, m3i) #not significant --> m31 is the less complex one --> m3i is the winner


if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m3i3 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
                aloneCompany_lag1 + s(posMax_lag1) +
                s(sumNA_lag1) + s(sumPA_lag1) + s(sleepQuality) +
                s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
                s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
              data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3i3, file="m3i3.rda", compress="xz")
} else{
  load("m3i3.rda")
}

compareML(m3i2, m3i3) #not significant --> m31 is the less complex one --> m3i is the winner
compareML(m3i3, m3i) #significant -->m3i is the more complex one --> m3i is the winner



if(FALSE){ #
  m3j <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1) + s(sumPA_lag1) + s(sleepQuality) +
               s(stickiness_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3j, file="m3j.rda", compress="xz")
} else{
  load("m3j.rda")
}

compareML(m3j, m3i3) #significant



if(FALSE){ #- sumPA
  m3k <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 + s(posMax_lag1) +
               s(sumNA_lag1)  + s(sleepQuality) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3k, file="m3k.rda", compress="xz")
} else{
  load("m3k.rda")
}

compareML(m3k, m3i3) #not significant



if(FALSE){ #- sumNA
  m3l <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
              s(sleepQuality) + s(posMax_lag1) +
               s(stickiness_lag1) + s(negMax_lag1) + s(listless_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3l, file="m3l.rda", compress="xz")
} else{
  load("m3l.rda")
}

compareML(m3l, m3k) #significant


if(FALSE){ #- listless
  m3m <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 * thoughtsTime_lag1 * thoughtsValence_lag1 +
               aloneCompany_lag1 +
               s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
               s(stickiness_lag1) + s(negMax_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3m, file="m3m.rda", compress="xz")
} else{
  load("m3m.rda")
}

compareML(m3m, m3i) #significant --> m3i is the winner


#m3i is the overall winner
#refitting m3i with fREML
if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m4 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 *
              thoughtsTime_lag1 * thoughtsValence_lag1 + aloneCompany_lag1 +
               s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
               s(stickiness_lag1) + s(negMax_lag1) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
             data = sc_data)
  
  summarym4 <- summary(m4)
  
  save(m4, summarym4, file="m4_winner.rda", compress="xz")
} else{
  load("m4_winner.rda")
}


summarym4


plot_smooth(m4, view="beepNum", 
            plot_all="group")

plot_diff(m4, view="beepNum",
          comp = list(group = c("controls", "remitted")))



#refitting with grIntPhase
if(FALSE){ #+ sleep metrics (not proper... just a quick check for now)
  m4b <- bam(ruminating ~ grIntPhase * block * thoughtsObject_lag1 *
              thoughtsTime_lag1 * thoughtsValence_lag1 + aloneCompany_lag1 +
              s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
              s(stickiness_lag1) + s(negMax_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(grIntPhase, bs="re"),
            data = sc_data)
  
  summarym4b <- summary(m4b)
  
  save(m4b, summarym4b, file="m4b_grIntPhase.rda", compress="xz")
} else{
  load("m4b_grIntPhase.rda")
}


plot_smooth(m4b, view = "beepNum",
            plot_all = "grIntPhase")


for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){
    g1 <- paste(g, int, "peri", sep = ".")
    g2 <- paste(g, int, "pre", sep = ".")
    print(plot_diff(m4b, view="beepNum", 
                    comp=list(grIntPhase=c(g1, g2))))
  }
}


if(FALSE){
  m4sc <- gam(ruminating ~ s(blockBeepNum) + group * intervention * phase * thoughtsObject_lag1 *
                thoughtsTime_lag1 * thoughtsValence_lag1 + aloneCompany_lag1 +
                s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
                s(stickiness_lag1) + s(negMax_lag1) +
                s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
              data = sc_data, family = "scat")
  
  summarym4sc <- summary(m4sc)
  save(m4sc, summarym4sc, file="m4sc.rda", compress="xz")
} else {
  load("m4sc.rda")
}

qqnorm(resid(m4sc))
qqline(resid(m4sc))

resid_m4sc <- resid(m4sc)
match(c(min(resid_m4sc),max(resid_m4sc)),resid_m4sc) #1980, 621


x <- resid(m4sc)
h <- hist(x, col = "red", breaks = 40)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

# PLOT 2:
plot(fitted(m4sc), resid(m4sc))
abline(h=0)

acf_resid(m4sc)
data_ar <- data
data_ar$Time <- data$beepNum
data_ar <- start_event(data_ar, event=c("subject"))
#dat_ar <- droplevels(dat_ar[order(dat$Subject, dat_ar$Month, dat_ar$Time),])
ar <- acf_resid(m4sc, plot = FALSE)

ar[2] #autocorrelation of ~0.126



plot_smooth(m4sc, view="beepNum", 
            plot_all="group")

plot_diff(m4sc, view="beepNum",
          comp = list(group = c("controls", "remitted")))




if(FALSE){
  m4_scat <- gam(ruminating ~ grIntPhase * block * thoughtsObject_lag1 *
                   thoughtsTime_lag1 * thoughtsValence_lag1 + aloneCompany_lag1 + s(beepNum) +
                   s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
                   s(stickiness_lag1) + s(negMax_lag1) +
                   s(beepNum, by = subject, bs="fs", m=1) + s(grIntPhase, bs="re"),
                 data = sc_data, family = "scat")
  
  
  summarym4scat <- summary(m4_scat)
  save(m4_scat, summarym4scat, file="m4scat.rda", compress="xz")
} else {
  load("m4scat.rda")
}
m4_scat <- gam(ruminating ~ grIntPhase * block * thoughtsObject_lag1 *
             thoughtsTime_lag1 * thoughtsValence_lag1 + aloneCompany_lag1 + s(beepNum) +
             s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
             s(stickiness_lag1) + s(negMax_lag1) +
             s(beepNum, by = subject, bs="fs", m=1) + s(grIntPhase, bs="re"),
           data = sc_data, family = "scat")


summarym4scat <- summary(m4_scat)


plot_smooth(m4_scat, view = "beepNum",
            plot_all = "grIntPhase")

for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){
    g1 <- paste(g, int, "peri", sep = ".")
    g2 <- paste(g, int, "pre", sep = ".")
    print(plot_diff(m4_scat, view="beepNum", 
                    comp=list(grIntPhase=c(g1, g2))))
  }
}




################################### with phaseBeepNum and subjP ##########################

m4sc <- gam(ruminating ~ group * intervention * blockPhase * thoughtsObject_lag1 *
              thoughtsTime_lag1 * thoughtsValence_lag1 + 
              aloneCompany_lag1 +
              s(sumNA_lag1) + s(posMax_lag1) + s(sleepQuality) +
              s(stickiness_lag1) + s(negMax_lag1) +
              s(phaseBeepNum, by = subjP, bs="fs", m=1) + s(group, bs="re"),
            data = sc_data, family = "scat")

summarym4sc <- summary(m4sc)


plot_smooth(m4, view="beepNum", 
            plot_all="group")

plot_diff(m4, view="beepNum",
          comp = list(group = c("controls", "remitted")))



























############################# same model but without lagged predictors #####################################
if(FALSE){ 
  m4 <- bam(ruminating ~ group * intervention * blockPhase * thoughtsObject *
              thoughtsTime * thoughtsValence + aloneCompany +
              s(sumNA) + s(sumPA) + s(sleepQuality) +
              s(stickiness) + s(negMax_lag1) + s(listless_lag1) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re"),
            data = sc_data)
  
  summarym4 <- summary(m4)
  
  save(m4, summarym4, file="m4_winner.rda", compress="xz")
} else{
  load("m4_winner.rda")
}


summarym4


plot_smooth(m4, view="beepNum", 
            plot_all="group")

plot_diff(m4, view="beepNum",
          comp = list(group = c("controls", "remitted")))













if(FALSE){ #without "distracted"
  m2d <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
               aloneCompany:group + aloneCompany:intervention + aloneCompany:blockPhase + aloneCompany:thoughtsTime +
               aloneCompany:thoughtsValence +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + ti(sleepQuality, sumNA) +
               s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
               s(blockPhase, bs="re") + s(intervention, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3d, file="ruminating_comprehensive_m3d-aloneCompany_interactions2.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m3.rda")
}

compareML(m3c, m3d) #not significant

if(FALSE){ #without "distracted"
  m3e <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
               aloneCompany:group + aloneCompany:intervention + aloneCompany:blockPhase + aloneCompany:thoughtsTime +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + ti(sleepQuality, sumNA) +
               s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
               s(blockPhase, bs="re") + s(intervention, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3e, file="ruminating_comprehensive_m3e-aloneCompany_interactions3.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m3.rda")
}

compareML(m3d, m3e)

if(FALSE){ #without "distracted"
  m3e2 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
               aloneCompany:intervention + aloneCompany:blockPhase + aloneCompany:thoughtsTime +
               s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
               s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + ti(sleepQuality, sumNA) +
               s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
               s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
               s(blockPhase, bs="re") + s(intervention, bs="re"),
             data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3e2, file="ruminating_comprehensive_m3e2-aloneCompany_interactions4.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m3e2.rda")
}

compareML(m3e, m3e2) #not significant


if(FALSE){ #without "distracted"
  m3e3 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
                aloneCompany:blockPhase + aloneCompany:thoughtsTime +
                s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + ti(sleepQuality, sumNA) +
                s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
                s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
                s(blockPhase, bs="re") + s(intervention, bs="re"),
              data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3e3, file="ruminating_comprehensive_m3e3-aloneCompany_interactions5.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m3e3-aloneCompany_interactions5.rda")
}

compareML(m3e3, m3e2) #significatn


if(FALSE){ #without "distracted"
  m3e4 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
                aloneCompany:intervention + aloneCompany:thoughtsTime +
                s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + ti(sleepQuality, sumNA) +
                s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
                s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
                s(blockPhase, bs="re") + s(intervention, bs="re"),
              data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3e4, file="ruminating_comprehensive_m3e4-aloneCompany_interactions6.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m3e4-aloneCompany_interactions6.rda")
}

compareML(m3e3, m3e4) #not significant


if(FALSE){ #without "distracted"
  m3e5 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
                aloneCompany:intervention +
                s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) + ti(sleepQuality, sumNA) +
                s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
                s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
                s(blockPhase, bs="re") + s(intervention, bs="re"),
              data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m3e5, file="ruminating_comprehensive_m3e5-aloneCompany_interactions6.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m3e5-aloneCompany_interactions6.rda")
}

compareML(m3e5, m3e4) #not significant


if(FALSE){ #without "distracted"
  m4 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
                aloneCompany:intervention +
                s(beepNum, by = group) + s(beepNum, by = intervention) + s(beepNum, by = blockPhase) +
                s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
                s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
                s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
                s(blockPhase, bs="re") + s(intervention, bs="re"),
              data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m4, file="ruminating_comprehensive_m4-sleepQualityXsumNA.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m4-sleepQualityXsumNA.rda")
}

compareML(m3e5, m4) #not significant



if(FALSE){ #without "distracted"
  m5 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
              aloneCompany:intervention +
              s(beepNum, by = group) + s(beepNum, by = intervention) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m5, file="ruminating_comprehensive_m5-beepNumBlockPhase.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m5-beepNumBlockPhase.rda")
}

compareML(m5, m4) #not significant


if(FALSE){ #without "distracted"
  m6 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence * thoughtsObject + aloneCompany +
              aloneCompany:intervention +
              s(beepNum, by = group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m6, file="ruminating_comprehensive_m6-beepNumIntervention.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m6-beepNumIntervention.rda")
}

compareML(m5, m6) #not significant
#View(subset(data[which(data$subject == "s81"),], select = c("subject", "block", "phase", "beepNum", "phaseBeepNum", "blockBeepNum")))


if(FALSE){ #without "distracted"
  m7 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsTime * thoughtsValence + thoughtsObject + aloneCompany +
              aloneCompany:intervention +
              s(beepNum, by = group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m7, file="ruminating_comprehensive_m7-thoughtsObjectInteraction.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m7-thoughtsObjectInteraction.rda")
}

compareML(m7, m6) #not significant


if(FALSE){ #without "distracted"
  m8 <- bam(ruminating_lag1 ~ group * intervention * blockPhase * thoughtsValence + thoughtsTime + thoughtsObject + aloneCompany +
              aloneCompany:intervention +
              s(beepNum, by = group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m8, file="ruminating_comprehensive_m8-thoughtsTimeInteraction.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m8-thoughtsTimeInteraction.rda")
}

compareML(m7, m8) #not significant

if(FALSE){
  m9 <- bam(ruminating_lag1 ~ group * intervention * blockPhase + thoughtsValence + thoughtsTime + thoughtsObject + aloneCompany +
              aloneCompany:intervention +
              s(beepNum, by = group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m9, file="ruminating_comprehensive_m9-thoughtsValenceInteraction.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m9-thoughtsValenceInteraction.rda")
}

compareML(m9, m8) #significant







################################ NOT RUN YET! ####################################

if(FALSE){
  m10 <- bam(ruminating ~ group * intervention * blockPhase + thoughtsValence * thoughtsTime * thoughtsObject + aloneCompany +
              aloneCompany:intervention +
              s(beepNum, by = group) +
              s(sumNA) + s(sumPA) + ti(sumNA, sumPA) + s(sleepQuality) +
              s(distracted) + s(listless) + s(stickiness) + s(negMax) + s(posMax) +
              s(beepNum, by = subject, bs="fs", m=1) + s(group, bs="re") +
              s(blockPhase, bs="re") + s(intervention, bs="re"),
            data = sc_data, method = "ML")
  
  #summarym3 <- summary(m3)
  
  save(m10, file="ruminating_comprehensive_m10_onlyThoughtsInteractionsaddedAgain.rda", compress="xz")
} else{
  load("ruminating_comprehensive_m10_onlyThoughtsInteractionsaddedAgain.rda")
}

compareML(m9, m10) #significant


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




                                                             

