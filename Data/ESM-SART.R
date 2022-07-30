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
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
              "meanRT", "meanRT_Go", "meanRT_NoGo", "propCor", "propCor_Go", "propCor_NoGo")

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
data <- data[which(data$subjB %in% pp),]
#sc_data <- sc_data[which(sc_data$blockBeepNum <= 140),]
#sc_data <- sc_data[which(is.na(sc_data$mindcog_db_non_response)),]

sart <- data[which(!is.na(data$meanRT)),]
sc_sart <- sc_data[which(!is.na(sc_data$meanRT)),]

###########################################################################################################
######################################### Inspecting Relation #############################################
###########################################################################################################
#The correlations are very weak

cor(sart$meanRT, sart$ruminating)
#[1] 0.02175574
cor(sart$meanRT, sart$distracted)
#[1] -0.01042558
cor(sart$meanRT, sart$stickiness)
#[1] 0.04576742
cor(sart$meanRT, sart$wakeful)
#[1] -0.06633524
cor(sart$meanRT, sart$sumNA)
#[1] -0.02192623
cor(sart$meanRT, sart$sumPA)
#[1] -0.07724179
plot(sart$meanRT, sart$ruminating)
plot(sart$meanRT, sart$stickiness)
plot(sart$meanRT, sart$distracted)
plot(sart$meanRT, sart$sumPA)


#let's look at change scores instead
cor(sart$meanRT, sart$ruminating_diff)
#[1] 0.02175574
cor(sart$meanRT, sart$distracted_diff)
#[1] 0.01155531
cor(sart$meanRT, sart$stickiness_diff)
#[1] -0.01699151
cor(sart$meanRT, sart$wakeful_diff)
#[1] 0.0581514
cor(sart$meanRT, sart$sumNA_diff)
#[1] 0.02787691
cor(sart$meanRT, sart$sumPA_diff)
#[1] -0.07349336
plot(sart$meanRT, sart$ruminating_diff)
plot(sart$meanRT, sart$stickiness_diff)
plot(sart$meanRT, sart$distracted_diff)
plot(sart$meanRT, sart$sumPA_diff)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++ Creating models with gam-variables +++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#gam varialbes are NA for entire pre-phase (while diff-variables are actual - baselineMean for all data)

avg2 <- ddply(sart, c("group", "intervention", "phaseBeepNum", "meanRT"), summarise,
              N    = length(meanRT),
              sd   = sd(meanRT),
              se   = sd / sqrt(N))
plot2 <- ggplot(avg2, aes(y=meanRT, x=phaseBeepNum, color=intervention)) +
  geom_point(position = position_jitter(w=0.1,h=0))+ facet_grid(.~group) +
  ylab("meanRT Difference peri-pre")+ xlab("Phase Assessment Number") + labs(color = "Intervention") +
  geom_hline(yintercept=0) + geom_smooth()  #+ scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7"))

plot2


#Maximum model with REML
rt.max.reml <- gam(meanRT ~ s(phaseBeepNum) + group * intervention +
                     s(ruminating_gam) + s(stickiness_gam) +
                     s(distracted_gam) +
                     s(phaseBeepNum, by = subject, bs="fs", m=1),
                   data = sc_sart)

summary(rt.max.reml)

#Maximum model with ML
rt.max <- gam(meanRT ~ s(phaseBeepNum) + group * intervention +
                    s(ruminating_gam) + s(stickiness_gam) +
                    s(distracted_gam) +
                    s(phaseBeepNum, by = subject, bs="fs", m=1),
                  data = sc_sart, method = "ML")

#removing interaction between group and intervention
rt1 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention +
                s(ruminating_gam) + s(stickiness_gam) +
                s(distracted_gam) +
                s(phaseBeepNum, by = subject, bs="fs", m=1),
              data = sc_sart, method = "ML")

compareML(rt.max, rt1) #not signficant


#removing distracted
rt2 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention +
             s(ruminating_gam) + s(stickiness_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt1) #not signficant


#removing stickiness
rt3 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention +
             s(ruminating_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt3) #signficant

#removing ruminating
rt4 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention +
             s(stickiness_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt4) #signficant


#removing intervention
rt5 <- gam(meanRT ~ s(phaseBeepNum) + group +
             s(ruminating_gam) + s(stickiness_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt5) #signficant


#removing group
rt6 <- gam(meanRT ~ s(phaseBeepNum) + intervention +
             s(ruminating_gam) + s(stickiness_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt6) #not signficant

#rt6 is the winnter but we might opt to keep group as a predictor anyways(?)
rt.m1 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention +
             s(ruminating_gam) + s(stickiness_gam) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart)

summary(rt.m1)


plot_smooth(rt.m1, view="phaseBeepNum", rug=F, plot_all="intervention")
plot_smooth(rt.m1, view="phaseBeepNum", rug=F, plot_all="group")

# check autocorrelation
model1.acf <- acf_resid(rt.m1)

#check model fit
gam.check(rt.m1)

#plot the model based predicted differences between interventions (summed effects)
plot_parametric(rt.m1, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(rt.m1, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")))

# plot differences
plot_diff(rt.m1, view="phaseBeepNum", comp=list("intervention"=c("mindfulness", "fantasizing")), main="mindfulness vs fantasizing")
plot_diff(rt.m1, view="phaseBeepNum", comp=list( group=c("remitted", "remitted"), intervention=c("mindfulness", "fantasizing")))
#,main="mindfulness vs fantasizing")
plot_diff(rt.m1, view="phaseBeepNum", comp=list( group=c("controls", "controls"), intervention=c("mindfulness", "fantasizing")))


# intercept model to compare with winner model
rt.int <- bam(meanRT ~ 1 + s(phaseBeepNum, by=subject, bs="fs", m=1), data=sc_sart, method="ML")
summary(rt.int)

compareML(rt.int, rt2) #rt2 clearly outperforms the intercept model





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################ Raw scores #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Maximum model with REML
rt.max.reml <- gam(meanRT ~ s(phaseBeepNum) + group * intervention * phase +
                     s(ruminating) + s(stickiness) +
                     s(distracted) + s(sumNA) + s(sumPA) +
                     s(phaseBeepNum, by = subject, bs="fs", m=1),
                   data = sc_sart)

summary(rt.max.reml)

#Maximum model with ML
rt.max <- gam(meanRT ~ s(phaseBeepNum) + group * intervention * phase +
                s(ruminating) + s(stickiness) +
                s(distracted) + s(sumNA) + s(sumPA) +
                s(phaseBeepNum, by = subject, bs="fs", m=1),
              data = sc_sart, method = "ML")

#removing interaction between group and phase
rt1 <- gam(meanRT ~ s(phaseBeepNum) + group * intervention + phase + phase:intervention +
                s(ruminating) + s(stickiness) +
                s(distracted) + s(sumNA) + s(sumPA) +
                s(phaseBeepNum, by = subject, bs="fs", m=1),
              data = sc_sart, method = "ML")

compareML(rt.max, rt1) #not significant

#removing interaction between intervention and phase
rt2 <- gam(meanRT ~ s(phaseBeepNum) + group * intervention + phase +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumNA) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt1) #not significant


#removing interaction between intervention and group
rt3 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention + phase +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumNA) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt2, rt3) #not significant


#removing phase
rt4 <- gam(meanRT ~ s(phaseBeepNum) + group + intervention +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumNA) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt4, rt3) #significant


#removing intervention
rt5 <- gam(meanRT ~ s(phaseBeepNum) + group + phase +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumNA) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt5, rt3) #not significant

#removing group
rt6 <- gam(meanRT ~ s(phaseBeepNum) + phase +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumNA) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt5, rt6) #significant


#removing sumPA
rt7 <- gam(meanRT ~ s(phaseBeepNum) + group + phase +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumNA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt5, rt7) #significant


#removing sumNA
rt8 <- gam(meanRT ~ s(phaseBeepNum) + group + phase +
             s(ruminating) + s(stickiness) +
             s(distracted) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt5, rt8) #not significant


#removing stickiness
rt9 <- gam(meanRT ~ s(phaseBeepNum) + group + phase +
             s(ruminating) +
             s(distracted) + s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt9, rt8) #not significant


#removing distracted
rt10 <- gam(meanRT ~ s(phaseBeepNum) + group + phase +
             s(ruminating) +
             s(sumPA) +
             s(phaseBeepNum, by = subject, bs="fs", m=1),
           data = sc_sart, method = "ML")

compareML(rt9, rt10) #not significant

#removing ruminating
rt11 <- gam(meanRT ~ s(phaseBeepNum) + group + phase +
              s(sumPA) +
              s(phaseBeepNum, by = subject, bs="fs", m=1),
            data = sc_sart, method = "ML")

compareML(rt11, rt10) #not significant





