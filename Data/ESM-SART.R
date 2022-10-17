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
library(BayesFactor)
library(lme4)
library(lmerTest)
library(languageR)
library(effectsize)

packageVersion("lme4") #1.1.28
packageVersion("BayesFactor") #0.9.12.4.3
packageVersion("itsadug") #2.4
packageVersion("mgcv") # 1.8.31

#read in data
data <- read.csv('merged_data.csv') 
numbers <- read.csv("sart_w_thoughtProbes.csv")


###########################################################################################################
######################################### Some more data prep #############################################
###########################################################################################################

data$group <- factor(data$group, levels = c("remitted", "controls"))
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

data$commissionError <- 1 - data$propCor_NoGo



###########################################################################################################
############################################ Scaling Data #################################################
###########################################################################################################
#creating variables minus baseline means per subject
met.vars <- c('ruminating', 'stickiness', 'meanNA',  'down', 'irritated', 'restless', 'anxious',
              'meanPA', 'wakeful', 'satisfied', 'energetic',
              'stressed', 'listless',  'distracted',
              'thoughtsPleasant', 'restOfDayPos',
              'posMax', 'posIntensity', 'negMax', 'negIntensity',
              "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
              "meanRT", "meanRT_Go", "meanRT_NoGo", "propCor", "propCor_Go", "propCor_NoGo",
              "commissionError")

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


#number of participants so far
length(unique(data$subjB)) #66 subjB (same subject, different block --> viewed as separate)
responses_block <- ddply(data, .(subjB), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))

meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.6%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subjB)) #36
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subjB)) #45
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subjB)) #53

responses_subject <- ddply(data, .(subject), plyr::summarise,
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
data <- data[which(data$subjB %in% pp),]
#sc_data <- sc_data[which(sc_data$blockBeepNum <= 140),]
#sc_data <- sc_data[which(is.na(sc_data$mindcog_db_non_response)),]

sart <- data[which(!is.na(data$meanRT)),]

sart$gameNumSubject <- NA
subjectIDs <- unique(sart$subject)
for(id in subjectIDs){
  lenDat <- length(sart[which(sart$subject==id),]$subject)
  sart[which(sart$subject==id),]$gameNumSubject <- 1:lenDat
}

sart$gameNumSubjB <- NA
subjBIDs <- unique(sart$subjB)
for(id in subjBIDs){
  lenDat <- length(sart[which(sart$subjB==id),]$subjB)
  sart[which(sart$subjB==id),]$gameNumSubjB <- 1:lenDat
}

#creating a scaled version of data
sc_sart <- copy(sart)
sc_sart[scale.vars] <- scale(sc_sart[scale.vars])

sc_sart <- sc_sart[which(!is.na(sc_sart$meanRT)),]


baseSart <- sart[which((sart$phase=="pre") & (sart$block==1)),]
sc_baseSart <- sc_sart[which((sc_sart$phase=="pre") & (sc_sart$block==1)),]


#creating variables minus baseline means per subject
met.vars <- c('responseTime', 'meanRT', 'meanRT_Go', 'meanRT_NoGo', 'propCor', 'propCor_Go', 'propCor_NoGo')

#in addition we create a new list which includes both the changed and unchanged met.vars for scaling later on
scale.vars <- c(rep(NA, length(met.vars)*3))
i = 0
for(v in met.vars){
  new_var <- paste(v, "_diff", sep = "")
  numbers[[new_var]] <- NA
  
  gam_var <- paste(v, "_gam", sep = "")
  numbers[[gam_var]] <- NA
  i = i+1
  scale.vars[[i]] <- v
  i = i+1
  scale.vars[[i]] <- new_var
  i = i+1
  scale.vars[[i]] <- gam_var
  
  for(id in unique(numbers$userID)){
    for(b in 1:2){
      pre_rows <- which((numbers$userID == id) & (numbers$phase=="pre") & (numbers$block==b))
      peri_rows <- which((numbers$userID == id) & (numbers$phase=="peri") & (numbers$block==b))
      s_rows <- which((numbers$userID == id) & (numbers$block==b))
      baselineMean <- mean(numbers[[v]][pre_rows], na.rm=TRUE)
      
      if(is.na(baselineMean)){
        baselineMean <- 0
      }
      
      numbers[[new_var]][s_rows] <- round(numbers[[v]][s_rows] - baselineMean, 2)
      
      numbers[[gam_var]][pre_rows] <- NA
      numbers[[gam_var]][peri_rows] <- round(numbers[[v]][peri_rows] - baselineMean, 2)
    }
  }  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################### SART + ESM ################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### baseline

## meanRT

RT.base.lin0 <- lmer(meanRT ~ group * gameNumSubject + (1 | subject) +
                       ruminating + meanNA + meanPA + distracted + propCor,
                     data = baseSart)

RT.base.lin1 <- lmer(meanRT ~ group * gameNumSubject + (1 | subject) +
                       ruminating + meanNA + meanPA + distracted,
                     data = baseSart)

anova(RT.base.lin0, RT.base.lin1)

RT.base.lin2 <- lmer(meanRT ~ group + gameNumSubject + (1 | subject) +
                       ruminating + meanNA + meanPA + distracted,
                     data = baseSart)

anova(RT.base.lin1, RT.base.lin2) #lin2 preferred

RT.base.lin3 <- lmer(meanRT ~ group + gameNumSubject + (1 | subject) +
                       ruminating + meanNA + distracted,
                     data = baseSart)

anova(RT.base.lin3, RT.base.lin2) #lin3 preferred

RT.base.lin4 <- lmer(meanRT ~ group + gameNumSubject + (1 | subject) +
                       ruminating + distracted,
                     data = baseSart)

anova(RT.base.lin3, RT.base.lin4) #lin4 preferred

RT.base.lin5 <- lmer(meanRT ~ group + gameNumSubject + (1 | subject) +
                       ruminating,
                     data = baseSart)

anova(RT.base.lin5, RT.base.lin4) #lin5 preferred

RT.base.lin6 <- lmer(meanRT ~ group + gameNumSubject + (1 | subject),
                     data = baseSart)

anova(RT.base.lin5, RT.base.lin6) #lin6 preferred

RT.base.lin7 <- lmer(meanRT ~ gameNumSubject + (1 | subject),
                     data = baseSart)

anova(RT.base.lin7, RT.base.lin6) #lin7 preferred


RT.base.lin8 <- lmer(meanRT ~ group + (1 | subject),
                     data = baseSart)
anova(RT.base.lin8, RT.base.lin6) #lin6 preferred
# --> so we compare lin7 with lin6 on Bayes Factor

RT.base.lin9 <- lmer(meanRT ~ gameNumSubject + ruminating + (1 | subject),
                     data = baseSart)

anova(RT.base.lin7, RT.base.lin9) #lin7 preferred
# --> so we compare lin7 with lin6 on Bayes Factor

summary(RT.base.lin6)
summary(RT.base.lin7)

param_tab <- parameters::model_parameters(RT.base.lin6, effects = "fixed")
d <- t_to_d(param_tab$t[2:3], param_tab$df_error[2:3])
interpret_cohens_d(d[1])


baseBF1 <- lmBF(meanRT ~ group + gameNumSubject + subject,
                whichRandom = c("subject"),
                data = baseSart)

baseBF0 <- lmBF(meanRT ~ gameNumSubject + subject,
                whichRandom = c("subject"),
                data = baseSart)

baseBF1 / baseBF0
# BF = 0.413

baseBF3 <- lmBF(meanRT ~ gameNumSubject + ruminating + subject,
                whichRandom = c("subject"),
                data = baseSart)

baseBF3 / baseBF0
# BF = 0.271

baseBF4 <- lmBF(meanRT ~ gameNumSubject + distracted + subject,
                whichRandom = c("subject"),
                data = baseSart)

baseBF4 / baseBF0
# BF = 0.311

baseBF5 <- lmBF(meanRT ~ gameNumSubject + meanNA + subject,
                whichRandom = c("subject"),
                data = baseSart)

baseBF5 / baseBF0
# BF = 2.203

baseBF6 <- lmBF(meanRT ~ gameNumSubject + meanPA + subject,
                whichRandom = c("subject"),
                data = baseSart)

baseBF6 / baseBF0
# BF = 0.297

baseBF7 <- lmBF(meanRT ~ subject, whichRandom = c("subject"),
                data = baseSart)

baseBF0 / baseBF7

## proportionCorrect on NoGo trials

pc.base1 <- glmer(propCor_NoGo ~ group * gameNumSubject +
                    ruminating + distracted + meanNA +
                    (1 | subject),
                  data = baseSart, family = binomial(), weights = propCor_NoGo_nTrials)

summary(pc.base1)

## ruminating

rum.base1 <- lmer(ruminating ~ group * gameNumSubject + meanRT +
                     commissionError + distracted + meanNA + meanPA +
                     (1|subject),
                   data = baseSart)

summary(rum.base1)

param_tab <- parameters::model_parameters(rum.base1, effects = "fixed")
d <- t_to_d(param_tab$t[2:9], param_tab$df_error[2:9])
interpret_cohens_d(d[1])

rum.base.BF1 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                       meanRT + commissionError + subject,
                whichRandom = c("subject"),
                data = baseSart[which(!is.na(baseSart$commissionError)),])

rum.base.BF2 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                       meanRT + subject,
                     whichRandom = c("subject"),
                     data = baseSart[which(!is.na(baseSart$commissionError)),])

rum.base.BF1 / rum.base.BF2
# BF = 0.253

rum.base.BF3 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                       commissionError + subject,
                     whichRandom = c("subject"),
                     data = baseSart[which(!is.na(baseSart$commissionError)),])

rum.base.BF1 / rum.base.BF3
# BF = 0.309  

rum.base.Go1 <- lmer(ruminating ~ group * gameNumSubject + meanRT_Go +
                       propCor_Go + distracted + meanNA + meanPA +
                       (1|subject),
                     data = sc_baseSart)

summary(rum.base.Go1)

rum.Go.BF1 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                       meanRT_Go + propCor_Go + subject,
                     whichRandom = c("subject"),
                     data = sc_baseSart)

rum.Go.BF2 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                       meanRT_Go + subject,
                     whichRandom = c("subject"),
                     data = sc_baseSart)

rum.Go.BF1 / rum.Go.BF2
# BF = 0.311

rum.Go.BF3 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                       propCor_Go + subject,
                     whichRandom = c("subject"),
                     data = sc_baseSart)

rum.Go.BF1 / rum.Go.BF3
# BF = 0.294


rum.base.NoGo1 <- lmer(ruminating ~ group * gameNumSubject + meanRT_NoGo +
                       propCor_NoGo + distracted + meanNA + meanPA +
                       (1|subject),
                     data = sc_baseSart)

summary(rum.base.NoGo1)

rum.NoGo.BF1 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                     meanRT_NoGo + propCor_NoGo + subject,
                   whichRandom = c("subject"),
                   data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

rum.NoGo.BF2 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                     meanRT_NoGo + subject,
                   whichRandom = c("subject"),
                   data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

rum.NoGo.BF1 / rum.NoGo.BF2
# BF = 0.345

rum.NoGo.BF3 <- lmBF(ruminating ~ group * gameNumSubject + distracted + meanNA + meanPA +
                     propCor_NoGo + subject,
                   whichRandom = c("subject"),
                   data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

rum.NoGo.BF1 / rum.NoGo.BF3
# BF = 0.272

## distraction

dist.base1 <- lmer(distracted ~ group * gameNumSubject + ruminating + meanNA + meanPA +
                    meanRT + commissionError +
                    (1|subject),
                  data = baseSart)

summary(dist.base1)

param_tab <- parameters::model_parameters(dist.base1, effects = "fixed")
d <- t_to_d(param_tab$t[2:9], param_tab$df_error[2:9])
interpret_cohens_d(d[1])

dist.base.BF1 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        meanRT + commissionError + subject,
                     whichRandom = c("subject"),
                     data = baseSart[which(!is.na(baseSart$commissionError)),])

dist.base.BF2 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        meanRT + subject,
                     whichRandom = c("subject"),
                     data = baseSart[which(!is.na(baseSart$commissionError)),])

dist.base.BF1 / dist.base.BF2
# BF = 0.322

dist.base.BF3 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        commissionError + subject,
                      whichRandom = c("subject"),
                      data = baseSart[which(!is.na(baseSart$commissionError)),])

dist.base.BF1 / dist.base.BF3
# BF = 0.377

dist.Go.base1 <- lmer(distracted ~ group * gameNumSubject + ruminating + meanNA +
                     meanRT_Go + propCor_Go +
                     (1|subject),
                   data = sc_baseSart)

summary(dist.Go.base1)

dist.Go.BF1 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        meanRT_Go + propCor_Go + subject,
                      whichRandom = c("subject"),
                      data = sc_baseSart)

dist.Go.BF2 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        meanRT_Go + subject,
                      whichRandom = c("subject"),
                      data = sc_baseSart)

dist.Go.BF1 / dist.Go.BF2
# BF = 3.904

dist.Go.BF3 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                      propCor_Go + subject,
                    whichRandom = c("subject"),
                    data = sc_baseSart)

dist.Go.BF1 / dist.Go.BF3
# BF = 0.640

dist.NoGo.base1 <- lmer(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        meanRT_NoGo + propCor_NoGo +
                        (1|subject),
                      data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

summary(dist.NoGo.base1)

dist.NoGo.BF1 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                      meanRT_NoGo + propCor_NoGo + subject,
                    whichRandom = c("subject"),
                    data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

dist.NoGo.BF2 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                      meanRT_NoGo + subject,
                    whichRandom = c("subject"),
                    data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

dist.NoGo.BF1 / dist.NoGo.BF2
# BF = 0.240

dist.NoGo.BF3 <- lmBF(distracted ~ group * gameNumSubject + ruminating + meanNA +
                        propCor_NoGo + subject,
                      whichRandom = c("subject"),
                      data = sc_baseSart[which((!is.na(sc_baseSart$meanRT_NoGo))),])

dist.NoGo.BF1 / dist.NoGo.BF3
# BF = 0.271

## negative affect

na.base1 <- lmer(meanNA ~ group * gameNumSubject + ruminating + distracted + meanPA +
                     meanRT + propCor +
                     (1|subject),
                   data = baseSart)

summary(na.base1)

param_tab <- parameters::model_parameters(na.base1, effects = "fixed")
d <- t_to_d(param_tab$t[2:9], param_tab$df_error[2:9])
interpret_cohens_d(d[1])

na.base.BF1 <- lmBF(meanNA ~ group * gameNumSubject + ruminating + distracted +
                        meanRT + propCor + subject,
                      whichRandom = c("subject"),
                      data = sc_baseSart)

na.base.BF2 <- lmBF(meanNA ~ group * gameNumSubject + ruminating + distracted +
                        meanRT + subject,
                      whichRandom = c("subject"),
                      data = sc_baseSart)

na.base.BF1 / na.base.BF2
# BF = 0.194

na.base.BF3 <- lmBF(meanNA ~ group * gameNumSubject + ruminating + distracted +
                      propCor + subject,
                    whichRandom = c("subject"),
                    data = sc_baseSart)

na.base.BF1 / na.base.BF3
# BF = 0.917

### peri-intervention

RT.peri1 <- lmer(meanRT_gam ~ group * intervention * gameNumSubjB +
                   ruminating_gam + meanNA_gam + meanPA_gam + distracted_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

RT.peri2 <- lmer(meanRT_gam ~ group * intervention + gameNumSubjB + gameNumSubjB:group +
                   ruminating_gam + meanNA_gam + meanPA_gam + distracted_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri1, RT.peri2) #peri2 preferred

RT.peri3 <- lmer(meanRT_gam ~ intervention * group + gameNumSubjB + gameNumSubjB:intervention +
                   ruminating_gam + meanNA_gam + meanPA_gam + distracted_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri3, RT.peri1) #peri3 preferred


RT.peri4 <- lmer(meanRT_gam ~ intervention * group + gameNumSubjB +
                   ruminating_gam + meanNA_gam + meanPA_gam + distracted_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri3, RT.peri4) #peri3 preferred
anova(RT.peri2, RT.peri4) #peri4 preferred


RT.peri5 <- lmer(meanRT_gam ~ intervention + group + gameNumSubjB + gameNumSubjB:intervention +
                   ruminating_gam + meanNA_gam + meanPA_gam + distracted_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri3, RT.peri5) #peri5 preferred

RT.peri6 <- lmer(meanRT_gam ~ group + intervention * gameNumSubjB +
                   ruminating_gam + meanNA_gam + meanPA_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri6, RT.peri5) #peri6 preferred


RT.peri7 <- lmer(meanRT_gam ~ group + intervention * gameNumSubjB +
                   ruminating_gam + meanNA_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri6, RT.peri7) #peri7 preferred


RT.peri8 <- lmer(meanRT_gam ~ group + intervention * gameNumSubjB +
                   ruminating_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri8, RT.peri7) #peri8 preferred


RT.peri9 <- lmer(meanRT_gam ~ group + intervention * gameNumSubjB +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri8, RT.peri9) #peri9 preferred

RT.peri10 <- lmer(meanRT_gam ~ intervention * gameNumSubjB +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri10, RT.peri9) #peri10 preferred

RT.peri11 <- lmer(meanRT_gam ~ intervention + gameNumSubjB +
                    (1 | subjB),
                  data = sart[which(!is.na(sart$meanRT_gam)),])

anova(RT.peri10, RT.peri11) #peri10 preferred


summary(RT.peri10)

param_tab <- parameters::model_parameters(RT.peri10, effects = "fixed")
d <- t_to_d(param_tab$t[2:4], param_tab$df_error[2:4])
interpret_cohens_d(d[1])


periBF0 <- lmBF(meanRT_gam ~ intervention + gameNumSubjB + subjB,
     whichRandom = c("subjB"),
     data = sart[which(!is.na(sart$meanRT_gam)),])


periBF1 <- lmBF(meanRT_gam ~ intervention * gameNumSubjB + subjB,
                whichRandom = c("subjB"),
                data = sart[which(!is.na(sart$meanRT_gam)),])

periBF1 / periBF0
#1.815

periBF1b <- lmBF(meanRT_gam ~  gameNumSubjB + subjB,
                whichRandom = c("subjB"),
                data = sart[which(!is.na(sart$meanRT_gam)),])

periBF1b / periBF0
#1.1562

periBF2 <- lmBF(meanRT_gam ~ intervention * gameNumSubjB + ruminating_gam+ subjB,
                whichRandom = c("subjB"),
                data = sart[which(!is.na(sart$meanRT_gam)),])

periBF2 / periBF1
#0.278

periBF3 <- lmBF(meanRT_gam ~ intervention * gameNumSubjB + distracted_gam+ subjB,
                whichRandom = c("subjB"),
                data = sart[which(!is.na(sart$meanRT_gam)),])

periBF3 / periBF1
#0.370


periBF4 <- lmBF(meanRT_gam ~ intervention * gameNumSubjB + meanNA_gam + subjB,
                whichRandom = c("subjB"),
                data = sart[which(!is.na(sart$meanRT_gam)),])

periBF4 / periBF1
#0.287


periBF5 <- lmBF(meanRT_gam ~ intervention * gameNumSubjB + meanPA_gam + subjB,
                whichRandom = c("subjB"),
                data = sart[which(!is.na(sart$meanRT_gam)),])

periBF5 / periBF1
#0.421


## rumination

rum.peri1 <- lmer(ruminating_gam ~ group + intervention * gameNumSubjB +
                   meanNA_gam + meanPA_gam + distracted_gam +
                   meanRT_gam + commissionError_gam +
                   (1 | subjB),
                 data = sart[which(!is.na(sart$meanRT_gam)),])

summary(rum.peri1)

param_tab <- parameters::model_parameters(rum.peri1, effects = "fixed")
d <- t_to_d(param_tab$t[2:10], param_tab$df_error[2:10])
interpret_cohens_d(d[1])

rum.peri.BF1 <- lmBF(ruminating_gam ~ group + intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       meanRT_gam + commissionError_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which((!is.na(sart$meanRT_gam)) & (!is.na(sart$commissionError))),])

rum.peri.BF2 <- lmBF(ruminating_gam ~ group + intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       meanRT_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which((!is.na(sart$meanRT_gam)) & (!is.na(sart$commissionError))),])

rum.peri.BF1 / rum.peri.BF2
#BF = 0.254

rum.peri.BF3 <- lmBF(ruminating_gam ~ group + intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       commissionError_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which((!is.na(sart$meanRT_gam)) & (!is.na(sart$commissionError))),])

rum.peri.BF1 / rum.peri.BF3
#BF = 0.342

rum.Go.peri1 <- lmer(ruminating_gam ~ group * intervention * gameNumSubjB +
                    meanNA_gam + meanPA_gam + distracted_gam +
                    meanRT_Go_gam + propCor_Go_gam +
                    (1 | subjB),
                  data = sart[which(!is.na(sart$meanRT_Go_gam)),])

summary(rum.Go.peri1)

rum.peri.Go1 <- lmBF(ruminating_gam ~ group * intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       meanRT_Go_gam + propCor_Go_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which(!is.na(sart$meanRT_Go_gam)),])

rum.peri.Go2 <- lmBF(ruminating_gam ~ group * intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       meanRT_Go_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which(!is.na(sart$meanRT_Go_gam)),])

rum.peri.Go1 / rum.peri.Go2
#BF = 0.381

rum.peri.Go3 <- lmBF(ruminating_gam ~ group * intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       propCor_Go_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which(!is.na(sart$meanRT_Go_gam)),])

rum.peri.Go1 / rum.peri.Go3
#BF = 0.296

rum.NoGo.peri1 <- lmer(ruminating_gam ~ group * intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       meanRT_NoGo_gam + propCor_NoGo_gam +
                       (1 | subjB),
                     data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

summary(rum.NoGo.peri1)

rum.peri.NoGo1 <- lmBF(ruminating_gam ~ group * intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + distracted_gam +
                       meanRT_NoGo_gam + propCor_NoGo_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

rum.peri.NoGo2 <- lmBF(ruminating_gam ~ group * intervention * gameNumSubjB +
                         meanNA_gam + meanPA_gam + distracted_gam +
                         meanRT_NoGo_gam + subjB,
                       whichRandom = c("subjB"),
                       data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

rum.peri.NoGo1 / rum.peri.NoGo2
#BF = 0.294

rum.peri.NoGo3 <- lmBF(ruminating_gam ~ group * intervention * gameNumSubjB +
                         meanNA_gam + meanPA_gam + distracted_gam +
                         propCor_NoGo_gam + subjB,
                       whichRandom = c("subjB"),
                       data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

rum.peri.NoGo1 / rum.peri.NoGo3
#BF = 0.332

## distraction

dist.peri1 <- lmer(distracted_gam ~ group + intervention * gameNumSubjB +
                    meanNA_gam + meanPA_gam + ruminating_gam +
                    meanRT_gam + commissionError_gam +
                    (1 | subjB),
                  data = sart[which(!is.na(sart$meanRT_gam)),])

summary(dist.peri1)

param_tab <- parameters::model_parameters(dist.peri1, effects = "fixed")
d <- t_to_d(param_tab$t[2:10], param_tab$df_error[2:10])
interpret_cohens_d(d[1])

dist.peri.BF1 <- lmBF(distracted_gam ~ group + intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + ruminating_gam +
                       meanRT_gam + commissionError_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which((!is.na(sart$meanRT_gam)) & (!is.na(sart$commissionError))),])

dist.peri.BF2 <- lmBF(distracted_gam ~ group + intervention * gameNumSubjB +
                       meanNA_gam + meanPA_gam + ruminating_gam +
                       meanRT_gam + subjB,
                     whichRandom = c("subjB"),
                     data = sart[which((!is.na(sart$meanRT_gam)) & (!is.na(sart$commissionError))),])

dist.peri.BF1 / dist.peri.BF2
#BF = 0.283

dist.peri.BF3 <- lmBF(distracted_gam ~ group + intervention * gameNumSubjB +
                        meanNA_gam + meanPA_gam + ruminating_gam +
                        commissionError_gam + subjB,
                      whichRandom = c("subjB"),
                      data = sart[which((!is.na(sart$meanRT_gam)) & (!is.na(sart$commissionError))),])

dist.peri.BF1 / dist.peri.BF3
#BF = 0.445

dist.peri.Go1 <- lmer(distracted_gam ~ group * intervention * gameNumSubjB +
                     meanNA_gam + meanPA_gam + ruminating_gam +
                     meanRT_Go_gam + propCor_Go_gam +
                     (1 | subjB),
                   data = sart[which(!is.na(sart$meanRT_Go_gam)),])

summary(dist.peri.Go1)

dist.peri.Go.BF1 <- lmBF(distracted_gam ~ group * intervention * gameNumSubjB +
                        meanNA_gam + meanPA_gam + ruminating_gam +
                        meanRT_Go_gam + propCor_Go_gam + subjB,
                      whichRandom = c("subjB"),
                      data = sart[which(!is.na(sart$meanRT_Go_gam)),])

dist.peri.Go.BF2 <- lmBF(distracted_gam ~ group * intervention * gameNumSubjB +
                           meanNA_gam + meanPA_gam + ruminating_gam +
                           meanRT_Go_gam + subjB,
                         whichRandom = c("subjB"),
                         data = sart[which(!is.na(sart$meanRT_Go_gam)),])

dist.peri.Go.BF1 / dist.peri.Go.BF2
#BF = 0.569

dist.peri.Go.BF3 <- lmBF(distracted_gam ~ group * intervention * gameNumSubjB +
                           meanNA_gam + meanPA_gam + ruminating_gam +
                           propCor_Go_gam + subjB,
                         whichRandom = c("subjB"),
                         data = sart[which(!is.na(sart$meanRT_Go_gam)),])

dist.peri.Go.BF1 / dist.peri.Go.BF3
#BF = 0.399

dist.peri.NoGo1 <- lmer(distracted_gam ~ group * intervention * gameNumSubjB +
                        meanNA_gam + meanPA_gam + ruminating_gam +
                        meanRT_NoGo_gam + propCor_NoGo_gam +
                        (1 | subjB),
                      data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

summary(dist.peri.NoGo1)

dist.peri.NoGo.BF1 <- lmBF(distracted_gam ~ group * intervention * gameNumSubjB +
                        meanNA_gam + meanPA_gam + ruminating_gam +
                        meanRT_NoGo_gam + propCor_NoGo_gam + subjB,
                      whichRandom = c("subjB"),
                      data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

dist.peri.NoGo.BF2 <- lmBF(distracted_gam ~ group * intervention * gameNumSubjB +
                             meanNA_gam + meanPA_gam + ruminating_gam +
                             meanRT_NoGo_gam + subjB,
                           whichRandom = c("subjB"),
                           data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

dist.peri.NoGo.BF1 / dist.peri.NoGo.BF2
#BF = 0.720

dist.peri.NoGo.BF3 <- lmBF(distracted_gam ~ group * intervention * gameNumSubjB +
                             meanNA_gam + meanPA_gam + ruminating_gam +
                             propCor_NoGo_gam + subjB,
                           whichRandom = c("subjB"),
                           data = sart[which(!is.na(sart$meanRT_NoGo_gam)),])

dist.peri.NoGo.BF1 / dist.peri.NoGo.BF3
#BF = 1.028

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################### SART only ################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### baseline

numbers$cRT <- numbers$responseTime - mean(numbers$responseTime, na.rm = TRUE)
numbers$scaledRT <- scale(numbers$responseTime)

## all responseTimes

length(numbers[which(numbers$phase=="pre"),]$subject) #19608 trials

base.m1 <- lmer(responseTime ~ group * isGo + factor(correct) + gameSessionID + (1 | subject),
                data = numbers[which(numbers$phase == "pre"),])

base.m2 <- lmer(responseTime ~ group + isGo + factor(correct) + gameSessionID + (1 | subject),
                data = numbers[which(numbers$phase == "pre"),])

anova(base.m1, base.m2) #m1 preferred

base.m3 <- lmer(responseTime ~ group * isGo + gameSessionID + (1 | subject),
                data = numbers[which(numbers$phase == "pre"),])

anova(base.m1, base.m3) #m3 preferred

base.m4 <- lmer(responseTime ~ group * isGo + (1 | subject),
                data = numbers[which(numbers$phase == "pre"),])

anova(base.m4, base.m3) #m3 preferred

summary(base.m3)

base.m3b <- lmer(responseTime ~ group * isGo + gameSessionID + factor(correct) + (1 | subject),
                data = numbers[which(numbers$phase == "pre"),])

summary(base.m3b)

param_tab <- parameters::model_parameters(base.m3, effects = "fixed")
d <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
interpret_cohens_d(d[1])


## Go responseTimes

numbersGo <- numbers[which(numbers$isGo==TRUE),]

length(numbersGo[which(numbersGo$phase=="pre"),]$subject) #17643 trials

base.Go.m1 <- lmer(responseTime ~ group + gameSessionID + (1 | subject),
                data = numbersGo[which(numbersGo$phase == "pre"),]) #correct is being dropped --> rank deficient matrix

base.Go.m2 <- lmer(responseTime ~  group + (1 | subject),
                data = numbersGo[which(numbersGo$phase == "pre"),])

anova(base.Go.m1, base.Go.m2) #m1 preferred

summary(base.Go.m1)

param_tab <- parameters::model_parameters(base.Go.m1, effects = "fixed")
d <- t_to_d(param_tab$t[2:3], param_tab$df_error[2:3])
interpret_cohens_d(d[1])

## No responseTimes

numbersNoGo <- numbers[which(numbers$isGo==FALSE),]

length(numbersNoGo[which(numbersNoGo$phase=="pre"),]$subject) #1965 trials

base.NoGo.m1 <- lmer(responseTime ~ group + correct + gameSessionID + (1 | subject),
                   data = numbersNoGo[which(numbersNoGo$phase == "pre"),]) 

base.NoGo.m2 <- lmer(responseTime ~  group + correct + (1 | subject),
                   data = numbersNoGo[which(numbersNoGo$phase == "pre"),])

anova(base.NoGo.m1, base.NoGo.m2) #m1 preferred

base.NoGo.m3 <- lmer(responseTime ~ group + gameSessionID + (1 | subject),
                     data = numbersNoGo[which(numbersNoGo$phase == "pre"),])

anova(base.NoGo.m1, base.NoGo.m3) #m3 preferred

summary(base.NoGo.m3)

param_tab <- parameters::model_parameters(base.NoGo.m3, effects = "fixed")
d <- t_to_d(param_tab$t[2:3], param_tab$df_error[2:3])
interpret_cohens_d(d[1])

## correct 
correct.m1 <- glmer(factor(correct) ~ group * scale(responseTime) * isGo + gameSessionID + (1 | subject),
                data = numbers, family = binomial())

summary(correct.m1)


#### peri

## all responseTimes

peri.m1 <- lmer(responseTime_gam ~ group * intervention * isGo + factor(correct) + gameSessionID + intervention:gameSessionID +
                  (1 | subject),
                data = numbers)

peri.m1b <- lmer(responseTime_gam ~ group * intervention * isGo + factor(correct) + gameSessionID +
                  (1 | subject),
                data = numbers)

anova(peri.m1, peri.m1b) #m1 preferred

peri.m1c <- lmer(responseTime_gam ~ group * intervention * isGo + gameSessionID + intervention:gameSessionID +
                   (1 | subject),
                 data = numbers)

anova(peri.m1, peri.m1c) #m1c preferred

peri.m2 <- lmer(responseTime_gam ~ group * intervention + isGo + group:isGo + gameSessionID + intervention:gameSessionID +
                  (1 | subject),
                data = numbers)

anova(peri.m1c, peri.m2) #m2 preferred

peri.m3 <- lmer(responseTime_gam ~ group * intervention + isGo + intervention:isGo + gameSessionID + intervention:gameSessionID +
                  (1 | subject),
                data = numbers)

anova(peri.m1c, peri.m3) #m3 preferred

peri.m4 <- lmer(responseTime_gam ~ group * intervention + isGo + gameSessionID + intervention:gameSessionID +
                  (1 | subject),
                data = numbers)

anova(peri.m4, peri.m2) #m4 preferred
anova(peri.m4, peri.m3) #m4 preferred

peri.m5 <- lmer(responseTime_gam ~ group + intervention + isGo + gameSessionID + intervention:gameSessionID +
                  (1 | subject),
                data = numbers)

anova(peri.m4, peri.m5) #m5 preferred

peri.m7 <- lmer(responseTime_gam ~ group + intervention + gameSessionID + intervention:gameSessionID +
                  (1 | subject),
                data = numbers)

anova(peri.m7, peri.m5) #m7 preferred

summary(peri.m7)

param_tab <- parameters::model_parameters(peri.m7, effects = "fixed")
d <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
interpret_cohens_d(d[1])

## Go responseTimes

peri.Go.m1 <- lmer(responseTime_gam ~ group * intervention * gameSessionID + correct +
                  (1 | subject),
                data = numbersGo) #adding correct interactions lead to rank deficiency

peri.Go.m2 <- lmer(responseTime_gam ~ group + intervention + gameSessionID + correct +
                     group:gameSessionID + group:intervention + intervention:gameSessionID + group:gameSessionID +
                   (1 | subject),
                 data = numbersGo)

anova(peri.Go.m1, peri.Go.m2) #m1 preferred

peri.Go.m3 <- lmer(responseTime_gam ~ group * intervention * gameSessionID +
                     (1 | subject),
                   data = numbersGo)

anova(peri.Go.m1, peri.Go.m3) #m1 preferred

summary(peri.Go.m1)

param_tab <- parameters::model_parameters(peri.Go.m1, effects = "fixed")
d <- t_to_d(param_tab$t[2:9], param_tab$df_error[2:9])
interpret_cohens_d(d[1])


## NoGo responseTimes

peri.NoGo.m1 <- lmer(responseTime_gam ~ group * intervention * gameSessionID + correct +
                       intervention:correct + gameSessionID:correct +
                     (1 | subject),
                   data = numbersNoGo) #adding group:correct leads to rank deficiency

peri.NoGo.m2 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       group:intervention + group:gameSessionID + intervention:gameSessionID +
                       correct + intervention:correct + gameSessionID:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m1, peri.NoGo.m2) #m2 preferred

peri.NoGo.m3 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       group:intervention + group:gameSessionID +
                       correct + intervention:correct + gameSessionID:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m3, peri.NoGo.m2) #m2 preferred

peri.NoGo.m4 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       group:intervention + intervention:gameSessionID +
                       correct + intervention:correct + gameSessionID:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m4, peri.NoGo.m2) #m4 preferred

peri.NoGo.m5 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       intervention:gameSessionID +
                       correct + intervention:correct + gameSessionID:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m4, peri.NoGo.m5) #m4 preferred

peri.NoGo.m6 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       group:intervention +
                       correct + intervention:correct + gameSessionID:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m4, peri.NoGo.m6) #m6 preferred

peri.NoGo.m7 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       correct + intervention:correct + gameSessionID:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m7, peri.NoGo.m6) #m7 preferred

peri.NoGo.m8 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       correct + intervention:correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m7, peri.NoGo.m8) #m8 preferred

peri.NoGo.m9 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       correct +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m9, peri.NoGo.m8) #m9 preferred

peri.NoGo.m10 <- lmer(responseTime_gam ~ group + intervention + gameSessionID +
                       (1 | subject),
                     data = numbersNoGo)

anova(peri.NoGo.m9, peri.NoGo.m10) #m10 preferred

peri.NoGo.m11 <- lmer(responseTime_gam ~ group + gameSessionID +
                        (1 | subject),
                      data = numbersNoGo)

anova(peri.NoGo.m11, peri.NoGo.m10) #m11 preferred

summary(peri.NoGo.m11)

param_tab <- parameters::model_parameters(peri.NoGo.m11, effects = "fixed")
d <- t_to_d(param_tab$t[2:3], param_tab$df_error[2:3])
interpret_cohens_d(d[1])


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################### Overall levels ################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

missing_data <- ddply(data, .(patient_id, id, group, intervention), plyr::summarise,
                      numBeeped = length(mindcog_db_open_from),
                      responseRate = round((numBeeped - length(unique(mindcog_db_non_response)))/numBeeped,2))


#using the values only of the closest ESM beeps
overall <- ddply(sart, .(subject, group, intervention, phase), plyr::summarise,
                totalMeanRT = mean(meanRT, na.rm=TRUE),
                totalMeanRum = mean(ruminating, na.rm=TRUE),
                totalMeanDis = mean(distracted, na.rm=TRUE),
                totalMeanSti = mean(stickiness, na.rm=TRUE))


#using all ESM beep values
overall2 <- ddply(data, .(subject, group, intervention, phase), plyr::summarise,
                 totalMeanRT = mean(meanRT, na.rm=TRUE),
                 totalMeanRum = mean(ruminating, na.rm=TRUE),
                 totalMeanDis = mean(distracted, na.rm=TRUE),
                 totalMeanSti = mean(stickiness, na.rm=TRUE))

cor(overall$totalMeanRT, overall$totalMeanRum)
cor(overall$totalMeanRT, overall$totalMeanDis)
cor(overall$totalMeanRT, overall$totalMeanSti)

cor(overall2$totalMeanRT, overall2$totalMeanRum, use="complete.obs")
cor(overall2$totalMeanRT, overall2$totalMeanDis, use="complete.obs")
cor(overall2$totalMeanRT, overall2$totalMeanSti, use="complete.obs")









