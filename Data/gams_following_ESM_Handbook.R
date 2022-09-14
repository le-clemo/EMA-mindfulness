rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data") #/ESM/mindcog_v202204

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/")
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
library(performance)
library(lme4)
library(effectsize)
library(lmerTest) #to more quickly be able to see significance
library(plotfunctions)
library(gratia)
library(arm)
library(ggthemes)
library(ggpattern)


R.version.string




########################################################## Rescaling function #############################################################
#source: https://stackoverflow.com/questions/23642111/how-to-unscale-the-coefficients-from-an-lmer-model-fitted-with-a-scaled-respon

rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  # print(beta)
  # print(mu)
  # print(sigma)
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  return(beta2)
}
########################################################## ################### #############################################################


#read in data
data <- read.csv('merged_data.csv') 

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

data$sumNA <- data$sumNA / 4
data$sumPA <- data$sumPA / 3



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



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++ Intraclass correlation coefficient +++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#The intraclass correlation coefficient (ICC) gives us the proportion of the variance explained by between-person effects.

baseDat <- data[which((data$phase=="pre")),]
sc_baseDat <- sc_data[which((sc_data$phase=="pre")),]

#ruminating
rum.int <- lmer(ruminating ~ 1 + (1|subject), data = baseDat)
summary(rum.int) #between-subject variance 177.0; within-subject 152.8
icc(rum.int) #0.54

#using scaled data
rum.int.sc <- lmer(ruminating ~ 1 + (1|subject), data = sc_baseDat)
summary(rum.int.sc) #between-subject variance 0.6022; within-subject 0.5201
icc(rum.int.sc) #0.54


#NA
na.int <- lmer(sumNA ~ 1 + (1|subject), data = baseDat)
summary(na.int) #1386.5; 972.8
icc(na.int) #0.59

#using scaled data
na.int.sc <- lmer(sumNA ~ 1 + (1|subject), data = sc_baseDat)
summary(na.int.sc) #0.5841  #0.4098

#PA
pa.int <- lmer(sumPA ~ 1 + (1|subject), data = baseDat)
summary(pa.int) #1392; 1391
icc(pa.int) #0.50

#scaled data
pa.int.sc <- lmer(sumPA ~ 1 + (1|subject), data = sc_baseDat)
summary(pa.int.sc) #0.4655; 0.4650



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++ Between-person effect +++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#ruminating
rum.bet <- lmer(ruminating ~ group + (1|subject), data = baseDat)
rum.bet2 <- gam(ruminating ~ group + s(subject, bs="re"), data = baseDat)
summary(rum.bet) #betw-subj var 153.8; within-subj var 152.8
summary(rum.bet2) #estimate groupcontrols: -10.850; t-value -2.282, #p=0.02253

#To calculate the variability explained by group we compare the between-subject variability of the intercept model
#with the one of the current model --> (177.0 - 153.8) / 177.0 = 13.11%

#we get the same result (appr.) with the scaled data
rum.bet.sc <- lmer(ruminating ~ group + (1|subject), data = sc_baseDat)
summary(rum.bet.sc) #betw-subj var 0.5234; within-subj var 0.5201
# (0.6022 - 0.5234) / 0.6022


#NA
na.bet <- lmer(sumNA ~ group + (1|subject), data = baseDat)
na.bet2 <- gam(sumNA ~ group + s(subject, bs="re"), data = baseDat)
summary(na.bet) #1258.1; 972.8
summary(na.bet2) #estimate -26.487; t-value -2.111, #p=0.0348
# --> 9.26%


#PA
pa.bet <- lmer(sumPA ~ group + (1|subject), data = baseDat)
pa.bet2 <- gam(sumPA ~ group + s(subject, bs="re"), data = baseDat)
summary(pa.bet) #1414; 1391
summary(pa.bet2) #estimate 9.846; t-value 0.719, #p=0.477
# -->  -0.015 --> 0%



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++ Within-person effect of events ++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### negMax

#ruminating
rum.event <- lmer(ruminating ~ negMax + (negMax | subject), data = sc_baseDat)
rum.event2 <- gam(ruminating ~ negMax + s(negMax, by = subject, bs="re"), data = sc_baseDat)
summary(rum.event) #betw-subj var 0.4514; within-subj var 0.4678
summary(rum.event2) #estimate 10.850; t-value 2.282, #p=0.02253

# calculate the proportion of within-subject variability explained by unpleasantness of event
# (0.5201 - 0.4678) / 0.5201 = 10.06%
# check_model(rum.event)

# rescale values
b1S <- fixed_effects(rum.event)
icol <- which(colnames(data)=="ruminating")
jcol <- which(colnames(data)=="negMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  # rescaled coefficient of negMax --> 0.1585054


#NA
na.event <- lmer(sumNA ~ negMax + (negMax | subject), data = sc_baseDat)
summary(na.event) #betw-subj var 0.46340; within-subj var 0.34179
# (0.4098 - 0.34179) / 0.4098 = 16.60%

b1S <- fixed_effects(na.event)
icol <- which(colnames(data)=="sumNA")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  # rescaled coefficient of negMax --> 1.303891


#PA
pa.event <- lmer(sumPA ~ negMax + (negMax | subject), data = sc_baseDat)
summary(pa.event) #betw-subj var 0.4514; within-subj var 0.422629
# (0.4650 - 0.422629 ) / 0.4650 = 9.11%

b1S <- fixed_effects(pa.event)
icol <- which(colnames(data)=="sumPA")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  # rescaled coefficient of negMax --> -0.411049


### posMax

#ruminating
rum.event <- lmer(ruminating ~ posMax + (posMax | subject), data = sc_baseDat)
summary(rum.event) #betw-subj var 0.58108; within-subj var 0.48072

# calculate the proportion of within-subject variability explained by pleasantness of event
# (0.5201 - 0.48072) / 0.5201 = 7.57%

# rescale values
b1S <- fixed_effects(rum.event)
icol <- which(colnames(data)=="ruminating")
jcol <- which(colnames(data)=="posMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  # rescaled coefficient of posMax --> -0.1300809

param_tab <- parameters::model_parameters(rum.event)
d <- t_to_d(param_tab$t[2], param_tab$df_error[2])
interpret_cohens_d(d[1])


#NA
na.event <- lmer(sumNA ~ posMax + (posMax | subject), data = sc_baseDat)
summary(na.event) #betw-subj var 0.59366; within-subj var 0.38020 

# calculate the proportion of within-subject variability explained by pleasantness of event
# (0.4098 - 0.38020) / 0.4098 = 7.22%

# rescale values
b1S <- fixed_effects(na.event)
icol <- which(colnames(data)=="sumNA")
jcol <- which(colnames(data)=="posMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  # rescaled coefficient of posMax --> -0.3579037

param_tab <- parameters::model_parameters(na.event)
d <- t_to_d(param_tab$t[2], param_tab$df_error[2])
interpret_cohens_d(d[1]) #0.17 --> very small


#PA
pa.event <- lmer(sumPA ~ posMax + (posMax | subject), data = sc_baseDat)
summary(pa.event) #betw-subj var 0.36272  ; within-subj var 0.35067  

# calculate the proportion of within-subject variability explained by pleasantness of event
# (0.4650 - 0.35067 ) / 0.4650 = 24.59%

# rescale values
b1S <- fixed_effects(pa.event)
icol <- which(colnames(data)=="sumPA")
jcol <- which(colnames(data)=="posMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  # rescaled coefficient of posMax --> 1.023515 

param_tab <- parameters::model_parameters(pa.event)
d <- t_to_d(param_tab$t[2], param_tab$df_error[2])
interpret_cohens_d(d[1]) #0.38 --> small

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++ Between-person effect of events +++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### negMax

#ruminating
rum.event.bet <- lmer(ruminating ~ group * negMax + (negMax | subject), data = sc_baseDat)
summary(rum.event.bet)

# calculate the proportion of between-subject variability explained by unpleasantness of event
# (0.0131 - 0.01347) / 0.0131 = -0.0282 --> 0%

b1S <- fixed_effects(rum.event.bet)
icol <- which(colnames(data)=="ruminating")
jcol <- which(colnames(data)=="negMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  

param_tab <- parameters::model_parameters(rum.event.bet)
d <- t_to_d(param_tab$t[2:4], param_tab$df_error[2:4])
interpret_cohens_d(d[1])

# rescaled negMax --> 0.14926427 ; groupremitted = 0.32045669; groupremitted:negMax = 0.02431809
# d                   0.23 (small);               0.07 (very small)                 0.02 (very small)

#NA
na.event.bet <- lmer(sumNA ~ group * negMax + (negMax | subject), data = sc_baseDat)
summary(na.event.bet)

# calculate the proportion of between-subject variability explained by unpleasantness of event
# (0.02069 - 0.02124) / 0.02069  = -0.02658 --> 0%

b1S <- fixed_effects(na.event.bet)
icol <- which(colnames(data)=="sumNA")
jcol <- which(colnames(data)=="negMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  

param_tab <- parameters::model_parameters(na.event.bet)
d <- t_to_d(param_tab$t[2:4], param_tab$df_error[2:4])
interpret_cohens_d(d[1])

# rescaled negMax --> 0.53916311 ; groupremitted = 0.71347241; groupremitted:negMax = 0.08494209
# d                   0.26 (small);               0.05 (very small)                 0.03 (very small)


#PA
pa.event.bet <- lmer(sumPA ~ group * negMax + (negMax | subject), data = sc_baseDat)
summary(pa.event.bet) 

# calculate the proportion of between-subject variability explained by unpleasantness of event
# (0.006473  - 0.007003) / 0.006473  = -0.081876 --> 0%

b1S <- fixed_effects(pa.event.bet)
icol <- which(colnames(data)=="sumPA")
jcol <- which(colnames(data)=="negMax")
p.order <- c(c(icol,jcol)[-icol])
m <- colMeans(data[p.order], na.rm = TRUE)
s <- apply(data,2,sd, na.rm=TRUE)[p.order]
rescale.coefs(b1S,m,s)  

param_tab <- parameters::model_parameters(pa.event.bet)
d <- t_to_d(param_tab$t[2:4], param_tab$df_error[2:4])
interpret_cohens_d(d[1])

# rescaled negMax --> -0.42849492 ; groupremitted = -0.19961521; groupremitted:negMax = 0.04533195
# d                   0.25 (small);               0.01 (very small)                 0.02 (very small)


### posMax

#ruminating
rum.event.bet2 <- lmer(ruminating ~ group * posMax + (posMax | subject), data = sc_baseDat)
summary(rum.event.bet2)

# calculate the proportion of between-subject variability explained by unpleasantness of event
# (0.0131 - 0.01688  ) / 0.0131 --> 0%


#NA
na.event.bet2 <- lmer(sumNA ~ group * posMax + (posMax | subject), data = sc_baseDat)
summary(na.event.bet2)

# calculate the proportion of between-subject variability explained by unpleasantness of event
# (0.02069 - 0.01934  ) / 0.02069  = -0.02658 --> 6.52%


#PA
pa.event.bet2 <- lmer(sumPA ~ group * posMax + (posMax | subject), data = sc_baseDat)
summary(pa.event.bet2) 

# calculate the proportion of between-subject variability explained by unpleasantness of event
# (0.006473  - 0.03313  ) / 0.03313 --> 0%




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++ Non-linear models ++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### rumination

rum.gam.max3b <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                       s(sumNA, by = group) + s(sumPA, by = group) +
                       s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                       s(distracted, by = group) + s(listless, by = group) + s(sleepQuality, by = group) + s(companyPleasant, by = group) +
                       s(phaseBeepNum, by = subjB, bs="fs", m=1),
                     data = sc_baseDat)

summary(rum.gam.max3b)
#report_stats(rum.gam.max3b)

rum.gam1 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                       s(sumNA, by = group) + s(sumPA, by = group) +
                       s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                       s(distracted, by = group) + s(listless, by = group) + s(sleepQuality, by = group) + s(companyPleasant, by = group) +
                       s(phaseBeepNum, by = subjB, bs="fs", m=1),
                     data = sc_baseDat, method = "ML")

#removing interaction sleepQuality x group
rum.gam2 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                  s(distracted, by = group) + s(listless, by = group) + s(sleepQuality) + s(companyPleasant, by = group) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam1, rum.gam2) #rum.gam2 better

#removing interaction companyPleasant x group
rum.gam3 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                  s(distracted, by = group) + s(listless, by = group) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam3, rum.gam2) #rum.gam3 better

#removing interaction listless x group
rum.gam4 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                  s(distracted, by = group) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam3, rum.gam4) #rum.gam4 better


#removing interaction distracted x group
rum.gam5 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam5, rum.gam4) #rum.gam5 better


#removing interaction stickiness x group
rum.gam6 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam5, rum.gam6) # significant --> rum.gam5 better


#removing interaction posMax x group
rum.gam7 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam7, rum.gam5) #rum.gam7 better

save(rum.gam7, file = "models_rumination/rum.gam7.rda")
load("models_rumination/rum.gam7.rda")

#removing interaction negMax x group
rum.gam8 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax) + s(posMax) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam7, rum.gam8) #rum.gam8 better

#removing interaction sumPA x group
rum.gam9 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA) +
                  s(negMax) + s(posMax) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam9, rum.gam8) #significant <- rum.gam8 better


#removing interaction sumNA x group
rum.gam10 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA) + s(sumPA, by = group) +
                  s(negMax) + s(posMax) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam10, rum.gam8) #significant <- rum.gam8 better

#removing companyPleasant
rum.gam11 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax) + s(posMax) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam11, rum.gam8) #significant <- rum.gam8 better


#removing sleepQuality
rum.gam12 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                   s(sumNA, by = group) + s(sumPA, by = group) +
                   s(negMax) + s(posMax) + s(stickiness, by = group) +
                   s(distracted) + s(listless) + s(companyPleasant) +
                   s(phaseBeepNum, by = subjB, bs="fs", m=1),
                 data = sc_baseDat, method = "ML")

compareML(rum.gam8, rum.gam12) #significant <- rum.gam8 better

#removing listless
rum.gam13 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                  s(sumNA, by = group) + s(sumPA, by = group) +
                  s(negMax) + s(posMax) + s(stickiness, by = group) +
                  s(distracted) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

compareML(rum.gam13, rum.gam8) #not significant rum.gam13 better

#removing distracted
rum.gam14 <- bam(ruminating ~ group + s(phaseBeepNum, by = group) +
                   s(sumNA, by = group) + s(sumPA, by = group) +
                   s(negMax) + s(posMax) + s(stickiness, by = group) +
                   s(sleepQuality) + s(companyPleasant) +
                   s(phaseBeepNum, by = subjB, bs="fs", m=1),
                 data = sc_baseDat, method = "ML")

compareML(rum.gam13, rum.gam14) #significant rum.gam13 better



# #removing interaction phaseBeepNum x group
# rum.gam10 <- bam(ruminating ~ group + s(phaseBeepNum) +
#                    s(sumNA, by = group) + s(sumPA, by = group) +
#                    s(negMax) + s(posMax) + s(stickiness, by = group) +
#                    s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
#                    s(phaseBeepNum, by = subjB, bs="fs", m=1),
#                  data = sc_baseDat, method = "ML")
# 
# compareML(rum.gam10, rum.gam8) # not significant but we keep rum.gam8


#refit without ML
rum.gam <- gam(ruminating ~ group + s(phaseBeepNum, by = group) +
                 s(sumNA, by = group) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness, by = group) +
                 s(distracted) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat)

rum.summary <- summary(rum.gam)

save(rum.gam, rum.summary, file = "models_rumination/rum_final_contemp.rda")
load(file = "models_rumination/rum_final_contemp.rda")

rum.gam_unscaled <- gam(ruminating ~ group + s(phaseBeepNum, by = group) +
                 s(sumNA, by = group) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness, by = group) +
                 s(distracted) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = baseDat)

rum.sum.unscaled <- summary(rum.gam_unscaled)


save(rum.gam_unscaled, rum.sum.unscaled, file = "models_rumination/rum_unscaled_contemp.rda")

load("models_rumination/rum_unscaled_contemp.rda")


plot_parametric(rum.gam, pred = list(group=c("remitted", "controls")),
                parametricOnly = T, col = c("red", "blue"))


report_stats(rum.gam)

model1.acf <- acf_resid(rum.gam)

par(mfrow = c(2,2))
plot_smooth(rum.gam, plot_all = "group", view = "phaseBeepNum", legend = FALSE,
            main ="", ylab = "Rumination", xlab = "Assessment Number")

plot_smooth(rum.gam, plot_all = "group", view = "stickiness", legend = FALSE,
            main ="", ylab = "", xlab = "Stickiness")

plot_smooth(rum.gam, plot_all = "group", view = "sumPA",
            main ="", ylab = "Rumination", xlab = "Positive Affect")

plot_smooth(rum.gam, plot_all = "group", view = "sumNA", legend = FALSE,
            main ="", ylab = "", xlab = "Negative Affect")


plot_diff(rum.gam, view="phaseBeepNum", comp = list(group = c("controls", "remitted")),
          main ="", ylab = "Rumination", xlab = "Assessment Number")

plot_diff(rum.gam, view="stickiness", comp = list(group = c("controls", "remitted")),
          main ="", ylab = "", xlab = "Stickiness")

plot_diff(rum.gam, view="sumNA", comp = list(group = c("controls", "remitted")),
          main ="", ylab = "Rumination", xlab = "Negative Affect")

plot_diff(rum.gam, view="sumPA", comp = list(group = c("controls", "remitted")),
          main ="", ylab = "", xlab = "Positive Affect")

#plot_modelfit(rum.gam, view="phaseBeepNum", event = "subjB")

param_tab <- parameters::model_parameters(rum.gam)
d <- t_to_d(param_tab$t[1:2], param_tab$df_error[1:2])
interpret_cohens_d(d[1])

gam.check(rum.gam)


par(mfrow = c(3,2))
plot_smooth(rum.gam, view = "negMax", ylab = "Rumination", xlab = "Event Unpleasantness")
plot_smooth(rum.gam, view = "posMax", ylab = "", xlab = "Event Pleasantness")
plot_smooth(rum.gam, view = "distracted", ylab = "Rumination", xlab = "Distracted")
plot_smooth(rum.gam, view = "sleepQuality", ylab = "", xlab = "Sleep Quality")
plot_smooth(rum.gam, view = "companyPleasant", ylab = "Rumination", xlab = "Company Pleasantness")


#############################################################################################################

### NA

na.gam1 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                  s(ruminating, by = group) + s(sumPA, by = group) +
                  s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                  s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = sc_baseDat, method = "ML")

# removing interaction group x stickiness
na.gam2 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA, by = group) +
                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam1, na.gam2) # significant --> na.gam2 better

# removing interaction group x sumPA
na.gam3 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA) +
                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam2, na.gam3) # significant --> na.gam2 better


# removing interaction group x posMax
na.gam4 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA, by = group) +
                 s(negMax, by = group) + s(posMax) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam4, na.gam2) # rum.gam4 better


# removing interaction group x negMax
na.gam5 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam4, na.gam5) #rum.gam5 better


# removing interaction group x sumPA
na.gam6 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA) +
                 s(negMax) + s(posMax) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam6, na.gam5) #significant --> na.gam5 better


# removing interaction group x ruminating
na.gam7 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam7, na.gam5) #na.gam5 better


# removing companyPleasant
na.gam8 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam8, na.gam5) #na.gam5 better


# removing distracted
na.gam9 <- bam(sumNA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness) +
                 s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(na.gam9, na.gam5) #na.gam5 better


#refitting with reml
na.gam <- gam(sumNA ~ group + s(phaseBeepNum, by = group) +
                s(ruminating, by = group) + s(sumPA, by = group) +
                s(negMax) + s(posMax) + s(stickiness) +
                s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                s(phaseBeepNum, by = subjB, bs="fs", m=1),
              data = sc_baseDat)

na.summary <- summary(na.gam)

save(na.gam, na.summary, file = "models_na/na_final_contemp.rda")
load("models_na/na_final_contemp.rda")


na.gam_unscaled <- gam(sumNA ~ group + s(phaseBeepNum, by = group) +
                s(ruminating, by = group) + s(sumPA, by = group) +
                s(negMax) + s(posMax) + s(stickiness) +
                s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                s(phaseBeepNum, by = subjB, bs="fs", m=1),
              data = baseDat)

na.sum.unscaled <- summary(na.gam_unscaled)

save(na.gam_unscaled, na.sum.unscaled, file = "models_na/na_unscaled_contemp.rda")
load("models_na/na_unscaled_contemp.rda")

ar <- acf_resid(na.gam)

report_stats(na.gam)

par(mfrow = c(3,3))
plot_smooth(na.gam, view = "negMax", ylab = "Rumination", xlab = "Event Unpleasantness")
plot_smooth(na.gam, view = "posMax", ylab = "", xlab = "Event Pleasantness")
plot_smooth(na.gam, view = "stickiness", ylab = "", xlab = "Stickiness")
plot_smooth(na.gam, view = "listless", ylab = "Rumination", xlab = "Listless")
plot_smooth(na.gam, view = "distracted", ylab = "", xlab = "Distracted")
plot_smooth(na.gam, view = "sleepQuality", ylab = "", xlab = "Sleep Quality")
plot_smooth(na.gam, view = "companyPleasant", ylab = "Rumination", xlab = "Company Pleasantness")



par(mfrow = c(2,2))
plot_smooth(na.gam, plot_all = "group", view = "phaseBeepNum", legend = FALSE,
            main = "", ylab = "Negative Affect", xlab = "Assessment Number")
plot_smooth(na.gam, plot_all = "group", view = "ruminating", legend = FALSE,
            main = "", ylab = "", xlab = "Rumination")
plot_smooth(na.gam, plot_all = "group", view = "sumPA",
            main = "", ylab = "Negative Affect", xlab = "Positive Affect")



plot_diff(na.gam, view="phaseBeepNum", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "Negative Affect", xlab = "Assessment Number")
plot_diff(na.gam, view="ruminating", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "", xlab = "Rumination")
plot_diff(na.gam, view="sumPA", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "Negative Affect", xlab = "Positive Affect")

#plot_modelfit(rum.gam, view="phaseBeepNum", event = "subjB")

param_tab <- parameters::model_parameters(na.gam)
d <- t_to_d(param_tab$t[1:2], param_tab$df_error[1:2])
interpret_cohens_d(d[1])

gam.check(na.gam)

#############################################################################################################

### PA

pa.gam1 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumNA, by = group) +
                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness, by = group) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

# removing interaction group x stickiness
pa.gam2 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumNA, by = group) +
                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(pa.gam1, pa.gam2) #not significant --> pa.gam2 is better


# removing interaction group x posMax
pa.gam3 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumNA, by = group) +
                 s(negMax, by = group) + s(posMax) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(pa.gam3, pa.gam2) #significant --> pa.gam2 is better


# removing interaction group x negMax
pa.gam4 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumNA, by = group) +
                 s(negMax) + s(posMax, by = group) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(pa.gam4, pa.gam2) # significant --> pa.gam2 is better


# removing interaction group x sumNA
pa.gam5 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumNA) +
                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(pa.gam5, pa.gam2) #not significant --> pa.gam5 is better


# removing interaction group x ruminating
pa.gam6 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating) + s(sumNA) +
                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(pa.gam5, pa.gam6) #significant --> pa.gam5 is better


# removing stickiness
pa.gam7 <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                 s(ruminating, by = group) + s(sumNA) +
                 s(negMax, by = group) + s(posMax, by = group) +
                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat, method = "ML")

compareML(pa.gam5, pa.gam7) #not significant --> pa.gam5 is better

# removing interaction group x ruminating
pa.gam <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                s(ruminating, by = group) + s(sumNA) +
                s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                s(phaseBeepNum, by = subjB, bs="fs", m=1),
              data = sc_baseDat)


pa.summary <- summary(pa.gam)

save(pa.gam, pa.summary, file = "models_pa/pa_final_contemp.rda")
load("models_pa/pa_final_contemp.rda")


pa.gam_unscaled <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
                s(ruminating, by = group) + s(sumNA) +
                s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
                s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
                s(phaseBeepNum, by = subjB, bs="fs", m=1),
              data = baseDat)

pa.sum.unscaled <- summary(pa.gam_unscaled)

save(pa.gam_unscaled, pa.sum.unscaled, file = "models_pa/pa_unscaled_contemp.rda")
load("models_pa/pa_unscaled_contemp.rda")

ar <- acf_resid(pa.gam)

report_stats(pa.gam)

par(mfrow = c(3,2))
plot_smooth(pa.gam, view = "sumNA", legend_plot_all = FALSE,
            main = "", ylab = "Positive Affect", xlab = "Negative Affect")
plot_smooth(pa.gam, view = "posMax",
            main = "", ylab = "", xlab = "Event Pleasantness")
plot_smooth(pa.gam, view = "listless",
            main = "", ylab = "Positive Affect", xlab = "Listless")
plot_smooth(pa.gam, view = "distracted",
            main = "", ylab = "", xlab = "Distracted")
plot_smooth(pa.gam, view = "sleepQuality",
            main = "Positive Affect", ylab = "", xlab = "Sleep Quality")
plot_smooth(pa.gam, view = "companyPleasant",
            main = "", ylab = "", xlab = "Company Pleasantness")


par(mfrow = c(2,2))
plot_smooth(pa.gam, plot_all = "group", view = "phaseBeepNum", legend = FALSE,
            main = "", ylab = "Positive Affect", xlab = "Assessment Number")

plot_smooth(pa.gam, plot_all = "group", view = "ruminating", legend = FALSE,
            main = "", ylab = "", xlab = "Rumination")

plot_smooth(pa.gam, plot_all = "group", view = "negMax", legend = FALSE,
            main = "", ylab = "Positive Affect", xlab = "Event Unpleasantness")

plot_smooth(pa.gam, plot_all = "group", view = "posMax",
            main = "", ylab = "", xlab = "Event Pleasantness")


plot_diff(pa.gam, view="phaseBeepNum", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "Positive Affect", xlab = "Assessment Number")

plot_diff(pa.gam, view="ruminating", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "", xlab = "Rumination")

plot_diff(pa.gam, view="negMax", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "Positive Affect", xlab = "Event Unpleasantness")

plot_diff(pa.gam, view="posMax", comp = list(group = c("controls", "remitted")),
          main = "", ylab = "", xlab = "Event Pleasantness")


#plot_modelfit(rum.gam, view="phaseBeepNum", event = "subjB")

param_tab <- parameters::model_parameters(pa.gam)
d <- t_to_d(param_tab$t[1:2], param_tab$df_error[1:2])
interpret_cohens_d(d[1])

gam.check(pa.gam)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++ Peri-intervention models ++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
peri_dat <- sc_data[which(sc_data$phase=="peri"),]
peri_dat_unscaled <- data[which(data$phase=="peri"),]

### rumination

rum.gam <- gam(ruminating ~ group + s(phaseBeepNum, by = group) +
                 s(sumNA, by = group) + s(sumPA, by = group) +
                 s(negMax) + s(posMax) + s(stickiness, by = group) +
                 s(distracted) + s(sleepQuality) + s(companyPleasant) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = sc_baseDat)

#refitting the baseline model for peri phase (with change scores)
rum.peri <- gam(ruminating_gam ~ s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
              s(sumNA_gam, by = interaction(group, intervention)) + s(sumPA_gam, by = interaction(group, intervention)) +
              s(negMax_gam) + s(posMax_gam) +
              s(stickiness_gam, by = interaction(group, intervention)) +
              s(distracted_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
              s(phaseBeepNum, by = subjB, bs="fs", m=1),
            data = peri_dat)

summary_rum.peri <- summary(rum.peri)
save(rum.peri, summary_rum.peri, file="models_rumination/rum_peri.rda")



rum.peri.test <- gam(ruminating_gam ~ s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                  s(sumNA_gam) + s(sumPA_gam) +
                  s(negMax_gam) + s(posMax_gam) +
                  s(stickiness_gam) +
                  s(distracted_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = peri_dat)

summary_rum.peri.test <- summary(rum.peri.test)



rum.peri_unscaled <- gam(ruminating_gam ~ s(phaseBeepNum, by = interaction(group, intervention)) + group * intervention +
                  s(sumNA_gam, by = interaction(group, intervention)) + s(sumPA_gam, by = interaction(group, intervention)) +
                  s(negMax_gam) + s(posMax_gam) +
                  s(stickiness_gam, by = interaction(group, intervention)) +
                  s(distracted_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = peri_dat_unscaled)

rum.peri.sum.unscaled <- summary(rum.peri_unscaled)

save(rum.peri_unscaled, rum.peri.sum.unscaled, file = "models_rumination/rum_unscaled_peri.rda")

load("models_rumination/rum_unscaled_peri.rda")

report_stats(rum.peri)

model1.acf <- acf_resid(rum.peri)

par(mfrow = c(2,2))
# plot_smooth(rum.peri, plot_all = c("group"), view = "phaseBeepNum", legend = F, ylab = "Rumination", ylim = c(-1.5,1.5))
# plot_smooth(rum.peri, plot_all = c("group"), view = "stickiness_gam", legend = FALSE, ylab = "", ylim = c(-3,3))
# plot_smooth(rum.peri, plot_all = c("group"), view = "sumNA_gam", legend = FALSE, ylab = "Rumination", ylim = c(-3,3))
# plot_smooth(rum.peri, plot_all = c("group"), view = "sumPA_gam", ylab = "", ylim = c(-3,3))

plot_smooth(rum.peri.test, plot_all = c("intervention"), cond = list(group = "remitted"), view = "phaseBeepNum",
            ylab = "Rumination", legend = F)
plot_smooth(rum.peri.test, plot_all = c("intervention"), cond = list(group = "remitted"), view = "stickiness_gam",
            ylab = "", legend = FALSE)
plot_smooth(rum.peri.test, plot_all = c("intervention"), cond = list(group = "remitted"), view = "sumNA_gam",
            ylab = "Rumination", legend = FALSE)
plot_smooth(rum.peri.test, plot_all = c("intervention"), cond = list(group = "remitted"), view = "sumPA_gam",
            ylab = "")

plot_smooth(rum.peri, plot_all = c("intervention"), cond = list(group = "controls"), view = "phaseBeepNum",
            ylab = "Rumination", legend = F)
plot_smooth(rum.peri, plot_all = c("intervention"), cond = list(group = "controls"),
            ylab = "", view = "stickiness_gam", legend = FALSE)
plot_smooth(rum.peri, plot_all = c("intervention"), cond = list(group = "controls"),
            ylab = "Rumination", view = "sumNA_gam", legend = FALSE)
plot_smooth(rum.peri, plot_all = c("intervention"), cond = list(group = "controls"),
            ylab = "", view = "sumPA_gam")


par(mfrow = c(2,2))
plot_diff(rum.peri, view="phaseBeepNum", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Rumination", main = "", xlab="Assessment Number")
plot_diff(rum.peri, view="stickiness_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Stickiness")
plot_diff(rum.peri, view="sumNA_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Rumination", main = "", xlab = "Negative Affect")
plot_diff(rum.peri, view="sumPA_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Positive Affect")

plot_diff(rum.peri, view="phaseBeepNum", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Rumination", main = "", xlab = "Assessment Number")
plot_diff(rum.peri, view="stickiness_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Stickiness")
plot_diff(rum.peri, view="sumNA_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Rumination", main = "", xlab = "Negative Affect")
plot_diff(rum.peri, view="sumPA_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Positive Affect")


par(mfrow=c(1,1))
#plot the model based predicted differences between interventions (summed effects)
plot_parametric(rum.peri, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(rum.peri, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")),
                main ="", xlab = "Rumination")

#plot_modelfit(rum.gam, view="phaseBeepNum", event = "subjB")

#gam.check(rum.peri)


par(mfrow = c(3,2))
plot_smooth(rum.gam, view = "negMax", ylab = "Rumination", xlab = "Event Unpleasantness")
plot_smooth(rum.gam, view = "posMax", ylab = "", xlab = "Event Pleasantness")
plot_smooth(rum.gam, view = "distracted", ylab = "Rumination", xlab = "Distracted")
plot_smooth(rum.gam, view = "sleepQuality", ylab = "", xlab = "Sleep Quality")
plot_smooth(rum.gam, view = "companyPleasant", ylab = "Rumination", xlab = "Company Pleasantness")


param_tab <- parameters::model_parameters(rum.peri)
d <- t_to_d(param_tab$t[2:4], param_tab$df_error[2:4])
interpret_cohens_d(d[1])

######################################################################################################################

### NA

# na.gam <- gam(sumNA ~ group + s(phaseBeepNum, by = group) +
#                 s(ruminating, by = group) + s(sumPA, by = group) +
#                 s(negMax) + s(posMax) + s(stickiness) +
#                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
#                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
#               data = sc_baseDat)

#refitting the baseline model for peri phase (with change scores)
na.peri <- gam(sumNA_gam ~ group * intervention + s(phaseBeepNum, by = interaction(group, intervention)) +
                  s(ruminating_gam, by = interaction(group, intervention)) + s(sumPA_gam, by = interaction(group, intervention)) +
                  s(negMax_gam) + s(posMax_gam) +
                  s(stickiness_gam) +
                  s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
                  s(phaseBeepNum, by = subjB, bs="fs", m=1),
                data = peri_dat)

summary_na.peri <- summary(na.peri)

save(na.peri, summary_na.peri, file="models_na/na_peri.rda")

na.peri_unscaled <- gam(sumNA_gam ~ group * intervention + s(phaseBeepNum, by = interaction(group, intervention)) +
                 s(ruminating_gam, by = interaction(group, intervention)) + s(sumPA_gam, by = interaction(group, intervention)) +
                 s(negMax_gam) + s(posMax_gam) +
                 s(stickiness_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = peri_dat_unscaled)

na.peri.sum.unscaled <- summary(na.peri_unscaled)

save(na.peri_unscaled, na.peri.sum.unscaled, file = "models_na/na_unscaled_peri.rda")
load("models_na/na_unscaled_peri.rda")

param_tab_na <- parameters::model_parameters(na.peri)
d <- t_to_d(param_tab_na$t[2:4], param_tab_na$df_error[2:4])
interpret_cohens_d(d[1])



par(mfrow = c(2,2))
plot_diff(na.peri, view="phaseBeepNum", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Negative Affect", main = "", xlab = "Assessment Number")
plot_diff(na.peri, view="ruminating_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Rumination", ylim = c(-5,5))
plot_diff(na.peri, view="sumPA_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Negative Affect", main = "", xlab = "Positive Affect")


par(mfrow = c(2,2))
plot_diff(na.peri, view="phaseBeepNum", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Negative Affect", main = "", xlab = "Assessment Number")
plot_diff(na.peri, view="ruminating_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Rumination", ylim = c(-5,5))
plot_diff(na.peri, view="sumPA_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Negative Affect", main = "", xlab = "Positive Affect")


par(mfrow=c(1,1))
#plot the model based predicted differences between interventions (summed effects)
plot_parametric(na.peri, pred=list(intervention=c("mindfulness", "fantasizing")))
#and between groups
plot_parametric(na.peri, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")),
                main ="", xlab = "Negative Affect")

par(mfrow = c(3,3))
plot_smooth(na.peri, view = "negMax_gam", legend_plot_all = FALSE, ylab = "Negative Affect", xlab = "Event Unpleasantness")
plot_smooth(na.peri, view = "posMax_gam", ylab = "", xlab = "Event Pleasantness")
plot_smooth(na.peri, view = "stickiness_gam", ylab = "", xlab = "Stickiness")
plot_smooth(na.peri, view = "listless_gam", ylab = "Negative Affect", xlab = "Listless")
plot_smooth(na.peri, view = "distracted_gam", ylab = "", xlab = "Distracted")
plot_smooth(na.peri, view = "sleepQuality_gam", ylab = "", xlab = "Sleep Quality")
plot_smooth(na.peri, view = "companyPleasant_gam", ylab = "Negative Affect", xlab = "Company Pleasantness")

######################################################################################################################

### PA

# pa.gam <- gam(sumPA ~ group + s(phaseBeepNum, by = group) +
#                 s(ruminating, by = group) + s(sumNA) +
#                 s(negMax, by = group) + s(posMax, by = group) + s(stickiness) +
#                 s(distracted) + s(listless) + s(sleepQuality) + s(companyPleasant) +
#                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
#               data = sc_baseDat)


#refitting the baseline model for peri phase (with change scores)
pa.peri <- gam(sumPA_gam ~ group * intervention + s(phaseBeepNum, by = interaction(group, intervention)) +
                 s(ruminating_gam, by = interaction(group, intervention)) + s(sumNA_gam) +
                 s(negMax_gam, by = interaction(group, intervention)) + s(posMax_gam, by = interaction(group, intervention)) +
                 s(stickiness_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = peri_dat)


summary_pa.peri <- summary(pa.peri)

pa.peri_unscaled <- gam(sumPA_gam ~ group * intervention + s(phaseBeepNum, by = interaction(group, intervention)) +
                 s(ruminating_gam, by = interaction(group, intervention)) + s(sumNA_gam) +
                 s(negMax_gam, by = interaction(group, intervention)) + s(posMax_gam, by = interaction(group, intervention)) +
                 s(stickiness_gam) +
                 s(distracted_gam) + s(listless_gam) + s(sleepQuality_gam) + s(companyPleasant_gam) +
                 s(phaseBeepNum, by = subjB, bs="fs", m=1),
               data = peri_dat_unscaled)


pa.peri.sum.unscaled <- summary(pa.peri_unscaled)


save(pa.peri_unscaled, pa.peri.sum.unscaled, file = "models_pa/pa_unscaled_peri.rda")

save(pa.peri, summary_pa.peri, file = "models_pa/pa_unscaled_peri.rda")
load("models_pa/pa_unscaled_peri.rda")

param_tab_pa <- parameters::model_parameters(pa.peri)
d <- t_to_d(param_tab_pa$t[2:4], param_tab_pa$df_error[2:4])
interpret_cohens_d(d[1])


par(mfrow = c(2,2))
#remitted
plot_diff(pa.peri, view="phaseBeepNum", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Positive Affect", main = "", xlab = "Assessment Number")

plot_diff(pa.peri, view="ruminating_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Rumination")

plot_diff(pa.peri, view="negMax_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Positive Affect", main = "", xlab = "Event Unpleasantness")

plot_diff(pa.peri, view="posMax_gam", comp = list(group = c("remitted", "remitted"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Event Pleasantness")


#controls
plot_diff(pa.peri, view="phaseBeepNum", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Positive Affect", main = "", xlab = "Assessment Number")

plot_diff(pa.peri, view="ruminating_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Rumination")

plot_diff(pa.peri, view="negMax_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "Positive Affect", main = "", xlab = "Event Unpleasantness")

plot_diff(pa.peri, view="posMax_gam", comp = list(group = c("controls", "controls"), intervention = c("fantasizing", "mindfulness")),
          ylab = "", main = "", xlab = "Event Pleasantness")


#summed effects
plot_parametric(pa.peri, pred=list(intervention=c("mindfulness", "fantasizing"), group=c("controls", "remitted")),
                main ="", xlab = "Positive Affect")


#single smooths
par(mfrow = c(3,2))
plot_smooth(pa.peri, view = "sumNA_gam", ylab = "Positive Affect", xlab = "Negative Affect")
plot_smooth(pa.peri, view = "stickiness_gam", ylab = "", xlab = "Stickiness")
plot_smooth(pa.peri, view = "listless_gam", ylab = "Positive Affect", xlab = "Listless")
plot_smooth(pa.peri, view = "distracted_gam", ylab = "", xlab = "Distracted")
plot_smooth(pa.peri, view = "sleepQuality_gam", ylab = "Positive Affect", xlab = "Sleep Quality")
plot_smooth(pa.peri, view = "companyPleasant_gam", ylab = "", xlab = "Company Pleasantness")


############################################## Plotting main results ##############################################################
### RQ1

rum_vals <- c(rum.sum.unscaled$p.coeff[1], rum.sum.unscaled$p.coeff[2]+rum.sum.unscaled$p.coeff[1])
rum_sds <- c(rum.sum.unscaled$se[1], rum.sum.unscaled$se[3])

na_vals <- c(na.sum.unscaled$p.coeff[1], na.sum.unscaled$p.coeff[2]+na.sum.unscaled$p.coeff[1])
na_sds <- c(na.sum.unscaled$se[1], na.sum.unscaled$se[2])

pa_vals <- c(pa.sum.unscaled$p.coeff[1], pa.sum.unscaled$p.coeff[2]+pa.sum.unscaled$p.coeff[1])
pa_sds <- c(pa.sum.unscaled$se[1], pa.sum.unscaled$se[2])

rq1_res <- data.frame(node = c(rep("Rumination",2), rep("Negative Affect",2), rep("Positive Affect",2)), group = c("remitted", "control"),
                      value = c(rum_vals, na_vals, pa_vals),
                      se = c(rum_sds, na_sds, pa_sds))

# Default bar plot
p<- ggplot(rq1_res, aes(x=factor(node, levels=c("Positive Affect", "Negative Affect", "Rumination")), y=value, fill=group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + ylim(0,75) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
                position=position_dodge(.9))  + coord_flip()

# Finished bar plot
p+labs(x="", y = "")+
  theme_economist_white() +  
  scale_fill_manual(values=c('#0099FF','#FF3333'))

## RQ2

rum_vals <- c(rum.peri.sum.unscaled$p.coeff[1], rum.peri.sum.unscaled$p.coeff[3]+rum.peri.sum.unscaled$p.coeff[1])
rum_sds <- c(rum.peri.sum.unscaled$se[1], rum.peri.sum.unscaled$se[3])

na_vals <- c(na.peri.sum.unscaled$p.coeff[1], na.peri.sum.unscaled$p.coeff[3]+na.peri.sum.unscaled$p.coeff[1])
na_sds <- c(na.peri.sum.unscaled$se[1], na.peri.sum.unscaled$se[3])

pa_vals <- c(pa.peri.sum.unscaled$p.coeff[1], pa.peri.sum.unscaled$p.coeff[3]+pa.peri.sum.unscaled$p.coeff[1])
pa_sds <- c(pa.peri.sum.unscaled$se[1], pa.peri.sum.unscaled$se[3])

rq2_res <- data.frame(node = c(rep("Rumination",2), rep("Negative Affect",2), rep("Positive Affect",2)),
                      intervention = c("mindfulness", "fantasizing"),
                      value = c(rum_vals, na_vals, pa_vals),
                      se = c(rum_sds, na_sds, pa_sds))

# Default bar plot
p<- ggplot(rq2_res, aes(x=factor(node, levels=c("Positive Affect", "Negative Affect", "Rumination")),
                        y=value, fill=factor(intervention, levels = c("mindfulness", "fantasizing")))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + ylim(-7.5,7.5) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
                position=position_dodge(.9)) + coord_flip()

# Finished bar plot
p+labs(x="", y = "")+
  theme_economist_white() +  
  scale_fill_manual(values=c('#1F5B79', '#F29731'))


# split by group


rum_vals <- c(rum.peri.sum.unscaled$p.coeff[1], #rem + mind
              rum.peri.sum.unscaled$p.coeff[1] + rum.peri.sum.unscaled$p.coeff[3], #rem + fant
              rum.peri.sum.unscaled$p.coeff[1] + rum.peri.sum.unscaled$p.coeff[2], #cont + mind
              rum.peri.sum.unscaled$p.coeff[1] + rum.peri.sum.unscaled$p.coeff[2] + rum.peri.sum.unscaled$p.coeff[3] + rum.peri.sum.unscaled$p.coeff[4]) #cont + fant
rum_sds <- c(rum.peri.sum.unscaled$se[1],
             rum.peri.sum.unscaled$se[3],
             rum.peri.sum.unscaled$se[2],
             rum.peri.sum.unscaled$se[2] + rum.peri.sum.unscaled$se[3])# + rum.peri.sum.unscaled$se[4])


na_vals <- c(na.peri.sum.unscaled$p.coeff[1], #rem + mind
             na.peri.sum.unscaled$p.coeff[1] + na.peri.sum.unscaled$p.coeff[3], #rem + fant
             na.peri.sum.unscaled$p.coeff[1] + na.peri.sum.unscaled$p.coeff[2], #cont + mind
             na.peri.sum.unscaled$p.coeff[1] + na.peri.sum.unscaled$p.coeff[2] + na.peri.sum.unscaled$p.coeff[3] + na.peri.sum.unscaled$p.coeff[4]) #cont + fant
na_sds <- c(na.peri.sum.unscaled$se[1],
            na.peri.sum.unscaled$se[3],
            na.peri.sum.unscaled$se[2],
            na.peri.sum.unscaled$se[2] + na.peri.sum.unscaled$se[3])# + na.peri.sum.unscaled$se[4])

pa_vals <- c(pa.peri.sum.unscaled$p.coeff[1], #rem + mind
             pa.peri.sum.unscaled$p.coeff[1] + pa.peri.sum.unscaled$p.coeff[3], #rem + fant
             pa.peri.sum.unscaled$p.coeff[1] + pa.peri.sum.unscaled$p.coeff[2], #cont + mind
             pa.peri.sum.unscaled$p.coeff[1] + pa.peri.sum.unscaled$p.coeff[2] + pa.peri.sum.unscaled$p.coeff[3] + pa.peri.sum.unscaled$p.coeff[4]) #cont + fant
pa_sds <- c(pa.peri.sum.unscaled$se[1],
            pa.peri.sum.unscaled$se[3],
            pa.peri.sum.unscaled$se[2],
            pa.peri.sum.unscaled$se[2] + pa.peri.sum.unscaled$se[3])# + pa.peri.sum.unscaled$se[4])

rq2_res <- data.frame(node = c(rep("Rumination",4), rep("Negative Affect",4), rep("Positive Affect",4)),
                      group = c(rep("remitted",2), rep("controls",2)),
                      intervention = c("mindfulness", "fantasizing"),
                      value = c(rum_vals, na_vals, pa_vals),
                      se = c(rum_sds, na_sds, pa_sds))

# Default bar plot
p<- ggplot(rq2_res, aes(x=factor(node, levels=c("Positive Affect", "Negative Affect", "Rumination")),
                        y=value,
                        fill=interaction(intervention, group))) + 
  # geom_col(aes(fill= factor(intervention))) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) + ylim(-7,7) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
                position=position_dodge(.9)) + coord_flip()

# Finished bar plot
p+labs(x="", y = "")+
  theme_economist_white() +  
  scale_fill_manual(values=c('#0099FF','#FF3333', '#0099FF','#FF3333'))

