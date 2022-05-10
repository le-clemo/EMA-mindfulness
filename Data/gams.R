rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

library(ggplot2)
library(data.table)
library(lme4)
library(mgcv)
library(itsadug)
library(dplyr)
library(plyr)
library(lmerTest) #to more quickly be able to see significance
library(plotfunctions)

R.version.string
packageVersion("mgcv")
packageVersion("itsadug")

#load preprocessed ESM data
data <- read.csv('ESM/mindcog_v202204/preprocessed_data.csv') 

#set factors
data$group <- factor(data$group, levels = c("controls", "remitted"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))
data$grInt <- as.factor(paste(data$group, data$intervention, sep = "."))
data$blockPhase <- as.factor(paste(data$phase, data$block, sep = "."))

#create a scaled version of data
pvars <- c( "ruminating_lag1", "sumNA", "stressed", "listless", "stickiness",# Scaling numeric parameters
            "sumPA")

sc_data <- copy(data)
sc_data[pvars] <- lapply(data[pvars],scale)

sc_data$subjB <- interaction(sc_data$subject, sc_data$block, drop = TRUE)

################################ blocks combined #############################
t1 <- gam(ruminating_lag1 ~ blockAssessmentDay * grInt * phase * sumNA * sumPA +
            s(blockAssessmentDay, by = subjB, bs = 'fs', m = 1) +
            s(blockAssessmentDay, by = grInt), data = sc_data)
#we need to lower k to 7

#t1c <- gam(Score ~ Group + s(Month, by = Group, k = 7) + s(Month, Subject, bs = 'fs', m = 1), data = dat) #is centering necessary?

summary(t1)
#summary(t1c) #minimally changes estimates. I'll keep Month centered.
#report_stats(t1)






















