rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/SART")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

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

















