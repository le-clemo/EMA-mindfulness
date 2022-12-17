rm(list = ls()) #clean all up


# load permutation test function (placed in .../EMA-mindfulness/Data)
source("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/NetworkPermutationTest.R")
setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202207")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202207")
library(psychonetrics)
library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(qgraph)          # Network estimation & inference       # Visualization
library(bootnet)         # Network stability        # Simplify R code
library(tidyr)           # Simplify R code
library(mgm)
library(graphicalVAR)
library(mlVAR)
library(reshape)
library(viridis)
library(lm.beta)
library(huge)
library(languageR)
library(Hmisc)
library(networktools)
library(NetworkComparisonTest)
library(psychNet)
library(parallel)
library(xtable)

packageVersion("mlVAR") #0.5
packageVersion("qgraph") #1.9.1

#read in data
data <- read.csv('preprocessed_data.csv') 

#Pick response rate cut-off value
cutOff <- 0.5

responses_block <- ddply(data, .(subject), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))

meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.6%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subject)) #20
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subject)) #26
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subject)) #33

#removing participants with a response rate lower than 60%
pp <- unique(responses_block[which(responses_block$responseRate >= cutOff),]$subject)
data <- data[which(data$subject %in% pp),]
data <- data[which(data$dayBeepNum < 11),] #three subjects had extra beeps on a day. Since this messes with the detrending code I just removed
#the extra beeps (<15 entries)

length(unique(data[which((data$group=="controls")),]$subject)) #21
length(unique(data[which((data$group=="remitted")),]$subject)) #12


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################## Detrend data #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Following Borsboom et al, 2021
# some of the variables have trends (i.e. linear or other changes over time)
# we remove these trends before estimation network structures
# we work with most variables here that we collected
# afterwards, we select those we want to estimate networks on

# Alpha to detrend:
alpha <- 0.05

# Variables to investigate:
vars <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'meanNA', 'meanPA', 'wakeful', 'down', 'satisfied',
          'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
          'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
          'negMax', 'negIntensity', "sleepQuality")



# Data frame with empty values for fitted effects (all):
fitted_all <- expand.grid(
  beep = seq(min(data$dayBeepNum),max(data$dayBeepNum)),
  day = seq(min(data$blockAssessmentDay),max(data$blockAssessmentDay))
)

# Data frame with empty values for day trends:
fitted_day <- data.frame(
  day = seq(min(data$blockAssessmentDay),max(data$blockAssessmentDay))
)

# Data frame with empty values for beeps:
fitted_beep <- data.frame(
  beep = seq(min(data$dayBeepNum),max(data$dayBeepNum))
)

# Data frame to store p-values:
p_values <- data.frame(
  var = c("day", "beep")
)

# Also empty data frame list for test statistics:
testStatistics <- list()
coefficients <- list()
stdcoefficients <- list()


# Make the beep variable factor in dataset:
data$beepFactor <- factor(data$dayBeepNum, levels = 1:10)#, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))
fitted_all$beepFactor <- factor(fitted_all$beep, levels = 1:10)#, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))
fitted_beep$beepFactor <- factor(fitted_beep$beep, levels = 1:10)#, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))

# Make day variable for dates:
data$date <- as.Date("2020-03-15") + data$blockAssessmentDay
fitted_all$date <- as.Date("2020-03-15") + fitted_all$day
fitted_day$date <- as.Date("2020-03-15") + fitted_day$day

#find midpoints
midpoints <- data.frame(beep = 1:10, midpoint = NA)
for(i in 1:max(data$dayBeepNum)){
  t <- as.POSIXct(strftime(data[which(data$dayBeepNum==i),]$mindcog_db_open_from, format="%H:%M:%S"), format="%H:%M:%S")
  midpoints$midpoint[i] <- strftime(mean(t), format="%H:%M:%S")
}

# Add the (very approximate) midpoints as time variable:
data$midTime <- as.character(factor(data$dayBeepNum, levels = 1:10, labels = c("8:30","10:00","11:30","13:00","14:30", "16:00",
                                                                               "17:00","18:30","20:00","21:30")))
data$midTime <- as.POSIXct(paste(data$date,data$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")

fitted_all$midTime <- as.character(factor(fitted_all$beep, levels = 1:10, labels = c("8:30","10:00","11:30","13:00","14:30", "16:00",
                                                                                     "17:00","18:30","20:00","21:30")))
fitted_all$midTime <- as.POSIXct(paste(fitted_all$date,fitted_all$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")

# Data frame to store detrended data:
data_detrended <- copy(data)

# Fix curves:
for (v in seq_along(vars)){
  formula <- as.formula(paste0(vars[v], " ~ 1 + blockAssessmentDay + factor(dayBeepNum)"))
  lmRes <- lm(formula, data = data)
  
  # Fixed effects:
  fixed <- coef(lmRes)
  
  # make zero if not significant at alpha:
  p_values[[vars[v]]] <- anova(lmRes)[["Pr(>F)"]][1:2]
  if (p_values[[vars[v]]][1] > alpha){
    fixed[2] <- 0
  }
  if (p_values[[vars[v]]][2] > alpha){
    fixed[3:5] <- 0
  }
  
  # Add to DFs:
  fitted_all[,vars[v]] <- fixed[1] + fixed[2] * fitted_all[["day"]]  +  fixed[3] * (fitted_all[["beep"]] == 1)  + 
    fixed[4] * (fitted_all[["beep"]] == 2) + fixed[5] *  (fitted_all[["beep"]] == 3)
  
  fitted_day[,vars[v]] <- fixed[1] + fixed[2] * fitted_day[["day"]]
  
  fitted_beep[,vars[v]] <- fixed[1] + fixed[2] * median(fitted_day[["day"]]) +  fixed[3] * (fitted_beep[["beep"]] == 1)  + 
    fixed[4] * (fitted_beep[["beep"]] == 2) + fixed[5] *  (fitted_beep[["beep"]] == 3)
  
  # Detrend data:
  data_detrended[,vars[v]] <- data[,vars[v]] - (fixed[1] + fixed[2] * data[["blockAssessmentDay"]]  +  fixed[3] * (data[["dayBeepNum"]] == 1)  + 
                                                  fixed[4] * (data[["dayBeepNum"]] == 2) + fixed[5] *  (data[["dayBeepNum"]] == 3))
  
  ids <- rownames(anova(lmRes))
  testStatistics[[v]] <- cbind(data.frame(var = vars[v], effect = ids), anova(lmRes))
  
  coefficients[[v]] <- data.frame(
    var = vars[v],
    type = names(coef(lmRes)),
    coef = coef(lmRes),
    std = coef(lm.beta(lmRes))
  )
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################## Scaling data #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#creating variables minus baseline means per subject

met.vars <- c('ruminating', 'stickiness', 'sumNA', 'meanNA', 'meanPA',  'down', 'irritated', 'restless', 'anxious',
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
  data_detrended[[new_var]] <- NA
  
  gam_var <- paste(v, "_gam", sep = "")
  data_detrended[[gam_var]] <- NA
  i = i+1
  scale.vars[[i]] <- v
  i = i+1
  scale.vars[[i]] <- new_var
  i = i+1
  scale.vars[[i]] <- gam_var
  
  for(id in unique(data_detrended$subject)){
    for(b in 1:2){
      pre_rows <- which((data_detrended$subject == id) & (data_detrended$phase=="pre") & (data_detrended$block==b))
      peri_rows <- which((data_detrended$subject == id) & (data_detrended$phase=="peri") & (data_detrended$block==b))
      s_rows <- which((data_detrended$subject == id) & (data_detrended$block==b))
      baselineMean <- mean(data_detrended[[v]][pre_rows], na.rm=TRUE)
      
      if(is.na(baselineMean)){
        baselineMean <- 0
      }
      
      data_detrended[[new_var]][s_rows] <- round(data_detrended[[v]][s_rows] - baselineMean, 2)
      
      data_detrended[[gam_var]][pre_rows] <- NA
      data_detrended[[gam_var]][peri_rows] <- round(data_detrended[[v]][peri_rows] - baselineMean, 2)
    }
  }  
}

# View(subset(data_detrended[which(data_detrended$subject=="s37"),],
#             select = c("subject", "phase", "block", "ruminating", "ruminating_gam", "ruminating_diff", "beepNum")))

#creating a scaled version of data
sc_data <- copy(data_detrended)
sc_data[scale.vars] <- scale(sc_data[scale.vars])


nodeVars <- c('ruminating', 'stickiness',
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless',
              'listless', 'distracted',
              'posMax', 'negMax', 'meanNA', 'meanPA')

#only complete cases
data_copy <- data.table::copy(sc_data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
sc_data <- data_copy[complete.cases(data_copy[nodeVars]),]


#paranormal transformation (because the model assumes normality) --> recommended by Epskamp
#huge.npn() should only be applied to data without missing values! Otherwise creates weird values!!!


data_t <- copy(sc_data)
data_t[,nodeVars] <- huge.npn(data_t[,nodeVars])

#grouping the variables --> for later use in network plotting
groups_list <- list(Rumination = c(1,2), PositiveAffect = c(3,4,5), NegativeAffect = c(6,7,8,9),
                    OtherNegative = c(10,11), 
                    Events = c(12,13))#,)#, Sleep = c(11)) , 
# Sleep=c(15))
groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53")#, "#0558ff")




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################### Permutation Testing ################################################################ 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+
# +++++++++++++++++++++++++++++++++++++ robustness tests ++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(data_t)[colnames(data_t) == 'NegativeAffect'] <- 'sumNA'
colnames(data_t)[colnames(data_t) == 'PositiveAffect'] <- 'sumPA'
colnames(data_t)[colnames(data_t) == 'EventUnpleasantness'] <- 'negMax'
colnames(data_t)[colnames(data_t) == 'EventPleasantness'] <- 'posMax'


# baseline networks
cont_pre_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre")),], nodes = nodeVars,
                       iterations = 100, filepath = "network_permutations/cont_pre_robust.rda",
                       localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))

rem_pre_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre")),], nodes = nodeVars,
                      iterations = 100, filepath = "network_permutations/rem_pre_robust.rda",
                      localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))



# +++++++++++++++++++++++++++++++++++++ Network comparison tests ++++++++++++++++++++++++++++++++++++++++++++++++++

#compare groups at baseline
dat <- data_t[which((data_t$phase=="pre")),]
compare_group_pre <- NPT(dat, nodes = nodeVars, iterations = 200, permuteBy = "group",
                          filepath = "network_permutations/compare_group_pre.rda",
                         localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################## True Networks ################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(data_t)[colnames(data_t) == 'sumNA'] <- 'NegativeAffect'
colnames(data_t)[colnames(data_t) == 'sumPA'] <- 'PositiveAffect'
colnames(data_t)[colnames(data_t) == 'negMax'] <- 'EventUnpleasantness'
colnames(data_t)[colnames(data_t) == 'posMax'] <- 'EventPleasantness'
colnames(data_t)[colnames(data_t) == 'ruminating'] <- 'Rumination'
colnames(data_t)[colnames(data_t) == 'energetic'] <- 'Energy'
colnames(data_t)[colnames(data_t) == 'wakeful'] <- 'Wakefulness'
colnames(data_t)[colnames(data_t) == 'satisfied'] <- 'Satisfaction'
colnames(data_t)[colnames(data_t) == 'down'] <- 'Sadness'
colnames(data_t)[colnames(data_t) == 'anxious'] <- 'Anxiety'
colnames(data_t)[colnames(data_t) == 'restless'] <- 'Restlessness'
colnames(data_t)[colnames(data_t) == 'irritated'] <- 'Irritation'
colnames(data_t)[colnames(data_t) == 'distracted'] <- 'Distraction'

groups_list <- list(Rumination = c(1), PositiveAffect = c(2,3,4), NegativeAffect = c(5,6,7,8),
                    Events = c(9,10), 
                    Other = c(11))#,)#, Sleep = c(11)) , 

groups_colors <- c("yellow", "#149F36", "#d60000", "#37A7F8" , "#C662F0")

nodeVars <- c('Rumination',
              'Energy', 'Wakefulness', 'Satisfaction',
              'Sadness', 'Irritation', 'Anxiety', 'Restlessness',
              'EventUnpleasantness', 'EventPleasantness',
              'Distraction')#, 'NegativeAffect', 'PositiveAffect')

cont.nets <- list(rep(NA,2))
temp.nets <- list(rep(NA,2))
bet.nets <- list(rep(NA,2))
i = 1
#creating baseline networks per group
for(g in c("controls", "remitted")){
  # Estimate network using multilevel VAR model
  print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
  print(g)
  print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
  #subData <- data_copy[which((data_copy$group==g) & (data_copy$phase=="pre")),]
  
  #subData <- sc_data[which((sc_data$group==g) & (sc_data$phase=="pre")),]
  
  subData <- data_t[which((data_t$group==g) & (data_t$phase=="pre")),]
  subData$subjB <- factor(subData$subjB)
  
  length(unique(subData$subjB))
  
  mlNet <- mlVAR(subData,
                 vars=nodeVars,
                 estimator="lmer",
                 idvar="subjB",
                 dayvar="phaseAssessmentDay",
                 beepvar="dayBeepNum",
                 lags = 1,
                 temporal = "orthogonal",
                 contemporaneous = "orthogonal",
                 nCores = detectCores()-2)
  
  
  #  # Get mlVAR networks:
  cont.nets[[i]] <- getNet(mlNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
  bet.nets[[i]]  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
  temp.nets[[i]] <- getNet(mlNet, "temporal", nonsig = "hide")
  
  i <- i + 1
  
}

L <- averageLayout(cont.nets[[1]], cont.nets[[2]], temp.nets[[1]], temp.nets[[2]])

pdf(width=12, height=4,
    file = "baseline_contemp_nets.pdf")
layout(matrix(c(1,1,1,1,1,2,2,2,2,2,2), nc=11, byrow = TRUE)) # 40% vs 60% widths

n1 <- qgraph(cont.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=F, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6.5, asize=8, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
n2 <- qgraph(cont.nets[[1]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Remitted: Temporal - Baseline")
             groups=groups_list, legend.cex=.7, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6.5, asize=8, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
dev.off()

print("Controls:")
cent_group1 <- centrality_auto(n1, weighted = T)$node.centrality
print(cent_group1)
x <- min(cent_group1$ExpectedInfluence) - 0.05
y <- max(cent_group1$ExpectedInfluence) + 0.05
print(smallworldIndex(n1)$index)
centralityPlot(n1, weighted = TRUE, labels = nodeVars,
               include = c("ExpectedInfluence")) + xlim(x,y)
print(centralityTable(n1, weighted = T, labels = nodeVars))
#global strength
print(sum(abs(cont.nets[[1]][which(upper.tri(cont.nets[[1]]))])))


print("Remitted:")
cent_group2 <- centrality_auto(n2, weighted = TRUE)$node.centrality
print(cent_group2)
x <- min(cent_group2$ExpectedInfluence) - 0.05
y <- max(cent_group2$ExpectedInfluence) + 0.05
print(smallworldIndex(n2)$index)
centralityPlot(n2, weighted = TRUE, labels = nodeVars,
               include = c("ExpectedInfluence")) + xlim(x,y)
print(centralityTable(n2, weighted = TRUE, labels = nodeVars))
#global strength
print(sum(abs(cont.nets[[2]][which(upper.tri(cont.nets[[2]]))])))

pdf(width=12, height=4,
    file = "baseline_temp_nets.pdf")
layout(matrix(c(1,1,1,1,1,2,2,2,2,2,2), nc=11, byrow = TRUE)) # 40% vs 60% widths
n3 <- qgraph(temp.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=F, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6.5, asize=6.5, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
n4 <- qgraph(temp.nets[[1]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Remitted: Temporal - Baseline")
             groups=groups_list, legend.cex=.7, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6.5, asize=6.5, curve=0.5, esize=3, color = groups_colors, label.cex = 2)

dev.off()

# n5 <- qgraph(bet.nets[[1]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
#              groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
#              vsize=6, asize=3, curve=0.5, esize=3)
# n6 <- qgraph(bet.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Remitted: Temporal - Baseline")
#              groups=groups_list, legend.cex=1, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
#              vsize=6, asize=3, curve=0.5, esize=3)

print("Controls:")
cent_group3 <- centrality_auto(n3, weighted = T)$node.centrality
print(cent_group3)
x <- min(c(cent_group3$OutExpectedInfluence, cent_group3$InExpectedInfluence)) - 0.05
y <- max(c(cent_group3$OutExpectedInfluence, cent_group3$InExpectedInfluence)) + 0.05
print(smallworldIndex(n3)$index)
centralityPlot(n3, weighted = T, labels = nodeVars,
               include = c("InExpectedInfluence", "OutExpectedInfluence")) + xlim(x,y)
print(centralityTable(n3, weighted = T, labels = nodeVars, standardized = T))
#global strength
print(sum(abs(temp.nets[[1]][which(upper.tri(temp.nets[[1]]))])))

#local connectivity (PA)
sum(sum(temp.nets[[1]]["energetic", c("wakeful", "satisfied")]),
    sum(temp.nets[[1]]["wakeful", c("energetic", "satisfied")]),
    sum(temp.nets[[1]]["satisfied", c("wakeful", "energetic")]))

#local connectivity (NA)
sum(sum(temp.nets[[1]]["down", c("irritated", "anxious", "restless")]),
    sum(temp.nets[[1]]["irritated", c("down", "anxious", "restless")]),
    sum(temp.nets[[1]]["anxious", c("irritated", "down", "restless")]),
    sum(temp.nets[[1]]["restless", c("irritated", "down", "anxious")]))


print("Remitted:")
cent_group4 <- centrality_auto(n4, weighted = TRUE)$node.centrality
print(cent_group4)
x <- min(c(cent_group4$OutExpectedInfluence, cent_group4$InExpectedInfluence)) - 0.05
y <- max(c(cent_group4$OutExpectedInfluence, cent_group4$InExpectedInfluence)) + 0.05
# print(smallworldIndex(n4)$index)
centralityPlot(n4, weighted = TRUE, labels = nodeVars,
               include = c("InExpectedInfluence", "OutExpectedInfluence")) + xlim(x,y)
print(centralityTable(n4, weighted = TRUE, labels = nodeVars, standardized = TRUE))
#global strength
print(sum(abs(temp.nets[[2]][which(upper.tri(temp.nets[[2]]))])))

#local connectivity (PA)
sum(sum(temp.nets[[2]]["energetic", c("wakeful", "satisfied")]),
    sum(temp.nets[[2]]["wakeful", c("energetic", "satisfied")]),
    sum(temp.nets[[2]]["satisfied", c("wakeful", "energetic")]))

#local connectivity (NA)
sum(sum(temp.nets[[2]]["down", c("irritated", "anxious", "restless")]),
    sum(temp.nets[[2]]["irritated", c("down", "anxious", "restless")]),
    sum(temp.nets[[2]]["anxious", c("irritated", "down", "restless")]),
    sum(temp.nets[[2]]["restless", c("irritated", "down", "anxious")]))


dev.off()


sum(sum(permutationResults$network$Temporal$EdgeWeights["energetic", c("wakeful", "satisfied")]),
    sum(permutationResults$network$Temporal$EdgeWeights["wakeful", c("energetic", "satisfied")]),
    sum(permutationResults$network$Temporal$EdgeWeights["satisfied", c("wakeful", "energetic")]))

sum(sum(permutationResults$network$Temporal$EdgeWeights["down", c("irritated", "anxious", "restless")]),
    sum(permutationResults$network$Temporal$EdgeWeights["irritated", c("down", "anxious", "restless")]),
    sum(permutationResults$network$Temporal$EdgeWeights["anxious", c("irritated", "down", "restless")]),
    sum(permutationResults$network$Temporal$EdgeWeights["restless", c("irritated", "down", "anxious")]))
# 
# xData <- data[which(data$dayBeepNum <=10),]


for(i in c("mindfulness", "fantasizing")){  
    
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    # print(g)
    print(i)
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    
    # Estimate network using multilevel VAR model
    
    #subData <- data_copy[which((data_copy$group==g) & (data_copy$phase=="pre")),]
    
    #subData <- sc_data[which((sc_data$group==g) & (sc_data$phase==p) & (sc_data$intervention==i)),]
    
    subData <- data_t[which((data_t$intervention==i) & (data_t$phase=="pre")),]
    subData$subjB <- factor(subData$subjB)
    
    print("number of subjects - pre:")
    print(length(unique(subData$subjB)))
    
    preNet <- mlVAR(subData,
                    vars=nodeVars,
                    estimator="lmer",
                    idvar="subjB",
                    dayvar="phaseAssessmentDay",
                    beepvar="dayBeepNum",
                    lags = 1,
                    temporal = "orthogonal",
                    contemporaneous = "orthogonal",
                    nCores = detectCores()-2)
    
    #  # Get mlVAR networks:
    preCont <- getNet(preNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
    #bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
    preTemp <- getNet(preNet, "temporal", nonsig = "hide")
    
    subData <- data_t[which((sc_data$intervention==i) & (data_t$phase=="peri")),]
    subData$subjB <- factor(subData$subjB)
    
    print("number of subjects - peri:")
    print(length(unique(subData$subjB)))
    
    periNet <- mlVAR(subData,
                     vars=nodeVars,
                     estimator="lmer",
                     idvar="subjB",
                     dayvar="assessmentDay",
                     beepvar="dayBeepNum",
                     lags = 1,
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     nCores = detectCores()-2)
    
    #  # Get mlVAR networks:
    periCont <- getNet(periNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
    #bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
    periTemp <- getNet(periNet, "temporal", nonsig = "hide")
    
    
    # pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
    # layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
    
    #plot contemporaneous networks
    L <- averageLayout(preCont, periCont,preTemp, periTemp)
    
    pdf(width=12, height=4,
        file = paste("contemporaneous_", i, ".pdf", sep = ""))
    layout(matrix(c(1,1,1,1,1,2,2,2,2,2,2), nc=11, byrow = TRUE)) # 40% vs 60% widths
    n1 <- qgraph(preCont, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
                 theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
                 groups=groups_list, legend.cex=1, legend=F, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6.5, asize=6.5, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
    
    n2 <- qgraph(periCont, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Peri-intervention", sep = " - ")
                 theme='colorblind', negDashed=FALSE, diag=T,
                 groups=groups_list, legend.cex=.7, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6.5, asize=6.5, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
    
    dev.off()
    
    #plot temporal networks
    # L <- averageLayout(preTemp, periTemp)
    
    pdf(width=12, height=4,
        file = paste("temporal_", i, ".pdf", sep = ""))
    layout(matrix(c(1,1,1,1,1,2,2,2,2,2,2), nc=11, byrow = TRUE)) # 40% vs 60% widths
    
    n3 <- qgraph(preTemp, layout = L, #title=paste("mlVAR: Temporal network", g, i, "Baseline", sep = " - "),
                 theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
                 groups=groups_list, legend.cex=1, legend=F, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6.5, asize=6.5, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
    
    n4 <- qgraph(periTemp, layout = L, #title=paste("mlVAR: Temporal network", g, i, "Peri-intervention", sep = " - ")
                 theme='colorblind', negDashed=FALSE, diag=T,
                 groups=groups_list, legend.cex=.7, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6.5, asize=6.5, curve=0.5, esize=3, color = groups_colors, label.cex = 2)
    
    dev.off()
    
    print("Contemporaneous Networks:")
    print('Pre')
    cent_measures1 <- centrality_auto(n1, weighted = TRUE)$node.centrality
    print(cent_measures1)

    print(sum(abs(preCont[which(upper.tri(preCont))])))

    print('Peri')
    cent_measures2 <- centrality_auto(n2, weighted = T)$node.centrality
    print(cent_measures2)

    print(sum(abs(periCont[which(upper.tri(periCont))])))

    
    print("Temporal Networks:")
    print('Pre')
    cent_measures3 <- centrality_auto(n3, weighted = TRUE)$node.centrality
    print(cent_measures3)

    #global strength
    print(sum(abs(preTemp[which(upper.tri(preTemp))])))

    
    print('Peri')
    cent_measures4 <- centrality_auto(n4, weighted = TRUE)$node.centrality
    print(cent_measures4)

    #global strength
    print(sum(abs(periTemp[which(upper.tri(periTemp))])))

}




reducedNodes <- c('Rumination', #'stickiness',
                  'energetic', 'wakeful', 'satisfied',
                  'down', 'irritated', 'anxious', 'restless',
                  'EventUnpleasantness', 'EventPleasantness',
                  'distracted')

#grouping the variables --> for later use in network plotting
reduced_list <- list(Rumination = c(1), PositiveAffect = c(2,3,4), NegativeAffect = c(5,6,7,8), Events=c(9,10), Other=c(11))
#Events = c(10,11))
# Sleep=c(15))
reduced_colors <- c("#d60000", "#149F36", "#53B0CF", "#72CF53", "#0558ff" )



# baseline networks
cont_pre_final <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre")),], nodes = reducedNodes,
                       iterations = 100, filepath = "network_permutations/cont_pre_final.rda",
                       localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))

# load(file = "network_permutations/cont_pre_robust.rda")

rem_pre_final <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre")),], nodes = reducedNodes,
                      iterations = 100, filepath = "network_permutations/rem_pre_final.rda",
                      localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))


#compare groups at baseline
dat <- data_t[which((data_t$phase=="pre")),]
compare_group_pre_final <- NPT(dat, nodes = reducedNodes, iterations = 100, permuteBy = "group",
                         filepath = "network_permutations/compare_group_pre_final.rda",
                         localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))


#remitted fantasizing pre / peri
all_pre_fant_final <- NPT(data_t[which((data_t$phase == "pre") & (data_t$intervention == "fantasizing")),],
                            nodes = nodeVars, iterations = 200, filepath = "network_permutations/all_pre_fant_final.rda")


all_peri_fant_final <- NPT(data_t[which((data_t$phase == "peri") & (data_t$intervention == "fantasizing")),],
                             nodes = nodeVars, iterations = 200, filepath = "network_permutations/all_peri_fant_final.rda")


#remitted mindfulness pre / peri
all_pre_mind_final <- NPT(data_t[which((data_t$phase == "pre") & (data_t$intervention == "mindfulness")),],
                            nodes = nodeVars, iterations = 200, filepath = "network_permutations/all_pre_mind_final.rda")


all_peri_mind_final <- NPT(data_t[which((data_t$phase == "peri") & (data_t$intervention == "mindfulness")),],
                             nodes = nodeVars, iterations = 200, filepath = "network_permutations/all_peri_mind_final.rda")


#comparison tests

#compare remitted fantasizing pre / peri
dat <- data_t[which((data_t$intervention=="fantasizing")),]
compare_all_fant_final <- NPT(dat, nodes = nodeVars, iterations = 300, permuteBy = "phase", idvar = "subjP",
                                         filepath = "network_permutations/compare_all_fant_final.rda")

#compare remitted mindfulness pre / peri
dat <- data_t[which((data_t$intervention=="mindfulness")),]
compare_all_mind_final <- NPT(dat, nodes = nodeVars, iterations = 300, permuteBy = "phase", idvar = "subjP",
                                         filepath = "network_permutations/compare_all_mind_final.rda")




load("network_permutations/rem_pre_final.rda")
rem_pre_final <- copy(permutationResults)

net1 <- all_pre_mind_final$network$Temporal$EdgeWeights
net2 <- all_pre_mind_final$network$Contemporaneous$EdgeWeights

# layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths

#plot contemporaneous networks
L <- averageLayout(net1, net2)

n1 <- qgraph(net1, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
             theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=alt_list, legend=FALSE, nodeNames = alternativeNodes, labels=c(1:length(alternativeNodes)),
             vsize=10, asize=8, curve=0.5, esize=3)


n2 <- qgraph(net, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
             theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=alt_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(alt_list)),
             vsize=6, asize=3, curve=0.5, esize=3)



#############################################################
mirror_matrix <- function(m) {
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  return(m)
}

load("network_permutations/cont_pre_final.rda")
cont_pre_final <- copy(permutationResults)

length(unique(cont_pre_final$data$subjB)) #38
length(unique(rem_pre_final$data$subjB)) #21

length(unique(all_pre_fant_final$data$subjB)) #32
length(unique(all_peri_fant_final$data$subjB)) #31

length(unique(all_pre_mind_final$data$subjB)) #27
length(unique(all_peri_mind_final$data$subjB)) #27


netAll <- all_pre_fant_final

sum((netAll$p_values$Temporal$EdgeWeights < 0.025), na.rm = T)
sum((netAll$p_values$Temporal$EdgeWeights != 1), na.rm = T)

net <- compare_group_pre_final[["testStats"]][["difference"]][["EdgeWeights"]][["Temporal"]][[1]] #[["network"]][["Temporal"]][["EdgeWeights"]]

n2 <- qgraph(net, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
             theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=.5, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3, color = groups_colors)



net_pvals <- netAll[["p_values"]][["Temporal"]][["EdgeWeights"]]

# net <- copy(netAll)
net[which(net_pvals > 0.025)] <- 0

net <- mirror_matrix(net)

# net[which(is.na(net))] <- 0

n2 <- qgraph(net, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
             theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3, color = groups_colors)

cont_pre_significant <- netAll[["network"]][["Temporal"]][["EdgeWeights"]]
cont_pre_significant[which(netAll[["p_values"]][["Temporal"]][["EdgeWeights"]] > 0.025)] <- 0

compare_rem_mind_pre_peri_final$testStats$difference$LocalStrengths[[2]]$Temporal[1]

mean((abs(test)) >= ((abs(compare_group_pre_final$testStats$difference$LocalStrengths[[2]]$Temporal[[1]]))))

# 
# len <- length(compare_group_pre_final$testStats$difference$LocalStrengths[[2]]$Temporal)
# test <- c(rep(NA,len))
# 
# for(i in 1:len){
#   test[i] <- compare_group_pre_final$testStats$difference$LocalStrengths[[2]]$Temporal[[i]]
# }
# 
# test


######################################## NST / NCT Latex tables ###################################################
load("network_permutations/rem_pre_final.rda")
rem_pre_final <- permutationResults

net_type <- "Contemporaneous"

net1 <- rem_pre_final[["network"]][[net_type]][["EdgeWeights"]]
pvals1 <- rem_pre_final[["p_values"]][[net_type]][["EdgeWeights"]]
net2 <- cont_pre_final[["network"]][[net_type]][["EdgeWeights"]]
pvals2 <- cont_pre_final[["p_values"]][[net_type]][["EdgeWeights"]]
netDiff <- compare_group_pre_final[["testStats"]][["difference"]][["EdgeWeights"]][[net_type]][[1]]
pvalsDiff <- compare_group_pre_final[["p_values"]][[net_type]][["EdgeWeights"]]

nets <- list(list(net1, pvals1), list(net2, pvals2), list(netDiff, pvalsDiff))
#if net_type == "Contemporaneous 66 else 121
mlength <- 66

m <- matrix(rep(NA,mlength), ncol = 1)
sign_m <- matrix(rep(NA,mlength), ncol = 1)

for(net in nets){
  
  for(n in net){
    rownames(n)[rownames(n) == 'sumNA'] <- 'NegativeAffect'
    rownames(n)[rownames(n) == 'sumPA'] <- 'PositiveAffect'
    rownames(n)[rownames(n) == 'negMax'] <- 'EventUnpleasantness'
    rownames(n)[rownames(n) == 'posMax'] <- 'EventPleasantness'
    rownames(n)[rownames(n) == 'ruminating'] <- 'Rumination'
    rownames(n)[rownames(n) == 'energetic'] <- 'Energy'
    rownames(n)[rownames(n) == 'wakeful'] <- 'Wakefulness'
    rownames(n)[rownames(n) == 'satisfied'] <- 'Satisfaction'
    rownames(n)[rownames(n) == 'down'] <- 'Sadness'
    rownames(n)[rownames(n) == 'anxious'] <- 'Anxiety'
    rownames(n)[rownames(n) == 'restless'] <- 'Restlessness'
    rownames(n)[rownames(n) == 'irritated'] <- 'Irritation'
    rownames(n)[rownames(n) == 'distracted'] <- 'Distraction'
  }

  net[[2]] <- as.matrix(net[[2]])
  
  if(net_type == "Contemporaneous"){
    
    e <- t(net[[1]])[lower.tri(t(net[[1]]), diag=TRUE)]
    e <- matrix(round(e,2), ncol = 1)
    p <- t(net[[2]])[lower.tri(t(net[[2]]), diag=TRUE)]
    p <- matrix(round(p,3), ncol = 1)
    
    #create significant only matrix  
    net[[1]][which(net[[2]]>0.025)] <- NA
    net[[2]][which(net[[2]]>0.025)] <- NA
    
    sign_e <- t(net[[1]])[lower.tri(t(net[[1]]), diag=TRUE)]
    sign_e <- matrix(round(sign_e,2), ncol = 1)
    sign_p <- t(net[[2]])[lower.tri(t(net[[2]]), diag=TRUE)]
    sign_p <- matrix(round(sign_p,3), ncol = 1)
    
  } else {
    
    #create full matrix
    e <- as.vector(t(net[[1]]))
    e <- matrix(round(e,2), ncol = 1)
    p <- as.vector(t(net[[2]]))
    p <- matrix(round(p,3), ncol = 1)
    
    #create significant only matrix  
    net[[1]][which(net[[2]]>0.025)] <- NA
    net[[2]][which(net[[2]]>0.025)] <- NA
    
    sign_e <- as.vector(t(net[[1]]))
    sign_e <- matrix(round(sign_e, 2), ncol = 1)
    sign_p <- as.vector(t(net[[2]]))
    sign_p <- matrix(round(sign_p, 3), ncol = 1)
    
  }
  
  m <- cbind(m, e, p)
  sign_m <- cbind(sign_m, sign_e, sign_p)
}

var_names <- rownames(n)

if(net_type=="Contemporaneous"){
  rnames <- c()
  loop_names <- var_names
  j <- 1
  for(n1 in loop_names){
    for(n2 in loop_names){
      rnames[j] <- paste(n1, n2, sep = " -- ")  
      j <- j + 1
    }
    loop_names <- loop_names[-1]
  } 
  
} else {
  rnames <- paste(rep(var_names, each = length(var_names)), var_names, sep = " --> ")
  
}

rownames(m) <- rnames
rownames(sign_m) <- rnames

m <- m[,-1]
sign_m <- sign_m[,-1]
sign_m <- sign_m[rowSums(is.na(sign_m)) != ncol(sign_m), ]

colnames(m) <- c("C-EW", "C-PV", "R-EW", "R-PV", "Diff", "Diff-PV")
colnames(sign_m) <- c("C-EW", "C-PV", "R-EW", "R-PV", "Diff", "Diff-PV")

df <- data.frame(m)
df <- df[df$C.PV < 0.025 | df$R.PV < 0.025 | df$Diff.PV < 0.025,]
df <- df[complete.cases(df),] 

Hmisc::latex(df, cdec=c(0,2,2,2,4), na.blank=TRUE,
      booktabs=TRUE, table.env=TRUE, center="none", file="", title="")

net_type <- "Contemporaneous"

net1 <- round(rem_pre_final[["network"]][[net_type]][["Centrality"]], 2)
pvals1 <- round(rem_pre_final[["p_values"]][[net_type]][["Centrality"]], 3)
net2 <- round(cont_pre_final[["network"]][[net_type]][["Centrality"]], 2)
pvals2 <- round(cont_pre_final[["p_values"]][[net_type]][["Centrality"]], 3)
# netDiff <- round(compare_group_pre_final[["testStats"]][["difference"]][["Centrality"]][[net_type]][[1]], 2)
pvalsDiff <- round(compare_group_pre_final[["p_values"]][[net_type]][["Centrality"]], 3)

if(net_type=="Contemporaneous"){
  centDF <- cbind(net1[,1], pvals1[,1],
                  net2[,1], pvals2[,1],
                  round(net1[,1]-net2[,1], 2), pvalsDiff[,1])
  
  colnames(centDF) <- c("R.Strength", "p", "C.Strength", "p", "Diff.Strength", "p")
  
} else {
  centDF <- cbind(net1[,1], pvals1[,1],
                  net1[,2], pvals1[,2],
                  net2[,1], pvals2[,1],
                  net2[,2], pvals2[,2],
                  round(net1[,1]-net2[,1], 2), pvalsDiff[,1],
                  round(net1[,2]-net2[,2], 2), pvalsDiff[,2])
  
  colnames(centDF) <- c("R.InS", "p", "R.Out", "p", "C.InS", "p", "C.OutS", "p", "Diff.InS", "p", "Diff.OutS", "p")  
  
}


rownames(centDF) <- var_names


Hmisc::latex(centDF, cdec=c(0,2,2,2,4), na.blank=TRUE,
             booktabs=TRUE, table.env=TRUE, center="none", file="", title="")

######################################## Calculate PA / NA Strength ###################################################

cont_pre_significant <- netAll[["network"]][["Temporal"]][["EdgeWeights"]]
cont_pre_significant[which(netAll[["p_values"]][["Temporal"]][["EdgeWeights"]] > 0.025)] <- 0


net

pa_nodes <- c("Energy", "Wakefulness", "Satisfaction")
na_nodes <- c("Sadness", "Irritation", "Anxiety", "Restlessness")


vars <- c('Rumination',
              'Energy', 'Wakefulness', 'Satisfaction',
              'Sadness', 'Irritation', 'Anxiety', 'Restlessness',
              'EventUnpleasantness', 'EventPleasantness',
              'Distraction')#, 'NegativeAffect', 'PositiveAffect')

pa_nodes <- c("energetic", "wakeful", "satisfied")
na_nodes <- c("down", "irritated", "anxious", "restless")

vars <- c('ruminating',
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless',
              'EventUnpleasantness', 'EventPleasantness',
              'distracted')

non_pa_nodes <- vars[which(vars %nin% pa_nodes)]
non_na_nodes <- vars[which(vars %nin% na_nodes)]


nets <- compare_group_pre_final$testStats$difference$EdgeWeights$Contemporaneous
nets <- rem_pre_final$testStats$network$EdgeWeights$Contemporaneous
lenDat <- length(nets)

pa_results <- list()
na_results <- list()

pa_results[["Strength"]] <- c(rep(NA, lenDat))
pa_results[["InStrength"]] <- c(rep(NA, lenDat))
pa_results[["OutStrength"]] <- c(rep(NA, lenDat))

na_results[["Strength"]] <- c(rep(NA, lenDat))
na_results[["InStrength"]] <- c(rep(NA, lenDat))
na_results[["OutStrength"]] <- c(rep(NA, lenDat))

for(i in 1:lenDat){
  current_net <- nets[[i]]
  if(isSymmetric(current_net)){
    
    pa_strength <-  sum(abs(current_net[pa_nodes, non_pa_nodes])) # [rows, columns]
    pa_results$Strength[i] <- pa_strength
    
    na_strength <-  sum(abs(current_net[na_nodes, non_na_nodes])) # [rows, columns]
    na_results$Strength[i] <- na_strength
    
  } else {
    
    pa_out_strength <- sum(abs(current_net[pa_nodes, non_pa_nodes])) #outstrength
    pa_in_strength <- sum(abs(current_net[non_pa_nodes, pa_nodes])) #instrength
    pa_results$InStrength[i] <- pa_in_strength
    pa_results$OutStrength[i] <- pa_out_strength
    
    na_out_strength <- sum(abs(current_net[na_nodes, non_na_nodes])) #outstrength
    na_in_strength <- sum(abs(current_net[non_na_nodes, na_nodes])) #instrength
    na_results$InStrength[i] <- na_in_strength
    na_results$OutStrength[i] <- na_out_strength
  }
  
}

if(isSymmetric(current_net)){
  print("Contemporaneous Network")
  
  pa_pval <- mean((pa_results$Strength) >= (pa_results$Strength[1]), na.rm = TRUE)
  
  na_pval <- mean((na_results$Strength) >= (na_results$Strength[1]), na.rm = TRUE)
  
  print("PA Strength p-value:")
  print(pa_pval)
  print(pa_results$Strength[1])
  
  print("NA Strength p-value:")
  print(na_pval)
  print(na_results$Strength[1])
  
} else {
  print("Temporal Network")
  
  pa_in_pval <- mean((pa_results$InStrength) >= (pa_results$InStrength[1]), na.rm = TRUE)
  pa_out_pval <- mean((pa_results$OutStrength) >= (pa_results$OutStrength[1]), na.rm = TRUE)
  
  na_in_pval <- mean((na_results$InStrength) >= (na_results$InStrength[1]), na.rm = TRUE)
  na_out_pval <- mean((na_results$OutStrength) >= (na_results$OutStrength[1]), na.rm = TRUE)  
  
  print("PA InStrength p-value:")
  print(pa_in_pval)
  print(pa_results$InStrength[1])
  
  print("PA OutStrength p-value:")
  print(pa_out_pval)
  print(pa_results$OutStrength[1])
  
  print("NA InStrength p-value:")
  print(na_in_pval)
  print(na_results$InStrength[1])
  
  print("NA OutStrength p-value:")
  print(na_out_pval)
  print(na_results$OutStrength[1])
}


###################################### Stuff for presentation ###################################################

## network estimation

build_up <- rem_pre_final[["network"]][["Contemporaneous"]][["EdgeWeights"]]

toAdd1 <- build_up["ruminating", "anxious"]
toAdd2 <- build_up["restless", "anxious"]

build_up["ruminating",] <- 0
build_up[,"ruminating"] <- 0

build_up["anxious",] <- 0
build_up[,"anxious"] <- 0

build_up["restless","distracted"] <- 0
build_up["distracted","restless"] <- 0

build_up["distracted", "irritated"] <- 0
build_up["irritated", "distracted"] <- 0

build_up["distracted", "energetic"] <- 0
build_up["energetic","distracted"] <- 0

build_up["satisfied", "energetic"] <- 0
build_up["energetic","satisfied"] <- 0

build_up["satisfied", "wakeful"] <- 0
build_up["wakeful", "satisfied"] <- 0

build_up["satisfied", "down"] <- 0
build_up["down", "satisfied"] <- 0

build_up["satisfied", "EventUnpleasantness"] <- 0
build_up["EventUnpleasantness", "satisfied"] <- 0

build_up["satisfied", "EventPleasantness"] <- 0
build_up["EventPleasantness", "satisfied"] <- 0

build_up["EventUnpleasantness", "EventPleasantness"] <- 0
build_up["EventPleasantness", "EventUnpleasantness"] <- 0

build_up["ruminating", "anxious"] <- toAdd1
build_up["anxious","ruminating"] <- toAdd1

build_up["restless","anxious"] <- toAdd2
build_up["anxious","restless"] <- toAdd2

n2 <- qgraph(build_up, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
             theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3, color = groups_colors)

## permutation testing

for(k in 1:5){
  perm.example <- rem_pre_final[["network"]][["Contemporaneous"]][["EdgeWeights"]]
  for(i in 1:nrow(perm.example)){
    for(j in 1:ncol(perm.example)){
      new_val <- sample(perm.example, 1)
      perm.example[i,j] <- new_val
    }
  }  
  
  perm.example <- mirror_matrix(perm.example)
  diag(perm.example) <- 0
  
  n2 <- qgraph(perm.example, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
               theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
               groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
               vsize=6, asize=3, curve=0.5, esize=3, color = groups_colors)
  print(n2)
}


mean(data[which((data$group == "remitted") & (data$phase == "pre")),]$energetic, na.rm = T)
mean(data[which((data$group == "controls") & (data$phase == "pre")),]$energetic, na.rm = T)
t.test(data[which((data$group == "remitted") & (data$phase == "pre")),]$energetic, data[which((data$group == "controls") & (data$phase == "pre")),]$energetic)

sd(data[which((data$group == "remitted") & (data$phase == "pre")),]$energetic, na.rm = T)
sd(data[which((data$group == "controls") & (data$phase == "pre")),]$energetic, na.rm = T)
var.test(data[which((data$group == "remitted") & (data$phase == "pre")),]$energetic, data[which((data$group == "controls") & (data$phase == "pre")),]$energetic)