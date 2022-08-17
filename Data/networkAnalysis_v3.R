#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################## Set up ########################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202205")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202205")
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
library(NetworkToolbox)


#read in data
data <- read.csv('preprocessed_data.csv') 


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################### Some data prep ################################################################# 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

responses_block <- ddply(data, .(subject), plyr::summarise,
                         numCompleted = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numCompleted - noResponse,
                         responseRate = round(response/numCompleted,2),
                         numDays = max(assessmentDay))

data$subjB <- interaction(data$subject, data$block, drop = TRUE)

meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.6%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subject)) #20
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subject)) #26
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subject)) #33

#removing participants with a response rate lower than 60%
pp <- unique(responses_block[which(responses_block$responseRate >= 0.5),]$subject)
data <- data[which(data$subject %in% pp),]
data <- data[which(data$dayBeepNum < 11),] #three subjects had extra beeps on a day. Since this messes with the detrending code I just removed
#the extra beeps (<15 entries)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############################################################ Detrend data #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Following Borsboom et al, 2021
# some of the variables have trends (i.e. linear or other changes over time)
# we remove these trends before estimation network structures
# we work with most variables here that we collected
# afterwards, we select those we want to estimate networks on

# Alpha to detrend:
alpha <- 0.05

# Variables to investigate:
vars <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'wakeful', 'down', 'satisfied',
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
############################################################ Node Selection #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all nodes we might be interested in
metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'wakeful', 'down', 'satisfied',
                'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
                'negMax', 'negIntensity', "sleepQuality")

#we may run into convergence issues when we include too many nodes. Especially when trying to explore the networks of
#group + intervention + phase + block...
#for that reason we perform hierarchical clustering to find highly correlated variables
#we have already done this for the mixed effects models. However, because we use much smaller parts of the data per
#network, we may need to simplify even more for network analysis.

dat_clust <- copy(data)
dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
dat_clust <- dat_clust[,metricCols]
dat_clust <- dat_clust[complete.cases(dat_clust), ]

goldbricker(dat_clust, p=.01, threshold = 0.5) #suggests reducing restOfDayPos and sumPA as well as stressed and restless

#only excluding sumNA and sumPA
collin.fnc(dat_clust[,-c(3, 4)])$cnumber #~27.5 --> near problematic collinearity
plot(varclus(as.matrix(dat_clust[,-c(3, 4)])))

#for the mixed-effects models we only kept the following
collin.fnc(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19)])$cnumber #~16.3 #shouldn't be problematic
plot(varclus(as.matrix(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19)])))

#for the network models, we ideally want to keep more of the PA and NA variables, not just their aggregate
collin.fnc(dat_clust[,-c(3, 4, 12, 14, 16, 17, 19)])$cnumber #~23.4 #also not too problematic probably
plot(varclus(as.matrix(dat_clust[,-c(3, 4, 12, 14, 16, 17, 19)])))
# 
# #for PA we might want to combine wakeful and energetic; for NA restless and anxious as well as down and irritated.
# #Potentially, we may also combine ruminating and stickiness
# data$energeticWakeful <- NA
# data$downIrritated <- NA
# data$anxiousRestless <- NA
# for(row in 1:nrow(data)){
#   if((!is.na(data$wakeful[row])) & (!is.na(data$energetic[row]))){
#     data$energeticWakeful[row] <- (data$wakeful[row] + data$energetic[row])/2
#   }
#   if((!is.na(data$down[row])) & (!is.na(data$irritated[row]))){
#     data$downIrritated[row] <- (data$down[row] + data$irritated[row])/2
#   }
#   if((!is.na(data$restless[row])) & (!is.na(data$anxious[row]))){
#     data$anxiousRestless[row] <- (data$anxious[row] + data$restless[row])/2
#   }
# }
# 
# metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA',
#                 'wakeful', 'satisfied', 'energetic', 'energeticWakeful',
#                 'down', 'irritated', 'restless', 'anxious', 'downIrritated', 'anxiousRestless',
#                 'listless', 'distracted',
#                 'posIntensity', 'negIntensity',
#                 'sleepQuality')
# 
# dat_clust <- copy(data)
# dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
# dat_clust <- dat_clust[,metricCols]
# dat_clust <- dat_clust[complete.cases(dat_clust), ]
# 
# collin.fnc(dat_clust[,-c(3, 4, 5, 7, 9, 10, 11, 12)])$cnumber #~17.7
# plot(varclus(as.matrix(dat_clust[,-c(3, 4, 5, 7, 9, 10, 11, 12)])))
# 
# nodeVars <- c('ruminating', 'stickiness','energeticWakeful', 'satisfied',
#               'downIrritated', 'anxiousRestless',
#               'posIntensity', 'negIntensity',
#               'listless', 'distracted', 'sleepQuality')
# 
# dat_clust <- copy(data)
# dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
# dat_clust <- dat_clust[,nodeVars]
# dat_clust <- dat_clust[complete.cases(dat_clust), ]
# 
# goldbricker(dat_clust) #doesn't suggest reductions...
# 
# 
# nodeVars <- c('ruminating', 'stickiness', 'wakeful','energetic', 'energeticWakeful', 'satisfied',
#               'down', 'irritated', 'restless', 'anxious', 'downIrritated', 'anxiousRestless',
#               'posIntensity', 'negIntensity',
#               'listless', 'distracted', 'sleepQuality')
# 

#we will try with these (basically the same as for gams, but splitting up sumPA and sumNA)
nodeVars <- c('ruminating', 'stickiness',
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless',
              'listless', 'distracted',
              'posMax', 'negMax')
              #'sleepQuality') #sleepQuality turned out to cause issues since it is only measured once a day
  
# nodeVars <- c('ruminating',#, 'stickiness',
#               'energetic', 'wakeful', 'satisfied',
#               'down', 'irritated', 'anxious', 'restless',
#               'listless', 'distracted')
# 
# nodeVars <- c('ruminating', 'stickiness',
#               'sumPA',
#               'sumNA',
#               'posIntensity', 'negIntensity',
#               'listless', 'distracted')
# # 'posIntensity',
# 'listless', 'distracted')

#creating a copy of data without missing cases
data_copy <- data.table::copy(data_detrended)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[complete.cases(data_copy[nodeVars]),]

#creating a scaled version
sc_data <- copy(data_copy)
sc_data[nodeVars] <- scale(data_copy[nodeVars])


#paranormal transformation (because the model assumes normality) --> recommended by Epskamp
#huge.npn() should only be applied to data without missing values! Otherwise creates weird values!!!
data_t <- copy(data_copy)
data_t[,nodeVars] <- huge.npn(data_t[,nodeVars])

#grouping the variables --> for later use in network plotting
groups_list <- list(Rumination = c(1,2), PositiveAffect = c(3,4,5), NegativeAffect = c(6,7,8,9),
                    NegativeEmotions = c(10,11), 
                    Events = c(12,13))#,)#, Sleep = c(11)) , 
# Sleep=c(15))
groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53")#, "#0558ff")

# #lambda is needed for certain psychonetrics estimators (which we probably won't use)
# Lambda = matrix(1, length(nodeVars),1)
  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################## Network Estimation ################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############################################################# baseline networks ################################################################
for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){
    for(p in c("pre", "peri")){
      
      print(paste(g, i, p, sep = " - "))
      
      dat <- data_t[which((data_t$group==g) & (data_t$intervention==i) & (data_t$phase==p)),] # 
      
      net <- estimateNetwork(dat, vars = nodeVars,
                                     default = "graphicalVAR",
                                     idvar = "subjB",
                                     dayvar = "blockAssessmentDay",
                                     beepvar = "dayBeepNum")
      
      npBoot <- bootnet(net, boots = 1000, nCores = 10, statistics = c("strength", "expectedInfluence", "edge")) 
      
      caseBoot <- bootnet(net, boots = 1000, nCores = 10, type = "case",
                                                   statistics = c("strength", "expectedInfluence", "edge")) 
      
      save(net, npBoot, caseBoot, file = paste("Full_", g, i, p, ".rda", sep = ""))
      
      
    }
  }
}


# source("NCT_temporal.R")
# source("NCT_test.R")
# source("NCT_estimators_test.R")


NCT_temporal(dat, dat1, it = 100, estimator = "graphicalVAR",
             estimatorArgs = list(vars = nodeVars,
                                  idvar = "subjB",
                                  dayvar = "blockAssessmentDay",
                                  beepvar = "dayBeepNum"))

NCT_test(dat, dat1, estimator = "ggm", estimatorArgs = list(vars = nodeVars))


filenames = c("Full_controlsfantasizingpre.rda", "Full_controlsfantasizingperi.rda", "Full_controlsmindfulnesspre.rda", "Full_controlsmindfulnessperi.rda",
              "Full_remittedfantasizingpre.rda", "Full_remittedfantasizingperi.rda", "Full_remittedmindfulnesspre.rda", "Full_remittedmindfulnessperi.rda")


for(f in filenames){
  load(f)
  
  print('+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
  print(f)
  print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
  
  layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
  TL <- averageLayout(net$graph$temporal, net$graph$contemporaneous)
  
  #plotting the temporal networks next to one another
  #we find that the temporal relations seem quite spurious
  plot(net, graph = "temporal",
       title = paste(f, "temporal", sep = " - "),
       nodeNames = nodeVars,
       groups = groups_list,
       legend = FALSE,
       labels = c(1:length(nodeVars)),
       layout = TL)
  
  plot(net, graph = "contemporaneous",
       title = paste(f, "contemporaneous", sep = " - "),
       nodeNames = nodeVars,
       groups = groups_list,
       legend.cex = 0.7,
       labels = c(1:length(nodeVars)),
       layout = TL)
  
  print(corStability(caseBoot))
  plot(npBoot, labels = FALSE, order = "sample", graph = "contemporaneous")
  plot(npBoot, graph = "contemporaneous", labels = FALSE)
  # plot(npBoot, graph = "contemporaneous", "strength", order = "sample")
  # plot(npBoot, "expectedInfluence", order = "sample", graph = "contemporaneous")
  # plot(npBoot, graph = "contemporaneous", "edge", order = "sample", plot = "difference", onlyNonZero = TRUE)
  
  plot(npBoot, labels = FALSE, order = "sample", graph = "temporal")
  plot(npBoot, graph = "temporal", labels = FALSE)
  # plot(npBoot, graph = "temporal", "strength", order = "sample")
  # plot(npBoot, "expectedInfluence", order = "sample", graph = "temporal")
  # plot(npBoot, graph = "temporal", "edge", order = "sample", plot = "difference", onlyNonZero = TRUE)
}



conBase <- data_t[which((data_t$group=="controls") & (data_t$phase=="pre")),] #using the non-paranormalized data
remBase <- data_t[which((data_t$group=="remitted") & (data_t$phase=="pre")),] # & (data_t$intervention=="mindfulness")

#we opt to use graphicalVAR
conNet_base <- estimateNetwork(conBase, vars = nodeVars,
                           default = "graphicalVAR",
                           idvar = "subjB",
                           dayvar = "blockAssessmentDay",
                           beepvar = "dayBeepNum")

remNet_base <- estimateNetwork(remBase, vars = nodeVars,
                           default = "graphicalVAR",
                           idvar = "subjB",
                           dayvar = "blockAssessmentDay",
                           beepvar = "dayBeepNum")

#some layout formatting
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
TL <- averageLayout(remNet_base$graph$temporal, conNet_base$graph$temporal)

#plotting the temporal networks next to one another
#we find that the temporal relations seem quite spurious
plot(conNet_base, graph = "temporal",
     title = "Controls - Baseline",
     nodeNames = nodeVars,
     groups = groups_list,
     legend = FALSE,
     labels = c(1:length(nodeVars)),
     layout = TL)

plot(remNet_base, graph = "temporal",
     title = "Remitted - Baseline",
     nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.7,
     labels = c(1:length(nodeVars)),
     layout = TL)

#plotting the contemporaneous networks
CL <- averageLayout(remNet_base$graph$contemporaneous, conNet_base$graph$contemporaneous)

plot(conNet_base, graph = "contemporaneous",
     nodeNames = nodeVars,
     groups = groups_list,
     legend = FALSE,
     labels = c(1:length(nodeVars)),
     layout = CL)

plot(remNet_base, graph = "contemporaneous",
     nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.7,
     labels = c(1:length(nodeVars)),
     layout = CL)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################## Stability Analysis ################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############################################################# baseline networks ################################################################

#nonparametric bootstrap (takes up to 7 hours!)
if(FALSE){
  conNet_boot_noMoodReactivity <- bootnet(conNet_base, boots = 500, nCores = 10, statistics = c("strength", "expectedInfluence", "edge")) 
  save(conNet_boot_noMoodReactivity, file = "conNet_boot_noMoodReactivity.rda")
} else {
  load("conNet_boot_noMoodReactivity.rda")
}

#creating different plots (most of which don't work because of the poor results?)
plot(conNet_base_boot1, labels = FALSE, order = "sample", graph = "contemporaneous")
plot(conNet_base_boot1, graph = "contemporaneous", labels = FALSE)
plot(conNet_base_boot1, graph = "contemporaneous", "strength", order = "sample")
plot(conNet_base_boot1, "expectedInfluence", order = "sample", graph = "contemporaneous")
plot(conNet_base_boot1, graph = "contemporaneous", "edge", order = "sample", plot = "difference", onlyNonZero = TRUE)

#case-dropping bootstrap (also takes up to 7 hours)
if(FALSE){
  conNet_boot_noMoodReactivity_case <- bootnet(conNet_base, boots = 500, nCores = 10, type = "case", statistics = c("strength", "expectedInfluence", "edge")) 
  save(conNet_boot_noMoodReactivity_case, file = "conNet_boot_noMoodReactivity_case.rda")
} else {
  load("conNet_boot_noMoodReactivity_case.rda")
}
#looking at the stability coefficients (plots are actually not necessary anymore --> the stability coefficient is sufficient)
corStability(conNet_boot_noMoodReactivity_case)

#we see stability coefficients of 0 for edge weight --> network possibly too complex

#nonparametric bootstrap (takes up to 7 hours!)
if(FALSE){
  remNet_boot_noMoodReactivity <- bootnet(remNet_base, boots = 500, nCores = 10, statistics = c("strength", "expectedInfluence", "edge")) 
  save(remNet_boot_noMoodReactivity, file = "remNet_boot_noMoodReactivity.rda")
} else {
  load("remNet_boot_noMoodReactivity.rda")
}

#creating different plots (most of which don't work because of the poor results?)
plot(remNet_base_boot1, labels = FALSE, order = "sample", graph = "contemporaneous")
plot(remNet_base_boot1, graph = "contemporaneous", labels = FALSE)
plot(remNet_base_boot1, graph = "contemporaneous", "strength", order = "sample")
plot(remNet_base_boot1, "expectedInfluence", order = "sample", graph = "contemporaneous")
plot(remNet_base_boot1, graph = "contemporaneous", "edge", order = "sample", plot = "difference", onlyNonZero = TRUE)

#case-dropping bootstrap (also takes up to 7 hours)
if(FALSE){
  remNet_boot_noMoodReactivity_case <- bootnet(remNet_base, boots = 500, nCores = 10, type = "case", statistics = c("strength", "expectedInfluence", "edge")) 
  save(remNet_boot_noMoodReactivity_case, file = "remNet_boot_noMoodReactivity_case.rda")
} else {
  load("remNet_boot_noMoodReactivity.rda")
}
#looking at the stability coefficients (plots are actually not necessary anymore --> the stability coefficient is sufficient)
corStability(remNet_boot_noMoodReactivity_case)
#we see stability coefficients of 0 for edge weight --> network possibly too complex













#Peri-networks
for(g in c("controls", "remitted")){
    f_dat <- data_t[which((data_t$group==g) & (data_t$intervention=="fantasizing") & (data_t$phase=="peri")),]
    m_dat <- data_t[which((data_t$group==g) & (data_t$intervention=="mindfulness") & (data_t$phase=="peri")),]
    
    f_boot <- estimateNetwork(f_dat, vars = nodeVars,
                               default = "graphicalVAR",
                               idvar = "subjB",
                               dayvar = "blockAssessmentDay",
                               dayBeepNumvar = "daydayBeepNumNum")
    
    m_boot <- estimateNetwork(m_dat, vars = nodeVars,
                              default = "graphicalVAR",
                              idvar = "subjB",
                              dayvar = "blockAssessmentDay",
                              dayBeepNumvar = "daydayBeepNumNum")
    
    layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
    TL <- averageLayout(f_boot$graph$temporal, m_boot$graph$temporal)
    
    plot(f_boot, graph = "temporal",
         title = paste(g, "Peri-Fantasizing", sep = " | "),
         nodeNames = nodeVars,
         groups = groups_list,
         legend = FALSE,
         labels = c(1:13),
         layout = TL)
    
    plot(m_boot, graph = "temporal",
         title = paste(g, "Peri-Mindfulness", sep = " | "),
         nodeNames = nodeVars,
         groups = groups_list,
         legend.cex = 0.5,
         labels = c(1:13),
         layout = TL)
    
    CL <- averageLayout(f_boot$graph$contemporaneous, m_boot$graph$contemporaneous)
    
    plot(f_boot, graph = "contemporaneous",
         title = paste(g, "Peri-Fantasizing", sep = " | "),
         nodeNames = nodeVars,
         groups = groups_list,
         legend = FALSE,
         labels = c(1:13),
         layout = CL)
    
    plot(m_boot, graph = "contemporaneous",
         title = paste(g, "Peri-Mindfulness", sep = " | "),
         nodeNames = nodeVars,
         groups = groups_list,
         legend.cex = 0.5,
         labels = c(1:13),
         layout = CL)
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###################################### Stability Analysis of the network with fewest data ###################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#This is to see if we can afford splitting the data by block or not
#If this network is not stable, we need to combine blocks to hopefully get stable results

#We only take the data for the remitted-mindfulness group in block 2 - peri intervention
rMP2 <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="mindfulness") & (data_t$phase=="peri") & (data_t$block==2)),]

#estimating the network
rMP2_net <- estimateNetwork(rMP2, vars = nodeVars,
                          default = "graphicalVAR",
                          idvar = "subjB",
                          dayvar = "blockAssessmentDay",
                          dayBeepNumvar = "daydayBeepNumNum")

#nonparametric bootstrap (takes up to 7 hours!)
if(FALSE){
  rMP2_boot1 <- bootnet(rMP2_net, boots = 1000, nCores = 8, statistics = c("strength", "expectedInfluence", "edge")) 
  save(rMP2_boot1, file = "remMF_Peri_Block2.rda")
} else {
  load("remMF_Peri_Block2.rda")
}

#creating different plots (most of which don't work because of the poor results?)
plot(rMP2_boot1, labels = FALSE, order = "sample", graph = "contemporaneous")
plot(rMP2_boot1, graph = "contemporaneous", labels = FALSE)
plot(rMP2_boot1, graph = "contemporaneous", "strength", order = "sample")
plot(rMP2_boot1, "expectedInfluence", order = "sample", graph = "contemporaneous")
plot(rMP2_boot1, graph = "contemporaneous", "edge", order = "sample", plot = "difference", onlyNonZero = TRUE)

#case-dropping bootstrap (also takes up to 7 hours)
if(FALSE){
  rMP2_boot2 <- bootnet(rMP2_net, boots = 1000, nCores = 10, type = "case", statistics = c("strength", "expectedInfluence", "edge")) 
  save(rMP2_boot2, file = "remMF_Peri_Block2_case.rda")
} else {
  load("remMF_Peri_Block2_case.rda")
}
#looking at the stability coefficients (plots are actually not necessary anymore --> the stability coefficient is sufficient)
corStability(rMP2_boot2)
#we see stability coefficients of 0 for all statistics --> our networks are definitely not interpretable














####################################################################################################################################################
# centralityPlot(f_boot$graph$contemporaneous, include = c("Strength", "ExpectedInfluence", "Closeness"))
# 
# centralityTable(f_boot$graph$contemporaneous)
# 
# getWmat(f_boot) #get partial correlation matrices 
# 
# f1 <- bootnet(f_boot, boots = 1000, nCores = 8, statistics = c("strength", "expectedInfluence", "edge")) #non-parametric bootstrap
# f2 <- bootnet(f_boot, boots = 1000, nCores = 6, type = "case", statistics = c("strength", "expectedInfluence", "edge")) #case-dropping bootstrap
# 
# corStability(f1)
# corStability(f2)




nodeVars <- c('ruminating', #'sumPA', 'sumNA')
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless')
              #'listless')
#'sleepQuality') #sleepQuality turned out to cause issues since it is only measured once a day

#creating a copy of data without missing cases
data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[complete.cases(data_copy[nodeVars]),]

#creating a scaled version
sc_data <- copy(data_copy)
sc_data[nodeVars] <- scale(data_copy[nodeVars])


#paranormal transformation (because the model assumes normality) --> recommended by Epskamp
#huge.npn() should only be applied to data without missing values! Otherwise creates weird values!!!
data_t <- copy(data_copy)
data_t[,nodeVars] <- huge.npn(data_t[,nodeVars])

#grouping the variables --> for later use in network plotting
groups_list <- list(Rumination = c(1), PositiveAffect = c(2:4), NegativeAffect = c(5:8))
                    #OtherNegative = c(9))#, Sleep = c(11)) 
groups_colors <- c("#d60000", "#149F36", "#53B0CF")#, "#f66a6a")#, "#72CF53")#, "#0558ff")

type_list <- c("g", "g", "g", "g", "g", "g", "g", "g")
level_list <- c(1, 1, 1, 1, 1, 1, 1, 1)
# #lambda is needed for certain psychonetrics estimators (which we probably won't use)
# Lambda = matrix(1, length(nodeVars),1)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########################################################## Network Estimation ################################################################## 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############################################################# baseline networks ################################################################
conBase <- data_t[which((data_t$group=="controls") & (data_t$phase=="pre")),] #using the non-paranormalized data
remBase <- data_t[which((data_t$group=="remitted") & (data_t$phase=="pre")),] 

#we opt to use graphicalVAR
conNet_base <- estimateNetwork(conBase, vars = nodeVars,
                               default = "graphicalVAR",
                               idvar = "subjB",
                               dayvar = "blockAssessmentDay",
                               dayBeepNumvar = "daydayBeepNumNum")

remNet_base <- estimateNetwork(remBase, vars = nodeVars,
                               default = "graphicalVAR",
                               idvar = "subjB",
                               dayvar = "blockAssessmentDay",
                               dayBeepNumvar = "daydayBeepNumNum")

#some layout formatting
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
TL <- averageLayout(remNet_base$graph$temporal, conNet_base$graph$temporal)

#plotting the temporal networks next to one another
#we find that the temporal relations seem quite spurious
plot(conNet_base, graph = "temporal",
     title = "Controls - Baseline",
     nodeNames = nodeVars,
     groups = groups_list,
     legend = FALSE,
     labels = c(1:8),
     layout = TL)

plot(remNet_base, graph = "temporal",
     title = "Remitted - Baseline",
     nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.7,
     labels = c(1:8),
     layout = TL)

#plotting the contemporaneous networks
CL <- averageLayout(remNet_base$graph$contemporaneous, conNet_base$graph$contemporaneous)

plot(conNet_base, graph = "contemporaneous",
     nodeNames = nodeVars,
     groups = groups_list,
     legend = FALSE,
     labels = c(1:8),
     layout = CL)

plot(remNet_base, graph = "contemporaneous",
     nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.7,
     labels = c(1:8),
     layout = CL)

#case-dropping bootstrap (also takes up to 7 hours)
if(FALSE){
  test_boot3 <- bootnet(remNet_base, boots = 500, nCores = 10, type = "case", statistics = c("strength", "expectedInfluence", "edge")) 
  save(test_boot3, file = "test_boot2_onlyRumPANAs_combined.rda")
} else {
  load("test_boot2_onlyRumPANAs_individually.rda")
}

corStability(test_boot)


# load("test_boot2_onlyRumPANAs_combined.rda")




### use Clique Percolation
W <- qgraph(n1$graph) 

thresholds <- cpThreshold(W, method = "weighted", k.range = 3,       
                          I.range = c(seq(0.20, 0.01, by = -0.01)), 
                          threshold = c("largest.components.ratio","chi")); thresholds

results <- cpAlgorithm(W, k = 3, method = "weighted", I = 0.12)

g3 <- cpColoredGraph(W, list.of.communities = results$list.of.communities.numbers, layout=L, theme='colorblind',
                     color=c('#a8e6cf', '#ff8b94', '#ffd3b6', '#444444'), labels=names, vsize=6, cut=0, border.width=1.5, 
                     border.color='black', nodeNames = longnames,legend.cex=.35,
                     edge.width = 1, title ="PTSD communities based on Clique Percolation")
dev.off()




####################################################### time-varying mgm ##############################################################

#the nodes
node_cols <- c('ruminating', 'stickiness', 'wakeful', 'down', 'satisfied',
               'irritated', 'energetic', 'restless', 'anxious', 'listless', 
               'distracted', 'posIntensity', 'negIntensity', "sleepQuality",
               "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany", "daydayBeepNumNum", "phaseAssessmentDay")

# node_cols <- c('ruminating', 'stickiness', 'wakeful', 'down', 'satisfied',
#                'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
#                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
#                'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
#                "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany")

#their types (g = Gaussian, c = categorical)
type_list <- c("g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
                "c", "c", "c", "c")

#putting them in (self-defined) groups
group_list <- list(NegativeAffect = c(4,6,8,9), PositiveAffect = c(3,5,7),
                    Cognition = c(1,2,15,16,17), OtherNegative = c(10,11),
                    MoodReactivity = c(12,13), Sleep = c(14), Social = c(18))

groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53", "#0558ff", "#B94B7B")

#the number of levels per node
level_list <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 3, 3, 3, 2)

#b. Estimating the network
#First we need to deal with missing data
#one approach would be to only work with complete cases:
data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[,node_cols]
# data_copy[which(is.na(data_copy$companyPleasant)),]$companyPleasant <- 0
# data_copy[which(is.na(data_copy$alonePleasant)),]$alonePleasant <- 0

data_copy <- data_copy[complete.cases(data_copy), ]




mvar_mod <- mvar(as.matrix(data_copy),
                 type = type_list,
                 level = level_list,
                 dayBeepNumvar = 1:10,
                 dayvar = 1:10,
                 lambdaSel = "EBIC",
                 lags = c(1))






