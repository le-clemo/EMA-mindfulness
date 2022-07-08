rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")
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
pp <- unique(responses_block[which(responses_block$responseRate >= 0.6),]$subject)
data <- data[which(data$subject %in% pp),]


# 
# node_cols <- c('ruminating', 'stickiness', 'wakeful', 'down', 'satisfied',
#                 'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
#                 'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
#                 'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
#                "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany")
# #"thinkingOf" removed --> not enought data for remitted-mindfulness-peri
# 
# types_list <- c("g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
#               "g", "g", "g", "g", "g", "g", "c", "c", "c", "c")
# 
# groups_list <- list(NegativeAffect = c(4,6,8,9), PositiveAffect = c(3,5,7),
#                     Cognition = c(1,2,23,24,25), OtherNegative = c(10,11,13,17,18),
#                     OtherPositive = c(12,14,15,16), Sleep = c(19,20,21,22), Social = c(26))
# groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53", "#0558ff", "#B94B7B")
# 
# levels_list <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
# 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 2)

# data_copy <- data.table::copy(data)
# data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
# data_copy <- data_copy[,node_cols]


########################## Following Borsboom et al (2021) ################################
metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'wakeful', 'down', 'satisfied',
                'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
                'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")

#we run into convergence issues when we include too many nodes. Especially when trying to explore the networks of
#group + intervention + phase...
#for that reason we perform hierarchical clustering to find highly correlated variables
#we have already done this for the mixed effects models. However, because we use much smaller parts of the data per
#network, we need to simplify even more for network analysis.

dat_clust <- copy(data)
dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
dat_clust <- dat_clust[,metricCols]
dat_clust <- dat_clust[complete.cases(dat_clust), ]

goldbricker(dat_clust, p=.01, threshold = 0.5) #doesn't suggest reductions...

#only excluding sumNA and sumPA
collin.fnc(dat_clust[,-c(3, 4)])$cnumber #~33.6 --> problematic collinearity
plot(varclus(as.matrix(dat_clust[,-c(3, 4)])))

#for the mixed-effects models we only kept the following
collin.fnc(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])$cnumber #~16.3
plot(varclus(as.matrix(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])))

#for the network models, we ideally want to keep more of the PA and NA variables, not just their aggregate
collin.fnc(dat_clust[,-c(3, 4, 12, 14, 16, 17, 19, 22, 23, 24)])$cnumber #~23.4
plot(varclus(as.matrix(dat_clust[,-c(3, 4, 12, 14, 16, 17, 19, 22, 23, 24)])))

#for PA we might want to combine wakeful and energetic; for NA restless and anxious as well as down and irritated.
#Potentially, we may also combine ruminating and stickiness
data$energeticWakeful <- NA
data$downIrritated <- NA
data$anxiousRestless <- NA
for(row in 1:nrow(data)){
  if((!is.na(data$wakeful[row])) & (!is.na(data$energetic[row]))){
    data$energeticWakeful[row] <- (data$wakeful[row] + data$energetic[row])/2
  }
  if((!is.na(data$down[row])) & (!is.na(data$irritated[row]))){
    data$downIrritated[row] <- (data$down[row] + data$irritated[row])/2
  }
  if((!is.na(data$restless[row])) & (!is.na(data$anxious[row]))){
    data$anxiousRestless[row] <- (data$anxious[row] + data$restless[row])/2
  }
}

metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA',
                'wakeful', 'satisfied', 'energetic', 'energeticWakeful',
                'down', 'irritated', 'restless', 'anxious', 'downIrritated', 'anxiousRestless',
                'listless', 'distracted',
                'posIntensity', 'negIntensity',
                'sleepQuality')

dat_clust <- copy(data)
dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
dat_clust <- dat_clust[,metricCols]
dat_clust <- dat_clust[complete.cases(dat_clust), ]

collin.fnc(dat_clust[,-c(3, 4, 5, 7, 9, 10, 11, 12)])$cnumber #~17.7
plot(varclus(as.matrix(dat_clust[,-c(3, 4, 5, 7, 9, 10, 11, 12)])))

nodeVars <- c('ruminating', 'stickiness','energeticWakeful', 'satisfied',
              'downIrritated', 'anxiousRestless',
              'posIntensity', 'negIntensity',
              'listless', 'distracted', 'sleepQuality')

dat_clust <- copy(data)
dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
dat_clust <- dat_clust[,nodeVars]
dat_clust <- dat_clust[complete.cases(dat_clust), ]

goldbricker(dat_clust) #doesn't suggest reductions...


nodeVars <- c('ruminating', 'stickiness', 'wakeful','energetic', 'energeticWakeful', 'satisfied',
              'down', 'irritated', 'restless', 'anxious', 'downIrritated', 'anxiousRestless',
              'posIntensity', 'negIntensity',
              'listless', 'distracted', 'sleepQuality')

scale.vars <- c(rep(NA, length(nodeVars)*3))
i = 0
for(v in nodeVars){
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

#the variable downIrritated is still causing issues. We can try with just one of the two variables. We opt for down.
# nodeVars <- c('ruminating', 'stickiness',
#               'wakeful','energetic', 'energeticWakeful', 'satisfied',
#               'down', 'irritated', 'restless', 'anxious','downIrritated', 'anxiousRestless',
#               'posIntensity', 'negIntensity',
#               'listless', 'distracted', 'sleepQuality')
# 
# diffVars <- c('ruminating_diff', 'stickiness_diff', 'wakeful_diff', 'energetic_diff', 'energeticWakeful_diff', 'satisfied_diff',
#               'down_diff', 'irritated_diff', 'restless_diff', 'anxious_diff','downIrritated_diff', 'anxiousRestless_diff',
#               'posIntensity_diff', 'negIntensity_diff',
#               'listless_diff', 'distracted_diff', 'sleepQuality_diff')

nodeVars <- c('ruminating',
              'energeticWakeful', 'satisfied',
              'down', 'anxiousRestless')

diffVars <- c('ruminating_diff', 'energeticWakeful_diff', 'satisfied_diff',
              'down_diff', 'restless_diff', 'anxious_diff', 'anxiousRestless_diff',
              'posIntensity_diff', 'negIntensity_diff',
              'listless_diff', 'distracted_diff')

data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[complete.cases(data_copy[nodeVars]),]


sc_data <- copy(data_copy)
sc_data[scale.vars] <- scale(data_copy[scale.vars])


#paranormal transformation (because the model assumes normality) --> recommended by Epskamp
#huge.npn() should only be applied to data without missing values! Otherwise creates weird values!!!
data_t <- copy(data_copy)
data_t[,nodeVars] <- huge.npn(data_t[,nodeVars])

groups_list <- list(Rumination = c(1), PositiveAffect = c(2,3), NegativeAffect = c(4,5))
                    #OtherNegative = c(6))#, MoodReactivity = c(8))#, Sleep = c(11)) , 
                   # Sleep=c(15))
groups_colors <- c("#d60000", "#149F36", "#53B0CF")#, "#f66a6a")#, "#72CF53")#, "#0558ff")

#creating baseline networks per group
for(g in c("controls", "remitted")){
  # Estimate network using multilevel VAR model
  
  #subData <- data_copy[which((data_copy$group==g) & (data_copy$phase=="pre")),]
  
  subData <- sc_data[which((sc_data$group==g) & (sc_data$phase=="pre")),]
  
  #subData <- data_t[which((data_t$group==g) & (data_t$phase=="pre")),]
  
  mlNet <- mlVAR(subData,
               vars=nodeVars,
               estimator="lmer",
               idvar="subject",
               dayvar="assessmentDay",
               beepvar="dayBeepNum",
               lags = 1,
               temporal = "orthogonal",
               contemporaneous = "orthogonal",
               nCores = 6)
  

  # gNet <- estimateNetwork(subData,
  #                 default = "graphicalVAR",
  #                 vars = diffVars,
  #                 idvar = "subject",
  #                 dayvar = "assessmentDay",
  #                 beepvar = "dayBeepNum",
  #                 lags = 1)

  # this is how we can save the object after the estimation; careful, over 100mb
  # save(res, file=paste0(datapath, "network_orthogonal.RData"))
  
  # you can later load it via
  # load(paste0(datapath, "network_orthogonal.RData"))
  
  
 #  # Get mlVAR networks:
  cont <- getNet(mlNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
  bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
  temp <- getNet(mlNet, "temporal", nonsig = "hide")

  L <- averageLayout(cont, temp)

 # pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
  layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
  n1 <- qgraph(cont, layout = L,
               title=paste("mlVAR: Contemporaneous network - Baseline", g, sep = " - "), theme='colorblind', negDashed=FALSE,
               groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:5),
               vsize=6, repulsion=1.1, esize=3)
  n2 <- qgraph(temp, layout = L,
               title=paste("mlVAR: Temporal network - Baseline", g, sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
               groups=groups_list, legend.cex=0.5, legend=TRUE, nodeNames = nodeVars, labels=c(1:5),
               vsize=6, asize=6, curve=0.75, curveAll=T, esize=3)

  
  # plot(gNet, graph = "contemporaneous",
  #      title =paste("graphicalVAR: Contemporaneous network - Baseline", g, sep = " - "),
  #      nodeNames = diffVars,
  #      groups = groups_list,
  #      legend.cex = 0.6,
  #      labels = c(1:13),
  #      layout = "spring")
  # 
  # plot(gNet, graph = "temporal",
  #      title = paste("graphicalVAR: Temporal network - Baseline", g, sep = " - "),
  #      nodeNames = diffVars,
  #      groups = groups_list,
  #      legend.cex = 0.6,
  #      labels = c(1:13),
  #      layout = "spring")
}

dev.off()



# 
# xData <- data[which(data$dayBeepNum <=10),]

for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){  
    for(p in c("pre", "peri")){
      # Estimate network using multilevel VAR model
      
      #subData <- data_copy[which((data_copy$group==g) & (data_copy$phase=="pre")),]
      
      subData <- sc_data[which((sc_data$group==g) & (sc_data$phase==p) & (sc_data$intervention==i)),]
      
      #subData <- data_t[which((data_t$group==g) & (data_t$phase=="pre")),]
      
      mlNet <- mlVAR(subData,
                     vars=nodeVars,
                     estimator="lmer",
                     idvar="subjB",
                     dayvar="assessmentDay",
                     beepvar="dayBeepNum",
                     lags = 1,
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     nCores = 6)
      
      #  # Get mlVAR networks:
      cont <- getNet(mlNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
      temp <- getNet(mlNet, "temporal", nonsig = "hide")
      
      L <- averageLayout(cont, temp)
      
      # pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
      layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
      n1 <- qgraph(cont, layout = L,
                   title=paste("mlVAR: Contemporaneous network", g, i, p, sep = " - "), theme='colorblind', negDashed=FALSE,
                   groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:7),
                   vsize=6, repulsion=1.1, esize=3)
      n2 <- qgraph(temp, layout = L,
                   title=paste("mlVAR: Temporal network", g, i, p, sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
                   groups=groups_list, legend.cex=0.5, legend=TRUE, nodeNames = nodeVars, labels=c(1:7),
                   vsize=6, asize=6, curve=0.75, curveAll=T, esize=3)
    }
  }
}




# # -------------------------------------------------------------------------
# # --------------- 3. Detrend data -----------------------------------------
# # -------------------------------------------------------------------------
# 
# # some of the variables have trends (i.e. linear or other changes over time)
# # we remove these trends before estimation network structures
# # we work with all 16 variables here that we collected
# # afterwards, we select those we want to estimate networks on
# 
# # Alpha to detrend:
# alpha <- 0.05
# 
# nodeVars <- c('ruminating', 'wakeful', 'down', 'satisfied',
#               'irritated', 'energetic', 'restless', 'anxious', 'stressed', 
#               'distracted', 'posIntensity',
#               'negIntensity')
# 
# # #add dayBeepNum
# # subject_IDs <- unique(data$subject)
# # data$dayBeepNum <- NA
# # for(id in subject_IDs){ #every participant
# #   print(id)
# #   numDays <- max(data[which(data$subject==id),]$assessmentDay) #some participants dropped out after the first block
# #   print(numDays)
# #   for(i in 1:numDays){
# #     day_rows <- which((data$subject == id) & (data$assessmentDay==i)) #row indices of rows associated with respondent    
# #       data[day_rows,]$dayBeepNum <- 1:length(day_rows)
# #   }
# # }
# 
# # #figure out levels (time window per dayBeepNum)
# # data$time <- strftime(data$mindcog_db_open_from, format="%H:%M:%S")
# timesPerBeep <- ddply(data, .(dayBeepNum), summarize,
#                       minTime = min(time),
#                       maxTime = max(time))
# 
# 
# data <- data[which(data$dayBeepNum<=10),]
# 
# 
# # Data frame with empty values for fitted effects (all):
# fitted_all <- expand.grid(
#   beep = seq(min(data$beepNum),max(data$beepNum)),
#   day = seq(min(data$assessmentDay),max(data$assessmentDay))
# )
# 
# # Data frame with empty values for day trends:
# fitted_day <- data.frame(
#   day = seq(min(data$assessmentDay),max(data$assessmentDay))
# )
# 
# # Data frame with empty values for beeps:
# fitted_beep <- data.frame(
#   beep = seq(min(data$beepNum),max(data$beepNum))
# )
# 
# # Data frame to store p-values:
# p_values <- data.frame(
#   var = c("assessmentDay", "beepNum")
# )
# 
# # Also empty data frame list for test statistics:
# testStatistics <- list()
# coefficients <- list()
# stdcoefficients <- list()
# 
# # Make the beep variable factor in dataset:
# data$beepFactor <- factor(data$beepNum, levels = 0:9)
# fitted_all$beepFactor <- factor(fitted_all$beep, levels = 0:9)
# fitted_beep$beepFactor <- factor(fitted_beep$beep, levels = 0:9)
# 
# # # Make day variable for dates:
# # Data5b$date <- as.Date("2020-03-15") + Data5b$day
# # fitted_all$date <- as.Date("2020-03-15") + fitted_all$day
# # fitted_day$date <- as.Date("2020-03-15") + fitted_day$day
# # 
# # # Add the midpoints as time variable:
# # Data5b$midTime <- as.character(factor(Data5b$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
# # Data5b$midTime <- as.POSIXct(paste(Data5b$date,Data5b$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")
# # 
# # fitted_all$midTime <- as.character(factor(fitted_all$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
# # fitted_all$midTime <- as.POSIXct(paste(fitted_all$date,fitted_all$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")
# 
# # Data frame to store detrended data:
# data_detrended <- data
# 
# # Fix curves:
# for (v in seq_along(nodeVars)){
#   formula <- as.formula(paste0(nodeVars[v], " ~ 1 + assessmentDay + factor(dayBeepNum)"))
#   lmRes <- lm(formula, data = data)
#   
#   # Fixed effects:
#   fixed <- coef(lmRes)
#   
#   # make zero if not significant at alpha:
#   p_values[[nodeVars[v]]] <- anova(lmRes)[["Pr(>F)"]][1:2]
#   if (p_values[[nodeVars[v]]][1] > alpha){
#     fixed[2] <- 0
#   }
#   if (p_values[[nodeVars[v]]][2] > alpha){
#     fixed[3:5] <- 0
#   }
#   
#   # Add to DFs:
#   fitted_all[,nodeVars[v]] <- fixed[1] + fixed[2] * fitted_all[["assessmentDay"]]  +  fixed[3] * (fitted_all[["dayBeepNumNum"]] == 1)  + 
#     fixed[4] * (fitted_all[["dayBeepNum"]] == 2) + fixed[5] *  (fitted_all[["dayBeepNum"]] == 3)
#   
#   fitted_day[,nodeVars[v]] <- fixed[1] + fixed[2] * fitted_day[["assessmentDay"]]
#   
#   fitted_beep[,nodeVars[v]] <- fixed[1] + fixed[2] * median(fitted_day[["assessmentDay"]]) +  fixed[3] * (fitted_beep[["dayBeepNum"]] == 1)  + 
#     fixed[4] * (fitted_beep[["dayBeepNum"]] == 2) + fixed[5] *  (fitted_beep[["dayBeepNum"]] == 3)
#   
#   # Detrend data:
#   data_detrended[,nodeVars[v]] <- data[,nodeVars[v]] - (fixed[1] + fixed[2] * data[["assessmentDay"]]  +  fixed[3] * (data[["dayBeepNum"]] == 1)  + 
#                                                           fixed[4] * (data[["dayBeepNum"]] == 2) + fixed[5] *  (data[["dayBeepNum"]] == 3))
#   
#   ids <- rownames(anova(lmRes))
#   testStatistics[[v]] <- cbind(data.frame(var = nodeVars[v], effect = ids), anova(lmRes))
#   
#   coefficients[[v]] <- data.frame(
#     var = nodeVars[v],
#     type = names(coef(lmRes)),
#     coef = coef(lmRes),
#     std = coef(lm.beta(lmRes))
#   )
# }
# 
# 

