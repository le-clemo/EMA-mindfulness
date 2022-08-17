rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202205")

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


###########################################################################################################
####################################### Hierarchical clustering ###########################################
###########################################################################################################

# metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'wakeful', 'down', 'satisfied',
#                 'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
#                 'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
#                 'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")
# 
# #we run into convergence issues when we include too many nodes. Especially when trying to explore the networks of
# #group + intervention + phase...
# #for that reason we perform hierarchical clustering to find highly correlated variables
# #we have already done this for the mixed effects models. However, because we use much smaller parts of the data per
# #network, we need to simplify even more for network analysis.
# 
# dat_clust <- copy(data)
# dat_clust <- dat_clust[which(is.na(dat_clust$mindcog_db_non_response)),]
# dat_clust <- dat_clust[,metricCols]
# dat_clust <- dat_clust[complete.cases(dat_clust), ]
# 
# goldbricker(dat_clust, p=.01, threshold = 0.5) #doesn't suggest reductions...
# 
# #only excluding sumNA and sumPA
# collin.fnc(dat_clust[,-c(3, 4)])$cnumber #~33.6 --> problematic collinearity
# plot(varclus(as.matrix(dat_clust[,-c(3, 4)])))
# 
# #for the mixed-effects models we only kept the following
# collin.fnc(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])$cnumber #~16.3
# plot(varclus(as.matrix(dat_clust[,-c(5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])))
# 
# #for the network models, we ideally want to keep more of the PA and NA variables, not just their aggregate
# collin.fnc(dat_clust[,-c(3, 4, 12, 14, 16, 17, 19, 22, 23, 24)])$cnumber #~23.4
# plot(varclus(as.matrix(dat_clust[,-c(3, 4, 12, 14, 16, 17, 19, 22, 23, 24)])))
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

#only complete cases
data_copy <- data.table::copy(sc_data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
sc_data <- data_copy[complete.cases(data_copy[nodeVars]),]


#paranormal transformation (because the model assumes normality) --> recommended by Epskamp
#huge.npn() should only be applied to data without missing values! Otherwise creates weird values!!!
data_t <- copy(sc_data)
data_t[,nodeVars] <- huge.npn(data_t[,nodeVars])

nodeVars <- c('ruminating', 'stickiness',
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless',
              'listless', 'distracted',
              'posMax', 'negMax')

#grouping the variables --> for later use in network plotting
groups_list <- list(Rumination = c(1,2), PositiveAffect = c(3,4,5), NegativeAffect = c(6,7,8,9),
                    NegativeEmotions = c(10,11), 
                    Events = c(12,13))#,)#, Sleep = c(11)) , 
# Sleep=c(15))
groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53")#, "#0558ff")

#creating baseline networks per group
for(g in c("controls", "remitted")){
  # Estimate network using multilevel VAR model
  print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
  print(g)
  print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
  #subData <- data_copy[which((data_copy$group==g) & (data_copy$phase=="pre")),]
  
  #subData <- sc_data[which((sc_data$group==g) & (sc_data$phase=="pre")),]
  
  subData <- data_t[which((data_t$group==g) & (data_t$phase=="pre")),]
  
  mlNet <- mlVAR(subData,
               vars=nodeVars,
               estimator="lmer",
               idvar="subject",
               dayvar="assessmentDay",
               beepvar="dayBeepNum",
               lags = 1,
               temporal = "orthogonal",
               contemporaneous = "orthogonal",
               nCores = 10)
  

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
 # bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
  temp <- getNet(mlNet, "temporal", nonsig = "hide")

  L <- averageLayout(cont, temp)

 # pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
  layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
  n1 <- qgraph(cont, layout = L,
               title=paste("mlVAR: Contemporaneous network - Baseline", g, sep = " - "), theme='colorblind', negDashed=FALSE,
               groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
               vsize=10, repulsion=1.1, esize=3)
  n2 <- qgraph(temp, layout = L,
               title=paste("mlVAR: Temporal network - Baseline", g, sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
               groups=groups_list, legend.cex=0.6, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
               vsize=10, asize=8, curve=0.75, curveAll=T, esize=3)

  print("Contemporaneous Network:")
  cent_group1 <- centrality_auto(n1, weighted = FALSE)$node.centrality
  print(cent_group1)
  print(smallworldIndex(n1)$index)
  centralityPlot(n1, weighted = FALSE, labels = nodeVars,
                 include = c("Strength", "ExpectedInfluence"))
  print(centralityTable(n1, weighted = FALSE, labels = nodeVars))

  
  print("Temporal Network:")
  cent_group2 <- centrality_auto(n2, weighted = TRUE)$node.centrality
  print(cent_group2)
  print(smallworldIndex(n2)$index)
  centralityPlot(n2, weighted = TRUE, labels = nodeVars,
                 include = c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence"))
  print(centralityTable(n2, weighted = TRUE, labels = nodeVars))


}

dev.off()

# 
# xData <- data[which(data$dayBeepNum <=10),]

for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){  
    
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    print(g)
    print(i)
    print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
    
      # Estimate network using multilevel VAR model
      
      #subData <- data_copy[which((data_copy$group==g) & (data_copy$phase=="pre")),]
      
      #subData <- sc_data[which((sc_data$group==g) & (sc_data$phase==p) & (sc_data$intervention==i)),]
      
      subData <- data_t[which((data_t$group==g) & (sc_data$intervention==i) & (data_t$phase=="pre")),]
      
      preNet <- mlVAR(subData,
                     vars=nodeVars,
                     estimator="lmer",
                     idvar="subjB",
                     dayvar="assessmentDay",
                     beepvar="dayBeepNum",
                     lags = 1,
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     nCores = 10)
      
      #  # Get mlVAR networks:
      preCont <- getNet(preNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      #bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
      preTemp <- getNet(preNet, "temporal", nonsig = "hide")
      
      
      subData <- data_t[which((data_t$group==g) & (sc_data$intervention==i) & (data_t$phase=="peri")),]
      
      
      
      periNet <- mlVAR(subData,
                      vars=nodeVars,
                      estimator="lmer",
                      idvar="subjB",
                      dayvar="assessmentDay",
                      beepvar="dayBeepNum",
                      lags = 1,
                      temporal = "orthogonal",
                      contemporaneous = "orthogonal",
                      nCores = 10)
      
      #  # Get mlVAR networks:
      periCont <- getNet(periNet, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      #bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
      periTemp <- getNet(periNet, "temporal", nonsig = "hide")
      
      
      # pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
      layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
      
      #plot contemporaneous networks
      L <- averageLayout(preCont, periCont,preTemp, periTemp)
      n1 <- qgraph(preCont, layout = L,
                   title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "), theme='colorblind', negDashed=FALSE,
                   groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                   vsize=10, repulsion=1.1, esize=3)
      
      n2 <- qgraph(periCont, layout = L,
                   title=paste("mlVAR: Contemporaneous network", g, i, "Peri-intervention", sep = " - "), theme='colorblind', negDashed=FALSE,
                   groups=groups_list, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                   vsize=10, repulsion=1.1, esize=3, legend.cex=0.6)
      
      
      #plot temporal networks
     # L <- averageLayout(preTemp, periTemp)

      n3 <- qgraph(preTemp, layout = L,
                   title=paste("mlVAR: Temporal network", g, i, "Baseline", sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
                   groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                   vsize=10, asize=6, curve=0.75, curveAll=T, esize=3)
      
      n4 <- qgraph(periTemp, layout = L,
                   title=paste("mlVAR: Temporal network", g, i, "Peri-intervention", sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
                   groups=groups_list, legend.cex=0.6, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                   vsize=10, asize=6, curve=0.75, curveAll=T, esize=3)
      
      
      print("Contemporaneous Networks:")
      print('Pre')
      cent_measures1 <- centrality_auto(n1, weighted = FALSE)$node.centrality
      print(cent_measures1)
      print(smallworldIndex(n1)$index)
      centralityPlot(n1, weighted = FALSE, labels = nodeVars, scale = "z-scores",
                     include = c("Strength", "ExpectedInfluence"))
      print(centralityTable(n1, weighted = FALSE, labels = nodeVars))
      
      print('Peri')
      cent_measures2 <- centrality_auto(n2, weighted = FALSE)$node.centrality
      print(cent_measures2)
      print(smallworldIndex(n2)$index)
      centralityPlot(n2, weighted = FALSE, labels = nodeVars, scale = "z-scores",
                     include = c("Strength", "ExpectedInfluence"))
      print(centralityTable(n2, weighted = FALSE, labels = nodeVars))
      
      print("Temporal Networks:")
      print('Pre')
      cent_measures3 <- centrality_auto(n3, weighted = TRUE)$node.centrality
      print(cent_measures3)
      print(smallworldIndex(n3)$index)
      centralityPlot(n3, weighted = TRUE, labels = nodeVars, scale = "z-scores",
                     include = c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence"))
      print(centralityTable(n3, weighted = TRUE, labels = nodeVars))
      
      print('Peri')
      cent_measures4 <- centrality(n4, weighted = TRUE)
      print(cent_measures4)
      print(smallworldIndex(n4)$index)
      centralityPlot(n4, weighted = TRUE, labels = nodeVars, scale = "z-scores",
                     include = c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence"))
      print(centralityTable(n4, weighted = TRUE, labels = nodeVars))
  }
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
################################################### Permutation Tests ######################################################### 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

extract_cent <- function(cent.table, n, m){
  #function to get a specific centrality measure for a specific node from a centrality table
  cent.val <- cent.table[which((cent.table$node==n) & cent.table$measure==m),]$value
  return(cent.val)
}


#number of permutations
perms <- 100

#centrality measures for contemporaneous and temporal networks, respectively
measures.cont <- c("Strength", "ExpectedInfluence")
measures.temp <- c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence")
measures <- c(measures.cont, measures.temp)

#actual data
real.dat <- data_t[which((data_t$group=="controls") & (data_t$phase=="pre")),]
real.dat <- real.dat[,c("subject", "assessmentDay", "dayBeepNum", nodeVars)]

real.net <- mlVAR(real.dat,
               vars=nodeVars,
               estimator="lmer",
               idvar="subject",
               dayvar="assessmentDay",
               beepvar="dayBeepNum",
               lags = 1,
               temporal = "orthogonal",
               contemporaneous = "orthogonal",
               nCores = 10)

#  # Get mlVAR networks:
real.cont <- getNet(real.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
#bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
real.temp <- getNet(real.net, "temporal", nonsig = "hide")

#get all centrality measures
real.cont.cents <- centralityTable(real.cont, weighted = FALSE, labels = nodeVars, standardized = FALSE)
real.temp.cents <- centralityTable(real.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)


#create a named nested list with centrality measures per node
testStats <- list()
#and a list for edge weight matrices per permutation
edgeWeights <- list(Contemporaneous = list(), Temporal = list())

#create empty dataframes for with row = permutation number and col = centrality measures
cent.df <- data.frame(matrix(ncol = length(measures), nrow = perms+1))
colnames(cent.df) <- measures


#add real centralities
for(n in nodeVars){
  for(m in measures.cont){
    val <- extract_cent(real.cont.cents, n, m)
    cent.df[1,m] <- val
    
  }
  for(m in measures.temp){
    val <- extract_cent(real.temp.cents, n, m)
    cent.df[1,m] <- val
  }
  testStats[[n]] <- cent.df
}

#add edge weight matrices
edgeWeights$Contemporaneous[[1]] <- cont
edgeWeights$Temporal[[1]] <- temp


#permutation test statistics
for(i in 1:perms+1){
  print(paste("Permutation", i, sep = " "))
  #create permuted data set (shuffled node labels)
  perm.dat <- copy(real.dat)
  perm.vars <- sample(nodeVars)
  colnames(perm.dat)[4:(length(nodeVars)+3)] <- perm.vars
  
  #fit permutated network
  perm.net <- mlVAR(perm.dat,
                    vars=nodeVars,
                    estimator="lmer",
                    idvar="subject",
                    dayvar="assessmentDay",
                    beepvar="dayBeepNum",
                    lags = 1,
                    temporal = "orthogonal",
                    contemporaneous = "orthogonal",
                    nCores = 10)
  
  #  # Get mlVAR networks:
  perm.cont <- getNet(perm.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
  perm.temp <- getNet(perm.net, "temporal", nonsig = "hide")
  
  edgeWeights$Contemporaneous[[i]] <- perm.cont
  edgeWeights$Temporal[[i]] <- perm.temp
  
  #get permuted centrality measures
  perm.cont.cents <- centralityTable(perm.cont, weighted = FALSE, labels = nodeVars, standardized = FALSE)
  perm.temp.cents <- centralityTable(perm.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
  
  #add permuted centralities
  for(n in nodeVars){
    for(m in measures.cont){
      val <- extract_cent(perm.cont.cents, n, m)
      cent.df[i,m] <- val
      
    }
    for(m in measures.temp){
      val <- extract_cent(perm.temp.cents, n, m)
      cent.df[i,m] <- val
    }
    testStats[[n]] <- cent.df
  }
  
}

#dataframe to store centrality measure p-vals
c.df <- data.frame(matrix(ncol = length(measures), nrow = length(nodeVars)))
colnames(c.df) <- measures
rownames(c.df) <- nodeVars

for(n in nodeVars){
  print(paste("++++++++++++++++++", n, "++++++++++++++++++", sep = " "))
  for(m in measures){
    print(paste("###", m, "###", sep = " "))
    print(testStats[[n]][m])
    if(testStats[[n]][1,m] > 0){
      pval <- mean((testStats[[n]][m]) >= (testStats[[n]][1,m]))
      
    } else if(testStats[[n]][1,m] < 0){
      pval <- mean((testStats[[n]][m]) <= (testStats[[n]][1,m]))
      
    } else {
      pval <- 1
    }
    
    c.df[n,m] <- pval
    print(pval)
  }
}


#dataframe for edge weight p-vals for contemporaneous and temporal
c.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
colnames(c.ew.df) <- nodeVars
rownames(c.ew.df) <- nodeVars

t.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
colnames(t.ew.df) <- nodeVars
rownames(t.ew.df) <- nodeVars

excluded.edges <- c(rep(NA, length(nodeVars)))
j <- 1
for(n in nodeVars){
  temp.weights <- c(rep(NA, perms+1))
  temp.edges <- nodeVars[nodeVars %nin% excluded.edges]
  

  #now we add the current node to the excluded edges because in contemporaneous nets we do not have self-loops
  excluded.edges[j] <- n
  
  cont.weights <- c(rep(NA, perms+1))
  cont.edges <- nodeVars[nodeVars %nin% excluded.edges]
  
  j <- j + 1
  
  for(e in cont.edges){
    print("++++++++++++++++++ Contemporaneous ++++++++++++++++++")
    print(paste(n, "--", e, sep = " "))
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
    for(i in 1:(perms+1)){
    cont.weights[i] <- edgeWeights$Contemporaneous[[i]][n,e]
    }
    
    if(cont.weights[1] > 0){
      pval <- mean((cont.weights) >= (cont.weights[1]))
      
    } else if(cont.weights[1] < 0){
      pval <- mean((cont.weights) <= (cont.weights[1]))
      
    } else {
      pval <- 1
    }
    print(pval)
    c.ew.df[n,e] <- pval
  } 
  
  
  for(e in temp.edges){
    print("++++++++++++++++++ Temporal ++++++++++++++++++")
    print(paste(n, "-->", e, sep = " "))
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
    for(i in 1:(perms+1)){
    temp.weights[i] <- edgeWeights$Temporal[[i]][n,e]
    }
    if(temp.weights[1] > 0){
      pval <- mean((temp.weights) >= (temp.weights[1]))
      
    } else if(temp.weights[1] < 0){
      pval <- mean((temp.weights) <= (temp.weights[1]))
      
    } else {
      pval <- 1
    }
    print(pval)
    t.ew.df[n,e] <- pval
  }
}



# 
# cent.df <- data.frame(matrix(ncol = length(measures), nrow = length(nodeVars)))
# colnames(cent.df) <- measures
# rownames(cent.df) <- nodeVars
# 
# 
# #add real centralities
# for(n in nodeVars){
#   for(m in measures.cont){
#     val <- extract_cent(real.cont.cents, n, m)
#     print(val)
#     cent.df[n,m] <- val
#     
#   }
#   for(m in measures.temp){
#     val <- extract_cent(real.temp.cents, n, m)
#     cent.df[n,m] <- val
#   }
#   
# }
# cent.vals[[1]] <- cent.df
# 
# 






