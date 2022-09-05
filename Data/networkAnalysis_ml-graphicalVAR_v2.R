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
library(parallel)

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
data$subjP <- interaction(data$subject, data$phase, drop = TRUE)

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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
################################################### Permutation Tests ######################################################### 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

extract_cent <- function(cent.table, n, m){
  #function to get a specific centrality measure for a specific node from a centrality table
  cent.val <- cent.table[which((cent.table$node==n) & cent.table$measure==m),]$value
  return(cent.val)
}


NPT <- function(data, nodes, addToFile, permuteBy="nodes", iterations=100, idvar="subjB", dayvar="assessmentDay", beepvar="dayBeepNum",
                nCores=detectCores()-2){
  #if permuteBy = "nodes" this function checks a networks ~robustness by shuffling the nodes per subject (i.e., the columns of the provided data)
  #at each iteration
  #else one has to provide the column name of the variable to be permuted by (e.g., "group", or "phase").
  #Note that the latter case will lead to a comparison test between two networks (e.g. group1 vs group2)
  
  start.time <- Sys.time()
  
  #number of permutations
  perms <- iterations
  nodeVars <- nodes
  
  #centrality measures for contemporaneous and temporal networks, respectively
  measures.cont <- c("Strength", "ExpectedInfluence")
  measures.temp <- c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence")
  measures <- c(measures.cont, measures.temp)
  
  if(hasArg(addToFile)){
    print("Loading existing file...")
    
    xfile <- load(addToFile)
    
    #create a named nested list with centrality measures per node
    testStats <- xfile$testStats
    #and a list for edge weight matrices per permutation
    edgeWeights <- list(Contemporaneous = testStats$EdgeWeights$Contemporaneous, Temporal = testStats$EdgeWeights$Temporal)
    
    #create empty dataframes for with row = permutation number and col = centrality measures
    cent.df <- data.frame(matrix(ncol = length(measures), nrow = perms+1))
    colnames(cent.df) <- measures

    
    
  } else {
    
    #create a named nested list with centrality measures per node
    testStats <- list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list())
    #and a list for edge weight matrices per permutation
    edgeWeights <- list(Contemporaneous = list(), Temporal = list())
    
    #create empty dataframes for with row = permutation number and col = centrality measures
    cent.df <- data.frame(matrix(ncol = length(measures), nrow = perms+1))
    colnames(cent.df) <- measures
    
    #also for real nets only with row = nodes and col = centrality measure
    real.cont.df <- data.frame(matrix(nrow = length(nodeVars), ncol = length(measures.cont)))
    colnames(real.cont.df) <- measures.cont
    rownames(real.cont.df) <- nodeVars
    
    real.temp.df <- data.frame(matrix(nrow = length(nodeVars), ncol = length(measures.temp)))
    colnames(real.temp.df) <- measures.temp
    rownames(real.temp.df) <- nodeVars
    
  }
  
  if(permuteBy == "nodes"){

    #actual data
    real.dat <- data[,c(idvar, dayvar, beepvar, nodeVars)]
    
    testSummary <- list(data = real.dat,
                        net1 = list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                                    Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list())),
                        p_values = list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                                        Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list())))
    
    n_idVars <- length(c(idvar, dayvar, beepvar))
    n_nodeVars <- length(nodeVars)
    
    real.net <- mlVAR(real.dat,
                      vars=nodeVars,
                      estimator="lmer",
                      idvar=idvar,
                      dayvar=dayvar,
                      beepvar=beepvar,
                      lags = 1,
                      temporal = "orthogonal",
                      contemporaneous = "orthogonal",
                      nCores = nCores)
    
    #  # Get mlVAR networks:
    real.cont <- getNet(real.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
    #bet  <- getNet(mlNet, "between", nonsig = "hide", rule = "and")
    real.temp <- getNet(real.net, "temporal", nonsig = "hide")
    
    
    #get all centrality measures
    real.cont.cents <- centralityTable(real.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
    real.temp.cents <- centralityTable(real.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
    
    
    
    #add real centralities
    for(n in nodeVars){
      for(m in measures.cont){
        val <- extract_cent(real.cont.cents, n, m)
        cent.df[1,m] <- val
        real.cont.df[n,m] <- val
        
      }
      for(m in measures.temp){
        val <- extract_cent(real.temp.cents, n, m)
        cent.df[1,m] <- val
        real.temp.df[n,m] <- val
      }
      testStats$Centrality[[n]] <- cent.df
    }
    
    
    #add edge weight matrices
    edgeWeights$Contemporaneous[[1]] <- real.cont
    edgeWeights$Temporal[[1]] <- real.temp
    
    #global strength
    gs.cont.vec <- c(rep(NA, perms+1))
    gs.temp.vec <- c(rep(NA, perms+1))
    gs.cont <- sum(abs(real.cont[which(upper.tri(real.cont))]))
    gs.temp <- sum(abs(real.temp[which(upper.tri(real.temp))]))
    
    gs.cont.vec[1] <- gs.cont
    gs.temp.vec[1] <- gs.temp
    
    #add real stats to testSummary
    testSummary$net1$Contemporaneous$Centrality <- real.cont.df
    testSummary$net1$Temporal$Centrality <- real.temp.df
    testSummary$net1$Contemporaneous$EdgeWeights <- real.cont
    testSummary$net1$Temporal$EdgeWeights <- real.temp
    testSummary$net1$Contemporaneous$GlobalStrength <- gs.cont
    testSummary$net1$Temporal$GlobalStrength <- gs.temp
    
    
    #permutation test statistics
    s_ids <- unique(real.dat[[idvar]])
    
    
    for(i in 1:perms+1){
      print(paste("Permutation", i-1, sep = " "))
      
      #create permuted data set (shuffled node labels per subject)
      perm.dat <- data.frame(matrix(ncol = length(c(idvar, dayvar, beepvar, nodeVars))))
      colnames(perm.dat) <- c(idvar, dayvar, beepvar, nodeVars)
      
      for(s in s_ids){
        #subset for only one subject 
        s_df <- real.dat[which(real.dat[[idvar]]==s),]
        
        #shuffle node lables for subject
        perm.vars <- sample(nodeVars)
        colnames(s_df)[(n_idVars + 1) : (n_nodeVars + n_idVars)] <- perm.vars
        
        #add to permutation data set
        perm.dat <- rbind(perm.dat, s_df)
        
      }

      #fit permutated network
      perm.net <- mlVAR(perm.dat,
                        vars=nodeVars,
                        estimator="lmer",
                        idvar=idvar,
                        dayvar=dayvar,
                        beepvar=beepvar,
                        lags = 1,
                        temporal = "orthogonal",
                        contemporaneous = "orthogonal",
                        nCores = nCores)
      
      # print("Net created")
      
      #  # Get mlVAR networks:
      perm.cont <- getNet(perm.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      perm.temp <- getNet(perm.net, "temporal", nonsig = "hide")
      
      
      
      # par(mfrow = c(1,2))
      # 
      # n3 <- qgraph(perm.temp,
      #              title=paste("Temporal", i, sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
      #              groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
      #              vsize=10, asize=6, curve=0.75, curveAll=T, esize=3)
      # 
      # n4 <- qgraph(perm.cont,
      #              title=paste("Contemporaneous", i, sep = " - "), theme='colorblind', negDashed=FALSE, diag=FALSE,
      #              groups=groups_list, legend.cex=0.6, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
      #              vsize=10, asize=6, curve=0.75, curveAll=T, esize=3)
      # 
      # plot(n3)
      # plot(n4)
      # 
      # print("Nets extracted")
      
      edgeWeights$Contemporaneous[[i]] <- perm.cont
      edgeWeights$Temporal[[i]] <- perm.temp
      
      # print("Edge weights added")
      # print(perm.cont)
      # print(perm.temp)
      
      #global strength
      perm.gs.cont <- sum(abs(perm.cont[which(upper.tri(perm.cont))]))
      perm.gs.temp <- sum(abs(perm.temp[which(upper.tri(perm.temp))]))
      gs.cont.vec[i] <- perm.gs.cont
      gs.temp.vec[i] <- perm.gs.temp
      
      # print("Global Strength added")
      
      #get permuted centrality measures
      perm.cont.cents <- centralityTable(perm.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      perm.temp.cents <- centralityTable(perm.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      
      
      # print("centrality tables created")
      # print(perm.cont.cents)
      # print(perm.temp.cents)
      
      #add permuted centralities
      for(n in nodeVars){
        for(m in measures.cont){
          val <- extract_cent(perm.cont.cents, n, m)
          # print(val)
          cent.df[i,m] <- val
          # print("Measures Cont created")
          
        }
        for(m in measures.temp){
          val <- extract_cent(perm.temp.cents, n, m)
          # print(val)
          cent.df[i,m] <- val
          # print("Measures Temp created")
        }
        # print(cent.df)
        # print(testStats[[n]])
        testStats$Centrality[[n]] <- cent.df
      }
      
      # print(edgeWeights)
      # print(gs.cont.vec)
      # print(gs.temp.vec)
      
      
    }
    
    print("Centralities added")
    
    #dataframe to store centrality measure p-vals
    c.cent.df <- data.frame(matrix(ncol = length(measures.cont), nrow = length(nodeVars)))
    colnames(c.cent.df) <- measures.cont
    rownames(c.cent.df) <- nodeVars
    
    t.cent.df <- data.frame(matrix(ncol = length(measures.temp), nrow = length(nodeVars)))
    colnames(t.cent.df) <- measures.temp
    rownames(t.cent.df) <- nodeVars
    
    for(n in nodeVars){
      # print(paste("++++++++++++++++++", n, "++++++++++++++++++", sep = " "))
      for(m in measures.cont){
        # print(paste("###", m, "###", sep = " "))
        # print(testStats$Centrality[[n]][m])
        if(testStats$Centrality[[n]][1,m] > 0){
          pval <- mean((testStats$Centrality[[n]][m]) >= (testStats$Centrality[[n]][1,m]))
          
        } else if(testStats$Centrality[[n]][1,m] < 0){
          pval <- mean((testStats$Centrality[[n]][m]) <= (testStats$Centrality[[n]][1,m]))
          
        } else {
          pval <- 1
        }
        
        c.cent.df[n,m] <- pval
        # print(pval)
      }
      for(m in measures.temp){
        # print(paste("###", m, "###", sep = " "))
        # print(testStats$Centrality[[n]][m])
        if(testStats$Centrality[[n]][1,m] > 0){
          pval <- mean((testStats$Centrality[[n]][m]) >= (testStats$Centrality[[n]][1,m]))
          
        } else if(testStats$Centrality[[n]][1,m] < 0){
          pval <- mean((testStats$Centrality[[n]][m]) <= (testStats$Centrality[[n]][1,m]))
          
        } else {
          pval <- 1
        }
        
        t.cent.df[n,m] <- pval
        # print(pval)
      }
    }
    
    print("Centrality pvalues added")
    
    #dataframe for edge weight p-vals for contemporaneous and temporal
    c.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
    colnames(c.ew.df) <- nodeVars
    rownames(c.ew.df) <- nodeVars
    
    t.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
    colnames(t.ew.df) <- nodeVars
    rownames(t.ew.df) <- nodeVars
    
    excluded.edges <- c(rep(NA, length(nodeVars)))
    
    print(edgeWeights$Contemporaneous)
    
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
        # print("++++++++++++++++++ Contemporaneous ++++++++++++++++++")
        # print(paste(n, "--", e, sep = " "))
        # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
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
        # print(pval)
        c.ew.df[n,e] <- pval
      } 
      
      
      
      for(e in temp.edges){
        # print("++++++++++++++++++ Temporal ++++++++++++++++++")
        # print(paste(n, "-->", e, sep = " "))
        # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
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
        # print(pval)
        t.ew.df[n,e] <- pval
      }
      
    }
    
    #print("Ew pvalues added")
    
    gs.cont.pval <- mean((gs.cont.vec) >= (gs.cont.vec[1]))
    gs.temp.pval <- mean((gs.temp.vec) >= (gs.temp.vec[1]))
    
   # print("Global Strength pvalues added")
    
    
    testSummary$p_values$Contemporaneous$Centrality <- c.cent.df
    testSummary$p_values$Temporal$Centrality <- t.cent.df
    testSummary$p_values$Contemporaneous$EdgeWeights <- c.ew.df
    testSummary$p_values$Temporal$EdgeWeights <- t.ew.df
    testSummary$p_values$Contemporaneous$GlobalStrength <- gs.cont.pval
    testSummary$p_values$Temporal$GlobalStrength <- gs.temp.pval
    
   # print("testSummary filled --> done")
    
    
    
  } else {
    
    n_idVars <- length(c(idvar, dayvar, beepvar, permuteBy))
    n_nodeVars <- length(nodeVars)
    permuteSet <- unique(data[[permuteBy]])
    
    testSummary <- list(data = data[,c(idvar, permuteBy, dayvar, beepvar, nodeVars)])
    
    #actual data
    testStats <- list(Centrality = list(), Temporal = list(EdgeWeights = list(), GlobalStrength = list()),
                      Contemporaneous = list(EdgeWeights = list(), GlobalStrength = list()), 
                      Difference = list(),
                      p_values = list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                                      Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list())))
    
    #list for edge weight matrices per permutation
    edgeWeights <- list()
    
    #vecs for global strengths
    globalStrengths <- list()
    
   # print("++++++++++++++++++++++++ Initialization done ++++++++++++++++++++++++")
    
    for(g in permuteSet){
      
      testSummary[[g]] <- list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                            Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()))
      
      
      edgeWeights[[g]] <- list()
      
      globalStrengths[[g]] <- list()
      
      real.dat <- data[which(data[[permuteBy]]==g),c(idvar, permuteBy, dayvar, beepvar, permuteBy, nodeVars)]

      real.net <- mlVAR(real.dat,
                        vars=nodeVars,
                        estimator="lmer",
                        idvar=idvar,
                        dayvar=dayvar,
                        beepvar=beepvar,
                        lags = 1,
                        temporal = "orthogonal",
                        contemporaneous = "orthogonal",
                        nCores = nCores)
      
      #  # Get mlVAR networks:
      real.cont <- getNet(real.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      real.temp <- getNet(real.net, "temporal", nonsig = "hide")
      
      #get all centrality measures
      real.cont.cents <- centralityTable(real.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      real.temp.cents <- centralityTable(real.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      
    #  print("++++++++++++++++++++++++ Net created ++++++++++++++++++++++++")
      
      #create empty dataframes for with row = permutation number and col = centrality measures
      cent.df <- data.frame(matrix(ncol = length(measures), nrow = perms+1))
      colnames(cent.df) <- measures
      
      #also for real nets only with row = nodes and col = centrality measure
      real.cont.df <- data.frame(matrix(nrow = length(nodeVars), ncol = length(measures.cont)))
      colnames(real.cont.df) <- measures.cont
      rownames(real.cont.df) <- nodeVars
      
      real.temp.df <- data.frame(matrix(nrow = length(nodeVars), ncol = length(measures.temp)))
      colnames(real.temp.df) <- measures.temp
      rownames(real.temp.df) <- nodeVars
      
      #add real centralities
      for(n in nodeVars){
        for(m in measures.cont){
          val <- extract_cent(real.cont.cents, n, m)
          cent.df[1,m] <- val
          real.cont.df[n,m] <- val
          
        }
        for(m in measures.temp){
          val <- extract_cent(real.temp.cents, n, m)
          cent.df[1,m] <- val
          real.temp.df[n,m] <- val
        }
        testStats$Centrality[[g]][[n]] <- cent.df
      }
      
      
      #add edge weight matrices
      edgeWeights[[g]]$Contemporaneous[[1]] <- real.cont
      edgeWeights[[g]]$Temporal[[1]] <- real.temp
      
      #global strength
      gs.cont <- sum(abs(real.cont[which(upper.tri(real.cont))]))
      gs.temp <- sum(abs(real.temp[which(upper.tri(real.temp))]))
      
      globalStrengths[[g]]$Contemporaneous[[1]] <- gs.cont
      globalStrengths[[g]]$Temporal[[1]] <- gs.temp
      
      testSummary[[g]]$Contemporaneous$Centrality <- real.cont.df
      testSummary[[g]]$Temporal$Centrality <- real.temp.df
      testSummary[[g]]$Contemporaneous$EdgeWeights <- real.cont
      testSummary[[g]]$Temporal$EdgeWeights <- real.temp
      testSummary[[g]]$Contemporaneous$GlobalStrength <- gs.cont
      testSummary[[g]]$Temporal$GlobalStrength <- gs.temp
      
    }
    
  #  print("++++++++++++++++++++++++ Test summaries per net created ++++++++++++++++++++++++")

    #Calculate test statistics (differences between networks)
    #Centrality
    for(n in nodeVars){
      testStats$Difference[[n]] <- data.frame(matrix(ncol = length(measures), nrow = perms+1))
      colnames(testStats$Difference[[n]]) <- measures
      testStats$Difference[[n]][1,] <-  testStats[[permuteSet[2]]][[n]][1,] - testStats[[permuteSet[1]]][[n]][1,]
    }
    
    testSummary$Difference <- list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                                   Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()))
    
    testSummary$Difference$Contemporaneous$Centrality <- testSummary[[permuteSet[2]]]$Contemporaneous$Centrality -
                                                                testSummary[[permuteSet[1]]]$Contemporaneous$Centrality
    
    testSummary$Difference$Temporal$Centrality <- testSummary[[permuteSet[2]]]$Temporal$Centrality -
                                                    testSummary[[permuteSet[1]]]$Temporal$Centrality
    
    
    # #add centrality differences to list
    # testStats$Difference <- list(Temporal = list(), Contemporaneous = list())
    # for(n in nodeVars){
    #   testStats$Difference[[n]] <- testStats[[permuteSet[2]]][[n]] - testStats[[permuteSet[1]]][[n]]
    #   # 
    #   # testSummary$Difference$Centrality[[n]] <- testStats$Difference[[n]][1,]
    # }
    
    
    #add edgeWeight differences to list for later calculation of p-values
    edgeWeights$Difference <- list(Temporal = list(c(rep(NA, perms+1))), Contemporaneous = list(c(rep(NA, perms+1))))
    edgeWeights$Difference$Contemporaneous[[1]] <- edgeWeights[[permuteSet[2]]]$Contemporaneous[[1]] -
                                                  edgeWeights[[permuteSet[1]]]$Contemporaneous[[1]]
    edgeWeights$Difference$Temporal[[1]] <- edgeWeights[[permuteSet[2]]]$Temporal[[1]] -
                                              edgeWeights[[permuteSet[1]]]$Temporal[[1]]
    
    #same with global strengths
    globalStrengths$Difference <- list(Temporal = list(c(rep(NA, perms+1))), Contemporaneous = list(c(rep(NA, perms+1))))
    globalStrengths$Difference$Contemporaneous[[1]] <- globalStrengths[[permuteSet[2]]]$Contemporaneous[[1]] -
                                                        globalStrengths[[permuteSet[1]]]$Contemporaneous[[1]]
    globalStrengths$Difference$Temporal[[1]] <- globalStrengths[[permuteSet[2]]]$Temporal[[1]] -
                                                    globalStrengths[[permuteSet[1]]]$Temporal[[1]]
                                                
    
    #also add to testSummary
    # testSummary$Difference$Centrality <- testStats$Difference
    testSummary$Difference$Contemporaneous$EdgeWeights <- edgeWeights$Difference$Contemporaneous[[1]]
    testSummary$Difference$Temporal$EdgeWeights <- edgeWeights$Difference$Temporal[[1]]
    testSummary$Difference$Contemporaneous$GlobalStrength <- globalStrengths$Difference$Contemporaneous[[1]]
    testSummary$Difference$Temporal$GlobalStrength <- globalStrengths$Difference$Temporal[[1]]
    
    #permutation test statistics
    s_ids <- unique(data[[idvar]])
    
    for(i in 1:perms+1){
      print(paste("Permutation", i-1, sep = " "))
      
      #create permuted data set (shuffled node labels per subject)
      perm.dat <- data.frame(matrix(ncol = length(c(idvar, dayvar, beepvar, permuteBy, nodeVars))))
      colnames(perm.dat) <- c(idvar, dayvar, beepvar, permuteBy, nodeVars)
      
      for(s in s_ids){
        #subset for only one subject 
        s_df <- data[which(data[[idvar]]==s),c(idvar, dayvar, beepvar, permuteBy, nodeVars)]
        
        #shuffle node lables for subject
        perm.group <- sample(permuteSet, 1)
        s_df[[permuteBy]] <- perm.group
        
        #add to permutation data set
        perm.dat <- rbind(perm.dat, s_df)
        
      }
      
      
      for(g in permuteSet){
        
        pg.dat <- perm.dat[which(perm.dat[[permuteBy]]==g),c(idvar, permuteBy, dayvar, beepvar, permuteBy, nodeVars)]
        
        #fit permutated network
        perm.net <- mlVAR(pg.dat,
                          vars=nodeVars,
                          estimator="lmer",
                          idvar=idvar,
                          dayvar=dayvar,
                          beepvar=beepvar,
                          lags = 1,
                          temporal = "orthogonal",
                          contemporaneous = "orthogonal",
                          nCores = nCores)
        
        #print("Net created")
        
        #  # Get mlVAR networks:
        perm.cont <- getNet(perm.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
        perm.temp <- getNet(perm.net, "temporal", nonsig = "hide")
        
      #  print("Nets extracted")
        
        edgeWeights[[g]]$Contemporaneous[[i]] <- perm.cont
        edgeWeights[[g]]$Temporal[[i]] <- perm.temp
        
      #  print("Edge weights added")
        
        #global strength
        perm.gs.cont <- sum(abs(perm.cont[which(upper.tri(perm.cont))]))
        perm.gs.temp <- sum(abs(perm.temp[which(upper.tri(perm.temp))]))
        globalStrengths[[g]]$Contemporaneous[[i]] <- perm.gs.cont
        globalStrengths[[g]]$Temporal[[i]] <- perm.gs.temp
        
      #  print("Global Strength added")
        
        #get permuted centrality measures
        perm.cont.cents <- centralityTable(perm.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
        perm.temp.cents <- centralityTable(perm.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
        
       # print("centrality tables created")
        
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
          testStats[[g]][[n]] <- cent.df
        }
        
      }
      
      for(n in nodeVars){
        testStats$Difference[[n]][i,] <- testStats[[permuteSet[2]]][[n]][i,] - testStats[[permuteSet[1]]][[n]][i,]  
      }
      
     # print("Centralities added")
      
      edgeWeights$Difference$Contemporaneous[[i]] <- edgeWeights[[permuteSet[2]]]$Contemporaneous[[i]] -
                                                      edgeWeights[[permuteSet[1]]]$Contemporaneous[[i]]
      
      edgeWeights$Difference$Temporal[[i]] <- edgeWeights[[permuteSet[2]]]$Temporal[[i]] -
                                               edgeWeights[[permuteSet[1]]]$Temporal[[i]]
      
      
      globalStrengths$Difference$Contemporaneous[[i]] <- globalStrengths[[permuteSet[2]]]$Contemporaneous[[i]] -
                                                          globalStrengths[[permuteSet[1]]]$Contemporaneous[[i]]
      
      globalStrengths$Difference$Temporal[[i]] <- globalStrengths[[permuteSet[2]]]$Temporal[[i]] -
                                                    globalStrengths[[permuteSet[1]]]$Temporal[[i]]
        
    }
    

    
    #dataframe to store centrality measure p-vals
    c.cent.df <- data.frame(matrix(ncol = length(measures.cont), nrow = length(nodeVars)))
    colnames(c.cent.df) <- measures.cont
    rownames(c.cent.df) <- nodeVars
    
    t.cent.df <- data.frame(matrix(ncol = length(measures.temp), nrow = length(nodeVars)))
    colnames(t.cent.df) <- measures.temp
    rownames(t.cent.df) <- nodeVars
    
    for(n in nodeVars){
      # print(paste("++++++++++++++++++", n, "++++++++++++++++++", sep = " "))
      for(m in measures.cont){
        # print(paste("###", m, "###", sep = " "))
        if(testStats$Difference[[n]][1,m] > 0){
          pval <- mean((testStats$Difference[[n]][m]) >= (testStats$Difference[[n]][1,m]))
          
        } else if(testStats$Difference[[n]][1,m] < 0){
          pval <- mean((testStats$Difference[[n]][m]) <= (testStats$Difference[[n]][1,m]))
          
        } else {
          pval <- 1
        }
        
        c.cent.df[n,m] <- pval
        # print(pval)
      }
      for(m in measures.temp){
        # print(paste("###", m, "###", sep = " "))
        if(testStats$Difference[[n]][1,m] > 0){
          pval <- mean((testStats$Difference[[n]][m]) >= (testStats$Difference[[n]][1,m]))
          
        } else if(testStats$Difference[[n]][1,m] < 0){
          pval <- mean((testStats$Difference[[n]][m]) <= (testStats$Difference[[n]][1,m]))
          
        } else {
          pval <- 1
        }
        
        t.cent.df[n,m] <- pval
        # print(pval)
      }
    }
    
   # print("Centrality pvalues added")
    
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
        # print("++++++++++++++++++ Contemporaneous ++++++++++++++++++")
        # print(paste(n, "--", e, sep = " "))
        # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
        for(i in 1:(perms+1)){
          cont.weights[i] <- edgeWeights$Difference$Contemporaneous[[i]][n,e]
        }
        
        if(cont.weights[1] > 0){
          pval <- mean((cont.weights) >= (cont.weights[1]))
          
        } else if(cont.weights[1] < 0){
          pval <- mean((cont.weights) <= (cont.weights[1]))
          
        } else {
          pval <- 1
        }
        # print(pval)
        c.ew.df[n,e] <- pval
      } 
      
      
      
      for(e in temp.edges){
        # print("++++++++++++++++++ Temporal ++++++++++++++++++")
        # print(paste(n, "-->", e, sep = " "))
        # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
        for(i in 1:(perms+1)){
          temp.weights[i] <- edgeWeights$Difference$Temporal[[i]][n,e]
        }
        if(temp.weights[1] > 0){
          pval <- mean((temp.weights) >= (temp.weights[1]))
          
        } else if(temp.weights[1] < 0){
          pval <- mean((temp.weights) <= (temp.weights[1]))
          
        } else {
          pval <- 1
        }
        # print(pval)
        t.ew.df[n,e] <- pval
      }
      
    }
    
  #  print("Ew pvalues added")
    
    gs.cont.pval <- mean((globalStrengths$Difference$Contemporaneous) >= (globalStrengths$Difference$Contemporaneous[[1]]))
    gs.temp.pval <- mean((globalStrengths$Difference$Temporal) >= (globalStrengths$Difference$Temporal[[1]]))
    
  #  print("Global Strength pvalues added")
    
    testSummary$p_values$Contemporaneous$Centrality <- c.cent.df
    testSummary$p_values$Temporal$Centrality <- t.cent.df
    testSummary$p_values$Contemporaneous$EdgeWeights <- c.ew.df
    testSummary$p_values$Temporal$EdgeWeights <- t.ew.df
    testSummary$p_values$Contemporaneous$GlobalStrength <- gs.cont.pval
    testSummary$p_values$Temporal$GlobalStrength <- gs.temp.pval
    
  #  print("testSummary filled --> done")
    
    
  }
  
  testSummary$testStats <- testStats
  testSummary$testStats$EdgeWeights <- list(Temporal = temp.weights, Contemporaneous = cont.weights)
  testSummary$testStats$GlobalStrength <- list(Temporal = gs.temp.vec, Contemporaneous = gs.cont.vec)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Time elapsed:", round(time.taken, 2), sep = " "))
  
  return(testSummary)
}



# +++++++++++++++++++++++++++++++++++++ robustness tests ++++++++++++++++++++++++++++++++++++++++++++++++++

# baseline networks
cont_pre_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre")),], nodes = nodeVars,
                       iterations = 100)

save(cont_pre_robust, file = "cont_pre_robust.rda")

rem_pre_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre")),], nodes = nodeVars,
                                         iterations = 100)
save(rem_pre_robust, file = "rem_pre_robust.rda")


#remitted fantasizing pre / peri
rem_pre_fant_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre") & (data_t$intervention == "fantasizing")),],
                           nodes = nodeVars, iterations = 100)

save(rem_pre_fant_robust, file = "rem_pre_fant_robust.rda")


rem_peri_fant_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "peri") & (data_t$intervention == "fantasizing")),],
                           nodes = nodeVars, iterations = 100)

save(rem_peri_fant_robust, file = "rem_peri_fant_robust.rda")


#remitted mindfulness pre / peri
rem_pre_mind_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre") & (data_t$intervention == "mindfulness")),],
                           nodes = nodeVars, iterations = 100)

save(rem_pre_mind_robust, file = "rem_pre_mind_robust.rda")

rem_peri_mind_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "peri") & (data_t$intervention == "mindfulness")),],
                            nodes = nodeVars, iterations = 100)

save(rem_peri_mind_robust, file = "rem_peri_mind_robust.rda")




#controls fantasizing pre / peri
cont_pre_fant_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre") & (data_t$intervention == "fantasizing")),],
                           nodes = nodeVars, iterations = 100)

save(cont_pre_fant_robust, file = "cont_pre_fant_robust.rda")


cont_peri_fant_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "peri") & (data_t$intervention == "fantasizing")),],
                            nodes = nodeVars, iterations = 100)

save(cont_peri_fant_robust, file = "cont_peri_fant_robust.rda")




#controls mindfulness pre / peri
cont_pre_mind_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre") & (data_t$intervention == "mindfulness")),],
                           nodes = nodeVars, iterations = 100)

save(cont_pre_mind_robust, file = "cont_pre_mind_robust.rda")

cont_peri_mind_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "peri") & (data_t$intervention == "mindfulness")),],
                            nodes = nodeVars, iterations = 100)

save(cont_peri_mind_robust, file = "cont_peri_mind_robust.rda")




# +++++++++++++++++++++++++++++++++++++ Network comparison tests ++++++++++++++++++++++++++++++++++++++++++++++++++

#compare groups at baseline
dat <- data_t[which((data_t$phase=="pre")),]
compare_groups_pre <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "group")
save(compare_groups_pre, file = "compare_groups_pre.rda")


#compare remitted fantasizing pre / peri
dat <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="fantasizing")),]
compare_rem_fant_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "phase", idvar = "subjP")
save(compare_rem_fant_pre_peri, file = "compare_rem_fant_pre_peri.rda")

#compare remitted mindfulness pre / peri
dat <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="mindfulness")),]
compare_rem_mind_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "phase", idvar = "subjP")
save(compare_rem_mind_pre_peri, file = "compare_rem_mind_pre_peri.rda")


#compare controls fantasizing pre / peri
dat <- data_t[which((data_t$group=="controls") & (data_t$intervention=="fantasizing")),]
compare_cont_fant_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "phase", idvar = "subjP")
save(compare_cont_fant_pre_peri, file = "compare_cont_fant_pre_peri.rda")

#compare controls fantasizing pre / peri
dat <- data_t[which((data_t$group=="controls") & (data_t$intervention=="mindfulness")),]
compare_cont_mind_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "phase", idvar = "subjP")
save(compare_cont_mind_pre_peri, file = "compare_cont_mind_pre_peri.rda")







###################### testing ##############################

test <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre")),], nodes = nodeVars,
                       iterations = 2)
