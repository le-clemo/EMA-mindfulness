rm(list = ls()) #clean all up

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

length(unique(data[which((data$group=="controls")),]$subject)) #23
length(unique(data[which((data$group=="remitted")),]$subject)) #16

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
############################################################## Scaling data #################################################################### 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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


nodeVars <- c('ruminating', 'stickiness',
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless',
              'listless', 'distracted',
              'posMax', 'negMax', 'sumNA', 'sumPA')

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



extract_cent <- function(cent.table, n, m){
  #function to get a specific centrality measure for a specific node from a centrality table
  if(m %in% unique(cent.table$measure)){
    cent.val <- cent.table[which((cent.table$node==n) & cent.table$measure==m),]$value
    
  } else {cent.val <- 0}
  
  return(cent.val)
}


NPT <- function(data, nodes, filepath, permuteBy="nodes", iterations=100,
                idvar="subjB", dayvar="phaseAssessmentDay", beepvar="dayBeepNum",
                localClustStrength=list(),
                nCores=detectCores()-2){
  
  #if permuteBy = "nodes" this function checks a networks ~robustness by shuffling the nodes per subject (i.e., the columns of the provided data)
  #at each iteration
  #else one has to provide the column name of the variable to be permuted by (e.g., "group", or "phase").
  #Note that the latter case will lead to a comparison test between two networks (e.g. group1 vs group2)
  
  if(missing(filepath)) stop("filepath required --> filepath.rda")
  
  testSummary <- list()
  testStats <- list()
  edgeWeights <- list()
  globalStrengths <- list()
  
  if(length(localClustStrength)>0){
    localStrengths <- list()
    n_local <- length(localClustStrength)
    for(l in 1:n_local){
      localStrengths[[l]] <- list()
    }
  }

  start.time <- Sys.time()
  
  #number of permutations
  perms <- iterations
  nodeVars <- nodes
  n_nodeVars <- length(nodeVars)
  
  #centrality measures for contemporaneous and temporal networks, respectively
  measures.cont <- c("Strength", "ExpectedInfluence")
  measures.temp <- c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence")
  measures <- c(measures.cont, measures.temp)
  
  if(permuteBy=="nodes"){
    permuteSet <- "network"
    n_idVars <- length(c(idvar, dayvar, beepvar))
    
    #actual data
    real.dat <- data[,c(idvar, dayvar, beepvar, nodeVars)]
    
  } else {
    permuteSet <- unique(data[[permuteBy]])
    n_idVars <- length(c(idvar, dayvar, beepvar, permuteBy))
    
    #actual data
    real.dat <- data[,c(idvar, dayvar, beepvar, nodeVars, permuteBy)]
  }
  
  
  if(file.exists(filepath)){
    
    f <- load(filepath)
    
    prev_results <- mget(f)[["permutationResults"]]
    
    #number of permutations saved in file (??minus 1 because it also contains to "true" file)
    
    #add stats of real net(s) to testSummary
    for(list_item in names(prev_results)){
      if(list_item == "p_values"){
        break
      }
      testSummary[[list_item]] <- prev_results[[list_item]]
    }
    
    prev_iter <- length(prev_results$testStats[[permuteSet[1]]]$EdgeWeights$Temporal) #- 1
    
    cent.df <- data.frame(matrix(ncol = length(measures), nrow = prev_iter + perms))
    colnames(cent.df) <- measures
    
    print(paste("# previous iterations:", prev_iter, sep = " "))
    print(paste("# permutations to perform:", perms, sep = " "))
    
    for(g in permuteSet){
      testStats[[g]] <- list(Centrality = list(), EdgeWeights = list(Temporal = list(), Contemporaneous = list()),
                             GlobalStrength = list(Temporal = list(), Contemporaneous = list()))
      
      # print("testStats[[g]] created")
      
      for(n in nodeVars){
        testStats[[g]]$Centrality[[n]] <- cent.df
        
        testStats[[g]]$Centrality[[n]][1:(prev_iter),] <- prev_results$testStats[[g]]$Centrality[[n]]
        
        # print(testStats[[g]]$Centrality[[n]])
      }
      
      edgeWeights[[g]] <- list(Temporal = prev_results$testStats[[g]]$EdgeWeights$Temporal,
                          Contemporaneous = prev_results$testStats[[g]]$EdgeWeights$Contemporaneous)
      
      globalStrengths[[g]] <- list(Temporal = prev_results$testStats[[g]]$GlobalStrength$Temporal,
                              Contemporaneous = prev_results$testStats[[g]]$GlobalStrength$Contemporaneous)
      
      if(length(localClustStrength)>0){
        for(l in 1:length(prev_results$testStats[[g]]$LocalStrength)){
          localStrengths[[l]][[g]] <- list(Temporal = prev_results$testStats[[g]]$LocalStrength[[l]]$Temporal,
                                      Contemporaneous = prev_results$testStats[[g]]$LocalStrength[[l]]$Contemporaneous)
        }
      }
    }
    
    #and a list for edge weight matrices per permutation
    
    
    #create vectors with appropriate size and populate first prev_iter positions
    # gs.cont.vec <- c(rep(NA, (prev_iter + perms)))
    # gs.temp.vec <- c(rep(NA, (prev_iter + perms)))
    # gs.cont.vec[1:(prev_iter)] <- f$testStats$GlobalStrength$Contemporaneous
    # gs.temp.vec[1:(prev_iter)] <- f$testStats$GlobalStrength$Temporal
    
  } else {
    
    testSummary$data <- real.dat
    
    prev_iter = 1 #the real network is the first and only previous iteration
    
    # #create a named nested list with centrality measures per node
    # testStats <- list()
    # #and a list for edge weight matrices per permutation
    # edgeWeights <- list()
    # globalStrengths <- list() #Contemporaneous = list(), Temporal = list()
    
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
    
    # gs.cont.vec <- c(rep(NA, (perms + 1)))
    # gs.temp.vec <- c(rep(NA, (perms + 1)))
    
    for(g in permuteSet){
      print(paste('----', g, '----', sep = ' '))
      testSummary[[g]] <- list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                               Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()))
      
      testStats[[g]] <- list(Centrality = list(), EdgeWeights = list(Temporal = list(), Contemporaneous = list()),
                             GlobalStrength = list(Temporal = list(), Contemporaneous = list()))
      
      
      edgeWeights[[g]] <- list()
      
      globalStrengths[[g]] <- list()
      
      if(length(localClustStrength)>0){
        for(l in 1:length(localClustStrength)){
          localStrengths[[l]][[g]] <- list()
        }
      }

      
      if(permuteSet[1] != "network"){
        real.dat <- data[which(data[[permuteBy]]==g),c(idvar, permuteBy, dayvar, beepvar, permuteBy, nodeVars)]
      }
      
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
      
      # 
      # n1 <- qgraph(real.temp, layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
      #              groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
      #              vsize=10, asize=8, curve=0.5, esize=3)
      # 
      # print(n1)
      
      #get all centrality measures
      real.cont.cents <- centralityTable(real.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      real.temp.cents <- centralityTable(real.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      
      print("++++++++++++++++++++++++ Net created ++++++++++++++++++++++++")
      
      #add real centralities
      for(n in nodeVars){
        testStats[[g]]$Centrality[[n]] <- cent.df
        for(m in measures.cont){
          val <- extract_cent(real.cont.cents, n, m)
          testStats[[g]]$Centrality[[n]][1,m] <- val
          real.cont.df[n,m] <- val
          
        }
        for(m in measures.temp){
          val <- extract_cent(real.temp.cents, n, m)
          testStats[[g]]$Centrality[[n]][1,m] <- val
          real.temp.df[n,m] <- val
        }
        
        #print(cent.df)
        # testStats[[g]]$Centrality[[n]] <- cent.df
      }
      
      print("Centralities added")
      
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
      
      #local strengths
      if(length(localClustStrength)>0){
        for(l in 1:n_local){
          clust <- localClustStrength[[l]]
          contSum <- 0
          tempSum <- 0
          for(cl in clust){
            contSum <- contSum + sum(real.cont[cl, clust]) #clust[which(clust %nin% cl)]])
            tempSum <- tempSum + sum(real.temp[cl, clust]) #[which(clust %nin% cl)]])
          }
          localStrengths[[l]][[g]]$Contemporaneous[[1]] <- contSum
          localStrengths[[l]][[g]]$Temporal[[1]] <- tempSum
          
          testSummary[[g]]$Contemporaneous$LocalStrenghts[[l]] <- contSum
          testSummary[[g]]$Temporal$LocalStrenghts[[l]] <- tempSum
          
        }
      }
    }
      
  }
 
  #permutation test statistics
  s_ids <- unique(real.dat[[idvar]])
  print("Number of subjects:")
  print(length(s_ids))

  
  for(i in (prev_iter+1):(prev_iter+perms)){
    print(paste("Permutation", i-1, sep = " "))
    
    if(permuteSet == "network"){
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
        pg.dat <- copy(perm.dat)
        
      }
      
    } else {
      
      perm.dat <- data.frame(matrix(ncol = length(c(idvar, dayvar, beepvar, permuteBy, nodeVars))))
      colnames(perm.dat) <- c(idvar, dayvar, beepvar, permuteBy, nodeVars)
      
      for(s in s_ids){
        # print(s)
        #subset for only one subject 
        s_df <- data[which(data[[idvar]]==s),c(idvar, dayvar, beepvar, permuteBy, nodeVars)]
        
        #shuffle node lables for subject
        perm.group <- sample(permuteSet, 1)
        
        s_df[[permuteBy]] <- perm.group
        
        #add to permutation data set
        perm.dat <- rbind(perm.dat, s_df)
        
      }
      
      #each group needs to have at least two subjects for mlVAR to run
      n_g1 <- length(unique(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[1]),][[idvar]]))
      n_g2 <- length(unique(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[2]),][[idvar]]))
      
      print(n_g1)
      print(n_g2)
      if(n_g1 < 2){
        print("Too few g1 samples")
        if(n_g1 == 0){
          n <- 2
        } else if(n_g1 == 1){
          n <- 1
        }
        rand.subj <- sample(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[2]),][[idvar]], n)
        perm.dat[which(perm.dat[[idvar]] %in% rand.subj),][[permuteBy]] <- permuteSet[1]
      }  
     
      if(n_g2 < 2){
        print("Too few g2 samples")
        if(n_g2 == 0){
          n <- 2
        } else if(n_g2 == 1){
          n <- 1
        }
        rand.subj <- sample(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[1]),][[idvar]], n)
        perm.dat[which(perm.dat[[idvar]] %in% rand.subj),][[permuteBy]] <- permuteSet[2]
      }
      
      
      s <- perm.dat[which(perm.dat[[permuteBy]]==permuteSet[1]),][[idvar]]
      
      #in case only one group was assigned to all subjects
      sampled.groups <- unique(perm.dat[[permuteBy]])
      sampled.groups <- sampled.groups[!is.na(sampled.groups)]
      # print("This is the sampled group:")
      # print(sampled.groups)
      # print(length(sampled.groups))
      if(length(sampled.groups) < 2){ #to ensure we end up with two groups
        print("Fixing issue")
        y <- permuteSet[which(permuteSet != sampled.groups)]
        print(y)
        random.subj <- sample(s_ids, 1)
        print(random.subj)
        perm.dat[which(perm.dat[[idvar]] == random.subj),][[permuteBy]] <- y
        print(unique(perm.dat[[permuteBy]]))
      }
    }
    
    permuteSet <- permuteSet[!is.na(permuteSet)]

    for(g in permuteSet){
      print(g)
      
      if(permuteSet[1] != "network"){
        pg.dat <- perm.dat[which(perm.dat[[permuteBy]]==g),c(idvar, permuteBy, dayvar, beepvar, permuteBy, nodeVars)]
        pg.dat[[idvar]] <- as.factor(pg.dat[[idvar]])
        # print(unique(pg.dat[[idvar]]))
        # print(levels(pg.dat[[idvar]]))
      }
      
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
      
      # print("Net created")
      
      #  # Get mlVAR networks:
      perm.cont <- getNet(perm.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      perm.temp <- getNet(perm.net, "temporal", nonsig = "hide")
      
       # print("Nets extracted")
      
      edgeWeights[[g]]$Contemporaneous[[i]] <- perm.cont
      edgeWeights[[g]]$Temporal[[i]] <- perm.temp
      
       # print("Edge weights added")
      
      #global strength
      perm.gs.cont <- sum(abs(perm.cont[which(upper.tri(perm.cont))]))
      perm.gs.temp <- sum(abs(perm.temp[which(upper.tri(perm.temp))]))
      globalStrengths[[g]]$Contemporaneous[[i]] <- perm.gs.cont
      globalStrengths[[g]]$Temporal[[i]] <- perm.gs.temp
      
       # print("Global Strength added")
      
      #get permuted centrality measures
      perm.cont.cents <- centralityTable(perm.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      perm.temp.cents <- centralityTable(perm.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      
      # print("centrality tables created")
      
      #add permuted centralities
      for(n in nodeVars){
        # print(n)
        for(m in measures.cont){
          # print(m)
          val <- extract_cent(perm.cont.cents, n, m)
          testStats[[g]]$Centrality[[n]][i,m] <- val
          
        }
        for(m in measures.temp){
          # print(m)
          val <- extract_cent(perm.temp.cents, n, m)
          testStats[[g]]$Centrality[[n]][i,m] <- val
          
        }
      }
      
      # print("Centrality added to testStats")
      
      testStats[[g]]$EdgeWeights <- list(Temporal = edgeWeights[[g]]$Temporal,
                                                 Contemporaneous = edgeWeights[[g]]$Contemporaneous)
      testStats[[g]]$GlobalStrength <- list(Temporal = globalStrengths[[g]]$Temporal,
                                                    Contemporaneous = globalStrengths[[g]]$Contemporaneous)
      

      
      # print("EdgeWeigthts added to testStats")
      
      #local strengths
      if(length(localClustStrength)>0){
        for(l in 1:n_local){
          clust <- localClustStrength[[l]]
          contSum <- 0
          tempSum <- 0
          for(cl in clust){
            contSum <- contSum + sum(perm.cont[cl, clust]) #[which(clust %nin% cl)]])
            tempSum <- tempSum + sum(perm.temp[cl, clust]) #[which(clust %nin% cl)]])
          }
          localStrengths[[l]][[g]]$Contemporaneous[[i]] <- contSum
          localStrengths[[l]][[g]]$Temporal[[i]] <- tempSum
          
          testStats[[g]]$LocalStrengths[[l]] <- list(Temporal = localStrengths[[l]][[g]]$Temporal,
                                                     Contemporaneous = localStrengths[[l]][[g]]$Contemporaneous)
        }
      }
      
    }
    # print(testStats$controls$GlobalStrength)
    
  }
  
  # print('TestStats (controls - ruminating):')
  # print(testStats$controls$Centrality$ruminating)
  
  if(permuteSet[1] != "network"){
    
    print("Calculating difference scores")
    
    diff.df <- data.frame(matrix(ncol = length(measures), nrow = (prev_iter + perms)))
    colnames(diff.df) <- measures
    
    testStats$difference <- list()
    
    p_valBase <- "difference"
    for(n in nodeVars){
      testStats$difference$Centrality[[n]] <- diff.df
      # print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
      # print(paste("Iteration", i, sep = " "))
      for(i in 1:(prev_iter+perms)){
        # print(n)
        # print(testStats$difference[[n]])
        # print('----------------------------------------------------------------------------')
        # print(testStats[[permuteSet[2]]]$Centrality[[n]][i,])
        # print(testStats[[permuteSet[1]]]$Centrality[[n]][i,])
        
        testStats$difference$Centrality[[n]][i,] <- testStats[[permuteSet[2]]]$Centrality[[n]][i,] - testStats[[permuteSet[1]]]$Centrality[[n]][i,]
        
        # print(testStats$difference[[n]][i,])
      }
      # print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      # print(testStats$difference)
    }
      # print("Centralities added")
    for(i in 1:(prev_iter+perms)){
     
      edgeWeights$difference$Contemporaneous[[i]] <- edgeWeights[[permuteSet[2]]]$Contemporaneous[[i]] -
        edgeWeights[[permuteSet[1]]]$Contemporaneous[[i]]
      
      edgeWeights$difference$Temporal[[i]] <- edgeWeights[[permuteSet[2]]]$Temporal[[i]] -
        edgeWeights[[permuteSet[1]]]$Temporal[[i]]
      
      
      globalStrengths$difference$Contemporaneous[[i]] <- globalStrengths[[permuteSet[2]]]$Contemporaneous[[i]] -
        globalStrengths[[permuteSet[1]]]$Contemporaneous[[i]]
      
      globalStrengths$difference$Temporal[[i]] <- globalStrengths[[permuteSet[2]]]$Temporal[[i]] -
        globalStrengths[[permuteSet[1]]]$Temporal[[i]]
      
      #local strengths
      if(length(localClustStrength)>0){
        for(l in 1:n_local){
            localStrengths[[l]]$difference$Contemporaneous[[i]] <- localStrengths[[l]][[permuteSet[2]]]$Contemporaneous[[i]] -
              localStrengths[[l]][[permuteSet[1]]]$Contemporaneous[[i]]
            
            localStrengths[[l]]$difference$Temporal[[i]] <- localStrengths[[l]][[permuteSet[2]]]$Temporal[[i]] -
              localStrengths[[l]][[permuteSet[1]]]$Temporal[[i]]

        }
      }
      
    }
    
    
    # print(edgeWeights)
    # print(globalStrengths)
  
  } else { p_valBase <- "network"}
  
  
  testStats[[p_valBase]]$EdgeWeights <- list(Temporal = edgeWeights[[p_valBase]]$Temporal,
                                             Contemporaneous = edgeWeights[[p_valBase]]$Contemporaneous)
  testStats[[p_valBase]]$GlobalStrength <- list(Temporal = globalStrengths[[p_valBase]]$Temporal,
                                                Contemporaneous = globalStrengths[[p_valBase]]$Contemporaneous)
  
  #local strengths
  if(length(localClustStrength)>0){
    for(l in 1:n_local){
      testStats[[p_valBase]]$LocalStrengths[[l]] <- list(Temporal = localStrengths[[l]][[p_valBase]]$Temporal,
                                                         Contemporaneous = localStrengths[[l]][[p_valBase]]$Contemporaneous)
    }
  }
  
  #dataframe to store centrality measure p-vals
  c.cent.df <- data.frame(matrix(ncol = length(measures.cont), nrow = length(nodeVars)))
  colnames(c.cent.df) <- measures.cont
  rownames(c.cent.df) <- nodeVars
  
  t.cent.df <- data.frame(matrix(ncol = length(measures.temp), nrow = length(nodeVars)))
  colnames(t.cent.df) <- measures.temp
  rownames(t.cent.df) <- nodeVars
  
  #dataframe for edge weight p-vals for contemporaneous and temporal
  c.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
  colnames(c.ew.df) <- nodeVars
  rownames(c.ew.df) <- nodeVars
  
  t.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
  colnames(t.ew.df) <- nodeVars
  rownames(t.ew.df) <- nodeVars
  
  excluded.edges <- c(rep(NA, length(nodeVars)))
  
  #print(testStats)
  # print(p_valBase)
  
  for(n in nodeVars){
    # print(paste("++++++++++++++++++", n, "++++++++++++++++++", sep = " "))
    # print(testStats[[p_valBase]]$Centrality[[n]])
    for(m in measures.cont){
      # print(paste("###", m, "###", sep = " "))
      # print(testStats[[p_valBase]]$Centrality[[n]][m])
      
      # pval <- mean(abs((testStats[[p_valBase]]$Centrality[[n]][m])) >= (abs(testStats[[p_valBase]]$Centrality[[n]][1,m])))
      
      if(testStats[[p_valBase]]$Centrality[[n]][1,m] > 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) >= (testStats[[p_valBase]]$Centrality[[n]][1,m]))

      } else if(testStats[[p_valBase]]$Centrality[[n]][1,m] < 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) <= (testStats[[p_valBase]]$Centrality[[n]][1,m]))

      } else {
        pval <- 1
      }
      
      c.cent.df[n,m] <- pval
      # print(pval)
    }
    
    # print("Cont. cent pvals calculated")
    
    for(m in measures.temp){
      
      # pval <- mean(abs((testStats[[p_valBase]]$Centrality[[n]][m])) >= (abs(testStats[[p_valBase]]$Centrality[[n]][1,m])))
      # print(paste("###", m, "###", sep = " "))
      # print(testStats$Centrality[[n]][m])
      if(testStats[[p_valBase]]$Centrality[[n]][1,m] > 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) >= (testStats[[p_valBase]]$Centrality[[n]][1,m]))

      } else if(testStats[[p_valBase]]$Centrality[[n]][1,m] < 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) <= (testStats[[p_valBase]]$Centrality[[n]][1,m]))

      } else {
        pval <- 1
      }
      
      t.cent.df[n,m] <- pval
      # print(pval)
    }
  }
  
  # print("Temp cent pvals calculated")
  
  # print(edgeWeights[[p_valBase]]$Contemporaneous)
  
  j <- 1
  for(n in nodeVars){

    #now we add the current node to the excluded edges because in contemporaneous nets we do not have self-loops
    excluded.edges[j] <- n
    
    cont.weights <- c(rep(NA, prev_iter + perms))
    cont.edges <- nodeVars[nodeVars %nin% excluded.edges]
    
    j <- j + 1
    
    for(e in cont.edges){
      # print("++++++++++++++++++ Contemporaneous ++++++++++++++++++")
      # print(paste(n, "--", e, sep = " "))
      # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
      for(i in 1:(prev_iter + perms)){
        cont.weights[i] <- edgeWeights[[p_valBase]]$Contemporaneous[[i]][n,e]
      }
      
      # pval <- mean(abs((cont.weights)) >= (abs(cont.weights[1])))
      
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
    
    #in temporal networks we have directed edges
    temp.weights <- c(rep(NA, prev_iter + perms))
    temp.edges <- nodeVars
    
    for(e in temp.edges){
      # print("++++++++++++++++++ Temporal ++++++++++++++++++")
      # print(paste(n, "-->", e, sep = " "))
      # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
      for(i in 1:(prev_iter + perms)){
        temp.weights[i] <- edgeWeights[[p_valBase]]$Temporal[[i]][n,e]
      }
      
      # pval <- mean(abs((temp.weights)) >= (abs(temp.weights[1])))
      
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
  # print("Ew pvalues added")
  
  gs.cont.vec <- c(rep(NA, prev_iter + perms))
  gs.temp.vec <- c(rep(NA, prev_iter + perms))
  for(i in 1:(prev_iter + perms)){
    gs.cont.vec[i] <- globalStrengths[[p_valBase]]$Contemporaneous[[i]]
    gs.temp.vec[i] <- globalStrengths[[p_valBase]]$Temporal[[i]]
  }
  
  gs.cont.pval <- mean((abs(gs.cont.vec)) >= ((abs(gs.cont.vec[1]))))
  gs.temp.pval <- mean((abs(gs.temp.vec)) >= ((abs(gs.temp.vec[1]))))
  
  #local strengths
  if(length(localClustStrength)>0){
    ls.cont.vec <- list()
    ls.temp.vec <- list()
    
    for(l in 1:n_local){
      ls.cont.vec[[l]] <- c(rep(NA, prev_iter + perms))
      ls.temp.vec[[l]] <- c(rep(NA, prev_iter + perms))
      for(i in 1:(prev_iter + perms)){
        ls.cont.vec[[l]][[i]] <- localStrengths[[l]][[p_valBase]]$Contemporaneous[[i]]
        ls.temp.vec[[l]][[i]] <- localStrengths[[l]][[p_valBase]]$Temporal[[i]]
      }
      
      ls.cont.pval <- mean((abs(ls.cont.vec[[l]])) >= ((abs(ls.cont.vec[[l]][[1]]))))
      ls.temp.pval <- mean((abs(ls.temp.vec[[l]])) >= ((abs(ls.temp.vec[[l]][[1]]))))
      
      testSummary$p_values$Contemporaneous$LocalStrengths[[l]] <- ls.cont.pval
      testSummary$p_values$Temporal$LocalStrengths[[l]] <- ls.temp.pval
    }
  }
  
  # print("Global Strength pvalues added")
  
  testSummary$p_values$Contemporaneous$Centrality <- c.cent.df
  testSummary$p_values$Temporal$Centrality <- t.cent.df
  testSummary$p_values$Contemporaneous$EdgeWeights <- c.ew.df
  testSummary$p_values$Temporal$EdgeWeights <- t.ew.df
  testSummary$p_values$Contemporaneous$GlobalStrength <- gs.cont.pval
  testSummary$p_values$Temporal$GlobalStrength <- gs.temp.pval
 
  
  # testStats[[p_valBase]]$EdgeWeights <- list(Temporal = edgeWeights[[p_valBase]]$Temporal,
  #                                    Contemporaneous = edgeWeights[[p_valBase]]$Contemporaneous)
  # testStats[[p_valBase]]$GlobalStrength <- list(Temporal = globalStrengths[[p_valBase]]$Temporal,
  #                                       Contemporaneous = globalStrengths[[p_valBase]]$Contemporaneous)
  
  testSummary$testStats <- testStats
  
  permutationResults <- copy(testSummary)
  
  save(permutationResults, file = filepath)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Time elapsed:", round(time.taken, 2), sep = " "))
  
  return(permutationResults)
}


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


# #remitted fantasizing pre / peri
# rem_pre_fant_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre") & (data_t$intervention == "fantasizing")),],
#                            nodes = nodeVars, iterations = 200, filepath = "network_permutations/rem_pre_fant_robust.rda",
#                            localClustStrength = c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless"))
# 
# 
# rem_peri_fant_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "peri") & (data_t$intervention == "fantasizing")),],
#                            nodes = nodeVars, iterations = 200, filepath = "network_permutations/rem_peri_fant_robust.rda",
#                            localClustStrength = c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless"))
# 
# 
# #remitted mindfulness pre / peri
# rem_pre_mind_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre") & (data_t$intervention == "mindfulness")),],
#                            nodes = nodeVars, iterations = 100, filepath = "network_permutations/rem_pre_mind_robust.rda",
#                            localClustStrength = c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless"))
# 
# 
# rem_peri_mind_robust <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "peri") & (data_t$intervention == "mindfulness")),],
#                             nodes = nodeVars, iterations = 200, filepath = "network_permutations/rem_peri_mind_robust.rda",
#                             localClustStrength = c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless"))

# 
# #controls fantasizing pre / peri
# cont_pre_fant_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre") & (data_t$intervention == "fantasizing")),],
#                            nodes = nodeVars, iterations = 1, filepath = "network_permutations/cont_pre_fant_robust.rda")
# 
# 
# cont_peri_fant_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "peri") & (data_t$intervention == "fantasizing")),],
#                             nodes = nodeVars, iterations = 2, filepath = "network_permutations/cont_peri_fant_robust.rda")
# 
# 
# #controls mindfulness pre / peri
# cont_pre_mind_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "pre") & (data_t$intervention == "mindfulness")),],
#                            nodes = nodeVars, iterations = 1, filepath = "network_permutations/cont_pre_mind_robust.rda")
# 
# 
# cont_peri_mind_robust <- NPT(data_t[which((data_t$group == "controls") & (data_t$phase == "peri") & (data_t$intervention == "mindfulness")),],
#                             nodes = nodeVars, iterations = 1, filepath = "network_permutations/cont_peri_mind_robust.rda")


# +++++++++++++++++++++++++++++++++++++ Network comparison tests ++++++++++++++++++++++++++++++++++++++++++++++++++

#compare groups at baseline
dat <- data_t[which((data_t$phase=="pre")),]
compare_group_pre <- NPT(dat, nodes = nodeVars, iterations = 200, permuteBy = "group",
                          filepath = "network_permutations/compare_group_pre.rda",
                         localClustStrength = list(c("energetic", "wakeful", "satisfied"), c("down", "irritated", "anxious", "restless")))

# 
# #compare remitted fantasizing pre / peri
# dat <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="fantasizing")),]
# compare_rem_fant_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 200, permuteBy = "phase", idvar = "subjP",
#                                  filepath = "network_permutations/compare_rem_fant_pre_peri.rda")
# 
# #compare remitted mindfulness pre / peri
# dat <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="mindfulness")),]
# compare_rem_mind_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 200, permuteBy = "phase", idvar = "subjP",
#                                  filepath = "network_permutations/compare_rem_mind_pre_peri.rda")
# 
# 
# #compare controls fantasizing pre / peri
# dat <- data_t[which((data_t$group=="controls") & (data_t$intervention=="fantasizing")),]
# compare_cont_fant_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "phase", idvar = "subjP",
#                                   filepath = "network_permutations/compare_cont_fant_pre_peri.rda")
# 
# 
# #compare controls fantasizing pre / peri
# dat <- data_t[which((data_t$group=="controls") & (data_t$intervention=="mindfulness")),]
# compare_cont_mind_pre_peri <- NPT(dat, nodes = nodeVars, iterations = 100, permuteBy = "phase", idvar = "subjP",
#                                   filepath = "network_permutations/compare_cont_mind_pre_peri.rda")
# 



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

# pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths

n1 <- qgraph(cont.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=8, curve=0.5, esize=3, color = groups_colors)
n2 <- qgraph(cont_pre_significant, layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Remitted: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=8, curve=0.5, esize=3)
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


n3 <- qgraph(temp.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3, colors = groups_colors)
n4 <- qgraph(temp.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Remitted: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3)

n5 <- qgraph(bet.nets[[1]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3)
n6 <- qgraph(bet.nets[[2]], layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Remitted: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3)

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
    n1 <- qgraph(preCont, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
                 theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
                 groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6, asize=3, curve=0.5, esize=3)
    
    n2 <- qgraph(periCont, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Peri-intervention", sep = " - ")
                 theme='colorblind', negDashed=FALSE, diag=T,
                 groups=groups_list, legend.cex=1, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6, asize=3, curve=0.5, esize=3)
    
    
    #plot temporal networks
    # L <- averageLayout(preTemp, periTemp)
    
    n3 <- qgraph(preTemp, layout = L, #title=paste("mlVAR: Temporal network", g, i, "Baseline", sep = " - "),
                 theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
                 groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6, asize=3, curve=0.5, esize=3)
    
    n4 <- qgraph(periTemp, layout = L, #title=paste("mlVAR: Temporal network", g, i, "Peri-intervention", sep = " - ")
                 theme='colorblind', negDashed=FALSE, diag=T,
                 groups=groups_list, legend.cex=1, legend=TRUE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
                 vsize=6, asize=3, curve=0.5, esize=3)
    
    
    print("Contemporaneous Networks:")
    print('Pre')
    cent_measures1 <- centrality_auto(n1, weighted = TRUE)$node.centrality
    print(cent_measures1)
    # x <- min(c(cent_measures1$ExpectedInfluence)) - 0.05
    # y <- max(c(cent_measures1$ExpectedInfluence)) + 0.05
    # # print(smallworldIndex(n1)$index)
    # centralityPlot(n1, weighted = TRUE, labels = reducedNodes,
    #                include = c("ExpectedInfluence")) + xlim(x,y)
    # print(centralityTable(n1, weighted = T, labels = reducedNodes, standardized = TRUE))
    #global strength
    print(sum(abs(preCont[which(upper.tri(preCont))])))
    #local connectivity (PA)
    # print(sum(sum(preCont["energetic", c("wakeful", "satisfied")]),
    #           sum(preCont["wakeful", c("energetic", "satisfied")]),
    #           sum(preCont["satisfied", c("wakeful", "energetic")])))
    # #local connectivity (NA)
    # print(sum(sum(preCont["down", c("irritated", "anxious", "restless")]),
    #           sum(preCont["irritated", c("down", "anxious", "restless")]),
    #           sum(preCont["anxious", c("irritated", "down", "restless")]),
    #           sum(preCont["restless", c("irritated", "down", "anxious")])))
    
    print('Peri')
    cent_measures2 <- centrality_auto(n2, weighted = T)$node.centrality
    print(cent_measures2)
    # x <- min(c(cent_measures2$ExpectedInfluence)) - 0.05
    # y <- max(c(cent_measures2$ExpectedInfluence)) + 0.05
    # # print(smallworldIndex(n2)$index)
    # centralityPlot(n2, weighted = T, labels = reducedNodes,
    #                include = c("ExpectedInfluence")) + xlim(x,y)
    # print(centralityTable(n2, weighted = T, labels = nodeVars, standardized = TRUE))
    #global strength
    print(sum(abs(periCont[which(upper.tri(periCont))])))
    #local connectivity (PA)
    # print(sum(sum(periCont["energetic", c("wakeful", "satisfied")]),
    #           sum(periCont["wakeful", c("energetic", "satisfied")]),
    #           sum(periCont["satisfied", c("wakeful", "energetic")])))
    # #local connectivity (NA)
    # print(sum(sum(periCont["down", c("irritated", "anxious", "restless")]),
    #           sum(periCont["irritated", c("down", "anxious", "restless")]),
    #           sum(periCont["anxious", c("irritated", "down", "restless")]),
    #           sum(periCont["restless", c("irritated", "down", "anxious")])))
    
    print("Temporal Networks:")
    print('Pre')
    cent_measures3 <- centrality_auto(n3, weighted = TRUE)$node.centrality
    print(cent_measures3)
    # x <- min(c(cent_measures3$OutExpectedInfluence, cent_measures3$InExpectedInfluence)) - 0.05
    # y <- max(c(cent_measures3$OutExpectedInfluence, cent_measures3$InExpectedInfluence)) + 0.05
    # # print(smallworldIndex(n3)$index)
    # centralityPlot(n3, weighted = TRUE, labels = reducedNodes,# scale = "z-scores",
    #                include = c("InExpectedInfluence", "OutExpectedInfluence")) + xlim(x,y)
    # print(centralityTable(n3, weighted = TRUE, labels = nodeVars))
    #global strength
    print(sum(abs(preTemp[which(upper.tri(preTemp))])))
    #local connectivity (PA)
    # print(sum(sum(preTemp["energetic", c("wakeful", "satisfied")]),
    #           sum(preTemp["wakeful", c("energetic", "satisfied")]),
    #           sum(preTemp["satisfied", c("wakeful", "energetic")])))
    # #local connectivity (NA)
    # print(sum(sum(preTemp["down", c("irritated", "anxious", "restless")]),
    #           sum(preTemp["irritated", c("down", "anxious", "restless")]),
    #           sum(preTemp["anxious", c("irritated", "down", "restless")]),
    #           sum(preTemp["restless", c("irritated", "down", "anxious")])))
    
    print('Peri')
    cent_measures4 <- centrality_auto(n4, weighted = TRUE)$node.centrality
    print(cent_measures4)
    # x <- min(c(cent_measures4$OutExpectedInfluence, cent_measures4$InExpectedInfluence)) - 0.05
    # y <- max(c(cent_measures4$OutExpectedInfluence, cent_measures4$InExpectedInfluence)) + 0.05
    # # print(smallworldIndex(n4)$index)
    # centralityPlot(n4, weighted = TRUE, labels = reducedNodes, #scale = "z-scores",
    #                include = c("InExpectedInfluence", "OutExpectedInfluence")) + xlim(x,y)
    # print(centralityTable(n4, weighted = TRUE, labels = nodeVars, standardized = TRUE))
    #global strength
    print(sum(abs(periTemp[which(upper.tri(periTemp))])))
    #local connectivity (PA)
    # print(sum(sum(periTemp["energetic", c("wakeful", "satisfied")]),
    #           sum(periTemp["wakeful", c("energetic", "satisfied")]),
    #           sum(periTemp["satisfied", c("wakeful", "energetic")])))
    # #local connectivity (NA)
    # print(sum(sum(periTemp["down", c("down", "irritated", "anxious", "restless")]),
    #           sum(periTemp["irritated", c("irritated", "down", "anxious", "restless")]),
    #           sum(periTemp["anxious", c("anxious", "irritated", "down", "restless")]),
    #           sum(periTemp["restless", c("restless", "irritated", "down", "anxious")])))
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

# 
# 
# 
# 
# 
# #remitted fantasizing pre / peri
# rem_pre_fant_reduced <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre") & (data_t$intervention == "fantasizing")),],
#                            nodes = reducedNodes, iterations = 100, filepath = "network_permutations/rem_pre_fant_reduced.rda")
# 
# 
# rem_peri_fant_reduced <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "peri") & (data_t$intervention == "fantasizing")),],
#                             nodes = reducedNodes, iterations = 100, filepath = "network_permutations/rem_peri_fant_reduced.rda")
# 
# 
# #remitted mindfulness pre / peri
# rem_pre_mind_reduced <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "pre") & (data_t$intervention == "mindfulness")),],
#                            nodes = reducedNodes, iterations = 100, filepath = "network_permutations/rem_pre_mind_reduced.rda")
# 
# 
# rem_peri_mind_reduced <- NPT(data_t[which((data_t$group == "remitted") & (data_t$phase == "peri") & (data_t$intervention == "mindfulness")),],
#                             nodes = reducedNodes, iterations = 100, filepath = "network_permutations/rem_peri_mind_reduced.rda")
# 
# 
# 
# #comparison tests
# 
# #compare remitted fantasizing pre / peri
# dat <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="fantasizing")),]
# compare_rem_fant_pre_peri_reduced <- NPT(dat, nodes = reducedNodes, iterations = 100, permuteBy = "phase", idvar = "subjP",
#                                  filepath = "network_permutations/compare_rem_fant_pre_peri_reduced.rda")
# 
# #compare remitted mindfulness pre / peri
# dat <- data_t[which((data_t$group=="remitted") & (data_t$intervention=="mindfulness")),]
# compare_rem_mind_pre_peri_reduced <- NPT(dat, nodes = reducedNodes, iterations = 100, permuteBy = "phase", idvar = "subjP",
#                                  filepath = "network_permutations/compare_rem_mind_pre_peri_reduced.rda")

# 
# #compare controls fantasizing pre / peri
# dat <- data_t[which((data_t$group=="controls") & (data_t$intervention=="fantasizing")),]
# compare_cont_fant_pre_peri_reduced <- NPT(dat, nodes = reducedNodes, iterations = 100, permuteBy = "phase", idvar = "subjP",
#                                   filepath = "network_permutations/compare_cont_fant_pre_peri_reduced.rda")
# 
# 
# #compare controls fantasizing pre / peri
# dat <- data_t[which((data_t$group=="controls") & (data_t$intervention=="mindfulness")),]
# compare_cont_mind_pre_peri_reduced <- NPT(dat, nodes = reducedNodes, iterations = 100, permuteBy = "phase", idvar = "subjP",
#                                   filepath = "network_permutations/compare_cont_mind_pre_peri_reduced.rda")

# colnames(data_t)[colnames(data_t) == 'sumNA'] <- 'NegativeAffect'
# colnames(data_t)[colnames(data_t) == 'sumPA'] <- 'PositiveAffect'
# colnames(data_t)[colnames(data_t) == 'negMax'] <- 'EventUnpleasantness'
# colnames(data_t)[colnames(data_t) == 'posMax'] <- 'EventPleasantness'
# 
# 
# alternativeNodes <- c('Rumination',
#                   'PositiveAffect',
#                   'NegativeAffect',
#                   'EventUnpleasantness',
#                   'Distraction')
# 
# #grouping the variables --> for later use in network plotting
# alt_list <- list(Rumination = c(1), Affect = c(2,3), Other = c(4,5))
# 
# alt_colors <- c("#d60000", "#149F36", "#53B0CF")#, "#72CF53")


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





load("network_permutations/all_peri_fant_final.rda")
all_peri_fant_final <- copy(permutationResults)

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

load("network_permutations/rem_pre_final.rda")
rem_pre_final <- copy(permutationResults)

netAll <- rem_pre_final

sum((netAll$p_values$Temporal$EdgeWeights < 0.025), na.rm = T)
sum((netAll$p_values$Temporal$EdgeWeights != 1), na.rm = T)

netAll <- netAll[["network"]][["Contemporaneous"]][["EdgeWeights"]]

n2 <- qgraph(netAll, layout = L, #title=paste("mlVAR: Contemporaneous network", g, i, "Baseline", sep = " - "),
             theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
             groups=groups_list, legend.cex=1, legend=T, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
             vsize=6, asize=3, curve=0.5, esize=3, color = groups_colors)



net_pvals <- netAll[["p_values"]][["Contemporaneous"]][["EdgeWeights"]]

net <- copy(netAll)
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


x <- seq(5, 15, length=1000)
y <- dnorm(x, mean=10, sd=3)
plot(x, y, type="l", lwd=1)



