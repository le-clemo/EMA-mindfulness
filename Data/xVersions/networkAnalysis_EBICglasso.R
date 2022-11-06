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
library(mlVAR)
library(reshape)
library(viridis)
library(lm.beta)
library(huge)

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


meanResponseRate_block <- mean(responses_block$responseRate) #the mean response rate is ~67.6%
length(unique(responses_block[which(responses_block$responseRate >= meanResponseRate_block),]$subject)) #20
length(unique(responses_block[which(responses_block$responseRate >= 0.6),]$subject)) #26
length(unique(responses_block[which(responses_block$responseRate >= 0.5),]$subject)) #33

#removing participants with a response rate lower than 60%
pp <- unique(responses_block[which(responses_block$responseRate >= 0.6),]$subject)
data <- data[which(data$subject %in% pp),]



############################################# All variables - MGM ###################################################

#1. Network Structure Estimation
#a. Node selection
#Here we simply include all metric variables that were found to be substantial in any of our GAMs
# (but PA and NA are not included as aggregate measures but split into their individual parts)
#plus some categorical variables

#the nodes
node_cols <- c('ruminating', 'stickiness', 'wakeful', 'down', 'satisfied',
               'irritated', 'energetic', 'restless', 'anxious', 'listless', 
               'distracted', 'posIntensity', 'negIntensity', "sleepQuality")

# node_cols <- c('ruminating', 'stickiness', 'wakeful', 'down', 'satisfied',
#                'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
#                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
#                'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
#                "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany")

#their types (g = Gaussian, c = categorical)
types_list <- c("g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g")

#putting them in (self-defined) groups
groups_list <- list(NegativeAffect = c(4,6,8,9), PositiveAffect = c(3,5,7),
                    Cognition = c(1,2), OtherNegative = c(10,11),
                    MoodReactivity = c(12,13), Sleep = c(14))

groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53", "#0558ff")

#the number of levels per node
levels_list <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

#b. Estimating the network
#First we need to deal with missing data
#one approach would be to only work with complete cases:
data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[,node_cols]
# data_copy[which(is.na(data_copy$companyPleasant)),]$companyPleasant <- 0
# data_copy[which(is.na(data_copy$alonePleasant)),]$alonePleasant <- 0

data_copy <- data_copy[complete.cases(data_copy), ]

#Note however, that we could also use other methods to impute missing data. Bootnet has some options!!!
dat <- data.table::copy(data)
dat <- dat[which(is.na(dat$mindcog_db_non_response)),]
dat <- dat[,node_cols]


#full network

#Another potential issue is that our data is non-normal. Epskamp recommends (a.o.) to use non-paranormal
#transformation (as implemented in the "huge" package)
#dat_t <- huge.npn(data_copy) --> can also be performed directly in estimateNetwork() via the corMethod argument

#next we estimate the network using Mixed graphical modeling (the only option right now for mixed data)
set.seed(1)
fit_EBICglasso <- estimateNetwork(dat, default = "EBICglasso",
                           corMethod = "spearman",
                           # missing = "pairwise",
                           threshold = TRUE)

#plotting the resulting network
plot(fit_EBICglasso,
     nodeNames = node_cols,
     groups = groups_list,
     legend.cex = 0.4,
     layout = "spring")

#plotting centrality per node
centrality(fit_EBICglasso)
centralityPlot(fit_EBICglasso)
# include =c("Degree","Strength",
#            "OutDegree","InDegree",
#            "OutStrength","InStrength"))
clusteringPlot(fit_EBICglasso)
#smallworldness(fit_mgm)

#Stability analysis
#first we bootstrap using case dropping
boot1 <- bootnet(fit_EBICglasso, nBoots = 1000,
                  type = "case", nCores = 6)

plot(boot1)

corStability(boot1)


boot2 <- bootnet(fit_EBICglasso, nBoots = 1000,
                  nCores = 6)

print(boot2)
summary(boot2)

plot(boot2, labels = FALSE,
     order = "sample")

plot(boot2, "edge", plot = "difference",
     onlyNonZero = TRUE, order = "sample")

plot(boot2, "strength")



##################################################### Baseline per group ######################################################
for(g in c("controls", "remitted")){
      datX <- data.table::copy(data[which((data$group==g) & (data$phase=="pre")),])
      datX <- datX[which(is.na(datX$mindcog_db_non_response)),]
      datX <- datX[,node_cols]
      print(length(datX$ruminating))
      
      netModel <- estimateNetwork(datX, default = "ggmModSelect",
                                  corMethod = "spearman",
                                  missing = "pairwise")
      #threshold = TRUE)
      
      plot(netModel,
           title = paste("ggmModSelect", g, "Baseline", sep = " - "),
           nodeNames = node_cols,
           groups = groups_list,
           legend.cex = 0.4,
           layout = "spring")
      
      #centralityPlot(netModel)
}




##################################################### Per group and phase ######################################################
for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){  
    for(p in c("pre", "peri")){
      datX <- data.table::copy(data[which((data$group==g) & (data$phase==p) & 
                                                 (data$intervention==i)),])
      datX <- datX[which(is.na(datX$mindcog_db_non_response)),]
      datX <- datX[,node_cols]
      print(length(datX$ruminating))

      netModel <- estimateNetwork(datX, default = "ggmModSelect",
                                  corMethod = "spearman",
                                  missing = "pairwise")
                                  #threshold = TRUE)
      
      plot(netModel,
           title = paste("ggmModSelect", g, i, p, sep = " - "),
           nodeNames = node_cols,
           groups = groups_list,
           legend.cex = 0.4,
           layout = "spring")
      
      #centralityPlot(netModel)
    }
  }
}















