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




node_cols <- c('ruminating', 'stickiness', 'wakeful', 'down', 'satisfied',
                'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
                'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup",
               "thoughtsTime", "thoughtsValence", "thoughtsObject", "aloneCompany")
#"thinkingOf" removed --> not enought data for remitted-mindfulness-peri

types_list <- c("g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
              "g", "g", "g", "g", "g", "g", "c", "c", "c", "c")

groups_list <- list(NegativeAffect = c(4,6,8,9), PositiveAffect = c(3,5,7),
                    Cognition = c(1,2,23,24,25), OtherNegative = c(10,11,13,17,18),
                    OtherPositive = c(12,14,15,16), Sleep = c(19,20,21,22), Social = c(26))
groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53", "#0558ff", "#B94B7B")

levels_list <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 2)

data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[,node_cols]
# data_copy[which(is.na(data_copy$companyPleasant)),]$companyPleasant <- 0
# data_copy[which(is.na(data_copy$alonePleasant)),]$alonePleasant <- 0

data_copy <- data_copy[complete.cases(data_copy), ]

# 
# for(val in node_cols){
#   print(class(data_copy[[val]]))
# }

set.seed(1)
fit_mgm <- mgm(data = data_copy,
               type = types_list,
               levels = levels_list, 
               k = 2, 
               lambdaSel = "CV",
               lambdaFolds = 10,
               ruleReg = "AND", overparameterize = TRUE)

int24 <- showInteraction(fit_mgm, int=c(2,3))

int24$parameters
round(fit_mgm$pairwise$wadj, 2)

showInteraction(object = fit_mgm, 
                int = c(1,4))


# -------------------- Making Predictions from Mixed Graphical Models --------------------

pred_mgm <- predict(object = fit_mgm, 
                    data = data_copy,
                    errorCon = c("RMSE", "R2"),
                    errorCat = c("CC", "nCC"))

pred_mgm$errors


# -------------------- Visualizing Mixed Graphical Models --------------------
errors <- c(pred_mgm$errors[1:22,3],
            pred_mgm$errors[23:27,4])

sc <- .8
pdf(paste0(figDir,'Fig_mgm_p4_example.pdf'), width = 7*sc, height = 4*sc)
set.seed(1)
qgraph(fit_mgm$pairwise$wadj, 
       edge.color = fit_mgm$pairwise$edgecolor, 

       nodeNames = node_cols,
       legend = TRUE,
       vsize = 15, 
       esize = 3.5)


qgraph(fit_mgm$pairwise$wadj, 
       layout = 'spring', repulsion = 1.3,
       edge.color = fit_mgm$pairwise$edgecolor, 
       nodeNames = node_cols,
       color = groups_colors, 
       groups = groups_list,
       pie = errors, 
       pieColor = "black",
       legend.mode="style2", legend.cex=.4, main = "All participants",
       vsize = 3.5, esize = 15)



for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){  
    for(p in c("pre", "peri")){
      data_copy <- data.table::copy(data[which((data$group==g) & (data$phase==p) & 
                                                 (data$intervention==i)),])
      data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
      data_copy <- data_copy[,node_cols]
      # data_copy[which(is.na(data_copy$companyPleasant)),]$companyPleasant <- 0
      # data_copy[which(is.na(data_copy$alonePleasant)),]$alonePleasant <- 0
      
      data_copy <- data_copy[complete.cases(data_copy), ]
      
      set.seed(1)
      fit_mgm <- mgm(data = data_copy,
                     type = types_list,
                     levels = levels_list, 
                     k = 2, 
                     lambdaSel = "CV",
                     lambdaFolds = 10,
                     ruleReg = "AND", overparameterize = TRUE)
      
      # int24 <- showInteraction(fit_mgm, int=c(2,3))
      # 
      # int24$parameters
      # round(fit_mgm$pairwise$wadj, 2)
      # 
      # showInteraction(object = fit_mgm, 
      #                 int = c(1,4))
      
      
      # -------------------- Making Predictions from Mixed Graphical Models --------------------
      
      pred_mgm <- predict(object = fit_mgm, 
                          data = data_copy,
                          errorCon = c("RMSE", "R2"),
                          errorCat = c("CC", "nCC"))
      
      # -------------------- Visualizing Mixed Graphical Models --------------------
      errors <- c(pred_mgm$errors[1:22,3],
                  pred_mgm$errors[23:26,4])
      
      sc <- .8
      set.seed(1)
      
      qgraph(fit_mgm$pairwise$wadj, 
             layout = 'spring', repulsion = 1.3,
             edge.color = fit_mgm$pairwise$edgecolor, 
             nodeNames = node_cols,
             color = groups_colors, 
             groups = groups_list,
             pie = errors,
             pieColor = "black",
             legend.mode="style2", legend.cex=.4, 
             vsize = 3.5, esize = 15,
             title = paste(g,i,p, sep = " - "))
      
    }
    
  
  }
  
}


########################## Following Borsboom et al (2021) ################################
# -------------------------------------------------------------------------
# --------------- 3. Detrend data -----------------------------------------
# -------------------------------------------------------------------------

# some of the variables have trends (i.e. linear or other changes over time)
# we remove these trends before estimation network structures
# we work with all 16 variables here that we collected
# afterwards, we select those we want to estimate networks on

# Alpha to detrend:
alpha <- 0.05

# Variables to investigate:
# vars <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18")



nodeVars <- c('ruminating', 'wakeful', 'down', 'satisfied',
              'irritated', 'energetic', 'restless', 'anxious', 'stressed', 
              'distracted', 'posIntensity',
              'negIntensity')

#add dayBeepNum
subject_IDs <- unique(data$subject)
data$dayBeepNum <- NA
for(id in subject_IDs){ #every participant
  print(id)
  numDays <- max(data[which(data$subject==id),]$assessmentDay) #some participants dropped out after the first block
  print(numDays)
  for(i in 1:numDays){
    day_rows <- which((data$subject == id) & (data$assessmentDay==i)) #row indices of rows associated with respondent    
      data[day_rows,]$dayBeepNum <- 1:length(day_rows)
  }
}
#figure out levels (time window per dayBeepNum)
data$time <- strftime(data$mindcog_db_open_from, format="%H:%M:%S")
timesPerBeep <- ddply(data, .(dayBeepNum), summarize,
      minTime = min(time),
      maxTime = max(time))


data <- data[which(data$dayBeepNum<=10),]


# Data frame with empty values for fitted effects (all):
fitted_all <- expand.grid(
  beep = seq(min(data$beepNum),max(data$beepNum)),
  day = seq(min(data$assessmentDay),max(data$assessmentDay))
)

# Data frame with empty values for day trends:
fitted_day <- data.frame(
  day = seq(min(data$assessmentDay),max(data$assessmentDay))
)

# Data frame with empty values for beeps:
fitted_beep <- data.frame(
  beep = seq(min(data$beepNum),max(data$beepNum))
)

# Data frame to store p-values:
p_values <- data.frame(
  var = c("assessmentDay", "beepNum")
)

# Also empty data frame list for test statistics:
testStatistics <- list()
coefficients <- list()
stdcoefficients <- list()

# Make the beep variable factor in dataset:
data$beepFactor <- factor(data$beepNum, levels = 0:9)
fitted_all$beepFactor <- factor(fitted_all$beep, levels = 0:9)
fitted_beep$beepFactor <- factor(fitted_beep$beep, levels = 0:9)

# # Make day variable for dates:
# Data5b$date <- as.Date("2020-03-15") + Data5b$day
# fitted_all$date <- as.Date("2020-03-15") + fitted_all$day
# fitted_day$date <- as.Date("2020-03-15") + fitted_day$day
# 
# # Add the midpoints as time variable:
# Data5b$midTime <- as.character(factor(Data5b$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
# Data5b$midTime <- as.POSIXct(paste(Data5b$date,Data5b$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")
# 
# fitted_all$midTime <- as.character(factor(fitted_all$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
# fitted_all$midTime <- as.POSIXct(paste(fitted_all$date,fitted_all$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")

# Data frame to store detrended data:
data_detrended <- data

# Fix curves:
for (v in seq_along(nodeVars)){
  formula <- as.formula(paste0(nodeVars[v], " ~ 1 + assessmentDay + factor(dayBeepNum)"))
  lmRes <- lm(formula, data = data)
  
  # Fixed effects:
  fixed <- coef(lmRes)
  
  # make zero if not significant at alpha:
  p_values[[nodeVars[v]]] <- anova(lmRes)[["Pr(>F)"]][1:2]
  if (p_values[[nodeVars[v]]][1] > alpha){
    fixed[2] <- 0
  }
  if (p_values[[nodeVars[v]]][2] > alpha){
    fixed[3:5] <- 0
  }
  
  # Add to DFs:
  fitted_all[,nodeVars[v]] <- fixed[1] + fixed[2] * fitted_all[["assessmentDay"]]  +  fixed[3] * (fitted_all[["dayBeepNumNum"]] == 1)  + 
    fixed[4] * (fitted_all[["dayBeepNum"]] == 2) + fixed[5] *  (fitted_all[["dayBeepNum"]] == 3)
  
  fitted_day[,nodeVars[v]] <- fixed[1] + fixed[2] * fitted_day[["assessmentDay"]]
  
  fitted_beep[,nodeVars[v]] <- fixed[1] + fixed[2] * median(fitted_day[["assessmentDay"]]) +  fixed[3] * (fitted_beep[["dayBeepNum"]] == 1)  + 
    fixed[4] * (fitted_beep[["dayBeepNum"]] == 2) + fixed[5] *  (fitted_beep[["dayBeepNum"]] == 3)
  
  # Detrend data:
  data_detrended[,nodeVars[v]] <- data[,nodeVars[v]] - (fixed[1] + fixed[2] * data[["assessmentDay"]]  +  fixed[3] * (data[["dayBeepNum"]] == 1)  + 
                                                          fixed[4] * (data[["dayBeepNum"]] == 2) + fixed[5] *  (data[["dayBeepNum"]] == 3))
  
  ids <- rownames(anova(lmRes))
  testStatistics[[v]] <- cbind(data.frame(var = nodeVars[v], effect = ids), anova(lmRes))
  
  coefficients[[v]] <- data.frame(
    var = nodeVars[v],
    type = names(coef(lmRes)),
    coef = coef(lmRes),
    std = coef(lm.beta(lmRes))
  )
}











nodeVars <- c('ruminating', 'wakeful', 'down', 'satisfied',
              'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless',
              'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posIntensity',
              'negIntensity', "sleepQuality")



# xData <- data[which(is.na(data$mindcog_db_non_response)),]

groups_list <- list(NegativeAffect = c(3,5,7,8), PositiveAffect = c(2,4,6),
                    Cognition = c(1), OtherNegative = c(9,10,12,15),
                    OtherPositive = c(11,13,14), Sleep=c(16))
groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53", "#B94B7B")

# Estimate network using multilevel VAR model
res <- mlVAR(data,
             vars=nodeVars,
             idvar="subject",
             dayvar="assessmentDay",
             beepvar="beepNum",
             lags = 1,
             temporal = "orthogonal",
             contemporaneous = "orthogonal",
             nCores = 8)

# this is how we can save the object after the estimation; careful, over 100mb
# save(res, file=paste0(datapath, "network_orthogonal.RData"))

# you can later load it via
# load(paste0(datapath, "network_orthogonal.RData"))



# Plot :
# names <- c("Relax","Irritable","Worry","Nervous","Future", "Anhedonia","Alone","Social-offline", "Social-online")
# 
# gr <- list('Stress'=c(1:6), 'Social'=c(7:9))

# Get networks:
cont <- getNet(res, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
bet  <- getNet(res, "between", nonsig = "hide", rule = "and")
temp <- getNet(res, "temporal", nonsig = "hide")

L <- averageLayout(cont, temp)

pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
n1 <- qgraph(cont, layout = L,
             title="Contemporaneous network", theme='colorblind', negDashed=FALSE,
             groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:16),
             vsize=6, repulsion=1.1, esize=3)
n2 <- qgraph(temp, layout = L,
             title="Temporal network", theme='colorblind', negDashed=FALSE, diag=FALSE,
             groups=groups_list, legend.cex=0.5, legend=TRUE, nodeNames = nodeVars, labels=c(1:16),
             vsize=6, asize=6, curve=0.75, curveAll=T, esize=3)
dev.off()

# 
# xData <- data[which(data$dayBeepNum <=10),]

for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){  
    for(p in c("pre", "peri")){
      data_detrended_copy <- data.table::copy(data_detrended[which((data_detrended$group==g) &
                                                                     (data_detrended$phase==p) &
                                                                     (data_detrended$intervention==i)),])
      data_detrended_copy <- data_detrended_copy[which(is.na(data_detrended_copy$mindcog_db_non_response)),]
      #data_copy <- data_copy[complete.cases(data_copy), ]
      
      res <- mlVAR(data_detrended_copy,
                   vars=nodeVars,
                   idvar="subject",
                   dayvar="assessmentDay",
                   beepvar="dayBeepNum",
                   lags = 1,
                   # temporal = "orthogonal",
                   # contemporaneous = "orthogonal",
                   nCores = 8)
      
      cont <- getNet(res, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      bet  <- getNet(res, "between", nonsig = "hide", rule = "and")
      temp <- getNet(res, "temporal", nonsig = "hide")
      
      L <- averageLayout(cont, temp)
      
      pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
      layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
      n1 <- qgraph(cont, layout = L,
                   title=paste("Contemporaneous network:",g,i,p, sep=" "), theme='colorblind', negDashed=FALSE,
                   groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:16),
                   vsize=6, repulsion=1.1, esize=3)
      n2 <- qgraph(temp, layout = L,
                   title=paste("Temporal network:",g,i,p, sep=" "), theme='colorblind', negDashed=FALSE, diag=FALSE,
                   groups=groups_list, legend.cex=0.5, legend=TRUE, nodeNames = nodeVars, labels=c(1:16),
                   vsize=6, asize=6, curve=0.75, curveAll=T, esize=3)
    }
  }
}








# #The red rings indicate the proportion of variance explained by neighboring nodes for the Gaussian variables
# dev.off()
# 
# 
# # -------------------- Bootstrap Sampling Distributions --------------------
# 
# set.seed(1)
# res_obj <- resample(object = fit_mgm, 
#                     data = data_copy, 
#                     nB = 50, 
#                     quantiles = c(.05, .95))
# 
# sc <- 1.5
# pdf(paste0(figDir,'Fig_mgm_p4_resampling.pdf'), width = 6*sc, height = 4*sc)
# plotRes(object = res_obj, 
#         quantiles = c(.05, .95), 
#         cex.label = 1.25, 
#         lwd.qtl = 2.5, 
#         cex.mean = .5)
# dev.off()
# 
# 
# # -------------------- Sampling from Mixed Graphical Model --------------------
# 
# # a) General Graph Info
# type <- c("g", "c", "c", "g")
# level <- c(1, 2, 4, 1)
# 
# # b) Define Thresholds
# thresholds <- list()
# thresholds[[1]] <- 0
# thresholds[[2]] <- rep(0, level[2])
# thresholds[[3]] <- rep(0, level[3])
# thresholds[[4]] <- 0
# 
# # c) Define Standard deviation of Gaussians
# sds <- rep(1, 4)
# 
# # d) Define Interaction
# factors <- list()
# factors[[1]] <- matrix(c(1,4,
#                          2,3,
#                          1,2), ncol=2, byrow = T)
# interactions <- list()
# interactions[[1]] <- vector("list", length = 3)
# # 2-way interaction: 1-4
# interactions[[1]][[1]] <- array(.5, dim = c(level[1], level[4]))
# # 2-way interaction: 2-3
# int_2 <- matrix(0, nrow = level[2], ncol = level[3])
# int_2[1, 1:2] <- 1
# interactions[[1]][[2]] <- int_2
# # 2-way interaction: 1-2
# int_1 <- matrix(0, nrow = level[1], ncol = level[2])
# int_1[1, 1] <- 1
# interactions[[1]][[3]] <- int_1
# 
# 
# # e) Sampling
# set.seed(1)
# mgm_data <- mgmsampler(factors = factors,
#                        interactions = interactions,
#                        thresholds = thresholds,
#                        sds = sds,
#                        type = type,
#                        level = level,
#                        N = 500)
# 
# head(mgm_data$data)
# 
# 
# # -------------------- Application: Autism and Quality of Life --------------------
# 
# dim(autism_data_large$data)
# 
# # Fit MGM Model
# set.seed(1)
# fit_ADS <- mgm(data = autism_data_large$data, 
#                type = autism_data_large$type,
#                level = autism_data_large$level,
#                k = 2, 
#                lambdaSel = 'EBIC', 
#                lambdaGam = 0.25)
# 
# # Make plot
# width <- 9
# pdf(paste0(figDir,'Fig_mgm_application_Autism.pdf'), width = width, height = width*.6)
# 
# qgraph(fit_ADS$pairwise$wadj, 
#        layout = 'spring', repulsion = 1.3,
#        edge.color = fit_ADS$pairwise$edgecolor, 
#        nodeNames = autism_data_large$colnames,
#        color = autism_data_large$groups_color, 
#        groups = autism_data_large$groups_list,
#        legend.mode="style2", legend.cex=.4, 
#        vsize = 3.5, esize = 15)
# 
# dev.off()
# 
# 
# # For paper text
# range(autism_data_large$level)
# autism_data_large$level[c(6,7,27)]
# 
# 
# 
# # -------------------- Application HOI: PTSD --------------------
# 
# head(PTSD_data$data)
# 
# # ----- Fit Model -----
# 
# set.seed(1)
# fit_mgmk <- mgm(data = PTSD_data$data, 
#                 type = PTSD_data$type, 
#                 level = PTSD_data$level,
#                 k = 3, 
#                 lambdaSel = "EBIC", 
#                 lambdaGam = .25, 
#                 overparameterize = TRUE)
# 
# fit_mgmk$interactions$indicator
# 
# 
# # ----- Factor Graph Visualization -----
# 
# width <- 8
# pdf(paste0(figDir,'Fig_mgm_HOI.pdf'), width = width, height = width*.5)
# 
# par(mfrow=c(1,2))
# 
# FactorGraph(object = fit_mgmk, 
#             labels = PTSD_data$names, 
#             PairwiseAsEdge = FALSE)
# 
# # Second panel is added manually with Adobe Illustrator (see code below)
# 
# dev.off()
# 
# 
# # ----- Create Three (conditional) 2-way tables (add with Illustrator to above plot) -----
# 
# # conditional 2-way table
# i <- c(4,2,3)
# PTSD_data$names[c(i[1],i[2],i[3])]
# 
# tb <- table(PTSD_data$data[,i[1]],
#             PTSD_data$data[,i[2]],
#             PTSD_data$data[,i[3]])
# tb[,,1] <- tb[,,1] / sum(tb[,,1])
# tb[,,2] <- tb[,,2] / sum(tb[,,2])
# tb <- round(tb, 2)
# 
# # margingal 2-way table
# tbm <- table(PTSD_data$data[,4], PTSD_data$data[,2])
# tbm <- round(tbm / sum(tbm), 2)
# tbm
# 
# 
# # ----- 1) Marginal (upset, dreams) -----
# 
# library(scales)
# cex_text <- 1.7
# cex_text_yn <- 1.5
# 
# library(RColorBrewer)
# nC3 <- brewer.pal(3, "Set1")
# 
# pdf(paste(figDir, 'Table1_Marg.pdf'), width = 8, height = 8)
# 
# plot.new()
# par(mar=c(2,2,2,2))
# plot.window(xlim=c(-.5, 1.7), ylim=c(-.5, 1.7))
# 
# # Probability Cells
# rect(0, 0, .5, .5, 
#      col = alpha('black', tbm[2,1])) # bottom left
# rect(.5, 1, 0, .5,
#      col = alpha('black', tbm[1,1])) # top left
# rect(.5, 0, 1, .5,
#      col = alpha('black', tbm[2,2])) # bottom right
# rect(.5, .5, 1, 1,
#      col = alpha('black', tbm[1,2])) # top right
# rect(0, 0, 1, 1, lwd = 2)
# 
# # Text
# text(0.25, 0.25, tbm[2,1], cex = cex_text) # bottom left
# text(0.25, 0.75, tbm[1,1], cex = cex_text) # bottom left
# text(0.75, 0.25, tbm[2,2], cex = cex_text) # bottom right
# text(0.75, 0.75, tbm[1,2], cex = cex_text) # top right
# 
# ## Description: shown variables
# # Upset
# rect(-.1, 0, -.05, 1, border=F, col = nC3[3])
# text(-.4, .5, 'Upset', col = nC3[3], cex = cex_text)
# text(-.2, .25, 'Yes', col = nC3[3], cex = cex_text_yn)
# text(-.2, .75, 'No', col = nC3[3], cex = cex_text_yn)
# 
# 
# rect(0, 1.1, 1, 1.05, border=F, col = nC3[1])
# text(.5, 1.38, 'Dreams', col = nC3[1], cex = cex_text)
# text(0.25, 1.2, 'No', col = nC3[1], cex = cex_text_yn)
# text(0.75, 1.2, 'Yes', col = nC3[1], cex = cex_text_yn)
# 
# dev.off()
# 
# 
# 
# # ----- 2.4.2) Conditional on: flashbacks == 0, 1  -----
# 
# for(i in 1:2) {
#   
#   pdf(paste(figDir, 'Table1_Cond',i-1,'.pdf'), width = 8, height = 8)
#   
#   plot.new()
#   par(mar=c(2,2,2,2))
#   plot.window(xlim=c(-.5, 1.7), ylim=c(-.5, 1.7))
#   
#   
#   # conidtional 2-way table
#   tbm <- tb[,,i]
#   
#   # Probability Cells
#   rect(0, 0, .5, .5, 
#        col = alpha('black', tbm[2,1])) # bottom left
#   rect(.5, 1, 0, .5,
#        col = alpha('black', tbm[1,1])) # top left
#   rect(.5, 0, 1, .5,
#        col = alpha('black', tbm[2,2])) # bottom right
#   rect(.5, .5, 1, 1,
#        col = alpha('black', tbm[1,2])) # top right
#   rect(0, 0, 1, 1, lwd = 2)
#   
#   # Text
#   text(0.25, 0.25, tbm[2,1], cex = cex_text) # bottom left
#   text(0.25, 0.75, tbm[1,1], cex = cex_text) # bottom left
#   text(0.75, 0.25, tbm[2,2], cex = cex_text) # bottom right
#   text(0.75, 0.75, tbm[1,2], cex = cex_text) # top right
#   
#   ## Description: shown variables
#   # Upset
#   rect(-.1, 0, -.05, 1, border=F, col = nC3[3])
#   text(-.4, .5, 'Upset', col = nC3[3], cex = cex_text)
#   text(-.2, .25, 'Yes', col = nC3[3], cex = cex_text_yn)
#   text(-.2, .75, 'No', col = nC3[3], cex = cex_text_yn)
#   
#   rect(0, 1.1, 1, 1.05, border=F, col = nC3[1])
#   text(.5, 1.4, 'Dreams', col = nC3[1], cex = cex_text)
#   text(0.25, 1.2, 'No', col = nC3[1], cex = cex_text_yn)
#   text(0.75, 1.2, 'Yes', col = nC3[1], cex = cex_text_yn)
#   
#   
#   if(i==1) answer = 'No' else answer = 'Yes'
#   
#   ## Description: shown variables
#   rect(1.1, 0, 1.05, 1, border=F, col = nC3[2])
#   text(1.5, .5, 'Flashbacks', col = nC3[2], cex = cex_text)
#   text(1.2, .25, answer, col = nC3[2], cex = cex_text_yn)
#   text(1.2, .75, answer, col = nC3[2], cex = cex_text_yn)
#   
#   dev.off()
#   
# }
# 
# 
