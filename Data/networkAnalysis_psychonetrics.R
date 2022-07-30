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
library(NetworkToolbox)


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


#the variable downIrritated is still causing issues. We can try with just one of the two variables. We opt for down.
# nodeVars <- c('ruminating', 'stickiness',
#               'wakeful','energetic', 'energeticWakeful', 'satisfied',
#               'down', 'irritated', 'restless', 'anxious','downIrritated', 'anxiousRestless',
#               'posIntensity', 'negIntensity',
#               'listless', 'distracted', 'sleepQuality')


nodeVars <- c('ruminating', 'stickiness',
              'energetic', 'wakeful', 'satisfied',
              'down', 'irritated', 'anxious', 'restless',
              'posIntensity', 'negIntensity',
              'listless', 'distracted')
              #'sleepQuality')


data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[complete.cases(data_copy[nodeVars]),]


sc_data <- copy(data_copy)
sc_data[nodeVars] <- scale(data_copy[nodeVars])


#paranormal transformation (because the model assumes normality) --> recommended by Epskamp
#huge.npn() should only be applied to data without missing values! Otherwise creates weird values!!!
data_t <- copy(data_copy)
data_t[,nodeVars] <- huge.npn(data_t[,nodeVars])

# groups_list <- list(Rumination = c(1, 2), PositiveAffect = c(3,4,5), NegativeAffect = c(6,7,8,9), MoodReactivity = c(10,11),
#                     OtherNegative = c(12,13))#, Sleep = c(11)) 

groups_list <- list(Rumination = c(1,2), PositiveAffect = c(3,4,5), NegativeAffect = c(6,7,8,9), MoodReactivity = c(10,11),
                    OtherNegative = c(12,13))#, Sleep = c(11)) 

groups_colors <- c("#d60000", "#149F36", "#53B0CF", "#f66a6a", "#72CF53")#, "#0558ff")

Lambda = matrix(1, length(nodeVars),1)
  
#subData <- sc_data[which((sc_data$group==g) & (sc_data$phase=="pre")),]
conBase <- data_t[which((data_t$group=="controls") & (data_t$phase=="pre")),] #non-paranormalized data  & (data_t$block==1)
remBase <- data_t[which((data_t$group=="remitted") & (data_t$phase=="pre")),] #non-paranormalized data


#Try with bootnet
#Baseline Networks
conBoot <- estimateNetwork(conBase, vars = nodeVars,
                           default = "graphicalVAR",
                           idvar = "subjB",
                           dayvar = "blockAssessmentDay",
                           beepvar = "dayBeepNum")


remBoot <- estimateNetwork(remBase, vars = nodeVars,
                           default = "graphicalVAR",
                           idvar = "subjB",
                           dayvar = "blockAssessmentDay",
                           beepvar = "dayBeepNum")

layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
TL <- averageLayout(remBoot$graph$temporal, conBoot$graph$temporal)

plot(conBoot, graph = "temporal",
     title = "Controls - Baseline",
     nodeNames = nodeVars,
     groups = groups_list,
     legend = FALSE,
     labels = c(1:12),
     layout = TL)

plot(remBoot, graph = "temporal",
     title = "Remitted - Baseline",
     nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.5,
     labels = c(1:12),
     layout = TL)

CL <- averageLayout(remBoot$graph$contemporaneous, conBoot$graph$contemporaneous)

plot(conBoot, graph = "contemporaneous",
     nodeNames = nodeVars,
     groups = groups_list,
     legend = FALSE,
     labels = c(1:12),
     layout = CL)

plot(remBoot, graph = "contemporaneous",
     nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.5,
     labels = c(1:12),
     layout = CL)

corStability(remBoot)

#Peri-networks
for(g in c("controls", "remitted")){
    f_dat <- data_t[which((data_t$group==g) & (data_t$intervention=="fantasizing") & (data_t$phase=="peri") & (data_t$block==1)),]
    m_dat <- data_t[which((data_t$group==g) & (data_t$intervention=="mindfulness") & (data_t$phase=="peri")),]
    
    f_boot <- estimateNetwork(f_dat, vars = nodeVars,
                               default = "graphicalVAR",
                               idvar = "subjB",
                               dayvar = "blockAssessmentDay",
                               beepvar = "dayBeepNum")
    
    m_boot <- estimateNetwork(m_dat, vars = nodeVars,
                              default = "graphicalVAR",
                              idvar = "subjB",
                              dayvar = "blockAssessmentDay",
                              beepvar = "dayBeepNum")
    
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
                          beepvar = "dayBeepNum")

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


centralityPlot(f_boot$graph$contemporaneous, include = c("Strength", "ExpectedInfluence", "Closeness"))

centralityTable(f_boot$graph$contemporaneous)

getWmat(f_boot) #get partial correlation matrices 

f1 <- bootnet(f_boot, boots = 1000, nCores = 8, statistics = c("strength", "expectedInfluence", "edge")) #non-parametric bootstrap
f2 <- bootnet(f_boot, boots = 1000, nCores = 6, type = "case", statistics = c("strength", "expectedInfluence", "edge")) #case-dropping bootstrap

corStability(f1)
corStability(f2)


conMod <- var1(conBase, #Note: var1() and gvar() are identical for our purpose
        vars = nodeVars,
        idvar = "subjB",
        dayvar = "blockAssessmentDay",
        beepvar = "dayBeepNum",
       # lambda = Lambda,
        estimator = "FIML",
        storedata = TRUE)
       #contemporaneous = "ggm")

conMod_ts <- tsdlvm1(conBase,
                  vars = nodeVars,
                  idvar = "subjB",
                  dayvar = "blockAssessmentDay",
                  beepvar = "dayBeepNum",
                  lambda = Lambda,
                  estimator = "FIML",
                  storedata = TRUE)




remMod <- var1(remBase, #Note: var1() and gvar() are identical for our purpose
               vars = nodeVars,
               idvar = "subjB",
               dayvar = "blockAssessmentDay",
               beepvar = "dayBeepNum",
               # lambda = Lambda,
               estimator = "FIML",
               storedata = TRUE)
#contemporaneous = "ggm")



conMod <- conMod %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch
conMod_ts <- conMod_ts %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch

remMod <- remMod %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch

compare(var1_model = conMod, tsdlvm1_model = conMod_ts)
#the var1 model has a significantly better fit

TL <- averageLayout(conNet_temp, bootNet1$graph$temporal)
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
plot(bootNet1, graph = "temporal", nodeNames = nodeVars,
          groups = groups_list,
          legend.cex = 0.6,
          labels = c(1:13),
          layout = TL)

qgraph(conNet_temp, layout = TL, theme = "colorblind", cut = 0,
       groups=groups_list, legend=FALSE, legend.cex = 0.4, nodeNames = nodeVars, labels=c(1:13),
       vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - Temporal")


CL <- averageLayout(conNet_temp, bootNet1$graph$contemporaneous)
qgraph(conNet_cont, layout = CL, theme = "colorblind", cut = 0,
       groups=groups_list, legend=FALSE, legend.cex = 0.6, nodeNames = nodeVars, labels=c(1:13),
       vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - Contemporaneous")

plot(bootNet1, graph = "contemporaneous", nodeNames = nodeVars,
     groups = groups_list,
     legend.cex = 0.6,
     labels = c(1:13),
     layout = CL)


test <- 1*(bootNet1$graph$temporal != 0)
model_frombootnet <- psychonetrics::var1(conBase, vars = nodeVars, omega = test) %>% 
  runmodel

compare(model_frombootnet, conMod)

temp_frombootnet <- psychonetrics::ggm(conBase, vars = nodeVars, omega = test) %>% 
  runmodel

conNet_temp <- getmatrix(conMod, matrix = "beta")
conNet_cont <- getmatrix(conMod, matrix = "omega_zeta")

# conNetTS_b <- getmatrix(conMod_ts, matrix = "beta")
conNetTS_c <- getmatrix(conMod_ts, matrix = "exo_cholesky")


remNet_temp <- getmatrix(remMod, matrix = "beta")
remNet_cont <- getmatrix(remMod, matrix = "omega_zeta")

centrality_auto(remNet_temp)
# corStability(conMod)

L <- averageLayout(conNet_temp, conNet_cont)

# pdf(paste0(figs, "figure.pdf"), width=6, height=2.5)
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths

# qgraph(conNetb,  theme = "colorblind", cut = 0, layout = "spring",
#        groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:13),
#        vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - BETA")

qgraph(conNet_temp, layout = L, theme = "colorblind", cut = 0, layout = "spring",
      groups=groups_list, legend=FALSE, legend.cex = 0.4, nodeNames = nodeVars, labels=c(1:13),
      vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - Temporal")

# qgraph(conNetTS_b,  theme = "colorblind", cut = 0, layout = "spring",
#        groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:13),
#        vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - BETA")

qgraph(conNet_cont, layout = L, theme = "colorblind", cut = 0, layout = "spring",
      groups=groups_list, legend=TRUE, legend.cex = 0.6, nodeNames = nodeVars, labels=c(1:13),
      vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - Contemporaneous")





TL <- averageLayout(conNet_temp, remNet_temp)

qgraph(conNet_temp, layout = TL, theme = "colorblind", cut = 0,
       groups=groups_list, legend=FALSE, legend.cex = 0.4, nodeNames = nodeVars, labels=c(1:12),
       vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - Temporal")


qgraph(remNet_temp, layout = TL, theme = "colorblind", cut = 0,
       groups=groups_list, legend=TRUE, legend.cex = 0.4, nodeNames = nodeVars, labels=c(1:12),
       vsize=6, repulsion=1.1, esize=3, title = "Remitted - Baseline - Temporal")

CL <- averageLayout(conNet_cont, remNet_cont)

qgraph(conNet_cont, layout = CL, theme = "colorblind", cut = 0,
       groups=groups_list, legend=FALSE, legend.cex = 0.4, nodeNames = nodeVars, labels=c(1:12),
       vsize=6, repulsion=1.1, esize=3, title = "Controls - Baseline - Temporal")


qgraph(remNet_cont, layout = CL, theme = "colorblind", cut = 0,
       groups=groups_list, legend=TRUE, legend.cex = 0.4, nodeNames = nodeVars, labels=c(1:12),
       vsize=6, repulsion=1.1, esize=3, title = "Remitted - Baseline - Temporal")




remMod <- tsdlvm1(remBase,
                 vars = nodeVars,
                 idvar = "subjB",
                 dayvar = "blockAssessmentDay",
                 beepvar = "dayBeepNum",
                 lambda = Lambda,
                 estimator = "FIML",
                 storedata = TRUE)


remMod <- remMod %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch  

remNet <- getmatrix(remMod, "exo_cholesky")

qgraph(remNet,  theme = "colorblind", cut = 0, layout = "spring",
      groups=groups_list, legend=TRUE, nodeNames = nodeVars, labels=c(1:13),
      vsize=6, repulsion=1.1, esize=3, title = "Remitted - Baseline")

fit(remMod)

centralityPlot(conNet)
corStability(conMod)



Lambda = matrix(1, length(nodeVars),1)

for(g in c("controls", "remitted")){
  for(i in c("fantasizing", "mindfulness")){
    
  
  # for(p in c("pre", "peri")){
    
  #create pre matrices
    #subData <- sc_data[which((sc_data$group==g) & (sc_data$phase=="pre")),] #scaled data
    subData <- data_t[which((data_t$group==g) & (data_t$phase=="pre")),] #non-paranormalized data
    
    preMod <- tsdlvm1(subData,
                     vars = nodeVars,
                     idvar = "subjB",
                     dayvar = "blockAssessmentDay",
                     beepvar = "dayBeepNum",
                     lambda = Lambda,
                     estimator = "FIML")
    
    
    preMod <- preMod %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch
    
    # model %>% setoptimizer
    
    preNet <- getmatrix(preMod, "exo_cholesky")
    
    
    qgraph(preNet$fantasizing, theme = "colorblind", cut = 0, layout = "spring",
           groups=groups_list, legend=TRUE, nodeNames = nodeVars, labels=c(1:13),
           vsize=6, repulsion=1.1, esize=3, title = paste(g, "baseline", "fantasizing", sep = " - "))
    
    qgraph(preNet$mindfulness, theme = "colorblind", cut = 0, layout = "spring",
           groups=groups_list, legend=TRUE, nodeNames = nodeVars, labels=c(1:13),
           vsize=6, repulsion=1.1, esize=3, title = paste(g, "baseline", "mindfulness", sep = " - "))
    
    
    #create peri matrices
    #subData <- sc_data[which((sc_data$group==g) & (sc_data$phase=="peri")),] #scaled data
    subData <- data_t[which((data_t$group==g) & (data_t$phase=="peri")),] #non-paranormalized data
    
    periMod <- tsdlvm1(subData,
                      vars = nodeVars,
                      groups = "intervention",
                      idvar = "subjB",
                      dayvar = "blockAssessmentDay",
                      beepvar = "dayBeepNum",
                      lambda = Lambda,
                      estimator = "FIML")
    
    
    #periMod <- periMod %>% runmodel %>% prune(alpha = 0.01) %>% stepup  %>% prune %>% stepup
    
    periMod <- periMod %>% runmodel %>% prune(alpha = 0.01) %>% modelsearch
    
    # model %>% setoptimizer
    
    periNet <- getmatrix(periMod, "exo_cholesky")
    
    
    qgraph(periNet$fantasizing, theme = "colorblind", cut = 0, layout = "spring",
           groups=groups_list, legend=TRUE, nodeNames = nodeVars, labels=c(1:13),
           vsize=6, repulsion=1.1, esize=3, title = paste(g, "peri", "fantasizing", sep = " - "))
    
    qgraph(periNet$mindfulness, theme = "colorblind", cut = 0, layout = "spring",
           groups=groups_list, legend=TRUE, nodeNames = nodeVars, labels=c(1:13),
           vsize=6, repulsion=1.1, esize=3, title = paste(g, "peri", "mindfulness", sep = " - "))
    
  }
}
psychonetrics::compare(preMod, periMod)
psychonetrics::fit(preMod)
psychonetrics::parameters(preMod)


conBoot <- psychonetrics::bootstrap(conMod)
conBoot <- conBoot %>% runmodel
conBoot

CIplot(conMod)
fit(conMod)













