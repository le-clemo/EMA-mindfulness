
rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/SART")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(Hmisc)
library(corrplot)
library(data.table)
library(ggplot2)
library(reshape)
library(ggpubr)
library(lubridate)
library(igraph)
library(qgraph)

#read in data
games <- read_csv('SART/SART_games_110422.csv') 
numbers <- read_csv('SART/SART_numbers_110422.csv') 
questions <- read_csv('SART/SART_questions_110422.csv') 

mylist <- lapply(excel_sheets('SART/Proefpersonen_Link_ID_Meting.xlsx'), read_excel, path = 'SART/Proefpersonen_Link_ID_Meting.xlsx')

# name the dataframes
names(mylist) <- c("wander_all", "wander_pre1", "wander_peri1", "wander_pre2", "wander_peri2")

# Bring the dataframes to the global environment
list2env(mylist ,.GlobalEnv)

#load preprocessed ESM data for matching
esm <- read_csv('ESM/mindcog_v202204/preprocessed_data.csv')

################################## Inspecting proefpersoon ###################################
length(unique(wander_all$Proefpersoon)) #34
wander_all$Proefpersoon <- tolower(wander_all$Proefpersoon)
wander_all$subject <- str_extract(wander_all$Proefpersoon, regex("^[^_]+(?=_)"))

colnames(wander_all)[c(4,5,12,13,14)] <- c("audio_id_block1", "wander_id_block1", "audio_id_block2",
                                           "wander_id_block2", "comments")

wander_all$group <- NA
for(row in 1:nrow(wander_all)) { 
  if(grepl("g1", wander_all$Proefpersoon[row], fixed = TRUE)){
    wander_all$group[row] <- "controls"
  } 
  if(grepl("g2", wander_all$Proefpersoon[row], fixed = TRUE)){
    wander_all$group[row] <- "remitted"
  }
}

#View(wander_all[which(wander_all$wander_id_block1=="628825449"),])

##################################### combine data ####################################
#from wander_all to games, numbers, questions
length(unique(wander_all$Proefpersoon)) #34

games$subject <- NA
games$dagboek_pre1 <- NA
games$dagboek_peri1 <- NA
games$dagboek_pre2 <- NA
games$dagboek_peri2 <- NA
games$group <- NA
games$wander1 <- FALSE
games$wander2 <- FALSE
games$audio1 <- FALSE
games$audio2 <- FALSE
for(i in 1:nrow(games)){
  for(j in 1:nrow(wander_all)){
    if((games$userID[i] == wander_all$wander_id_block1[j]) & (!is.na(wander_all$wander_id_block1[j]))){
      games$subject[i] <- wander_all$subject[j]
      games$dagboek_pre1[i] <- wander_all$Nr_Dagboek_voormeting1[j]
      games$dagboek_peri1[i] <- wander_all$Nr_Dagboek_Interventie1[j]
      games$dagboek_pre2[i] <- wander_all$Nr_Dagboek_voormeting2[j]
      games$dagboek_peri2[i] <- wander_all$Nr_Dagboek_Interventie2[j]
      games$group[i] <- wander_all$group[j]
      games$wander1[i] <- TRUE
    }
      
    if((games$userID[i] == wander_all$wander_id_block2[j]) & (!is.na(wander_all$wander_id_block2[j]))){
      games$subject[i] <- wander_all$subject[j]
      games$dagboek_pre1[i] <- wander_all$Nr_Dagboek_voormeting1[j]
      games$dagboek_peri1[i] <- wander_all$Nr_Dagboek_Interventie1[j]
      games$dagboek_pre2[i] <- wander_all$Nr_Dagboek_voormeting2[j]
      games$dagboek_peri2[i] <- wander_all$Nr_Dagboek_Interventie2[j]
      games$group[i] <- wander_all$group[j]
      games$wander2[i] <- TRUE
    }
      
    if((games$userID[i] == wander_all$audio_id_block1[j]) & (!is.na(wander_all$audio_id_block1[j]))){
      games$subject[i] <- wander_all$subject[j]
      games$dagboek_pre1[i] <- wander_all$Nr_Dagboek_voormeting1[j]
      games$dagboek_peri1[i] <- wander_all$Nr_Dagboek_Interventie1[j]
      games$dagboek_pre2[i] <- wander_all$Nr_Dagboek_voormeting2[j]
      games$dagboek_peri2[i] <- wander_all$Nr_Dagboek_Interventie2[j]
      games$group[i] <- wander_all$group[j]
      games$audio1[i] <- TRUE
    }
      
    if((games$userID[i] == wander_all$audio_id_block2[j]) & (!is.na(wander_all$audio_id_block2[j]))){
      games$subject[i] <- wander_all$subject[j]
      games$dagboek_pre1[i] <- wander_all$Nr_Dagboek_voormeting1[j]
      games$dagboek_peri1[i] <- wander_all$Nr_Dagboek_Interventie1[j]
      games$dagboek_pre2[i] <- wander_all$Nr_Dagboek_voormeting2[j]
      games$dagboek_peri2[i] <- wander_all$Nr_Dagboek_Interventie2[j]
      games$group[i] <- wander_all$group[j]
      games$audio2[i] <- TRUE
    }
  }
}

length(games[(which(!is.na(games$subject))),]$subject) #3399 games associated with participant

################################## Inspecting games ###################################
length(unique(games$userID)) #72
length(games$gameSessionID) #3771

#class(games$time) #already POSIXct

#adding intervention, phase and block to games
games$intervention <- NA
games$phase <- NA
games$block <- NA
games$fullID <- NA
ivec <- rep(NA,nrow(games)) #creating vectors of needed size (is more efficient)
pvec <- rep(NA,nrow(games))
bvec <- rep(NA,nrow(games))
idvec <- rep(NA,nrow(games))
g_ind <- which(!is.na(games$subject)) #only loop over relevant rows
for(i in g_ind){
  g_date <- format(games$time[i], format = "%Y-%m-%d") #remove time (only date)
  subj <- games$subject[i] #the current subject
  e_ind <- which(esm$subject==subj) #only loop over esm data of current subject
  for(j in e_ind){
    e_date <- format(esm$mindcog_db_open_from[j], format = "%Y-%m-%d")
    if(e_date==g_date){
      ivec[i] <- esm$intervention[j] #add to vector first
      pvec[i] <- esm$phase[j]
      bvec[i] <- esm$block[j]
      idvec[i] <- esm$id[j]
      #print('Conditions met!')
      break #as soon as a match is found break out of the inner for loop
      }
  }
}
games$intervention <- ivec #only now add info to games df (more efficient)
games$block <- bvec
games$phase <- pvec
games$fullID <- idvec

games_summary <- ddply(games, .(subject, fullID, userID), plyr::summarise,
                       nGames = length(unique(gameSessionID)))

# View(esm[which((esm$mindcog_db_open_from=="2021-11-09") & (esm$subject=="s7")),])
# View(games[which( (games$subject=="s7")),])                       
################################# Inspecting numbers ##################################
length(unique(numbers$userID)) #71

numbers$fullID <- NA
numbers$subject <- NA
numbers$group <- NA
numbers$intervention <- NA
numbers$phase <- NA
numbers$block <- NA
idvec <- rep(NA,nrow(numbers))
svec <- rep(NA,nrow(numbers))
gvec <- rep(NA,nrow(numbers))
ivec <- rep(NA,nrow(numbers))
pvec <- rep(NA,nrow(numbers))
bvec <- rep(NA,nrow(numbers))
for(i in 1:nrow(numbers)){ #no NAs so we just loop over everything
  nID <- numbers$userID[i]
  sesh <- numbers$gameSessionID[i]
  ind <- which((games$userID==nID) & (games$gameSessionID==sesh))[1]
  svec[i] <- games$subject[ind]
  gvec[i] <- games$group[ind]
  ivec[i] <- games$intervention[ind]
  pvec[i] <- games$phase[ind]
  bvec[i] <- games$block[ind]
  idvec[i] <- games$fullID[ind]
}

numbers$fullID <- idvec
numbers$subject <- svec
numbers$group <- gvec
numbers$intervention <- ivec
numbers$phase <- pvec
numbers$block <- bvec


numbers_summary <- ddply(numbers, .(userID), plyr::summarise,
                         nGames = length(unique(gameSessionID)),
                         nTrials = length(correct),
                         proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                         meanRT = round(mean(responseTime),2))

max(numbers_summary$nTrials) #21493
mean(numbers_summary$nTrials) #1612
#View(numbers[which(numbers$userID=="148649783"),])                      

################################ Inspecting questions ##################################
length(unique(questions$userID)) #62

questions$fullID <- NA
questions$subject <- NA
questions$group <- NA
questions$intervention <- NA
questions$phase <- NA
questions$block <- NA
idvec <- rep(NA,nrow(questions))
svec <- rep(NA,nrow(questions))
gvec <- rep(NA,nrow(questions))
ivec <- rep(NA,nrow(questions))
pvec <- rep(NA,nrow(questions))
bvec <- rep(NA,nrow(questions))
for(i in 1:nrow(questions)){ #no NAs so we just loop over everything
  nID <- questions$userID[i]
  sesh <- questions$gameSessionID[i]
  ind <- which((games$userID==nID) & (games$gameSessionID==sesh))[1]
  svec[i] <- games$subject[ind]
  gvec[i] <- games$group[ind]
  ivec[i] <- games$intervention[ind]
  pvec[i] <- games$phase[ind]
  bvec[i] <- games$block[ind]
  idvec[i] <- games$fullID[ind]
}

questions$fullID <- idvec
questions$subject <- svec
questions$group <- gvec
questions$intervention <- ivec
questions$phase <- pvec
questions$block <- bvec



questions_summary <- ddply(questions, .(userID), plyr::summarise,
                           nGames = length(unique(gameSessionID)),
                           nQuestions = length(questionID))

unique(questions$answer)
questions_summary2 <- ddply(questions, .(questionID), plyr::summarise,
                            proportionAnswer0 = round(length(questionID[which(answer==0)])/length(questionID),2),
                            proportionAnswer1 = round(length(questionID[which(answer==1)])/length(questionID),2),
                            proportionAnswer2 = round(length(questionID[which(answer==2)])/length(questionID),2),
                            proportionAnswer3 = round(length(questionID[which(answer==3)])/length(questionID),2),
                            proportionAnswer4 = round(length(questionID[which(answer==4)])/length(questionID),2),
                            proportionAnswer5 = round(length(questionID[which(answer==5)])/length(questionID),2))

################################









