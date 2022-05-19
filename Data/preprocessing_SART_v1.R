
rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

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

# esm$group <- factor(esm$group, levels = c("controls", "remitted"))
# esm$intervention <- factor(esm$intervention, levels = c("mindfulness", "fantasizing"))
# esm$phase <- factor(esm$phase, levels = c("pre", "peri"))

################################## Adapting proefpersoon ###################################
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
length(unique(wander_all$Proefpersoon)) #34 participants

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
length(unique(games$userID)) #72 (participants that took part in both blocks are counted twice!)
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

games_subj <- ddply(games, .(group,intervention), plyr::summarise,
                    nSubj = length(unique(subject)))

length(unique(games$subject))

# View(esm[which((esm$mindcog_db_open_from=="2021-11-09") & (esm$subject=="s7")),])
# View(games[which( (games$subject=="s7")),])                       

################################ Inspecting questions ##################################
#Q0 = What were you just thinking about?
      #a0 = I was fully concentrated on my task
      #a1 = I rated aspects of the task (e.g. my performance or how long it takes)
      #a2 = I was thinking about personal matters
      #a3 = I was distracted by my surroundings (e.g. sound, temperature, my physical state)
      #a4 = I was daydreaming / I was thinking about task unrelated things
      #a5 = I wasn't paying attention, but I wasn't thinking of anything specific

#Q1 = Did your thoughts have a negative, neutral or positive charge?
      #a0 = negative
      #a1 = neutral
      #a2 = positive

#Q2 = How hard was it to let go of the thought?
      #a0 = very difficult
      #a1 = difficult
      #a2 = neither difficult nor easy
      #a3 = easy
      #a4 = very easy

#Q3 = What was the time orientation of your thought?
      #a0 = past
      #a1 = present
      #a2 = future



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


#many duplicate rows to remove
# questions <- questions[duplicated(questions[,c("userID", "gameSessionID", "subject", "time")]),]
# 
# questions <- questions[!(duplicated(questions[c("userID","gameSessionID", "subject", "time")]) |
#                            duplicated(questions[c("userID","gameSessionID", "subject", "time")], fromLast = TRUE)), ]

questions <- questions %>%
  distinct(userID, gameSessionID, subject, time, .keep_all = TRUE)

questions_summary <- ddply(questions, .(userID), plyr::summarise,
                           nGames = length(unique(gameSessionID)),
                           nQuestions = length(questionID))

#unique(questions$answer)
remitted_summary <- ddply(questions[which(questions$group=="remitted"),], .(questionID, phase), plyr::summarise,
                            nResponses = length(group),
                            proportionAnswer0 = round(length(questionID[which(answer==0)])/length(questionID),2),
                            proportionAnswer1 = round(length(questionID[which(answer==1)])/length(questionID),2),
                            proportionAnswer2 = round(length(questionID[which(answer==2)])/length(questionID),2),
                            proportionAnswer3 = round(length(questionID[which(answer==3)])/length(questionID),2),
                            proportionAnswer4 = round(length(questionID[which(answer==4)])/length(questionID),2),
                            proportionAnswer5 = round(length(questionID[which(answer==5)])/length(questionID),2))

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
numbers$date <- format(as.POSIXct(numbers$time, format="%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")

#many duplicate rows to remove
numbers <- numbers[duplicated(numbers[,c("userID", "gameSessionID", "subject", "time")]),]

numbers_summary <- ddply(numbers, .(group), plyr::summarise,
                         nGames = length(unique(gameSessionID)),
                         nTrials = length(correct),
                         proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                         meanRT = round(mean(responseTime),2),
                         sdRT = round(sd(responseTime),2))

numbers_summary2 <- ddply(numbers, .(group, intervention, phase), plyr::summarise, rm.na=TRUE,
                          nGames = length(unique(gameSessionID)),
                          nTrials = length(correct),
                          proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                          meanRT = round(mean(responseTime),2),
                          sdRT = round(sd(responseTime),2))

numbers_per_participant <- ddply(numbers, .(userID, subject, group, phase, block, date), plyr::summarise,
                                 nGames = length(unique(gameSessionID)),
                                 nTrials = length(correct),
                                 proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                                 meanRT = round(mean(responseTime),2),
                                 sdRT = round(sd(responseTime),2))

max(numbers_summary2$nTrials) #28136
mean(numbers_summary2$nTrials) #6023
#View(numbers[which(numbers$userID=="148649783"),])                      

numbers <- numbers %>%
  distinct(userID, gameSessionID, subject, time, responseTime, .keep_all = TRUE)

numbers$cycle <- NA #a column to determine which number guesses are associated with which thought probes
subject_IDs <- unique(numbers$subject) #to loop over all subjects
questions$cycle <- NA #same for questions df

for(subj in subject_IDs){
  if(is.na(subj)){ #if it's NA --> skip iteration
    #print(subj)
    next
  } else { #else get all the userIDs for this subject
    print(subj)
    user_IDs <- unique(numbers[which(numbers$subject==subj),]$userID)
  }

  for(id in user_IDs){ #for each userID get all game sessions
    game_ids <- unique(questions[which((questions$userID==id) & (questions$subject==subj)),]$gameSessionID)
    for(gid in game_ids){ #for each game Session get the times of the first thought probe
      times_q0 <- questions[which((questions$userID==id) & (questions$subject==subj) & (questions$gameSessionID==gid) &
                                    (questions$questionID==0)),]$time
      times_q0 <- times_q0[order(times_q0)] #and reverse its order (from highest to lowest)
      
      q_rows <- which((questions$userID==id) & (questions$subject==subj) & (questions$gameSessionID==gid)) #get all relevant questions rows
      n_rows <- which((numbers$userID==id) & (numbers$subject==subj) & (numbers$gameSessionID==gid)) #same for numbers
      num_cycles <- length(times_q0) #how many cycles are there? i.e. how many times were the thought probes presented
      q_cycles <- list(rep(NA, length(q_rows))) #create empty list of correct length
      n_cycles <- list(rep(NA, length(n_rows)))
      q <- 0 #to be able to correctly place the values in the empty lists
      n <- 0
      for(row in q_rows){ #for every row
        #print(row)
        q <- q + 1
        for(i in 1:num_cycles){
          if(questions$time[row] >= times_q0[i]){ #is the time lower than the last thought probe?
            #thought probes always start with 0 (0 will be equal with time, all other probes of the cycle will be higher)
            q_cycles[[1]][q] <- i
            
          } #else {break} #if its not greater or equal the thought probe must be associated with the cycle of the prior iteration
        }
      }  
      
      questions[q_rows,]$cycle <- q_cycles[[1]] #now we actually add them to the df
      
      for(row in n_rows){
        #print(row)
        n <- n + 1
        for(i in 1:num_cycles){
          if(numbers$time[row] < times_q0[i]){ #as the numbers game take place before the thought probes we look for lower times
            n_cycles[[1]][n] <- i
            break #the first cycle it is lower than will be the correct one
          }
        }
      }
      numbers[n_rows,]$cycle <- n_cycles[[1]] #actually add to df
    }
  }
}



numbers$Q0 <- NA
numbers$Q1 <- NA
numbers$Q2 <- NA
numbers$Q3 <- NA #these will contain the answer to the respective question
for(subj in subject_IDs){
  if(is.na(subj)){ #if it's NA --> skip iteration
    #print(subj)
    next
  } else { #else get all the userIDs for this subject
    user_IDs <- unique(numbers[which(numbers$subject==subj),]$userID)
  }
  
  for(id in user_IDs){ #for each userID get all game sessions
    game_ids <- unique(numbers[which((numbers$userID==id) & (numbers$subject==subj)),]$gameSessionID)
    for(gid in game_ids){ 
      num_cycles <- length(unique(questions[which((questions$userID==id) & (questions$subject==subj) &
                                                    (questions$gameSessionID==gid)),]$cycle))

      for(i in 1:num_cycles){
        
        answer0 <- questions[which((questions$userID==id) & (questions$gameSessionID==gid) & (questions$subject==subj) &
                                     (questions$cycle==i) & (questions$questionID==0)),]$answer
          
        answer1 <- questions[which((questions$userID==id) & (questions$gameSessionID==gid) & (questions$subject==subj) &
                                       (questions$cycle==i) & (questions$questionID==1)),]$answer
        
        answer2 <- questions[which((questions$userID==id) & (questions$gameSessionID==gid) & (questions$subject==subj) &
                                     (questions$cycle==i) & (questions$questionID==2)),]$answer
        
        answer3 <- questions[which((questions$userID==id) & (questions$gameSessionID==gid) & (questions$subject==subj) &
                                     (questions$cycle==i) & (questions$questionID==3)),]$answer
        
        c_rows <- which((numbers$userID==id) & (numbers$gameSessionID==gid) & (numbers$subject==subj) &
                           numbers$cycle==i)
        # answers <- list(rep(NA, length(c_rows)))
        if(length(answer0)>0){
          numbers[c_rows,]$Q0 <- answer0
        }
        
        if(length(answer1)>0){
          numbers[c_rows,]$Q1 <- answer1
        }
        
        
        if(length(answer2)>0){
          numbers[c_rows,]$Q2 <- answer2
        }
        
        
        if(length(answer3)>0){
          numbers[c_rows,]$Q3 <- answer3
        }
      }

    }
  }
}

numbers_q0 <- ddply(numbers, .(group, Q0), summarise,
                              nSubj = length(unique(subject)),
                              nTrials = length(correct),
                              proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                              meanRT = round(mean(responseTime),2),
                              sdRT = round(sd(responseTime),2))

numbers_q1 <- ddply(numbers, .(group, Q1), summarise,
                    nSubj = length(unique(subject)),
                    nTrials = length(correct),
                    proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                    meanRT = round(mean(responseTime),2),
                    sdRT = round(sd(responseTime),2))

numbers_q2 <- ddply(numbers, .(group, Q2), summarise,
                    nSubj = length(unique(subject)),
                    nTrials = length(correct),
                    proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                    meanRT = round(mean(responseTime),2),
                    sdRT = round(sd(responseTime),2))#

numbers_q3 <- ddply(numbers, .(group, Q3), summarise,
                    nSubj = length(unique(subject)),
                    nTrials = length(correct),
                    proportionCorrect = round(length(correct[which(correct == TRUE)]) / nTrials,2),
                    meanRT = round(mean(responseTime),2),
                    sdRT = round(sd(responseTime),2))


dat_text <- data.frame(
  label = c("100% | 97%", "93% | 94%", "97% | 97%", "93% | 96%", "94% | 95%", "95% | 99%"),
  # label = c("N=261 | 100%", "N=161 | 93%", "N=110 | 97%", "N=184 | 93%", "N=36 | 94%", "N=43 | 95%",
  #           "N=1014 | 97%", "N=181 | 94%", "N=194 | 97%", "N=423 | 96%", "N=191 | 95%", "N=221 | 99%"),
  Q0   = c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5),
  group = c("controls", "controls", "controls", "controls", "controls", "controls",
            "remitted", "remitted", "remitted", "remitted", "remitted", "remitted")
)

ans_q0 <- c(
  `0` = "Task",
  `1` = "Aspects of task",
  `2` = "Personal Matters",
  `3` = "Surroundings",
  `4` = "Daydreaming",
  `5`= "Distracted/other"
)

melt_q0 <- melt(numbers, id.vars=c( "group","Q0"), measure.vars=c("responseTime"))
melt_q0 <- melt_q0[which(!is.na(melt_q0$Q0)),]

ggplot(melt_q0) +
  geom_boxplot(aes(x=Q0, y=value, color=group)) +
  facet_grid(. ~ Q0, scale = "free", labeller = as_labeller(ans_q0)) +
  labs(title = "Q0: What were you just thinking about?", x = "", y = "Response time") + 
   theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
  geom_text(
      data    = dat_text,
      mapping = aes(x = -Inf, y = -Inf, label = label),
      hjust = -0.45,
      vjust = -50
    )


dat_text <- data.frame(
  label = c(
            "94% | 100%", "92% | 97%", "95% | 97%", "100% | 97%", "98% | 98%"),
  Q1   = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
  group = c("controls", "controls", "controls", "controls", "controls",
            "remitted", "remitted", "remitted", "remitted", "remitted")
)

ans_q1 <- c(
  `0` = "Very hard",
  `1` = "Hard",
  `2` = "Neither",
  `3` = "Easy",
  `4` = "Very easy"
)

melt_q1 <- melt(numbers, id.vars=c( "group","Q1"), measure.vars=c("responseTime"))
melt_q1 <- melt_q1[which(!is.na(melt_q1$Q1)),]

ggplot(melt_q1) +
  geom_boxplot(aes(x=Q1, y=value, color=group)) +
  facet_grid(. ~ Q1, scale = "free", labeller = as_labeller(ans_q1)) +
  labs(title = "Q1: How hard was it to let go of the thought?", x = "", y = "Response time") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label),
    hjust = -0.45,
    vjust = -50
  )


dat_text <- data.frame(
  label = c(
            "95% | 98%", "96% | 97%", "97% | 97%"),
  Q2   = c(0, 1, 2, 0, 1, 2),
  group = c("controls", "controls", "controls",
            "remitted", "remitted", "remitted")
)

ans_q2 <- c(
  `0` = "Negative",
  `1` = "Neutral",
  `2` = "Positive"
)

melt_q2 <- melt(numbers, id.vars=c( "group","Q2"), measure.vars=c("responseTime"))
melt_q2 <- melt_q2[which(!is.na(melt_q2$Q2)),]

ggplot(melt_q2) +
  geom_boxplot(aes(x=Q2, y=value, color=group)) +
  facet_grid(. ~ Q2, scale = "free", labeller = as_labeller(ans_q2)) +
  labs(title = "Q2: Did your thoughts have a negative, neutral or positive charge?", x = "", y = "Response time") +
  scale_fill_manual(values = c("green", "red")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label),
    hjust = -1.5,
    vjust = -50
  )



dat_text <- data.frame(
  label = c(
            "86% | 98%", "96% | 97%", "99% | 97%"),
  Q3   = c(0, 1, 2, 0, 1, 2),
  group = c("controls", "controls", "controls",
            "remitted", "remitted", "remitted")
)

ans_q3 <- c(
  `0` = "Past",
  `1` = "Present",
  `2` = "Future"
)

melt_q3 <- melt(numbers, id.vars=c( "group","Q3"), measure.vars=c("responseTime"))
melt_q3 <- melt_q3[which(!is.na(melt_q3$Q3)),]

ggplot(melt_q3) +
  geom_boxplot(aes(x=Q3, y=value, color = group)) +
  facet_grid(. ~ Q3, scale = "free", labeller = as_labeller(ans_q3)) +
  labs(title = "Q3: What was the time orientation of your thought?", x = "", y = "Response time") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label),
    hjust = -1.5,
    vjust = -50
  )

#Q0 = What were you just thinking about?
#a0 = I was fully concentrated on my task
#a1 = I rated aspects of the task (e.g. my performance or how long it takes)
#a2 = I was thinking about personal matters
#a3 = I was distracted by my surroundings (e.g. sound, temperature, my physical state)
#a4 = I was daydreaming / I was thinking about task unrelated things
#a5 = I wasn't paying attention, but I wasn't thinking of anything specific

#Q1 = Did your thoughts have a negative, neutral or positive charge?
#a0 = negative
#a1 = neutral
#a2 = positive

#Q2 = How hard was it to let go of the thought?
#a0 = very difficult
#a1 = difficult
#a2 = neither difficult nor easy
#a3 = easy
#a4 = very easy

#Q3 = What was the time orientation of your thought?
#a0 = past
#a1 = present
#a2 = future


