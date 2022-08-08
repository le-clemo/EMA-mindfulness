
#################################### Set up ####################################
rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202202-2")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202202-2")

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
data <- read_xlsx('mindcog_db_2022-02-14.xlsx') 


#################################### Data clean up ####################################
#drop unnecessary columns
data <- subset(data, select = -c(roqua_id, hide_pii_from_researchers, gender, birth_year,
                                 hide_values_from_professionals, respondent_label, respondent_type,
                                 mindcog_db_project, mindcog_db_notes, mindcog_db_location,
                                 mindcog_db_invited_at, mindcog_db_emailed_at, mindcog_db_variant,
                                 mindcog_db_anonymous))


#turn all ids to lower case for easier error handling below
data$id <- tolower(data$id)

#fix various errors in id column
for(row in 1:nrow(data)) { #some "s" are missing
  if( ! (grepl("s", data$id[row], fixed=TRUE)) & (!(is.na(data$id[row])))){ 
    data$id[row] <- paste("s", data$id[row], sep = "")
  }  
  if( ! (grepl("_g", data$id[row], fixed=TRUE))){ #some "_" prior to "g" are missing
    data$id[row] <- sub("g", "\\1_g", data$id[row])
  }
  if( ! (grepl("_m", data$id[row], fixed = TRUE))){ #some "_" prior to "m" are missing
    data$id[row] <- sub("m", "\\1_m", data$id[row])
  }
  if( (grepl("meting", data$id[row], fixed = TRUE))){#some had "meting" instead of just "m"
    data$id[row] <- sub("meting", "\\1m", data$id[row])
  }
  
}


#add group
data$group <- NA
for(row in 1:nrow(data)) { 
  if((grepl("g1", data$id[row], fixed = TRUE))){
    data$group[row] <- "controls"
  } 
  if((grepl("g2", data$id[row], fixed = TRUE))){
    data$group[row] <- "remitted"
  }
}

#add phase
data$phase <- NA
for(row in 1:nrow(data)) { 
  if((grepl("m1", data$id[row], fixed = TRUE)) | (grepl("m3", data$id[row], fixed = TRUE))){
    data$phase[row] <- "pre"
  } 
  if((grepl("m2", data$id[row], fixed = TRUE)) | (grepl("m4", data$id[row], fixed = TRUE))){
    data$phase[row] <- "peri"
  }
}

#add block (1 = first intervention cycle, 2 = second intervention cycle)
data$block <- NA
for(row in 1:nrow(data)) { 
  if((grepl("m1", data$id[row], fixed = TRUE)) | (grepl("m2", data$id[row], fixed = TRUE))){
    data$block[row] <- 1
  } else {
    data$block[row] <- 2
  }
}


missing_data <- ddply(data, .(patient_id, id, group, intervention), plyr::summarise,
                      numBeeped = length(mindcog_db_open_from),
                      responseRate = round((numBeeped - length(unique(mindcog_db_non_response)))/numBeeped,2))


na_data <- missing_data[(is.na(missing_data$patient_id)) |
                          is.na((missing_data$group)) |
                          is.na((missing_data$intervention)), ]


#write.csv(na_data, file = "patientID_issues.csv")

#convert excel na to R na and remove respondents without group (for now)
#data[data=="#N/A"] = NA

#drop subjects without an assigned group
data <- drop_na(data, group)
data <- drop_na(data, patient_id)
data <- drop_na(data, id)

#unique(data$id)

#test <- subset(data, select = c(id, phase, block))


#add new column "subject" -> extract all characters up until the first underscore in column "id"
data$subject <- str_extract(data$id, regex("^[^_]+(?=_)"))

#Changing ESM item names
#Get numbers of ESM item columns
item1 <- which( colnames(data)=="mindcog_db_1" ) #this is the first column that interests us
item33 <- which( colnames(data)=="mindcog_db_33" )#last item of interest
colNamesOld <- setNames(data.frame(colnames(data[, item1:item33])), "columns")

colNamesNew <- c('firstEntry', 'sleepQuality', 'toBedHour', 'toBedMinute', 'trySleepHour', 'trySleepMinute',
                 'durationFallAsleep', 'wakeupHour', 'wakeupMinute', 'restednessWakeup', 'wakeful',
                 'sad', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
                 'listless', 'thinkingOf', 'worried', 'stickiness', 'thoughtsPleasant',
                 'thoughtsTime', 'thoughtsValence', 'thoughtsObject', 'distracted',
                 'restOfDayPos', 'aloneCompany', 'companyPleasant', 'alonePleasant', 'posMax',
                 'posIntensity', 'negMax', 'negIntensity', 'comments')

setnames(data, old = colNamesOld$columns, new = colNamesNew)

#################################### Initial analyses  ####################################

#Convert dates from characters to datetimes
data[['mindcog_db_open_from']] <- as.POSIXct(data[['mindcog_db_open_from']],
                                   format = "%d/%m/%Y %H:%M")

data[['mindcog_db_started_at']] <- as.POSIXct(data[['mindcog_db_started_at']],
                                             format = "%d/%m/%Y %H:%M")

data[['mindcog_db_completed_at']] <- as.POSIXct(data[['mindcog_db_completed_at']],
                                             format = "%d/%m/%Y %H:%M")

#calculate the time it took a participant to start after being informed (in minutes)
data$response_delay <- (data$mindcog_db_started_at - data$mindcog_db_open_from)/60
#calculate how long it took a participant to complete the questionnaire (in minutes)
data$response_duration <- (data$mindcog_db_completed_at - data$mindcog_db_started_at)/60

#calculate the minutes that have past since the last beep
subject_IDs <- unique(data$subject) #get a list of all unique IDs
data$minLastBeep <- NA
for(id in subject_IDs){ #for loop to fill the column with the day numbers
  prev_row = 0
  xDate <- as.Date("01/01/1999", format = '%d/%m/%Y') #low arbitrary date for comparison of first assessment date
  respondent_rows <- which(data$subject == id) #row indices of rows associated with respondent
  for (row in respondent_rows) { #loop over these rows
    #if this rows date is greater than the previous assessment's date
    if(as.Date(data$mindcog_db_open_from[row]) == as.Date(xDate)){
      timePast = hms(as.ITime(data$mindcog_db_open_from[row]) - as.ITime(data$mindcog_db_open_from[prev_row]))
      minutesPast = hour(timePast)*60 + minute(timePast) #increment the assessment day count
      data$minLastBeep[row] <- minutesPast #add the minutes past
      prev_row = row #update the previous row index (not plus one bc numbers are not continuous!)
      xDate = data$mindcog_db_open_from[row] #set new comparison date to date of current row
    } else {
      #otherwise this row's date is equal to the previous row's --> same assessment day
      data$minLastBeep[row] <- NA
      prev_row = row
      xDate = data$mindcog_db_open_from[row] #set new comparison date to date of current row
    }
    
  }
  
}

#test <- subset(data, select = c(id, subject, mindcog_db_open_from, minLastBeep))

#group by respondent_id and count the number on nonresponses
participant_responses <- ddply(data, .(subject), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2))

#the mean response rate is ~65%
meanResponseRate <- mean(participant_responses$responseRate)
#sd of 23.46
sdResponseRate <- sd(participant_responses$responseRate)

#################################### Beep number, assessment day and lagged variables  ####################################

#adding beep number (continuous count of sent assessment queries)
data$beepNum <- NA

for(id in subject_IDs){ #every participant
  respondent_rows <- which(data$subject == id) #row indices of rows associated with respondent
  #adding a number per assessment
  data[respondent_rows,]$beepNum <- 1:length(respondent_rows) 
}

#beep number per phase
data$phaseBeepNum <- NA
phases <- unique(data$phase)
for(id in subject_IDs){ #every participant
  for(phase in phases){
    phase_rows <- which((data$subject == id) & (data$phase == phase)) #row indices of rows associated with respondent    
    if(length(phase_rows) > 0){
      data[phase_rows,]$phaseBeepNum <- 1:length(phase_rows)
    }
  }
}

#beep number per block
data$blockBeepNum <- NA
for(id in subject_IDs){ #every participant
  for(block in 1:2){
    block_rows <- which((data$subject == id) & (data$block == block)) #row indices of rows associated with respondent    
    if(length(block_rows) > 0){
      data[block_rows,]$blockBeepNum <- 1:length(block_rows)
    }
  }
}

#test <- subset(data[data$subject == "s8",], select = c(subject, phase, block, blockBeepNum, phaseBeepNum, beepNum))

#adding assessment day
data$assessmentDay <- NA #adding an empty column for assessment days
for(id in subject_IDs){ #for loop to fill the column with the day numbers
  assessment_day <- 0 
  prev_row = 0
  xDate <- as.Date("01/01/1999", format = '%d/%m/%Y') #low arbitrary date for comparison of first assessment date
  respondent_rows <- which(data$subject == id) #row indices of rows associated with respondent
  for (row in respondent_rows) { #loop over these rows
    #if this rows date is greater than the previous assessment's date
    if(as.Date(data$mindcog_db_open_from[row]) > xDate){
      assessment_day = assessment_day + 1 #increment the assessment day count
      data$assessmentDay[row] <- assessment_day #and add this new number as assessment day for this row
      prev_row = row #update the previous row index (not plus one bc numbers are not continuous!)
      xDate = as.Date(data$mindcog_db_open_from[row]) #set new comparison date to date of current row
    } else {
      #otherwise this row's date is equal to the previous row's --> same assessment day
      data$assessmentDay[row] <- data$assessmentDay[prev_row]
      prev_row = row
    }
  }
}

#test <- subset(data, select = c(id, subject, mindcog_db_open_from, assessmentDay))

#assessment day per block
data$blockAssessmentDay <- NA #adding an empty column for assessment days
for(id in subject_IDs){ #for loop to fill the column with the day numbers
  for(block in 1:2){
    assessment_day <- 0 
    prev_row = 0
    xDate <- as.Date("01/01/1999", format = '%d/%m/%Y') #low arbitrary date for comparison of first assessment date
    block_rows <- which((data$subject == id) & (data$block == block)) #row indices of rows associated with respondent
    if(length(block_rows) > 0){
      for (row in block_rows) { #loop over these rows
        #if this rows date is greater than the previous assessment's date
        if(as.Date(data$mindcog_db_open_from[row]) > xDate){
          assessment_day = assessment_day + 1 #increment the assessment day count
          data$blockAssessmentDay[row] <- assessment_day #and add this new number as assessment day for this row
          prev_row = row #update the previous row index (not plus one bc numbers are not continuous!)
          xDate = as.Date(data$mindcog_db_open_from[row]) #set new comparison date to date of current row
        } else {
          #otherwise this row's date is equal to the previous row's --> same assessment day
          data$blockAssessmentDay[row] <- data$blockAssessmentDay[prev_row]
          prev_row = row
        }
      }
    }
  }
}

#test <- subset(data, select = c(id, subject, block, mindcog_db_open_from, blockAssessmentDay))
#View(test[test$subject == "s8",])

#recreacting with assessment days per participant
participant_responses <- ddply(data, .(subject), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay))

#recreacting with assessment days per group / intervention / phase
group_responses <- ddply(data, .(group), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay)) #6% higher response rate in controls


#Creating lagged variables
#the variables to be lagged
cols <- c('wakeful', 'sad', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
             'listless', 'thinkingOf', 'worried', 'stickiness', 'thoughtsPleasant', 'thoughtsTime',
             'thoughtsValence', 'thoughtsObject', 'distracted', 'restOfDayPos', 'aloneCompany',
             'companyPleasant', 'alonePleasant', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

#creating a vector with new "lagged" column names
laggedCols <- c()
for(col in cols) {
  laggedCols <- c(laggedCols, paste(col, "lag1", sep = "_"))
}

#adding empty columns with "lagged" names
data[, laggedCols] <- NA

#zipping current and lagged column names for the for loop
colZip <- mapply(c, cols, laggedCols, SIMPLIFY = FALSE)


#for loop to add the lagged values to their corresponding new columns
for(id in subject_IDs){
  respondent_rows <- which(data$subject == id) #one respondent at a time
  
  for(col in colZip) { #looping over the zipped column name pairs
    prev_value <- NA #previous value starts out as NA for every column
    current_day <- 1 #current day at the beginning of every column per respondent is 1

    for(row in respondent_rows) { #looping over the rows associated with the current respondent
      
      if(data$assessmentDay[row] == current_day) { #if the assessment day matches the current day
        data[row, col[2]] <- prev_value #add the previous value as the value for the lagged column
        prev_value <- data[row, col[1]] #update the previous value
        
      } else { #if assessment day and current day do not match
        data[row, col[2]] <- NA #then the lagged value should be NA (new day!)
        current_day <- data$assessmentDay[row] #update the current day (could also just be +1)
        prev_value <- data[row, col[1]] #update previous value
      }
    } 
  }
}

#################################### Change scores ####################################

cols <- c('wakeful', 'sad', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
          'listless', 'worried', 'stickiness', 'thoughtsPleasant',  'thoughtsObject', 'distracted', 'restOfDayPos',
          'companyPleasant', 'alonePleasant', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

#creating a vector with new "lagged" column names
changeCols <- c()
for(col in cols) {
  changeCols <- c(changeCols, paste(col, "change", sep = "_"))
}

#adding empty columns with "lagged" names
data[, changeCols] <- NA

#zipping current and lagged column names for the for loop
colZip <- mapply(c, cols, changeCols, SIMPLIFY = FALSE)


#for loop to add the lagged values to their corresponding new columns
for(id in subject_IDs){
  respondent_rows <- which(data$subject == id) #one respondent at a time
  
  for(col in colZip) { #looping over the zipped column name pairs
    prev_value <- 0
    change_score <- NA #previous value starts out as NA for every column
    current_day <- 1 #current day at the beginning of every column per respondent is 1
    
    for(row in respondent_rows) { #looping over the rows associated with the current respondent
      
      if(data$assessmentDay[row] == current_day) { #if the assessment day matches the current day
        change_score <- data[row, col[1]] - prev_value #calculate next change score
        data[row, col[2]] <- change_score #add the change_score as the value for the lagged column
        prev_value <- data[row, col[1]] #update the previous value
        
      } else { #if assessment day and current day do not match
        data[row, col[2]] <- NA #then the change score should be NA (new day!)
        current_day <- data$assessmentDay[row] #update the current day (could also just be +1)
        prev_value <- data[row, col[1]] #update previous value
      }
    } 
  }
}

# test <- subset(data, select = c(subject, phase, sad, sad_change, assessmentDay))
# View(test)

#write.csv(data, file = "ESM_preprocessed_25032022.csv")

#################################### Initial analyses by group ####################################

#number of respondents (i.e., participants?) so far
length(unique(data$subject)) #38 associated with a group

#summary(data)

#calculating statistics per group (remitted vs controls)
grp_avgs <- ddply(data, .(group), plyr::summarize,
                 n_Subj = length(unique(subject)),
                 db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                 db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                 db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                 db9_sad_avg = mean(sad, na.rm = TRUE),
                 db9_sad_sd = sd(sad, na.rm = TRUE),
                 db10_satisfied_avg = mean(satisfied, na.rm = TRUE),
                 db10_satisfied_sd = sd(satisfied, na.rm = TRUE),
                 db11_irritated_avg = mean(irritated, na.rm = TRUE),
                 db12_energetic_avg = mean(energetic, na.rm = TRUE),
                 db13_restless_avg = mean(restless, na.rm = TRUE),
                 db14_stressed_avg = mean(stressed, na.rm = TRUE),
                 db15_anxious_avg = mean(anxious, na.rm = TRUE),
                 db16_listless_avg = mean(listless, na.rm = TRUE),
                 db18_worrying_avg = mean(worried, na.rm = TRUE),
                 db19_stickiness_avg = mean(stickiness, na.rm = TRUE),
                 db20_easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                 db24_distracted_avg = mean(distracted, na.rm = TRUE),
                 db25_restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                 db27_companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                 db28_solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                 db29_enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                 db30_intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                 db31_unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                 db32_intensityNeg_avg = mean(negIntensity, na.rm = TRUE),
                 response_delay_avg = round(mean(response_delay, na.rm = TRUE), 2),
                 response_duration_avg = round(mean(response_duration, na.rm = TRUE), 2))

#Metric columns
metricCols <- c('wakeful', 'sad', 'satisfied', 'irritated', 'energetic', 'restless',
                'stressed', 'anxious', 'listless', 'worried', 'stickiness', 'thoughtsPleasant', 'distracted',
                'restOfDayPos', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

#for some strange dplyr-ralted reason I need to do this to get melt() to work
data <- as.data.frame(data)

#boxplot comparisons
meltData1 <- melt(data[, c("group", metricCols[1:9])])
#boxplot(data=meltData, value~variable)

p1 <- ggplot(meltData1, aes(factor(variable), value, fill = group)) 
p1 + geom_boxplot() + facet_wrap(~variable, scale="free")


meltData2 <- melt(data[, c("group", metricCols[10:18])])
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = group)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")


############################################################################################
# grp_avgs <- ddply(data, .(group), plyr::summarize,
#                   n_Subj = length(unique(respondent_id)),
#                   db2_sleep_avg = mean(mindcog_db_2, na.rm = TRUE),
#                   db2_sleep_sd = sd(mindcog_db_2, na.rm = TRUE),
#                   db8_wakeful_avg = mean(mindcog_db_8, na.rm = TRUE),
#                   db9_sad_avg = mean(mindcog_db_9, na.rm = TRUE),
#                   db9_sad_sd = sd(mindcog_db_9, na.rm = TRUE),
#                   db10_satisfied_avg = mean(mindcog_db_10, na.rm = TRUE),
#                   db10_satisfied_sd = sd(mindcog_db_10, na.rm = TRUE),
#                   db11_irritated_avg = mean(mindcog_db_11, na.rm = TRUE),
#                   db12_energetic_avg = mean(mindcog_db_12, na.rm = TRUE),
#                   db13_restless_avg = mean(mindcog_db_13, na.rm = TRUE),
#                   db14_stressed_avg = mean(mindcog_db_14, na.rm = TRUE),
#                   db15_anxious_avg = mean(mindcog_db_15, na.rm = TRUE),
#                   db16_listless_avg = mean(mindcog_db_16, na.rm = TRUE),
#                   db18_worrying_avg = mean(mindcog_db_18, na.rm = TRUE),
#                   db19_stickiness_avg = mean(mindcog_db_19, na.rm = TRUE),
#                   db20_easeThoughts_avg = mean(mindcog_db_20, na.rm = TRUE),
#                   db24_distracted_avg = mean(mindcog_db_24, na.rm = TRUE),
#                   db25_restOfDayPos_avg = mean(mindcog_db_25, na.rm = TRUE),
#                   db27_companyPos_avg = mean(mindcog_db_27, na.rm = TRUE),
#                   db28_solitudePos_avg = mean(mindcog_db_28, na.rm = TRUE),
#                   db29_enjoyabilityMax_avg = mean(mindcog_db_29, na.rm = TRUE),
#                   db30_intensityPos_avg = mean(mindcog_db_30, na.rm = TRUE),
#                   db31_unpleasantMax_avg = mean(mindcog_db_31, na.rm = TRUE),
#                   db32_intensityNeg_avg = mean(mindcog_db_32, na.rm = TRUE))



#################################### Initial analyses by group and intervention ####################################

int_avgs <- ddply(data, .(group, intervention), plyr::summarize,
                  n_Subj = length(unique(respondent_id)),
                  db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                  db9_sad_avg = mean(sad, na.rm = TRUE),
                  db9_sad_sd = sd(sad, na.rm = TRUE),
                  db10_satisfied_avg = mean(satisfied, na.rm = TRUE),
                  db10_satisfied_sd = sd(satisfied, na.rm = TRUE),
                  db11_irritated_avg = mean(irritated, na.rm = TRUE),
                  db12_energetic_avg = mean(energetic, na.rm = TRUE),
                  db13_restless_avg = mean(restless, na.rm = TRUE),
                  db14_stressed_avg = mean(stressed, na.rm = TRUE),
                  db15_anxious_avg = mean(anxious, na.rm = TRUE),
                  db16_listless_avg = mean(listless, na.rm = TRUE),
                  db18_worrying_avg = mean(worried, na.rm = TRUE),
                  db19_stickiness_avg = mean(stickiness, na.rm = TRUE),
                  db20_easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                  db24_distracted_avg = mean(distracted, na.rm = TRUE),
                  db25_restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                  db27_companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                  db28_solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                  db29_enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                  db30_intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                  db31_unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                  db32_intensityNeg_avg = mean(negIntensity, na.rm = TRUE))

#boxplot comparisons
meltData1 <- melt(data[, c("group", "intervention", metricCols[1:9])])
meltData1 <- meltData1 %>% drop_na(intervention)
meltData1$grInt <- sprintf("%s.%s", as.character(meltData1$group), meltData1$intervention)
#boxplot(data=meltData, value~variable)


p1 <- ggplot(meltData1, aes(factor(variable), value, fill = grInt)) #interaction = intervention)) 
p1 + geom_boxplot() + facet_wrap(~variable, scale="free")


meltData2 <- melt(data[, c("group", "intervention", metricCols[10:18])])
meltData2 <- meltData2 %>% drop_na(intervention)
meltData2$grInt <- sprintf("%s.%s", as.character(meltData2$group), meltData2$intervention)
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = grInt)) #interaction = intervention)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")


phase_avgs <- ddply(data, .(group, intervention, phase), plyr::summarize,
                  n_Subj = length(unique(respondent_id)),
                  db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                  db9_sad_avg = mean(sad, na.rm = TRUE),
                  db9_sad_sd = sd(sad, na.rm = TRUE),
                  db10_satisfied_avg = mean(satisfied, na.rm = TRUE),
                  db10_satisfied_sd = sd(satisfied, na.rm = TRUE),
                  db11_irritated_avg = mean(irritated, na.rm = TRUE),
                  db12_energetic_avg = mean(energetic, na.rm = TRUE),
                  db13_restless_avg = mean(restless, na.rm = TRUE),
                  db14_stressed_avg = mean(stressed, na.rm = TRUE),
                  db15_anxious_avg = mean(anxious, na.rm = TRUE),
                  db16_listless_avg = mean(listless, na.rm = TRUE),
                  db18_worrying_avg = mean(worried, na.rm = TRUE),
                  db19_stickiness_avg = mean(stickiness, na.rm = TRUE),
                  db20_easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                  db24_distracted_avg = mean(distracted, na.rm = TRUE),
                  db25_restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                  db27_companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                  db28_solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                  db29_enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                  db30_intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                  db31_unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                  db32_intensityNeg_avg = mean(negIntensity, na.rm = TRUE))


for(g in c("controls", "remitted")){
  #boxplot comparisons
  meltData3 <- melt(data[data$group == g, c("intervention", "phase", metricCols[1:9])])
  meltData3 <- meltData3 %>% drop_na(intervention)
  meltData3 <- meltData3 %>% drop_na(phase)
  meltData3$interventionPhase <- sprintf("%s.%s", as.character(meltData3$intervention), meltData3$phase)
  meltData3$interventionPhase <- factor(meltData3$interventionPhase,
                                        levels = c("fantasizing.pre", "fantasizing.peri", "mindfulness.pre",
                                                   "mindfulness.peri"))
  meltData3 <- meltData3 %>% drop_na(interventionPhase)
  
  p1 <- ggplot(meltData3, aes(factor(variable), value, fill = interventionPhase)) #interaction = intervention)) 
  p1 <- p1 + geom_boxplot() + facet_wrap(~variable, scale="free") +
    ggtitle(g)
  print(p1)
  
  meltData4 <- melt(data[data$group == g, c("intervention", "phase", metricCols[10:18])])
  meltData4 <- meltData4 %>% drop_na(intervention)
  meltData4 <- meltData4 %>% drop_na(phase)
  meltData4$interventionPhase <- sprintf("%s.%s", as.character(meltData4$intervention), meltData4$phase)
  meltData4$interventionPhase <- factor(meltData4$interventionPhase,
                                        levels = c("fantasizing.pre", "fantasizing.peri", "mindfulness.pre",
                                                   "mindfulness.peri"))
  meltData4 <- meltData4 %>% drop_na(interventionPhase)
  
  #boxplot(data=meltData, value~variable)
  
  p2 <- ggplot(meltData4, aes(factor(variable), value, fill = interventionPhase)) #interaction = intervention)) 
  p2 <- p2 + geom_boxplot() + facet_wrap(~variable, scale="free") +
    ggtitle(g)
  print(p2)
}




#################################### naive time series plots ####################################
#developments over time
time_avgs <- ddply(data, .(group, intervention, blockBeepNum), plyr::summarize,
                  #n_Subj = length(unique(respondent_id)),
                  NA_avg = mean(sumNA, na.rm = TRUE),
                  PA_avg = mean(sumPA, na.rm = TRUE),
                  sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  wakeful_avg = mean(wakeful, na.rm = TRUE),
                  down_avg = mean(down, na.rm = TRUE),
                  down_sd = sd(down, na.rm = TRUE),
                  satisfied_avg = mean(satisfied, na.rm = TRUE),
                  satisfied_sd = sd(satisfied, na.rm = TRUE),
                  irritated_avg = mean(irritated, na.rm = TRUE),
                  energetic_avg = mean(energetic, na.rm = TRUE),
                  restless_avg = mean(restless, na.rm = TRUE),
                  stressed_avg = mean(stressed, na.rm = TRUE),
                  anxious_avg = mean(anxious, na.rm = TRUE),
                  listless_avg = mean(listless, na.rm = TRUE),
                  ruminating_avg = mean(ruminating, na.rm = TRUE),
                  stickiness_avg = mean(stickiness, na.rm = TRUE),
                  easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                  distracted_avg = mean(distracted, na.rm = TRUE),
                  restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                  companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                  solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                  enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                  intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                  unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                  intensityNeg_avg = mean(negIntensity, na.rm = TRUE))

time_avgs$grInt <- paste(time_avgs$group, time_avgs$intervention, sep = "-")
time_avgs <- drop_na(time_avgs, intervention)

dependent_vars = c("NA_avg", "PA_avg", "sleep_avg", "wakeful_avg", "down_avg", "satisfied_avg", "irritated_avg",
                   "energetic_avg", "restless_avg", "stressed_avg", "anxious_avg", "listless_avg",
                   "ruminating_avg", "stickiness_avg", "easeThoughts_avg", "distracted_avg", "restOfDayPos_avg",
                   "companyPos_avg", "solitudePos_avg", "enjoyabilityMax_avg", "intensityPos_avg",
                   "unpleasantMax_avg", "intensityNeg_avg")

for(var in dependent_vars){
  meltData <- melt(drop_na(time_avgs, grInt), id = c( "blockBeepNum", "group", "intervention"), measure.vars = var)
  
  p1 <- ggplot(meltData, aes(blockBeepNum, value, color = factor(group), group = variable)) +
          geom_point() +
          facet_grid(intervention ~ group) +
          geom_smooth(method='lm', formula = "y ~ x") +
          labs(y = var, x = "Block Beep Number") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
                #strip.background = element_rect(fill = "lightblue"),
                legend.position="None")

  print(p1)
  
}

#per phase
# for(var in dependent_vars){
#   meltData <- melt(drop_na(time_avgs, grInt), id = c( "beepNum", "grInt", "phase"), measure.vars = var)
#   
#   p1 <- ggplot(meltData, aes(beepNum, value, color = factor(grInt), group = variable)) +
#     geom_point() +
#     facet_grid("grInt ~ phase") +
#     geom_smooth(method='lm', formula = "y ~ x") +
#     labs(y = var, x = "Assessment Number") +
#     theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
#           #strip.background = element_rect(fill = "lightblue"),
#           legend.position="None")
#   
#   print(p1)
#   
# }

#developments over time per phase
phase_avgs <- ddply(data, .(group, intervention, block, phase, blockBeepNum), plyr::summarize,
                   #n_Subj = length(unique(respondent_id)),
                   sleep_avg = mean(sleepQuality, na.rm = TRUE),
                   sleep_sd = sd(sleepQuality, na.rm = TRUE),
                   wakeful_avg = mean(wakeful, na.rm = TRUE),
                   sad_avg = mean(sad, na.rm = TRUE),
                   sad_sd = sd(sad, na.rm = TRUE),
                   satisfied_avg = mean(satisfied, na.rm = TRUE),
                   satisfied_sd = sd(satisfied, na.rm = TRUE),
                   irritated_avg = mean(irritated, na.rm = TRUE),
                   energetic_avg = mean(energetic, na.rm = TRUE),
                   restless_avg = mean(restless, na.rm = TRUE),
                   stressed_avg = mean(stressed, na.rm = TRUE),
                   anxious_avg = mean(anxious, na.rm = TRUE),
                   listless_avg = mean(listless, na.rm = TRUE),
                   worrying_avg = mean(worried, na.rm = TRUE),
                   stickiness_avg = mean(stickiness, na.rm = TRUE),
                   easeThoughts_avg = mean(thoughtsPleasant, na.rm = TRUE),
                   distracted_avg = mean(distracted, na.rm = TRUE),
                   restOfDayPos_avg = mean(restOfDayPos, na.rm = TRUE),
                   companyPos_avg = mean(companyPleasant, na.rm = TRUE),
                   solitudePos_avg = mean(alonePleasant, na.rm = TRUE),
                   enjoyabilityMax_avg = mean(posMax, na.rm = TRUE),
                   intensityPos_avg = mean(posIntensity, na.rm = TRUE),
                   unpleasantMax_avg = mean(negMax, na.rm = TRUE),
                   intensityNeg_avg = mean(negIntensity, na.rm = TRUE))

phase_avgs$grInt <- paste(phase_avgs$group, phase_avgs$intervention, sep = "-")
phase_avgs <- drop_na(phase_avgs, intervention)
phase_avgs <- drop_na(phase_avgs, phase)

# dependent_vars = c("sleep_avg", "wakeful_avg", "sad_avg", "satisfied_avg", "irritated_avg",
#                    "energetic_avg", "restless_avg", "stressed_avg", "anxious_avg", "listless_avg",
#                    "worrying_avg", "stickiness_avg", "easeThoughts_avg", "distracted_avg", "restOfDayPos_avg",
#                    "companyPos_avg", "solitudePos_avg", "enjoyabilityMax_avg", "intensityPos_avg",
#                    "unpleasantMax_avg", "intensityNeg_avg")

dependent_vars = c("sleep_avg", "wakeful_avg", "sad_avg", "satisfied_avg", "irritated_avg",
                   "energetic_avg", "restless_avg", "stressed_avg", "anxious_avg", "listless_avg",
                   "worrying_avg", "stickiness_avg", "easeThoughts_avg", "distracted_avg",
                   "intensityPos_avg", "intensityNeg_avg")

for(var in dependent_vars){
  meltData <- melt(drop_na(phase_avgs, grInt),
                   id = c("block", "phase", "blockBeepNum", "grInt"), measure.vars = var)
  
  p1 <- ggplot(meltData, aes(blockBeepNum, value, color = factor(grInt), group = variable)) +
    geom_point() +
    facet_grid("grInt") +
    geom_smooth(method='lm', formula = "y ~ x") +
    labs(y = var, x = "Assessment Number") +
    theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
          #strip.background = element_rect(fill = "lightblue"),
          legend.position="None") #+
    #geom_vline(xintercept = max(phase_avgs[phase_avgs$phase == "pre1",]$phaseBeepNum)) +
    #geom_vline(xintercept = max(phase_avgs[phase_avgs$phase == "pre2",]$phaseBeepNum))
  
  print(p1)
  
}

#################################### Correlation Matrix ####################################

#creating a correlation matrix with numeric data
corrMat <- as.matrix(data[, metricCols])

#calculate the correlations
res <- rcorr(corrMat, type = c("pearson"))

corrplot(res$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "All participants")


#separately for remitted and controls
corrMatRm <- as.matrix(data[data$group == "remitted", metricCols])
corrMatCont <- as.matrix(data[data$group == "controls", metricCols])

#calculate the correlations
resRm <- rcorr(corrMatRm, type = c("pearson"))
resCont <- rcorr(corrMatCont, type = c("pearson"))


corrplot(resRm$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "Remitted")

corrplot(resCont$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "Controls")

par(mfrow = c(1,2))
corrplot(resRm$r, method = "number", order = 'alphabet', main = "Remitted")
corrplot(resCont$r, method = "number", order = 'alphabet', main = "Controls")

par(mfrow = c(1,1))

#################################### Individual plots ####################################

dependent_var <- "stickiness"

remittedIDs <-  unique(data[data$group == "remitted",]$respondent_id) #get a list of all unique IDs
set.seed(125)
remittedSample <- sample(remittedIDs, 3)

contIDs <-  unique(data[data$group == "controls",]$respondent_id) #get a list of all unique IDs
contSample <- sample(contIDs, 3)

randData1 <- data[data$respondent_id %in% remittedSample,]
randData2 <- data[data$respondent_id %in% contSample,]

meltData1 <- melt(randData1, id = c("respondent_id", "assessmentDay", "beepNum"), measure.vars = dependent_var)
meltData2 <- melt(randData2, id = c("respondent_id", "assessmentDay", "beepNum"), measure.vars = dependent_var)

#for x: use beepNum for time or another variable to see relationship
p1 <- ggplot(meltData1,aes(x=beepNum,y=value,colour=factor(respondent_id), group = variable)) +
  geom_line() + geom_smooth(method='lm')
p2 <- ggplot(meltData2,aes(x=beepNum,y=value,colour=factor(respondent_id), group = variable)) +
  geom_line() + geom_smooth(method='lm')

#to split one plot into a grid of multiple plots
p1 <- p1 + facet_grid(rows = vars(factor(respondent_id))) + xlab("Assessment Number") + ylab(dependent_var)
p2 <- p2 + facet_grid(rows = vars(factor(respondent_id))) + xlab("Assessment Number") + ylab(dependent_var)

p1 <- p1 + theme(legend.position="None")
p2 <- p2 + theme(legend.position="None") #+ geom_vline(xintercept = assessmentDay, linetype = "dashed", color = "red")

figure <- ggarrange(p1, p2,
                    labels = c("Remitted", "Controls"),
                    ncol = 2, nrow = 1)#, common.legend = TRUE)
figure


#test <- subset(data[data$respondent_id == 3602171,], select = c(respondent_id, mindcog_db_protocol, assessmentDay, beepNum, mindcog_db_open_from))


#################################### Rumination measures ##################################

pc_time <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                  N = length(group[which(!is.na(thoughtsTime))]),
                  past = round(length(group[which(thoughtsTime == 1)])/N, 2),
                  present = round(length(group[which(thoughtsTime == 2)])/N, 2),
                  future = round(length(group[which(thoughtsTime == 3)])/N, 2))

pc_time <- drop_na(pc_time, intervention)

pc_val <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                 N = length(group[which(!is.na(thoughtsValence))]),
                 negative = round(length(group[which(thoughtsValence == 1)])/N, 2),
                 neutral = round(length(group[which(thoughtsValence == 2)])/N, 2),
                 positive = round(length(group[which(thoughtsValence == 3)])/N, 2))

pc_val <- drop_na(pc_val, intervention)


pc_object <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                   N = length(group[which(!is.na(thoughtsObject))]),
                   self = round(length(group[which(thoughtsObject == 1)])/N, 2),
                   somebody = round(length(group[which(thoughtsObject == 2)])/N, 2),
                   neither = round(length(group[which(thoughtsObject == 3)])/N, 2))

pc_object <- drop_na(pc_object, intervention)


pc_thinkingOf <- ddply(data, .(group, intervention, phase, block), plyr::summarize,
                   N = length(group[which(!is.na(thinkingOf))]),
                   currentActivity = round(length(group[which(thinkingOf == 1)])/N, 2),
                   externalStimuli = round(length(group[which(thinkingOf == 2)])/N, 2),
                   currentFeelings = round(length(group[which(thinkingOf == 3)])/N, 2),
                   personalConcerns = round(length(group[which(thinkingOf == 4)])/N, 2),
                   daydreaming = round(length(group[which(thinkingOf == 5)])/N, 2),
                   other = round(length(group[which(thinkingOf == 6)])/N, 2))

pc_thinkingOf <- drop_na(pc_thinkingOf, intervention)

merge(pc_time, pc_val, pc_object, pc_thinkingOf, by = c(group, intervention, phase, block))

pc_summary <- Reduce(function(x, y) merge(x, y, all=TRUE), list(pc_time, pc_val, pc_object, pc_thinkingOf))
