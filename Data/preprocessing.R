
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


################################# add id, intervention #################################################
#get sheet names
#sheetnames <- excel_sheets('Medoq_informatie_2.xlsx')
mylist <- lapply(excel_sheets('Medoq_informatie_2.xlsx'), read_excel, path = 'Medoq_informatie_2.xlsx')

# name the dataframes
names(mylist) <- c("matchingMindfulness", "matchingFantasizing")

# Bring the dataframes to the global environment
list2env(mylist ,.GlobalEnv)

matchingMindfulness$intervention <- "mindfulness" #add intervention type per sheet
matchingFantasizing$intervention <- "fantasizing"
matchingData <- rbind(matchingMindfulness, matchingFantasizing) #bind into one df

#changing column names since spaces lead to weird errors
colnames(matchingData)[c(1,2,4,5,6)] <- c("id", "recordedDates", "meeting_id", "DatesBaseline", "DatesIntervention")

#extract start and end dates from "Datum baseline" and "Datum interventie" columns
for(row in 1:nrow(matchingData)) { #change all "t/m" to "tm"
  if( ! (grepl("tm", matchingData$DatesBaseline[row], fixed=TRUE))){ 
    matchingData$DatesBaseline[row] <- sub("t/m", "\\tm", matchingData$DatesBaseline[row])
  }
  if( ! (grepl("tm", matchingData$DatesIntervention[row], fixed=TRUE))){ 
    matchingData$DatesIntervention[row] <- sub("t/m", "\\tm", matchingData$DatesIntervention[row])
  }  
  if( ! (grepl("tm", matchingData$recordedDates[row], fixed=TRUE))){
    matchingData$recordedDates[row] <- sub("t/m", "\\tm", matchingData$recordedDates[row])
  }

}

matchingData$baselineStart <- NA
matchingData$baselineEnd <- NA
matchingData$interventionStart <- NA
matchingData$interventionEnd <- NA
matchingData$recordedStart <- NA
matchingData$recordedEnd <- NA
for(row in 1:nrow(matchingData)){
  if(! is.na(matchingData$DatesBaseline[row])){
    matchingData$baselineStart[row] <- sub("\\ tm.*", "", matchingData$DatesBaseline[row]) #extract start date
    matchingData$baselineEnd[row] <- sub(".* tm", "", matchingData$DatesBaseline[row]) #extract end date
    
    #same for intervention dates
    matchingData$interventionStart[row] <- sub("\\ tm.*", "", matchingData$DatesIntervention[row])
    matchingData$interventionEnd[row] <- sub(".* tm", "", matchingData$DatesIntervention[row])
  }
  if(! is.na(matchingData$recordedDates[row])){
    matchingData$recordedStart[row] <- sub("\\ tm.*", "", matchingData$recordedDates[row])
    matchingData$recordedEnd[row] <- sub(".* tm", "", matchingData$recordedDates[row])
    
  }
}

#performing a "vlookup" of the md... numbers and adding corresponding columns
#from matchingData to data
data$id <- NA
data$intervention <- NA
data$baselineStart <- NA
data$baselineEnd <- NA
data$interventionStart <- NA
data$interventionEnd <- NA
data$recordedStart <- NA
data$recordedEnd <- NA
for(i in 1:nrow(data)){
  for(j in 1:nrow(matchingData)){
    if(data$patient_id[i] == matchingData$meeting_id[j]){
      data$id[i] <- matchingData$id[j]
      #data$recordedDates[i] <- matchingData$recordedDates[j]
      data$intervention[i] <- matchingData$intervention[j]
      
      if(is.na(data$mindcog_db_non_response[i])){
        data$baselineStart[i] = matchingData$baselineStart[j]
        data$baselineEnd[i] = matchingData$baselineEnd[j]
        data$interventionStart[i] <- matchingData$interventionStart[j]
        data$interventionEnd[i] <- matchingData$interventionEnd[j]
        data$recordedStart[i] <- matchingData$recordedStart[j]
        data$recordedEnd[i] <- matchingData$recordedEnd[j]
      }
    }
  }
}

#################################### Data clean up ####################################
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
  if(grepl("g1", data$id[row], fixed = TRUE)){
    data$group[row] <- "controls"
  } 
  if(grepl("g2", data$id[row], fixed = TRUE)){
    data$group[row] <- "remitted"
  }
}

#add new column "subject" -> extract all characters up until the first underscore in column "id"
data$subject <- str_extract(data$id, regex("^[^_]+(?=_)"))

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

#Convert dates from characters to datetimes
data[['mindcog_db_open_from']] <- as.POSIXct(data[['mindcog_db_open_from']],
                                             format = "%d/%m/%Y %H:%M")

data[['mindcog_db_started_at']] <- as.POSIXct(data[['mindcog_db_started_at']],
                                              format = "%d/%m/%Y %H:%M")

data[['mindcog_db_completed_at']] <- as.POSIXct(data[['mindcog_db_completed_at']],
                                                format = "%d/%m/%Y %H:%M")

data[['mindcog_db_date']] <- format(as.POSIXct(data[['mindcog_db_date']],
                                                format = "%d/%m/%Y %H:%M"), format="%d/%m/%Y")


missing_data <- ddply(data, .(patient_id, id, group, intervention), plyr::summarise,
                      numBeeped = length(mindcog_db_open_from),
                      responseRate = round((numBeeped - length(unique(mindcog_db_non_response)))/numBeeped,2))

na_data <- missing_data[(is.na(missing_data$patient_id)) |
                          is.na((missing_data$group)) |
                          is.na((missing_data$intervention)), ]

write.csv(na_data, file = "patientID_issues.csv")


#drop subjects without an assigned group
data <- drop_na(data, group)
data <- drop_na(data, patient_id)

View(data[which(is.na(data$mindcog_db_date)),])

#fix problem with dates (whether entry belongs to pre- or peri-intervention phase)
error_demo <- ddply(data[which(data$subject=="s8"),],
                    .(subject, id, phase, block, mindcog_db_date, recordedStart, recordedEnd,
                      baselineStart, baselineEnd, interventionStart, interventionEnd), plyr::summarise,
                    nEntries <- length(subject))

for(row in 1:nrow(data)){
  if((!is.na(data$recordedStart[row])) & (!is.na(data$mindcog_db_date[row]))){
    y <- format(as.POSIXct(data$mindcog_db_date[row], format = "%d/%m/%Y"), format="%Y")
    data$recordedStart[row] <- paste(data$recordedStart[row], y, sep = "-")
  }
}

if(!is.na(data$baselineStart))
  

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

#################################### add measures on response times  ####################################

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

phases <- c("pre", "peri")
data$phaseAssessmentDay <- NA
for(id in subject_IDs){
  for(p in phases){
    for(block in 1:2){
      assessment_day <- 0
      prev_row = 0
      xDate <- as.Date("01/01/1999", format = '%d/%m/%Y')
      phase_rows <- which((data$subject == id) & (data$block == block) & (data$phase == p))
      if(length(phase_rows) > 0){
        for (row in phase_rows) { #loop over these rows
          #if this rows date is greater than the previous assessment's date
          if(as.Date(data$mindcog_db_open_from[row]) > xDate){
            assessment_day = assessment_day + 1 #increment the assessment day count
            data$phaseAssessmentDay[row] <- assessment_day #and add this new number as assessment day for this row
            prev_row = row #update the previous row index (not plus one bc numbers are not continuous!)
            xDate = as.Date(data$mindcog_db_open_from[row]) #set new comparison date to date of current row
          } else {
            #otherwise this row's date is equal to the previous row's --> same assessment day
            data$phaseAssessmentDay[row] <- data$phaseAssessmentDay[prev_row]
            prev_row = row
          }
        }
      }
    }
  }
}

# View(subset(data[which(data$subject == "s8"),],
#     select = c("patient_id", "id", "intervention", "phase", "mindcog_db_open_from",
#           "assessmentDay", "blockAssessmentDay", "phaseAssessmentDay")))

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

#drop unnecessary columns and reorder columns for convenience

columnNames <- c(colnames(data))
data <- data %>% select(patient_id, id, subject, group, intervention, phase, block,
                        #18 = db_open_from; 23 = db_date; 26:61 = firstEntry:comments; 67:76
                        columnNames[18:23], baselineDates, interventionDates, columnNames[26:61],
                        #67:78 = response measures
                        columnNames[70:78])

# data <- subset(data, select = -c(roqua_id, hide_pii_from_researchers, gender, birth_year,
#                                  hide_values_from_professionals, respondent_label, respondent_type,
#                                  mindcog_db_project, mindcog_db_notes, mindcog_db_location,
#                                  mindcog_db_invited_at, mindcog_db_emailed_at, mindcog_db_variant,
#                                  mindcog_db_anonymous, mindcog_db_protocol, mindcog_db_measurement))

###################################### Creating lagged variables #############################################
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

############################################ Change scores ####################################################

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

write.csv(data, "preprocessed_data.csv", row.names = FALSE)
