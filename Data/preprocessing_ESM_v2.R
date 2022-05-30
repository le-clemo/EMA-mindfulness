
#################################### Set up ####################################
rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

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
data <- read.csv('mindcog_db_2022-05-19.csv', sep = ";") 


################################# load Medoq info and clean up #################################################
#get sheet names
#sheetnames <- excel_sheets('Medoq_informatie_2.xlsx')
mylist <- lapply(excel_sheets('Medoq_informatie_2.xlsx'), read_excel, path = 'Medoq_informatie_2.xlsx')

# name the dataframes
names(mylist) <- c("matchingFantasizing", "matchingMindfulness")

# Bring the dataframes to the global environment
list2env(mylist ,.GlobalEnv)

matchingMindfulness$intervention <- "mindfulness" #add intervention type per sheet
matchingFantasizing$intervention <- "fantasizing"
matchingData <- rbind(matchingMindfulness, matchingFantasizing) #bind into one df

#changing column names since spaces lead to weird errors
colnames(matchingData)[c(1,2,4,5,6)] <- c("id", "recordedDates", "meeting_id", "DatesBaseline", "DatesIntervention")

#turn all ids to lower case for easier error handling below
matchingData$id <- tolower(matchingData$id)

#fix various errors in id column
for(row in 1:nrow(matchingData)) { #some "s" are missing
  if( ! (grepl("s", matchingData$id[row], fixed=TRUE)) & (!(is.na(matchingData$id[row])))){ 
    matchingData$id[row] <- paste("s", matchingData$id[row], sep = "")
  }  
  if( ! (grepl("_g", matchingData$id[row], fixed=TRUE))){ #some "_" prior to "g" are missing
    matchingData$id[row] <- sub("g", "\\1_g", matchingData$id[row])
  }
  if( ! (grepl("_m", matchingData$id[row], fixed = TRUE))){ #some "_" prior to "m" are missing
    matchingData$id[row] <- sub("m", "\\1_m", matchingData$id[row])
  }
  if( (grepl("meting", matchingData$id[row], fixed = TRUE))){#some had "meting" instead of just "m"
    matchingData$id[row] <- sub("meting", "\\1m", matchingData$id[row])
  }
  
}

#there are some duplicate entries that would cause issues later on
n_occur <- data.frame(table(matchingData$id))
#n_occur[n_occur$Freq > 1,]
duplicate_entries <- subset(matchingData[matchingData$id %in% n_occur$Var1[n_occur$Freq > 1],],
                            select = c(id, recordedDates, DatesBaseline, DatesIntervention))


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


# #s3_g1_m3 and s3_g1_m4 have duplicate entries in MatchingData (once without corrected dates) --> remove wrong one
# #View(matchingData[which((matchingData$id=="s3_g1_m3") | (matchingData$id=="s3_g1_m4") ),])
# matchingData <- matchingData[!(matchingData$id == "s3_g1_m3" & is.na(matchingData$recordedDates)),]
# matchingData <- matchingData[!(matchingData$id == "s3_g1_m4" & is.na(matchingData$recordedDates)),]
##################################### combine data ####################################
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
      
      #if(is.na(data$mindcog_db_non_response[i])){
      data$baselineStart[i] = matchingData$baselineStart[j]
      data$baselineEnd[i] = matchingData$baselineEnd[j]
      data$interventionStart[i] <- matchingData$interventionStart[j]
      data$interventionEnd[i] <- matchingData$interventionEnd[j]
      data$recordedStart[i] <- matchingData$recordedStart[j]
      data$recordedEnd[i] <- matchingData$recordedEnd[j]
      #}
    }
  }
}

# View(subset(data, id=="s156_g2_m1" | id=="s156_g2_m2"))
# unique(subset(data, id=="s156_g2_m1" | id=="s156_g2_m2")$patient_id)
#################################### add variables  ####################################
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
                                             format = "%d-%m-%Y %H:%M:%S")

data[['mindcog_db_started_at']] <- as.POSIXct(data[['mindcog_db_started_at']],
                                              format = "%d-%m-%Y %H:%M:%S")

data[['mindcog_db_completed_at']] <- as.POSIXct(data[['mindcog_db_completed_at']],
                                                format = "%d-%m-%Y %H:%M:%S")

data[['mindcog_db_date']] <- format(as.POSIXct(data[['mindcog_db_date']],
                                                format = "%d-%m-%Y %H:%M:%S"), format="%Y-%m-%d")



################################# What data are we missing? ###################################
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

#View(data[which(is.na(data$mindcog_db_date)),])

#View(data[which(is.na(data$phase)),])

############################# Handle issue with diverging dates (1/2) ###########################
#fix problem with dates (whether entry belongs to pre- or peri-intervention phase)
# error_demo <- ddply(data[which(data$subject=="s3"),],
#                     .(subject, id, phase, block, mindcog_db_date, recordedStart, recordedEnd,
#                       baselineStart, baselineEnd, interventionStart, interventionEnd), plyr::summarise,
#                     nEntries <- length(subject))

#turning the recordedStart/-End, baselineStart/-End, ... into actual dates with corresponding year
for(row in 1:nrow(data)){ #first extract year from the mindcog_db_date column
  y <- format(as.POSIXct(data$mindcog_db_open_from[row], format = "%Y-%m-%d %H:%M:%S"), format="%Y")
  #if there is an entry for recordedStart and mindcog_db_date (i.e., if its not a non-response)
  if((!is.na(data$recordedStart[row])) & (!is.na(data$mindcog_db_open_from[row]))){
    #combine day-month from recordedStart (etc.) with extracted year and reformat to match other dates in df
    data$recordedStart[row] <- format(as.POSIXct(paste(data$recordedStart[row], y, sep = "-"),
                                                 format = "%d-%m-%Y"), format = "%Y-%m-%d")
    data$recordedEnd[row] <- format(as.POSIXct(paste(data$recordedEnd[row], y, sep = "-"),
                                               format = "%d-%m-%Y"), format = "%Y-%m-%d")
  }# repeat for other columns
  if((!is.na(data$baselineStart[row])) & (!is.na(data$mindcog_db_open_from[row]))){
    data$baselineStart[row] <- format(as.POSIXct(paste(data$baselineStart[row], y, sep = "-"),
                                                 format = "%d-%m-%Y"), format = "%Y-%m-%d")
    data$baselineEnd[row] <- format(as.POSIXct(paste(data$baselineEnd[row], y, sep = "-"),
                                               format = "%d-%m-%Y"), format = "%Y-%m-%d")
    data$interventionStart[row] <- format(as.POSIXct(paste(data$interventionStart[row], y, sep = "-"),
                                                     format = "%d-%m-%Y"), format = "%Y-%m-%d")
    data$interventionEnd[row] <- format(as.POSIXct(paste(data$interventionEnd[row], y, sep = "-"),
                                                   format = "%d-%m-%Y"), format = "%Y-%m-%d")
  }
}


############################### add actual dates to matching Data for later use with SART data ####################
for(i in 1:nrow(data)){
  for(j in 1:nrow(matchingData)){
    if(data$patient_id[i] == matchingData$meeting_id[j]){
      matchingData$baselineStart[j] <- data$baselineStart[i]
      matchingData$baselineEnd[j] <- data$baselineEnd[i]
      matchingData$interventionStart[j] <- data$interventionStart[i]
      matchingData$interventionEnd[j] <- data$interventionEnd[i]
    }
  }
}

write.csv(matchingData, file = "matchingData.csv")

############################# Handle issue with diverging dates (2/2) ###########################
pre_to_peri <- c() #empty lists for row indices of faulty entries
peri_to_pre <- c()
i <- 1 #to add to the lists (in a computationally efficient way)
j <- 1
for(row in 1:nrow(data)){
  #if there is a date in baselineStart (which means there is one in the other relevant columns too)
  # and if it is not a non-response
  if((!is.na(data$baselineStart[row])) & (!is.na(data$mindcog_db_open_from[row]))){
    #if phase is "pre"
    if((!is.na(data$phase[row])) & (data$phase[row] == "pre")){
      #if the recorded mindcog date is greater than the end of baseline date
      if((format(data$mindcog_db_open_from[row], format = "%Y-%m-%d") > data$baselineEnd[row])){
        pre_to_peri[i] <- row #add row index to list
        i <- i+1 #increment list index count
      }
    } #same for entries coded as peri that should be pre
    if((!is.na(data$phase[row])) & (data$phase[row] == "peri")){
      if((format(data$mindcog_db_open_from[row], format = "%Y-%m-%d") < data$interventionStart[row])){
        peri_to_pre[j] <- row
        j <- j+1
      }
    }
  }
}

# View(data[pre_to_peri,])
# View(data[peri_to_pre,])

length(pre_to_peri)
length(peri_to_pre)
pre_to_peri_df <- (subset(data[pre_to_peri,], select=c("id", "mindcog_db_open_from", "phase", "baselineStart",
                                                       "baselineEnd", "interventionStart", "interventionEnd")))

peri_to_pre_df <- (subset(data[peri_to_pre,], select=c("id", "mindcog_db_open_from", "phase", "baselineStart",
                                                       "baselineEnd", "interventionStart", "interventionEnd")))

data[pre_to_peri,]$phase <- "peri"
for(row in pre_to_peri){
  if((grepl("m1", data$id[row], fixed=TRUE))){
    data$id[row] <- sub("m1", "\\m2", data$id[row])
  }
  if((grepl("m3", data$id[row], fixed=TRUE))){
    data$id[row] <- sub("m3", "\\m4", data$id[row])
  }
}

data[peri_to_pre,]$phase <- "pre"
for(row in peri_to_pre){
  if((grepl("m2", data$id[row], fixed=TRUE))){
    data$id[row] <- sub("m2", "\\m1", data$id[row])
  }
  if((grepl("m4", data$id[row], fixed=TRUE))){
    data$id[row] <- sub("m4", "\\m3", data$id[row])
  }
}



######################################## Changing ESM item names ##############################
#Get numbers of ESM item columns
item1 <- which( colnames(data)=="mindcog_db_1" ) #this is the first column that interests us
item33 <- which( colnames(data)=="mindcog_db_33" )#last item of interest
colNamesOld <- setNames(data.frame(colnames(data[, item1:item33])), "columns")

colNamesNew <- c('firstEntry', 'sleepQuality', 'toBedHour', 'toBedMinute', 'trySleepHour', 'trySleepMinute',
                 'sleepLatency', 'wakeupHour', 'wakeupMinute', 'restednessWakeup', 'wakeful',
                 'down', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
                 'listless', 'thinkingOf', 'ruminating', 'stickiness', 'thoughtsPleasant',
                 'thoughtsTime', 'thoughtsValence', 'thoughtsObject', 'distracted',
                 'restOfDayPos', 'aloneCompany', 'companyPleasant', 'alonePleasant', 'posMax',
                 'posIntensity', 'negMax', 'negIntensity', 'comments')

setnames(data, old = colNamesOld$columns, new = colNamesNew)

################################### Sleep Measures ####################################
# data$xNightDate <- as.POSIXct("2000-01-01", format = "%Y-%m-%d") #arbitrary date (earlier day)
# data$toBedTime <- as.POSIXct(paste(data$xNightDate, paste(data$toBedHour, data$toBedMinute, "00", sep = ":"), sep = " "),
#                              format = "%Y-%m-%d %H:%M:%S")
# 
# data$xMorningDate <- as.POSIXct("2000-01-02", format = "%Y-%m-%d") #arbitrary date ('next' day)
# data$outOfBedTime <- as.POSIXct(paste(data$xMorningDate, paste(data$wakeupHour, data$wakeupMinute, "00", sep = ":"), sep = " "),
#                              format = "%Y-%m-%d %H:%M:%S")
# 
# data$sleepDuration <- difftime(data$outOfBedTime, data$toBedTime)

# Code von Clara Schier
data <- data %>% add_column(sleepDuration = NA)
data <- data %>% add_column(sleepDuration2 = NA)
data <- data %>% add_column(actualSleepDuration = NA)


# iterate through ESM file
for (i in 1:nrow(data)) {
  if ((data$firstEntry[i] != 1) || (is.na(data$firstEntry[i]))){
    # skip rows if they do not contain the 1st entry per day
    next
  }
  
  # copy relevant sleep data
  bedtimeHour <- data$toBedHour[i]
  bedtimeMinute <- data$toBedMinute[i]
  sleepHour <- data$trySleepHour[i]
  sleepMinute <- data$trySleepMinute[i]
  gotupHour <- data$wakeupHour[i]
  gotupMinute <- data$wakeupMinute[i]
  sleepLatency <- data$sleepLatency[i] #sleepLatency
  
  # if people used PM time format for bedtime
  # assuming they usually go to be some time between 9pm and 9am, otherwise this could create a mistake
  # if (bedtimeHour > 8 && bedtimeHour < 13) {
  #   bedtimeHour <- bedtimeHour+12
  # }
  # if (sleepHour > 8 && sleepHour < 13) {
  #   sleepHour <- sleepHour+12
  # }
  # 
  # calculate bed/sleep times
  bedtimeAll <- bedtimeHour*60 + bedtimeMinute
  sleepAll <- sleepHour*60 + sleepMinute
  gotupAll <- gotupHour*60 + gotupMinute
  
  sleepDuration <- NA
  sleepDuration2 <- NA
  
  # calculate sleep/bed time duration
  # if bedtime after midnight
  if (gotupAll > bedtimeAll) {
    sleepDuration <- gotupAll - bedtimeAll
  } else {
    # if bedtime before midnight
    gotupAll <- gotupAll+1440
    sleepDuration <- gotupAll - bedtimeAll
  }
  
  # reset gotupAll
  gotupAll <- gotupHour*60 + gotupMinute
  
  # if bedtime after midnight
  if (gotupAll > sleepAll) {
    sleepDuration2 <- gotupAll - sleepAll
  } else {
    # if bedtime before midnight
    gotupAll <- gotupAll+1440
    sleepDuration2 <- gotupAll - sleepAll
  }
  
  
  # if someone put in a wrong time accidentally and bedtime and tryfallasleep don't match up 
  if(sleepDuration2 - sleepDuration > 500){
    sleepDuration2 <- sleepDuration
  } else if(sleepDuration - sleepDuration2 > 500){
    sleepDuration <- sleepDuration2
  }
  
  # total sleep time: sleep duration minus sleep latency
  actualSleepDuration <- sleepDuration2-sleepLatency
  
  data$sleepDuration[i] <- sleepDuration
  data$sleepDuration2[i] <- sleepDuration2
  data$actualSleepDuration[i] <- actualSleepDuration
  
}




################################### Positive / Negative Affect ####################################

# data$meanPA <- rowMeans(subset(data, select = c(wakeful, satisfied, energetic)), na.rm = TRUE)
# data$meanNA <- rowMeans(subset(data, select = c(down, irritated, restless,
#                                                 stressed, anxious, listless)), na.rm = TRUE)

data$sumPA <- NA
data$sumNA <- NA
for(row in 1:nrow(data)){
  if((!is.na(data$wakeful[row])) & (!is.na(data$satisfied[row])) & (!is.na(data$energetic[row]))){
    data$sumPA[row] <- data$wakeful[row] + data$satisfied[row] + data$energetic[row]
  }
  if((!is.na(data$down[row])) & (!is.na(data$irritated[row])) & (!is.na(data$restless[row])) &
     (!is.na(data$anxious[row]))){
    data$sumNA[row] <- data$down[row] + data$irritated[row] + data$restless[row] + data$anxious[row]
  }
}

# data$sumPA <- rowSums(subset(data, select = c(wakeful, satisfied, energetic)), na.rm = TRUE)
# data$sumNA <- rowSums(subset(data, select = c(down, irritated, restless,
#                                                 stressed, anxious, listless)), na.rm = TRUE)


#################################### add measures on response times  ####################################

#calculate the time it took a participant to start after being informed (in minutes)
data$response_delay <- round((data$mindcog_db_started_at - data$mindcog_db_open_from)/60, 2)
#calculate how long it took a participant to complete the questionnaire (in minutes)
data$response_duration <- round((data$mindcog_db_completed_at - data$mindcog_db_started_at)/60, 2)

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


####################### assessment day and lagged variables  #############################


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



#adding half days
data$halfDay <- NA #adding an empty column for assessment days
for(id in subject_IDs){ #for loop to fill the column with the day numbers
  half_day <- 0 
  num_days <- length(unique(data[which(data$subject==id),]$assessmentDay)) #how many assessment days are recorded?
  for(d in 1:num_days){ #for each day
    half_day <- half_day + 1 #increment half_day by 1
    respondent_rows <- which((data$subject == id) & data$assessmentDay==d)#row indices of rows associated with respondent
    numEntries <- length(respondent_rows) #how many entries are associated with that day?
    if(numEntries >= 5){ #if there are more than 5 (a normal day has 10 assessments)
      data[respondent_rows[1:5],]$halfDay <- half_day #set the first half of the day 
    } else { #if there are fewer than 5 assessments for a day, they all belong to the first half day
      data[respondent_rows,]$halfDay <- half_day
      half_day <- half_day + 1 #we then skip one half day 
      break
    }
    #this will only run if the else - break doesn't run, so only if there are at least 5 entries
    half_day <- half_day + 1 
    respondent_rows <- respondent_rows[6:numEntries]#row indices of rows associated with respondent
    data[respondent_rows,]$halfDay <- half_day
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


###################################### fixing remaining issues #######################################
# View(subset(data[which(data$phaseAssessmentDay>7),],
#             select=c("group", "intervention", "id", "phase", "block",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date",
#                      "baselineStart", "baselineEnd", "interventionStart", "interventionEnd")))

#s163 block 2 is all non-responses --> remove
# View(subset(data[which((data$subject=="s163") & (data$block==2)),],
#             select=c("group", "intervention", "id", "phase", "block",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date",
#                      "baselineStart", "baselineEnd", "interventionStart", "interventionEnd")))
data <- data[!(data$subject == "s163" & data$block ==2 ),]

#s81_g1_m3 ESM started one day early (only non response on that day) --> remove first day + adjust assessmentDay columns
# View(subset(data[which(data$subject=="s81"),],
#             select=c("group", "intervention", "id", "phase", "block", "assessmentDay", "blockAssessmentDay",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date",
#                      "baselineStart", "baselineEnd", "interventionStart", "interventionEnd")))

# data <- data[!(data$id == "s81_g1_m3" & data$blockAssessmentDay ==1 ),]
#adjust assessmentDay column
data[((data$subject == "s81") &
        (data$block == 2)),]$assessmentDay = data[((data$subject == "s81") &
                                                     (data$block == 2)),]$assessmentDay -1
#adjust blockAssessmentDay column
data[((data$subject == "s81") &
        (data$block == 2)),]$blockAssessmentDay = data[((data$subject == "s81") &
                                                          (data$block == 2)),]$blockAssessmentDay -1
#adjust phaseAssessmentDay column (not for entire block 2; only for pre-intervention phase)
data[(data$id == "s81_g1_m3"),]$phaseAssessmentDay = data[(data$id == "s81_g1_m3"),]$phaseAssessmentDay -1

#s24_g1_m2 one extra day with only one response --> remove extra day (day 8 of peri phase block 1)
# View(subset(data[which(data$subject=="s24"),],
#             select=c("group", "intervention", "id", "phase", "block", "assessmentDay", "blockAssessmentDay",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date",
#                      "baselineStart", "baselineEnd", "interventionStart", "interventionEnd")))
#remove extra day
data <- data[!(data$id == "s24_g1_m2" & data$phaseAssessmentDay ==8 ),]

#adjust assessmentDay column
data[((data$subject == "s24") &
        (data$block == 2)),]$assessmentDay = data[((data$subject == "s24") &
                                                     (data$block == 2)),]$assessmentDay -1

#s108_g1_m3 12 days in pre 2 and 6 days in pre 2 + a lot of non responses in block 2 --> remove block 2 (?)
# View(subset(data[which(data$subject=="s108"),],
#             select=c("group", "intervention", "id", "phase", "block", "assessmentDay", "blockAssessmentDay",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date",
#                      "baselineStart", "baselineEnd", "interventionStart", "interventionEnd")))

# data <- data[!(data$subject == "s108" & data$block == 2 ),]

################################################## adding half days (must be done after above issues are fixed) ##########################################
#adding half days per phase
data$phaseHalfDay <- NA #adding an empty column for assessment days
for(id in subject_IDs){
 # print(id)
  numBlocks <- max(data[which(data$subject==id),]$block)
  for(b in 1:numBlocks){
    phases <- unique(data[which((data$subject==id) & (data$block==b)),]$phase)
    for(p in phases){
      half_day <- 0 
      num_days <- length(unique(data[which((data$subject==id) & (data$block==b) & (data$phase==p)),]$phaseAssessmentDay)) #how many assessment days are recorded?
      for(d in 1:num_days){ #for each day
        half_day <- half_day + 1 #increment half_day by 1
        respondent_rows <- which((data$subject == id) & (data$phaseAssessmentDay==d) & (data$block==b) & (data$phase==p))#row indices of rows associated with respondent
        numEntries <- length(respondent_rows) #how many entries are associated with that day?
       # print(d)
       # print(numEntries)
        # print(respondent_rows)
        if(numEntries > 5){ #if there are more than 5 (a normal day has 10 assessments)
          data[respondent_rows[1:5],]$phaseHalfDay <- half_day #set the first half of the day 
          half_day <- half_day + 1
          data[respondent_rows[6:numEntries],]$phaseHalfDay <- half_day
          
        } else if(numEntries==5){ #if there are fewer than 5 assessments for a day, they all belong to the first half day
          data[respondent_rows[1:5],]$phaseHalfDay <- half_day #set the first half of the day 
          half_day <- half_day + 1
        } else {
          data[respondent_rows,]$phaseHalfDay <- half_day
          half_day <- half_day + 1 #we then skip one half day 
          #break
        }
      }
    }
  }#for loop to fill the column with the day numbers
}




#adding half days per block
data$blockHalfDay <- NA #adding an empty column for assessment days
for(id in subject_IDs){
 # print(id)
  numBlocks <- max(data[which(data$subject==id),]$block)
  for(b in 1:numBlocks){
    half_day <- 0 
    num_days <- length(unique(data[which((data$subject==id) & (data$block==b)),]$blockAssessmentDay)) #how many assessment days are recorded?
    for(d in 1:num_days){ #for each day
      half_day <- half_day + 1 #increment half_day by 1
      respondent_rows <- which((data$subject == id) & (data$blockAssessmentDay==d) & (data$block==b))#row indices of rows associated with respondent
      numEntries <- length(respondent_rows) #how many entries are associated with that day?
      #print(d)
     # print(numEntries)
      # print(respondent_rows)
      if(numEntries > 5){ #if there are more than 5 (a normal day has 10 assessments)
        data[respondent_rows[1:5],]$blockHalfDay <- half_day #set the first half of the day 
        half_day <- half_day + 1
        data[respondent_rows[6:numEntries],]$blockHalfDay <- half_day
        
      } else if(numEntries==5){ #if there are fewer than 5 assessments for a day, they all belong to the first half day
        data[respondent_rows[1:5],]$blockHalfDay <- half_day #set the first half of the day 
        half_day <- half_day + 1
      } else {
        data[respondent_rows,]$blockHalfDay <- half_day
        half_day <- half_day + 1 #we then skip one half day 
        #break
      }
    }
  }#for loop to fill the column with the day numbers
  
}

#View(subset(data[data$subject=="s81",], select = c("subject", "assessmentDay", "blockAssessmentDay", "blockHalfDay", "phaseAssessmentDay", "phaseHalfDay", "mindcog_db_open_from")))

####################################### adding beep numbers (continuous count of queries sent per subject) #################################################
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
  numBlocks <- max(data[which(data$subject==id),]$block) #some participants dropped out after the first block
  for(b in 1:numBlocks){
    for(phase in phases){
      phase_rows <- which((data$subject == id) & (data$phase == phase) & (data$block==b)) #row indices of rows associated with respondent    
      if(length(phase_rows) > 0){
        data[phase_rows,]$phaseBeepNum <- 1:length(phase_rows)
      }
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

############################## Some changes for convenience ################################
#drop unnecessary columns and reorder columns for convenience
data$sleepDuration <- data$actualSleepDuration

columnNames <- c(colnames(data))
data <- data %>% select(patient_id, id, subject, group, intervention, phase, block, respondent_id,
                        #18 = db_open_from; 23 = db_date; 26:61 = firstEntry:comments; 67:76
                        columnNames[18:23], firstEntry, sleepQuality, sleepLatency, sleepDuration,
                        restednessWakeup, columnNames[36:61], sumPA, sumNA, 
                        #67:78 = response measures
                        columnNames[79:90])

# data <- subset(data, select = -c(roqua_id, hide_pii_from_researchers, gender, birth_year,
#                                  hide_values_from_professionals, respondent_label, respondent_type,
#                                  mindcog_db_project, mindcog_db_notes, mindcog_db_location,
#                                  mindcog_db_invited_at, mindcog_db_emailed_at, mindcog_db_variant,
#                                  mindcog_db_anonymous, mindcog_db_protocol, mindcog_db_measurement))

###################################### Creating lagged variables #############################################
#the variables to be lagged
cols <- c('wakeful', 'down', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
             'listless', 'thinkingOf', 'ruminating', 'stickiness', 'thoughtsPleasant', 'thoughtsTime',
             'thoughtsValence', 'thoughtsObject', 'distracted', 'restOfDayPos', 'aloneCompany',
             'companyPleasant', 'alonePleasant', 'posMax', 'posIntensity', 'negMax', 'negIntensity',
              'sumPA', 'sumNA')

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

# ############################################ Change scores ####################################################
# 
# cols <- c('wakeful', 'down', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
#           'listless', 'ruminating', 'stickiness', 'thoughtsPleasant',  'thoughtsObject', 'distracted', 'restOfDayPos',
#           'companyPleasant', 'alonePleasant', 'posMax', 'posIntensity', 'negMax', 'negIntensity',
#           'sumPA', 'sumNA')
# 
# #creating a vector with new "lagged" column names
# changeCols <- c()
# for(col in cols) {
#   changeCols <- c(changeCols, paste(col, "change", sep = "_"))
# }
# 
# #adding empty columns with "lagged" names
# data[, changeCols] <- NA
# 
# #zipping current and lagged column names for the for loop
# colZip <- mapply(c, cols, changeCols, SIMPLIFY = FALSE)
# 
# #for loop to add the lagged values to their corresponding new columns
# for(id in subject_IDs){
#   respondent_rows <- which(data$subject == id) #one respondent at a time
#   
#   for(col in colZip) { #looping over the zipped column name pairs
#     prev_value <- 0
#     change_score <- NA #previous value starts out as NA for every column
#     current_day <- 1 #current day at the beginning of every column per respondent is 1
#     
#     for(row in respondent_rows) { #looping over the rows associated with the current respondent
#       
#       if(data$assessmentDay[row] == current_day) { #if the assessment day matches the current day
#         change_score <- data[row, col[1]] - prev_value #calculate next change score
#         data[row, col[2]] <- change_score #add the change_score as the value for the lagged column
#         prev_value <- data[row, col[1]] #update the previous value
#         
#       } else { #if assessment day and current day do not match
#         data[row, col[2]] <- NA #then the change score should be NA (new day!)
#         current_day <- data$assessmentDay[row] #update the current day (could also just be +1)
#         prev_value <- data[row, col[1]] #update previous value
#       }
#     } 
#   }
# }

# test <- subset(data, select = c(subject, phase, down, down_change, assessmentDay))
# View(test)

write.csv(data, "preprocessed_data.csv", row.names = FALSE)
