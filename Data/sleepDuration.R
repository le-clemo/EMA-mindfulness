ESM_output <- read.csv("C:/Users/cschi/Documents/Studium/Master_BCN_RUG/2_Semester/2_Minor_Project/Data/ESM_roqua/ESM_preprocessed_g2.csv", header=TRUE)

ESM_output <- ESM_output %>% add_column(sleepDuration = NA)
ESM_output <- ESM_output %>% add_column(sleepDuration2 = NA)
ESM_output <- ESM_output %>% add_column(actualSleepDuration = NA)

# rename a column
names(ESM_output)[names(ESM_output)=="sleepLatency"] <- "durationFallAsleep"

# iterate through ESM file
for (i in 1:nrow(ESM_output)) {
  if ((ESM_output$firstEntry[i] != 1) || (is.na(ESM_output$firstEntry[i]))){
    # skip rows if they do not contain the 1st entry per day
    next
  }
  
  # copy relevant sleep data
  bedtimeHour <- ESM_output$toBedHour[i]
  bedtimeMinute <- ESM_output$toBedMinute[i]
  sleepHour <- ESM_output$trySleepHour[i]
  sleepMinute <- ESM_output$trySleepMinute[i]
  gotupHour <- ESM_output$wakeupHour[i]
  gotupMinute <- ESM_output$wakeupMinute[i]
  durationFallAsleep <- ESM_output$durationFallAsleep[i] #sleepLatency
  
  # if people used PM time format for bedtime
  # assuming they usually go to be some time between 9pm and 9am, otherwise this could create a mistake
  if (bedtimeHour > 8 && bedtimeHour < 13) {
    bedtimeHour <- bedtimeHour+12
  }
  if (sleepHour > 8 && sleepHour < 13) {
    sleepHour <- sleepHour+12
  }
  
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
  actualSleepDuration <- sleepDuration2-durationFallAsleep
  
  ESM_output$sleepDuration[i] <- sleepDuration
  ESM_output$sleepDuration2[i] <- sleepDuration2
  ESM_output$actualSleepDuration[i] <- actualSleepDuration
  
}

# order by id
ESM_output <- ESM_output[order(ESM_output$id),]
# uncomment to save output as csv
write.csv(ESM_output, paste(my_path,"ESM_", my_group,"_incl-sleepDuration.csv",sep=""), row.names=FALSE)