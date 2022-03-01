setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/mindcog_v202202-2")

#read in data
data <- read.csv('mindcog_db_2022-02-14.csv', sep = ";") 

#drop unnecessary columns
data <- subset(data, select = -c(roqua_id, hide_pii_from_researchers,
                                 hide_values_from_professionals, respondent_label))

#number of respondents (i.e., participants?) so far
length(unique(data$respondent_id)) #130




















