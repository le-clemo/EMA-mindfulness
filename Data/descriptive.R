
#################################### Set up ####################################
rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202204")

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
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

#read in data
data <- read.csv('preprocessed_data.csv') 

data$group <- factor(data$group, levels = c("controls", "remitted"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))

################################# response-related measures #####################################
# #group by id and count the number of nonresponses
participant_responses <- ddply(data, .(subject), plyr::summarise,
                               numCompleted = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay))

#number of participants so far
length(unique(data$subject)) #39 associated with a group

#the mean response rate is ~68%
meanResponseRate <- mean(participant_responses$responseRate)
#sd of 18.74
sdResponseRate <- sd(participant_responses$responseRate)


# View(subset(data[which(data$phaseAssessmentDay>7),],
#             select=c("group", "intervention", "id", "phase", "block",
#                      "phaseAssessmentDay", "mindcog_db_open_from", "mindcog_db_non_response", "mindcog_db_date")))

responses_by_block <- ddply(data, .(group, intervention, phase, block, phaseAssessmentDay), plyr::summarise,
                               numCompleted = length(mindcog_db_open_from))

#recreacting with assessment days per group
group_responses <- ddply(data, .(group), plyr::summarise,
                               nSubj = length(unique(subject)),
                               numCompleted = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numCompleted - noResponse,
                               responseRate = round(response/numCompleted,2),
                               numDays = max(assessmentDay)) #6% higher response rate in controls



#################################### Some naive plots ############################################
#for all data
plot(data$sumPA, data$sumNA)

plot(data$sumPA, data$ruminating)

plot(data$sumNA, data$ruminating)


plot(data$ruminating, data$stickiness)
# model.fit <- lm(data$ruminating~data$stickiness)
# summary(model.fit)
# trend= 3.628506 + 0.635861*data$stickiness
# lines(trend,col='green')

#pre vs peri

#create dfs with only one group
controls <- data[which(data$group=="controls"),]
remitted <- data[which(data$group=="remitted"),]

#plot the two groups in different colors
plot(remitted$sumPA, remitted$sumNA, col = "red")
points(controls$sumPA, controls$sumNA, col = "green")

plot(remitted$ruminating, remitted$sumNA, col = "red")
points(controls$ruminating, controls$sumNA, col = "green")

#plot different phases
# plot(remitted[which(remitted$phase=="pre"),]$sumPA, remitted[which(remitted$phase=="pre"),]$sumNA, col = "red")
# points(remitted[which(remitted$phase=="peri"),]$sumPA, remitted[which(remitted$phase=="peri"),]$sumNA, col = "green")
# 
# plot(controls[which(controls$phase=="pre"),]$sumPA, controls[which(controls$phase=="pre"),]$sumNA, col = "red")
# points(controls[which(controls$phase=="peri"),]$sumPA, controls[which(controls$phase=="peri"),]$sumNA, col = "green")

#do the intervention lessen the relationship between negative affect and rumination?
controls_fa <- controls[which(controls$intervention=="fantasizing"),]
controls_mf <- controls[which(controls$intervention=="mindfulness"),]
remitted_fa <- remitted[which(remitted$intervention=="fantasizing"),]
remitted_mf <- remitted[which(remitted$intervention=="mindfulness"),]

plot(remitted_fa[which(remitted_fa$phase=="pre"),]$ruminating_lag1, remitted_fa[which(remitted_fa$phase=="pre"),]$sumNA, col = "red")
points(remitted_fa[which(remitted_fa$phase=="peri"),]$ruminating_lag1, remitted_fa[which(remitted_fa$phase=="peri"),]$sumNA, col = "green")

plot(remitted_mf[which(remitted_fa$phase=="pre"),]$ruminating_lag1, remitted_mf[which(remitted_fa$phase=="pre"),]$sumNA, col = "red")
points(remitted_mf[which(remitted_fa$phase=="peri"),]$ruminating_lag1, remitted_mf[which(remitted_fa$phase=="peri"),]$sumNA, col = "green")

#plot time lines for selected variables
aggData <- with(data, aggregate(list(ruminating = ruminating, stickiness = stickiness,
                                     sumNA = sumNA, sumPA = sumPA),
                                by = list(group = group, intervention = intervention,
                                          blockAssessmentDay = blockAssessmentDay), 
                                FUN = function(x) { mon.mean = mean(x, na.rm = TRUE) } ))

p1 <- ggplot(aggData, aes(x = blockAssessmentDay, y = sumNA,
                          colour = interaction(group, intervention), group = interaction(group, intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)), legend.direction = "horizontal",
        legend.position = c(1,0.925)) + guides(color=guide_legend(NULL))+
  geom_vline(xintercept = 7, linetype="dashed")

p2 <- ggplot(aggData, aes(x = blockAssessmentDay, y = sumPA,
                          colour = interaction(group, intervention), group = interaction(group, intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None") + geom_vline(xintercept = 7, linetype="dashed")

p3 <- ggplot(aggData, aes(x = blockAssessmentDay, y = ruminating,
                          colour = interaction(group, intervention), group = interaction(group, intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None") + geom_vline(xintercept = 7, linetype="dashed")

p4 <- ggplot(aggData, aes(x = blockAssessmentDay, y = stickiness,
                          colour = interaction(group, intervention), group = interaction(group, intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None") + geom_vline(xintercept = 7, linetype="dashed")

legend <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")
grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3)


# # set the colour palette
# cols <- matrix(brewer.pal(4,'Dark2'), nrow = 2, ncol = 2)
# #define margins, plus how many plots
# par(oma = c(6.5,1,1,1), mfrow = c(2, 1), mar = c(1, 1, 2, 1))
# gvec <- c("controls", "remitted")
# ivec <- c("fantasizing", "mindfulness")
# #for the legend
# leg <- matrix(nrow=2, ncol=2)
# 
# for(v in c("sumNA", "sumPA")){
#   if(v=="sumNA"){
#     dv <- 4
#   } else if(v=="sumPA"){
#     dv <- 3
#   } 
#   
#   plot(NULL, xlim=c(0,30), ylim=c(0,100), ylab=v, xlab="Assessment Day")
#   abline(v=7)
#   abline(v=14, lty=3, col = "black")
#   abline(v=21)
#   
#   for(i in 1:2){
#     g <- gvec[i]
#     for(j in 1:2){
#       int <- ivec[j]
#       leg[i,j] <- paste(g,int,sep="-")
#       ind <- which((aggData$group==g) &  (aggData$intervention==int))
#       val <- aggData[ind,v]/dv
#       lines(aggData[ind,]$assessmentDay, val, col = cols[i,j], type = "l", lwd = 3)
#     }
#   }
# }
# par(oma = c(0,0,0,0), mar = c(0, 0, 0, 0), mfrow=c(1,1), new = TRUE)
# legend('bottom',
#        legend = c(leg[1,1], leg[1,2], leg[2,1], leg[2,2]),
#        col = c(cols[1,1], cols[1,2], cols[2,1], cols[2,2]),
#        lwd = 5, xpd = TRUE, horiz = TRUE, cex = 0.8, seg.len=1, bty = 'n')

aggData <- with(data, aggregate(list(ruminating = ruminating, stickiness = stickiness,
                                     sumNA = sumNA, sumPA = sumPA),
                                by = list(group = group, intervention = intervention,
                                          assessmentDay = assessmentDay), 
                                FUN = function(x) { mon.mean = mean(x, na.rm = TRUE) } ))

p1 <- ggplot(aggData, aes(x = assessmentDay, y = sumNA,
                          colour = interaction(group, intervention), group = interaction(group,intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)), legend.direction = "horizontal",
        legend.position = c(1,0.925)) + guides(color=guide_legend(NULL))+
  geom_vline(xintercept = 7, linetype="dashed") +
  geom_vline(xintercept = 14, linetype="solid",) +
  geom_vline(xintercept = 21, linetype="dashed")

p2 <- ggplot(aggData, aes(x = assessmentDay, y = sumPA,
                          colour = interaction(group, intervention), group = interaction(group,intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None") + geom_vline(xintercept = 7, linetype="dashed") +
  geom_vline(xintercept = 14, linetype="solid",) + geom_vline(xintercept = 21, linetype="dashed")

p3 <- ggplot(aggData, aes(x = assessmentDay, y = ruminating,
                          colour = interaction(group, intervention), group = interaction(group,intervention))) +
                          stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None") + geom_vline(xintercept = 7, linetype="dashed") +
  geom_vline(xintercept = 14, linetype="solid",) + geom_vline(xintercept = 21, linetype="dashed")

p4 <- ggplot(aggData, aes(x = assessmentDay, y = stickiness,
                          colour = interaction(group, intervention), group = interaction(group,intervention))) +
  stat_summary(fun = sum, geom = "line") +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None") + geom_vline(xintercept = 7, linetype="dashed") +
  geom_vline(xintercept = 14, linetype="solid",) + geom_vline(xintercept = 21, linetype="dashed")

legend <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")
grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3)

################################### Positive / Negative Affect ####################################

group_PANA <- ddply(data, .(group), plyr::summarize,
                  n_Subj = length(unique(subject)),
                  Pos = mean(sumPA, na.rm = TRUE),
                  sdPA = sd(sumPA, na.rm = TRUE),
                  Neg = mean(sumNA, na.rm = TRUE),
                  sdNA = sd(sumNA, na.rm = TRUE))

phase_PANA <- ddply(data, .(group, phase), plyr::summarize,
                    n_Subj = length(unique(subject)),
                    Pos = mean(sumPA, na.rm = TRUE),
                    sdPA = sd(sumPA, na.rm = TRUE),
                    Neg = mean(sumNA, na.rm = TRUE),
                    sdNA = sd(sumNA, na.rm = TRUE))

block_PANA <- ddply(data, .(group, block, intervention, phase), plyr::summarize,
                    n_Subj = length(unique(subject)),
                    Pos = mean(sumPA, na.rm = TRUE),
                    sdPA = sd(sumPA, na.rm = TRUE),
                    Neg = mean(sumNA, na.rm = TRUE),
                    sdNA = sd(sumNA, na.rm = TRUE))


#for some strange dplyr-ralted reason I need to do this to get melt() to work
data <- as.data.frame(data)

# check baseline assessment per group (Block 1, pre)
meltData1 <- melt(data[which((data$phase=="pre") & (data$block==1)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
meltData2 <- melt(data[which((data$phase=="peri") & (data$block==1)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
meltData3 <- melt(data[which((data$phase=="pre") & (data$block==2)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
meltData4 <- melt(data[which((data$phase=="peri") & (data$block==2)), c("group", "sumPA", "sumNA")], na.rm = TRUE)

p1 <- ggplot(meltData1, aes(factor(variable), value, fill = group)) 
p1 <- p1 + geom_boxplot() + facet_wrap(~"block", scale="free") +
  ggtitle("Block 1 - Pre") +
  scale_fill_manual(values = c("green", "red")) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)), legend.direction = "horizontal",
        legend.position = c(1,1))

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = group)) 
p2 <- p2 + geom_boxplot() + facet_wrap(~"block", scale="free") +
  ggtitle("Block 1 - Peri") +
  scale_fill_manual(values = c("green", "red")) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None")

p3 <- ggplot(meltData3, aes(factor(variable), value, fill = group)) 
p3 <- p3 + geom_boxplot() + facet_wrap(~"block", scale="free") +
  ggtitle("Block 2 - Pre") +
  scale_fill_manual(values = c("green", "red")) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None")

p4 <- ggplot(meltData4, aes(factor(variable), value, fill = group)) 
p4 <- p4 + geom_boxplot() + facet_wrap(~"block", scale="free") +
  ggtitle("Block 2 - Peri") +
  scale_fill_manual(values = c("green", "red")) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
        #strip.background = element_rect(fill = "lightblue"),
        legend.position="None")

legend <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")
grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3)

#check per group, intervention and phase

meltData <- melt(data, id.vars=c("group", "intervention", "phase", "block"),
                 measure.vars = c("sumPA", "sumNA"), na.rm = TRUE)

meltData$groupByInt <- factor(paste(meltData$group, meltData$intervention),
                              levels = c("controls mindfulness", "controls fantasizing",
                                         "remitted mindfulness", "remitted fantasizing"))
meltData$blockPhase <- factor(paste(meltData$phase, meltData$block, sep = " "),
                              levels = c("pre 1", "peri 1", "pre 2", "peri 2"))


ggplot(data = meltData, aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ blockPhase, scale = "free") +
  labs(title = "Levels of PA and NA",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#with change scores
# meltChange <- melt(data, id.vars=c("group", "intervention", "phase", "block"),
#                  measure.vars = c("sumPA_change", "sumNA_change"), na.rm = TRUE)
# 
# meltChange$groupByInt <- factor(paste(meltChange$group, meltChange$intervention),
#                               levels = c("controls mindfulness", "controls fantasizing",
#                                          "remitted mindfulness", "remitted fantasizing"))
# meltChange$blockPhase <- factor(paste(meltChange$phase, meltChange$block, sep = " "),
#                               levels = c("pre 1", "peri 1", "pre 2", "peri 2"))
# 
# 
# ggplot(data = meltChange, aes(variable, y = value, fill = factor(group))) +
#   geom_boxplot() + facet_grid(factor(intervention) ~ blockPhase, scale = "free") +
#   labs(title = "Change Scores of PA and NA",
#        subtitle = " by group, intervention, block and phase",
#        y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


#Plotting changes against average baseline assessment values
#per phaseBeepNum
#create a df with only the relevant variables
meltChange <- melt(data[which(data$phase=="peri"),], id.vars=c("group", "intervention", "phase", "block", "phaseBeepNum"),
                                         measure.vars = c("sumPA", "sumNA"), na.rm = TRUE)

#cacluate the average values per phaseBeepNum
meltChange_avg <- ddply(meltChange, .(group, intervention, phase, block, phaseBeepNum, variable), plyr::summarise,
                        n_values = length(group),
                        avgValue = mean(value),
                        sdValue = sd(value))

#plot the average values of sumPA and sumNA (peri) against the average of the baseline assessment periods (pre)
for(b in 1:2){
  for(g in c("controls", "remitted")){
    for(intervention in c('mindfulness', 'fantasizing')){
      for(v in c('sumPA', 'sumNA')){
        
        if(v == 'sumPA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                       (data$intervention==intervention)),]$sumPA, na.rm=TRUE)
          color <- "green"
          lab_y <- "Avg change in PA"
          main <- "Change in Positive Affect per individual assessment"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")
        }
        if(v == 'sumNA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                       (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
          color <- "red"
          lab_y <- "Avg change in NA"
          main <- "Change in Negative Affect per individual assessment"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")
        }
        
        
        px <- ggplot(data = meltChange_avg[which((meltChange_avg$group==g) &
                                                   (meltChange_avg$block==b) &
                                                   (meltChange_avg$variable==v) &
                                                   (meltChange_avg$intervention==intervention)),],
                     aes(x=phaseBeepNum, y=avgValue-meanVal)) +
          geom_line(color = color) + geom_point() + scale_fill_manual(values=color) +
          labs(title=main, subtitle=subtitle) + xlab('Phase Assessment Number') + ylab(lab_y) +
          geom_errorbar(aes(ymin=avgValue-sdValue-meanVal, ymax=avgValue+sdValue-meanVal), width=.2,
                        position=position_dodge(.9)) 
        print(px)
      }
    }
  }
}

#per Assessment day
#create a df with only the relevant variables
meltChange <- melt(data[which(data$phase=="peri"),], id.vars=c("group", "intervention", "phase", "block", "phaseAssessmentDay"),
                   measure.vars = c("sumPA", "sumNA"), na.rm = TRUE)

#cacluate the average values per phaseBeepNum
meltChange_avg <- ddply(meltChange, .(group, intervention, phase, block, phaseAssessmentDay, variable), plyr::summarise,
                        n_values = length(group),
                        avgValue = mean(value),
                        sdValue = sd(value))

#plot the average values of sumPA and sumNA (peri) against the average of the baseline assessment periods (pre)
for(b in 1:2){
  for(g in c("controls", "remitted")){
    for(intervention in c('mindfulness', 'fantasizing')){
      for(v in c('sumPA', 'sumNA')){
        
        if(v == 'sumPA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                       (data$intervention==intervention)),]$sumPA, na.rm=TRUE)
          color <- "green"
          lab_y <- "Avg change in PA"
          main <- "Change in Positive Affect per Assessment Day"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")
        }
        if(v == 'sumNA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                       (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
          color <- "red"
          lab_y <- "Avg change in NA"
          main <- "Change in Negative Affect per Assessment Day"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")
        }
        
        
        px <- ggplot(data = meltChange_avg[which((meltChange_avg$group==g) &
                                                   (meltChange_avg$block==b) &
                                                   (meltChange_avg$variable==v) &
                                                   (meltChange_avg$intervention==intervention)),],
                     aes(x=phaseAssessmentDay, y=avgValue-meanVal)) +
          geom_line(color = color) + geom_point() + scale_fill_manual(values=color) +
          labs(title=main, subtitle=subtitle) + xlab('Phase Assessment Day') + ylab(lab_y) +
          geom_errorbar(aes(ymin=avgValue-sdValue-meanVal, ymax=avgValue+sdValue-meanVal), width=.2,
                        position=position_dodge(.9)) 
        print(px)
      }
    }
  }
}

meltChange_avg <- ddply(meltChange, .(group, intervention, phase, phaseAssessmentDay, variable), plyr::summarise,
                        n_values = length(group),
                        avgValue = mean(value),
                        sdValue = sd(value))

#combining both blocks
for(g in c("controls", "remitted")){
  for(intervention in c('mindfulness', 'fantasizing')){
    for(v in c('sumPA', 'sumNA')){
      
      if(v == 'sumPA'){
        meanVal <- mean(data[which((data$phase=="pre") & (data$group==g) &
                                     (data$intervention==intervention)),]$sumPA, na.rm=TRUE)
        color <- "green"
        lab_y <- "Avg change in PA"
        main <- "Change in Positive Affect per Assessment Day"
        subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, both blocks)")
      }
      if(v == 'sumNA'){
        meanVal <- mean(data[which((data$phase=="pre") & (data$group==g) &
                                     (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
        color <- "red"
        lab_y <- "Avg change in NA"
        main <- "Change in Negative Affect per Assessment Day"
        subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, both blocks)")
      }
      
      
      px <- ggplot(data = meltChange_avg[which((meltChange_avg$group==g) &
                                                 (meltChange_avg$variable==v) &
                                                 (meltChange_avg$intervention==intervention)),],
                   aes(x=phaseAssessmentDay, y=avgValue-meanVal)) +
        geom_line(color = color) + geom_point() + scale_fill_manual(values=color) +
        labs(title=main, subtitle=subtitle) + xlab('Phase Assessment Day') + ylab(lab_y) +
        geom_errorbar(aes(ymin=avgValue-sdValue-meanVal, ymax=avgValue+sdValue-meanVal), width=.2,
                      position=position_dodge(.9)) 
      print(px)
    }
  }
}



################################### Initial analyses - raw values ####################################

#calculating averages per group (remitted vs controls)
grp_avgs <- ddply(data, .(group), plyr::summarize,
                 n_Subj = length(unique(subject)),
                 db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                 db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                 db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                 db9_down_avg = mean(down, na.rm = TRUE),
                 db9_down_sd = sd(down, na.rm = TRUE),
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
metricCols <- c('wakeful', 'down', 'satisfied', 'irritated', 'energetic', 'restless',
                'stressed', 'anxious', 'listless', 'worried', 'stickiness', 'thoughtsPleasant', 'distracted',
                'restOfDayPos', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

#boxplot comparisons
meltData1 <- melt(data[which((data$phase=="pre") & (data$block==1)), c("group", metricCols[1:9])])
#boxplot(data=meltData, value~variable)

p1 <- ggplot(meltData1, aes(factor(variable), value, fill = group)) 
p1 + geom_boxplot() + facet_wrap(~variable, scale="free")


meltData2 <- melt(data[which((data$phase=="pre") & (data$block==1)), c("group", metricCols[10:18])])
#boxplot(data=meltData, value~variable)

p2 <- ggplot(meltData2, aes(factor(variable), value, fill = group)) 
p2 + geom_boxplot() + facet_wrap(~variable, scale="free")

#per group and intervention
int_avgs <- ddply(data, .(group, intervention), plyr::summarize,
                  n_Subj = length(unique(subject)),
                  db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                  db9_down_avg = mean(down, na.rm = TRUE),
                  db9_down_sd = sd(down, na.rm = TRUE),
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

#per group, intervention, and phase
phase_avgs <- ddply(data, .(group, intervention, phase), plyr::summarize,
                  n_Subj = length(unique(subject)),
                  db2_sleep_avg = mean(sleepQuality, na.rm = TRUE),
                  db2_sleep_sd = sd(sleepQuality, na.rm = TRUE),
                  db8_wakeful_avg = mean(wakeful, na.rm = TRUE),
                  db9_down_avg = mean(down, na.rm = TRUE),
                  db9_down_sd = sd(down, na.rm = TRUE),
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
time_avgs <- ddply(data, .(group, intervention, beepNum), plyr::summarize,
                  #n_Subj = length(unique(id)),
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

time_avgs$grInt <- paste(time_avgs$group, time_avgs$intervention, sep = "-")
time_avgs <- drop_na(time_avgs, intervention)

dependent_vars = c("sleep_avg", "wakeful_avg", "down_avg", "satisfied_avg", "irritated_avg",
                   "energetic_avg", "restless_avg", "stressed_avg", "anxious_avg", "listless_avg",
                   "worrying_avg", "stickiness_avg", "easeThoughts_avg", "distracted_avg", "restOfDayPos_avg",
                   "companyPos_avg", "solitudePos_avg", "enjoyabilityMax_avg", "intensityPos_avg",
                   "unpleasantMax_avg", "intensityNeg_avg")

for(var in dependent_vars){
  meltData <- melt(drop_na(time_avgs, grInt), id = c( "beepNum", "grInt"), measure.vars = var)
  
  p1 <- ggplot(meltData, aes(beepNum, value, color = factor(grInt), group = variable)) +
          geom_point() +
          facet_grid("grInt") +
          geom_smooth(method='lm', formula = "y ~ x") +
          labs(y = var, x = "Assessment Number") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
                #strip.background = element_rect(fill = "lightblue"),
                legend.position="None")
  print(p1)
}

#developments over time per phase
phase_avgs <- ddply(data, .(group, intervention, block, phase, blockBeepNum), plyr::summarize,
                   #n_Subj = length(unique(id)),
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

dependent_vars = c("sleep_avg", "wakeful_avg", "down_avg", "satisfied_avg", "irritated_avg",
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

remittedIDs <-  unique(data[data$group == "remitted",]$id) #get a list of all unique IDs
set.seed(125)
remittedSample <- sample(remittedIDs, 3)

contIDs <-  unique(data[data$group == "controls",]$id) #get a list of all unique IDs
contSample <- sample(contIDs, 3)

randData1 <- data[data$id %in% remittedSample,]
randData2 <- data[data$id %in% contSample,]

meltData1 <- melt(randData1, id = c("id", "assessmentDay", "beepNum"), measure.vars = dependent_var)
meltData2 <- melt(randData2, id = c("id", "assessmentDay", "beepNum"), measure.vars = dependent_var)

#for x: use beepNum for time or another variable to see relationship
p1 <- ggplot(meltData1,aes(x=beepNum,y=value,colour=factor(id), group = variable)) +
  geom_line() + geom_smooth(method='lm')
p2 <- ggplot(meltData2,aes(x=beepNum,y=value,colour=factor(id), group = variable)) +
  geom_line() + geom_smooth(method='lm')

#to split one plot into a grid of multiple plots
p1 <- p1 + facet_grid(rows = vars(factor(id))) + xlab("Assessment Number") + ylab(dependent_var)
p2 <- p2 + facet_grid(rows = vars(factor(id))) + xlab("Assessment Number") + ylab(dependent_var)

p1 <- p1 + theme(legend.position="None")
p2 <- p2 + theme(legend.position="None") #+ geom_vline(xintercept = assessmentDay, linetype = "dashed", color = "red")

figure <- ggarrange(p1, p2,
                    labels = c("Remitted", "Controls"),
                    ncol = 2, nrow = 1)#, common.legend = TRUE)
figure


#test <- subset(data[data$id == 3602171,], select = c(id, mindcog_db_protocol, assessmentDay, beepNum, mindcog_db_open_from))


#################################### Exploratory rumination analysis ##################################

#Rumination averages and changes compared to baseline
# add here!!!!!!!!!!!!!!!!11
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#time spent thinking about the past / present / future by group, phase and block
pc_time <- ddply(data, .(group, phase, block), plyr::summarize,
                  N = length(group[which(!is.na(thoughtsTime))]),
                  past = round(length(group[which(thoughtsTime == 1)])/N, 2),
                  present = round(length(group[which(thoughtsTime == 2)])/N, 2),
                  future = round(length(group[which(thoughtsTime == 3)])/N, 2))

#same but with intervention
pc_time_int <- ddply(data, .(group, intervention, phase), plyr::summarize,
                 N = length(group[which(!is.na(thoughtsTime))]),
                 past = round(length(group[which(thoughtsTime == 1)])/N, 2),
                 present = round(length(group[which(thoughtsTime == 2)])/N, 2),
                 future = round(length(group[which(thoughtsTime == 3)])/N, 2))

pc_time_int <- drop_na(pc_time_int, intervention)

#valence of thoughts
pc_val <- ddply(data, .(group, phase, block), plyr::summarize,
                    N = length(group[which(!is.na(thoughtsValence))]),
                    negative = round(length(group[which(thoughtsValence == 1)])/N, 2),
                    neutral = round(length(group[which(thoughtsValence == 2)])/N, 2),
                    positive = round(length(group[which(thoughtsValence == 3)])/N, 2))

#same with intervention
pc_val_int <- ddply(data, .(group, intervention, phase), plyr::summarize,
                 N = length(group[which(!is.na(thoughtsValence))]),
                 negative = round(length(group[which(thoughtsValence == 1)])/N, 2),
                 neutral = round(length(group[which(thoughtsValence == 2)])/N, 2),
                 positive = round(length(group[which(thoughtsValence == 3)])/N, 2))

pc_val_int <- drop_na(pc_val_int, intervention)

#object of thoughts
pc_object <- ddply(data, .(group, phase), plyr::summarize,
                   N = length(group[which(!is.na(thoughtsObject))]),
                   self = round(length(group[which(thoughtsObject == 1)])/N, 2),
                   somebody = round(length(group[which(thoughtsObject == 2)])/N, 2),
                   neither = round(length(group[which(thoughtsObject == 3)])/N, 2))

#same with interventions
pc_object_int <- ddply(data, .(group, intervention, phase), plyr::summarize,
                   N = length(group[which(!is.na(thoughtsObject))]),
                   self = round(length(group[which(thoughtsObject == 1)])/N, 2),
                   somebody = round(length(group[which(thoughtsObject == 2)])/N, 2),
                   neither = round(length(group[which(thoughtsObject == 3)])/N, 2))

pc_object_int <- drop_na(pc_object_int, intervention)

#what are they thinking about?
pc_thinkingOf <- ddply(data, .(group, phase), plyr::summarize,
                       N = length(group[which(!is.na(thinkingOf))]),
                       currentActivity = round(length(group[which(thinkingOf == 1)])/N, 2),
                       externalStimuli = round(length(group[which(thinkingOf == 2)])/N, 2),
                       currentFeelings = round(length(group[which(thinkingOf == 3)])/N, 2),
                       personalConcerns = round(length(group[which(thinkingOf == 4)])/N, 2),
                       daydreaming = round(length(group[which(thinkingOf == 5)])/N, 2),
                       other = round(length(group[which(thinkingOf == 6)])/N, 2))

#same with intervention
pc_thinkingOf_int <- ddply(data, .(group, intervention, phase), plyr::summarize,
                   N = length(group[which(!is.na(thinkingOf))]),
                   currentActivity = round(length(group[which(thinkingOf == 1)])/N, 2),
                   externalStimuli = round(length(group[which(thinkingOf == 2)])/N, 2),
                   currentFeelings = round(length(group[which(thinkingOf == 3)])/N, 2),
                   personalConcerns = round(length(group[which(thinkingOf == 4)])/N, 2),
                   daydreaming = round(length(group[which(thinkingOf == 5)])/N, 2),
                   other = round(length(group[which(thinkingOf == 6)])/N, 2))

pc_thinkingOf_int <- drop_na(pc_thinkingOf_int, intervention)

#merge(pc_time, pc_val, pc_object, pc_thinkingOf, by = c(group, intervention, phase, block))

#pc_summary <- Reduce(function(x, y) merge(x, y, all=TRUE), list(pc_time, pc_val, pc_object, pc_thinkingOf))

#per Assessment day
#create a df with only the relevant variables
meltChange <- melt(data[which(data$phase=="peri"),], id.vars=c("group", "intervention", "phase", "block", "phaseAssessmentDay"),
                   measure.vars = "ruminating", na.rm = TRUE)

#cacluate the average values per phaseBeepNum
meltChange_avg <- ddply(meltChange, .(group, intervention, phase, block, phaseAssessmentDay, variable), plyr::summarise,
                        n_values = length(group),
                        avgValue = mean(value),
                        sdValue = sd(value))

#plot the average values of sumPA and sumNA (peri) against the average of the baseline assessment periods (pre)
for(b in 1:2){
  for(g in c("controls", "remitted")){
    if(g=="controls"){
      color = "green"
    } else {
      color = "red"
    }
    for(intervention in c('mindfulness', 'fantasizing')){
        
        meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                     (data$intervention==intervention)),]$ruminating, na.rm=TRUE)
        lab_y <- "Avg change in rumination"
        main <- "Change in Rumination per Assessment Day"
        subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")

      
      px <- ggplot(data = meltChange_avg[which((meltChange_avg$group==g) &
                                                 (meltChange_avg$block==b) &
                                                 (meltChange_avg$intervention==intervention)),],
                   aes(x=phaseAssessmentDay, y=avgValue-meanVal)) +
        geom_line(color = color) + geom_point() + scale_fill_manual(values=color) +
        labs(title=main, subtitle=subtitle) + xlab('Phase Assessment Day') + ylab(lab_y) +
        geom_errorbar(aes(ymin=avgValue-sdValue-meanVal, ymax=avgValue+sdValue-meanVal), width=.2,
                      position=position_dodge(.9)) 
      print(px)
    }
  }
}

#without block
meltChange_avg <- ddply(meltChange, .(group, intervention, phase, phaseAssessmentDay, variable), plyr::summarise,
                        n_values = length(group),
                        avgValue = mean(value),
                        sdValue = sd(value))

#combining both blocks

for(g in c("controls", "remitted")){
  if(g=="controls"){
    color = "green"
  } else {
    color = "red"
  }
  for(intervention in c('mindfulness', 'fantasizing')){
    
    meanVal <- mean(data[which((data$phase=="pre") & (data$group==g) &
                                 (data$intervention==intervention)),]$ruminating, na.rm=TRUE)

    color <- "green"
    lab_y <- "Avg change in rumination"
    main <- "Change in Rumination per Assessment Day"
    subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, both blocks", ")")
    
    
    px <- ggplot(data = meltChange_avg[which((meltChange_avg$group==g) &
                                               (meltChange_avg$intervention==intervention)),],
                 aes(x=phaseAssessmentDay, y=avgValue-meanVal)) +
      geom_line(color = color) + geom_point() + scale_fill_manual(values=color) +
      labs(title=main, subtitle=subtitle) + xlab('Phase Assessment Day') + ylab(lab_y) +
      geom_errorbar(aes(ymin=avgValue-sdValue-meanVal, ymax=avgValue+sdValue-meanVal), width=.2,
                    position=position_dodge(.9)) 
    print(px)
  }
}




################################### Change scores ######################################
cols <- c('wakeful', 'down', 'satisfied', 'irritated', 'energetic', 'restless', 'stressed', 'anxious',
          'listless', 'thinkingOf', 'worried', 'stickiness', 'thoughtsPleasant', 'thoughtsTime',
          'thoughtsValence', 'thoughtsObject', 'distracted', 'restOfDayPos', 'aloneCompany',
          'companyPleasant', 'alonePleasant', 'posMax', 'posIntensity', 'negMax', 'negIntensity')

changeCols <- c()
for(col in metricCols) {
  changeCols <- c(changeCols, paste(col, "change", sep = "_"))
}

grp_avg_cs<- ddply(data, .(group), plyr::summarize,
                  n_Subj = length(unique(subject)),
                  # sleep_avg = mean(sleepQuality_change, na.rm = TRUE),
                  # sleep_sd = sd(sleepQuality_change, na.rm = TRUE),
                  wakeful_avg = mean(wakeful_change, na.rm = TRUE),
                  down_avg = mean(down_change, na.rm = TRUE),
                  down_sd = sd(down_change, na.rm = TRUE),
                  satisfied_avg = mean(satisfied_change, na.rm = TRUE),
                  satisfied_sd = sd(satisfied_change, na.rm = TRUE),
                  irritated_avg = mean(irritated_change, na.rm = TRUE),
                  energetic_avg = mean(energetic_change, na.rm = TRUE),
                  restless_avg = mean(restless_change, na.rm = TRUE),
                  stressed_avg = mean(stressed_change, na.rm = TRUE),
                  anxious_avg = mean(anxious_change, na.rm = TRUE),
                  listless_avg = mean(listless_change, na.rm = TRUE),
                  worrying_avg = mean(worried_change, na.rm = TRUE),
                  stickiness_avg = mean(stickiness_change, na.rm = TRUE),
                  easeThoughts_avg = mean(thoughtsPleasant_change, na.rm = TRUE),
                  distracted_avg = mean(distracted_change, na.rm = TRUE),
                  restOfDayPos_avg = mean(restOfDayPos_change, na.rm = TRUE),
                  companyPos_avg = mean(companyPleasant_change, na.rm = TRUE),
                  solitudePos_avg = mean(alonePleasant_change, na.rm = TRUE),
                  enjoyabilityMax_avg = mean(posMax_change, na.rm = TRUE),
                  intensityPos_avg = mean(posIntensity_change, na.rm = TRUE),
                  unpleasantMax_avg = mean(negMax_change, na.rm = TRUE),
                  intensityNeg_avg = mean(negIntensity_change, na.rm = TRUE))


#per group, intervention, and phase
phase_avg_cs<- ddply(data, .(group, intervention, phase), plyr::summarize,
                   n_Subj = length(unique(subject)),
                   # sleep_avg = mean(sleepQuality_change, na.rm = TRUE),
                   # sleep_sd = sd(sleepQuality_change, na.rm = TRUE),
                   wakeful_avg = mean(wakeful_change, na.rm = TRUE),
                   down_avg = mean(down_change, na.rm = TRUE),
                   down_sd = sd(down_change, na.rm = TRUE),
                   satisfied_avg = mean(satisfied_change, na.rm = TRUE),
                   satisfied_sd = sd(satisfied_change, na.rm = TRUE),
                   irritated_avg = mean(irritated_change, na.rm = TRUE),
                   energetic_avg = mean(energetic_change, na.rm = TRUE),
                   restless_avg = mean(restless_change, na.rm = TRUE),
                   stressed_avg = mean(stressed_change, na.rm = TRUE),
                   anxious_avg = mean(anxious_change, na.rm = TRUE),
                   listless_avg = mean(listless_change, na.rm = TRUE),
                   worrying_avg = mean(worried_change, na.rm = TRUE),
                   stickiness_avg = mean(stickiness_change, na.rm = TRUE),
                   easeThoughts_avg = mean(thoughtsPleasant_change, na.rm = TRUE),
                   distracted_avg = mean(distracted_change, na.rm = TRUE),
                   restOfDayPos_avg = mean(restOfDayPos_change, na.rm = TRUE),
                   companyPos_avg = mean(companyPleasant_change, na.rm = TRUE),
                   solitudePos_avg = mean(alonePleasant_change, na.rm = TRUE),
                   enjoyabilityMax_avg = mean(posMax_change, na.rm = TRUE),
                   intensityPos_avg = mean(posIntensity_change, na.rm = TRUE),
                   unpleasantMax_avg = mean(negMax_change, na.rm = TRUE),
                   intensityNeg_avg = mean(negIntensity_change, na.rm = TRUE))


for(g in c("controls", "remitted")){
  #boxplot comparisons
  meltData3 <- melt(data[data$group == g, c("intervention", "phase", changeCols[1:9])])
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
  
  meltData4 <- melt(data[data$group == g, c("intervention", "phase", changeCols[10:18])])
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



