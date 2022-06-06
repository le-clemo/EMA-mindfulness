
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
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(broom)
library(AICcmodavg)
library(effectsize)
library(languageR)

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
                               response = numCompleted - noResponse,
                               responseRate = round(response/numCompleted,2),
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

responses_by_block <- ddply(data, .(group, intervention, phase, block), plyr::summarise,
                               numCompleted = length(mindcog_db_open_from),
                                noResponse = length(unique(mindcog_db_non_response)),
                                response = numCompleted - noResponse,
                                responseRate = round(response/numCompleted,2))
                                #numDays = max(phaseAssessmentDay))

#recreacting with assessment days per group
group_responses <- ddply(data, .(group, intervention), plyr::summarise,
                               nSubj = length(unique(subject)),
                               numCompleted = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numCompleted - noResponse,
                               responseRate = round(response/numCompleted,2)) #6% higher response rate in controls



#################################### Some naive plots ############################################
#for all data
plot(data$sumPA, data$sumNA)

plot(data$sumPA, data$ruminating)

plot(data$sumNA, data$ruminating)


plot(data$ruminating, data$stickiness, col="blue")
points(data$ruminating_lag1, data$stickiness, col="red")
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

# #plot different phases
# # plot(remitted[which(remitted$phase=="pre"),]$sumPA, remitted[which(remitted$phase=="pre"),]$sumNA, col = "red")
# # points(remitted[which(remitted$phase=="peri"),]$sumPA, remitted[which(remitted$phase=="peri"),]$sumNA, col = "green")
# # 
# # plot(controls[which(controls$phase=="pre"),]$sumPA, controls[which(controls$phase=="pre"),]$sumNA, col = "red")
# # points(controls[which(controls$phase=="peri"),]$sumPA, controls[which(controls$phase=="peri"),]$sumNA, col = "green")
# 
# #do the intervention lessen the relationship between negative affect and rumination?
# controls_fa <- controls[which(controls$intervention=="fantasizing"),]
# controls_mf <- controls[which(controls$intervention=="mindfulness"),]
# remitted_fa <- remitted[which(remitted$intervention=="fantasizing"),]
# remitted_mf <- remitted[which(remitted$intervention=="mindfulness"),]
# 
# r_pre <- remitted_fa[which(remitted_fa$phase=="pre"),]$ruminating
# rlag1_pre <- remitted_fa[which(remitted_fa$phase=="pre"),]$ruminating_lag1
# NA_pre <- remitted_fa[which(remitted_fa$phase=="pre"),]$sumNA
# PA_pre <- remitted_fa[which(remitted_fa$phase=="pre"),]$sumPA
# 
# r_peri <- remitted_fa[which(remitted_fa$phase=="peri"),]$ruminating
# rlag1_peri <- remitted_fa[which(remitted_fa$phase=="peri"),]$ruminating_lag1
# NA_peri <- remitted_fa[which(remitted_fa$phase=="peri"),]$sumNA
# PA_peri <- remitted_fa[which(remitted_fa$phase=="peri"),]$sumPA
# 
# #NA
# plot(r_pre, NA_pre, col = "red", main = "Remitted, fantasizing", ylab="Negative Affect", xlab="Rumination")
# points(r_peri, NA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(r_pre, NA_pre, use = "complete.obs")
# reg_pre <- lm(r_pre ~ NA_pre)
# abline(reg_pre, col = "red")
# 
# cor(r_peri, NA_peri, use = "complete.obs")
# reg_peri <- lm(r_peri ~ NA_peri)
# abline(reg_peri, col = "green")
# 
# #PA
# plot(r_pre, PA_pre, col = "red", main = "Remitted, fantasizing", ylab="Positive Affect", xlab="Rumination")
# points(r_peri, PA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(r_pre, PA_pre, use = "complete.obs")
# reg_pre <- lm(r_pre ~ PA_pre)
# abline(reg_pre, col = "red")
# 
# cor(r_peri, PA_peri, use = "complete.obs")
# reg_peri <- lm(r_peri ~ PA_peri)
# abline(reg_peri, col = "green")
# 
# #with ruminating_lag1 and NA
# plot(rlag1_pre, PA_pre, col = "red", main = "Remitted, fantasizing", ylab="Positive Affect", xlab="Rumination (lag1)")
# points(rlag1_peri, PA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(rlag1_pre, PA_pre, use = "complete.obs")
# reg_pre <- lm(rlag1_pre ~ PA_pre)
# abline(reg_pre, col = "red")
# 
# cor(rlag1_peri, PA_peri, use = "complete.obs")
# reg_peri <- lm(rlag1_peri ~ PA_peri)
# abline(reg_peri, col = "green")
# 
# #with ruminating_lag1 and PA
# plot(rlag1_pre, PA_pre, col = "red", main = "Remitted, fantasizing", ylab="Positive Affect", xlab="Rumination")
# points(rlag1_peri, PA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(rlag1_pre, PA_pre, use = "complete.obs")
# reg_pre <- lm(rlag1_pre ~ PA_pre)
# abline(reg_pre, col = "red")
# 
# cor(rlag1_peri, PA_peri, use = "complete.obs")
# reg_peri <- lm(rlag1_peri ~ PA_peri)
# abline(reg_peri, col = "green")
# 
# #for mindfulness group
# r_pre <- remitted_mf[which(remitted_mf$phase=="pre"),]$ruminating
# rlag1_pre <- remitted_mf[which(remitted_mf$phase=="pre"),]$ruminating_lag1
# NA_pre <- remitted_mf[which(remitted_mf$phase=="pre"),]$sumNA
# PA_pre <- remitted_mf[which(remitted_mf$phase=="pre"),]$sumPA
# 
# r_peri <- remitted_mf[which(remitted_mf$phase=="peri"),]$ruminating
# rlag1_peri <- remitted_mf[which(remitted_mf$phase=="peri"),]$ruminating_lag1
# NA_peri <- remitted_mf[which(remitted_mf$phase=="peri"),]$sumNA
# PA_peri <- remitted_mf[which(remitted_mf$phase=="peri"),]$sumPA
# 
# #NA
# plot(r_pre, NA_pre, col = "red", main = "Remitted, mindfulness", ylab="Negative Affect", xlab="Rumination")
# points(r_peri, NA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(r_pre, NA_pre, use = "complete.obs")
# reg_pre <- lm(r_pre ~ NA_pre)
# abline(reg_pre, col = "red")
# 
# cor(r_peri, NA_peri, use = "complete.obs")
# reg_peri <- lm(r_peri ~ NA_peri)
# abline(reg_peri, col = "green")
# 
# #with ruminating_lag1
# plot(rlag1_pre, NA_pre, col = "red", main = "Remitted, fantasizing", ylab="Negative Affect", xlab="Rumination (lag1)")
# points(rlag1_peri, NA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(rlag1_pre, NA_pre, use = "complete.obs")
# reg_pre <- lm(rlag1_pre ~ NA_pre)
# abline(reg_pre, col = "red")
# 
# cor(rlag1_peri, NA_peri, use = "complete.obs")
# reg_peri <- lm(rlag1_peri ~ NA_peri)
# abline(reg_peri, col = "green")
# 
# #PA
# plot(r_pre, PA_pre, col = "red", main = "Remitted, fantasizing", ylab="Positive Affect", xlab="Rumination")
# points(r_peri, PA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(r_pre, PA_pre, use = "complete.obs")
# reg_pre <- lm(r_pre ~ PA_pre)
# abline(reg_pre, col = "red")
# 
# cor(r_peri, PA_peri, use = "complete.obs")
# reg_peri <- lm(r_peri ~ PA_peri)
# abline(reg_peri, col = "green")
# 
# #with ruminating_lag1 and PA
# plot(rlag1_pre, PA_pre, col = "red", main = "Remitted, fantasizing", ylab="Positive Affect", xlab="Rumination")
# points(rlag1_peri, PA_peri, col = "green")
# legend("topleft", legend=c("Pre", "Peri"), col=c("red", "green"), pch=c(1,1))
# 
# cor(rlag1_pre, PA_pre, use = "complete.obs")
# reg_pre <- lm(rlag1_pre ~ PA_pre)
# abline(reg_pre, col = "red")
# 
# cor(rlag1_peri, PA_peri, use = "complete.obs")
# reg_peri <- lm(rlag1_peri ~ PA_peri)
# abline(reg_peri, col = "green")
# 

#
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

#Metric columns
metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'wakeful', 'down', 'satisfied',
                'irritated', 'energetic', 'restless', 'stressed', 'anxious', 'listless', 
                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
                'negMax', 'negIntensity')



#analyzing potential changes in variance
df <- data.frame(matrix(ncol = 2, nrow = 10000))
x <- c("pre", "peri")
colnames(df) <- x
for(g in c("controls", "remitted")){
  for(int in c("fantasizing", "mindfulness")){

    for(col in metricCols){
      i <- 1
      phase_var <- list(rep(NA,2))
      phase_sd <- list(rep(NA,2))
      df[,"pre"] <- NA
      df[,"peri"] <- NA
      
      for(p in c("pre", "peri")){
      dat <- data[which((data$group==g) & (data$intervention==int) & (data$phase==p)),]
      df[[p]] <- dat[[col]]
      phase_var[i] <- round(var(dat[[col]], na.rm=TRUE), 2)
      phase_sd[i] <- round(sd(dat[[col]], na.rm=TRUE), 2)
      i <- i + 1
      }
      print("")
      print("###############################################################################")
      print(paste(g,int,col, sep = " | "))
      print("Variance:")
      print(paste("Pre:", phase_var[[1]], sep=" "))
      print(paste("Peri:", phase_var[[2]], sep=" "))
      print(paste("Change (Peri - Pre):", round(phase_var[[2]] - phase_var[[1]], 2), sep=" "))
      print(var.test(df[,"pre"], df[,"peri"]))
      
      #print(paste("Change in %:", round((phase_var[[2]] - phase_var[[1]])/phase_var[[1]], 2)))
      
      # print("Standard Deviation:")
      # print(paste("Pre:", phase_sd[[1]], sep=" "))
      # print(paste("Peri:", phase_sd[[2]], sep=" "))
      # print(paste("Change (Peri - Pre):", round(phase_sd[[2]] - phase_sd[[1]], 2), sep=" "))
    }
  }
}

var.test(data[which((data$group=="controls") & (data$intervention=="fantasizing") & (data$phase=="pre")),]$ruminating,
         data[which((data$group=="controls") & (data$intervention=="mindfulness") & (data$phase=="peri")),]$ruminating)

# Histograms and density lines
par(mfrow=c(2, 2))
for(g in c("controls", "remitted")){
  for(col in metricCols) {
    for(int in c("fantasizing", "mindfulness")){
      for(p in c("pre", "peri")){
        hist(data[which((data$group==g) & (data$intervention==int) & (data$phase==p)),col], xlim=c(0, 310), breaks=seq(0, 310, 10),
             main=paste(g,int,p, sep = " | "), probability=TRUE, col="gray", border="white", xlab = col)
        d <- density(data[which((data$group==g) & (data$intervention==int)),col], na.rm = TRUE)
        lines(d, col="red")
      }

    }
  }
}
par(mfrow=c(1,1))



#for some strange dplyr-ralted reason I need to do this to get melt() to work
data <- as.data.frame(data)

# # check baseline assessment per group (Block 1, pre)
# meltData1 <- melt(data[which((data$phase=="pre") & (data$block==1)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
# meltData2 <- melt(data[which((data$phase=="peri") & (data$block==1)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
# meltData3 <- melt(data[which((data$phase=="pre") & (data$block==2)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
# meltData4 <- melt(data[which((data$phase=="peri") & (data$block==2)), c("group", "sumPA", "sumNA")], na.rm = TRUE)
# 
# p1 <- ggplot(meltData1, aes(factor(variable), value, fill = group)) 
# p1 <- p1 + geom_boxplot() + facet_wrap(~"block", scale="free") +
#   ggtitle("Block 1 - Pre") +
#   scale_fill_manual(values = c("green", "red")) +
#   theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)), legend.direction = "horizontal",
#         legend.position = c(1,1))
# 
# p2 <- ggplot(meltData2, aes(factor(variable), value, fill = group)) 
# p2 <- p2 + geom_boxplot() + facet_wrap(~"block", scale="free") +
#   ggtitle("Block 1 - Peri") +
#   scale_fill_manual(values = c("green", "red")) +
#   theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
#         #strip.background = element_rect(fill = "lightblue"),
#         legend.position="None")
# 
# p3 <- ggplot(meltData3, aes(factor(variable), value, fill = group)) 
# p3 <- p3 + geom_boxplot() + facet_wrap(~"block", scale="free") +
#   ggtitle("Block 2 - Pre") +
#   scale_fill_manual(values = c("green", "red")) +
#   theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
#         #strip.background = element_rect(fill = "lightblue"),
#         legend.position="None")
# 
# p4 <- ggplot(meltData4, aes(factor(variable), value, fill = group)) 
# p4 <- p4 + geom_boxplot() + facet_wrap(~"block", scale="free") +
#   ggtitle("Block 2 - Peri") +
#   scale_fill_manual(values = c("green", "red")) +
#   theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
#         #strip.background = element_rect(fill = "lightblue"),
#         legend.position="None")
# 
# legend <- get_legend(p1)
# p1 <- p1 + theme(legend.position="none")
# grid.arrange(p1, p2, p3, p4, legend, ncol=2, nrow = 3)

#check per group, intervention and phase

meltData <- melt(data, id.vars=c("group", "intervention", "phase", "block"),
                 measure.vars = c("sumPA", "sumNA", "ruminating",
                                  "sleepQuality", "restednessWakeup", "stickiness",
                                  "anxious", "down", "irritated", "restless",
                                  "wakeful", "energetic", "satisfied",
                                  "listless", "distracted", "restOfDayPos", "thoughtsPleasant",
                                  "companyPleasant", "alonePleasant", "posMax", "posIntensity",
                                  "negMax", "negIntensity"), na.rm = TRUE)

meltData$groupByInt <- factor(paste(meltData$group, meltData$intervention),
                              levels = c("controls mindfulness", "controls fantasizing",
                                         "remitted mindfulness", "remitted fantasizing"))
meltData$blockPhase <- factor(paste(meltData$phase, meltData$block, sep = " "),
                              levels = c("pre 1", "peri 1", "pre 2", "peri 2"))

#overview negative/positive affect
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ blockPhase, scale = "free") +
  labs(title = "Levels of PA and NA",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#alternative view
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(blockPhase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of PA and NA",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red", "blue", "purple"))

#alternative view (blocks combined)
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of PA and NA",
       subtitle = " by group, intervention, and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red", "blue", "purple"))


#splitting them into their respective components
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="wakeful") |
                               (meltData$variable=="satisfied") | (meltData$variable=="energetic")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of PA and its components",
       subtitle = " by group, intervention, and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red", "blue", "purple"))

ggplot(data = meltData[which((meltData$variable=="sumNA") | (meltData$variable=="anxious") |
                               (meltData$variable=="irritated") | (meltData$variable=="down") |
                               (meltData$variable=="restless") ),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of PA and its components",
       subtitle = " by group, intervention, and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red", "blue", "purple"))


ggplot(data = meltData[which((meltData$variable=="anxious") |
                               (meltData$variable=="irritated") | (meltData$variable=="down") |
                               (meltData$variable=="restless") ),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of NA and its components",
       subtitle = " by group, intervention, and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red", "blue", "purple"))

#Rumination
#overview of rumination levels by phase only (blocks combined)
ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of Rumination",
       subtitle = " by group, intervention, and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))

ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(x = value, fill = factor(phase))) +
  geom_histogram(aes(y= after_stat(count / sum(count))), bins=25)


#overview of sleepQuality by phase only (blocks combined)
ggplot(data = meltData[which(meltData$variable=="sleepQuality"),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Sleep Quality",
       subtitle = " by group, intervention, and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))

#overview of stickiness by phase only (blocks combined)
ggplot(data = meltData[which(meltData$variable=="stickiness"),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of Stickiness",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))

#overview of stickiness by phase only (blocks combined)
ggplot(data = meltData[which((meltData$variable=="listless") | (meltData$variable=="distracted")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Levels of Listlessness and Distraction",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))


ggplot(data = meltData[which((meltData$variable=="posMax") | (meltData$variable=="posIntensity")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Pleasantness and Intensity of most pleasant event",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))

ggplot(data = meltData[which((meltData$variable=="negMax") | (meltData$variable=="negIntensity")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Unpleasantness and Intensity of most unpleasant event",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))

ggplot(data = meltData[which((meltData$variable=="companyPleasant") | (meltData$variable=="alonePleasant")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Enjoying current company or enjoying currently being alone",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))

ggplot(data = meltData[which((meltData$variable=="thoughtsPleasant") | (meltData$variable=="restOfDayPos")),],
       aes(variable, y = value, fill = factor(phase))) +
  geom_boxplot() + facet_grid(factor(intervention) ~ group, scale = "free") +
  labs(title = "Pleasantness of thoughts and positivity of outlook for the rest of the day",
       subtitle = " by group, intervention, block and phase",
       y = "Raw score", x = "")# + scale_fill_manual(values = c("green", "red"))





######################################### ANOVA ##########################################################################

for(v in c("ruminating", "sumNA", "sumPA", "companyPleasant", "alonePleasant", "negMax", "negIntensity",
           "posMax", "posIntensity", "stickiness", "anxious", "sleepQuality")){
  print("###############################################################")
  print(v)
  one.way <- aov(data[[v]] ~ group * intervention, data = data)
  print(eta_squared(one.way))
  #print(summary(one.way))
  print(TukeyHSD(one.way, "group"))
}

for(g in c("controls", "remitted")){
  print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(g)
  for(int in c("fantasizing", "mindfulness")){
    print(int)
    dat <- data[which((data$group==g) & (data$intervention==int)),]
    for(v in c("ruminating", "sumNA", "sumPA", "companyPleasant", "alonePleasant", "negMax", "negIntensity",
               "posMax", "posIntensity", "stickiness", "anxious")){
      print(v)
      one.way <- aov(dat[[v]] ~ phase, data = dat)
      print(eta_squared(one.way))
      #print(summary(one.way))
      print(TukeyHSD(one.way))
    }
  }
  print("###############################################################")
}




one.way <- aov(ruminating ~ phase, data = data[which((data$group=="controls") & (data$intervention=="fantasizing")),])
summary(one.way)
TukeyHSD(one.way)


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
          main <- "Change in Positive Affect per individual assessment compared to baseline mean"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")
        }
        if(v == 'sumNA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                       (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
          color <- "red"
          lab_y <- "Avg change in NA"
          main <- "Change in Negative Affect per individual assessment compared to baseline mean"
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

#both blocks
#cacluate the average values per phaseBeepNum
meltChange_avg <- ddply(meltChange, .(group, intervention, phase, phaseBeepNum, variable), plyr::summarise,
                        n_values = length(group),
                        avgValue = mean(value),
                        sdValue = sd(value))

#plot the average values of sumPA and sumNA (peri) against the average of the baseline assessment periods (pre)
for(b in 1:2){
  for(g in c("controls", "remitted")){
    for(intervention in c('mindfulness', 'fantasizing')){
      for(v in c('sumPA', 'sumNA')){
        
        if(v == 'sumPA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$group==g) &
                                       (data$intervention==intervention)),]$sumPA, na.rm=TRUE)
          color <- "green"
          lab_y <- "Avg change in PA"
          main <- "Change in Positive Affect per individual assessment compared to baseline mean"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, both blocks )")
        }
        if(v == 'sumNA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$group==g) &
                                       (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
          color <- "red"
          lab_y <- "Avg change in NA"
          main <- "Change in Negative Affect per individual assessment compared to baseline mean"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, both blocks )")
        }
        
        
        px <- ggplot(data = meltChange_avg[which((meltChange_avg$group==g) &
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
          main <- "Change in Positive Affect per Assessment Day compared to baseline"
          subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, block", b, ")")
        }
        if(v == 'sumNA'){
          meanVal <- mean(data[which((data$phase=="pre") & (data$block==b) & (data$group==g) &
                                       (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
          color <- "red"
          lab_y <- "Avg change in NA"
          main <- "Change in Negative Affect per Assessment Day compared to baseline"
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
        main <- "Change in Positive Affect per Assessment Day compared to baseline"
        subtitle <- paste("(", g, ",", intervention, ",", "peri-intervention, both blocks)")
      }
      if(v == 'sumNA'){
        meanVal <- mean(data[which((data$phase=="pre") & (data$group==g) &
                                     (data$intervention==intervention)),]$sumNA, na.rm=TRUE)
        color <- "red"
        lab_y <- "Avg change in NA"
        main <- "Change in Negative Affect per Assessment Day compared to baseline"
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
                 db18_worrying_avg = mean(ruminating, na.rm = TRUE),
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
                  db18_worrying_avg = mean(ruminating, na.rm = TRUE),
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
                  db18_worrying_avg = mean(ruminating, na.rm = TRUE),
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
                  worrying_avg = mean(ruminating, na.rm = TRUE),
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
                   worrying_avg = mean(ruminating, na.rm = TRUE),
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

corrplot(res$r, method = "number", order = 'alphabet', main = "All participants")

#combine negative affect and positive affect measures, respectively
corrCols <- c('stressed', 'listless', 'ruminating', 'stickiness', 'thoughtsPleasant', 'distracted',
              'restOfDayPos', 'posMax', 'posIntensity', 'negMax', 'negIntensity', 'sumNA', 'sumPA')
corrMat <- as.matrix(data[, corrCols])
#calculate the correlations
res <- rcorr(corrMat, type = c("pearson"))
corrplot(res$r, method = "number", order = 'alphabet', main = "All participants")

#separately for remitted and controls
corrMatRm <- as.matrix(data[data$group == "remitted", metricCols])
corrMatCont <- as.matrix(data[data$group == "controls", metricCols])

#calculate the correlations
resRm <- rcorr(corrMatRm, type = c("pearson"))
resCont <- rcorr(corrMatCont, type = c("pearson"))

par(mfrow = c(1,2))
corrplot(resRm$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "Remitted")

corrplot(resCont$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, main = "Controls")

par(mfrow = c(1,1))


################################################## Checking for collinearity #########################################################
#Source: http://www.sfs.uni-tuebingen.de/~hbaayen/publications/baayenCUPstats.pdf

#a condition number of around 30 is considered potentially harmful


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



metricCols <- c('ruminating', 'stickiness', 'sumNA', 'sumPA', 'wakeful', 'down', 'satisfied',
                'irritated', 'energetic', 'restless', 'anxious', 'stressed', 'listless', 
                'thoughtsPleasant', 'distracted', 'restOfDayPos', 'posMax', 'posIntensity',
                'negMax', 'negIntensity', "sleepQuality", "sleepLatency", "sleepDuration", "restednessWakeup")#, "companyPleasant", "alonePleasant")

data_copy <- data.table::copy(data)
data_copy <- data_copy[which(is.na(data_copy$mindcog_db_non_response)),]
data_copy <- data_copy[,metricCols]
# data_copy[which(is.na(data_copy$companyPleasant)),]$companyPleasant <- 0
# data_copy[which(is.na(data_copy$alonePleasant)),]$alonePleasant <- 0

data_copy <- data_copy[complete.cases(data_copy), ]

#with ruminating but without sumNA, sumPA
collin.fnc(data_copy[,-c(3, 4)])$cnumber #~31.5 --> potentially problematic collinearity
plot(varclus(as.matrix(data_copy[,-c(3, 4)])))

#without ruminating, sumNA, sumPA
collin.fnc(data_copy[,-c(1, 3, 4)])$cnumber #~30.9 --> still potentially problematic collinearity
plot(varclus(as.matrix(data_copy[,-c(1, 3, 4)])))

#including sumNA, sumPA instead of their individual components
collin.fnc(data_copy[,-c(1, 5,6,7,8,9,10,11)])$cnumber #~27 --> better but still not great
plot(varclus(as.matrix(data_copy[,-c(1, 5,6,7,8,9,10,11)])))

#based on the collinearites exibhited by the last plot, we remove several variables that seem too highly correlated / redundant
collin.fnc(data_copy[,-c(1, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])$cnumber #~15.7 --> shouldn't be an issue
plot(varclus(as.matrix(data_copy[,-c(1, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 22, 23, 24)])))

#The plot indicates that e.g. restOfDayPos and sumPA are strongly correlated (thoughtsPleasant also but less so). 
#Therefore, we may exclude restOfDayPos (and thoughtsPleeasant)
#negMax and negIntensity are (unsurprisingly) also correlated strongly --> we could combine them (sum?)
#though its not reflected in the plot, we might wanna think about doing the same with posMax and posIntensity
#stressed may also be redundant as it correlates with sumNA strongly

#one way to deal with collinearity is to only pick one predictor per cluster. This is not preferred however!
#instead we turn one or more of the clusters of predictors into PCAs, reducing the number of predictors


items.pca = prcomp(data_copy[,-c(10, 19, 20)], center = T, scale = T)
summary(items.pca) #17 PCAs
sum((items.pca$sdev^2/sum(items.pca$sdev^2))[1:11]) #11 pcas explain about 90% of the variance
collin.fnc(items.pca$x)$cnumb #condition number of 1 --> they are indeed perfectly uncorrelated

pcaMat = as.data.frame(items.pca$rotation[,1:11])
#pcaMat[order(pcaMat$PC1), ] #the first PCA distinguishes between positive and negative variables
data_copy$PC1 = items.pca$x[,1]
data_copy$PC2 = items.pca$x[,2]
data_copy$PC3 = items.pca$x[,3]
data_copy$PC4 = items.pca$x[,4]
data_copy$PC5 = items.pca$x[,5]
data_copy$PC6 = items.pca$x[,6]
data_copy$PC7 = items.pca$x[,7]
data_copy$PC8 = items.pca$x[,8]
data_copy$PC9 = items.pca$x[,9]
data_copy$PC10 = items.pca$x[,10]
data_copy$PC11 = items.pca$x[,11]
data_copy$PC12 = items.pca$x[,12]

responseRows <- which(is.na(data$mindcog_db_non_response))
nonNARows <- which(complete.cases(data[responseRows,metricCols]))

data$PC1 <- NA
data$PC2 <- NA
data$PC3 <- NA
data$PC4 <- NA
data$PC5 <- NA
data$PC6 <- NA
data$PC7 <- NA
data$PC8 <- NA
data$PC9 <- NA
data$PC10 <- NA
data$PC11 <- NA
data$PC12 <- NA
data[nonNARows,"PC1"] <- data_copy$PC1
data[nonNARows,"PC2"] <- data_copy$PC2
data[nonNARows,"PC3"] <- data_copy$PC3
data[nonNARows,"PC4"] <- data_copy$PC4
data[nonNARows,"PC5"] <- data_copy$PC5
data[nonNARows,"PC6"] <- data_copy$PC6
data[nonNARows,"PC7"] <- data_copy$PC7
data[nonNARows,"PC8"] <- data_copy$PC8
data[nonNARows,"PC9"] <- data_copy$PC9
data[nonNARows,"PC10"] <- data_copy$PC10
data[nonNARows,"PC11"] <- data_copy$PC11
data[nonNARows,"PC12"] <- data_copy$PC12



#################################### Individual plots ####################################

dependent_var <- "ruminating"

remittedIDs <-  unique(data[which((data$group == "remitted") & (data$block==1) & (data$phase=="pre")),]$subject) #get a list of all unique IDs
set.seed(234)
remittedSample <- sample(remittedIDs, 3)

contIDs <-  unique(data[which((data$group == "controls") & (data$block==1) & (data$phase=="pre")),]$subject) #get a list of all unique IDs
contSample <- sample(contIDs, 3)

randData1 <- data[data$subject %in% remittedSample,]
randData2 <- data[data$subject %in% contSample,]

meltData1 <- melt(randData1, id = c("subject", "phaseAssessmentDay", "phaseBeepNum"), measure.vars = dependent_var)
meltData2 <- melt(randData2, id = c("subject", "phaseAssessmentDay", "phaseBeepNum"), measure.vars = dependent_var)

#for x: use phaseBeepNum for time or another variable to see relationship
p1 <- ggplot(meltData1,aes(x=phaseBeepNum,y=value,colour=factor(subject), group = variable)) +
  geom_line() + geom_smooth(method='lm')
p2 <- ggplot(meltData2,aes(x=phaseBeepNum,y=value,colour=factor(subject), group = variable)) +
  geom_line() + geom_smooth(method='lm')

#to split one plot into a grid of multiple plots
p1 <- p1 + facet_grid(rows = vars(factor(subject))) + xlab("Assessment Number") + ylab(dependent_var)
p2 <- p2 + facet_grid(rows = vars(factor(subject))) + xlab("Assessment Number") + ylab(dependent_var)

p1 <- p1 + theme(legend.position="None")
p2 <- p2 + theme(legend.position="None") #+ geom_vline(xintercept = assessmentDay, linetype = "dashed", color = "red")

figure <- ggarrange(p1, p2,
                    labels = c("Remitted", "Controls"),
                    ncol = 2, nrow = 1)#, common.legend = TRUE)
figure


#test <- subset(data[data$id == 3602171,], select = c(id, mindcog_db_protocol, assessmentDay, beepNum, mindcog_db_open_from))


#################################### Exploratory rumination analysis ##################################
ans_q0 <- c(
  `1` = "Past",
  `2` = "Present",
  `3` = "Future"
)

melt_q0 <- melt(data, id.vars=c( "group","thoughtsTime"), measure.vars=c("ruminating"))
melt_q0 <- melt_q0[which(!is.na(melt_q0$thoughtsTime)),]

ggplot(melt_q0) +
  geom_boxplot(aes(x=thoughtsTime, y=value, color=group)) +
  facet_grid(. ~ thoughtsTime, scale = "free", labeller = as_labeller(ans_q0)) +
  labs(title = "Rumination and time-orientation of thoughts", x = "", y = "Rumination") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



pc_time1 <- ddply(data, .(group), plyr::summarize,
                 N = length(group[which(!is.na(thoughtsTime))]),
                 past = round(length(group[which(thoughtsTime == 1)])/N, 2),
                 present = round(length(group[which(thoughtsTime == 2)])/N, 2),
                 future = round(length(group[which(thoughtsTime == 3)])/N, 2))

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

ans_q0 <- c(
  `1` = "Negative",
  `2` = "Neutral",
  `3` = "Positive"
)

melt_q0 <- melt(data, id.vars=c( "subject", "group","thoughtsValence"), measure.vars=c("ruminating"))
melt_q0 <- melt_q0[which(!is.na(melt_q0$thoughtsValence)),]

ggplot(melt_q0) +
  geom_boxplot(aes(x=thoughtsValence, y=value, color=group)) +
  facet_grid(. ~ thoughtsValence, scale = "free", labeller = as_labeller(ans_q0)) +
  labs(title = "Rumination and valence of thoughts", x = "", y = "Rumination") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

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


ans_q0 <- c(
  `1` = "Myself",
  `2` = "Someone else",
  `3` = "Other"
)

melt_q0 <- melt(data, id.vars=c( "group","thoughtsObject"), measure.vars=c("sumNA"))
melt_q0 <- melt_q0[which(!is.na(melt_q0$thoughtsObject)),]

ggplot(melt_q0) +
  geom_boxplot(aes(x=thoughtsObject, y=value, color=group)) +
  facet_grid(. ~ thoughtsObject, scale = "free", labeller = as_labeller(ans_q0)) +
  labs(title = "Rumination and time-orientation of thoughts", x = "", y = "Rumination") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


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

ans_q0 <- c(
  `1` = "Activity",
  `2` = "Surroundings",
  `3` = "Feelings",
  `4` = "Concerns",
  `5` = "Daydreaming",
  `6` = "Other"
)

melt_q0 <- melt(data, id.vars=c( "group","thinkingOf"), measure.vars=c("ruminating"))
melt_q0 <- melt_q0[which(!is.na(melt_q0$thinkingOf)),]

ggplot(melt_q0) +
  geom_boxplot(aes(x=thinkingOf, y=value, color=group)) +
  facet_grid(. ~ thinkingOf, scale = "free", labeller = as_labeller(ans_q0)) +
  labs(title = "Rumination and current focus", x = "", y = "Rumination") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#merge(pc_time, pc_val, pc_object, pc_thinkingOf, by = c(group, intervention, phase, block))

#pc_summary <- Reduce(function(x, y) merge(x, y, all=TRUE), list(pc_time, pc_val, pc_object, pc_thinkingOf))



meltData <- melt(data, id.vars=c("group", "thoughtsTime"),
                 measure.vars = c("sumPA", "sumNA", "ruminating",
                                  "sleepQuality", "restednessWakeup", "stickiness",
                                  "anxious", "down", "irritated", "restless",
                                  "wakeful", "energetic", "satisfied",
                                  "listless", "distracted", "restOfDayPos", "thoughtsPleasant",
                                  "companyPleasant", "alonePleasant", "posMax", "posIntensity",
                                  "negMax", "negIntensity"), na.rm = TRUE)

thoughtsTime_names <- c(
  `1` = "Past",
  `2` = "Present",
  `3` = "Future"
)

#overview negative/positive affect
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsTime~., scale = "free", labeller=as_labeller(thoughtsTime_names)) +
  labs(title = "Levels of PA and NA",
       subtitle = " by group and time-orientation of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#overview rumination
ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsTime~., scale = "free", labeller = as_labeller(thoughtsTime_names)) +
  labs(title = "Levels of Rumination",
       subtitle = " by group and time-orientation of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


ggplot(data = meltData[which(meltData$variable=="stickiness"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsTime~., scale = "free", labeller = as_labeller(thoughtsTime_names)) +
  labs(title = "Levels of Stickiness",
       subtitle = " by group and time-orientation of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


#with thoughtsValence
meltData <- melt(data, id.vars=c("group", "thoughtsValence"),
                 measure.vars = c("sumPA", "sumNA", "ruminating",
                                  "sleepQuality", "restednessWakeup", "stickiness",
                                  "anxious", "down", "irritated", "restless",
                                  "wakeful", "energetic", "satisfied",
                                  "listless", "distracted", "restOfDayPos", "thoughtsPleasant",
                                  "companyPleasant", "alonePleasant", "posMax", "posIntensity",
                                  "negMax", "negIntensity"), na.rm = TRUE)

thoughtsValence_names <- c(
  `1` = "Negative",
  `2` = "Neutral",
  `3` = "Positive"
)

#overview negative/positive affect
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsValence~., scale = "free", labeller=as_labeller(thoughtsValence_names)) +
  labs(title = "Levels of PA and NA",
       subtitle = " by group and valence of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#overview rumination
ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsValence~., scale = "free", labeller = as_labeller(thoughtsValence_names)) +
  labs(title = "Levels of Rumination",
       subtitle = " by group and valence of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


ggplot(data = meltData[which(meltData$variable=="stickiness"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsValence~., scale = "free", labeller = as_labeller(thoughtsValence_names)) +
  labs(title = "Levels of Stickiness",
       subtitle = " by group and valence of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


#with thoughtsObject
meltData <- melt(data, id.vars=c("group", "thoughtsObject"),
                 measure.vars = c("sumPA", "sumNA", "ruminating",
                                  "sleepQuality", "restednessWakeup", "stickiness",
                                  "anxious", "down", "irritated", "restless",
                                  "wakeful", "energetic", "satisfied",
                                  "listless", "distracted", "restOfDayPos", "thoughtsPleasant",
                                  "companyPleasant", "alonePleasant", "posMax", "posIntensity",
                                  "negMax", "negIntensity"), na.rm = TRUE)

thoughtsObject_names <- c(
  `1` = "Myself",
  `2` = "Someone else",
  `3` = "Neither"
)

#overview negative/positive affect
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsObject~., scale = "free", labeller=as_labeller(thoughtsObject_names)) +
  labs(title = "Levels of PA and NA",
       subtitle = " by group and object of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#overview rumination
ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsObject~., scale = "free", labeller = as_labeller(thoughtsObject_names)) +
  labs(title = "Levels of Rumination",
       subtitle = " by group and object of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


ggplot(data = meltData[which(meltData$variable=="stickiness"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thoughtsObject~., scale = "free", labeller = as_labeller(thoughtsObject_names)) +
  labs(title = "Levels of Stickiness",
       subtitle = " by group and object of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#with thinkingOf
meltData <- melt(data, id.vars=c("group", "thinkingOf"),
                 measure.vars = c("sumPA", "sumNA", "ruminating",
                                  "sleepQuality", "restednessWakeup", "stickiness",
                                  "anxious", "down", "irritated", "restless",
                                  "wakeful", "energetic", "satisfied",
                                  "listless", "distracted", "restOfDayPos", "thoughtsPleasant",
                                  "companyPleasant", "alonePleasant", "posMax", "posIntensity",
                                  "negMax", "negIntensity"), na.rm = TRUE)

thinkingOf_names <- c(
  `1` = "Current Activity",
  `2` = "External Stimuli",
  `3` = "Current Feelings",
  `4` = "Personal Concerns",
  `5` = "Daydreaming",
  `6` = "Other"
)

#overview negative/positive affect
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thinkingOf~., scale = "free", labeller=as_labeller(thinkingOf_names)) +
  labs(title = "Levels of PA and NA",
       subtitle = " by group and object of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#overview rumination
ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thinkingOf~., scale = "free", labeller = as_labeller(thinkingOf_names)) +
  labs(title = "Levels of Rumination",
       subtitle = " by group and object of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


ggplot(data = meltData[which(meltData$variable=="stickiness"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(thinkingOf~., scale = "free", labeller = as_labeller(thinkingOf_names)) +
  labs(title = "Levels of Stickiness",
       subtitle = " by group and content of current thoughts",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


#with aloneCompany
meltData <- melt(data, id.vars=c("group", "aloneCompany"),
                 measure.vars = c("sumPA", "sumNA", "ruminating",
                                  "sleepQuality", "restednessWakeup", "stickiness",
                                  "anxious", "down", "irritated", "restless",
                                  "wakeful", "energetic", "satisfied",
                                  "listless", "distracted", "restOfDayPos", "thoughtsPleasant",
                                  "companyPleasant", "alonePleasant", "posMax", "posIntensity",
                                  "negMax", "negIntensity"), na.rm = TRUE)

aloneCompany_names <- c(
  `1` = "Alone",
  `2` = "in Company"
)

#overview negative/positive affect
ggplot(data = meltData[which((meltData$variable=="sumPA") | (meltData$variable=="sumNA")),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(aloneCompany~., scale = "free", labeller=as_labeller(aloneCompany_names)) +
  labs(title = "Levels of PA and NA",
       subtitle = " by group and social context",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))

#overview rumination
ggplot(data = meltData[which(meltData$variable=="ruminating"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(aloneCompany~., scale = "free", labeller = as_labeller(aloneCompany_names)) +
  labs(title = "Levels of Rumination",
       subtitle = " by group and social context",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


ggplot(data = meltData[which(meltData$variable=="stickiness"),],
       aes(variable, y = value, fill = factor(group))) +
  geom_boxplot() + facet_grid(aloneCompany~., scale = "free", labeller = as_labeller(aloneCompany_names)) +
  labs(title = "Levels of Stickiness",
       subtitle = " by group and social context",
       y = "Raw score", x = "") + scale_fill_manual(values = c("green", "red"))


##################################################### Not so interesting plots ###########################################################
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


################################### sleep ######################################
meltChange <- melt(data[which(data$phase=="peri"),], id.vars=c("group", "intervention", "phase", "block", "phaseAssessmentDay"),
                   measure.vars = "sleepQuality", na.rm = TRUE)
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
                                 (data$intervention==intervention)),]$sleepQuality, na.rm=TRUE)
    
    color <- "green"
    lab_y <- "Avg change in rumination"
    main <- "Change in Sleep Quality per Assessment Day compared to baseline mean"
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
          'listless', 'thinkingOf', 'ruminating', 'stickiness', 'thoughtsPleasant', 'thoughtsTime',
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
                  worrying_avg = mean(ruminating_change, na.rm = TRUE),
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
                   worrying_avg = mean(ruminating_change, na.rm = TRUE),
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



