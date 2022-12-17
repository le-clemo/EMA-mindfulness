
#################################### Set up ####################################
rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/") #set wd accordingly
source("./common_plot_theme.R")

figure_path <- "./Figures/ESM_descriptive"

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data/ESM/mindcog_v202205")

library(readxl)
library(ggh4x)
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
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(broom)
library(effectsize)
library(languageR)
library(ggthemes)
library(ggpattern)
library(Rmisc)
library(tikzDevice)

#read in data
data <- read.csv('ESM/mindcog_v202207/preprocessed_data.csv') 

#Pick response rate cut-off value
cutOff <- 0.5

################################# response-related measures #####################################
# #group by id and count the number of nonresponses
participant_responses <- ddply(data, .(subject, group), plyr::summarise,
                               numBeeped = length(mindcog_db_open_from),
                               noResponse = length(unique(mindcog_db_non_response)),
                               response = numBeeped - noResponse,
                               responseRate = round(response/numBeeped,2),
                               numDays = max(assessmentDay))

#number of participants so far
length(unique(data$subject)) #39 associated with a group

#number of responses
sum(participant_responses$response) #~6000+

#the mean response rate is ~67%
meanResponseRate <- mean(participant_responses$responseRate)
#sd of 18.29
sdResponseRate <- sd(participant_responses$responseRate)

sdResponseRateCon <- round(sd(participant_responses[which(participant_responses$group == "controls"),]$responseRate), 2)
sdResponseRateRem <- round(sd(participant_responses[which(participant_responses$group == "remitted"),]$responseRate), 2)

quantile(participant_responses$responseRate, probs = c(.1, .9))
quantile(participant_responses[which(participant_responses$group == "controls"),]$responseRate, probs = c(.1, .9))
quantile(participant_responses[which(participant_responses$group == "remitted"),]$responseRate, probs = c(.1, .9))

group_responses <- ddply(data, .(group), plyr::summarise,
                                nSubj = length(unique(subject)),
                                numBeeps = length(mindcog_db_open_from),
                                noResponse = length(unique(mindcog_db_non_response)),
                                response = numBeeps - noResponse,
                                responseRate = round(response/numBeeps,2))

group_responses_baseline <- ddply(data[which(data$phase=="pre"),], .(group), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2),
                         SDResponseRate = round(sd(response/numBeeps, na.rm = TRUE), 2))


responses_by_phase <- ddply(data, .(group, intervention, phase), plyr::summarise,
                            nSubj = length(unique(subject)),
                            numBeeps = length(mindcog_db_open_from),
                            noResponse = length(unique(mindcog_db_non_response)),
                            response = numBeeps - noResponse,
                            responseRate = round(response/numBeeps,2))

#group by phase
groupXphase <- ddply(data, .(group, phase), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2),
                         SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))

responses_block <- ddply(data, .(block), plyr::summarise,
                         nSubj = length(unique(subject)),
                         numBeeps = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeps - noResponse,
                         responseRate = round(response/numBeeps,2),
                         SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))

responses_phase <- ddply(data, .(phase), plyr::summarise,
                     nSubj = length(unique(subject)),
                     numBeeps = length(mindcog_db_open_from),
                     noResponse = length(unique(mindcog_db_non_response)),
                     response = numBeeps - noResponse,
                     responseRate = round(response/numBeeps,2),
                     SDResponseRate = round(sd(responseRate, na.rm = TRUE),2))


#Chi-squared tests
#group difference?
chisq.test(group_responses[,c("noResponse", "response")]) #significant difference
#x-sq = 5.92, p = 0.01496

chisq.test(group_responses_baseline[,c("noResponse", "response")]) #significant difference
#X-squared = 7.01, df = 1, p-value = 0.008106

#general difference between blocks
chisq.test(responses_block[, c("noResponse", "response")])

#general difference between phases
chisq.test(responses_phase[, c("noResponse", "response")])

#difference by phase (groupXintervention)?
groups <- c("controls", "remitted")
interventions <- c("fantasizing", "mindfulness")
for(g in groups){
  for(int in interventions){
    print(paste(g, int, sep = " + "))
    responses <- responses_by_phase[which((responses_by_phase$group==g) & (responses_by_phase$intervention==int)),]
    print(chisq.test(responses[, c("noResponse", "response")]))
  }
} #response rates are always worse in peri


######################################## Removing low-response subjects ##############################################

responses_block <- ddply(data, .(subject, group), plyr::summarise,
                         numBeeped = length(mindcog_db_open_from),
                         noResponse = length(unique(mindcog_db_non_response)),
                         response = numBeeped - noResponse,
                         responseRate = round(response/numBeeped,2),
                         numDays = max(assessmentDay))

data$subjB <- interaction(data$subject, data$block, drop = TRUE)

#removing participants with a response rate lower than 50%
pp <- unique(responses_block[which(responses_block$responseRate >= cutOff),]$subject)
data <- data[which(data$subject %in% pp),]

length(unique(data[which(data$group == "remitted"),]$subject)) #12
length(unique(data[which(data$group == "controls"),]$subject)) #21

################################################ Missed assessments ##################################################

missedAssessments <- ddply(data, .(group, phase, blockAssessmentDay), plyr::summarize,
                           nBeeps = length(subjB),
                           nMissed =  length(unique(mindcog_db_non_response)),
                           propMissed = round(nMissed/nBeeps, 2))


p <- ggplot(missedAssessments, aes(x=blockAssessmentDay, y=propMissed, color=group)) +
  geom_line() + geom_point() + geom_vline(xintercept = 7.5, lty = "dashed") +
  ylab("Proportion of assessments missed") + xlab("Block assessment day") 
p$labels$colour <- "Group"
p


##################################### Daily avg per individual ########################################

melt.dat <- melt(data, id.vars=c("subject", "group", "assessmentDay"),
                 measure.vars = c("ruminating", "meanPA", "meanNA"), na.rm = TRUE)
melt.dat <- aggregate(melt.dat$value, by=list(subject=melt.dat$subject, assessmentDay=melt.dat$assessmentDay,
                                              group=melt.dat$group, variable=melt.dat$variable), FUN=mean)

ggplot(melt.dat[which((melt.dat$variable=="ruminating") & (melt.dat$assessmentDay<=7)),],
       aes(x=assessmentDay, y=x, group=subject, color=group, fill = group)) + theme_bw() +
  geom_line(alpha = 0.3) +#+ stat_summary(fun = mean, na.rm = TRUE, geom ='line', group="group", lwd = 1) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", lwd = 3, group = "") +
  stat_summary(fun = mean, na.rm = TRUE, geom = "line", group = "group") + ylab("Rumination")


#################################################### random subject scores ###############################################
data$group <- factor(data$group, levels = c("remitted", "controls"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))

set.seed(1)
control_subj <- unique(data[which(data$group == "controls"),]$subject)
remitted_subj <- unique(data[which(data$group == "remitted"),]$subject)

rand.cs <- sample(control_subj, 6)
rand.rm <- sample(remitted_subj, 6)
rand.subj <- append(rand.cs, rand.rm)

subCon <- data[which((data$subject %in% rand.cs) &
                       (data$block==1) &
                       (data$phase == "pre") & 
                       (data$phaseBeepNum <= 70)),] 
subRem <- data[which((data$subject %in% rand.rm) &
                       (data$block==1) &
                       (data$phase == "pre") &
                       (data$phaseBeepNum <= 70)),]



idVars <- c("subject", "group", "intervention", "phase", "beepNum")

meltCon <- melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, beepNum, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, beepNum, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltCon <- meltCon[!duplicated(meltCon[c(1,8)]), ]
meltRem <- meltRem[!duplicated(meltRem[c(1,8)]), ]

levels(meltCon$group) <- c("rMDD", "HC")
levels(meltRem$group) <- c("rMDD", "HC")

levels(meltCon$intervention) <- c("Mindfulness", "Fantasizing")
levels(meltRem$intervention) <- c("Mindfulness", "Fantasizing")

# Reduce the opacity of the grid lines: Default is 255
# col_grid <- rgb(255, 255, 255, 60, maxColorValue = 255)

pdf(width = 10, height = 5.5,
    file = paste(figure_path, "individual_rum_plots.pdf", sep = "/"))  #bg = "#D5E4EB"
#individual rumination plots
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() + 
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61), labels = c(1:7))

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61))
  # scale_y_continuous(minor_breaks = seq(0,100,25))

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination", bottom = "Assessment Day")

dev.off()


#individual NA plots
pdf(width = 10, height = 5.5,
    file = paste(figure_path, "individual_NA_plots.pdf", sep = "/"))  #bg = "#D5E4EB"
p1 <- ggplot(data = meltCon[which(meltCon$variable=="meanNA"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() + 
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61), labels = c(1:7))

p2 <- ggplot(data = meltRem[which(meltRem$variable=="meanNA"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61))
# scale_y_continuous(minor_breaks = seq(0,100,25))

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Negative Affect", bottom = "Assessment Day")

dev.off()


#individual PA plots
pdf(width = 10, height = 5.5,
    file = paste(figure_path, "individual_PA_plots.pdf", sep = "/"))  #bg = "#D5E4EB"
p1 <- ggplot(data = meltCon[which(meltCon$variable=="meanPA"),],
             aes(x=beepNum, y=Mean)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() + 
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61), labels = c(1:7))

p2 <- ggplot(data = meltRem[which(meltRem$variable=="meanPA"),],
             aes(x=beepNum, y=Mean, color = "pink")) +
  geom_line() + geom_point() + ylim(0,100) +
  facet_grid(group~factor(subject), scale = "free") +
  multi_plot_theme() +
  theme(axis.text.x = element_blank()) +
  scale_x_continuous(breaks=c(1,11,21,31,41,51,61))
# scale_y_continuous(minor_breaks = seq(0,100,25))

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Positive Affect", bottom = "Assessment Day")

dev.off()


###################################### variable as a function of most unpleasant previous event ###################################################
idVars <- c("subject", "group", "intervention", "phase", "negMax")

meltCon <- melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)
meltRem <- melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars)


meltCon <-  within( melt(subCon[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, negMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltRem <-  within( melt(subRem[, c(idVars, "ruminating", "meanNA", "meanPA"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, negMax, variable, subject, FUN=function(x) mean(x,na.rm=T))})

levels(meltCon$group) <- c("rMDD", "HC")
levels(meltRem$group) <- c("rMDD", "HC")

#rumination
pdf(width = 10, height = 5.5,
    file = paste(figure_path, "individual_rum_by_negMax.pdf", sep = "/"))  #bg = "#D5E4EB"
p1 <- ggplot(data = meltCon[which(meltCon$variable=="ruminating"),],
             aes(x=negMax, y=Mean)) +
  geom_point(color = "#619CFF") + ylim(0,100) + xlim(0,100) +
  multi_plot_theme() +
  facet_grid(group~factor(subject), scale = "free") +
  geom_smooth(method = "lm", se = FALSE, col = "black")

p2 <- ggplot(data = meltRem[which(meltRem$variable=="ruminating"),],
             aes(x=negMax, y=Mean, color = "pink")) +
  geom_point() + ylim(0,100) + xlim(0,100) +
  multi_plot_theme() +
  facet_grid(group~factor(subject), scale = "free") +
  geom_smooth(method = "lm", se = FALSE, col = "black")

grid.arrange(p2, p1, ncol=1, nrow = 2, left = "Rumination",
             bottom = "Event Unpleasantness")

dev.off()


################################################ Baseline boxplots #####################################################################

### boxplot comparisons with all data points separately
group.colors = c(rMDD = "#F8766D", HC = "#619CFF")

### boxplots of daily avg
idVars <- c("subject", "group", "intervention", "phase", "assessmentDay")

meltDat <-  within( melt(data[, c(idVars, "ruminating", "meanNA", "meanPA", "negMax", "posMax",
                                  "down", "irritated", "restless", "anxious",
                                  "satisfied", "wakeful", "energetic"),], id.vars = idVars), {
  variable<- gsub("\\_.*","",variable)
  Mean<- ave(value, assessmentDay, variable, subject, FUN=function(x) mean(x,na.rm=T))})

meltDat <- meltDat[!duplicated(meltDat[c(1,5,8)]), ]
meltDat$group <- factor(meltDat$group, levels = c("remitted", "controls"))
meltDat$intervention <- factor(meltDat$intervention, levels = c("mindfulness", "fantasizing"))

levels(meltDat$group) <- c("rMDD", "HC")
levels(meltDat$intervention) <- c("Mindfulness", "Fantasizing")

pdf(width = 8, height = 6,
    file = paste(figure_path, "daily_avg_rum_boxplots.pdf", sep = "/"))
ggplot(data = meltDat[which(meltDat$variable=="ruminating"),],
             aes(x= factor(phase, levels = c("pre", "peri")), y=Mean,
                 fill = group)) +
  geom_boxplot() + ylim(0,100) +
  single_plot_theme() +
  ylab("Rumination") + 
  xlab("Phase") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  scale_fill_manual(values=group.colors)

dev.off()

#NA
pdf(width = 8, height = 6,
    file = paste(figure_path, "daily_avg_NA_boxplots.pdf", sep = "/"))
ggplot(data = meltDat[which(meltDat$variable=="meanNA"),],
       aes(x= factor(phase, levels = c("pre", "peri")), y=Mean,
           fill = group)) +
  geom_boxplot() + ylim(0,100) +
  single_plot_theme() +
  ylab("Negative Affect") +
  xlab("Phase") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  # axis.ticks.x=element_blank()) +
  scale_fill_manual(values=group.colors)

dev.off()


#PA
pdf(width = 8, height = 6,
    file = paste(figure_path, "daily_avg_PA_boxplots.pdf", sep = "/"))
ggplot(data = meltDat[which(meltDat$variable=="meanPA"),],
       aes(x= factor(phase, levels = c("pre", "peri")), y=Mean,
           fill = group)) +
  geom_boxplot() + ylim(0,100) +
  single_plot_theme() +
  ylab("Positive Affect") +
  xlab("Phase") +
  facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  # axis.ticks.x=element_blank()) +
  scale_fill_manual(values=group.colors)

dev.off()

################################# RQ1 visualization ################################################

group.colors = c(HC = "#619CFF", rMDD = "#F8766D")

SE_rum1 <- summarySE(data[which((data$block==1)),], measurevar = "ruminating", groupvars = c("group", "phase"), 
                   na.rm = T)

SE_rum1$group <- factor(SE_rum1$group, levels = c("controls", "remitted"))
levels(SE_rum1$group) <- c("HC", "rMDD")
SE_rum1$phase <- factor(SE_rum1$phase, levels = c("pre", "peri"))
levels(SE_rum1$phase) <- c("Pre", "Peri")

p1 <- ggplot(SE_rum1[which(SE_rum1$phase=="Pre"),], aes(y =  ruminating, x=group, group=1), color = group) +
  geom_line(color = c("black"), size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = ruminating-se, ymax = ruminating+se),width = 0.5, color = rev(group.colors), size=0.5) +
  geom_point(size = 3, color = rev(group.colors))+
  ylim(5,25) +
  single_plot_theme() +
  ylab("Rumination") + 
  # facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        # axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  # coord_cartesian(clip='off') +
  scale_fill_manual(values=rev(group.colors)) +
  geom_text(
    mapping = aes(x = 1.5, y = 13.4, label = "*"), size = 10
  )
  # scale_x_discrete(position = "top")
  # annotation_custom(
  #   grob=muh_grob, xmin = -0.2, xmax = 1, ymin = 29, ymax=33
  # )


SE_na1 <- summarySE(data[which((data$block==1)),], measurevar = "meanNA", groupvars = c("group", "phase"), 
                     na.rm = T)

SE_na1$group <- factor(SE_na1$group, levels = c("controls", "remitted"))
levels(SE_na1$group) <- c("HC", "rMDD")
SE_na1$phase <- factor(SE_na1$phase, levels = c("pre", "peri"))
levels(SE_na1$phase) <- c("Pre", "Peri")

p2 <- ggplot(SE_na1[which(SE_na1$phase=="Pre"),], aes(y =  meanNA, x=group, group=1), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanNA-se, ymax = meanNA+se),width = 0.5, color = rev(group.colors), size=0.5) +
  geom_point(size = 3, color = rev(group.colors))+
  ylim(5,25) +
  single_plot_theme() +
  ylab("Negative Affect") + 
  # facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_blank(),
        axis.title.x=element_blank()) +
  scale_fill_manual(values=rev(group.colors)) +
  geom_text(
    mapping = aes(x = 1.5, y = 11.2, label = "*"), size = 10
  )


SE_pa1 <- summarySE(data[which((data$block==1)),], measurevar = "meanPA", groupvars = c("group", "phase"), 
                    na.rm = T)

SE_pa1$group <- factor(SE_pa1$group, levels = c("controls", "remitted"))
levels(SE_pa1$group) <- c("HC", "rMDD")
SE_pa1$phase <- factor(SE_pa1$phase, levels = c("pre", "peri"))
levels(SE_pa1$phase) <- c("Pre", "Peri")

p3 <- ggplot(SE_pa1[which(SE_pa1$phase=="Pre"),], aes(y =  meanPA, x=group, group=1), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanPA-se, ymax = meanPA+se),width = 0.5, color = rev(group.colors), size=0.5) +
  geom_point(size = 3, color = rev(group.colors))+
  ylim(50,70) +
  single_plot_theme() +
  ylab("Positive Affect") + 
  xlab("Group") +
  # facet_grid(factor(intervention)~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  scale_fill_manual(values=rev(group.colors))

rq1_plot <- grid.arrange(p1, p2, p3, nrow=3)

ggsave(rq1_plot, file=paste(figure_path, "rq1_group_diff.pdf", sep = "/"), width = 4, height = 10)



############################################# RQ 2 visualization ###############################################################
# calculate SE by taking repeated measures into account
group.colors <- c("#619CFF", "#F8766D")
group.colors2 <- c("#619CFF", "#619CFF", "#F8766D", "#F8766D")
peri_dat <- data[which(data$phase=="peri"),]

source("./summarySEwithin2.R") #load function

SE_rum2 <- summarySEwithin2(peri_dat, measurevar = "ruminating_gam", betweenvars = c("group"),
                     withinvars = c("intervention"), idvar="subject", na.rm = T)

SE_rum2$group <- factor(SE_rum2$group)
levels(SE_rum2$group) <- c("HC", "rMDD")
SE_rum2$intervention <- factor(SE_rum2$intervention)
levels(SE_rum2$intervention) <- c("Fantasizing", "Mindfulness")


dat_text <- data.frame(
  label = c("*", "*", "*"),
  group   = c("HC", "HC", "HC"),
  x     = c(1, 1.5, 2),
  y     = c(2.5, -0.15, -3.05)
)

p1 <- ggplot(SE_rum2, aes(y =  ruminating_gam, x=intervention, group=1,), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = ruminating_gam-ci, ymax = ruminating_gam+ci),width = 0.5, size=0.5, color = group.colors2) +
  geom_point(size = 3, color = group.colors2)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) +
  # annotate("text", x = 4, y=2, label = "ship") +
  ylim(-5,5) +
  single_plot_theme() +
  ylab("Rumination") + 
  facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none", strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_manual(values=group.colors) +
  geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label), size = 10
)

##na
SE_na2 <- summarySEwithin2(peri_dat, measurevar = "meanNA_gam", betweenvars = c("group"),
                            withinvars = c("intervention"), idvar="subject", na.rm = T)

SE_na2$group <- factor(SE_na2$group)
levels(SE_na2$group) <- c("HC", "rMDD")
SE_na2$intervention <- factor(SE_na2$intervention)
levels(SE_na2$intervention) <- c("Fantasizing", "Mindfulness")

dat_text <- data.frame(
  label = c("*", "*", "*", "*", "*", "*"),
  group   = c("HC", "HC", "HC", "rMDD", "rMDD", "rMDD"),
  x     = c(1, 1.5, 2, 1, 1.5, 2),
  y     = c(0.25, -0.7, -1.6, 0.25, -0.6, -1.4)
)

p2 <- ggplot(SE_na2, aes(y =  meanNA_gam, x=intervention, group=1,), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanNA_gam-se, ymax = meanNA_gam+se),width = 0.5, size=0.5, color = group.colors2) +
  geom_point(size = 3, color = group.colors2)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) +
  ylim(-5,5) +
  single_plot_theme() +
  ylab("Negative Affect") + 
  facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none",
        strip.text = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank()) +
  scale_fill_manual(values=group.colors) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label), size = 10
  )

##pa
SE_pa2 <- summarySEwithin2(peri_dat, measurevar = "meanPA_gam", betweenvars = c("group"),
                           withinvars = c("intervention"), idvar="subject", na.rm = T)

SE_pa2$group <- factor(SE_pa2$group)
levels(SE_pa2$group) <- c("HC", "rMDD")
SE_pa2$intervention <- factor(SE_pa2$intervention)
levels(SE_pa2$intervention) <- c("Fantasizing", "Mindfulness")

dat_text <- data.frame(
  label = c("*", "*", "*", "*"),
  group   = c("HC", "HC", "rMDD", "rMDD"),
  x     = c(1.5, 2, 1.5, 2),
  y     = c(0.3, 1.2, -1.7, -3.4)
)

p3 <- ggplot(SE_pa2, aes(y =  meanPA_gam, x=intervention, group=1,), color = group) +
  geom_line(color = "black", size=1, alpha = 0.15) +
  geom_errorbar(aes(ymin = meanPA_gam-se, ymax = meanPA_gam+se),width = 0.5, size=0.5, color = group.colors2) +
  geom_point(size = 3, color = group.colors2)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.5) +
  ylim(-5,5) +
  xlab("Intervention") +
  single_plot_theme() +
  ylab("Positive Affect") + 
  facet_grid(.~factor(group), scale = "free") +
  theme(legend.position="none",
        strip.text.x = element_blank(),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=8),
        strip.background = element_blank()) +
  scale_fill_manual(values=group.colors) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label), size = 10
  )

rq2_plot <- grid.arrange(p1, p2, p3, nrow=3)

ggsave(rq2_plot, file=paste(figure_path, "rq2.pdf", sep = "/"), width = 4, height = 10)





