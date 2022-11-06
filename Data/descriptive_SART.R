
rm(list = ls()) #clean all up
source("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data/common_plot_theme.R")
setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape)
library(ggpubr)
library(lubridate)
library(igraph)

numbers <- read.csv("sart_w_thoughtProbes.csv")

numbers <- numbers[which(!is.na(numbers$group)),]

numbers$group <- factor(numbers$group, levels = c("remitted", "controls"))
numbers$intervention <- factor(numbers$intervention, levels = c("mindfulness", "fantasizing"))
levels(numbers$group) <- c("rMDD", "HC")
levels(numbers$intervention) <- c("Mindfulness", "Fantasizing")


length(unique(numbers$subject)) #35
length(unique(numbers[which(numbers$group == "rMDD"),]$subject)) #14
length(unique(numbers[which(numbers$group == "HC"),]$subject)) #21
unique_sessions <- numbers %>% distinct(subject, gameSessionID)
length(unique_sessions$gameSessionID) #589
length(numbers$id) #28221
length(numbers[which(numbers$isGo==FALSE),]$id) #2781
length(numbers[which(numbers$isGo==TRUE),]$id) #25440

mean(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1)),]$responseTime, na.rm=TRUE)
mean(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1)),]$responseTime, na.rm=TRUE)

sd(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1)),]$responseTime, na.rm=TRUE)
sd(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1)),]$responseTime, na.rm=TRUE)

mean(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1)),]$correct, na.rm=TRUE)
mean(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1)),]$correct, na.rm=TRUE)

sd(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1)),]$correct, na.rm=TRUE)
sd(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1)),]$correct, na.rm=TRUE)

mean(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$correct, na.rm=TRUE)
mean(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$correct, na.rm=TRUE)

sd(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$correct, na.rm=TRUE)
sd(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$correct, na.rm=TRUE)

mean(numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness")),]$responseTime, na.rm=TRUE)
mean(numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing")),]$responseTime, na.rm=TRUE)

mean(numbers[which((numbers$group=="HC") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness")),]$responseTime, na.rm=TRUE)
mean(numbers[which((numbers$group=="HC") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing")),]$responseTime, na.rm=TRUE)


## response times at baseline
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1)),]$responseTime,
       numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1)),]$responseTime)

t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==TRUE)),]$responseTime,
       numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==TRUE)),]$responseTime)

t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$responseTime,
       numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$responseTime)


## correct trials at baseline
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1)),]$correct,
       numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1)),]$correct)

t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==TRUE)),]$correct,
       numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==TRUE)),]$correct)

t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$correct,
       numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$block==1) & (numbers$isGo==FALSE)),]$correct)

## response times peri-intervention
# rMDD x mindfulness
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$intervention=="Mindfulness")),]$responseTime,
       numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness")),]$responseTime)
# rMDD x fantasizing
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$intervention=="Fantasizing")),]$responseTime,
       numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing")),]$responseTime)

# HC x mindfulness
t.test(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$intervention=="Mindfulness")),]$responseTime,
       numbers[which((numbers$group=="HC") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness")),]$responseTime)
# rMDD x fantasizing
t.test(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$intervention=="Fantasizing")),]$responseTime,
       numbers[which((numbers$group=="HC") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing")),]$responseTime)

## correct trials peri-intervention
# rMDD x mindfulness
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$intervention=="Mindfulness")),]$correct,
       numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness")),]$correct)
# rMDD x fantasizing
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$intervention=="Fantasizing")),]$correct,
       numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing")),]$correct)

# rMDD x mindfulness x NoGo
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$intervention=="Mindfulness") & (numbers$isGo==FALSE)),]$correct,
       numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness") & (numbers$isGo==FALSE)),]$correct)
# rMDD x fantasizing x NoGo
t.test(numbers[which((numbers$group=="rMDD") & (numbers$phase=="pre") & (numbers$intervention=="Fantasizing") & (numbers$isGo==FALSE)),]$correct,
       numbers[which((numbers$group=="rMDD") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing") & (numbers$isGo==FALSE)),]$correct)

# HC x mindfulness
t.test(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$intervention=="Mindfulness")),]$correct,
       numbers[which((numbers$group=="HC") & (numbers$phase=="peri") & (numbers$intervention=="Mindfulness")),]$correct)
# rMDD x fantasizing
t.test(numbers[which((numbers$group=="HC") & (numbers$phase=="pre") & (numbers$intervention=="Fantasizing")),]$correct,
       numbers[which((numbers$group=="HC") & (numbers$phase=="peri") & (numbers$intervention=="Fantasizing")),]$correct)

numbers_mean <- numbers %>%
  distinct(userID, gameSessionID, .keep_all = TRUE)

group.colors = c(rMDD = "#F8766D", HC = "#619CFF")

meltDat <- melt(numbers_mean, id.vars = c("group", "intervention", "phase"), measure.vars = c("meanRT"))
# [which((numbers_mean$correct==TRUE) & (numbers_mean$phase=="pre") & (numbers_mean$block==1)),]

pdf(width = 8, height = 6,
    file = "meanRT_boxplots.pdf")
ggplot(meltDat[which((!is.na(meltDat$intervention) & (!is.na(meltDat$group)))),]) +
  geom_boxplot(aes(x= factor(phase, levels = c("pre", "peri")), y=value, fill=group)) +
  facet_grid(intervention~group) +
  labs(y = "Mean response time", x = "Phase") + 
  ylim(300,1000) +
  single_plot_theme() +
  theme(legend.position = "None",
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size =15),
        axis.text.x = element_text(size=15)) +
  scale_fill_manual(values=group.colors)

dev.off()

mean(meltDat[which(meltDat$group=="HC"),]$value, na.rm=T) #631.390
mean(meltDat[which(meltDat$group=="rMDD"),]$value, na.rm=T) #613.380

sd(meltDat[which(meltDat$group=="HC"),]$value, na.rm=T) #229.758
sd(meltDat[which(meltDat$group=="rMDD"),]$value, na.rm=T) #102.179

quantile(meltDat[which(meltDat$group=="HC"),]$value, probs = c(.1, .9), na.rm=T)
#10% = 508.112
#90% = 745.597

quantile(meltDat[which(meltDat$group=="rMDD"),]$value, probs = c(.1, .9), na.rm=T)
#10% = 511.232
#90% = 749.340


meltDat <- melt(numbers_mean[which((numbers_mean$isGo==FALSE) & (numbers_mean$phase=="pre")),], id.vars = c("group"), measure.vars = c("propCor"))

ggplot(meltDat) +
  geom_boxplot(aes(y=value, fill=group)) +
  facet_grid(.~group) +
  labs(y = "Mean proportion of correct NoGo-trials") + 
  ylim(0.8,1) +
  single_plot_theme() +
  theme(legend.position = "None",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values=group.colors)

length(meltDat[which(meltDat$group  =="rMDD"),]$value)
length(meltDat[which(meltDat$group  =="HC"),]$value)


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
  ylim(0,1200) +
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
  ylim(0,1200) +
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
  ylim(0,1200) +
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
  ylim(0,1200) +
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


