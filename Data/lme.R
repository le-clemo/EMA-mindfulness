rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

library(ggplot2)
library(data.table)
library(lme4)
library(mgcv)
library(itsadug)
library(dplyr)
library(plyr)
library(lmerTest) #to more quickly be able to see significance
library(plotfunctions)

R.version.string
packageVersion("mgcv")
packageVersion("itsadug")

#load preprocessed ESM data
data <- read.csv('ESM/mindcog_v202204/preprocessed_data.csv') 

#set factors
data$group <- factor(data$group, levels = c("controls", "remitted"))
data$intervention <- factor(data$intervention, levels = c("mindfulness", "fantasizing"))
data$phase <- factor(data$phase, levels = c("pre", "peri"))
data$grInt <- as.factor(paste(data$group, data$intervention, sep = "."))
data$blockPhase <- as.factor(paste(data$phase, data$block, sep = "."))

hist(data$ruminating)
qqnorm(data$ruminating)
qqline(data$ruminating)

hist(log(data$ruminating+1))
qqnorm(log(data$ruminating+1))
qqline(log(data$ruminating+1))

hist(data$sumNA)
hist(data$sumPA)
hist(data$stickiness)
hist(data$sleepQuality)

############################################ Data visualization #################################################
aggData <- with(data, aggregate(list(ruminating = ruminating, stickiness = stickiness,
                                    sumNA = sumNA, sumPA = sumPA, sleepQuality = sleepQuality),
                               by = list(subject = subject, group = group,
                                         phaseAssessmentDay = phaseAssessmentDay), FUN = mean, na.rm = TRUE))

hist(aggData$ruminating)

means <- with(aggData, aggregate(list(meanRum = ruminating, meanStick = stickiness, 
                                      meanNA = sumNA, meanPA = sumPA, meanSleep = sleepQuality),
                                 by = list(group = group, phaseAssessmentDay = phaseAssessmentDay), 
                                 FUN = mean, na.rm = TRUE))

aggBlock <- with(data, aggregate(list(ruminating = ruminating, stickiness = stickiness,
                                     sumNA = sumNA, sumPA = sumPA, sleepQuality = sleepQuality),
                                by = list(subject = subject, group = group, intervention = intervention,
                                          phaseAssessmentDay = phaseAssessmentDay), FUN = mean, na.rm = TRUE))

#scatter plots with smooth
#rumination
ggplot(aggData, aes(phaseAssessmentDay, ruminating, group = subject, color=group)) + geom_point()+
    geom_smooth(method = "lm", aes(group=group))
#NA
ggplot(aggData, aes(phaseAssessmentDay, sumNA, group = subject, color=group)) + geom_point()+
  geom_smooth(method = "lm", aes(group=group))
#PA
ggplot(aggData, aes(phaseAssessmentDay, sumPA, group = subject, color=group)) + geom_point()+
  geom_smooth(method = "lm", aes(group=group))
#stickiness
ggplot(aggData, aes(phaseAssessmentDay, stickiness, group = subject, color=group)) + geom_point()+
  geom_smooth(method = "lm", aes(group=group))
#sleep quality
ggplot(aggData, aes(phaseAssessmentDay, sleepQuality, group = subject, color=group)) + geom_point()+
  geom_smooth(method = "lm", aes(group=group))


############################################ RE structure #################################################
# m1 <- lmer(ruminating_lag1 ~  grInt * blockPhase * sumNA * sumPA * stressed * listless * thinkingOf *
#              stickiness * thoughtsPleasant * thoughtsTime * thoughtsValence * thoughtsObject * distracted *
#              restOfDayPos * aloneCompany * companyPleasant * alonePleasant * posMax * posIntensity *
#              negMax * negIntensity + (1 + assessmentDay | subject) + (1 | blockPhase) + (1 | grInt), data = data)
# #converging warning --> consider rescaling
pvars <- c( "ruminating_lag1", "sumNA", "stressed", "listless", "stickiness",# Scaling numeric parameters
            "sumPA")

sc_data <- copy(data)
sc_data[pvars] <- lapply(data[pvars],scale)


################################# Simpler LM for illustration #######################################
#RE structure
sc_data$subjB <- interaction(sc_data$subject, sc_data$block, drop = TRUE)

# t1 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * sumPA * phase +
#              (1 + phaseAssessmentDay | subjB) + (1 + phaseAssessmentDay | group) + (1 + phaseAssessmentDay | intervention) + 
#              (1 + phaseAssessmentDay|phase), data = sc_data)
# 
# t1 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * sumPA * phase +
#              (0 + phaseAssessmentDay | subjB) + (1|subjB) + (1|group), data = sc_data)
# 
# t2 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * sumPA * phase +
#              (0 + phaseAssessmentDay | subjB) + (1|subjB) + (1|group) + (0 + phaseAssessmentDay | intervention), data = sc_data)
# 
# anova(t2,t1,refit=FALSE)#not significant

t3 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * sumPA * phase +
                (0 + phaseAssessmentDay | subjB) + (1|subjB), data = sc_data)

# anova(t1, t3, refit=FALSE) #not significant -> t3 is better
# 
# t4 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * sumPA * phase +
#              (1|subjB), data = sc_data)
# 
# anova(t3, t4, refit=FALSE) #significant --> t3 best model
# 
# 
# t5 <- lmer(ruminating_lag1 ~  phaseAssessmentDay + group * intervention * sumNA * sumPA * phase +
#              (1|subjB), data = sc_data)
# anova(t3, t5)#significant
# 
# t6 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * phase + sumNA * sumPA +
#              (1|subjB), data = sc_data)
# anova(t3, t6)#sig
# 
# t7 <- lmer(ruminating_lag1 ~  intervention + phaseAssessmentDay * group * phase * sumNA * sumPA +
#              (1|subjB), data = sc_data)
# anova(t3, t7)#significant
# 
# t8 <- lmer(ruminating_lag1 ~  group + phaseAssessmentDay * intervention * phase * sumNA * sumPA +
#              (1|subjB), data = sc_data)
# anova(t3, t8)#sig
# 
# t9 <- lmer(ruminating_lag1 ~  phase + phaseAssessmentDay * group * intervention * sumNA * sumPA +
#              (1|subjB), data = sc_data)
# anova(t3, t9)
# 
# t10 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * phase * sumNA + sumPA +
#               (1|subjB), data = sc_data)
# anova(t3, t10)

#residual diagnostics
qqnorm(resid(t3))
qqline(resid(t3))
#residuals definitely not normally distributed --> more of a t-distribution

#Plot1
x <- resid(t3)
h <- hist(x, col = "grey", breaks = 40)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

# PLOT 2:
plot(fitted(t3), resid(t3))
abline(h=0)
plot(data$phaseAssessmentDay, resid(t3))
abline(h=0)
# PLOT 3:
acf(resid(t3))

resid_m <- resid(t3)
match(c(min(resid_m),max(resid_m)),resid_m) #indices 434 3781
fe_m <- fitted(t3)

resid_m[c(434, 3781)]
fe_m[c(434, 3781)]

########################################## Hopefully cool plot #########################################
summary(t3)

sc_data$subject_num <- NA
for(j in 1:nrow(sc_data)){
  sc_data$subject_num[j] <- as.numeric(sc_data$subjB[j])
}
#plotting random effects
cf.m2 <- coef(t3)
groups <- list('controls.fantasizing', 'controls.mindfulness',
               'remitted.fantasizing', 'remitted.mindfulness')
par(cex=1.1)
emptyPlot(range(data$phaseAssessmentDay), range(fitted(t3)),
          xlab="phaseAssessmentDay", ylab="Rumination score (lag 1)", v0=0,
          xmark=TRUE, ymark=TRUE, las=1,
          main="Subject lines")

#test <- c(23,28,25,17)
start <- 1
end <- 0
cx <- 1

for (g in groups){
  len_gr <- length(unique(sc_data[sc_data$grInt == g,]$subject_num))
  end <- end + len_gr
  a_list = list()
  b_list = list()
  for(i in start:end ){
    #intercept adjustments
    intercept <- cf.m2$subjB$`(Intercept)`[i]
    # group.intervention <- ifelse(sc_data[sc_data$subjB == i,]$grInt[1] == 'controls.mindfulness',
    #                              cf.m2$subjB$grIntcontrols.mindfulness[i],
    #                              ifelse(sc_data[sc_data$subjB==i,]$grInt[1]=="remitted.fantasizing",
    #                                     cf.m2$subjB$grIntremitted.fantasizing[i],
    #                                     ifelse(sc_data[sc_data$subjB==i,]$grInt[1]=="remitted.mindfulness",
    #                                            cf.m2$subjB$grIntremitted.mindfulness[i], 0)))
    
    gr <- ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted", cf.m2$subjB$groupremitted[i],0)
    interv <- ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing", cf.m2$subjB$interventionfantasizing[i],0)
    peri <- ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri", cf.m2$subjB$phaseperi[i], 0)
    gr.int <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                        sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                     cf.m2$subjB$`groupremitted:interventionfantasizing`[i],0)
    gr.ph <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                       sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                    cf.m2$subjB$`groupremitted:phaseperi`[i],0)
    int.ph <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                        sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                     cf.m2$subjB$`interventionfantasizing:phaseperi`[i],0)
    rem_fant_peri <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                               sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") &
                              (sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                            cf.m2$subjB$`groupremitted:interventionfantasizing:phaseperi`[i],0)
    

    #slope adjustments
    day <- cf.m2$subjB$phaseAssessmentDay[i]
    sNA <- cf.m2$subjB$sumNA[i]
    sPA <- cf.m2$subjB$sumPA[i]
    rem_day <- ifelse(sc_data[sc_data$subject_num==i,]$group=="remitted",
                      cf.m2$subjB$`phaseAssessmentDay:groupremitted`[i],0)
    fant_day <- ifelse(sc_data[sc_data$subject_num==i,]$intervention=="fantasizing",
                       cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing`[i],0)
    NA_day <- cf.m2$subjB$`phaseAssessmentDay:sumNA`[i]
    rem_NA <- ifelse(sc_data[sc_data$subject_num==i,]$group=="remitted",
                     cf.m2$subjB$`groupremitted:sumNA`[i],0)
    fant_NA <- ifelse(sc_data[sc_data$subject_num==i,]$intervention=="fantasizing",
                      cf.m2$subjB$`interventionfantasizing:sumNA`[i],0)
    PA_day <- cf.m2$subjB$`phaseAssessmentDay:sumPA`[i]
    rem_PA <- ifelse(sc_data[sc_data$subject_num==i,]$group=="remitted",
                     cf.m2$subjB$`groupremitted:sumPA`[i],0)
    fant_PA <- ifelse(sc_data[sc_data$subject_num==i,]$intervention=="fantasizing",
                      cf.m2$subjB$`interventionfantasizing:sumPA`[i],0)
    PANA <- cf.m2$subjB$`sumNA:sumPA`[i]
    phase_day <- ifelse(sc_data[sc_data$subject_num==i,]$phase=="peri",
                        cf.m2$subjB$`phaseAssessmentDay:phaseperi`[i],0)
    phase_NA <- ifelse(sc_data[sc_data$subject_num==i,]$phase=="peri",
                        cf.m2$subjB$`sumNA:phaseperi`[i],0)
    phase_PA <- ifelse(sc_data[sc_data$subject_num==i,]$phase=="peri",
                        cf.m2$subjB$`sumPA:phaseperi`[i],0)
    day_rem_fant <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                               sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                            cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing`[i],0)
    day_rem_NA <-  ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted",
                            cf.m2$subjB$`phaseAssessmentDay:groupremitted:sumNA`[i],0)
    day_fant_NA <-  ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                          cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:sumNA`[i],0)
    rem_fant_NA <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                               sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                            cf.m2$subjB$`groupremitted:interventionfantasizing:sumNA`[i],0)
    day_rem_PA <-  ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted",
                          cf.m2$subjB$`phaseAssessmentDay:groupremitted:sumPA`[i],0)
    day_fant_PA <-  ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                           cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:sumPA`[i],0)
    day_NA_PA <- cf.m2$subjB$`phaseAssessmentDay:sumNA:sumPA`[i]
    rem_NA_PA <-  ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted",
                          cf.m2$subjB$`groupremitted:sumNA:sumPA`[i],0)
    fant_NA_PA <-  ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                           cf.m2$subjB$`interventionfantasizing:sumNA:sumPA`[i],0)
    day_rem_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                                    sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                                    cf.m2$subjB$`phaseAssessmentDay:groupremitted:phaseperi`[i],0)
    day_fant_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                               sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                            cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:phaseperi`[i],0)
    day_NA_phase <- ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri",
                             cf.m2$subjB$`phaseAssessmentDay:sumNA:phaseperi`[i],0)
    day_PA_phase <- ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri",
                           cf.m2$subjB$`phaseAssessmentDay:sumPA:phaseperi`[i],0)
    rem_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                                sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                             cf.m2$subjB$`groupremitted:sumNA:phaseperi`[i],0)
    fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                                sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                             cf.m2$subjB$`interventionfantasizing:sumNA:phaseperi`[i],0)
    rem_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                              sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                           cf.m2$subjB$`groupremitted:sumPA:phaseperi`[i],0)#
    fant_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                               sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                            cf.m2$subjB$`interventionfantasizing:sumPA:phaseperi`[i],0)
    NA_PA_phase <-  ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri",
                         cf.m2$subjB$`sumNA:sumPA:phaseperi`[i],0)
    day_rem_fant_NA <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                              sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                           cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumNA`[i],0)
    day_rem_fant_PA <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                              sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                           cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumPA`[i],0) 
    day_rem_NA_PA <-  ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted",
                         cf.m2$subjB$`phaseAssessmentDay:groupremitted:sumNA:sumPA`[i],0)
    day_fant_NA_PA <-  ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                         cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:sumNA:sumPA`[i],0)
    rem_fant_NA_PA <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                               cf.m2$subjB$`groupremitted:interventionfantasizing:sumNA:sumPA`[i],0)
    day_rem_fant_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                    sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") &
                                   (sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                                 cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:phaseperi`[i],0) 
    day_rem_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                    sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                                 cf.m2$subjB$`phaseAssessmentDay:groupremitted:sumNA:phaseperi`[i],0) 
    day_fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                               cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:sumNA:phaseperi`[i],0) 
    rem_fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                  sc_data[sc_data$subject_num==i,]$group[1]=="remitted") &
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                               cf.m2$subjB$`groupremitted:interventionfantasizing:sumNA:phaseperi`[i],0) 
    day_rem_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                  sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                               cf.m2$subjB$`phaseAssessmentDay:groupremitted:sumPA:phaseperi`[i],0) 
    day_fant_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                   sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                                cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:sumPA:phaseperi`[i],0) 
    rem_fant_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                   sc_data[sc_data$subject_num==i,]$group[1]=="remitted") &
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                                cf.m2$subjB$`groupremitted:interventionfantasizing:sumPA:phaseperi`[i],0) 
    day_NA_PA_phase <- ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri",
                           cf.m2$subjB$`phaseAssessmentDay:sumNA:sumPA:phaseperi`[i],0)
    rem_NA_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                  sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                               cf.m2$subjB$`groupremitted:sumNA:sumPA:phaseperi`[i],0)
    fant_NA_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                 sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                              cf.m2$subjB$`interventionfantasizing:sumNA:sumPA:phaseperi`[i],0)
    day_rem_fant_NA_PA <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                                 sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                              cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumNA:sumPA`[i],0)
    day_rem_fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                   sc_data[sc_data$subject_num==i,]$group[1]=="remitted") &
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                                cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumNA:phaseperi`[i],0) 
    day_rem_fant_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                       sc_data[sc_data$subject_num==i,]$group[1]=="remitted") &
                                      sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                                    cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumPA:phaseperi`[i],0) 
    day_rem_NA_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                 sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                              cf.m2$subjB$`phaseAssessmentDay:groupremitted:sumNA:sumPA:phaseperi`[i],0)
    day_fant_NA_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                               cf.m2$subjB$`phaseAssessmentDay:interventionfantasizing:sumNA:sumPA:phaseperi`[i],0)
    rem_fant_NA_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                                    sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                                     (sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                                 cf.m2$subjB$`groupremitted:interventionfantasizing:sumNA:sumPA:phaseperi`[i],0)
    day_rem_fant_NA_PA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                                      sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                                     (sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                                   cf.m2$subjB$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumNA:sumPA:phaseperi`[i],0)
    
    a <- intercept + gr + interv + peri + gr.int + gr.ph + int.ph + rem_fant_peri
    b <- day + sNA + sPA + rem_day[i] + fant_day[i] + NA_day + rem_NA[i] + fant_NA[i] + PA_day + rem_PA[i] +
      fant_PA[i] + PANA + phase_day[i] + phase_NA[i] + phase_PA[i] + day_rem_fant + day_rem_NA + day_fant_NA +
      rem_fant_NA + day_rem_PA + day_fant_PA + day_NA_PA + rem_NA_PA  + fant_NA_PA + day_rem_phase +
      day_fant_phase + day_NA_phase + day_PA_phase + rem_NA_phase + fant_NA_phase + rem_PA_phase +
      fant_PA_phase + NA_PA_phase + day_rem_fant_NA + day_rem_fant_PA + day_rem_NA_PA +
      day_fant_NA_PA + rem_fant_NA_PA + day_rem_fant_phase + day_rem_NA_phase + day_fant_NA_phase +
      rem_fant_NA_phase + day_rem_PA_phase + day_fant_PA_phase + rem_fant_PA_phase + day_NA_PA_phase +
      rem_NA_PA_phase + fant_NA_PA_phase + day_rem_fant_NA_PA + day_rem_fant_NA_phase + 
      day_rem_fant_PA_phase + day_rem_NA_PA_phase + day_fant_NA_PA_phase + rem_fant_NA_PA_phase +
      day_rem_fant_NA_PA_phase
    
    a_list[i] <- a
    b_list[i] <- b


    abline(a=a, b=b, col=alpha(cx, 0.6))
    
    
    #if(g == "G4b"){
    #  print(i)
    #  print(a)
    #}
    
  }
  abline(a=mean(unlist(a_list)), b=mean(unlist(b_list)), col=alpha(cx,1), lwd=4)
  cx = cx+1
  start <- end+1
}

legend(.5, 3.5, legend=rev(groups),
       col=c(4,3,2,1), title = 'Group', lty = 1, lwd = 2, cex=.5, bty = 'n')

###############################################################################################

sc_data$subjP <- interaction(sc_data$subject, sc_data$phase, sc_data$block, drop = TRUE)
sc_data <- sc_data[which(sc_data$subjP!="s112.peri.1"),]
m1 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * phase +
             (1 + phaseAssessmentDay | subjP), data = sc_data)

m2 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * phase +
             (0 + phaseAssessmentDay | subjP) + (1|subjP), data = sc_data)
anova(m2, m1, refit=FALSE)#not significant --> m2 better

m3 <- lmer(ruminating_lag1 ~  phaseAssessmentDay * group * intervention * sumNA * phase +
            (1|subjP), data = sc_data)
anova(m3,m2, refit=FALSE)#significant



#residual diagnostics
qqnorm(resid(m2))
qqline(resid(m2))
#residuals definitely not normally distributed --> more of a t-distribution

#Plot1
x <- resid(m2)
h <- hist(x, col = "grey", breaks = 40)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

# PLOT 2:
plot(fitted(m2), resid(m2))
abline(h=0)
plot(data$phaseAssessmentDay, resid(m2))
abline(h=0)
# PLOT 3:
acf(resid(m2))

resid_m <- resid(m2)
match(c(min(resid_m),max(resid_m)),resid_m) #indices 434 3781
fe_m <- fitted(m2)

resid_m[c(434, 3781)]
fe_m[c(434, 3781)]

########################################## Hopefully cool plot #########################################
summary(m2)


sc_data$grIntPh <- paste(sc_data$grInt, sc_data$phase, sep=".")

sc_data$subject_num <- NA
for(j in 1:nrow(sc_data)){
  sc_data$subject_num[j] <- as.numeric(sc_data$subjP[j])
}

#plotting random effects
cf.m2 <- coef(m2)
# groups <- list('controls.fantasizing.pre.1', 'controls.mindfulness.pre.1',
#                'controls.fantasizing.pre.2', 'controls.mindfulness.pre.2',
#                'controls.fantasizing.peri.1', 'controls.mindfulness.peri.1',
#                'controls.fantasizing.peri.2', 'controls.mindfulness.peri.2',
#                'remitted.fantasizing.pre.1', 'remitted.mindfulness.pre.1',
#                'remitted.fantasizing.pre.2', 'remitted.mindfulness.pre.2',
#                'remitted.fantasizing.peri.1', 'remitted.mindfulness.peri.1',
#                'remitted.fantasizing.peri.2', 'remitted.mindfulness.peri.2')
groups <- list('controls.fantasizing.pre', 'controls.mindfulness.pre',
               'controls.fantasizing.peri', 'controls.mindfulness.peri',
               'remitted.fantasizing.pre', 'remitted.mindfulness.pre',
               'remitted.fantasizing.peri', 'remitted.mindfulness.peri')
par(cex=1.1)
emptyPlot(range(data$phaseAssessmentDay), range(fitted(m2)),
          xlab="phaseAssessmentDay", ylab="Rumination score (lag 1)", v0=0,
          xmark=TRUE, ymark=TRUE, las=1,
          main="Subject lines")

#test <- c(23,28,25,17)
start <- 1
end <- 0
cx <- 1

for (g in groups){
  print(g)
  len_gr <- length(unique(sc_data[sc_data$grIntPh == g,]$subject_num))
  end <- end + len_gr
  a_list = list()
  b_list = list()
  for(i in start:end ){
    print(i)
    #intercept adjustments
    intercept <- cf.m2$subjP$`(Intercept)`[i]
    gr <- ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted", cf.m2$subjP$groupremitted[i],0)
    interv <- ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing", cf.m2$subjP$interventionfantasizing[i],0)
    peri <- ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri", cf.m2$subjP$phaseperi[1], 0)
    gr.int <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                        sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                     cf.m2$subjP$`groupremitted:interventionfantasizing`[i],0)
    gr.ph <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                       sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                    cf.m2$subjP$`groupremitted:phaseperi`[i],0)
    int.ph <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                        sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                     cf.m2$subjP$`interventionfantasizing:phaseperi`[i],0)
    rem_fant_peri <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                               sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") &
                              (sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                            cf.m2$subjP$`groupremitted:interventionfantasizing:phaseperi`[i],0)
    
    
    #slope adjustments
    day <- cf.m2$subjP$phaseAssessmentDay[i]
    sNA <- cf.m2$subjP$sumNA[i]
    rem_day <- ifelse(sc_data[sc_data$subject_num==i,]$group=="remitted",
                      cf.m2$subjP$`phaseAssessmentDay:groupremitted`[i],0)
    fant_day <- ifelse(sc_data[sc_data$subject_num==i,]$intervention=="fantasizing",
                       cf.m2$subjP$`phaseAssessmentDay:interventionfantasizing`[i],0)
    NA_day <- cf.m2$subjP$`phaseAssessmentDay:sumNA`[1]
    rem_NA <- ifelse(sc_data[sc_data$subject_num==i,]$group=="remitted",
                     cf.m2$subjP$`groupremitted:sumNA`[i],0)
    fant_NA <- ifelse(sc_data[sc_data$subject_num==i,]$intervention=="fantasizing",
                      cf.m2$subjP$`interventionfantasizing:sumNA`[i],0)
    phase_day <- ifelse(sc_data[sc_data$subject_num==i,]$phase=="peri",
                        cf.m2$subjP$`phaseAssessmentDay:phaseperi`[i],0)
    phase_NA <- ifelse(sc_data[sc_data$subject_num==i,]$phase=="peri",
                       cf.m2$subjP$`sumNA:phaseperi`[i],0)
    day_rem_fant <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                               sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                            cf.m2$subjP$`phaseAssessmentDay:groupremitted:interventionfantasizing`[i],0)
    day_rem_NA <-  ifelse(sc_data[sc_data$subject_num==i,]$group[1]=="remitted",
                          cf.m2$subjP$`phaseAssessmentDay:groupremitted:sumNA`[i],0)
    day_fant_NA <-  ifelse(sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                           cf.m2$subjP$`phaseAssessmentDay:interventionfantasizing:sumNA`[i],0)
    rem_fant_NA <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                              sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                           cf.m2$subjP$`groupremitted:interventionfantasizing:sumNA`[i],0)
    day_rem_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                               sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                            cf.m2$subjP$`phaseAssessmentDay:groupremitted:phaseperi`[i],0)
    day_fant_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                                sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                             cf.m2$subjP$`phaseAssessmentDay:interventionfantasizing:phaseperi`[i],0)
    day_NA_phase <- ifelse(sc_data[sc_data$subject_num==i,]$phase[1]=="peri",
                           cf.m2$subjP$`phaseAssessmentDay:sumNA:phaseperi`[i],0)
    rem_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                              sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                           cf.m2$subjP$`groupremitted:sumNA:phaseperi`[i],0)
    fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") & 
                               sc_data[sc_data$subject_num==i,]$phase[1]=="peri"),
                            cf.m2$subjP$`interventionfantasizing:sumNA:phaseperi`[i],0)
    day_rem_fant_NA <-  ifelse(((sc_data[sc_data$subject_num==i,]$group[1]=="remitted") & 
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                               cf.m2$subjP$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumNA`[i],0)
    day_rem_fant_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                    sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing") &
                                   (sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                                 cf.m2$subjP$`phaseAssessmentDay:groupremitted:interventionfantasizing:phaseperi`[i],0) 
    day_rem_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                  sc_data[sc_data$subject_num==i,]$group[1]=="remitted"),
                               cf.m2$subjP$`phaseAssessmentDay:groupremitted:sumNA:phaseperi`[i],0) 
    day_fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                   sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing"),
                                cf.m2$subjP$`phaseAssessmentDay:interventionfantasizing:sumNA:phaseperi`[i],0) 
    rem_fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                   sc_data[sc_data$subject_num==i,]$group[1]=="remitted") &
                                  sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                                cf.m2$subjP$`groupremitted:interventionfantasizing:sumNA:phaseperi`[i],0) 
    day_rem_fant_NA_phase <- ifelse(((sc_data[sc_data$subject_num==i,]$phase[1]=="peri") & 
                                       sc_data[sc_data$subject_num==i,]$group[1]=="remitted") &
                                      sc_data[sc_data$subject_num==i,]$intervention[1]=="fantasizing",
                                    cf.m2$subjP$`phaseAssessmentDay:groupremitted:interventionfantasizing:sumNA:phaseperi`[i],0) 

    
    a <- intercept + gr + interv + peri + gr.int + gr.ph + int.ph + rem_fant_peri
    b <- day + sNA + rem_day[1] + fant_day[1] + NA_day + rem_NA[1] + fant_NA[1] +
      phase_day[1] + phase_NA[1] + day_rem_fant + day_rem_NA + day_fant_NA +
      rem_fant_NA + day_rem_phase + day_fant_phase + day_NA_phase + rem_NA_phase + fant_NA_phase +
      day_rem_fant_NA + day_rem_fant_phase + day_rem_NA_phase + day_fant_NA_phase +
      rem_fant_NA_phase
    
    a_list[i] <- a
    b_list[i] <- b
    
    
    abline(a=a, b=b, col=alpha(cx, 0.05))
    
    
    #if(g == "G4b"){
    #  print(i)
    #  print(a)
    #}
    
  }
  abline(a=mean(unlist(a_list)), b=mean(unlist(b_list)), col=alpha(cx,1), lwd=4)
  cx = cx+1
  start <- end+1
}

legend(5, 0.5, legend=rev(groups),
       col=c(8,7,6,5,4,3,2,1), title = 'Group', lty = 1, lwd = 2, cex=.5, bty = 'n')






for(item in unique(sc_data$subjP)){
  print(item)
  print(item %in% unique(rownames(cf.m2$subjP[1])))
}



