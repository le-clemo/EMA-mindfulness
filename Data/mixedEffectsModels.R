rm(list = ls()) #clean all up

#setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

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

############################################ Data visualization #################################################
aggData <- with(data, aggregate(list(ruminating = ruminating, stickiness = stickiness,
                                    sumNA = sumNA, sumPA = sumPA, sleepQuality = sleepQuality),
                               by = list(subject = subject, group = group,
                                         blockAssessmentDay = blockAssessmentDay), FUN = mean, na.rm = TRUE))

means <- with(aggData, aggregate(list(meanRum = ruminating, meanStick = stickiness, 
                                      meanNA = sumNA, meanPA = sumPA, meanSleep = sleepQuality),
                                 by = list(group = group, blockAssessmentDay = blockAssessmentDay), 
                                 FUN = mean, na.rm = TRUE))

aggBlock <- with(data, aggregate(list(ruminating = ruminating, stickiness = stickiness,
                                     sumNA = sumNA, sumPA = sumPA, sleepQuality = sleepQuality),
                                by = list(subject = subject, group = group, intervention = intervention,
                                          blockAssessmentDay = blockAssessmentDay), FUN = mean, na.rm = TRUE))

ggplot(aggData, aes(blockAssessmentDay, ruminating, group = subject, color=group)) + geom_point()+
  geom_line(means, mapping = aes(blockAssessmentDay, meanRum, color = group))

ggplot(aggData, aes(blockAssessmentDay, sumNA, group = subject, color=group)) + geom_point()

ggplot(aggData, aes(blockAssessmentDay, sumPA, group = subject, color=group)) + geom_point()


############################################ RE structure #################################################
m1 <- lmer(ruminating ~  grInt * blockPhase * sumNA * sumPA +
             (1 + assessmentDay | subject) + (1 | blockPhase) + (1 | grInt), data = data)
#converging warning --> consider rescaling
pvars <- c( "ruminating", "sumNA",# Scaling numeric parameters
            "sumPA")

sc_data <- copy(data)
sc_data[pvars] <- lapply(data[pvars],scale)

m1 <- lmer(ruminating ~  grInt * blockPhase * sumNA * sumPA +
             (0 + assessmentDay | subject) + (1|grInt), data = sc_data)

summary(m1)

m2 <- lmer(ruminating ~  grInt * phase * block * sumNA * sumPA +
             (1|subject) + (0 + assessmentDay | subject), data = data)

anova(m1, m2, refit = FALSE)# very significant --> keep maximum RE-structure


re2b <- ranef(m1)
re.s <- re2b[[1]]
head(re.s)
class(re.s)#plot(re2b[[1]][,1], re2b[[1]][,2]) #they still appear correlated

par(cex=1.1)
plot(re.s[,1], re.s[,2],
     xlab="intercept", ylab="slope",
     bty='n', pch=16)
abline(h=0, v=0, lty=3)

#there is definitely correlation between the random effects, which is not ideal.

############################################ FE structure #################################################
#m1 already includes the most complex fixed-effects structure
#backwards fitting for model selection.

m3 <- lmer(ruminating ~  grInt * phase * block + sumNA:sumPA +
             (1 + assessmentDay | subject), data = data)

#important! in anova(): refit = FALSE only for RE structure!
anova(m1, m3) #highly significant!

summary(m1)

#some residual diagnostics
qqnorm(resid(m1))
qqline(resid(m1))

x <- resid(m1)
h <- hist(x, col = "grey", breaks = 40)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

# PLOT 2:
plot(fitted(m1), resid(m1))
abline(h=0)
plot(data$assessmentDay, resid(m1))
abline(h=0)
# PLOT 3:
acf(resid(m1))

resid_m1 <- resid(m1)
match(c(min(resid_m1),max(resid_m1)),resid_m1) #indices 4179, 5411
fe_m1 <- fitted(m1)

resid_m1[c(4179, 5411)]
fe_m1[c(4179, 5411)]

data_copy <- data
data_copy$fitted <- fe_m1
data_copy$resids <- resid_m1


#some fancy plot attempt
cf.m1 <- coef(m1)

par(cex=1.1)
emptyPlot(range(data$assessmentDay), range(fitted(m1)),
          xlab="assessmentDay", ylab="Score", v0=0,
          xmark=TRUE, ymark=TRUE, las=1,
          main="subject lines")

grInts <- list('controls.fantasizing', 'controls.mindfulness', 'remitted.fantasizing', 'remitted.mindfulness')
test <- c(23,28,25,17)
start <- 1
end <- 0
cx <- 1
for (g in grInts){
  len_gr <- length(unique(dat[data$grInt == g,]$subject))
  end <- end + len_gr
  
  for(i in start:end ){
    #intercept adjustments
    intercept <- cf.m1$subject$`(Intercept)`[i]
    ag78 <- ifelse(data[dataa$subject == i,]$group[1] == 'controls', cf.m1$subject$`AgeGroup7-8`[i], 0)
    old <- ifelse(dat[dat$subject == i,]$Method[1] == 'Old', cf.m1$subject$MethodOld[i], 0)
    ag78_old <- ifelse(dat[dat$subject == i,]$Group[1] == 'A2-Old', cf.m1$subject$`AgeGroup7-8:MethodOld`[i],0)
    
    #slope adjustments
    month <- cf.m1$subject$cMonth[i]
    ag78_month <- ifelse(dat[dat$subject == i,]$AgeGroup[1] == '7-8', cf.m1$subject$`AgeGroup7-8:cMonth`[i],0)
    old_month <- ifelse(dat[dat$subject == i,]$Method[1] == 'Old', cf.m1$subject$`MethodOld:cMonth`[i],0)
    ag78_old_month <- ifelse(dat[dat$subject == i,]$Group[1] == 'A2-Old',
                             cf.m1$subject$`AgeGroup7-8:MethodOld:cMonth`[i],0)
    
    a <- intercept + ag78 + old + ag78_old
    b <- month + ag78_month + old_month + ag78_old_month
    
    abline(a=a, b=b, col=alpha(cx, 0.8))
    #if(g == "G4b"){
    #  print(i)
    #  print(a)
    #}
    
  }
  cx = cx+1
  start <- end+1
}

legend(-1.5, 100, legend=rev(groups),
       col=c(4,3,2,1), title = 'Group', lty = 1, lwd = 2, cex=.8, bty = 'n')



###################################### Combining blocks #######################################
#--> treating the same subject in different blocks as separate entities

#RE structure
sc_data$subjB <- interaction(sc_data$subject, sc_data$block, drop = TRUE)

m1 <- lmer(ruminating ~  blockAssessmentDay * grInt * sumNA * sumPA * phase +
             (1 + blockAssessmentDay | subjB) + (1 | grInt) + (1|phase), data = sc_data)
#failed to converge


m1b <- lmer(ruminating ~  blockAssessmentDay * grInt * sumNA * sumPA * phase + 
             (0 + blockAssessmentDay | subjB) + (1|grInt) + (1|phase) + (1|subjB), data = sc_data)
#failed to converge

m1c <- lmer(ruminating ~ blockAssessmentDay * grInt * sumNA * sumPA * phase + 
             (0 + blockAssessmentDay | subjB) + (1|subjB) + (1|grInt) , data = sc_data)
#failed to converge

m1d <- lmer(ruminating ~ blockAssessmentDay * grInt * sumNA * sumPA * phase + 
             (0 + blockAssessmentDay | subjB) + (1|subjB) + (1|phase) , data = sc_data)
#failed to converge

m2 <- lmer(ruminating ~ blockAssessmentDay * grInt * sumNA * sumPA * phase + 
             (0 + blockAssessmentDay | subjB) + (1|subjB), data = sc_data)
#successfully converged

m2b <- lmer(ruminating ~ blockAssessmentDay * grInt * sumNA * sumPA * phase + 
              (1|subjB), data = sc_data)

anova(m2, m2b, refit=FALSE) #significant --> m2 better


m2c <- lmer(ruminating ~  grInt * sumNA * sumPA * phase + 
             (0 + blockAssessmentDay | subjB) , data = sc_data)

anova(m2, m2c, refit = FALSE) # significant + rand intercept for subject should arguably be kept anyways
#m2 is the winner

re2 <- ranef(m2)
re.s <- re2[[1]]
#head(re.s)
#class(re.s)
#plot(re2b[[1]][,1], re2b[[1]][,2]) #no obvious correlation

par(cex=1.1)
plot(re.s[,1], re.s[,2],
     xlab="intercept", ylab="slope",
     bty='n', pch=16)
abline(h=0, v=0, lty=3)

#there is no strong apparent correlation. --> good

#FE structure
m3 <- lmer(ruminating ~  blockAssessmentDay * grInt * phase + sumNA * sumPA + 
                    (0 + blockAssessmentDay | subjB) + (1|subjB) , data = sc_data)
#fails to converge (surprisingly?)

m3b <- lmer(ruminating ~  grInt + blockAssessmentDay * phase * sumNA * sumPA + 
              (0 + blockAssessmentDay | subjB) + (1|subjB) , data = sc_data)

anova(m2, m3b) #significant

m3c <- lmer(ruminating ~  blockAssessmentDay * grInt * phase * sumNA + sumPA + 
              (0 + blockAssessmentDay | subjB) + (1|subjB) , data = sc_data)

anova(m2, m3c) #significant

m3d <- lmer(ruminating ~  phase + blockAssessmentDay * grInt * sumNA * sumPA + 
              (0 + blockAssessmentDay | subjB) + (1|subjB) , data = sc_data)
#model fails to converge

m3e <- lmer(ruminating ~  blockAssessmentDay + grInt * phase * sumNA * sumPA + 
              (0 + blockAssessmentDay | subjB) + (1|subjB) , data = sc_data)

anova(m2, m3e)#significant

#m2 appears best

#residual diagnostics
qqnorm(resid(m2))
qqline(resid(m2))
#residuals definitely not normally distributed

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
plot(data$blockAssessmentDay, resid(m2))
abline(h=0)
# PLOT 3:
acf(resid(m2))

resid_m2 <- resid(m2)
match(c(min(resid_m2),max(resid_m2)),resid_m2) #indices 4742, 5411
fe_m2 <- fitted(m2)

resid_m2[c(4742, 5411)]
fe_m2[c(4742, 5411)]

summary(m2)

#plotting random effects
cf.m2 <- coef(m2)
par(cex=1.1)
emptyPlot(range(dat$cMonth), range(fitted(lin2b)),
          xlab="cMonth", ylab="Score", v0=0,
          xmark=TRUE, ymark=TRUE, las=1,
          main="Subject lines")

groups <- list('A1-New', 'A1-Old', 'A2-New', 'A2-Old')
test <- c(23,28,25,17)
start <- 1
end <- 0
cx <- 1
for (g in groups){
  len_gr <- length(unique(dat[dat$Group == g,]$Subject))
  end <- end + len_gr
  
  for(i in start:end ){
    #intercept adjustments
    intercept <- cf.m2$Subject$`(Intercept)`[i]
    ag78 <- ifelse(dat[dat$Subject == i,]$AgeGroup[1] == '7-8', cf.m2$Subject$`AgeGroup7-8`[i], 0)
    old <- ifelse(dat[dat$Subject == i,]$Method[1] == 'Old', cf.m2$Subject$MethodOld[i], 0)
    ag78_old <- ifelse(dat[dat$Subject == i,]$Group[1] == 'A2-Old', cf.m2$Subject$`AgeGroup7-8:MethodOld`[i],0)
    
    #slope adjustments
    month <- cf.m2$Subject$cMonth[i]
    ag78_month <- ifelse(dat[dat$Subject == i,]$AgeGroup[1] == '7-8', cf.m2$Subject$`AgeGroup7-8:cMonth`[i],0)
    old_month <- ifelse(dat[dat$Subject == i,]$Method[1] == 'Old', cf.m2$Subject$`MethodOld:cMonth`[i],0)
    ag78_old_month <- ifelse(dat[dat$Subject == i,]$Group[1] == 'A2-Old',
                             cf.m2$Subject$`AgeGroup7-8:MethodOld:cMonth`[i],0)
    
    a <- intercept + ag78 + old + ag78_old
    b <- month + ag78_month + old_month + ag78_old_month
    
    abline(a=a, b=b, col=alpha(cx, 0.8))
    #if(g == "G4b"){
    #  print(i)
    #  print(a)
    #}
    
  }
  cx = cx+1
  start <- end+1
}

legend(-1.5, 100, legend=rev(groups),
       col=c(4,3,2,1), title = 'Group', lty = 1, lwd = 2, cex=.8, bty = 'n')









