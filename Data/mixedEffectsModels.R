rm(list = ls()) #clean all up

setwd("C:/Users/cleme/Documents/Education/RUG/Thesis/EMA-mindfulness/Data")

#setwd("~/Documents/RUG/Thesis/EMA-mindfulness/Data")

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


############################################ RE structure #################################################
m1 <- lmer(ruminating ~  grInt * phase * block * sumNA * sumPA +
             (1 + assessmentDay | subject), data = data)

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

anova(m1, m3, refit = FALSE) #highly significant!

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









