### Load the packages we will need
library(here)
library(nlme)

### Load the time course data
time <- read.csv(here('time.csv'))
head(time, 10)

### Figure 4.2: Panel plots for Control Group
par(mfcol=c(5,5))
for (i in 1:25){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", type="l", lwd=2, pch=4, ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
}
mtext("Control Group", side=3, outer=TRUE, line=-1.2)

### Figure 4.2: Panel plots for Treatment Group
par(mfcol=c(5,5))
for (i in 26:50){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", type="o", pch=4, ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
}
mtext("Treatment Group", side=3, outer=TRUE, line=-1.2)

### Run linear growth model with AR(1) errors
lgmodel <- lme(fixed=intimacy ~ time01*treatment, data=time, random=~time01 | id, 
               correlation = corAR1())
summary(lgmodel)

### Figure 4.4: Panel plots for Control Group with Actual and Model-Predicted Time Course
par(mfcol=c(5,5))
for (i in 1:25){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", type="o", pch=4, ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}
mtext("Control Group", side=3, outer=TRUE, line=-1.2)

### Figure 4.4: Panel plots for Treatment Group with Actual and Model-Predicted Time Course
par(mfcol=c(5,5))
for (i in 26:50){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", type="o", pch=4, ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}
mtext("Treatment Group", side=3, outer=TRUE, line=-1.2)

### Figure 4.5: Spaghetti Plots for Control Group and Treatment Group
par(mfcol=c(1,2))
plot(time$time[time$treatment==0], time$intimacy[time$treatment==0], 
     ylab="intimacy", xlab="time", type="n", pch=4, ylim=c(0,8), main="Control")
for (i in 1:25){
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}
predc<-2.8989745 + ((0.7352012)/15)*time$time
lines(time$time, predc, lwd=4)

plot(time$time[time$treatment==1], time$intimacy[time$treatment==1], 
     ylab="intimacy", xlab="time", type="n", pch=4, ylim=c(0,8), main="Treatment")
for (i in 26:50){
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}
predt<-(2.8989745 - 0.0564426) + ((0.7352012 + 0.9214365)/15)*time$time
lines(time$time, predt, lwd=4)

### Set up dataset with id, relqual, and the EB estimates of slopes and intercepts. 
### Print out the first 10 cases.
coefs<-data.frame(lgmodel$coefficients$random$id)
names(coefs)[c(2,1)] <- c("ebslope","ebintercept")
coefs$id <- with(coefs, 1:50)
coefs$treatment<-aggregate(treatment ~ id, data=time, mean)[, 2]
coefs$slope <- with(coefs, ebslope + 0.7352012 + 0.9214365*treatment)
ordcoefs<-coefs[order(coefs$treatment, coefs$slope),]

head(coefs, 10)

### Find percentiles for slope distribution for Control Group
quantile(coefs$slope[coefs$treatment==0], c(0.0, .05, .25, .50, .75, .95, 1.0))

### Figure 2 of example write-up for Chapter 4: Panel plots for five selected IDs in Control Group
par(mfcol=c(1,5))
for (i in c(22, 8, 12, 15, 4)){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", type="o", pch=4, ylim=c(0,8), main=paste("id =", i, sep = " "))
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}
mtext("Control Group", side=3, outer=TRUE, line=-1.2)

### Find percentiles for slope distribution for Treatment Group
quantile(coefs$slope[coefs$treatment==1], c(0.0, .05, .25, .50, .75, .95, 1.0))

### Figure 2 of example write-up for Chapter 4: Panel plots for five selected IDs in Treatment Group
par(mfcol=c(1,5))
for (i in c(46, 50, 26, 49, 37)){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", type="o", pch=4, ylim=c(0,8), main=paste("id =", i, sep = " "))
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}
mtext("Treatment Group", side=3, outer=TRUE, line=-1.2)

