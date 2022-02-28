
### Load the packages we will need
library(here)
library(nlme)

### Load the process dataset
process <- read.csv(here('process.csv'))
head(process, 10)

### Figure 5.2: Time Course Plots for Intimacy for the Low Relationship Quality Group
par(mfrow=c(4,8))
for (i in process$id[process$time==0 & process$relqual==0])#low-quality relationships
{
  plot(process$time[process$id==i], process$intimacy[process$id==i], 
       ylab="Intimacy", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Figure 5.2: Time Course Plots for Intimacy for the High Relationship Quality Group
par(mfrow=c(5,8))
for (i in process$id[process$time==0 & process$relqual==1])#high-quality relationships
{
  plot(process$time[process$id==i], process$intimacy[process$id==i], 
       ylab="Intimacy", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Figure 5.2: Time Course Plots for Conflict for the Low Relationship Quality Group
par(mfrow=c(4,8))
for (i in process$id[process$time==0 & process$relqual==0])
{
  plot(process$time[process$id==i], process$conflict[process$id==i], 
       ylab="Conflict", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(-0.1, 1.1), 
       main=paste("id =", i, sep = " "))
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Figure 5.2: Time Course Plots for Conflict for the High Relationship Quality Group
par(mfrow=c(5,8))
for (i in process$id[process$time==0 & process$relqual==1])
{
  plot(process$time[process$id==i], process$conflict[process$id==i], 
       ylab="Conflict", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(-0.1, 1.1), 
       main=paste("id =", i, sep = " "))
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Run causal process model with AR(1) errors
cpmodel <- lme(fixed=intimacy ~ time7c + confcw*relqual + confcb*relqual, data=process, 
               random=~confcw | id, correlation = corAR1())
summary(cpmodel)

### Put the Estimated Best Linear Unbiased Predictions (EBLUPS) of the random effects into a separate dataet
cfs<-ranef(cpmodel) #pull person-specific random effects from the model and create a dataset out of them
cfs$id<-1:66 #Add in id numbers

#Fix the names of the EBLUPs
names(cfs) <- make.names(names(cfs))
names(cfs)[c(2,1)] <- c("ebslope","ebintercept") #change names of variables

#Add relqual to the data frame with EBLUPs
cfs$relqual<-aggregate(relqual ~ id, data=process, mean)[, 2] #adding in whether each person was in the high or low RQ group

#Add the fixed effects to the EBLUPs
cfs$intercept<- (4.53219977 + 0.64726123*cfs$relqual + cfs$ebintercept) #person-specific intercept
cfs$fixinter<- (4.53219977 + 0.64726123*cfs$relqual) #not used below
cfs$slope<- (-2.01061871 + 1.01641068*cfs$relqual + cfs$ebslope) #person-specific conflict slope
cfs$fixslope<- (-2.01061871 + 1.01641068*cfs$relqual) #not used below

#Take a look
head(cfs, 10)

### Merge upper-level variables with the process data frame
process<-merge(process, cfs, all=TRUE, by="id")
process$relqual<-process$relqual.x

### Create model-based predicted values of intimacy as a function of conflict with time7c = 0
process$pred<-(process$intercept + process$slope*process$conflict)

### To create graphs of predictions for observed data only
###  1. Sort the dataset by relationship quality, ID, and daily conflict. 
###  2. Sort the upper-level data frame by relationship quality, conflict slope and ID
ordprocess<-process[order(process$relqual, process$slope, process$id, process$conflict),] #1
ordcfs<-cfs[order(cfs$relqual, cfs$slope, cfs$id),] #2

### Figure 5.4: Intimacy as a Function of Conflict: Raw Data and Model Predictions for the Low Relationship Quality Group
par(mfrow=c(4,8))
for (i in c(48, 45, 58,  4, 43, 26,  8, 27, 52, 65,  7, 28, 56, 
            47, 17, 35, 62,  9, 22, 46, 18, 34, 30, 60, 41, 21))
{   #Subjects in low rq group
  plot(process$conflict[process$id==i], process$intimacy[process$id==i], 
       ylab="intimacy", xlab="conflict (0,1)", type="p", pch=1, xlim=c(0, 1), ylim=c(0,10), 
       main=paste(round(cfs$slope[cfs$id==i], digits=3)))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i]) 
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Figure 5.4: Intimacy as a Function of Conflict: Raw Data and Model Predictions for the High Relationship Quality Group
par(mfrow=c(5,8))
for (i in c(38, 11, 59, 42, 63, 19, 54,  6, 49, 61,  5, 66, 16, 10, 36, 14, 39, 44, 51, 
            55, 64, 25, 57, 33, 20, 50, 37,  1, 53, 24,  2, 15, 32, 40, 23, 12, 13, 29,  3, 31))
{ #Subjects in high rq group
  plot(process$conflict[process$id==i], process$intimacy[process$id==i], 
       ylab="intimacy", xlab="conflict (0,1)", type="p", pch=1, xlim=c(0, 1), ylim=c(0,10),
       main=paste(round(cfs$slope[cfs$id==i], digits=3)))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Figure 5.5: Spaghetti Plots for Low and High Relationship Quality Groups
par(mfcol=c(1,2)) #set up a grid with one row and two columns (one column representing low-RQ and the other high-RQ)
par(lwd=.5)
#low relqual
plot(process$conflict[process$relqual==0], process$intimacy[process$relqual==0], #setting up the plot for low-RQ
     ylab="Intimacy", xlab="Conflict", type="n", ylim=c(0,8), main="Low Relationship Quality")
for (i in cfs$id[cfs$relqual==0]){ #setting up plot on the left for low-RQ individuals
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i]) #plotting the individual specific 
  #daily conflict/intimacy slopes for low-RQ individuals
}
#fixed line for low group
predl<-4.532200 -2.010619*process$conflict[process$relqual==0] 
lines(process$conflict[process$relqual==0], predl, col="Red", lwd=4)

#high relqual
plot(process$conflict[process$relqual==1], process$intimacy[process$relqual==1],  #setting up the plot for high-RQ
     ylab="intimacy", xlab="time", type="n", pch=4, ylim=c(0,8), main="High Relationship Quality")
for (i in cfs$id[cfs$relqual==1]){  #setting up plot on the right for high-RQ individuals
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i]) #plotting the individual specific 
  #daily conflict/intimacy slopes for high-RQ individuals
}
#fixed line for high group
predh<-(4.532200 + 0.64726123) + (-2.010619 + 1.01641068)*process$conflict[process$relqual==1]  
lines(process$conflict[process$relqual==1], predh, col="Red", lwd=4)

### Find percentiles for slope distribution for Low Relationship Quality Group
quantile(cfs$slope[cfs$relqual==0], c(0.0, .05, .25, .50, .75, .95, 1.0))

### Figure 2 of example write-up for Chapter 5: Panel plots for five selected IDs in Low RQ Group
par(mfcol=c(1,5))
for (i in c(48, 8, 65, 46, 41))
{
  plot(ordprocess$conflict[ordprocess$id==i], ordprocess$intimacy[ordprocess$id==i], 
       ylab="Intimacy", xlab="Conflict", type="p", pch=1, ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)

### Find percentiles for slope distribution for High Relationship Quality Group
quantile(cfs$slope[cfs$relqual==1], c(0.0, .05, .25, .50, .75, .95, 1.0))

### Figure 2 of example write-up for Chapter 5: Panel plots for five selected IDs in High Relationship Quality Group
par(mfcol=c(1,5))
for (i in c(11, 5, 14, 50, 29))
{
  plot(ordprocess$conflict[ordprocess$id==i], ordprocess$intimacy[ordprocess$id==i], 
       ylab="Intimacy", xlab="Conflict", type="p", pch=1, ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)

