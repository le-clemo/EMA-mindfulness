
### Load the packages we will need
library(here)
library(nlme)
library(lme4)
library(brms)
set.seed(666)

### Load the categorical dataset
categorical <- read.csv('categorical.csv')
head(categorical, 10)

### Create a upper-level data frame with id and amangcb
catlev2<-aggregate(amangcb ~ id, data=categorical, mean)

### Figure 6.2: Female Partner Anger and Male Partner Conflict Report: Raw Data Only
par(mfrow=c(2,4))
for (i in catlev2$id[catlev2$id <= 12]){
  plot(categorical$amang[categorical$id==i], categorical$pconf[categorical$id==i], 
       ylab="Male Conflict", xlab="Female Anger", type="p", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1), 
       main=paste("id =", i, sep = " "))
}
mtext("Male Partner Conflict by Female Partner Morning Anger", side=3, outer=TRUE, line=-1.2)

### Run random intercept model in lme4's glmer
catmodel <- glmer(pconf ~ amangcw + amangcb + lpconfc + time7c +
                    (1 | id), family = binomial, data = categorical)
summary(catmodel)
confint(catmodel, "amangcw")

### Specify random intercept and slope for amangcw 
catmod.r.slope <- glmer(pconf ~ amangcw + amangcb + lpconfc + time7c +
                          (amangcw | id), family = binomial, data = categorical)
summary(catmod.r.slope)

### Put the EBLUPs of the random intercept into a separate dataet
rint<-ranef(catmodel)
rintn<-data.frame(rint$id)
rintn$id<-as.numeric(rownames(rintn))

#Merge with upper level dataset
catlev2a<-merge(catlev2, rintn, all=TRUE, by="id")
names(catlev2a)
names(catlev2a)[c(3)] <- c("ebint")
catlev2a$intercept<-(-1.90219 + catlev2a$ebint)
catlev2a$slope<-0.21574
head(catlev2a, 10)

### Merge upper-level variables with the process data frame and create predicted values based on the 
### within-subject causal model (setting time7c at its mean value of zero)
categorical<-merge(categorical, catlev2a, all=TRUE, by="id")
categorical$pred<-(1/(1 + exp(-categorical$intercept - categorical$slope*categorical$amangcw)))
categorical$predM<-(1/(1 + exp(-(-1.90219 + 0.21574*categorical$amangc))))
head(categorical, 10)

### To create graphs based on observed data only: Sort the dataset by relationship quality, ID, and daily conflict
ordcat<-categorical[order(categorical$id, categorical$amang),]

### Figure 6.4: Female Partner Anger and Male Partner Conflict Report: Raw Data and Model Predictions
par(mfrow=c(3,4))
for (i in catlev2$id){  
  plot(ordcat$amang[ordcat$id==i], ordcat$pconf[ordcat$id==i], 
       ylab="Male Conflict", xlab="Female Anger", type="p", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1), 
       main=paste("id =", i, sep = " "))
  lines(ordcat$amang[ordcat$id==i], ordcat$pred[ordcat$id==i]) 
}
mtext("Male Conflict by Female Anger", side=3, outer=TRUE, line=-1.2)


### To create graphs based on observed data only: Sort the dataset by female morning anger
ordcat1<-categorical[order(categorical$amangc),]

### Figure 6.5: Female Partner Anger and Male Partner Conflict Report: Raw Data and Predictions for Typical Couple
par(mfrow=c(1,1))
plot(jitter(ordcat1$amang, 2), jitter(ordcat1$pconf, .5), 
     ylab="Male Conflict", xlab="Female Anger", type="p", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1))
lines(ordcat1$amang, ordcat1$predM, col="Red", lwd=4) 
mtext("Male Conflict by Female Anger", side=3, outer=TRUE, line=-1.2)


### Spaghetti Plot
par(lwd=.75)
plot(jitter(ordcat1$amang, 2), jitter(ordcat1$pconf, .5), 
     ylab="Male Conflict", xlab="Female Anger", type="n", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1))
for (i in catlev2$id){
  lines(ordcat$amang[ordcat$id==i], ordcat$pred[ordcat$id==i])
}
lines(ordcat1$amang, ordcat1$predM, col="Red", lwd=4) 


### Run a Bayesian version of the random intercept model
categorical$amangcb<-categorical$amangcb.x #Restore original varname 

prior<-set_prior("normal(0, 10)", class = "b") #Mildly informative prior
catmodB.r.int<-brm(pconf ~ amangcw + amangcb + time7c + lpconfc + 
                     (1| id), family = bernoulli(), data = categorical,
                   chains=4, cores=4,
                   prior=prior, save_all_pars = TRUE)
summary(catmodB.r.int)

### Get posterior samples; summarize the amangcw posterior
cmB.r.int.samp<-posterior_samples(catmodB.r.int)
hist(cmB.r.int.samp$b_amangcw)
mean(cmB.r.int.samp$b_amangcw) #0.2153631
sd(cmB.r.int.samp$b_amangcw) #0.06783943

### Get filled density plot of fixed effect of amangcw
d <- density(cmB.r.int.samp$b_amangcw)
plot(d, main="amangcw fixed effect: kernel density")
polygon(d, col="red", border="blue")
abline(v=c(0, 0.2153631), lwd=4)

### Get Bayesian version of random slopes model
prior<-set_prior("normal(0, 10)", class = "b") #Mildly informative prior
catmodB.r.slope<-brm(pconf ~ amangcw + amangcb + lpconfc + time7c +
                       (1 + amangcw| id), family = bernoulli(), data = categorical,
                     chains=4, cores=4,
                     prior=prior, save_all_pars = TRUE)
summary(catmodB.r.slope)

### #Extract posterior samples for slope of amangcw and summarize
cmB.r.slope.samp<-posterior_samples(catmodB.r.slope)

mean(cmB.r.slope.samp$b_amangcw) #0.1174383
median(cmB.r.slope.samp$b_amangcw) #0.1278437
sd(cmB.r.slope.samp$b_amangcw) #0.1245716
mean(cmB.r.slope.samp$sd_id__amangcw) #0.3700805
density(cmB.r.slope.samp$sd_id__amangcw)$x[which.max(density(cmB.r.slope.samp$sd_id__amangcw)$y)] #mode of SD: 0.3470892


### Filled density plot of fixed effect of amangcw for Bayesian random slopes model
d1 <- density(cmB.r.slope.samp$b_amangcw)
d1$x[which.max(d1$y)] #find mode of amangcw slope: 0.1586235
plot(d1, main="amangcw slope", xlim=c(-0.4, 0.45), ylim=c(0,6))
lines(d)
polygon(d1, col="red", border="blue")
abline(v=c(0), lwd=3, col="blue")
abline(v=c(0.1168948, 0.2153631), lty=3)
ecdf(cmB.r.int.samp$b_amangcw)(0) #Get cumulative quantile at 0: 0.002; 0.998 above
ecdf(cmB.r.slope.samp$b_amangcw)(0) #Get cumulative quantile at 0: 0.166; 0.834 above

### Scatterplot of posterior mean and sd for amangcw
plot(cmB.r.slope.samp$b_amangcw, cmB.r.slope.samp$sd_id__amangcw,
     xlab="amanxcw mean", ylab = "amanxcw SD")
abline(v=0.1168948, h=0.3673857, col="red", lwd=3)

