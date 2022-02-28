
### Load the packages we will need
library(here)
library(lme4)
library(brms)

### Load the psychometrics dataset
psychometrics <- read.csv(here('psychometrics.csv'))
head(psychometrics, 10)

### Create versions of person, time and item that R treats as factors
psychometrics$catitem <- as.factor(psychometrics$item)
psychometrics$catperson <- as.factor(psychometrics$person)
psychometrics$cattime <- as.factor(psychometrics$time)

### Estimate a model with crossed random effects of person, time, item and their interactions
mod1<-lmer(y ~  1 + (1|catperson) + (1|cattime) + (1|catitem)
           + (1|catperson:cattime) + (1|catperson:catitem) + (1|cattime:catitem), data=psychometrics)
summary(mod1)


### Extract the specific variance estimates

VarCorr(mod1)[[1]][1,1] #catperson:cattime 

VarCorr(mod1)[[2]][1,1] #catperson:catitem 

VarCorr(mod1)[[3]][1,1] #catperson 

VarCorr(mod1)[[4]][1,1] #cattime:catitem 

VarCorr(mod1)[[5]][1,1] #cattime 

VarCorr(mod1)[[6]][1,1] #catitem 

sigma(mod1)^2            #Residual


### Calculate the total variance from the components
Totvar<-VarCorr(mod1)[[1]][1,1] +
  VarCorr(mod1)[[2]][1,1] +
  
  VarCorr(mod1)[[3]][1,1] +
  
  VarCorr(mod1)[[4]][1,1] +
  
  VarCorr(mod1)[[5]][1,1] +
  
  VarCorr(mod1)[[6]][1,1] +
  
  sigma(mod1)^2            #Residual

Totvar

var(psychometrics$y, na.rm=T) #Variance of original y


### Use specific components to calculate Rc, the reliability of change
Rc<-(VarCorr(mod1)[[1]][1,1])/((VarCorr(mod1)[[1]][1,1]) + (sigma(mod1)^2)/4)
Rc

### Run using brms
brmod1<-brm(y ~  1 + (1|catperson) + (1|cattime) + (1|catitem)
            + (1|catperson:cattime) + (1|catperson:catitem) + (1|cattime:catitem),
            seed=1, cores=4, chains=2, data=psychometrics)
print(summary(brmod1), digits = 6)

### Take a look at the posterior distributions
plot(brmod1)

### Calculate the total variance using the Bayesian estimates of variance components
brmtotvar <-
  0.393386^2 +   #catitem
  0.615676^2 +   #catperson
  0.439904^2 +   #catperson:catitem
  0.504661^2 +   #catperson:cattime
  0.049373^2 +   #cattime  
  0.069099^2 +   #cattime:catitem
  0.549069^2     #sigma
brmtotvar

### Bayesian Version: Use specific components to calculate bRc, the reliability of change
bRc<-(0.504661^2)/(0.504661^2  + (0.549069^2)/4)
bRc

Rc
