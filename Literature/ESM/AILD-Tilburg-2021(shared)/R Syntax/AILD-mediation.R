
## Loading Libraries
library(ggplot2); theme_set(theme_bw()) #for data viz
library(lme4) #for multilevel models
library(nlme) #for multilevel models
library(psych) #for describing data
library(tidyr); library(dplyr) #for manipulating data 
library(broom) #for converting results summary to df

#read in the .csv file 
data <- read.csv("B&Lmediation.csv", header = TRUE)

# Examine first few rows of the data set
head(data, 10)

# Describe the variables of interest
vars <- c("fwkstrcw", "fwkdiscw", "freldiscw", "x", "m", "y")
describe(data[, vars])


### **x --> y**: The **c** path. fwkstrcw --> freldiscw

# within-person regressions, x --> y
ggplot(data = data[which(data$id <= 106), ], 
       aes(x = fwkstrcw, y = freldiscw, group = id)) +
  geom_point(color = "black", alpha = .7) + 
  xlab("x = Work Stress (within)") +
  ylab("y = Relationship Dissatisfaction (within)") +
  theme_bw() +
  facet_wrap( ~ id)


### **x --> m**: The **a** path. fwkstrcw --> fwkdiscw                                                                                                                                                                                                                                                                                                                                Let's interpret ...   

# within-person regressions, x --> m
ggplot(data = data[which(data$id <= 106),], 
       aes(x = fwkstrcw, y = fwkdiscw, group = id)) +
  geom_point(color = "blue",  alpha = .7) + 
  xlab("x = Work Stress (within)") +
  ylab("m = Work Dissatisfaction (within)") +
  theme_bw()+
  facet_wrap( ~ id)


### **m --> y**: The **b** path. fwkdiscw --> freldiscw

# within-person regressions, m --> y
ggplot(data = data[which(data$id <= 106),], 
       aes(x = fwkdiscw, y = freldiscw, group = id)) +
  geom_point(color = "red",  alpha = .7) + 
  xlab("m = Work Dissatisfaction (within)") +
  ylab("y = Relationship Dissatisfaction (within)") +
  theme_bw()+
  facet_wrap( ~ id)


## Restructuring the Data Set into Double-entry (stacked) data.

# 1. Subset down to the variables of interest. 
data1 <- dplyr::select(data, id, time, timec, fwkstrcw, fwkdiscw, freldiscw, x, m, y)
head(data1, 10)


# 2. Combine the two outcome variables (`y` and `m`) into a single `z` variable, resulting in a "longer" dataset. 
#    We will use the `pivot_longer()` function in the `tidyr` library.  

# Making data longer by putting m and y into a single column, z
datalong <- pivot_longer(data = data1,
                         cols = c("m", "y"), # variables we want to combine
                         names_to = "dv", # column that has the variable names
                         values_to = "z") # column with m and y values

# Reorder rows for convenience 
datalong <- arrange(datalong, id, time, dv)

#look at updated data set
head(datalong, 10)


# 3. Add dummy variables that indicate row entry. 

#adding the double indicators 
datalong$dy <- ifelse(datalong$dv == "y", 1, 0)
datalong$dm <- ifelse(datalong$dv == "m", 1, 0)
datalong$dvnum <- ifelse(datalong$dv == "m", 1, 0)

#look at updated data set
head(datalong, 10)


### Running the mediation model

#lme mediation model
model_lme <- lme(fixed = z ~ -1 + 
                   dm + dm:fwkstrcw + dm:timec + #m as outcome
                   dy + dy:fwkdiscw + dy:fwkstrcw + dy:timec, #y as outcome
                 random = ~ -1  +  dm:fwkstrcw + dy:fwkdiscw + dy:fwkstrcw | id, 
                 weights = varIdent(form = ~ 1 | dvnum), #separate sigma^{2}_{e} for each outcome
                 data = datalong,
                 na.action = na.exclude,
                 control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400))

summary(model_lme)


### Fixed Effects

#pulling out fixed effects info
FE <- fixef(model_lme)
FE


# Let's put the parameters into named objects, as these will be useful later.
a <- as.numeric(FE[3])
a
b <- as.numeric(FE[5])
b
cprime <- as.numeric(FE[6])
cprime


### Random Effects

#pulling out random effects info
VarCorr(model_lme)


# Easier to interpret and work with when placed in objects... 

#The *variance* of the *a* paths
sig2_a <- as.numeric(VarCorr(model_lme)["dm:fwkstrcw", "Variance"])
sig2_a

#The *variance* of the *b* paths
sig2_b <- as.numeric(VarCorr(model_lme)["dy:fwkdiscw", "Variance"])
sig2_b

#The variance of the *c'* paths 
sig2_cprime <- as.numeric(VarCorr(model_lme)["fwkstrcw:dy", "Variance"])
sig2_cprime

#The residual *variance* of the mediator variable, 
sig2_em <- (1.00*as.numeric(VarCorr(model_lme)["Residual", "StdDev"]))^2
sig2_em

#The residual *variance* of the outcome variable, 
sig2_ey <- (0.8492324*as.numeric(VarCorr(model_lme)["Residual", "StdDev"]))^2
sig2_ey

#The *covariance* between the $a_{j}$ and $b_{j}$ paths 
#We define a function to convert the correlation to a covariance using the variances
cortocov <- function (r, var1, var2) {
  cov=r*((var1*var2)^0.5)
  return(cov)
}
covajbj <- cortocov(r = as.numeric(VarCorr(model_lme)["dy:fwkdiscw","Corr"]),
         var1 = as.numeric(VarCorr(model_lme)["dm:fwkstrcw","Variance"]),
         var2 = as.numeric(VarCorr(model_lme)["dy:fwkdiscw","Variance"]))
covajbj


### Calculating mediated effect (e.g., indirect effect)  
indirecteffect <- a*b + covajbj   
indirecteffect


### Calculating total effect *c*    
totaleffect <- cprime + a*b + covajbj 
totaleffect


### Calculating percent of total effect accounted for by mediated effect    
percentmediated <- 100*(indirecteffect/totaleffect)  
percentmediated


### Calculating percent of mediated effect accounted for by ab covariance   
percentcovariance <- 100*(covajbj/indirecteffect) 
percentcovariance
