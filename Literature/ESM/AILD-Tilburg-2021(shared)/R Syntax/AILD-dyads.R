
### Preliminaries

# Loading Libraries
library(dplyr) #for data manipulation
library(reshape2) #for data manipulation
library(ggplot2) #for plotting
library(nlme) #for mlm analysis
library(psych) #for descriptives

# Loading Data
BLdyads_long <- read.csv("BLdyads_long.csv",header=TRUE)
head(BLdyads_long)

# 1. Reformatting the data
  
# Create double-entry data (note that the outcome variables to be combined are commented out of the id.vars list)
BLdyads_melt <- melt(data = BLdyads_long,
                     id.vars = c("coupleid","time","time7c",
                                 "f_personid",    #"f_reldis",
                                 "f_wrkstrs","f_wrkstrsc","f_wrkstrsc_b","f_wrkstrsc_w",
                                 "m_personid",    #"m_reldis",
                                 "m_wrkstrs","m_wrkstrsc","m_wrkstrsc_b","m_wrkstrsc_w"),
                     na.rm = FALSE, 
                     value.name = "reldis") #naming new variable

# Reorder rows for convenience 
BLdyads_melt <- BLdyads_melt[order(BLdyads_melt$coupleid, BLdyads_melt$time, BLdyads_melt$var), ]

# Add the two dummy indicators 
BLdyads_melt$female <- ifelse(BLdyads_melt$variable == "f_reldis", 1, 0)
BLdyads_melt$male <- ifelse(BLdyads_melt$variable == "m_reldis", 1, 0)

# Add another gender indicator
BLdyads_melt$gender <- as.numeric(factor(BLdyads_melt$variable))

# Create integrated personid variable
BLdyads_melt$personid <- ifelse(BLdyads_melt$female == 1, BLdyads_melt$f_personid, BLdyads_melt$m_personid)

# Subset and organize into a new data set for easy viewing and efficiency
dput(names(BLdyads_melt)) #to obtain variable list for easy cut&paste
BLdyads_doubleentry <- BLdyads_melt[ ,c("coupleid", "personid","time", "time7c",
                                "gender", "reldis","female", "male",
                                "f_wrkstrs", "f_wrkstrsc", "f_wrkstrsc_b", "f_wrkstrsc_w",
                                "m_wrkstrs", "m_wrkstrsc", "m_wrkstrsc_b", "m_wrkstrsc_w")]

head(BLdyads_doubleentry)

# 2. Plotting the Data.
ggplot(data = BLdyads_doubleentry, aes(x = reldis)) +
  geom_histogram(fill = "white", color = "black") + 
  labs(x = "Relationship Dissatisfaction") +
  facet_grid(. ~ gender) # creating a separate plot for each gender

ggplot(data = subset(BLdyads_doubleentry, coupleid <= 9), aes(x = time, group = personid), legend = FALSE) +
  geom_rect(mapping = aes(xmin = time-.5, xmax = time+.5, ymin = 0, ymax = 5, fill = f_wrkstrsc_w), alpha = 0.6) + # creating rectangles in the background of the plot colored by female work stressors
  geom_rect(mapping = aes(xmin = time-.5, xmax = time+.5, ymin = 5, ymax = 10, fill = m_wrkstrsc_w), alpha = 0.6) + # creating rectangles in the background of the plot colored by male work stressors
  geom_point(aes(x = time, y = reldis, color = factor(gender)), shape = 17, size = 3) + # creating a different colored point for each gender
  geom_line(aes(x = time, y = reldis, color = factor(gender)), lty = 1, size=1) + # creating a different colored line for each gender
  xlab("Time") + 
  ylab("Relationship Dissatisfaction") + ylim(0, 10) +
  scale_x_continuous(breaks=seq(0, 20, by = 5)) + 
  facet_wrap( ~ coupleid) +# creating a separate plot for each dyad
  theme(legend.position = "none")

#between-peson differences
#females
ggplot(data = subset(BLdyads_doubleentry, time == 0 & female == 1), 
       aes(x = f_wrkstrsc_b)) +
  geom_histogram(fill = "white", color = "black") + 
  labs(x = "Female Work Stress (dyad-level centered)")

#males
ggplot(data = subset(BLdyads_doubleentry, time == 0 & male == 1), aes(x = f_wrkstrsc_b)) +
  geom_histogram(fill = "white", color = "black") + 
  labs(x = "Male Work Stress (dyad-level centered)")


# 3. The Dyadic Multilevel Model.

# Fitting the full model with both between-dyad and within-dyad actor and partner effects
model1 <- lme(fixed = reldis ~ -1 + 
                              female + #intercept
                              female:f_wrkstrsc_b + female:f_wrkstrsc_w + #actor-effects
                              female:m_wrkstrsc_b + female:m_wrkstrsc_w + #partner-effects
                              male + #intercept
                              male:m_wrkstrsc_b + male:m_wrkstrsc_w + #actor-effects
                              male:f_wrkstrsc_b + male:f_wrkstrsc_w + #partner-effects 
                              male:time7c + female:time7c,            #fixed effects of time for each gender
              random = ~ -1 + 
                        female + male + #intercepts
                        female:f_wrkstrsc_w + male:m_wrkstrsc_w + #actor-effects
                        female:m_wrkstrsc_w + male:f_wrkstrsc_w | coupleid, #partner-effects
              weights=varIdent(form = ~1 | gender),    # invokes separate sigma^{2}_{e} for each gender
              corr=corSymm(form = ~1 | coupleid/time), # invokes off-diagonal sigma_{e1e2} symmetric structure for level 1 residuals
              data=BLdyads_doubleentry,
              control=lmeControl(maxIter=10000, opt="optim")) 

summary(model1)


model2 <- lme(fixed = reldis ~ -1 + 
                female + #intercept
                female:f_wrkstrsc_b + female:f_wrkstrsc_w + #actor-effects
                female:m_wrkstrsc_b + female:m_wrkstrsc_w + #partner-effects
                male + #intercept
                male:m_wrkstrsc_b + male:m_wrkstrsc_w + #actor-effects
                male:f_wrkstrsc_b + male:f_wrkstrsc_w + #partner-effects 
                male:time7c + female:time7c,            #fixed effects of time for each gender
              random = ~ -1 + 
                female + male + #intercepts
                female:f_wrkstrsc_w + male:m_wrkstrsc_w | coupleid, #actor-effects
              female:m_wrkstrsc_w + male:f_wrkstrsc_w | coupleid, #partner-effects
              weights=varIdent(form = ~1 | gender),             #invokes separate sigma^{2}_{e} for each gender
              corr=corAR1(form = ~1 | coupleid/gender/time),    #invokes an AR(1) structure for level 1 residuals
              data=BLdyads_doubleentry,
              control=list(maxIter=1000, opt="optim")) 

summary(model2)


