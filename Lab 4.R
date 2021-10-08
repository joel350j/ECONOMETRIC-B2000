#Joel Andrade
#Lab 4
load("~/Desktop/R WORK/ecob2000_lecture1/acs2017_ny/acs2017_ny_data.RData")
attach(acs2017_ny)
summary(acs2017_ny) 
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
attach(dat_use)
summary(dat_use)
model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
plot(model_temp1)
# maybe get fancy
require(stargazer)
stargazer(model_temp1, type = "text")


suppressMessages(require(AER))
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <- subset(dat_use,graph_obs)  

plot(dat_graph)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)


plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,300000), data = dat_graph)

# change this line to fit your regression

to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)

to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)



lines(yhat ~ AGE, data = to_be_predicted2)
to_be_predicted2 <- data.frame(AGE = 25:55, female = 0, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 1)

to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)



lines(yhat ~ AGE, data = to_be_predicted2)

#  When viewing the plot, the first example of "to be predicted2" showed a line plot in the lower incwage region, this line plot
#  took into consideration women without an advance degree.  In the second line plot we changed women to false and incorporated 
#  advance degree.  The showed a higher incwage for people with an advance degree compared to people with out a advance degree.

#  In the regression we found that all the variables showed signaficance except Asian & Amindian based on the "P values".  Thus we 
#  have failed to reject the Null Hypo for Asian and Amindian becuase there is no correlation on Incwage.  The the case of the 
#  signaficant variables, we reject the Null Hypothesis becuase the value show a signaficant correlation between Incwage.

#  When examining the "T value" we can determine the results to based on "P Value" becuase P comes from T.
#  
model_temp1 <- lm(log1p(INCWAGE) ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
# For the independent variables (AfAm, Asain, Amindian, Hispanic) we fail to reject the Null Hyopothesis.
suppressMessages(require(AER))
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <- subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,20), data = dat_graph)

to_be_predicted2 <- data.frame(AGE = 25:55, female = 0, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 1, educ_somecoll = 0, educ_college = 0, educ_advdeg = 0)

to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)



lines(yhat ~ AGE, data = to_be_predicted2)
to_be_predicted2 <- data.frame(AGE = 25:55, female = 0, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 1)

to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)



lines(yhat ~ AGE, data = to_be_predicted2)
# When running the log of the regression we find that two additional varibales are now not signaficant (AfAm & Hispanic) Also the Log
# has compressed the range between data points.  In the log of the regression we find the data is more concentrated distribution. 

detach()
detach()
detach()
detach()

attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK <= 50) & (INCTOT <= 24999)
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
attach(dat_use)
#############
summary(dat_use)
model_temp1 <- lm(INCWAGE ~ AGE + AfAm + educ_college + educ_advdeg + below_povertyline + FOODSTMP)
summary(model_temp1)
plot(model_temp1)
# maybe get fancy
require(stargazer)
stargazer(model_temp1, type = "text")
# play with stargazer instead of summary, it can look nicer!


suppressMessages(require(AER))
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <- subset(dat_use,graph_obs)  

plot(dat_graph)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)


plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(-30,30000), data = dat_graph)

# change this line to fit your regression

to_be_predicted2 <- data.frame(AGE = 25:55, AfAm = 1, educ_college = 0, educ_advdeg = 0, below_povertyline = 1, FOODSTMP = 1)

to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)



lines(yhat ~ AGE, data = to_be_predicted2)

to_be_predicted2 <- data.frame(AGE = 25:55, AfAm = 1, educ_college = 1, educ_advdeg = 1, below_povertyline = 1, FOODSTMP = 1)

to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)



lines(yhat ~ AGE, data = to_be_predicted2)

# In this new regression we have a dependent variable of income wage and looked at the independent variables AfAm, Educ_college, Educ-advdeg, 
# Below_povertyline, and FOODSTMP.  We reject the Null Hypothesis, we found significance accross all independent variables and conclude the Alt 
# Hypothesis and conclude there is a relation between these independent variables and income wage.