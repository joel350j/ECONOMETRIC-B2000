#JOEL ANDRADE
#LAB 5
load("~/Desktop/R WORK/ecob2000_lecture1/acs2017_ny/acs2017_ny_data.RData")
attach(acs2017_ny)
summary(acs2017_ny) 

use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (Hispanic == 1) & (female == 1) & ((educ_college == 1) | (educ_advdeg == 1))
dat_use <- subset(acs2017_ny,use_varb) 
detach()
attach(dat_use)
summary(dat_use)

MODEL_INFO <- lm(INCWAGE ~ educ_college )
summary(MODEL_INFO)

MODEL_INFO2 <- lm(INCWAGE ~ educ_college + educ_somecoll + in_Queens)
summary(MODEL_INFO2)
#WHEN I RUN THE REGRESSION WITH COLLAGE AND SOME COLLEGE IT CAUSES SOME COLLEGE TO SHOW UP AS N/A OMIT PEOPLE WHO DONT HAVE ATLEAST A BACHELORS.
detach()

attach(acs2017_ny)
use_varb <-  (in_NYC == 1) & (AGE >= 18) & (AGE <= 70) & (LABFORCE == 2) & (WKSWORK2 > 4)  & (UHRSWORK >= 35) & (Hispanic == 1) 
dat_use <- subset(acs2017_ny,use_varb) 
MODEL_1 <- lm(INCWAGE ~ AGE + I(AGE^2) + educ_somecoll + educ_college + educ_advdeg)
summary(MODEL_1)
NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.5) 
dat_graph <-subset(dat_use,graph_obs)  
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,300000), data = dat_graph)
to_be_predicted2 <- data.frame(AGE = 18:70, educ_somecoll = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(MODEL_1, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2, col = "red")
summary(to_be_predicted2$yhat)

to_be_predicted2 <- data.frame(AGE = 18:70, educ_somecoll = 0, educ_college = 0, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(MODEL_1, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2, col = "blue")
summary(to_be_predicted2$yhat)

#WHEN WE EXAMINE THE GRAPH WE SEE THE SUBSET HAVING SLIGHT CURVE WITH A PEAK IN HE MIDDLE MAXING AT 125018, WE CAN CONCLUDE THAT WAGE FOR HISPANICS IS CONSISTENT WITH EDUCATION PEAKING IN THE MIDDLE OF LIFEE
#WE SEE A SIMILAR GRAPH WITH ONLY ADVANCE EDUCATION WITH THE ENDS TILTED DOWN AND THE MAX IN THE MIDDLE OF 85725

MODEL_2 <- lm(INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + educ_somecoll + educ_college + educ_advdeg)
summary(MODEL_2)
NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.5) 
dat_graph <-subset(dat_use,graph_obs)  
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,300000), data = dat_graph)
to_be_predicted2 <- data.frame(AGE = 18:70, educ_somecoll = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(MODEL_2, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2, col = "GREEN")
summary(to_be_predicted2$yhat)
#IN THIS GRAPH WE SEE IT MORE SKEWED TO THE LEFT WITH A MAX OF 125936 IN THE 30'S TO 40'S RANGE

MODEL_3 <- lm((INCWAGE ~ AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + educ_somecoll + educ_college + educ_advdeg ) )
summary(MODEL_3)
NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.5) 
dat_graph <-subset(dat_use,graph_obs)  
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,300000), data = dat_graph)
to_be_predicted2 <- data.frame(AGE = 18:70, educ_somecoll = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(MODEL_3, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2, col = "blue")
summary(to_be_predicted2$yhat)
#IN THIS GRAPH WE SEE IT MORE SKEWED TO THE RIGHT WITH A MAX OF 131747 IN THE 40'S TO 50'S RANGE



MODEL_4 <- lm(INCWAGE ~ I(log(AGE)) + I(log(AGE^2)) + I(log(AGE^3)) )
summary(MODEL_4)


MODEL_A <- lm(INCWAGE ~ AGE + I(AGE^2) + educ_somecoll + educ_college + educ_advdeg)
summary(MODEL_A)
NNobs <- length(INCWAGE)
set.seed(12345) 
graph_obs <- (runif(NNobs) < 0.5) 
dat_graph <-subset(dat_use,graph_obs)  
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,300000), data = dat_graph)
to_be_predicted2 <- data.frame(AGE = 18:70, educ_somecoll = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(MODEL_A, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2, col = "black")
summary(to_be_predicted2$yhat)
#THE WAGES ARE: 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#94483  111149  118743  116300  123407  125018 
#WE CAN SEE WAGES MORE HHIGHER THAN AVERAGE AND IT MAKES SENSE SINCE THESE INDIVIDUALS ARE MORE EDUCATED
to_be_predicted2 <- data.frame(AGE = 18:70, educ_somecoll = 0, educ_college = 0, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(MODEL_A, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2, col = "purple")
summary(to_be_predicted2$yhat)
#THE WAGES ARE: 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1502   18168   25762   23318   30426   32037 
#IF WE COMPARE THESE TWO WE SEE LOWER WAGES AND THIS MAKES SENSE WHEN WE TAKE OUT EDUCATION HAVING EDUCATION AS A POSITIVE EFFECT ON WAGES
