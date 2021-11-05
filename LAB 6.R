#Joel Andrade
#Lab 6
load("~/Desktop/R WORK/Household_Pulse_data/Household_Pulse_data.RData")
attach(Household_Pulse_data)
summary(Household_Pulse_data$RHISPANIC)

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
summary(Household_Pulse_data$vaxx)
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA")
summary(Household_Pulse_data$vaxx)

pick_use1 <- (Household_Pulse_data$REGION == "South") 
dat_use1 <- subset(Household_Pulse_data, pick_use1)
summary(Household_Pulse_data$REGION)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 
model_logit1 <- glm(vaxx ~ EEDUC*RRACE + MS + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)
# Using vaccination as the dependent variable and if we focus on education and race as an independent factor we see that
# an other race and an Associate degree tend to be more vaccination with an estimate of 1.72523 and a level of significance of 0.01
#because the p-value is 0.008149 and the level of significance is 0.01 we reject the null that aduccation and race does not have a role in vaccination and 
#(accept)/replace the alternative where race and education play a role in vaccination

to_be_predicted1<- data.frame(EEDUC = "some coll", RRACE = "Other", MS = "separated",RHISPANIC = "Hispanic",  GENID_DESCRIBE = "female",
                                 data = dat_use1)
to_be_predicted1$yhat<-predict(model_logit1, to_be_predicted1, type="response")
summary(to_be_predicted1$yhat)
#we see that there is a 66.42% probability that this subgroup would be vaccinated making it significant.

#Also estimate a probit model (details in Lecture Notes) and OLS, with the same X and Y variables. Compare the results, such as coefficients and predicted values. If you’re eager, try to split the sample into training and test data, then compare which model predicts better in the sample that it hasn’t seen yet.
model_probit1 <- glm(vaxx ~ EEDUC*RRACE + MS + RHISPANIC + GENID_DESCRIBE,
                     family = binomial, data = dat_use1)

summary(model_probit1)
#when we compare using the probit model looking at vaccination as the dependent variable focusing on education and race as an independent factor we see that
# an other race and an Associate degree tend to be more vaccination with an estimate of 1.64201 and a level of significance of 0.01, if we compare we got 1.72523 before.
#Because the p-value is 0.008149 and the level of significance is 0.01 we reject the null that aduccation and race does not have a role in vaccination and 
#(accept)/replace the alternative where race and education play a role in vaccination

pick_use2 <- (Household_Pulse_data$REGION == "South") 
dat_use2 <- subset(Household_Pulse_data, pick_use2)
summary(Household_Pulse_data$REGION)
dat_use2$RECVDVACC <- droplevels(dat_use2$RECVDVACC) 
model_logit2 <- glm(vaxx ~ EEDUC+ RRACE*MS + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use2)
summary(model_logit2)

# Using vaccination as the dependent variable and if we focus on education and race as an independent factor we see that
# black race and separated tend to be more vaccination with an estimate of 2.008972 and a level of significance of 0.01
#because the p-value is 0.004521 and the level of significance is 0.01 we reject the null that martial status and race does not have a role in vaccination and 
#(accept)/replace the alternative where race and martial status play a role in vaccination


to_be_predicted2<- data.frame(EEDUC = "some coll", RRACE = "Black", MS = "separated",RHISPANIC = "Hispanic",  GENID_DESCRIBE = "female",
                              data = dat_use2)
to_be_predicted2$yhat<-predict(model_logit2, to_be_predicted2, type="response")
summary(to_be_predicted2$yhat)
#we see that there is a 83% probability that this subgroup would be vaccinated making it significant.
#Also estimate a probit model (details in Lecture Notes) and OLS, with the same X and Y variables. Compare the results, such as coefficients and predicted values. If you’re eager, try to split the sample into training and test data, then compare which model predicts better in the sample that it hasn’t seen yet.
model_probit2 <- glm(vaxx ~ EEDUC+ RRACE* MS + RHISPANIC + GENID_DESCRIBE,
                     family = binomial, data = dat_use2)

summary(model_probit2)
#when we compare using the probit model looking at vaccination as the dependent variable focusing on martial status and race as an independent factor we see that
# an black race and seperated tend to be more vaccination with an estimate of 2.008972 and a level of significance of 0.01, if we compare we got 2.008972 before.
#Because the p-value is 0.004521 and the level of significance is 0.01 we reject the null that martial status and race does not have a role in vaccination and 
#(accept)/replace the alternative where race and martial status play a role in vaccination

