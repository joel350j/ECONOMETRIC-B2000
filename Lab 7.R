#Joel Andrade 
#Lab 7
load("~/Desktop/R WORK/Household_Pulse_data updated/Household_Pulse_data.RData")
attach(Household_Pulse_data)

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")

pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR <= 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)


d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
summary(d_educ)
levels(dat_use1$EEDUC)

d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))

d_vaxx <- data.frame(model.matrix(~ dat_use1$vaxx)) # check number of obs to see that this snips off NA values

# note that, depending on your subgroup, this might snip off some columns so make sure to check summary() of each -- don't want Min = Max = 0!

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],
  dat_use1$TBIRTH_YEAR[!is.na(dat_use1$vaxx)],
  d_educ[!is.na(dat_use1$vaxx),2:7],
  d_marstat[!is.na(dat_use1$vaxx),2:6],
  d_race[!is.na(dat_use1$vaxx),2:4],
  d_hispanic[!is.na(dat_use1$vaxx),2],
  d_gender[!is.na(dat_use1$vaxx),2:5],
  d_region[!is.na(dat_use1$vaxx),2:4]) # need [] since model.matrix includes intercept term


# this is just about me being anal-retentive, see difference in names(dat_for_analysis_sub) before and after running this bit
names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[2] <- "TBIRTH_YEAR"
names(dat_for_analysis_sub)[17] <- "Hispanic"



require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.1) # use 10% as training data
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(vaxx ~ TBIRTH_YEAR + EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      Hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      REGIONSouth + REGIONMidwest + REGIONWest
                    , dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)


#linear model
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$vaxx)

#when we look at the linear model we see 6012 and 31255 as being correctly false and ture respectively.
#however we see incorrect values of 1777 and 22531 with a true predicted but false value and false predicted but true value respectively

# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)

#when we look at the linear model we see 543 and 53546 as being correctly false and ture respectively.
#however we see incorrect values of 7246 and 240 with a true predicted but false value and false predicted but true value respectively

#when we comapre both models we see that the logit model has a lot more true values predicted truthfully  and less false values correctly predicted false 