#Joel Andrade
#Exam 2
load("~/Desktop/R WORK/Midterm 2/NHIS2020_data.RData")
attach(data_NHIS)
detach(data_NHIS)
summary(data_NHIS)
#1

#Estimate:
#Standard error:
#t-stat:
#p-value:
#Confidence interval:


#2 
#a. What is the predicted probability that a 30-year-old person (not a female) will have a job with benefits?
#log(1-p)= -0.019*30 + (0.00002 *30^2) + (-0.470 )*0+0.0082*0 + (-0.00001*0) - 1.84=0.7838
#p=0.7838
#b. What is the predicted probability that a 30-year-old female person will have a job with benefits?
#log(1-p)= -0.019*30 + (0.00002 *30^2) + (-0.470 )*1+0.0082*1 + (-0.00001*30^2)*1 - 1.82=
#p=0.7417
#c. Describe and explain the impact of the squared age terms in the model, for male and female.
#p=0.500003
#Rate change of log with the age of the person is 2*(-0.00001) it is more female


#3
#Now do your own analysis using “NHIS2020_data.RData”. Explore likelihood of people working at a job that offers health insurance, or choose a different topic if you’d like. The data includes information on region, whether urban or rural area, usual hours of work, fulltime or parttime, health status, height, weight and BMI, along with demographics like age, race/ethnicity, gender, sexual orientation, family size, marital status, whether served in armed forces, citizenship with years in US, and household income.
summary(data_NHIS$AGE)
data_NHIS$insurance <- (data_NHIS$HINOTCOVE == "yes got insurance")

pick_use1 <- (data_NHIS$AGE <= 25)
dat_use1 <- subset(data_NHIS, pick_use1)

d_educ <- data.frame(model.matrix(~ dat_use1$EDUC))
summary(d_educ)
levels(dat_use1$EDUC)
d_marstat <- data.frame(model.matrix(~ dat_use1$MARST))
d_race <- data.frame(model.matrix(~ dat_use1$RACEA))
d_hispanic <- data.frame(model.matrix(~ dat_use1$HISPETH))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))

d_insurance <- data.frame(model.matrix(~ dat_use1$HINOTCOVE))

dat_for_analysis_sub <- data.frame(
  d_insurance[,2],
  d_educ[!is.na(dat_use1$insurance),2:7],
  d_marstat[!is.na(dat_use1$insurance),2:6],
  d_race[!is.na(dat_use1$insurance),2:4],
  d_hispanic[!is.na(dat_use1$insurance),2],
  d_region[!is.na(dat_use1$insurance),2:4]) 

names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "insurance"
names(dat_for_analysis_sub)[17] <- "Hispanic"

summary(dat_for_analysis_sub)

require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$insurance)
restrict_1 <- (runif(NN) < 0.1) 
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(insurance ~  EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
                      MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + RACEAAmerican.Indian +
                       Hispanic + REGIONSouth + REGIONWest + d_hispanic..is.na.insurance...2.
                    , dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)


#Logit Model
model_logit1 <- glm(insurance ~ EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
                      MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + RACEAAmerican.Indian +
                      Hispanic + REGIONSouth + REGIONWest + d_hispanic..is.na.insurance...2.,
                    family = binomial, data = dat_for_analysis_sub)
summary(model_logit1)
#Using insurance as the dependent variable and if we focus on education and race as an independent factor we see that
# RACEAAleut.Alaskan, RACEAAmerican.Indian, EDUCless.than.hs, EDUCHS.diploma, EDUCsome.college, tend to have insurance with an estimate of 1.1142, 1.0529, 0.9992, 1.9937, 1.5514, 0.9208 respectively. They have a level of significance of 0.001
#because the p-value is smaller than the level of significance we reject the null that education and race does not have a role in insurance and 
#(accept)/replace the alternative where race and education play a role in insurance.

model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$insurance)

#OLS


model_temp1 <- lm(insurance ~ EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
                    MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + RACEAAmerican.Indian +
                    Hispanic + REGIONSouth + REGIONWest + d_hispanic..is.na.insurance...2., data = dat_for_analysis_sub)
summary(model_temp1)

#Using insurance as the dependent variable and if we focus on education and race as an independent factor we see that
# RACEAAleut.Alaskan, RACEAAmerican.Indian, EDUCless.than.hs, EDUCHS.diploma, EDUCsome.college, tend to have insurance with an estimate of 0.1165384, 0.0928424, 0.2152030, 0.1410700, 0.0610950, respectively. They have a level of significance of 0.001
#because the p-value is smaller than the level of significance we reject the null that education and race does not have a role in insurance and 
#(accept)/replace the alternative where race and education play a role in insurance.



model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$insurance)

#When we compare the logit model and the OLS we see they oth come to same conclusion being that insurance is influenced by race and education.








