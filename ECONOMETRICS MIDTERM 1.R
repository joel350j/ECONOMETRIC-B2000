#JOEL ANDRADE
#ECONOMETRICS MIDTERM 1
load("~/Desktop/R WORK/Household_Pulse_data/Household_Pulse_data.RData")
attach(Household_Pulse_data)
xtabs(~ EEDUC + RECVDVACC + REGION)
summary(Household_Pulse_data)
#1
REGIONS_ANSWER1<-lm(as.numeric(RECVDVACC)~ EEDUC* REGION)
summary(REGIONS_ANSWER1)
#If we look at the western regionwe see that individuals with an highschool degree the significance is high if they receive an highschool degree and not. if we look at the higher levels of educations it is still significant for all of west.
#If we look at the midwest we see the significance is higher for highschool than the other higher level of educations
#when looking at the south some highschool and highschool diploma is significant but not other degrees.
#when we compare the different regions we see that the west is more likely to be vaccinated.
REGIONS_ANSWER2<-lm(as.numeric(RECVDVACC)~ EEDUC* GENID_DESCRIBE)
summary(REGIONS_ANSWER2)
#When we look at gender identity for individuals we see male and female being more significant than other identities like transgender and other. Male and female are more likely to get vaccinated than transgender and other.
##3
#compared mortgage and race to see how race plays a factor in property ownership
EEDUC1 <- as.numeric( EEDUC)
RACE <- RRACE
regression <- lm(EEDUC1 ~ RACE)
summary(regression)

#Using a k-nn classifier, can you find relevant information to predict an interesting outcome? How good is the classifier? Discuss.
MORTGAGE <- MORTCUR
MORTGAGE <- as.numeric(MORTCUR)
RACE <- as.numeric(RRACE)
INDX1 <- factor((MORTGAGE + 2*RACE), levels=c(1,2), labels = c("MORTGAGE" , "RACE"))


indx <- factor(RRACE)
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

race <- as.numeric(RRACE)
norm_race <- norm_varb(race)

data_use_prelim <- data.frame(norm_race)
good_obs_data_use <- complete.cases(data_use_prelim,INDX1)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(INDX1,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(Household_Pulse_data,select1)
test_data <- subset(Household_Pulse_data,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
prop.table(summary(cl_data))

summary(train_data)


for (indx in seq(1, 9, by= 2)) {
  pred_RACE <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_RACE == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
#The knn did not work but if it were to work would have seen mortgage and race being influencial 

#Can you explain some other interesting information about this data? Some interesting crosstabs? Maybe regressions? Impress me.
summary(EGENID_BIRTH)
GEN_BIRTH <- as.numeric(EGENID_BIRTH)
SEX_ORT <- SEXUAL_ORIENTATION
regression <- lm(GEN_BIRTH ~ SEX_ORT)
summary(regression)
#when we compare sexual orientation and birth we see a significance more with gay and lesbian, bisexual, and something else
#All of the work on this exam is my own, answered honestly as rules state.
#Name:JOEL ANDRADE
#Date:OCTOBER 14 2021