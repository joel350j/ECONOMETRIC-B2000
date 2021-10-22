#JOEL ANDRADE
#MIDTERM HW
load("~/Desktop/R WORK/Household_Pulse_data/Household_Pulse_data.RData")
attach(Household_Pulse_data)


# Question 1

northeast_highschool_vaccinated <- (Household_Pulse_data$REGION == "Northeast") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") & (Household_Pulse_data$EEDUC == "HS diploma")
summary(northeast_highschool_vaccinated)
summary(as.numeric(northeast_highschool_vaccinated))
south_advance_notvaccinated <- (Household_Pulse_data$REGION == "South") & (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$EEDUC == "adv deg")
summary(south_advance_notvaccinated)
summary(as.numeric(south_advance_notvaccinated))
# In conclusion, we can compare the different proportions from those that were vaccinated, in the northeast and have a high school diploma.
# against people that live in the south, are not vaccinated and have an advance degree.
#summary(northeast_highschool_vaccinated)
#Mode   FALSE    TRUE
#logical   68086    1028
#> summary(south_advance_notvaccinated)
#Mode   FALSE    TRUE
#logical   68799     315
#get as numeric
table <- table(northeast_highschool_vaccinated, south_advance_notvaccinated)
prop.table(table)
prop.test(table)

#  Question 2
xtabs(~EEDUC + RECVDVACC + GENID_DESCRIBE)
HighSchoolTransgender_vaccinated<- (Household_Pulse_data$EEDUC=="HS diploma") & (Household_Pulse_data$GENID_DESCRIBE =="transgender") & (Household_Pulse_data$RECVDVACC == "yes got vaxx")
summary(HighSchoolTransgender_vaccinated)
female_advance_notvaccinated <- (Household_Pulse_data$GENID_DESCRIBE == "female") & (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$EEDUC == "adv deg")
summary(female_advance_notvaccinated)

table <- table(HighSchoolTransgender_vaccinated, female_advance_notvaccinated)
prop.table(table)
prop.test(table)
# In conclusion, we can compare the different proportions from those that were vaccinated, the transgender and have a high school diploma.
# against people female with advance degree,that are not vaccinated.


# Question 3

attach(Household_Pulse_data)
variable1 <- xtabs(~ EEDUC + RECVDVACC + REGION)
summary(variable1)
#Call: xtabs(formula = ~EEDUC + RECVDVACC + REGION)
#Number of cases in table: 69114
#Number of factors: 3
#Test for independence of all factors:
#  Chisq = 3318, df = 72, p-value = 0
#Chi-squared approximation may be incorrect
summary(RRACE)
BLACK <- (Household_Pulse_data$RRACE=="OTHER")
summary(BLACK)
subsetBlack <- subset(Household_Pulse_data, BLACK)
summary (subsetBlack)
#Choose a subgroup of the sample to consider and provide summary statistics of that subgroup. Explain why this subgroup is interesting.
summary(INCOME)
BLACKINCOME <-subsetBlack$INCOME =="HH income less than $25k"
summary(BLACKINCOME)
#

#We notice that  Blacks making less than $25,000 in the whole data set are accounted around 8815. This may include NAs that are NOT showing in the results summary.
#I am conclude that Blacks are in majority the poor peoples of this sample . given their their level of income
EVERYBODYELSE <-subset(Household_Pulse_data, !BLACK)
summary(EVERYBODYELSE)
EVERYBODYELSEINCOME <- EVERYBODYELSE$INCOME == "HH income less than $25k"
summary(EVERYBODYELSEINCOME)
#  Mode   FALSE    TRUE
#logical   59632    4070
#We can tell that all other races, Asians and Whites making less than $25, 000 and WHO ARE NOT BLACK in the whole data are accounted for 4070 only. Which half of the global Black community in all three regions.
#regression: Form a hypothesis test about an interesting variable, explore whether your chosen subgroup differs from the rest of sample.
attach(Household_Pulse_data)
detach(Household_Pulse_data)
summary(RRACE == "Black")
BLACK <- (Household_Pulse_data$RRACE == "Black") & (Household_Pulse_data$INCOME == "HH income less than $25k")
summary(BLACK)
OTHERRACES <-((Household_Pulse_data$RRACE == "White") & (Household_Pulse_data$INCOME == "HH income less than $25k"))
summary(OTHERRACES)
VARIABLE2 <-lm((OTHERRACES)~ BLACK * REGION)
summary(VARIABLE2)
summary(EEDUC)
ASSOCIATES <- (EEDUC == "assoc deg")
BACHELORS <-  (EEDUC ==  "bach deg")
ADVDEGREE <- (EEDUC == "adv deg")
COMBO <- factor((ASSOCIATES + 2*BACHELORS + 3*ADVDEGREE), levels=c(1,2,3), labels = c("ASSOCIATES" , "BACHELORS", "ADVDEGREE"))
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}
summary(INCOME)
BLACK <- (RRACE == "Black")
POORBLACK <- norm_varb(INCOME == "HH income $35k - 49.9")
DECENTBLACK <- norm_varb(INCOME == "HH income $50k - 74.9")
RICHBLACK <- norm_varb(INCOME == "HH income $75 - 99.9")
data_use_prelim <- data.frame(POORBLACK, DECENTBLACK, RICHBLACK, BLACK)
good_obs_data_use <- complete.cases(data_use_prelim,COMBO)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(COMBO,good_obs_data_use)
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
  pred_hadcovid <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_hadcovid == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

# d) Impress me


# different regressions

regr2 <- lm(as.numeric(HADCOVID)~INCOME+RRACE)
summary(regr2)
# this regression enlightens the fact that there is positive correlation between 
# HADCOVID and Income at all levels. though there is small increase for high income, the Covid impacts all level of Income)
# in The case of race , there is interesting part with the black variable that has a negative estimate against the variable HADCOVID.
#while the Asian has opposite estimate. this draws attention about the health structure of Black.
detach()
attach(Household_Pulse_data)
regr3 <- lm(as.numeric(KIDDOSES)~ REGION+ PRIVHLTH+ PUBHLTH)
summary(regr3)
# the summary gives interesting point of view about region and the willingness to let kids.
# we see with the West and Midwest , the estimate is little higher than the South.
# this could be the piture of political pole and the divide country between red and blue about COVID.
# another interesting fact is that the holder of public insurance get more childern vaccinated than private insurance holder. 
