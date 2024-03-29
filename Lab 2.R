#JOEL ANDRADE 
#Lab 2
#Group Members: JOEL ANDRADE, KOUAKOU ALLAH
load("~/Desktop/R WORK/ecob2000_lecture1/acs2017_ny/acs2017_ny_data.RData")
attach(acs2017_ny)
#Variales in the database
names(acs2017_ny)
summary(acs2017_ny)
#Overall Goal: Using PUMS data, consider how outcomes of interest vary with college major choice. 
#Groups should prepare a 1-min presentation by one of the group members about their experiment process and results. You get 45 min to prepare.
#For now don’t get lost in looking at every major but maybe pick a couple of majors to compare (for example, Econ & Psych, but now I’ve stolen that idea so pick a different one).

mean(INCWAGE[LABFORCE==2])
# Mean wage of people in the labor force is 55047.35
mean(INCWAGE[DEGFIELD=="Fine Arts" & LABFORCE==2])
# Mean wage of people who have a Fine Arts degree and in the labor force is 58541.59
mean(INCWAGE[DEGFIELD=="Social Sciences" & LABFORCE==2])
# Mean wage of people who have a Social Sciences degree and in the labor force is 102817.8
# Mean wage for people with a Social Sciences degree is greater then those with a Fine Arts degree

degfield<-c("Fine Arts", "Social Sciences") 
meanincome<-c(mean(INCWAGE[DEGFIELD=="Fine Arts" & LABFORCE==2]), 
mean(INCWAGE[DEGFIELD=="Social Sciences" & LABFORCE==2])) 
xtabs(meanincome~degfield)

#How do the two groups differ, in who chooses that major? Are there differences in gender, race/ethnicity, ancestry, immigration status? 
#What other outcomes differ – for example, neighborhood?

#Differences in wages for Male and Females in Fine Arts and Social Sciences fields
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & LABFORCE==2])
## Mean wage for females in Fine Arts and in the labor force is 53692.25
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & LABFORCE==2])
## Mean wage for males in Fine Arts and in the labor force is 64614.12
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & LABFORCE==2]) 
## Mean wage for females in Social Sciences and in the labor force is 73544.76
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & LABFORCE==2]) 
## Mean wage for males in Social Sciences and in the labor force is 128324.4

#Difference between Males and Females in the Fine Arts field that are in the labor force is 10921.87
Netdiff_FineArts<-mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & LABFORCE==2])-mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & LABFORCE==2]) 
#Difference between Males and Females in the Social Sciences field that are in the labor force is 54779.68
Netdiff_SocialSciences<-mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & LABFORCE==2])-mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & LABFORCE==2])
Netdiff_FineArts
Netdiff_SocialSciences
# The difference in Social Sciences field is much greater between genders

# We breakdown the sample size by examining races between hispanic and white population
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & Hispanic & LABFORCE==2])
# Mean wage for females in Fine Arts and in the labor force that are hispanic is 63600.45
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & Hispanic & LABFORCE==2])
# Mean wage for males in Fine Arts and in the labor force that are hispanic is 49741.76
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & Hispanic & LABFORCE==2])
# Mean wage for females in Social Sciences and in the labor force that are hispanic is 62207.47
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & Hispanic & LABFORCE==2])
# Mean wage for males in Social Sciences and in the labor force that are hispanic is 115180.2
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & white & LABFORCE==2])
# Mean wage for females in Fine Arts and in the labor force that are white is 52613.12
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & white & LABFORCE==2])
# Mean wage for males in Fine Arts and in the labor force that are white is 66710.86
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & white & LABFORCE==2])
# Mean wage for females in Social Sciences and in the labor force that are white is 75949.85
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & white & LABFORCE==2])
# Mean wage for males in Social Sciences and in the labor force that are white is 138232

#Wage differences between Male and Females hispanic in the Fine Arts field
Netdiff_FineArts_Hispanic<-mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & Hispanic & LABFORCE==2])-mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & Hispanic & LABFORCE==2])
#Wage differences between Male and Female hispanic in the Social Sciences field
Netdiff_SocialSciences_Hispanic<-mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & Hispanic & LABFORCE==2])-mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & Hispanic & LABFORCE==2])
Netdiff_FineArts_Hispanic 
## -13858.69
# based on the data hispanic males with a Fine Arts degree make less than females in the labor force by a difference of 13858.69
Netdiff_SocialSciences_Hispanic 
## 52972.77
# based on the data hispanic males with a Fine Arts degree make more than females in the labor force by a difference of 52972.77

#Wage differences between white Male and Females in the Fine Arts field
Netdiff_FineArts_white<-mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & white & LABFORCE==2])-mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & white & LABFORCE==2])
#Wage differences between white Male and Females in the Social Sciences field
Netdiff_SocialSciences_white<-mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & white & LABFORCE==2])-mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & white & LABFORCE==2])
Netdiff_FineArts_white
## 14097.74
# based on the data white males with a Fine Arts degree make more than females in the labor force by a difference of 14097.74
Netdiff_SocialSciences_white 
## 62282.14
# based on the data white males with a Social Sciences degree make more than females in the labor force by a difference of 62282.14

#Now consider the experiment protocol after peeking at some of the data. You’re implicitly comparing a subgroup, of those people who finished a 4-year degree and reported information. 
#What additional restrictions might you impose? Should you include retired people? People not in the labor force?
#Looking at that subgroup, what is the size of the difference in outcome? 
#What is the standard error of that difference measure? Using your stats knowledge, how confident are you, that the difference is actually there and not an artefact of sampling?
#Look at the crosstabs and compute the marginal probabilities. 
#How do those inform? To check if you did it right, compute some of the marginal probabilities using Bayes’ Theorem. Is your crosstab mutually exclusive and exhaustive?


c<- sd(INCWAGE[female == 0 & DEGFIELD == "Social Sciences" & Hispanic==1 & LABFORCE == 2])
d<- sqrt(length(INCWAGE[female == 0 & DEGFIELD == "Social Sciences" & Hispanic==1 & LABFORCE == 2]))
x<- mean(INCWAGE[female == 0 & DEGFIELD == "Social Sciences" & Hispanic==1 & LABFORCE == 2])
sem1 <- c/d

c(x-2*sem1,x+2*sem1)
## [1] 88694.21 141666.27
e<- sd(INCWAGE[female == 0 & DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2])
f<- sqrt(length(INCWAGE[female == 0 & DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2]))
y<- mean(INCWAGE[female == 0 & DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2])
sem2 <- e/f
c(y-2*sem2,y+2*sem2)
## [1] 90985.23 129594.05
library(plyr)
dat_use1 <- subset(acs2017_ny,((INCWAGE > 0) & in_NYC))
ddply(dat_use1, .(PUMA), summarize, inc90 = quantile(INCWAGE,probs = 0.9), inc10 = quantile(INCWAGE,probs = 0.1), n_obs = length(INCWAGE))

ddply(dat_use1, .(female), summarize, inc90 = quantile(INCWAGE[DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2],probs = 0.9), inc10 = quantile(INCWAGE[DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2],probs = 0.1), n_obs = length(INCWAGE[DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2]))

#Marginal Probabilities
#Our null hypothesis was that males would have larger wages when looking at the labor force, gender and races in the same degree fields
prop.table(ddply(dat_use1, .(female), summarize, inc90 = quantile(INCWAGE[DEGFIELD == "Social Sciences" & white==1 & LABFORCE ==2],probs = 0.9), inc10 = quantile(INCWAGE[DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2],probs = 0.1), n_obs = length(INCWAGE[DEGFIELD == "Social Sciences" & white==1 & LABFORCE == 2])))
#From the results we confirmed this hypothesis so our p-value was equal to the observed value 


#What other factors could explain the difference in outcome? Among your list of differences in who chooses the major, are there some potential confounders? 
#Can you look at other educational attainments?
#What additional evidence would you look at? 
#What conclusions could you draw from that? How confident would you be, in the conclusions made? 
#What other conclusions could be drawn, from that same evidence? If you were to try to persuade someone, imagine what evidence would be required to persuade a person with the opposite view?


#We look at the martial status of individuals in these subgroups 
#We first look at the white subgroup and when we incorporate martial status we see that married individuals earn more for Fine Arts and Social Sciences fields
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & white & LABFORCE==2 & unmarried == 0])
## 57520.08
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & white & LABFORCE==2 & unmarried == 0])
## 81659.32
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & white & LABFORCE==2]) 
## 52613.12
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & white & LABFORCE==2])
## 66710.86
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & white & LABFORCE==2 & unmarried == 0])
## 81502.38
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & white & LABFORCE==2 & unmarried == 0]) 
## 160804
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & white & LABFORCE==2]) 
## 75949.85
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & white & LABFORCE==2]) 
## 138232


#When we examine the hispanic subgroup we see that they also make more when we incorporate the martial status 
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & Hispanic & LABFORCE==2 & unmarried == 0])
## 79255.1
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & Hispanic & LABFORCE==2 & unmarried == 0]) 
## 68492.11
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==1 & Hispanic & LABFORCE==2])
## 63600.45
mean(INCWAGE[DEGFIELD=="Fine Arts" & female==0 & Hispanic & LABFORCE==2])
## 49741.76
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & Hispanic & LABFORCE==2 & unmarried == 0])
## 66360.19
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & Hispanic & LABFORCE==2 & unmarried == 0])
## 126400.4
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==1 & Hispanic & LABFORCE==2])
## 62207.47
mean(INCWAGE[DEGFIELD=="Social Sciences" & female==0 & Hispanic & LABFORCE==2]) 
## 115180.2

#As a result we can say getting married can benefit individual to make more money and is a factor in convincing others to get married


