#HW1 
## AUTHOR: JOEL ANDRADE

##1 JOEL ANDRADE, KOUAKOU ALLAH
##3 Random Dice Rolls: 1,4,1,2,6,1,5,3,6,3,5,4,2,6,3,6,5,2,6,5
##Probabilities- 1:3/20, 2:3/20, 3:3/20, 4:2/20, 5:4/20, 6:5/20
##Simulation
Dice_roll <- sample(x= 1:6, size=20, replace= TRUE)
plot(table(Dice_roll), xlab = 'Dice number', ylab = 'Frequency', main = '20 Rolls for 2 Fair Dice')
sum(Dice_roll == 6)
##Altered dice by scrapping the sides of the number 6 in the dice
## Altered dice roll: 6,4,2,6,1,5,6,4,5,6,3,1,6,1,3,6,2,5,3,6
##Probabilities- 1:3/20, 2:2/20, 3:3/20, 4:2/20, 5:3/20, 6:7/20

#4
x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
#attach(example1)
fm <- lm(y ~ x)
summary(fm)
lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))
detach()
load("acs2017_ny_data.RData")
#glimpse(acs2017_ny) try this later
acs2017_ny[1:10,1:7]
attach(acs2017_ny)
summary(acs2017_ny)
print(NN_obs <- length(AGE))
summary(AGE[female == 1])
summary(AGE[!female])
# here i want to find average ages of men and women
mean(AGE[female == 1])
sd(AGE[female == 1])
mean(AGE[!female])
sd(AGE[!female])

##Tell me something else interesting, that you learned from the data, for example about educational attainments in different neighborhoods in the city. Are there surprises for you?

acs2017_ny[1:10,1:7]
#female's have a higher education compared to males in New York
mean(educ_advdeg[female == 1])
mean(educ_advdeg[female == 0])
#Higher degree females live in manhattan 
mean(educ_advdeg[(in_Bronx)&(female == 1)])
mean(educ_advdeg[(in_Manhattan)&(female == 1)])
mean(educ_advdeg[(in_StatenI)&(female == 1)])
mean(educ_advdeg[(in_Brooklyn)&(female == 1)])
mean(educ_advdeg[(in_Queens)&(female == 1)])

#Higher degree males live in Manhattan 
mean(educ_advdeg[(in_Bronx)&(female == 0)])
mean(educ_advdeg[(in_Manhattan)&(female == 0)])
mean(educ_advdeg[(in_StatenI)&(female == 0)])
mean(educ_advdeg[(in_Brooklyn)&(female == 0)])
mean(educ_advdeg[(in_Queens)&(female == 0)])

#However more females are below the poverty line comapares to men 
#giving me the conclusion that although there are more females in below poverty more and more are closing the gap with higher levels of education
mean(below_povertyline[female == 1])
mean(below_povertyline[female == 0])


