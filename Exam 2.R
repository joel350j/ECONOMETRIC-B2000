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




data_NHIS$insurance <- (data_NHIS$HINOTCOVE == "yes got insurance")
> 
> pick_use1 <- (data_NHIS$AGE <= 25)
> dat_use1 <- subset(data_NHIS, pick_use1)
> 
> d_educ <- data.frame(model.matrix(~ dat_use1$EDUC))
> summary(d_educ)
  X.Intercept. dat_use1.EDUCno.school dat_use1.EDUCless.than.hs
 Min.   :1     Min.   :0.0000000      Min.   :0.00000          
 1st Qu.:1     1st Qu.:0.0000000      1st Qu.:0.00000          
 Median :1     Median :0.0000000      Median :0.00000          
 Mean   :1     Mean   :0.0003821      Mean   :0.01312          
 3rd Qu.:1     3rd Qu.:0.0000000      3rd Qu.:0.00000          
 Max.   :1     Max.   :1.0000000      Max.   :1.00000          
 dat_use1.EDUC12th.grade.no.diploma dat_use1.EDUCHS.diploma dat_use1.EDUCGED  
 Min.   :0.00000                    Min.   :0.00000         Min.   :0.000000  
 1st Qu.:0.00000                    1st Qu.:0.00000         1st Qu.:0.000000  
 Median :0.00000                    Median :0.00000         Median :0.000000  
 Mean   :0.00866                    Mean   :0.07718         Mean   :0.004075  
 3rd Qu.:0.00000                    3rd Qu.:0.00000         3rd Qu.:0.000000  
 Max.   :1.00000                    Max.   :1.00000         Max.   :1.000000  
 dat_use1.EDUCsome.college dat_use1.EDUCassoc.deg.in.tech.or.occ
 Min.   :0.00000           Min.   :0.000000                     
 1st Qu.:0.00000           1st Qu.:0.000000                     
 Median :0.00000           Median :0.000000                     
 Mean   :0.08049           Mean   :0.006113                     
 3rd Qu.:0.00000           3rd Qu.:0.000000                     
 Max.   :1.00000           Max.   :1.000000                     
 dat_use1.EDUCassoc.deg.academic dat_use1.EDUCbachelors dat_use1.EDUCmasters
 Min.   :0.00000                 Min.   :0.00000        Min.   :0.000000    
 1st Qu.:0.00000                 1st Qu.:0.00000        1st Qu.:0.000000    
 Median :0.00000                 Median :0.00000        Median :0.000000    
 Mean   :0.01694                 Mean   :0.05069        Mean   :0.003693    
 3rd Qu.:0.00000                 3rd Qu.:0.00000        3rd Qu.:0.000000    
 Max.   :1.00000                 Max.   :1.00000        Max.   :1.000000    
 dat_use1.EDUCprofessional.degree dat_use1.EDUCdoctoral dat_use1.EDUCrefused
 Min.   :0.0000000                Min.   :0.0000000     Min.   :0           
 1st Qu.:0.0000000                1st Qu.:0.0000000     1st Qu.:0           
 Median :0.0000000                Median :0.0000000     Median :0           
 Mean   :0.0003821                Mean   :0.0003821     Mean   :0           
 3rd Qu.:0.0000000                3rd Qu.:0.0000000     3rd Qu.:0           
 Max.   :1.0000000                Max.   :1.0000000     Max.   :0           
 dat_use1.EDUCdont.know
 Min.   :0.0000000     
 1st Qu.:0.0000000     
 Median :0.0000000     
 Mean   :0.0005094     
 3rd Qu.:0.0000000     
 Max.   :1.0000000     
> levels(dat_use1$EDUC)
 [1] "NIU"                      "no school"                "less than hs"            
 [4] "12th grade no diploma"    "HS diploma"               "GED"                     
 [7] "some college"             "assoc deg in tech or occ" "assoc deg academic"      
[10] "bachelors"                "masters"                  "professional degree"     
[13] "doctoral"                 "refused"                  "dont know"               
> d_marstat <- data.frame(model.matrix(~ dat_use1$MARST))
> d_race <- data.frame(model.matrix(~ dat_use1$RACEA))
> d_hispanic <- data.frame(model.matrix(~ dat_use1$HISPETH))
> d_region <- data.frame(model.matrix(~ dat_use1$REGION))
> 
> d_insurance <- data.frame(model.matrix(~ dat_use1$HINOTCOVE))
> 
> dat_for_analysis_sub <- data.frame(
+   d_insurance[,2],
+   d_educ[!is.na(dat_use1$insurance),2:7],
+   d_marstat[!is.na(dat_use1$insurance),2:6],
+   d_race[!is.na(dat_use1$insurance),2:4],
+   d_hispanic[!is.na(dat_use1$insurance),2],
+   d_region[!is.na(dat_use1$insurance),2:4]) 
> 
> names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
> names(dat_for_analysis_sub)[1] <- "insurance"
> names(dat_for_analysis_sub)[17] <- "Hispanic"
> 
> summary(dat_for_analysis_sub)
   insurance       EDUCno.school       EDUCless.than.hs  EDUC12th.grade.no.diploma
 Min.   :0.00000   Min.   :0.0000000   Min.   :0.00000   Min.   :0.00000          
 1st Qu.:0.00000   1st Qu.:0.0000000   1st Qu.:0.00000   1st Qu.:0.00000          
 Median :0.00000   Median :0.0000000   Median :0.00000   Median :0.00000          
 Mean   :0.06852   Mean   :0.0003821   Mean   :0.01312   Mean   :0.00866          
 3rd Qu.:0.00000   3rd Qu.:0.0000000   3rd Qu.:0.00000   3rd Qu.:0.00000          
 Max.   :1.00000   Max.   :1.0000000   Max.   :1.00000   Max.   :1.00000          
 EDUCHS.diploma       EDUCGED         EDUCsome.college   MARSTMarried    
 Min.   :0.00000   Min.   :0.000000   Min.   :0.00000   Min.   :0.00000  
 1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000  
 Median :0.00000   Median :0.000000   Median :0.00000   Median :0.00000  
 Mean   :0.07718   Mean   :0.004075   Mean   :0.08049   Mean   :0.02458  
 3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000  
 Max.   :1.00000   Max.   :1.000000   Max.   :1.00000   Max.   :1.00000  
 MARSTMarried.spouse.not.there MARSTMarried.spouse.NA  MARSTWidowed MARSTDivorced     
 Min.   :0.000000              Min.   :0.0000000      Min.   :0     Min.   :0.000000  
 1st Qu.:0.000000              1st Qu.:0.0000000      1st Qu.:0     1st Qu.:0.000000  
 Median :0.000000              Median :0.0000000      Median :0     Median :0.000000  
 Mean   :0.001528              Mean   :0.0001274      Mean   :0     Mean   :0.001528  
 3rd Qu.:0.000000              3rd Qu.:0.0000000      3rd Qu.:0     3rd Qu.:0.000000  
 Max.   :1.000000              Max.   :1.0000000      Max.   :0     Max.   :1.000000  
   RACEABlack     RACEAAleut.Alaskan RACEAAmerican.Indian d_hispanic..is.na.insurance...2.
 Min.   :0.0000   Min.   :0.000000   Min.   :0.00000      Min.   :0.0000                  
 1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.00000      1st Qu.:0.0000                  
 Median :0.0000   Median :0.000000   Median :0.00000      Median :0.0000                  
 Mean   :0.1093   Mean   :0.008915   Mean   :0.01363      Mean   :0.1388                  
 3rd Qu.:0.0000   3rd Qu.:0.000000   3rd Qu.:0.00000      3rd Qu.:0.0000                  
 Max.   :1.0000   Max.   :1.000000   Max.   :1.00000      Max.   :1.0000                  
    Hispanic      REGIONSouth      REGIONWest    
 Min.   :0.000   Min.   :0.000   Min.   :0.0000  
 1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000  
 Median :0.000   Median :0.000   Median :0.0000  
 Mean   :0.221   Mean   :0.344   Mean   :0.2687  
 3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.:1.0000  
 Max.   :1.000   Max.   :1.000   Max.   :1.0000  
> 
> require("standardize")
> set.seed(654321)
> NN <- length(dat_for_analysis_sub$insurance)
> restrict_1 <- (runif(NN) < 0.1) 
> summary(restrict_1)
   Mode   FALSE    TRUE 
logical    7052     800 
> dat_train <- subset(dat_for_analysis_sub, restrict_1)
> dat_test <- subset(dat_for_analysis_sub, !restrict_1)
> 
> sobj <- standardize(insurance ~  EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
+                       MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + RACEAAmerican.Indian +
+                        Hispanic + REGIONSouth + REGIONWest + d_hispanic..is.na.insurance...2.
+                     , dat_train, family = binomial)
> 
> s_dat_test <- predict(sobj, dat_test)
> model_logit1 <- glm(insurance ~ EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
+                       MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + RACEAAmerican.Indian +
+                       Hispanic + REGIONSouth + REGIONWest + d_hispanic..is.na.insurance...2.,
+                     family = binomial, data = dat_for_analysis_sub)
> summary(model_logit1)

Call:
glm(formula = insurance ~ EDUCno.school + EDUCless.than.hs + 
    EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
    MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + 
    MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + 
    RACEAAmerican.Indian + Hispanic + REGIONSouth + REGIONWest + 
    d_hispanic..is.na.insurance...2., family = binomial, data = dat_for_analysis_sub)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5572  -0.3706  -0.2655  -0.2293   2.7570  

Coefficients: (1 not defined because of singularities)
                                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)                       -3.7780     0.1598 -23.636  < 2e-16 ***
EDUCno.school                      3.1483     1.2786   2.462 0.013803 *  
EDUCless.than.hs                   1.9937     0.2385   8.359  < 2e-16 ***
EDUC12th.grade.no.diploma          0.8331     0.3896   2.138 0.032484 *  
EDUCHS.diploma                     1.5514     0.1253  12.383  < 2e-16 ***
EDUCGED                            1.3404     0.5085   2.636 0.008385 ** 
EDUCsome.college                   0.9208     0.1438   6.403 1.52e-10 ***
MARSTMarried                       0.4898     0.2122   2.309 0.020958 *  
MARSTMarried.spouse.not.there    -12.9685   233.0301  -0.056 0.955619    
MARSTMarried.spouse.NA            15.5209   882.7434   0.018 0.985972    
MARSTWidowed                           NA         NA      NA       NA    
MARSTDivorced                      0.2133     0.8368   0.255 0.798828    
RACEABlack                         0.1601     0.1427   1.121 0.262108    
RACEAAleut.Alaskan                 1.1142     0.3277   3.400 0.000674 ***
RACEAAmerican.Indian               1.0529     0.2865   3.675 0.000238 ***
Hispanic                           0.2905     0.1897   1.531 0.125746    
REGIONSouth                        0.9992     0.1694   5.898 3.67e-09 ***
REGIONWest                         0.1528     0.1881   0.812 0.416571    
d_hispanic..is.na.insurance...2.   0.9812     0.1139   8.615  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 3922.7  on 7851  degrees of freedom
Residual deviance: 3508.5  on 7834  degrees of freedom
AIC: 3544.5

Number of Fisher Scoring iterations: 13

> #Using insurance as the dependent variable and if we focus on education and race as an independent factor we see that
> # RACEAAleut.Alaskan, RACEAAmerican.Indian, EDUCless.than.hs, EDUCHS.diploma, EDUCsome.college, tend to have insurance with an estimate of 1.1142, 1.0529, 0.9992, 1.9937, 1.5514, 0.9208 respectively. They have a level of significance of 0.001
> #because the p-value is smaller than the level of significance we reject the null that education and race does not have a role in insurance and 
> #(accept)/replace the alternative where race and education play a role in insurance.
> model_temp1 <- lm(insurance ~ EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + EDUCHS.diploma + EDUCGED + EDUCsome.college + 
+                     MARSTMarried + MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + RACEAAmerican.Indian +
+                     Hispanic + REGIONSouth + REGIONWest + d_hispanic..is.na.insurance...2., data = dat_for_analysis_sub)
> summary(model_temp1)

Call:
lm(formula = insurance ~ EDUCno.school + EDUCless.than.hs + EDUC12th.grade.no.diploma + 
    EDUCHS.diploma + EDUCGED + EDUCsome.college + MARSTMarried + 
    MARSTMarried.spouse.not.there + MARSTMarried.spouse.NA + 
    MARSTWidowed + MARSTDivorced + RACEABlack + RACEAAleut.Alaskan + 
    RACEAAmerican.Indian + Hispanic + REGIONSouth + REGIONWest + 
    d_hispanic..is.na.insurance...2., data = dat_for_analysis_sub)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.67942 -0.07638 -0.02917 -0.01329  0.98745 

Coefficients: (1 not defined because of singularities)
                                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)                       0.0132891  0.0069017   1.925  0.05420 .  
EDUCno.school                     0.5529754  0.1416079   3.905 9.50e-05 ***
EDUCless.than.hs                  0.2152030  0.0243077   8.853  < 2e-16 ***
EDUC12th.grade.no.diploma         0.0558154  0.0298271   1.871  0.06134 .  
EDUCHS.diploma                    0.1410700  0.0106375  13.262  < 2e-16 ***
EDUCGED                           0.1077112  0.0433844   2.483  0.01306 *  
EDUCsome.college                  0.0610950  0.0102363   5.968 2.50e-09 ***
MARSTMarried                      0.0582278  0.0182730   3.187  0.00145 ** 
MARSTMarried.spouse.not.there    -0.1415042  0.0708363  -1.998  0.04579 *  
MARSTMarried.spouse.NA            0.7622877  0.2446682   3.116  0.00184 ** 
MARSTWidowed                             NA         NA      NA       NA    
MARSTDivorced                     0.0374142  0.0707672   0.529  0.59703    
RACEABlack                        0.0081574  0.0091178   0.895  0.37099    
RACEAAleut.Alaskan                0.1165384  0.0293814   3.966 7.36e-05 ***
RACEAAmerican.Indian              0.0928424  0.0238252   3.897 9.83e-05 ***
Hispanic                          0.0077249  0.0089784   0.860  0.38960    
REGIONSouth                       0.0549319  0.0083536   6.576 5.15e-11 ***
REGIONWest                       -0.0007414  0.0088697  -0.084  0.93338    
d_hispanic..is.na.insurance...2.  0.0756283  0.0083115   9.099  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2443 on 7834 degrees of freedom
Multiple R-squared:  0.0671,	Adjusted R-squared:  0.06508 
F-statistic: 33.15 on 17 and 7834 DF,  p-value: < 2.2e-16

> #Using insurance as the dependent variable and if we focus on education and race as an independent factor we see that
> # RACEAAleut.Alaskan, RACEAAmerican.Indian, EDUCless.than.hs, EDUCHS.diploma, EDUCsome.college, tend to have insurance with an estimate of 0.1165384, 0.0928424, 0.2152030, 0.1410700, 0.0610950, respectively. They have a level of significance of 0.001
> #because the p-value is smaller than the level of significance we reject the null that education and race does not have a role in insurance and 
> #(accept)/replace the alternative where race and education play a role in insurance.
> #When we compare the logit model and the OLS we see they oth come to same conclusion being that insurance is influenced by race and education.
> 



