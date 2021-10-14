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







#CONSOLE OUTPUT:
xtabs(~ EEDUC + RECVDVACC + REGION)
, , REGION = Northeast

              RECVDVACC
EEDUC            NA yes got vaxx no did not get vaxx
  less than hs    0           31                  16
  some hs         1          107                  19
  HS diploma     27         1028                 190
  some coll      20         1576                 222
  assoc deg      15          928                 120
  bach deg       35         2901                 162
  adv deg        20         2961                  99

, , REGION = South

              RECVDVACC
EEDUC            NA yes got vaxx no did not get vaxx
  less than hs    0          103                  48
  some hs         5          211                 107
  HS diploma     32         2052                 652
  some coll      62         3868                 858
  assoc deg      30         1903                 387
  bach deg       63         5780                 546
  adv deg        76         5582                 315

, , REGION = Midwest

              RECVDVACC
EEDUC            NA yes got vaxx no did not get vaxx
  less than hs    4           41                  20
  some hs         5          114                  56
  HS diploma     29         1326                 370
  some coll      35         2306                 511
  assoc deg      29         1352                 269
  bach deg       65         3598                 343
  adv deg        41         2977                 160

, , REGION = West

              RECVDVACC
EEDUC            NA yes got vaxx no did not get vaxx
  less than hs    2          115                  31
  some hs         4          220                  87
  HS diploma     25         1691                 435
  some coll      61         4272                 805
  assoc deg      36         2083                 356
  bach deg       75         5993                 514
  adv deg        54         5207                 239

> summary(Household_Pulse_data)
        RHISPANIC       RRACE                EEDUC               MS       
 Not Hispanic:62660   White:56938   less than hs:  411   NA       :  881  
 Hispanic    : 6454   Black: 5412   some hs     :  936   married  :40036  
                      Asian: 3561   HS diploma  : 7857   widowed  : 3872  
                      Other: 3203   some coll   :14596   divorced :10310  
                                    assoc deg   : 7508   separated: 1214  
                                    bach deg    :20075   never    :12801  
                                    adv deg     :17731                    
 EGENID_BIRTH       GENID_DESCRIBE       SEXUAL_ORIENTATION
 male  :27592   NA         : 1131   NA            : 1506   
 female:41522   male       :26796   gay or lesbian: 2343   
                female     :40263   straight      :61238   
                transgender:  202   bisexual      : 2288   
                other      :  722   something else:  871   
                                    dont know     :  868   
                                                           
                      KIDS_LT5Y                         KIDS_5_11Y   
 NA                        :62342   NA                       :58467  
 Yes children under 5 in HH: 6772   Yes children 5 - 11 in HH:10647  
                                                                     
                                                                     
                                                                     
                                                                     
                                                                     
                     KIDS_12_17Y                                 ENROLLNONE   
 NA                        :58046   NA                                :64285  
 Yes children 12 - 17 in HH:11068   children not in any type of school: 4829  
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
               RECVDVACC                          DOSESRV     
 NA                 :  851   NA                       : 9105  
 yes got vaxx       :60326   yes got all doses        :57762  
 no did not get vaxx: 7937   yes plan to get all doses: 1993  
                             no will not get all doses:  254  
                                                              
                                                              
                                                              
                     GETVACRV                                   KIDDOSES    
 NA                      :61159   NA                                :58318  
 definitely will get vaxx:  609   Yes kids got or will get all doses: 7135  
 probably will get vaxx  :  731   no kids did not or will not       : 3661  
 unsure about vaxx       : 1584                                             
 probably not            : 1599                                             
 definitely not          : 3432                                             
                                                                            
                    KIDGETVAC                          HADCOVID    
 NA                      :65384   NA                       : 1363  
 definitely will get vaxx:  487   yes doctor told had covid: 9122  
 probably will get vaxx  :  439   no did not               :58221  
 unsure about vaxx       :  736   not sure                 :  408  
 probably not            :  593                                    
 definitely not          : 1036                                    
 dont know yet           :  439                                    
                  WRKLOSSRV                              ANYWORK     
 NA                    : 1961   NA                           : 2135  
 yes recent HH job loss: 8058   yes employment in last 7 days:39237  
 no recent HH job loss :59095   no employment in last 7 days :27742  
                                                                     
                                                                     
                                                                     
                                                                     
                KINDWORK                RSNNOWRKRV   
 NA                 :30540   NA              :42659  
 work for govt      : 6378   retired         :15024  
 work for private co:21370   other           : 4027  
 work for nonprofit : 5055   sick or disabled: 1451  
 self employed      : 4966   caring for kids : 1329  
 work in family biz :  805   laid off        : 1164  
                             (Other)         : 3460  
                                      CHLDCARE    
 NA                                       :58419  
 yes impacts to childcare because pandemic: 2566  
 no                                       : 8129  
                                                  
                                                  
                                                  
                                                  
                          CURFOODSUF   
 NA                            : 6770  
 had enough food               :49234  
 had enough but not what wanted: 9947  
 sometimes not enough food     : 2486  
 often not enough food         :  677  
                                       
                                       
                                               CHILDFOOD    
 NA                                                 :64258  
 often kids not eating enough because couldnt afford:  271  
 sometimes kids not eating enough                   : 1191  
 kids got enough food                               : 3394  
                                                            
                                                            
                                                            
                                            ANXIOUS     
 NA                                             : 7946  
 no anxiety over past 2 wks                     :26611  
 several days anxiety over past 2 wks           :19794  
 more than half the days anxiety over past 2 wks: 6140  
 nearly every day anxiety                       : 8623  
                                                        
                                                        
                                             WORRY      
 NA                                             : 8016  
 no worry over past 2 wks                       :31876  
 several days worried over past 2 wks           :17936  
 more than half the days worried over past 2 wks: 4979  
 nearly every day worry                         : 6307  
                                                        
                                                        
                           TENURE     
 NA                           :11103  
 housing owned free and clear :16738  
 housing owned with mortgage  :28016  
 housing rented               :12579  
 housing occupied without rent:  678  
                                      
                                      
                                LIVQTRRV                RENTCUR     
 live in detached 1 family          :41348   NA             :56572  
 NA                                 :11336   current on rent:11239  
 live in bldg w 5+ apts             : 6731   behind on rent : 1303  
 live in 1 family attached to others: 4628                          
 live in mobile home                : 1781                          
 live in building with 3-4 apts     : 1737                          
 (Other)                            : 1553                          
                MORTCUR                                             EVICT      
 NA                 :41200   NA                                        :67859  
 current on mortgage:26462   very likely evicted in next 2 months      :  207  
 behind on mortgage : 1452   somewhat likely evicted in next 2 months  :  377  
                             not very likely evicted in next 2 months  :  345  
                             not at all likely evicted in next 2 months:  326  
                                                                               
                                                                               
                                          FORCLOSE               EST_ST     
 NA                                           :67695   California   : 5359  
 very likely forclosed in next 2 months       :   65   Texas        : 3766  
 somewhat likely forclosed in next 2 months   :  218   Florida      : 2728  
 not very likely forclosed in next 2 months   :  474   Washington   : 2634  
 not at all forclosed evicted in next 2 months:  662   Massachusetts: 1965  
                                                       Oregon       : 1934  
                                                       (Other)      :50728  
                   PRIVHLTH                      PUBHLTH            REGION     
 has private health ins:46869   has public health ins:23346   Northeast:10478  
 no private health ins :11275   no public health ins :33381   South    :22680  
 NA                    :10970   NA                   :12387   Midwest  :13651  
                                                              West     :22305  
                                                                               
                                                                               
                                                                               
                     INCOME      Num_kids_Pub_School Num_kids_Priv_School
 NA                     :20335   Min.   :0.00        Min.   :0.00        
 HH income $75 - 99.9   :10117   1st Qu.:1.00        1st Qu.:0.00        
 HH income $35k - 49.9  : 9330   Median :2.00        Median :1.00        
 HH income $50k - 74.9  : 7830   Mean   :1.71        Mean   :1.03        
 HH income $150 - 199   : 6117   3rd Qu.:2.00        3rd Qu.:2.00        
 HH income $25k - $34.9k: 5805   Max.   :4.00        Max.   :2.00        
 (Other)                : 9580   NA's   :55108       NA's   :66430       
 Num_kids_homeschool        Works_onsite            works_remote  
 Min.   :0.00        NA           : 6350   NA             : 8022  
 1st Qu.:0.00        worked onsite:34918   worked remotely:22863  
 Median :1.00        no           :27846   no             :38229  
 Mean   :0.87                                                     
 3rd Qu.:2.00                                                     
 Max.   :2.00                                                     
 NA's   :67421                                                    
          Shop_in_store                   eat_in_restaurant
 NA              : 6873   NA                       : 7217  
 shopped in store:53576   eat at restaurant indoors:32405  
 no              : 8665   no                       :29492  
                                                           
                                                           
                                                           
                                                           
> #1
> REGIONS_ANSWER1<-lm(as.numeric(RECVDVACC)~ EEDUC* REGION)
> summary(REGIONS_ANSWER1)

Call:
lm(formula = as.numeric(RECVDVACC) ~ EEDUC * REGION)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.31579 -0.14480 -0.06940 -0.03744  0.97435 

Coefficients:
                              Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    2.34043    0.04896  47.806  < 2e-16 ***
EEDUCsome hs                  -0.19869    0.05730  -3.467 0.000526 ***
EEDUCHS diploma               -0.20950    0.04987  -4.201 2.66e-05 ***
EEDUCsome coll                -0.22931    0.04959  -4.625 3.76e-06 ***
EEDUCassoc deg                -0.24165    0.05003  -4.830 1.37e-06 ***
EEDUCbach deg                 -0.29943    0.04933  -6.070 1.28e-09 ***
EEDUCadv deg                  -0.31478    0.04933  -6.381 1.77e-10 ***
REGIONSouth                   -0.02254    0.05606  -0.402 0.687575    
REGIONMidwest                 -0.09427    0.06426  -1.467 0.142393    
REGIONWest                    -0.14448    0.05620  -2.571 0.010142 *  
EEDUCsome hs:REGIONSouth       0.19660    0.06617   2.971 0.002968 ** 
EEDUCHS diploma:REGIONSouth    0.11823    0.05722   2.066 0.038820 *  
EEDUCsome coll:REGIONSouth     0.07768    0.05682   1.367 0.171560    
EEDUCassoc deg:REGIONSouth     0.07765    0.05742   1.352 0.176312    
EEDUCbach deg:REGIONSouth      0.05715    0.05654   1.011 0.312127    
EEDUCadv deg:REGIONSouth       0.03691    0.05655   0.653 0.513988    
EEDUCsome hs:REGIONMidwest     0.24397    0.07524   3.243 0.001185 ** 
EEDUCHS diploma:REGIONMidwest  0.16103    0.06546   2.460 0.013904 *  
EEDUCsome coll:REGIONMidwest   0.15006    0.06505   2.307 0.021062 *  
EEDUCassoc deg:REGIONMidwest   0.14095    0.06561   2.148 0.031682 *  
EEDUCbach deg:REGIONMidwest    0.12267    0.06476   1.894 0.058205 .  
EEDUCadv deg:REGIONMidwest     0.10607    0.06482   1.636 0.101782    
EEDUCsome hs:REGIONWest        0.26963    0.06639   4.062 4.88e-05 ***
EEDUCHS diploma:REGIONWest     0.20416    0.05745   3.554 0.000380 ***
EEDUCsome coll:REGIONWest      0.17817    0.05694   3.129 0.001753 ** 
EEDUCassoc deg:REGIONWest      0.17500    0.05753   3.042 0.002351 ** 
EEDUCbach deg:REGIONWest       0.17018    0.05667   3.003 0.002673 ** 
EEDUCadv deg:REGIONWest        0.15247    0.05670   2.689 0.007169 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3356 on 69086 degrees of freedom
Multiple R-squared:  0.03462,	Adjusted R-squared:  0.03424 
F-statistic: 91.77 on 27 and 69086 DF,  p-value: < 2.2e-16

> #If we look at the western regionwe see that individuals with an highschool degree the significance is high if they receive an highschool degree and not. if we look at the higher levels of educations it is still significant for all of west.
> #If we look at the midwest we see the significance is higher for highschool than the other higher level of educations
> #when looking at the south some highschool and highschool diploma is significant but not other degrees.
> #when we compare the different regions we see that the west is more likely to be vaccinated.
> REGIONS_ANSWER2<-lm(as.numeric(RECVDVACC)~ EEDUC* GENID_DESCRIBE)
> summary(REGIONS_ANSWER2)

Call:
lm(formula = as.numeric(RECVDVACC) ~ EEDUC * GENID_DESCRIBE)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.45455 -0.15471 -0.07999 -0.04340  1.50806 

Coefficients:
                                          Estimate Std. Error t value Pr(>|t|)
(Intercept)                                2.07143    0.08802  23.533  < 2e-16
EEDUCsome hs                              -0.39143    0.10994  -3.560 0.000371
EEDUCHS diploma                           -0.42208    0.09194  -4.591 4.42e-06
EEDUCsome coll                            -0.39510    0.09095  -4.344 1.40e-05
EEDUCassoc deg                            -0.57949    0.09286  -6.241 4.38e-10
EEDUCbach deg                             -0.51030    0.08993  -5.674 1.40e-08
EEDUCadv deg                              -0.49157    0.09014  -5.454 4.95e-08
GENID_DESCRIBEmale                         0.11844    0.09184   1.290 0.197160
GENID_DESCRIBEfemale                       0.23249    0.09099   2.555 0.010619
GENID_DESCRIBEtransgender                  0.38312    0.13270   2.887 0.003889
GENID_DESCRIBEother                        0.38690    0.11076   3.493 0.000478
EEDUCsome hs:GENID_DESCRIBEmale            0.47068    0.11428   4.119 3.81e-05
EEDUCHS diploma:GENID_DESCRIBEmale         0.44739    0.09580   4.670 3.01e-06
EEDUCsome coll:GENID_DESCRIBEmale          0.35893    0.09475   3.788 0.000152
EEDUCassoc deg:GENID_DESCRIBEmale          0.52039    0.09671   5.381 7.44e-08
EEDUCbach deg:GENID_DESCRIBEmale           0.38564    0.09374   4.114 3.90e-05
EEDUCadv deg:GENID_DESCRIBEmale            0.33904    0.09395   3.609 0.000308
EEDUCsome hs:GENID_DESCRIBEfemale          0.39444    0.11328   3.482 0.000498
EEDUCHS diploma:GENID_DESCRIBEfemale       0.31799    0.09490   3.351 0.000807
EEDUCsome coll:GENID_DESCRIBEfemale        0.25488    0.09389   2.715 0.006638
EEDUCassoc deg:GENID_DESCRIBEfemale        0.43028    0.09580   4.492 7.08e-06
EEDUCbach deg:GENID_DESCRIBEfemale         0.28637    0.09289   3.083 0.002051
EEDUCadv deg:GENID_DESCRIBEfemale          0.23105    0.09310   2.482 0.013076
EEDUCsome hs:GENID_DESCRIBEtransgender     0.07974    0.19350   0.412 0.680277
EEDUCHS diploma:GENID_DESCRIBEtransgender  0.18182    0.14895   1.221 0.222228
EEDUCsome coll:GENID_DESCRIBEtransgender   0.03489    0.14205   0.246 0.805962
EEDUCassoc deg:GENID_DESCRIBEtransgender   0.32495    0.16036   2.026 0.042730
EEDUCbach deg:GENID_DESCRIBEtransgender    0.07749    0.14250   0.544 0.586577
EEDUCadv deg:GENID_DESCRIBEtransgender     0.27512    0.14342   1.918 0.055075
EEDUCsome hs:GENID_DESCRIBEother           0.13310    0.14842   0.897 0.369872
EEDUCHS diploma:GENID_DESCRIBEother        0.21117    0.11870   1.779 0.075249
EEDUCsome coll:GENID_DESCRIBEother         0.03052    0.11606   0.263 0.792595
EEDUCassoc deg:GENID_DESCRIBEother         0.28208    0.11995   2.352 0.018698
EEDUCbach deg:GENID_DESCRIBEother          0.19052    0.11516   1.654 0.098040
EEDUCadv deg:GENID_DESCRIBEother           0.20585    0.11528   1.786 0.074159
                                             
(Intercept)                               ***
EEDUCsome hs                              ***
EEDUCHS diploma                           ***
EEDUCsome coll                            ***
EEDUCassoc deg                            ***
EEDUCbach deg                             ***
EEDUCadv deg                              ***
GENID_DESCRIBEmale                           
GENID_DESCRIBEfemale                      *  
GENID_DESCRIBEtransgender                 ** 
GENID_DESCRIBEother                       ***
EEDUCsome hs:GENID_DESCRIBEmale           ***
EEDUCHS diploma:GENID_DESCRIBEmale        ***
EEDUCsome coll:GENID_DESCRIBEmale         ***
EEDUCassoc deg:GENID_DESCRIBEmale         ***
EEDUCbach deg:GENID_DESCRIBEmale          ***
EEDUCadv deg:GENID_DESCRIBEmale           ***
EEDUCsome hs:GENID_DESCRIBEfemale         ***
EEDUCHS diploma:GENID_DESCRIBEfemale      ***
EEDUCsome coll:GENID_DESCRIBEfemale       ** 
EEDUCassoc deg:GENID_DESCRIBEfemale       ***
EEDUCbach deg:GENID_DESCRIBEfemale        ** 
EEDUCadv deg:GENID_DESCRIBEfemale         *  
EEDUCsome hs:GENID_DESCRIBEtransgender       
EEDUCHS diploma:GENID_DESCRIBEtransgender    
EEDUCsome coll:GENID_DESCRIBEtransgender     
EEDUCassoc deg:GENID_DESCRIBEtransgender  *  
EEDUCbach deg:GENID_DESCRIBEtransgender      
EEDUCadv deg:GENID_DESCRIBEtransgender    .  
EEDUCsome hs:GENID_DESCRIBEother             
EEDUCHS diploma:GENID_DESCRIBEother       .  
EEDUCsome coll:GENID_DESCRIBEother           
EEDUCassoc deg:GENID_DESCRIBEother        *  
EEDUCbach deg:GENID_DESCRIBEother         .  
EEDUCadv deg:GENID_DESCRIBEother          .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3293 on 69079 degrees of freedom
Multiple R-squared:  0.07051,	Adjusted R-squared:  0.07006 
F-statistic: 154.1 on 34 and 69079 DF,  p-value: < 2.2e-16

> #When we look at gender identity for individuals we see male and female being more significant than other identities like transgender and other. Male and female are more likely to get vaccinated than transgender and other.
> ##3
> #compared mortgage and race to see how race plays a factor in property ownership
> EEDUC1 <- as.numeric( EEDUC)
> RACE <- RRACE
> regression <- lm(EEDUC1 ~ RACE)
> summary(regression)

Call:
lm(formula = EEDUC1 ~ RACE)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.7832 -1.3159  0.6841  1.2168  2.0165 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.315887   0.006053  878.28   <2e-16 ***
RACEBlack   -0.306094   0.020544  -14.90   <2e-16 ***
RACEAsian    0.467320   0.024948   18.73   <2e-16 ***
RACEOther   -0.332434   0.026227  -12.68   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.444 on 69110 degrees of freedom
Multiple R-squared:  0.01113,	Adjusted R-squared:  0.01109 
F-statistic: 259.3 on 3 and 69110 DF,  p-value: < 2.2e-16

> 
> #Using a k-nn classifier, can you find relevant information to predict an interesting outcome? How good is the classifier? Discuss.
> MORTGAGE <- MORTCUR
> MORTGAGE <- as.numeric(MORTCUR)
> RACE <- as.numeric(RRACE)
> INDX1 <- factor((MORTGAGE + 2*RACE), levels=c(1,2), labels = c("MORTGAGE" , "RACE"))
> 
> 
> indx <- factor(RRACE)
> norm_varb <- function(X_in) {
+   (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
+ }
> 
> race <- as.numeric(RRACE)
> norm_race <- norm_varb(race)
> 
> data_use_prelim <- data.frame(norm_race)
> good_obs_data_use <- complete.cases(data_use_prelim,INDX1)
> dat_use <- subset(data_use_prelim,good_obs_data_use)
> y_use <- subset(INDX1,good_obs_data_use)
> set.seed(12345)
> NN_obs <- sum(good_obs_data_use == 1)
> select1 <- (runif(NN_obs) < 0.8)
> train_data <- subset(Household_Pulse_data,select1)
> test_data <- subset(Household_Pulse_data,(!select1))
> cl_data <- y_use[select1]
> true_data <- y_use[!select1]
> summary(cl_data)
MORTGAGE     RACE 
       0        0 
> prop.table(summary(cl_data))
MORTGAGE     RACE 
     NaN      NaN 
> 
> summary(train_data)
        RHISPANIC   RRACE            EEDUC           MS    EGENID_BIRTH
 Not Hispanic:0   White:0   less than hs:0   NA       :0   male  :0    
 Hispanic    :0   Black:0   some hs     :0   married  :0   female:0    
                  Asian:0   HS diploma  :0   widowed  :0               
                  Other:0   some coll   :0   divorced :0               
                            assoc deg   :0   separated:0               
                            bach deg    :0   never    :0               
                            adv deg     :0                             
     GENID_DESCRIBE      SEXUAL_ORIENTATION                      KIDS_LT5Y
 NA         :0      NA            :0        NA                        :0  
 male       :0      gay or lesbian:0        Yes children under 5 in HH:0  
 female     :0      straight      :0                                      
 transgender:0      bisexual      :0                                      
 other      :0      something else:0                                      
                    dont know     :0                                      
                                                                          
                     KIDS_5_11Y                     KIDS_12_17Y
 NA                       :0    NA                        :0   
 Yes children 5 - 11 in HH:0    Yes children 12 - 17 in HH:0   
                                                               
                                                               
                                                               
                                                               
                                                               
                              ENROLLNONE               RECVDVACC
 NA                                :0    NA                 :0  
 children not in any type of school:0    yes got vaxx       :0  
                                         no did not get vaxx:0  
                                                                
                                                                
                                                                
                                                                
                      DOSESRV                      GETVACRV
 NA                       :0   NA                      :0  
 yes got all doses        :0   definitely will get vaxx:0  
 yes plan to get all doses:0   probably will get vaxx  :0  
 no will not get all doses:0   unsure about vaxx       :0  
                               probably not            :0  
                               definitely not          :0  
                                                           
                               KIDDOSES                    KIDGETVAC
 NA                                :0   NA                      :0  
 Yes kids got or will get all doses:0   definitely will get vaxx:0  
 no kids did not or will not       :0   probably will get vaxx  :0  
                                        unsure about vaxx       :0  
                                        probably not            :0  
                                        definitely not          :0  
                                        dont know yet           :0  
                      HADCOVID                  WRKLOSSRV
 NA                       :0   NA                    :0  
 yes doctor told had covid:0   yes recent HH job loss:0  
 no did not               :0   no recent HH job loss :0  
 not sure                 :0                             
                                                         
                                                         
                                                         
                          ANYWORK                 KINDWORK
 NA                           :0   NA                 :0  
 yes employment in last 7 days:0   work for govt      :0  
 no employment in last 7 days :0   work for private co:0  
                                   work for nonprofit :0  
                                   self employed      :0  
                                   work in family biz :0  
                                                          
                                          RSNNOWRKRV
 NA                                            :0   
 did not want                                  :0   
 am/was sick w covid or caring for sick w covid:0   
 caring for kids                               :0   
 caring for elderly                            :0   
 concerned about spreading                     :0   
 (Other)                                       :0   
                                      CHLDCARE
 NA                                       :0  
 yes impacts to childcare because pandemic:0  
 no                                       :0  
                                              
                                              
                                              
                                              
                          CURFOODSUF
 NA                            :0   
 had enough food               :0   
 had enough but not what wanted:0   
 sometimes not enough food     :0   
 often not enough food         :0   
                                    
                                    
                                               CHILDFOOD
 NA                                                 :0  
 often kids not eating enough because couldnt afford:0  
 sometimes kids not eating enough                   :0  
 kids got enough food                               :0  
                                                        
                                                        
                                                        
                                            ANXIOUS 
 NA                                             :0  
 no anxiety over past 2 wks                     :0  
 several days anxiety over past 2 wks           :0  
 more than half the days anxiety over past 2 wks:0  
 nearly every day anxiety                       :0  
                                                    
                                                    
                                             WORRY  
 NA                                             :0  
 no worry over past 2 wks                       :0  
 several days worried over past 2 wks           :0  
 more than half the days worried over past 2 wks:0  
 nearly every day worry                         :0  
                                                    
                                                    
                           TENURE                                 LIVQTRRV
 NA                           :0   NA                                 :0  
 housing owned free and clear :0   live in mobile home                :0  
 housing owned with mortgage  :0   live in detached 1 family          :0  
 housing rented               :0   live in 1 family attached to others:0  
 housing occupied without rent:0   live in bldg w 2 apartments        :0  
                                   live in building with 3-4 apts     :0  
                                   (Other)                            :0  
            RENTCUR                 MORTCUR 
 NA             :0   NA                 :0  
 current on rent:0   current on mortgage:0  
 behind on rent :0   behind on mortgage :0  
                                            
                                            
                                            
                                            
                                        EVICT  
 NA                                        :0  
 very likely evicted in next 2 months      :0  
 somewhat likely evicted in next 2 months  :0  
 not very likely evicted in next 2 months  :0  
 not at all likely evicted in next 2 months:0  
                                               
                                               
                                          FORCLOSE        EST_ST 
 NA                                           :0   Alabama   :0  
 very likely forclosed in next 2 months       :0   Alaska    :0  
 somewhat likely forclosed in next 2 months   :0   Arizona   :0  
 not very likely forclosed in next 2 months   :0   Arkansas  :0  
 not at all forclosed evicted in next 2 months:0   California:0  
                                                   Colorado  :0  
                                                   (Other)   :0  
                   PRIVHLTH                  PUBHLTH        REGION 
 has private health ins:0   has public health ins:0   Northeast:0  
 no private health ins :0   no public health ins :0   South    :0  
 NA                    :0   NA                   :0   Midwest  :0  
                                                      West     :0  
                                                                   
                                                                   
                                                                   
                      INCOME  Num_kids_Pub_School Num_kids_Priv_School
 NA                      :0   Min.   : NA         Min.   : NA         
 HH income less than $25k:0   1st Qu.: NA         1st Qu.: NA         
 HH income $25k - $34.9k :0   Median : NA         Median : NA         
 HH income $35k - 49.9   :0   Mean   :NaN         Mean   :NaN         
 HH income $50k - 74.9   :0   3rd Qu.: NA         3rd Qu.: NA         
 HH income $75 - 99.9    :0   Max.   : NA         Max.   : NA         
 (Other)                 :0                                           
 Num_kids_homeschool        Works_onsite          works_remote
 Min.   : NA         NA           :0     NA             :0    
 1st Qu.: NA         worked onsite:0     worked remotely:0    
 Median : NA         no           :0     no             :0    
 Mean   :NaN                                                  
 3rd Qu.: NA                                                  
 Max.   : NA                                                  
                                                              
          Shop_in_store                 eat_in_restaurant
 NA              :0     NA                       :0      
 shopped in store:0     eat at restaurant indoors:0      
 no              :0     no                       :0      
                                                         
                                                         
                                                         
                                                         
> 
> 
> for (indx in seq(1, 9, by= 2)) {
+   pred_RACE <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+   num_correct_labels <- sum(pred_RACE == true_data)
+   correct_rate <- num_correct_labels/length(true_data)
+   print(c(indx,correct_rate))
+ }
Error in knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE,  : 
  could not find function "knn"
> #The knn did not work but if it were to work would have seen mortgage and race being influencial 
> 
> #Can you explain some other interesting information about this data? Some interesting crosstabs? Maybe regressions? Impress me.
> summary(EGENID_BIRTH)
  male female 
 27592  41522 
> GEN_BIRTH <- as.numeric(EGENID_BIRTH)
> SEX_ORT <- SEXUAL_ORIENTATION
> regression <- lm(GEN_BIRTH ~ SEX_ORT)
> summary(regression)

Call:
lm(formula = GEN_BIRTH ~ SEX_ORT)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.7671 -0.6015  0.3985  0.3985  0.6035 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
(Intercept)            1.61222    0.01256 128.402  < 2e-16 ***
SEX_ORTgay or lesbian -0.21572    0.01609 -13.404  < 2e-16 ***
SEX_ORTstraight       -0.01073    0.01271  -0.844  0.39860    
SEX_ORTbisexual        0.15483    0.01617   9.576  < 2e-16 ***
SEX_ORTsomething else  0.07091    0.02074   3.418  0.00063 ***
SEX_ORTdont know      -0.05116    0.02077  -2.464  0.01376 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4873 on 69108 degrees of freedom
Multiple R-squared:  0.01017,	Adjusted R-squared:  0.01009 
F-statistic:   142 on 5 and 69108 DF,  p-value: < 2.2e-16

> #when we compare sexual orientation and birth we see a significance more with gay and lesbian, bisexual, and something else
> #All of the work on this exam is my own, answered honestly as rules state.
> #Name:JOEL ANDRADE
> #Date:OCTOBER 14 2021
