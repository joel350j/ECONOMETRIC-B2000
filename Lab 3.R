#Joel Andrade
#Lab 3
load("~/Desktop/R WORK/ecob2000_lecture1/acs2017_ny/acs2017_ny_data.RData")
attach(acs2017_ny)
summary(acs2017_ny) 
norm_varb <- function(X_in) {(X_in - mean(X_in, na.rm = TRUE))/sd(X_in, na.rm = TRUE)} 
female <- as.factor(female)
levels(female) <- c("male","female")
summary(female)
summary((female=1))
summary(!female)
detach(acs2017_ny)
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE >20)&(acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI +
                       4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
levels = c(1,2,3,4,5) 
labels= c("Bronx", "Manhattan", "Staten Island", "Brooklyn"," Queens")
norm_varb <- function(X_in) {(X_in - mean(X_in, na.rm = TRUE))/sd(X_in, na.rm = TRUE)}
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost) 
good_obs_data_use <- complete.cases(data_use_prelim,borough_f) 
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use) 
detach(dat_NYC)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1) 
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1) 
test_data <- subset(dat_use,(!select1)) 
cl_data <- y_use[select1]
true_data <- y_use[!select1] 
summary(cl_data)

prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
num_correct_labels <- sum(pred_borough == true_data)
correct_rate <- num_correct_labels/length(true_data)
print(c(indx,correct_rate))}

plot(train_data)
#In the test group we can see a negative value showing the relationship between population housing cost and its level of income. So it confirms the hypothesis that the majority can’t afford buying a place to live.
plot(test_data)
#The test group is showing the same characteristic as the trained group. Only a very tiny number within this population would live in their owned place and is reflected by that single dot.

pairs(train_data)
plot(cl_data)
#The best practice was to normalize the data and finally run the k-nn algorithms and compare against the simple means. 
#We can notice that income levels justify needs and confirm capabilities. Our sampled community cannot really afford living in owned places and seem to be more inclined to rent. 
#As we can see in the last plots, higher concentrations of renters are reflected in the Bronx and Brooklyn. The simplicity of K-nn algorithms facilitates the understanding of relationships between income levels, borough, level of education. 
#Our perception would be to guess that other factors could influence the data. However, better sampling could not be made to show these results.

