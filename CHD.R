##
##  CHD Logistic Example
##
## idx_remove = apply(dat[, 2:7], 1, function(x){ any(x == 0)})
## data_use = dat[-idx_remove,]

setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW8/MOD8")

rm(list = ls()) #### delete all variables

CHD_data = read.csv("CHD.csv",stringsAsFactors = TRUE) #turn "yes" or "no" into a factor "1 or 0"

names(CHD_data)
attach(CHD_data)   ## the moves the chd variables into the global environment


chd_numeric = as.numeric(CHD_data$chd)-1

################ side-by-side box plot (slide 3)

boxplot(age ~ chd)

################ scatterplot (slide 4 and 5)

library(ggplot2)

plot(age,chd_numeric,pch=19,cex=.5,xlab="Age",ylab="CHD")

plot(jitter(age,amount=.5),jitter(chd_numeric,amount=.1),
     pch=19,cex=.5,xlab="Age",ylab="CHD")


ggplot(CHD_data,aes(x=age,y=as.numeric(chd)-1)) + geom_jitter(width=.5,height=.1) + 
  geom_smooth()

scatter.smooth(age,chd_numeric,xlab="age",ylab="CHD",pch=19)

ggplot(CHD_data,aes(x=age,y=chd_numeric)) + 
  geom_smooth()+ 
  geom_jitter(width=.5,height=.1)

scatter.smooth(age,chd_numeric,xlab="age",ylab="CHD",pch=19)


ggplot(CHD_data,aes(x=cigs,y=chd_numeric)) + 
  geom_smooth()+ 
  geom_jitter(width=.5,height=.1)

##### ##### ##### ##### ##### 
##### empirical log-odds
##### ##### ##### ##### ##### 

age_prob = tapply(chd_numeric,age,mean)
n_age = c(table(age))
lower = age_prob - 1.96 * sqrt(age_prob * (1 - age_prob) / n_age)
upper =  age_prob + 1.96 * sqrt(age_prob * (1 - age_prob) / n_age)


plot(sort(unique(age)),age_prob,type = "l",lwd = 2, 
     col ="blue",ylim = c(0,1))
lines(sort(unique(age)),lower,type = "l",lwd = 2, 
      col ="red",ylim = c(0,1))
lines(sort(unique(age)),upper,type = "l",lwd = 2, 
      col ="red",ylim = c(0,1))


logit = function(x){
  log(x/(1-x))
}

par(mar = c(5,5,1,1))
plot(sort(unique(age)),logit(age_prob),type = "o",lwd = 2, col ="blue",
     ylim = c(-3,2),cex.lab = 1.4,xlab = "Age (years)",ylab = "log-odds",
     cex = 1.5* n_age /max(n_age),pch = 19)
lines(sort(unique(age)),logit(lower),type = "l",lwd = 2, col ="red")
lines(sort(unique(age)),logit(upper),type = "l",lwd = 2, col ="red")

################ Table (slide 6)

table(cigs,chd)


## add row and column sums

addmargins(table(cigs,chd))

## You can export tables to latex using xtable
library(xtable)  ### latex table
library(knitr)   ### markdown table

addmargins(table(cigs,chd))


xtable(addmargins(table(cigs,chd)),digits = 0)
kable(addmargins(table(cigs,chd)))

##################################################################
###################### GLM Model fitting
##################################################################

### you have to add family = "binomial" or 
## it won't do logistic regression

head(CHD_data)

logistic_reg_model = glm(chd~.,data=CHD_data,family="binomial")

logistic_reg_model = glm(chd~age + height + weight + sbp + dbp +
                           chol + cigs, data=CHD_data,family="binomial")

#logistic_reg_model = glm(chd~.,data=CHD_data[,-9],family="binomial")

### the summary looks very similar to what we had before

library(car)

summary(logistic_reg_model)
vif(logistic_reg_model)

##################################################################
###################### Variable Selection
##################################################################

### Just as before, you need to have chd as the last variable in your data.frame
library(bestglm)
### subset if you have chd_numeric in data.frame

var.select = bestglm(CHD_data,IC="BIC",family=binomial,
                     method = "exhaustive")
var.select$BestModel

## Fit the best model
best_model = glm(chd ~ age + weight + sbp + chol + cigs, 
                 data=CHD_data, family="binomial")

##################################################################
###################### Looking at coefficients (slides 13 - 15)
##################################################################

summary(best_model)


## transform coefficients

best_model$coefficients                   #### interpretted on log-odds scale
round(exp(best_model$coefficients),3)     #### interpretted on the odds scale
100 * (exp(best_model$coefficients) - 1)  #### interpretted as percent change in odds

## Calculate confidence intervals

CI = confint(best_model) 

CI                             #### interpretted on log-odds scale
round(exp(CI),3)                        #### interpretted on odds scale
round(100*(exp(CI)-1),3)               #### interpretted on percent change scale

##################################################################
###################### Predictions
###################### person with age = 45, height = 67, weight = 168, sbp = 124, dbp = 80, chol = 220, cigs = 9
##################################################################

#### log-odds - default
#### prob - type = "response"

pred_xb = predict.glm(best_model,
                      newdata = list(age = 45, height = 67, weight = 168, 
                                     sbp = 124, dbp = 80, chol = 220, cigs = 9),
                      se.fit=TRUE)

pred_xb

pred_xb$fit   ### what the heck? WHy isn't this a probability?????

#################### Let's do this BY HAND

### Choose your quantile for your prediction interval using qnorm
### qnorm(0.975) = 1.96 gives you a 95% confidence interval

pred_xb$fit + c(-1,1)* 1.96*pred_xb$se.fit
pred_xbupper1 = pred_xb$fit + 1.96*pred_xb$se.fit
pred_xblower1 = pred_xb$fit - 1.96*pred_xb$se.fit
pred_xbupper1 
pred_xblower1 


conf_int = c(pred_xblower1,pred_xbupper1)


#################### Who cares about the x * beta (Log-odds scale)


exp(conf_int)/(1 + exp(conf_int))
best_model$family$linkinv(conf_int)

conf_interval_prob = best_model$family$linkinv(conf_int)


conf_interval_prob

##################################################################
## Let's set up a confusion matrix
##################################################################

######## R can actually just give you predicted probabilities (see below)
######## BUT the "by hand" code above is better for getting good intervals

pred.probs = predict.glm(best_model,type="response")
cutoff = 0.5 

preds = pred.probs > cutoff                     ##### predict 
preds = 1 * (pred.probs > cutoff)               ##### predict 
preds = ifelse(pred.probs > cutoff,1,0)         ##### predict 

conf.mat = table(preds,CHD_data$chd)            ##### confusion matrix
conf.mat

################# ################# ################# 
################# MAKE SURE TO CHECK YOUR OUTPUT
################# ################# ################# 

misclass_rate = 1 - sum(diag(conf.mat)) / sum(conf.mat)  ### miscalssification rate
misclass_rate       ### we are wrong 29.2% of the time using this cutoff


############################## Let's do a lot cutoffs to see what it looks like
### sequence of cutoffs

n_cutoff = 100
cutoff = seq(0.05,0.95,length=n_cutoff)
misclass_rate = rep(0,n_cutoff)
sensitivity = rep(0,n_cutoff)
specificity = rep(0,n_cutoff)

for(i in 1:n_cutoff){
  preds = 1 * (pred.probs > cutoff[i])               ##### predict 
  conf.mat = table(preds,CHD_data$chd)                  ##### get confusion matrix
  misclass_rate[i] = 1 - sum(diag(conf.mat))/sum(conf.mat)    #### get misclassification rate
  sensitivity[i] = conf.mat[2,2] / (conf.mat[2,2] + conf.mat[1,2]) 
  specificity[i] = conf.mat[1,1] / (conf.mat[1,1] + conf.mat[2,1]) 
}

plot(cutoff,misclass_rate,type="l",ylab="Misclassification Rate",xlab="Cutoff")
abline(v = cutoff[which.min(misclass_rate)])

plot(cutoff,sensitivity,type="l",ylab="sensitivity",xlab="Cutoff")
plot(cutoff,specificity,type="l",ylab="specificity",xlab="Cutoff")
plot(cutoff,specificity + sensitivity,type="l",
     ylab="specificity + sensitivity",xlab="Cutoff")

cutoff[which.min(misclass_rate)]  ### the cutoff that minimizes missclassification rate
cutoff[which.max(specificity + sensitivity)]  ### the cutoff that minimizes missclassification rate


cutoff[which.max(specificity + sensitivity)]  ### the cutoff that minimizes missclassification rate


## Confusion matrix

cutoff_use = cutoff[which.min(misclass_rate)]
pred_use = pred.probs > cutoff_use
conf.mat = table(pred_use,chd)
conf.mat


addmargins(table(pred_use,chd))

### ### ### ### ### ### ### ### ### ### ### ### ### 
###  Model Comparison/Performance Metrics
### ### ### ### ### ### ### ### ### ### ### ### ### 

conf.mat

### sensitivity

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[1,2])    ## TP/(TP + FN)

### specificity 

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[2,1])    ## TN/(FP + TN)

###  Positive predictive value

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[2,1])    ## TP/(FP + TP)

###  Negative predictive value

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[1,2])    ## TN/(FN + TN)

### percent predicted correctly

sum(diag(conf.mat))/sum(conf.mat)


### Brier Score
### we have to make these 1s and 0s. They go to 2 and 1 here.
chd_number = as.numeric(chd) - 1 
chd_number

## model based estimate
mean((pred.probs - chd_number)^2)

## naive sample mean 
mean((mean(chd_number) - chd_number)^2)

## naive coin flip estimate
mean((0.5 - chd_number)^2)


## AUC

library(pROC)

my.roc = roc(chd,pred.probs)
plot(my.roc,legacy.axes=TRUE)
auc(my.roc)

## Psuedo R^2  --- Deviance = -2 * log-likelihood

1 - best_model$deviance/best_model$null.deviance  ## our model vs. intercept only.

## Check VIFs

library(car)

vif(best_model)

vif(logistic_reg_model)


## ## ## ## ## ## ## ## ## ## ## 
## Use test set for validations
## ## ## ## ## ## ## ## ## ## ## 
n.test = 100

test.obs = sample(1:nrow(CHD_data),n.test)

test.data = CHD_data[test.obs,]
train.data = CHD_data[-test.obs,]

train.mod = glm(chd ~ age + weight + sbp + chol + cigs, data=train.data, 
                family="binomial")


test.preds = predict.glm(train.mod,newdata = test.data, type="response")

test.class = ifelse(test.preds > 0.5, 1, 0)

conf.mat = table(test.data$chd,test.class) ##True Class is column

conf.mat = addmargins(conf.mat)

conf.mat

auc(roc(test.data$chd,test.preds)) 

### sensitivity

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[1,2])    ## TP/(TP + FN)

### specificity 

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[2,1])    ## TN/(FP + TN)

###  Positive predictive value

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[2,1])    ## TP/(FP + TP)

###  Negative predictive value

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[1,2])    ## TN/(FN + TN)

### percent predicted correctly

sum(diag(conf.mat))/sum(conf.mat)


