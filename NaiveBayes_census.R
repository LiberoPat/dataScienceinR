
# this is analysi using a Naive Bayes classifier to predict whether people in the 1994 census summary data have income above or below $50,000.   The data can be found here:
#https://raw.githubusercontent.com/grbruns/cst383/master/1994-census-summary.csv
# analysis has a generic form of:
#●	data input and preprocessing
#●	data exploration and visualization
#●	splitting data into training and test sets
#●	development of at least two models, where, for each model you:
#  ○	build the model from training data
#○	note anything interesting about the model itself
#○	generate predictions 
#○	compute accuracy and a confusion matrix from your predictions
#○	generate other statistics and plots as appropriate
#
#The attribute ‘label’ in the data set indicates whether income is above or below $50,000.  It is your Output that you are trying to model!
# be mindfule about that when applying modeling function and functional relationship to features.

# yinstall bellow libraries, so click Tools -> Install Packages
#and serach for apackage with library name#
# do census data prediction with naive bayes
#  I have an OSx and had tough time installing using install packages method due to my R version. In the end I had to 
# do it manually by runing following command
# install.packages("~/Downloads/e1071_1.6-8.tar.gz", repos = NULL, type = "source")  after downloading from web repo older version of e1701
# e1071_1.6-8.tar.gz, in old sources section of https://cran.r-project.org/web/packages/e1071/index.html


library('e1071')
source("https://raw.githubusercontent.com/grbruns/cst383/master/lin-regr-util.R")


#
# read the data
#

dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/1994-census-summary.csv")

# remove irrelevant features

dat$usid = NULL
dat$fnlwgt = NULL



#
# data exploration
#

cols = c("red", "green")[as.numeric(dat$label == "<=50K")+1]
plot(dat$capital_gain, dat$education, col=cols, pch=16)

# need to worry about correlated variables
plot(dat[1:1000,])

# what about people with high/low capital gains?

par(mfrow=c(1,2))
hist(dat[dat$label=="<=50K",]$capital_gain, xlim=c(0,20000), col="red")
hist(dat[dat$label==">50K",]$capital_gain, xlim=c(0,20000), col="green")
sum(dat$capital_gain > 5000)/nrow(dat)

dat_gain = dat[dat$capital_gain > 5000]

# what about people with high/low capital losses?

hist(dat[dat$label=="<=50K",]$capital_loss, xlim=c(0,5000), col="red")
hist(dat[dat$label==">50K",]$capital_loss, xlim=c(0,5000), col="green")
sum(dat$capital_loss > 1000)/nrow(dat)

summary(dat$capital_loss)

#
# build training and test data sets
#

set.seed(124)
splits = split_data(dat, frac=c(3,1))
tr_data = splits[[1]]
te_data = splits[[2]]

#
# model using only capital gain and education
#
# build a Naive BAyes classifier to predict output using only capital gain and education. Buld classifier with training data. 
#   1.  Assign the result to variable fit.
#   2.  Assign to variable  predicts output of predictions from the result of modeling, use test data (make sure type="class" in method)
#   3.  calculate  variable actuals

fit = naiveBayes(output~capital_gain + education, data = tr_data)
predicts = predict(fit, newdata=te_data, type="class")
actuals = te_data$output


# results not bad for just two features
table(actuals, predicts)
mean(actuals == predicts)

#
# model using all input features
#
# build a Naive BAyes classifier to predict output using all input features. Buld classifier with training data. 
#   1.  Assign the result to variable fit.
#   2.  Assign to variable  predicts output of predictions from the result of modeling, use test data (make sure type="class" in method)
#   3.  calculate  variable actuals

fit = naiveBayes(output ~ . , data = tr_data)
predicts = predict(fit, newdata=te_data, type="class")
actuals = te_data$output

table(actuals, predicts)
mean(actuals == predicts)

#
# see if it helps to convert some features to categorical (it didn't)
#

dat$education_num = factor(dat$education_num)
dat$hours_per_week = factor(dat$hours_per_week)

set.seed(124)
splits = split_data(dat, frac=c(3,1))
tr_data = splits[[1]]
te_data = splits[[2]]

fit = naiveBayes(label ~ ., data=tr_data)
predicts = predict(fit, te_data, type="class")
actuals = te_data$label

table(actuals, predicts)
mean(actuals == predicts)

dat$education_num = as.integer(as.character(dat$education_num))
dat$hours_per_week = as.integer(as.character(dat$hours_per_week))

#
# function to build and predict with all features
#

run_full_model = function(dat) {
  set.seed(124)
  splits = split_data(dat, frac=c(3,1))
  tr_data = splits[[1]]
  te_data = splits[[2]]
  
  fit = naiveBayes(label ~ ., data=tr_data)
  predicts = predict(fit, te_data, type="class")
  actuals = te_data$label
  
  print(table(actuals, predicts))
  print(mean(actuals == predicts))
}

#
# try education > 12 or not, since education num not a Gaussian
# (this barely improved accuracy)
#

dat1 = dat
dat1$higher_ed = factor(dat$education_num > 12)
dat1$education_num = NULL

run_full_model(dat1)

#
# try turning capital gains and losses into categorical features;
# they are not well approximated by Gaussians
# (this reduced accuracy)
#

dat1 = dat
dat1$cap_gain = factor(dat$capital_gain > 0, labels=c("cap_gain_zero", "cap_gain_pos"))
dat1$capital_gain = NULL
dat1$cap_loss = factor(dat$capital_loss > 0, labels=c("cap_loss_zero", "cap_loss_pos"))
dat1$capital_loss = NULL

run_full_model(dat1)

#
# avoid correlated features
#

fit = naiveBayes(label ~ age + workclass + education + marital_status + occupation + sex + capital_gain + native_country, data=tr_data)
predicts = predict(fit, te_data, type="class")
actuals = te_data$label

# results not bad for just two features
table(actuals, predicts)
mean(actuals == predicts)

# what about results on training data?
predicts = predict(fit, tr_data, type="class")
actuals = tr_data$label
mean(actuals == predicts)

#
# would Laplace smoothing help? (doesn't really)
#


fit = naiveBayes(label ~ ., data=tr_data, laplace=2)
predicts = predict(fit, te_data, type="class")
actuals = te_data$label

table(actuals, predicts)
mean(actuals == predicts)

# look at some misclassifications

errors = te_data[actuals != predicts,][1:50,]

# how would tree classifier do on this data?
# answer: about the same as Naive Bayes
#
# model using all input features
#
# build a Trees classifier to predict output using all input features. Buld classifier with training data. 
#   1.  Assign the result to variable fit.
#   2.  Assign to variable  predicts output of predictions from the result of modeling, use test data (make sure type="class" in method)
#   3.  calculate  variable actuals
#   4.  calculate accuracy



library(rpart)

fit = rpart(output ~ ., data = tr_dat, method = "class")
predicts = predict(fit, te_dat, type="class")
actuals = te_dat$output
table(actuals, predicts)
mean(actuals == predicts)




