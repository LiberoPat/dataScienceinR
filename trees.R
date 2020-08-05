
# "Predicting heart disease with classification trees"
#might have to install bellow libraries, so click Tools -> Install Packages
#and serach for apackage with library name

library(rpart)
library(rpart.plot)
library(maptree)
# the following utility files can be found attached below
source("https://raw.githubusercontent.com/grbruns/cst383/master/lin-regr-util.R")
source("https://raw.githubusercontent.com/grbruns/cst383/master/class-util.R")


#read and preprocess the data.</i>

heart = read.table("https://raw.githubusercontent.com/grbruns/cst383/master/heart.dat", quote = "/")
names(heart) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL",
"SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR",
"THAL", "OUTPUT")
names(heart) = tolower(names(heart))

# convert output to factor
heart$output = factor(heart$output)

#
# do a little exploration
#

# these are the real-valued features:
rv = c("age", "restbp", "chol", "maxhr", "dep")

hist(heart$age)

# good -- predictors not strongly correlated
plot(heart[,rv])

# another way of looking at the correlation
heatmap(cor(heart[,rv]))

x = as.numeric(as.character(heart$output))
plot(age ~ restbp, data=heart, col=c("green", "red")[x], pch=20)

plot(age ~ maxhr, data=heart, col=c("green", "red")[x], pch=20)

plot(restbp ~ maxhr, data=heart, col=c("green", "red")[x], pch=16,
     main="heart disease (red) by rest blood pressure and max heartrate")

###############################################################################
#
# model 1
#
###############################################################################

### Building a classification tree

# Create training and test data sets,
# and build a first classification tree with the training data using package 'rpart'.

# training and test sets
set.seed(132)
split = split_data(heart)
tr_dat = split[[1]]
te_dat = split[[2]]

# tree classifier to predict output using three features: chestpain, restbp, maxhr. Buld classifier with training data. 

fit = rpart(Output ~ chestpain + restbp + maxhr, data = tr_dat, method = "class")

# predictions on training data
predicted = predict(fit, tr_dat, type="class")
table(tr_dat$output, predicted)

# predictions on test data
predicted = predict(fit, te_dat, type="class")
actual = te_dat$output

table(actual, predicted)
mean(actual == predicted)
class_stats(actual, predicted, "1", "2")

# get probabilities from test data
probs = predict(fit, te_dat)[,2]

#
# examine the tree
#

prp(fit, extra=106, varlen=-10, main="classification tree for heart disease",
    box.col=c("palegreen", "pink")[fit$frame$yval])  # see rtree.plot documentation

#
# diagnostics
#

# look at output when heart disease present, and not

par(mfrow=c(1,2))
hist(probs[te_dat$output == "1"], col="red4", main="Classifier probs, no heart disease")
hist(probs[te_dat$output == "2"], col="red4", main="Classifier probs, heart disease")

# double density plot

par(mfrow=c(1,1))
plot(density(probs[te_dat$output == "1"]), col="blue", lwd=2, "Tree classifier; double-density plot")
lines(density(probs[te_dat$output == "2"]), col="orange", lwd=2)
legend("top", c("no heart disease", "heart disease"), fill=c("blue", "orange"), inset=0.05)

# ROC plot

df = compute_stats_by_thresh(actual, probs, "1", "2")

plot(tpr~fpr, data=df, xlim=c(0,1), ylim=c(0,1), col="red4", 
     main="receiver operating characteristic", type="l", lwd=2)
abline(0, 1, lty="dashed")

# precision-recall curve

plot(precision ~ recall, data=df, main="precision-recall curve", type="l", col="red4", lwd=2)

###############################################################################
#
# model 2
#
###############################################################################

# build a tree classifier to predict output using ALL features. Buld classifier with training data. 
# pay attention to the method in rpart (we are doing classification here!). Assign the result to variable fit. 

fit = rpart(Output ~ ., data = tr_dat, method = "class")


# get predictions on test data
predicted = predict(fit, te_dat, type="class")
probs = predict(fit, te_dat)[,2]
actual = te_dat$output

table(actual, predicted)
mean(actual == predicted)


#
# examine the tree
#

prp(fit, extra=106, varlen=-10, main="classification tree for heart disease",
    box.col=c("palegreen", "pink")[fit$frame$yval])  # see rtree.plot documentation

#
# double density plot
#

par(mfrow=c(1,1))
plot(density(probs[te_dat$output == "1"]), col="blue", lwd=2, "Tree classifier; double-density plot")
lines(density(probs[te_dat$output == "2"]), col="orange", lwd=2)
legend("top", c("no heart disease", "heart disease"), fill=c("blue", "orange"), inset=0.05)

#
# ROC plot
#

# precision recall hardly change over range because probs
# compute by rcart are always extreme
df = compute_stats_by_thresh(actual, probs, "1", "2")

plot(tpr~fpr, data=df, xlim=c(0,1), ylim=c(0,1), col="red4", 
     main="receiver operating characteristic", type="l", lwd=2)
abline(0, 1, lty="dashed")

#
# precision-recall curve
#

plot(precision ~ recall, data=df, main="precision-recall curve", type="l", col="red4", lwd=2)

#
# learning curve
#

sizes = floor(seq(0.2, 1, 0.2)*nrow(tr_dat))
l_curve = data.frame()
for (sz in sizes) {
  trd = tr_dat[1:sz,]
  fit = rpart(output ~ thal + maxhr + chestpain, data=trd, method="class")
  # fit = rpart(output ~ ., data=trd, method="class")
  tr_error = 1 - mean(predict(fit, trd, type="class") == trd$output)
  te_error = 1 - mean(predict(fit, te_dat, type="class") == te_dat$output)
  l_curve = rbind(l_curve, data.frame(tr_error, te_error))
}

plot(tr_error ~ sizes, dat=l_curve, col="blue", type="l", ylim=c(0, 0.40), ylab="error", 
     xlab="training set size", main="Learning curve (training error in blue)")
lines(te_error ~ sizes, dat=l_curve, col="green3")

#
# Not easy to diagnose the learning curve.  The training set error
# is high, which is not what we'd expect with overfitting.  But
# there is also a big gap between training set and test set error,
# suggesting overfitting.  
#


# training error if the entire data set as training
# data is used?

fit = rpart(output ~ ., data=heart, method="class")
predicted = predict(fit, te_dat, type="class")
actual = te_dat$output

table(actual, predicted)
mean(actual == predicted)

#
# training set error is definitely better; this suggests that
# the problem is not overfitting, given the small data set
#

#
# try decreasing the min size needed for a split
#

fit = rpart(output ~ ., data=tr_dat, method="class", control=rpart.control(minsplit=5))
predicted = predict(fit, te_dat, type="class")
actual = te_dat$output

table(actual, predicted)
mean(actual == predicted)

#
# try using loss parameter
#

# lmat[2,1] is the loss associated with false negatives
# lmat[1,2] is the loss associated with false positives
# (in the list just below, item two is FN cost, item 3 is FP cost)
lmat = matrix(c(0,100,1,0), nrow=2)
fit = rpart(output ~ ., data=tr_dat, method="class", parms=list(loss=lmat))
predicted = predict(fit, te_dat, type="class")
actual = te_dat$output

prp(fit, extra=106, varlen=-10, main="classification tree for heart disease",
    box.col=c("palegreen", "pink")[fit$frame$yval])  # see rtree.plot documentation


table(actual, predicted)

mean(actual == predicted)

