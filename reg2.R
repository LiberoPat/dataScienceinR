##############################################################
#
#linear regression with Kuipers 2008 used car data 
#
##############################################################

#@ START
# Set the working directory to be whatever directory you want to use.
# This directory should contain the files "lin-regr-util.R" and
# "kuiper-2008-cars.csv" that you downloaded from iLearn.
#
# Also, "source" the file "lin-regr-util.R"

# YOU CAN SET YOUR WORKING DIR AS FOLLOWING, change path!

mydir="/Users/patr1ckt/Downloads/"
setwd(mydir)

source("lin-regr-util.R")

#
# read the data
#

#@ 1
# Download the file "kuiper-2008-cars.csv" .
# read the data as a data frame and assign
# it to variable cars


cars = read.csv("kuiper-2008-cars.csv")

#
# preprocess data
#

# notes:
#  - mileage means total number of miles
#  - R treats factors as indicator variables, automatically

# turn Doors into an indicator variable
cars$FourDoor = as.numeric(cars$Doors == 4)
cars$Doors = NULL

# turn Cylinder into an indicator variable
cars$cyl6 = as.numeric(cars$Cylinder == 6)
cars$cyl8 = as.numeric(cars$Cylinder == 8)
cars$Cylinder = NULL

# turn Type into indicator variable (use all but one to avoid correlation)
cars$Convertible = as.numeric(cars$Type == "Convertible")
cars$Coupe = as.numeric(cars$Type == "Coupe")
cars$Hatchback = as.numeric(cars$Type == "Hatchback")
cars$Sedan = as.numeric(cars$Type == "Sedan")
cars$Type = NULL

# don't need make as it is implied by Model, but put
# Make at the front of Model
cars$Model = paste(cars$Make, cars$Model)
cars$Make = NULL

#@ 2
# produce a grid of scatterplots for the cars data, but using only 
# Price, Mileage, and Liter

plot(cars[,c("Price", "Mileage", "Liter")])

#@ 3
# using lm, perform simple linear regression to create a model
# that estimates Price using feature Mileage.  Assign your
# model to variable 'fit'.


fit = lm(Price ~ Mileage, data=cars)

#@ 4
# produce a scatter plot of Price (y axis) by Mileage (x axis), and
# superimpose the linear model you created as a dotted line on the
# scatterplot

# YOUR CODE HERE

plot(Price ~ Mileage, data=cars, col="red4", 
     main="Used car price by mileage, with fitted model")
abline(fit, col="blue", lwd=2, lty=2)

# the summary of the fit shows high error
summary(fit)

# plots of the fit show that the distribution of residuals is
# not too far from a normal distribution
plot(fit)


#@ 5
# Using lm, create a linear model that estimates Price using 
# features Mileage, Cruise, and Leather.  
# Assign your model to variable 'fit2'.



fit2 = lm(Price ~ Mileage + Cruise + Leather, data=cars)

# R-squared statistic is not good, but better
summary(fit2)

#@ 6
# produce a scatter plot in which you plot the actual
# used car prices (y axis) against the predicted used
# car prices.  Plot a dotted line showing where the
# points would be if the predicted values equalled the
# actual values.  Give your plot an appropriate title.



predicted = predict(fit2, newdata=cars)
plot(predicted, cars$Price, col="red4", pch=20, 
     main="Actual used car prices by predicted prices")
abline(0, 1, col="blue", lty=2)

#@ 7
# produce a similar scatter plot, but this time use
# function 'plot_predict_actual' that is defined in 
# file 'lin-regr-util.R'.  Use 2000 for the error band
# parameter, and use an appropriate title.



plot_predict_actual(predicted, cars$Price, 2000, 
                    "Used car prices: actual by predicted")

# the Residuals vs Fitted plot show errors getting bigger with higher price
plot(fit2)

# 
# We want our model to fits our data well, but especially want
# a model that will make good predictions with *future* data.  In other
# words, we want our model to generalize well.  To get an idea
# of how our model will generalize, we split our data into 
# training and test data.
#

#
# split data into test and training sets using function 'split_data'
# from the lin-regr-util.R file.  The second parameter of the function
# shows how many files the data frame of the first parameter should
# be split into, and their relative sizes.  In this usage the training/test
# data sets use a 75/25 split.
#

dsets = split_data(cars, c(3,1))
tr_dat = dsets[[1]]
te_dat = dsets[[2]]

#@ 8
# Using lm, create another linear model that estimates Price using 
# features Mileage, Cruise, and Leather.  However, this time
# fit your model to the training data set tr_dat.  
# Assign your model to variable 'fit3'.

fit3 = lm(Price ~ Mileage + Cruise + Leather, data=tr_dat)

#@ 9
# Using function 'plot_predict_actual', plot the actual
# and predicted values from your model.  Get actual
# and predicted values based on the *training data*.



predicted = predict(fit3)
plot_predict_actual(predicted, tr_dat$Price, 2000, "")

#@ 10
# Do the same thing as the last problem, but this time,
# get actual and predicted values based on the test data.


predicted = predict(fit3, newdata=te_dat)
plot_predict_actual(predicted, te_dat$Price, 2000, "")

#@ 11
# calculate the RMSE based on the test data, and assign
# it to variable 'rmse'

predicted = predict(fit3, newdata=te_dat)
RMSE = sqrt(mean((predicted-te_dat$Price)^2))


#@ 12
# Create another linear model by adding feature 'cyl8' to
# the features used in your last model.  Use the training
# data set to create your model.
# Assign your model to variable 'fit4'.

fit4 = lm(Price ~ Mileage + Cruise + Leather + cyl8, data=tr_dat)


# R-squared value is improving
# it looks like leather doesn't affect used car price much
summary(fit4)

#@ 13
# Plot the distribution of the residuals using a density plot

plot(density(fit4$residuals))

#@ 14
# Using function 'plot_predict_actual', plot the actual
# and predicted values from your model.  Get actual
# and predicted values based on the *testing* data.



predicted = predict(fit4, newdata=te_dat)
plot_predict_actual(predicted, te_dat$Price, 2000, "")

# residuals look better, but are not very normal
plot(fit3)

# A problem is that we can't use the test data to pick the "best"
# set of features, because then we can't use the test data to give
# a fair idea of the how our model will do on future data.
#
# We also can't use the training data for this purpose, as we already know.
#
# An alternative is to split our data set into *three* pieces, one for
# training, another (called the "validation" data set) for comparing
# alternative sets of features, and the last for doing the final test
# of our model.
# 
# A problem with splitting the data three ways is that the size of each
# of the three pieces is not very big.  This means our validation results
# are not very reliable.
#

# Here's an example showing a 50/25/25 split of the data set into training,
# validation, and test sets.  We train a model on the training data, then
# test a feature set using the validation data.  Every time we do this,
# we have randomly 

rmses = c()
for (i in 1:100) {
  dsets = split_data(cars, c(2, 1, 1))
  train_dat = dsets[[1]]
  valid_dat = dsets[[2]]
  
  fit=lm(Price ~ Mileage + Cruise + Leather + Sound, data=train_dat)
  
  # compute the RMSE based on the validation data set, and assign
  # it to variable 'rmse'
  
  predicted = predict(fit, newdata=valid_dat)
  rmse = sqrt(sum((valid_dat$Price - predicted)^2)/nrow(valid_dat))
  
  rmses = c(rmses, rmse)
}

# Note the range of RMSE values that you get
hist(rmses, col="red4")

#
# cross validaton
#
# A way to get more reliable validation results is to "cross validate",
# which basically allows the training data to be re-used.  One cross validation
# method is called 10-fold cross validation.  In this method you split the
# training data into 10 pieces.  To see how well a set of features works,
# you train the model on 9 of the 10 pieces, and validate on the remaining piece.
# You then repeat this 9 more times, each time using a different subset of 9
# of the 10 pieces.  In each of the 10 validation runs, you compute a result,
# like the RMSE.

# The function "cross_validate_lm" in the file lin-regr-util.R does
# cross validation for feature selection in linear regression.
# if you run the following line of R code a bunch of times, you'll see 
# that the results are quite stable

cross_validate_lm(tr_dat, "Price", c("Mileage", "Cruise", "Leather", "Sound"))

# Notes on cross validation:
#  - Cross validation is used for all kinds of machine learning problems,
#    not just feature selection in linear regression.
#  - Please read the cross_validate_lm code and understand how it works.
#  - There are special methods that allow for very cheap cross validation
#    with linear regression.  We're using a general-purpose cross validation
#    method here so that you can see the idea.



# What is the best single feature to use?  Let's write a loop
# to test all the features and see which gives the best RMSE.

features = setdiff(names(cars), "Price")

min_rmse = 100000
for (feature in features) {
  rmse = cross_validate_lm(tr_dat, "Price", feature)
  
  if (rmse < min_rmse) {
    min_rmse = rmse
    min_feature = feature
  }
}

paste0("best feature to predict Price: ", min_feature, "; RMSE = ", round(min_rmse))

#@ 16
# Using function 'plot_predict_actual', plot the actual
# and predicted values from the model using the best feature
# Get actual and predicted values based on the *testing* data.

ff = reformulate(min_feature, "Price")
fit = lm(ff, tr_dat)


predicted = predict(fit, newdata=te_dat)
plot_predict_actual(predicted, te_dat$Price, 2000, "Actual vs. predicated used car prices")

# R-squared statistic is much better than before
summary(fit)


#@ 17
# What is best second feature to use?  Let's write a loop
# to test all the features and see which gives the best RMSE.
# Have your loop assign values to variables min2_rmse and min2_feature.

features = setdiff(names(cars), c("Price", min_feature))

# TODO



#@ 18
# Repeat problem 15, but this time using the two features found
# so far.

ff = reformulate(c(min_feature, min2_feature), "Price")
fit = lm(ff, tr_dat)

predicted = predict(fit, newdata=te_dat)
plot_predict_actual(predicted, te_dat$Price, 2000, "Actual vs. predicated used car prices")

# R-squared statistic is quite good now
summary(fit)




