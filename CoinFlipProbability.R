
#
# R homework for CST383, week 3
#

# You may use the output from one problem in later problems.


# Simulate flipping a coin 200 times that has a 90% chance of
# landing heads.  Store result in a vector x of length
# 200 that contains only 0 or 1, where 1 represents heads.

x = sample(0:1, 200,prob = c(.1,.9), replace = TRUE)


#  R function 'runif'.
#  runif(200) gives a vector of length 200 with random
# values between 0 and 1.  Obviously will need to do
# further processing to turn the output of runif into your
# vector x.

x = sample(0:1, 200,prob = c(runif(2, min=0, max = 1)), replace = TRUE)


# computes the fraction of "heads" in vector x

(sum(x==1))/length(x)


# Perform 100 repetitions of the experiment of flipping the weighted coin 200 times,
# compute the fraction of heads for each experiment, and store the
# result in a vector y1.  
y1=c()
y1 = replicate(100,{x = sample(0:1, 200,prob = c(runif(2, min=0, max = 1)), replace = TRUE); y1 = c(y1,(sum(x==1))/length(x))})


# plot a histogram of the values in y1

hist(y1)


# compute a vector y2 that is just like y1
# but it is 1000 coin flips in each experiment, not 200.

y2=c()
y2 = replicate(100,{x = sample(0:1, 1000,prob = c(runif(2, min=0, max = 1)), replace = TRUE); y2 = c(y2,(sum(x==1))/length(x))})


# plot histograms for y1 and y2, with the histogram for y1 above 
# the plot for y2.  Our lecture notes show how to do this, using
# parameter 'mfrow'.  In both histograms, let the x axis values range from
# 0.85 to 0.95.
# (produce a plot)
par(mfrow=c(2,1))
hist(y1, main = "", xlim = c(0.85,0.95))
hist(y2, main = "", xlim = c(0.85,0.95))


# Generate a vector x of length 10000 consisting of 1's and 0's.
# The vector should be created by flipping a coin having a 0.01
# probability of heads, where 1 represents heads.
# (assign to x)

x = sample(0:1, 10000,prob = c(.99,.01), replace = TRUE)


# sort vector x, so that the 0's appear before the 1's
# (assign to x)
x = sort(x, decreasing = FALSE)


# Create a vector y1, with length equal to the number
# of 0's in vector x, such that 95 percent of the elements
# in vector y1 are 0, and the rest are 1.  This vector
# represents predictions as to whether a value is 0 when it really
# is 0.
# (assign to y1)
y1 = vector(mode = "integer", length = sum(x==0))
y1 = sample(0:1, sum(x==0),prob = c(.95,.05), replace = TRUE)
y1 = sort(y1, decreasing = FALSE)


# Similarly create a vector y2, with length equal to the
# number of 1's in vector x, such that 98 percent of the
# elements in vector y2 are 1, and the rest are 0.  This
# vector represents predictions as to whether a value is 1 when it really
# is 1.
# (assign to y2)
y2 = vector(mode = "integer", length = sum(x==1))
y2 = sample(0:1, sum(x==1),prob = c(.02,.98), replace = TRUE)
y2 = sort(y2, decreasing = FALSE)


# create a vector y by concatenating y1 and y2, in that order
# (assign to y)
y = c(y1,y2)


# create a data frame 'tests' from vectors x and y, with the first
# column containing the x values, and named 'actual', and the
# second column containing the y values, and named 'predict'.
# (assign to tests)
tests = data.frame(actual = x, predict = y)

# create a data frame 'ptests' that consists of the rows of 'tests'
# in which the 'predict' value is 1
# (assign to ptests)
ptests = data.frame(tests[tests$predict==1,])


# compute the fraction of the rows in ptests in which 'actual' is 1
# (write an expression)
(nrow(ptests) - nrow(data.frame(ptests[ptests$actual==1,])))/ nrow(ptests)



# package up the code you've written to compute the value in the
# previous problem into an R function named 'prob_cond_given_pos'.  
# It should have three parameters
#    prob_cond (your code used 0.01)
#    prob_pos_given_cond (your code used 0.98)
#    prob_neg_given_no_cond (your code used 0.95)
# The function should return the value just calculated.
# (assign to prob_cond_given_pos)
prob_cond_given_pos = function(prob_cond, prob_pos_given_cond, prob_neg_given_no_cond){
  x = sample(0:1, 10000,prob = c(1-prob_cond,prob_cond), replace = TRUE)
  x = sort(x, decreasing = FALSE)
  
  y1 = vector(mode = "integer", length = sum(x==0))
  y1 = sample(0:1, sum(x==0),prob = c(prob_neg_given_no_cond,1.0-prob_neg_given_no_cond), replace = TRUE)
  y1 = sort(y1, decreasing = FALSE)
  
  y2 = vector(mode = "integer", length = sum(x==1))
  y2 = sample(0:1, sum(x==1),prob = c(1.0-prob_pos_given_cond,prob_pos_given_cond), replace = TRUE)
  y2 = sort(y2, decreasing = FALSE)
  
  y = c(y1,y2)
  
  tests = data.frame(actual = x, predict = y)
  ptests = data.frame(tests[tests$predict==1,])

  return ((nrow(ptests) - nrow(data.frame(ptests[ptests$actual==1,])))/ nrow(ptests))
}

#@ 17
# run your function with parameters set as follows:
# prob_cond = 0.01
# prob_pos_given_cond = 0.95
# prob_neg_given_no_cond = 0.95
# (write an expression)
prob_cond_given_pos(.01,.95,.95)

#@ 18
# produce a plot where the x axis is values for prob_cond
# let x range from 0.01 to 0.1
# let prob_pos_given_cond and prob_neg_given_no_cond both be 0.95.
# Hint: create a vector of the x values you want to use,
# then use 'sapply' with those x values, and using a
# function like 'function(x) prob_cond_given_pos(x, 0.95, 0.95)'.
# (produce a plot)
x1 = seq(0,.1, by = .01)
length(x1)
i = 1
y = 0
for(i in 1:10){
  y = c(y,prob_cond_given_pos(x1[i],.95,.95))
}
plot(x1,y,main="", xlab = "prob_cond", ylab = "% rows, actual=1",xlim = c(.01,.1), ylim = c(0,1.0))

#@ 19
# In your function 'prob_cond_given_pos', you start by creating a
# vector of length 10000.  Change that value to 100000 and re-produce
# the plot of the last step.
# (assign to prob_cond_given_pos and produce a plot)
prob_cond_given_pos = function(prob_cond, prob_pos_given_cond, prob_neg_given_no_cond){
  x = sample(0:1, 100000,prob = c(1-prob_cond,prob_cond), replace = TRUE)
  x = sort(x, decreasing = FALSE)
  
  y1 = vector(mode = "integer", length = sum(x==0))
  y1 = sample(0:1, sum(x==0),prob = c(prob_neg_given_no_cond,1.0-prob_neg_given_no_cond), replace = TRUE)
  y1 = sort(y1, decreasing = FALSE)
  
  y2 = vector(mode = "integer", length = sum(x==1))
  y2 = sample(0:1, sum(x==1),prob = c(1.0-prob_pos_given_cond,prob_pos_given_cond), replace = TRUE)
  y2 = sort(y2, decreasing = FALSE)
  
  y = c(y1,y2)
  
  tests = data.frame(actual = x, predict = y)
  ptests = data.frame(tests[tests$predict==1,])
  
  return ((nrow(ptests) - nrow(data.frame(ptests[ptests$actual==1,])))/ nrow(ptests))
}
x1 = seq(0,.1, by = .01)
length(x1)
i = 1
y = 0
for(i in 1:10){
  y = c(y,prob_cond_given_pos(x1[i],.95,.95))
}
plot(x1,y,main="", xlab = "prob_cond", ylab = "% rows, actual=1",xlim = c(.01,.1), ylim = c(0,1.0))

#@ 20
# Your function is concerned with predicting a condition given
# a positive test result.  Should the result depend on the
# probability of a negative test given no condition?  
# Produce a plot as in the previous problem, but this time plot
# two lines instead of one, using different values of the parameter
# 'prob_neg_given_no_cond'.  Use values 0.95 and 0.80.  Set the range 
# of the y axis to c(0,1) to make it easy to compare.  Hint: use function 
# 'lines' to plot on a previously created plot.
# (produce a plot)




