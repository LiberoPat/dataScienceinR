#
# k nearest neighbor with applications to college data
#
# a simple kNN classifier
#

# return a knn classifier
# dat    - training data; a data frame with only numeric data
# labels - a vectors of labels with length equal to the number of rows in dat
knn_create = function(dat, labels) {
  return (list(dat=dat, labels=labels))
}

# return the mode of the given vector x
# (the mode is the value that appears most often)
# In case of tie return any of the "most appearing" values.

vmode = function(x) {

  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
  # using R function 'table' or 'tabular'
  
}

# given a numeric vector x, and a string vector 'labels' of the 
# same length as x, return the vector containing the labels that 
# correspond to the k smallest values in vector x. 
# 
# example:
# if x      = c(3.1, 2.3, 0.1, 4.2) and
#    labels = c("a", "b", "c", "d") and
#    k = 2
# then smallest_labels(x, labels, k) should return c("c", "b")

  smallest_labels = function(x, labels, k) {
  f = c(order(x))
  if(x > 0){
    for(x in seq(1,k,1)) {
      return(labels[f[x]])
    }
  }
  return(NULL)

  
}

# given two data frames x,y of numeric data, having the
# same column names, return a matrix of m rows and n columns
# (where nrow(x) = m and nrow(y) = n) where element
# m[i,j] gives the euclidean distance between the ith row of x and
# the jth row of y
#
# example:
# if x = data.frame(a=1:2, b=2:3)
#    y = data.frame(a=1:3, b=2:4)
# then the returned matrix should have 2 rows and 3 columns,
# and the value of the first row and third column should
# be the distance between the first row of x and the third
# row of y.
distances_between = function(x,y) {
  m = nrow(x)
  n = nrow(y)
  
  
  # rbind the two data frames together, get an
  # (m+n) by (m+n) distance matrix using R function 'dist',
  # and then get the rows and columns you need from the matrix
  
}

# return the predicted labels of x using a k nearest neighbor classifier
# knn - a knn object
# x   - a data frame with the same number columns as the knn object data
# k   - the k in knn; a positive odd number should be used
# the returned value is a vector of labels, the same length as the number
# of rows in x
knn_predict = function(knn, x, k = 5) {
  

  
  
  # HINT: first use your function distances_between to get
  # a matrix of the distances from each row of x to each row
  # of knn$dat.  Next, apply your functions smallest_labels 
  # and vmode to each row of the matrix.  This will give the
  # most common label of the k nearest neighbors, for each
  # row of x. 
  # Return your result.
}

# Using a knn classifier, predict which colleges are private
# using the specified feature set and value k.
# Return a confusion matrix showing the result of classification.
predict_private = function(dat, features, k) {
  #
  # prepare the training and test sets
  #

  # make the college names the row names
  rownames(dat) = dat[,1]
  dat = dat[,-1]
  
  # scale all features
  dat[,2:18] = scale(dat[,2:18])

  # randomize data before creating training and test sets
  set.seed(123)
  dat = dat[sample(1:nrow(dat)),]
  
  # number of training examples
  n = floor(nrow(dat)*.80)
  
  # feature vectors training and testing data
  fvs = dat[,features]
  tr_dat = fvs[1:n,]
  te_dat = fvs[-(1:n),]
  
  # labels for training and test data
  labels = dat[,"Private"]
  tr_labels = labels[1:n]
  te_labels = labels[-(1:n)]
  
  #
  # run the classifier and build confusion matrix
  #
  
  # get predictions
  knn = knn_create(tr_dat, tr_labels)
  
  # TODO
  
  
  # call function knn_predict and assign the result to variable 'predicts'.
  #  call should provide the knn object, the test data set, and the parameter k
  
  # create confusion matrix
  actuals = te_labels
  tbl = table(actuals, predicts)
  
  return(tbl)
}



# dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/College.csv")
# features = c("PhD", "Personal")
# predict_private(dat, features, k=3)


