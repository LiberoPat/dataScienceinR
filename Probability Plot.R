

z = x


msg = "hello"

# check whether variable msg is numeric
is.numeric(msg)
#@ check whether variable b is logical
is.logical(b)
#@ assign the result of squaring x to variable x2
x2=x*x
#@ 6 
# compute the length of the string obtained by appending strings 
# s1 and s2, with a space between them
nchar(paste(s1,s2))

#  compute the substring consisting of the 2nd-4th characters of string s
substr(s,2,4)


xt = x > 5

is.na(x)

# Assign to variable x a vector of length 10 of
# randomly-generated numbers from 1 to 3.
x = sample(1:3, 10, replace = TRUE)

# Compute a vector 'bdays' 
# containing the birthdays (as a number from 1 to 365) of 30 randomly-selected people.

bdays = sample(1:365, 30, replace = TRUE)

# Computes 0 if vector x contains no duplicates, and 
# computes 1 otherwise.
# (expression)
ifelse(any(duplicated(x)) == FALSE,0,1)

# Compute the birthdays of 30
# randomly-selected people 1000 times, and assign
# to variable bday_prob the fraction of the time that 
# two people share the same birthday

y=0
for (i in 1:1000){
  bdays = sample(1:365, 30, replace = TRUE)
  y = y + ifelse(any(duplicated(bdays)) == FALSE,0,1)
}
bday_prob = y/1000

# Estimates the probability that
# two people among a room of randomly-selected people will
# share the same birthday.  The function should be named
# 'bday_prob', and should take a parameter 'num_people'.

bday_prob = function(num_people){
  y=0
  for (i in 1:1000){
    bdays = sample(1:365, num_people, replace = TRUE)
    y = y + ifelse(any(duplicated(bdays)) == FALSE,0,1)
  }
  return(y/1000)
}

# Test function 'bday_prob' by calling it with input parameter values 10, 20, 30, 40, and 50

bday_prob(10)
bday_prob(20)
bday_prob(30)
bday_prob(40)
bday_prob(50)

# Create a plot that will show the estimated probability that
# two people in a room will share the same birthday.  The x axis
# of the plot should range from 5 to 50, and indicate the number
# of people in the room.  The y axis should range from 0 to 1 and
# represent the probability.  To create data for the plot, run
# your birthday function on all input values of 5 to 50.Improve your plot 
#by using color, by adding a title, x and y axis labels, and a grid.  
#Make the title "Esimated prob. of sharing a birthday",
# the x axis label "Number of people", and the y axis label "Prob. of sharing a birthday".
# Plot the curve as a line, not points.
# (plot)
bday_prob = function(num_people){
  y=0
  for (i in 1:1000){
    bdays = sample(1:365, num_people, replace = TRUE)
    y = y + ifelse(any(duplicated(bdays)) == FALSE,0,1)
  }
  y/1000
}

x = 5:50
i = 5
y = 0
for(i in 1:45){
  y = c(y,bday_prob(i))
}
plot(x,y,main="Esimated prob. of sharing a birthday", xlab = "Number of people", ylab = "Prob. of sharing a birthday",xlim = c(5,50), ylim= c(0,1))
abline(v=(seq(0,50,2)),h=(seq(0,1,.1)), col="lightgray", lty="dotted")
points(x,y, col="blue")

