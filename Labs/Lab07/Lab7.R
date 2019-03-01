#Lab 7: Functions

#Basic structure of a function:
#myFunction <- function(arg1 = defaultVal, arg2 = defaultVal) {
#  # step 1:
#  
#  # step 2:
#  
#  # etc.... 
#  
#  return(results)
#}

###########
### Problem 1.

#Function to calculate the area of a triangle given base and height
triangleArea <- function(base,height){
  #Where base is the side of the triangle perpendicular to the height measurement
  
  #Find the area
  area <- 0.5 * base * height
  
  #return area as output of function
  return(area)
}

#Example use:
triangleArea(5,10)
2 == triangleArea(2,2)

############
### Problem 2.

#Function which finds absolute value of v
myAbs <- function(v){ #Where v is any value or vector
  negs <- v < 0  #which are negative values?
  
  #Index the negative values and make them positive 
  v[negs] <- v[negs] * (-1)
  
  #Return the new v with no negative values
  return(v)
}

#Exmaple use:
#1
myAbs(5)
#2
myAbs(-2.3)
#3
myAbs(c(1.1,2,0,-4.3,9,-12))
#All check out!

##############
### Problem 3.

#Returns vector of fibonacci numbers of length n
#Where the first to values are [1,1]
myFib <- function(n) {
  
  #Create begining of fibonacci
  fib <- c(1,1)
  
  #Set n to atleast 3, and print error if non-positive integer was given as n
  if ( n <= 2 ){
    if ( n%%1 != 0 ) { 
      #Is n's number of decimal places not equal to 1?
      #Or is it negative?
      print('Non-integer given, Fibonacci length set to 3')
    }else{
      print('Length too low, Fibonacci length set to 3')
    }
    n <- 3 #Just set to 3, report no error
  }
  
  
  #Run through, adding to the sequence
  for ( i in seq(3,n) ){ #For a number of times n-3
    #Find next fibonacci number
    newnum <- fib[i-1] + fib[i-2]
    #Append fib with new value
    fib <- c( fib, newnum )
  }
  
  #Return full length fibonacci
  return(fib)
}

#Example use:
myFib(9)
myFib(-1)
myFib(.00004)


###############
### Problem 4) a.

#returns the square of the difference between x and y
SquareDif <- function(x,y){
  #Find the square of the difference
  diff2 <- (x - y)^2
  
  #return calculation
  return(diff2)
}

#Example use:
SquareDif(3,5)
SquareDif(c(2,4,6),4)
#R vectorizes waaaayyyy better than MATLAB! Woohoo!


### Problem 4) b.

#Calculate the mean of the imput vector
myMean <- function(x){#Where x is a vector
  
  #Divide the sum by the number of values in x
  average <- sum(x)/length(x)
  
  return(average)
}

#Example use:
myMean(c(5, 15, 10))

setwd("~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab07" )
#Import 
Vector_x <- read.csv('DataForLab07.csv')
myMean(Vector_x[,1]) #Read first column of vector x
#Works!


### Problem 4) c.
TotalSumSquare <- function(x) {
  #Find the mean of the data
  average <- myMean(x)
  
  #Set inital sum of squares value to zero
  sumSquares = 0 
  
  for ( j in 1:length(x) ) { #For each data point
    datapoint <- x[j]
    #calculate the square of the difference
    squarediff <- SquareDif( datapoint, average )
    
    #add that to the sum total
    sumSquares = sumSquares + squarediff
  }
  
  return(sumSquares)
}

TotalSumSquare(Vector_x[,1])
#Done!