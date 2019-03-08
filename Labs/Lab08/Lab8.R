### Lab 8
rm(list=ls())

### 1. Markdown tutorial
# Done! 
# I did not know how to do the [space][space] to get a new line without forcing a double space before.


### 2. New README
#Done!


### 3.
### 3. a)
# Discrete-time logistic growth equation
# n[t] = n[t-1] + ( r * n[t-1] * (K - n[t-1])/K )
# n[t] is the abundance of population at time t
# r is the intrinsic growth rate of the population
# K is the environmental carrying capacity for the population
n <- vector()

n[1] <- 2500 #Individuals
K <- 10000 #carrying capacity
r <- 0.8 #growth rate
years_in_study <- 2:12 #Setting squence from 2 on since that's when equation is valid

for ( t in years_in_study ){   
  print(t) #Print t
  current_pop = (n[t-1] + ( r * n[t-1] * (K - n[t-1])/K )) #Where t is the current year
  print(current_pop) #current population
  n[t] <- n[t-1] + ( r * n[t-1] * (K - n[t-1])/K )
}

# At year 12 the population is 9999.985 
#(Or 9999, if you round to nearest individual throughout the whole code using trunc)
year <- seq(1,12)
#to graph with the automatic axis labels, I'll change n to abundance
abundance1 <- n
plot(year, abundance1) 


### 3. b-c)
#Make into a function
rm(list=ls())

#Start your years function at year after the initial values (this could be fixed in future code)
log_growth <- function(n, r=0.8, K=10000, years=2:12){
  for ( t in years ){   
    current_pop = (n[t-1] + ( r * n[t-1] * (K - n[t-1])/K )) #Where t is the current year
    n[t] <- n[t-1] + ( r * n[t-1] * (K - n[t-1])/K )
  }
  #Set x values for plot from 1 to the last year
  year <- seq(1,max(years))
  #BONUS: added to save image to use in markdown
  png('Growth_Model.png')
  #plot
  plot(year,n, type='l', col = 'blue', #Where type is line and col is color and blue
       xlab="Year", ylab="Abundance")
  #Finish saving the png of the plot
  dev.off()
  return(n)
}

check <- log_growth(2500)
#check == abundance1 #From original lab 4 output - does not work with rm above function
#Works!

### 3. d)
testgrowth <- log_growth(100, r=0.9, 2200)
#Works!

### 3. e) 
growth <- log_growth(200, r=0.9, 6000)
#Put variables together in matrix
gen_growth <- matrix(c(seq(1,length(growth)),growth),ncol = 2)
#Add column names
colnames(gen_growth) <- list('Generation','Abundance')
setwd('~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab08')
write.csv(gen_growth, file = "LogisticGrowthResults.csv")

# Notes: all of this save code could be done in the function too.
