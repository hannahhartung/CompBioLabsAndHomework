# Lab 5 

## Part I

# 1. 
x <- runif(1,1,10) #1 Random number between 1 and ten
#Print whether x is greater than or less than than 5
if ( x > 5 ) {
  cat('x is greater than 5')
} else if ( x < 5 ) {
  cat('x is less than 5')
} else {
  cat('x is equal to 5')
}
#Check answer
print(x)


# 2.
setwd( "~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab05" ) #Set working directory to access Vector1.csv
#Import 
Vector1 <- read.csv('Vector1.csv')

# 2. a)
for ( i in seq(1:nrow(Vector1)) ) {
  if ( Vector1[i,1] < 0) { 
    Vector1[i,1] <- NA
    }
}

#2. b)
logic_b <- is.na(Vector1)  # Makes T if Vector1 is NA
Vector1[logic_b] <- NaN

#2. c)
Vector1[which(is.nan(Vector1[,1])),1] <- 0  #Boulean for where Vector1 is NaN, indexed as the row with column=1

# 2. d)
#Find values between 50 and 100
logic_d <- (Vector1 > 50 & Vector1 < 100)
total50_100 = sum(logic_d)
#There are 498 values between 50 and 100

# 2. e)
FiftyToOneHundred = Vector1[logic_d] #Index using boulean from before
# summary(FiftyToOneHundred)   #Check that the min and max fall inbetween 50 and 100

# 2. f)
write.csv(FiftyToOneHundred, file="FiftyToOneHundred.csv")


# 3.
CO2 <- read.csv('CO2_data_cut_paste.csv')

# 3. a)
#First year when "Gas" emissions were non-zero
#indexing which[1] gives first index for which this is true
FirstGasYear <- CO2[ which( CO2[,"Gas"] > 0 )[1], "Year" ] #Indexing to that in first column gives year
# 1885

# 3. b)
logic_200_300 <- ( CO2[,"Total"] > 200 & CO2[,"Total"] < 300 )
Yearsbetween_200_300 <- CO2[ logic_200_300, "Year"]
print(Yearsbetween_200_300)
# From 1879 to 1887


## Part II
totalGenerations <- 1000
initPrey <- 100 	  # initial prey abundance at time t = 1
initPred <- 10		# initial predator abundance at time t = 1
a <- 0.01 		# attack rate
r <- 0.2 		# growth rate of prey
m <- 0.05 		# mortality rate of predators
k <- 0.1 		# conversion constant of prey into predators

# Create vectors
time <- seq(1, totalGenerations)
prey <- integer(totalGenerations) #prey abundance = n, vector currently full of zeros
predators <- integer(totalGenerations) #predators abundance = p
#Set initial values
prey[1] <- initPrey
predators[1] <- initPred

# Build for loop to complete calculations
for ( i in 2:totalGenerations ) {
  prey[i] <- prey[i-1] + (r * prey[i-1]) - (a * prey[i-1] * predators[i-1])
  predators[i] <- predators[i-1] + (k * a * prey[i-1] * predators[i-1]) - (m * predators[i-1])
} 

# Check for negative values and set to 0 in each loop
for ( i in 2:totalGenerations ) {
  prey[i] <- prey[i-1] + (r * prey[i-1]) - (a * prey[i-1] * predators[i-1])
  predators[i] <- predators[i-1] + (k * a * prey[i-1] * predators[i-1]) - (m * predators[i-1])
  if ( prey[i] < 0 ) { prey[i] <- 0 }
} 

# Plot Abundances 
plot(time, prey, type='l', col = 'blue',
     xlab="Generation", ylab="Abundance")
lines(time, predators, col = 'red')
legend("topright",c("Prey","Predators"),fill=c("blue","red"))

# Create and Save Matrix
myResults <- matrix(c(time,prey,predators),totalGenerations,3) #3 refers to the number of columns (given by problem)
#Set header names
colnames(myResults) <- c('TimeStep','PreyAbundance','PredatorAbundance')
write.csv(x = myResults, file = "PredPreyResults.csv")


## Part 3
#Set up
totalGenerations <- 1000
initPreyVec <- seq(from = 10, to = 100, by = 10)
initPred <- 10		# initial predator abundance at time t = 1
a <- 0.01 		# attack rate
r <- 0.2 		# growth rate of prey
m <- 0.05 		# mortality rate of predators
k <- 0.1 		# conversion constant of prey into predators

time <- seq(1, totalGenerations) #time vector will remain the same though treatments

#Set up empty vector which is generation x abundance x set of results
PredPrey <- array( ,c(totalGenerations, 3, length(initPreyVec))) #There must be a less "dirty" way to set an empty matrix of given dimensions
for (s in 1:length(initPreyVec)) { #For each of the treatments (indicated by starting prey #)
  #Reset vectors for each treatment
  prey <- integer(totalGenerations) #prey abundance = n, vector currently full of zeros
  predators <- integer(totalGenerations) #predators abundance = p
  predators[1] <- initPred
  prey[1] <- initPreyVec[s] #changes the initPrey
  for ( i in 2:totalGenerations ) {
    prey[i] <- prey[i-1] + (r * prey[i-1]) - (a * prey[i-1] * predators[i-1])
    predators[i] <- predators[i-1] + (k * a * prey[i-1] * predators[i-1]) - (m * predators[i-1])
    if ( prey[i] < 0 ) { prey[i] <- 0 }
  } 
  PredPrey[,1,s] <- time #same between loops
  PredPrey[,2,s] <- prey
  PredPrey[,3,s] <- predators
}

#Data frames cannot be 3D nor can a matrix house column titles. I chose to put my data in the matrix.

# In order to find the data you are interested in please folloe these guide lines
#Data is in a 3D matrix. 
#The first index in the matrix [#,,] corresponds to the generational time step from 1 to 1000
#The second index [,#,] refers to the data of interest
            #Where 1 indicates the generation value
            #Where 2 indicates the prey abundance at that time
            #And 3 indicated predator abundance at that time
#The third index [,,#] refers to the treament
      #Treatments are based on starting prey numbers
      #10 times the index value is the starting prey number
      #For example [,,7] is the 2D matrix describing the system when initial prey number is 70
