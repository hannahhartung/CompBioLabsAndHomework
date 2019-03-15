# Lab 9
################
rm(list=ls())

setwd('~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab09')

camData <- read.csv("Cusack_et_al_random_versus_trail_camera_trap_data_Ruaha_2013_14.csv", stringsAsFactors = F)

### Problem 1
smallDates <- camData$DateTime[1:5]

# Ruaha National Park, southern Tanzania Timezone is GMT + 3 hours
# strptime automatically displays GMT and there is no abbreviation I that R knows for that zone
# Just ignore GMT and know that it is in MSK or GMT + 3 hours because relative time is all that matters

# Test
transferdate <- strptime(smallDates, format = '%d/%m/%Y %H:%M')
transferdate[2]-transferdate[3]
# Works

Times <- strptime(camData$DateTime, format = '%d/%m/%Y %H:%M')

#Test numericalness for random points in data set
rand1 <- floor(runif(1, min=0, max=1001))
rand2 <- floor(runif(1, min=0, max=1001))
Times[rand1]-Times[rand2]
#Works

#Test if a) mean works, and b) that the time makes sense
mean(Times)
#Seems some years are less than 2013, possibly abbreviated in original data set

Times[Times < 2012] #Some are less than 2012, set to 0013 instead of 2013

Times[Times < 2012][1] + 2000 #Test if this just adds 2000 years to one and fixes issue
#Nope

Times[Times < 2012][1]

# Found new function: gsub which replaces patterns
gsub(0013, 2013, Times[Times < 2012][1])
# gsub does not seem to work well with free 0s
gsub("0013", 2013, Times[Times < 2012][1])
#This works, just need to check that it is still a time
t1 <- gsub("0013", 2013, Times[Times < 2012][rand1])
t2 <- gsub("0013", 2013, Times[Times < 2012][rand2])
t1-t2
#Nope, its a string now

# Try formatting as a strptime again (clunky)
t1 <- strptime(gsub("0013", 2013, Times[Times < 2012][rand1]), format = '%Y-%m-%d %H:%M:%S')
t2 <- strptime(gsub("0013", 2013, Times[Times < 2012][rand2]), format = '%Y-%m-%d %H:%M:%S')
t1-t2
#Woot

# Do this cluckiness for all Times
# Blocked out to do procedure later
#logic1 <- Times < 2012
#Times[logic1] <- strptime(gsub("0013", 2013, Times[Times < 2012]), format = '%Y-%m-%d %H:%M:%S')

mean(Times)
#Not there yet, but closer?

summary(Times)
#Ah, there are still 0014s too

# Do for both
logic1 <- Times < 0014 #For all 13s
Times[logic1] <- strptime(gsub("0013", 2013, Times[Times < 2012]), format = '%Y-%m-%d %H:%M:%S')
logic2 <- Times < 0015 #For all 14s
Times[logic2] <- strptime(gsub("0014", 2014, Times[Times < 2012]), format = '%Y-%m-%d %H:%M:%S')

summary(Times)
#Nice!

camData$DateTime <- Times #Replace final


### Problem 2
sum(logic1)
sum(logic2)
# Specific incorrect entries are those where logic 1 or 2 are true
# Check to see if one particular station recorded incorrectly
camData$Station[logic1]
# Nope, this problem is across the board

### Problem 3
# Done
rand1 <- floor(runif(1, min=0, max=1001))
rand2 <- floor(runif(1, min=0, max=1001))
camData$DateTime[rand1]-camData$DateTime[rand2]


### Problem 4
# Assume times are already in chronolocial order
WetTime <- c()
DryTime <- c()

for ( i in seq(1:length(camData$Season)-1) ) {
  timeBetween <- abs(camData$DateTime[i]-camData$DateTime[i+1])
  if ( camData$Season[i] == 'W' & camData$Season[i+1] == 'W' ) {
    WetTime <- c(WetTime,timeBetween)
  } else if ( camData$Season[i] == 'D' & camData$Season[i+1] == 'D' ) {
    DryTime <- c(DryTime,timeBetween)
  } else { # Not recorded as average of either because between seasons  
    }
}
# Produces error at end because i becomes 14604L thus yielding null values in if statement

meanWet <- mean(WetTime)
meanDry <- mean(DryTime)
# This is close but it does not yet preserve units (sometimes output in min/sec/hour).


# Try using difftime instead
WetTime <- c()
DryTime <- c()
for ( i in seq(1:length(camData$Season)-1) ) {
  timeBetween <- abs(difftime(camData$DateTime[i],camData$DateTime[i+1], units = 'hours'))
  #All times in hours
  print(timeBetween)
  if ( camData$Season[i] == 'W' & camData$Season[i+1] == 'W' ) {
    WetTime <- c(WetTime,timeBetween)
  } else if ( camData$Season[i] == 'D' & camData$Season[i+1] == 'D' ) {
    DryTime <- c(DryTime,timeBetween)
  } else { # Not recorded as average of either because between seasons  
  }
}

# In hours
meanWet <- mean(WetTime) 
meanDry <- mean(DryTime)
# Works! Other biological questions could be answered in a similar way.