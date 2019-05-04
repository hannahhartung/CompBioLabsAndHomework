################ Final Project ################
#               Hannah Hartung                #
#                                             #
#      How does distance between isolated     #
# populations impact potential for speciation #
#  due to genetic drift or sexual selection?  #
###############################################

# Clear workspace, graphics and set directory
rm(list=ls())
graphics.off()
setwd('~/Desktop/CompBio/CompBioLabsAndHomework/Final_Project/Assignment09')
library(ggplot2)




################ Mating Data ################
MateData <- read.csv("Runemark_and_Svensson_MatingData.csv", stringsAsFactors = F)

#### Cleaning Data

#There are NAs for certain data points so I will find and remove them
#It appears that if one column contains NAs, all other problem columns will be NAs
MateData$Time.spent.in.same.square[-which(is.na(MateData$Time.spent.in.same.square))]

MateData <- MateData[-which(is.na(MateData$Time.spent.in.same.square)),]
#Here the number of samples without any NAs is 365 out of the original 417
#Check the other potential problem collumns (times)
MateData$Time.spent.in.diagonally.opposite.squares[-which(is.na(MateData$Time.spent.in.diagonally.opposite.squares))]
MateData$Time.spent.in.two.adjacent.squares...short.side.[-which(is.na(MateData$Time.spent.in.two.adjacent.squares...short.side.))]
MateData$Time.spent.in.two.adjacent.squares..long.side.[-which(is.na(MateData$Time.spent.in.two.adjacent.squares..long.side.))]

#I also noticed a negative value - seemingly a typo so I made it positive
MateData$Time.spent.in.same.square[which((MateData$Time.spent.in.same.square) < 0)]
MateData$Time.spent.in.same.square[which((MateData$Time.spent.in.same.square) < 0)] <- -MateData$Time.spent.in.same.square[which((MateData$Time.spent.in.same.square) < 0)]

#Check other time columns
MateData$Time.spent.in.diagonally.opposite.squares[which(MateData$Time.spent.in.diagonally.opposite.squares < 0)]
MateData$Time.spent.in.two.adjacent.squares...short.side.[which(MateData$Time.spent.in.two.adjacent.squares...short.side. < 0)]
MateData$Time.spent.in.two.adjacent.squares..long.side.[which(MateData$Time.spent.in.two.adjacent.squares..long.side. < 0)]
#All check out


#### Analysis

#Identify the sites used for this experiment
Locations <- unique(c(unique(MateData$Female.population.of.origin),unique(MateData$Male.population.of.origin)))

#Import matrix of distances between sites
#I calculated this data given the GPS coordinates from their paper and google maps
DistanceData <- read.csv("Runemark_SiteLocationDistances.csv", row.names = 1 )
#In km

#Calculate ratio of successful mates
mateRatio <- MateData$Mating / (MateData$Mating + MateData$Escapes)
#Change NAs to zeros
mateRatio[which(is.na(mateRatio))] <- 0

#Pair mate ratios to distances between collection sites
distances <- c()
for (i in seq(1:dim(MateData)[1])) {
  #Index distances by the female island and the male island
  distances <- c(distances, DistanceData[[c(MateData$Female.population.of.origin[i]),sub(" ", ".",c(MateData$Male.population.of.origin[i]))]])
}

#Plot distances versus ratios to get a sense for the information
#Turn vectors into a single dataframe
mateRatio <- data.frame(distance = distances, compatibility = mateRatio)
ggplot(mateRatio) + geom_point(aes(distance,compatibility))

#Set up land distance versus sea distance
distancesLand <- c()
distancesSea <- c() 
for (i in seq(1:dim(MateData)[1])) {
  if ( MateData$Female.habitat.of.origin == 'Island' | MateData$Male.habitat.of.origin == 'Island') {
    #If either or both Island, then the distance is over sea
    distancesSea <- c(distancesSea, DistanceData[[c(MateData$Female.population.of.origin[i]),sub(" ", ".",c(MateData$Male.population.of.origin[i]))]])
  } else {
    distancesLand <- c(distancesLand, DistanceData[[c(MateData$Female.population.of.origin[i]),sub(" ", ".",c(MateData$Male.population.of.origin[i]))]])
  }
}
#Upon closer examination of the data, these are all sea distances.
#This makes sense given the goals of the original data set. 
#Therefore the rest of my code will focus just on using sea distance to explain speciation potential


#Create linear model
lm_fit <- lm(mateRatio$compatibility ~ mateRatio$distance)
summary(lm_fit)
#P value is less than 0.05

#Plot the points along with a regression line
ggplot(mateRatio, aes(distance,compatibility)) + 
  geom_point(color='seagreen1') +
  #Setting geom_smooth to lm creates a line with the same coefficients from lm above
  #The grey area are 95% confidence intervals
  geom_smooth(method = 'lm',color='seagreen3', fill= 'seagreen3') +
  theme_minimal() + 
  scale_x_continuous(name="Distance (km)") +
  scale_y_continuous(name="Ratio of Breeding Success to Failure")




################ Pheromonal Data ################
PheromoneData <- read.csv("Chemical_interest_trial_data_Runemark_et_al.csv", stringsAsFactors = F)

#### Cleaning Data
#No obvious issues. So I will check general data
summary(PheromoneData$Number.of.tongue.flicks.10min)
sum(is.na(PheromoneData))
#Seems good

#Identify the sites used for this experiment
Locations2 <- unique(c(unique(PheromoneData$Population.of.origin.of.smell.donor),
                       unique(PheromoneData$Population.of.origin.of.tongue.flicker)))
#This experiment uses four of the field site locations
#The control population is from P taurica but sometimes reported as Control and sometimes as P taurica
#Replace P. taurica with control
PheromoneData[PheromoneData=="P taurica"] <- "Control"

#Upon Further Examination of GPS corrdinates, Diabatez and Mesa Diabates are the same location
PheromoneData[PheromoneData=="Mesa Diavates"] <- "Diabatez"
#I also found Lakonissa is the same as Lakonisi and Lakonissi
PheromoneData[PheromoneData=="Lakonisi"] <- "Lakonissa"


#### Analysis
#First I need to analyze the control for how often they might just lick something
PheromoneData$Number.of.tongue.flicks.10min[PheromoneData$Population.of.origin.of.smell.donor=="Control"]
summary(PheromoneData$Number.of.tongue.flicks.10min[PheromoneData$Population.of.origin.of.smell.donor=="Control"])
#The data is divided into non gendered and gendered controls
maleControl <- PheromoneData$Number.of.tongue.flicks.10min[PheromoneData$Population.of.origin.of.smell.donor=="Control" &
                                                             PheromoneData$Sex.of.smell.donor =="Male"  ]
femaleControl <- PheromoneData$Number.of.tongue.flicks.10min[PheromoneData$Population.of.origin.of.smell.donor=="Control" &
                                                               PheromoneData$Sex.of.smell.donor =="Female"  ]
ungenderedControl <- PheromoneData$Number.of.tongue.flicks.10min[PheromoneData$Population.of.origin.of.smell.donor=="Control" &
                                                                  PheromoneData$Sex.of.smell.donor =="Control"  ]
summary(maleControl)
summary(femaleControl)
summary(ungenderedControl)
#Given that the ungendered uses has a far larger sample size, I will use its mean as my control.
ControlConst <- mean(ungenderedControl)
#Variation in the control:
ControlStderr <- sd(ungenderedControl)/(sqrt(length(ungenderedControl)))
#57.7 +- 5.8

#Now I will plot each value for licking ploted along distance minus the control
#With error bars around 0 (= to the control) for the standard error in the control

#index distances and licks for the second set of data
distances2 <- c()
licks <- c()
lickgender <- c()
donorgender <- c()
for (i in seq(1:dim(PheromoneData)[1])) {
  if ( c(PheromoneData$Population.of.origin.of.smell.donor[i]) == 'Control' | sub(" ", ".",c(PheromoneData$Population.of.origin.of.tongue.flicker[i])) == 'Control'  ) {
    #If either gender is a control more into the next step of the for loop
    next
  }
  distances2 <- c(distances2, DistanceData[[c(PheromoneData$Population.of.origin.of.smell.donor[i]),sub(" ", ".",c(PheromoneData$Population.of.origin.of.tongue.flicker[i]))]])
  #All in the loop to remove the control lick studies
  licks <- c(licks, PheromoneData$Number.of.tongue.flicks.10min[i])
  lickgender <- c(lickgender, PheromoneData$Sex.of.tongue.flicker[i])
  donorgender <- c(donorgender, PheromoneData$Sex.of.smell.donor[i])
}
#Correct for control
licks <- licks - ControlConst
#Combine into dataframe
LicksDistance <- data.frame(distance = distances2, interest = licks) #Licks indicate sexual interest

ggplot(LicksDistance, aes(distance,interest)) + 
  geom_point(color='sienna1') +
  #The control
  geom_hline(yintercept=0, color = 'black') +
  #The standard error of the control mean
  geom_ribbon(aes(ymin=-2*ControlStderr, ymax=2*ControlStderr), alpha=0.2) +  #2 times standard error is approximately 95% cofidence interval
  geom_smooth(method = 'lm',color='sienna3', fill= 'sienna2') + #Create a linear fit line 
  scale_x_continuous(name="Distance (km)") +
  scale_y_continuous(name="Number of Pheromone Licks")


#Calculate actual values for linear model
lm_fit2 <- lm(LicksDistance$interest ~ LicksDistance$distance)
summary(lm_fit2)
#P value is 0.002871

#This data can also be examined from the perspective of male vs female mouse interest
#Did they ever have a male smell from a male or a female from a female ect.?
sum(lickgender == donorgender)
#No

#Create a data frame indicating gender
LicksDistanceGendered <- data.frame(distance = distances2, interest = licks, Gender = lickgender) 

#Linear model fits
lm_fitfemale <- lm(LicksDistanceGendered$interest[LicksDistanceGendered$gender == 'Female'] ~ LicksDistance$distance[LicksDistanceGendered$gender == 'Female'])
summary(lm_fitfemale)

lm_fitmale <- lm(LicksDistanceGendered$interest[LicksDistanceGendered$gender == 'Male'] ~ LicksDistance$distance[LicksDistanceGendered$gender == 'Male'])
summary(lm_fitmale)

ggplot(LicksDistanceGendered, aes(distance,interest, color=Gender)) + 
  geom_point() +
  geom_hline(yintercept=0, color = 'black') +
  geom_ribbon(aes(ymin=-2*ControlStderr, ymax=2*ControlStderr), alpha=0.2, color=NA) + #2 times standard error is approximately 95% cofidence interval
  geom_smooth(method = lm, aes(color=Gender, fill=Gender)) + theme_minimal() + 
  scale_x_continuous(name="Distance (km)") +
  scale_y_continuous(name="Number of Pheromone Licks")
