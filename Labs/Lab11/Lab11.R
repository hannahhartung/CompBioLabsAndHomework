#### Lab 11
rm(list=ls())
setwd('~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab11')

#################
# How does wood density vary across families of tree species?
################

### Part 1
WoodDensity = read.csv("GlobalWoodDensity.csv", stringsAsFactors = F)

###############
### Part 2
## 4. a) 
which(is.na(WoodDensity$Wood.density..g.cm.3...oven.dry.mass.fresh.volume))
# position 12150

## 4. b) 
# All positions which are not NA
notNA <- which(!is.na(WoodDensity$Wood.density..g.cm.3...oven.dry.mass.fresh.volume))
WoodDensity <- WoodDensity[notNA,]

## 5. 
species <- unique(WoodDensity$Binomial)

# Test for simple example:
# species <- head(species)

# Create empty matrix species x variables
UniqueWoodDensity <- matrix(nrow = length(species), ncol = 3)
#Collumns: 1 = Family, 2 = Species, 3 = Density

for ( i in seq(1,length(species))){
  binom <- species[i]
  #Find which indices in the big matrix represent this species
  indices <- which(WoodDensity$Binomial == binom)
  #Create empty array of densities
  speciesDensities <- c()
  for ( j in indices){
    #Concatenate density for this ref to densities for species as a whole
    speciesDensities <- c( speciesDensities, WoodDensity$Wood.density..g.cm.3...oven.dry.mass.fresh.volume[j] )
  }
  #Mean of densities
  meanDensity <- mean(speciesDensities)
  #Insert all three values
  UniqueWoodDensity[i,] <- c(WoodDensity$Family[min(indices)], WoodDensity$Binomial[min(indices)],meanDensity)
}

#Check size of matrix
dim(UniqueWoodDensity)
#Good!

## BONUS 5
library(dplyr)
# I googled dlpyr use in general and used the RDocumentation to figure this out
meanDensities2 <- WoodDensity %>%
  group_by(Binomial) %>%
  summarise(n = n(),
            Wood.density..g.cm.3...oven.dry.mass.fresh.volume = mean(Wood.density..g.cm.3...oven.dry.mass.fresh.volume),
            Family = Family[1])
            # Might be sloppy but I add in the Family here so I can pull straight from here for creating my matrix
            # Family[1] means the family belonging to the first of the matching binomials (they should all be the same)

#Format into identical matrix
UniqueWoodDensity2 <- matrix(data = c(meanDensities2$Family, meanDensities2$Binomial, meanDensities2$Wood.density..g.cm.3...oven.dry.mass.fresh.volume),
                             nrow = dim(meanDensities2)[1], ncol = 3)

sum(UniqueWoodDensity != UniqueWoodDensity2)
# They are identical!

## 6 a)
# Note: cannot use dplyr easily on matrices. Pratice converting matrix to dataframe
SpeciesWoodDensity <- as.data.frame(UniqueWoodDensity, stringsAsFactors = F)
colnames(SpeciesWoodDensity) <- c('Family', 'Binomial', 'WoodDensity_gcm3')

# Converting to dataframe made them all characters. Convert back to numerical:
SpeciesWoodDensity$WoodDensity_gcm3 <- as.numeric( SpeciesWoodDensity$WoodDensity_gcm3 )

FamilyWoodDensity <- SpeciesWoodDensity %>%
  group_by(Family) %>%
  summarise(WoodDensity_gcm3 = mean(WoodDensity_gcm3))

# 191 long! Looks good!

## 6. b)
FamilyWoodDensity %>% arrange(-WoodDensity_gcm3)
# Arranging by negative wood density puts the most dense at top.

## 6. c)
highest8 <- (FamilyWoodDensity %>% arrange(-WoodDensity_gcm3) %>% head(Family,n=8))[,1]
lowest8 <- (FamilyWoodDensity %>% arrange(WoodDensity_gcm3) %>% head(Family,n=8))[,1]

##############

### Part 3
library(ggplot2)

## 7.
highest8 <- as.matrix(highest8)
lowest8 <- as.matrix(lowest8)


for ( i in seq(1, dim(highest8)[1])){
  FamDensity <- as.data.frame(WoodDensity %>% filter(Family == highest8[i]))
  boxplot( FamDensity$Wood.density..g.cm.3...oven.dry.mass.fresh.volume )
}


WoodDensity %>% filter(Family == 'Acanthaceae')

