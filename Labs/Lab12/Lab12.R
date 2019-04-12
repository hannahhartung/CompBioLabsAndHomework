################# Lab 12 ##################
rm(list=ls())
setwd('~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab12')

library(ggplot2)
camData <- read.csv("Cusack_et_al_random_versus_trail_camera_trap_data_Ruaha_2013_14.csv", stringsAsFactors = F)

species <- unique(camData$Species)

library(dplyr)


## 1. Barplot
ggplot(camData) + geom_bar(aes(camData$Species))


## 2. Rotate labels
ggplot(camData) + geom_bar(aes(camData$Species)) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
#Where hjust is justification to the right, otherwise they are centered


## 3. Orientation, Scaling, Sorting
# Coord flip
ggplot(camData) + geom_bar(aes(camData$Species)) + 
  coord_flip()

# Scaling
ggplot(camData) + geom_bar(aes(camData$Species)) + 
  coord_flip() + 
  scale_y_continuous(name= 'Observations', trans='log10')
#Note: even though it is now x axis it is technically still y flipped.

#Sorting
#Using less tidyverse version
#Recorder works when you have a numerical value to order them by stored somewhere

#Table creates a set of counts.
ggplot(camData) + geom_bar(aes(reorder(camData$Species, table(camData$Species)[camData$Species]))) + 
  coord_flip() + 
  scale_y_continuous(name= 'Observations', trans='log10')

#Putting -table sorts them from lowest to highest
ggplot(camData) + geom_bar(aes(reorder(camData$Species, -table(camData$Species)[camData$Species]))) + 
  coord_flip() + 
  scale_y_continuous(name= 'Observations', trans='log10')

#Nice y axis label (old x axis)
ggplot(camData) + 
  geom_bar(aes(reorder(camData$Species, -table(camData$Species)[camData$Species]))) + 
  coord_flip() + 
  scale_y_continuous(name= 'Count', trans='log10') + 
  labs(x = "Species")
  