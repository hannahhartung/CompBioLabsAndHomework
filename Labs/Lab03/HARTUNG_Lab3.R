#Star Wars Movie Watching Party!!

##PART 1
#lab step #3 (& #4)
#Identifying the number of cheetos and guests
chips <- 5 #number of cheetos bags
guests <- 8

#lab step #5 (& #6)
#Define number of bags eaten by each person
bagsperguest <- 0.4

#lab step #7
#I am also eating chips
people <- guests + 1

#Remaining chips = bags of chips - people*chips/person
leftover = chips - (people*bagsperguest)
#I have 1.4 bags left

##PART 2
#lab step 8
#define rankings
me <- c(7,6,5,1,2,3,4) #not accurate
Penny <- c(5,7,6,3,1,2,4)
Jenny <- c(4,3,2,7,6,5,1)
Lenny <- c(1,7,3,4,6,5,2)
Stewie <- c(6,7,5,4,3,1,2)

#lab step 9
PennyIV <- Penny[4] #How Penny feels about episode 4
LennyIV <- Lenny[4] #How Lenny feels about episode 4

#lab step 10
#combine all the rankings of all films
all = cbind(me, Penny, Jenny, Lenny, Stewie)
# Stores in dataframe rather than matrix

# For future reference: transpose "t()" turns the rows into collumns

#lab step 11
str(PennyIV) #A number
str(Penny) #A vector of length 7
str(all) #A a matrix of which is 5x7

#lab step 12
all2 <- data.frame(me, Penny, Jenny, Lenny, Stewie)
#groups the vectors
as.data.frame(all)
#displays error if I try to input the vectors, but accepts the matrix input

#lab step 13
dim(all)
dim(all2)
# Both objects have the same dimensions
typeof(all)
typeof(all2)
#Type of the one created in cbind (matrix) is called a "double"
#Whereas the data frame is called a list
str(all)
str(all2)
#The data frame knows that it contains 7 observations from 5 different variables
#Whereas the matrix considers them all together but lables the headers too
all == all2
#Eventhough they are different classes of objects, R can recognize that they contain the same information

#lab step 14
episodename <- c("I", "II", "III", "IV", "V", "VI", "VII")
#made a vector containing each of the episode numbers

#lab step 15
help(row.names)
row.names(all2) <- episodename 
row.names(all) <- episodename
#change the row lables to the numbers for the movies

#lab step 16
#Episode 3 rankings (from matrix)
all[3,]

#lab step 17
#Episode 4 rankings (from dataframe)
all2[4,]

#note: in the display from dataframe row it displayed title of row (IV), for matrix it did not (eventhough matrix has row lables)

#lab step 18
all[1,5]
# "I" gave episode 5 a 6 which is close to accurate with respect to my real world oppinions 

#lab step 19
all[2,2]
#Penny gave episode 2 a 7

#lab step 20
#Rankings for epsiodes 4-6
all[c(4,5,6),] #episode IV, V and VI

#lab step 21
#Rankings for episodes 2,5, and 7
all[c(2,5,7),] #episode II

#lab step 22
all[c(4,6),c(2,3,5)]

#lab step 23
#read ahead and saw that you can use headers to designate location
#save Lenny 2nd ranking to new variable
oldL2 <- all[2,"Lenny"]
#Put lenny 5 into lenny 2
all[2,"Lenny"] <- all[5,"Lenny"]
#put old lenny 2 as new lenny 5
all[5,"Lenny"] <- oldL2
#check
all[2,"Lenny"]
all[5,"Lenny"] 
#Switched Lenny's ranking for movie 2 and movie 5

#lab step 24
all["III","Penny"] #matrix
all2["III","Penny"] #data frame
#Both returned the same answer: 6

#lab step 25
#save Lenny 5th ranking to new variable
oldLenny5 <- all["V","Lenny"]
#Put lenny 2 into lenny 5
all["V","Lenny"] <- all["II","Lenny"]
#put old lenny 5 as new lenny 2
all["II","Lenny"] <- oldLenny5
#check
all["V","Lenny"]
all["II","Lenny"]
#reverted back to the way it was in the given table

#lab step 26
#this method does not work for matrix
#put current lenny 5 into new variable
newLen2 <- all2$Lenny[5]
#put lenny 2 as new lenny 5
all2$Lenny[5] <- all2$Lenny[2]
#put new value for lenny 2 in lenny 2
all2$Lenny[2] <- newLen2
#check
all2$Lenny[2]
all2$Lenny[5]
#switched from orginal table

#done!