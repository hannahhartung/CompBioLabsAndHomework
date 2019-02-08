# Lab 4

## Part 1

#1.
for( i in 1:10 ) {
  print("hi")
}

#2. 
tim_money <- 10 #dollars
allowance <- 5 #$ per week
numbergum <- 2 #sticks bought per week
pricegum <- 1.34 #$ - Including tax

times <- seq(1:8) #weeks

for( week in times ) {
  new_money <- tim_money + allowance - numbergum * pricegum
  print(c("After week", week, "Tim has $", new_money))
  tim_money <- new_money #Set the amount he has at the start of next week
}


#3.
lossrate <- 0.05  # %5
current_pop <- 2000 #individuals in current population
times <- seq(7)

for( years in times ){
  new_pop <- trunc( current_pop * (1 - lossrate) ) #Trunc just for me: round to single individuals
  print(c(years,new_pop,"animals remaining"))
  current_pop <- new_pop #Set new population
}

#Could also be done by setting survivor percentage (to avoid magic #):
#And without truncating
lossrate <- 0.05  # %5
survivorship <- 1 - lossrate  # % that survive each year
current_pop <- 2000 #individuals in current population
times <- seq(7)

for( years in times ){
  new_pop <- current_pop * survivorship
  print(c(years,new_pop,"animals remaining"))
  current_pop <- new_pop #Set new population
}


#4. 
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


## Part 2

#5. a)
vect_a <- rep(0, 18)
print(vect_a)

#5. b)

for( i in seq(1,18) ){
  vect_a[i] <- 3 * i #multiply by 3
}
print(vect_a)

#5. c)
vect_c <- rep(0, 18)
vect_c[1] = 1 #set first value to 1
print(vect_c)


#5. d)
for( i in seq(2,18) ){  #Start with 2 to not replace the first number
  vect_c[i] <- 1 + 2 * vect_c[i-1]
}
print(vect_c)


#6. Fibonacci
Fib <- rep(1, 20) #Setting the initial numbers to 1 to save time
#Change the first value to 0 rather than change the second and third both to 1
Fib[1] <- 0 #modern usage of Fibonacci

for(i in seq(3,20) ){ #Start with 3 because 1 and 2 are required both for 3 (cannot solve for 1 or 2)
  Fib[i] <- Fib[i-1] + Fib[i-2]
  }
print(Fib)


#7. Expand on population model
#Already stored n (abundance) in a vector
print(n)
year <- seq(1,12)
#to graph with the automatic axis labels, I'll change n to abundance
abundance <- n
plot(year, abundance) 


## Part 3

#8. a)
setwd( "~/Desktop/CompBio/CompBioLabsAndHomework/Labs/Lab04" )
#8. b)
CO2 <- read.csv('CO2_data_cut_paste.csv', colClasses=c('integer','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

#8. c) Percentage change year to year
#There are 263 years (found by looking at Data description)
percents <- array(dim=c(263,8)) #empty array

for (i in 2:263) {
  for (j in 2:8) { #The number of categories other than years
    percents[i,j] <- ( CO2[i,j] / CO2[i-1,j] )
  }
  
}

#Set years
years_for_percent = 1751:2013 #From given

#Build  a data frame so I can add appropriate titles
percent_change <- data.frame("Year" = years_for_percent,
                             "Total" = percents[,2], 
                             "Gas" = percents[,3], 
                             "Liquid" = percents[,4],
                             "Solid" = percents[,5], 
                             "CementProduction" = percents[,6],
                             "GasFlaring" = percents[,7],
                             "Per Capita" = percents[,8])


#8. d) output to CSV
write.csv(percent_change, file="YearlyPercentChangesInCO2.csv")


#8. e) Change from year 1
changefrom1 <- array(dim=c(263,8)) #empty array

for (i in 2:263) {
  for (j in 2:8) { #The number of categories other than years
    changefrom1[i,j] <- ( CO2[i,j] / CO2[1,j] )
  }
  
}

#Build  a data frame so I can add appropriate titles
percent_changefrom1 <- data.frame("Year" = years_for_percent, 
                                  "Total" = changefrom1[,2], 
                              "Gas" = changefrom1[,3], 
                             "Liquid" = changefrom1[,4],
                             "Solid" = changefrom1[,5], 
                             "CementProduction" = changefrom1[,6],
                             "GasFlaring" = changefrom1[,7],
                             "Per Capita" = changefrom1[,8])
#Write csv
write.csv(percent_changefrom1, file="PercentChangesInCO2Since1751.csv")
#note: the full per capita is NaN because 1751 is NaN so it is never calculable. 

