## Logistic Growth Model

The discrete-time logistic growth model is used to predict population growth. It takes into account the initial population size, the intrinsic growth rate, and the carrying capacity of the system. This model is run over a discrete number of time steps (non-continuous). It is typically represented by the following equation:  
`n[t] = n[t-1] + (r * n[t-1] * (K - n[t-1]) / K)`   
Where `n` is the population abundance, `n[0]` is the initial population size, `r` is the instrinsic growth rate, `K` is the carrying capacity of the system, and `t` is the current generation.

The code in Lab8 runs a logistic growth model using a function. The only required input for the function is `n[0]`, the initial population. The function assumes that it will occur over 12 generations, at a growth rate of 0.8, for a system with a carrying capacity of 10,000 unless new input is added. This code was initially built from [Dr. Flaxman's instructions](https://github.com/flaxmans/CompBio_on_git/blob/master/Labs/Lab04/Lab04_ForLoops.md) and adapted from [my code](https://github.com/hannahhartung/CompBioLabsAndHomework/blob/master/Labs/Lab04/Lab4.R).

### Example run
To illustrate the viability of this code, I ran it for a system with the following inputs:  
`n[0]=200`  
`r=0.9,  K = 6000`
Yielding the results file in this repository named LogisticGrowthResults.csv. That file describes the system shown below:     

![Logistic Growth Example](https://github.com/hannahhartung/CompBioLabsAndHomework/blob/master/Labs/Lab08/Growth_Model.png)



This repository contains the submissions for **Hannah Hartung**.



