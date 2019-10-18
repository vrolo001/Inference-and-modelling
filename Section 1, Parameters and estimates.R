library(tidyverse)
library(dslabs)
take_poll(25)

###### Section 1.1: Parameters and estimates

  #Q1: Suppose you poll a population in which a proportion p of voters are Democrats and 1???p are Republicans. Your sample size
      #is N=25. Consider the random variable S, which is the total number of Democrats in your sample. What is the expected 
      #value of this random variable S? Answer: E(S) = 25p

  #Q2: What is the standard error of S? Answer: SE(S) = sqrt(25*p*(1-p))

  #Q3: Consider the random variable S/N, which is equivalent to the sample average that we have been denoting as X¯.  
      #What is the expected value of X¯?  Answer: E(X¯) = p

  #Q4: What is the standard error of the sample average, X¯?  Answer: SE(X¯) = sqrt(p*(1-p)/N)

  #Q5: Write a line of code that calculates the standard error se of a sample average when you poll 25 people in the 
      #population. Generate a sequence of 100 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats).
      #Plot se versus p for the 100 different proportions

p <- seq(0,1, length = 100)
se <- sqrt(p*(1-p)/25)
plot(p, se)

  #Q6: Using the same code as in the previous exercise, create a for-loop that generates three plots of p versus se when the
      #sample sizes equal N=25, N=100, and N=1000.

par(mfrow=c(1,3))         #puts plots into 1 row and 3 columns

p <- seq(0,1, length = 100)

sample_sizes <- c(25, 100, 1000)

for (N in sample_sizes) {
  se <- sqrt(p*(1-p)/N) 
  plot(p, se, ylim = c(0, 0.10))
}

  #Q7: Our estimate for the difference in proportions of Democrats and Republicans is d=X¯???(1???X¯).
      # Which derivation correctly uses the rules we learned about sums of random variables and scaled random variables to
      #derive the expected value of d?  Answer: d) E[X¯???(1???X¯)]=E[2X¯???1] =2E[X¯]???1 =2p???1 =p???(1???p)

  #Q8: Which derivation correctly uses the rules we learned about sums of random variables and scaled random variables to derive
      #the standard error of d?   Answer: SE[X¯???(1???X¯)]=SE[2X¯???1] =2SE[X¯] =2*sqrt(p*(1-p)/N)

  #Q9: Say the actual proportion of Democratic voters is p=0.45. In this case, the Republican party is winning by a relatively
      #large margin of d=???0.1, or a 10% margin of victory. What is the standard error of the spread 2X¯???1 in this case?

p <- 0.45
2*sqrt(p*(1-p)/25)

  #Q10: So far we have said that the difference between the proportion of Democratic voters and Republican voters is about 10%
      #and that the standard error of this spread is about 0.2 when N=25. Select the statement that explains why this sample 
      #size is sufficient or not.

      #This sample size is too small because the standard error 2*sqrt(p*(1-p)/25) is larger than the spread of 2*p-1.
