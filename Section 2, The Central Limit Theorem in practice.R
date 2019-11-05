###### Section 2.1: The Central Limit Theorem in practice ######

  #Q1: Write a function called take_sample that takes the proportion of Democrats p and the sample size N as arguments and
      #returns the sample average of Democrats (1) and Republicans (0). Calculate the sample average if the proportion of
      #Democrats equals 0.45 and the sample size is 100.

set.seed(1)
p <- 0.45
take_sample <- function(p, N) {
  mean(sample(c(1, 0), N, replace = TRUE, prob = c(p, 1-p)))
}
take_sample(0.45, 100)


  #Q2: The take_sample function you defined previously generates our estimate, Xbar.Replicate the random sampling 10,000 times
      #and calculate p ??? Xbar for each random sample. Save these differences as a vector called errors. Find the average of 
      #errors and plot a histogram of the distribution.

set.seed(1)
B <- 10000
errors <- replicate(B, {
  errors <- p - take_sample(0.45, 100)
})
mean(errors)
hist(errors)

  #Q3: Look at the histogram for errors. Which statement best describes the distribution of the errors?
      
      #The errors are symmetrically distributed around 0.

  #Q4: The error p ??? Xbar is a random variable. In practice, the error is not observed because we do not know the actual
      #proportion of Democratic voters, p. However, we can describe the size of the error by constructing a simulation.
      #What is the average size of the error if we define the size by taking the absolute value ???p ??? Xbar??? ?

mean(abs(errors))

  #Q5: The standard error is related to the typical size of the error we make when predicting. We say size because, as we just
      #saw, the errors are centered around 0. In that sense, the typical error is 0. For mathematical reasons related to the
      #CLT, we actually use the standard deviation of errors rather than the average of the absolute values.
      #As we have discussed, the standard error is the square root of the average squared distance (Xbar ??? p)2. The standard
      #deviation is defined as the square root of the distance squared. Calculate the standard deviation of the spread.

sqrt(mean(errors^2))

  #Q6: Estimate the standard error given an expected value of 0.45 and a sample size of 100.

sqrt(0.45*(1-0.45)/100)

  #Q7: In practice, we don't know p, so we construct an estimate of the theoretical prediction based by plugging in Xbar for p.
      #Calculate the standard error of the estimate SEhat(Xbar).

set.seed(1)
sqrt(take_sample(0.45, 100)*(1-take_sample(0.45, 100))/100)

  #Q8: Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, how large does the 
      #sample size have to be to have a standard error of about 1%?

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)

plot(N, se)  #therefore, the sample size must be of approx 2500 for the se to be about 1%

  #Q9: For N=100, the central limit theorem tells us that the distribution of X^ is...
      #Answer: approximately normal with expected value p and standard error sqrt (p*(1-p)/N)

  #Q10: We calculated a vector errors that contained, for each simulated sample, the difference between the actual value p and
       #our estimate Xhat. The errors Xbar - p are:
       #Answer: approximately normal with expected value 0 and standard error sqrt(p*(1-p)/N)

  #Q11: Make a qq-plot of the errors you generated previously to see if they follow a normal distribution.

set.seed(1)
B <- 10000
errors <- replicate(B, {
  errors <- p - take_sample(0.45, 100)
})

qqnorm(errors) 
qqline(errors)

  #Q12: If p=0.45 and N=100, use the central limit theorem to estimate the probability that Xbar > 0.5

1-pnorm(0.5, p, sqrt(p*(1-p)/N))

  #Q13: Assume you are in a practical situation and you don't know p. Take a sample of size N=100 and obtain a sample average of
      #Xbar = 0.51. What is the CLT approximation for the probability that your error size is equal or larger than 0.01?

X_bar <- 0.51
se_hat <- sqrt(X_bar * (1-X_bar)/100)

p_less_neg_01 <- pnorm(-0.01/se_hat)
p_greater_pos_01 <- 1- pnorm(0.01/se_hat) 

p_less_neg_01 + p_greater_pos_01
