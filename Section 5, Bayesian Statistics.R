##### Section 5.1: Bayesian Statistics #####

  #Q2: Assume that there is a genetic component to SIDS and the probability of Pr(second case|first case)=1/100,
      #which is much higher than the Pr of a first case (1 in 8500). What is the probability of both of Sally
      #Clark's sons dying of SIDS?

Pr_1 <- 1/8500  #Rate of SIDS 1st case (A)
Pr_2  <- 1/100   #Rate of SIDS 2nd case (B|A)

      #Pr(A&B) = Pr(B|A)*Pr(A), therefore:
Pr_AB <- Pr_2 * R_SIDS1

  #Q3: Reports stated that the expert claimed the Pr of Clark being innocent is 1 in 73 million. Perhaps the jury
      #and judge also interpreted the testimony this way: Pr(mom is murderer|2 sons found dead w/no evidence of harm)
      #Bayes' rules tells us this probability is equal to:
      #Answer: Pr(2 sons found dead|mom is murderer)*Pr(mom is murderer)/Pr(2 sons dead)

  #Q4: Assume the Pr of a murderer killing her 2 kids without leaving evidence is 0.50. Assume that the murder rate
      #among moms is 1 in 1,000,000. Use Baye's rule to calculate the Pr that the mother is a murderer given the 2 sons 
      #were found dead without evidence of harm

Pr_1 <- 1/8500    #Pr 1st son dying of SIDS
Pr_2 <- 1/100     #Pr 2nd son dies of SIDS
Pr_B <- Pr_1*Pr_2 #Pr both sons die of SIDS
Pr_A <- 1/10000001#Pr mothers are murderers
Pr_BA <- 0.50     #Pr 2 sons died given mom is the murderer
Pr_AB <- Pr_BA*Pr_A/Pr_B
Pr_AB

  #Q6: Create a table with the poll spread results from Florida taken during the last days before the election using the
      #code provided. Calculate a spread average and provide an estimate of the standard error

library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

results <- polls %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))
results

  #Q8: Use the formulas for the posterior distribution to calculate the expected value of the posterior distribution if
      #we set miu = 0 and tau = .01

miu <- 0
tau <- .01
sigma <- results$se
Y <- results$avg

B <- sigma^2/(sigma^2 + tau^2)
p <- (B*miu) + (1-B)*Y

  #Q9: Compute the standard error of the posterior distribution

se <- sqrt(1/(1/sigma^2 + 1/tau^2))

  #Q10: Create credible intervals that have a 95% chance of occurring centered at the posterior expected value.

ci<- c(p - qnorm(.975)*se, p + qnorm(.975)*se)

  #Q11: According to this analysis, what was the Pr that Trump won Florida?
pnorm(0,p,se)

  #Q12: Change the prior variance tau to include values ranging from 0.005 to 0.05 and observe how the probability of 
        #Trump winning Florida changes by making a plot.

taus <- seq(0.005, 0.05, len = 100)
p_calc <- function(tau){
  B <- sigma^2/(sigma^2 + tau^2)
  p <- (B*miu) + (1-B)*Y
  se <- sqrt(1/(1/sigma^2 + 1/tau^2))
  pnorm(0,p,se)
}
  
ps <- p_calc(taus) 
plot(taus, ps)



