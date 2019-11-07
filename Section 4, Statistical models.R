###### Section 4.1: Statistical models ######

library(dslabs)
library(dplyr)
data(heights)

#Let's revisit the heights dataset. For now, consider x to be the heights of all males in the data set.

x <- heights %>% filter(sex == "Male") %>%
  .$height

  #Q1: What are the population average and standard deviation?

mean(x)
sd(x)

  #Q2: Take a sample of size 50, with replacement, and construct an estimate for ?? and sigma

set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)
mean(X)
sd(X)

  #Q3: What does the central limit theory tell us about the sample average and how it is related to ??, the population average?
    #Answer: it is a random variable with expected value ?? and standard error sigma/sqrt N

  #Q4: Construct a 95% confidence interval for ??

set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)
se <- sd(X)/sqrt(N)
ci <- c(mean(X)- qnorm(0.975)*se, mean(X)+ qnorm(0.975)*se)

  #Q5: Run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have just done. What proportion of 
      #these intervals include ???

set.seed(1)
N <- 50
B <- 10000
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- between(mean(x), mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)
})

mean(res)

  #Q6: In this section, we used visualization to motivate the presence of pollster bias in election polls. Here we will examine 
      #that bias more rigorously. Lets consider two pollsters that conducted daily polls and look at national polls for the month
      #before the election. Is there a poll bias? Make a plot of the spreads for each poll.

library(ggplot2)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

polls %>% ggplot(aes(pollster, spread)) +
  geom_boxplot() + geom_point()

  #Q13: Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the
      #spread

sigma <- polls %>%
  group_by(pollster) %>%
  summarize(s = sd(spread))

  #Q15: Is b2 - b1 different from 0? Construct a 95% confidence interval for the difference b2 and b1. Does this interval 
      #contain zero?    Note: standard error for Y2-Y1 is sqrt(sd2^2/N + sd1^2/N)

res <- polls %>%
  group_by(pollster) %>%
  summarize(avg = mean(spread), s = sd(spread), n = n())

estimate <- max(res$avg) - min(res$avg)
se_hat <- sqrt(max(res$s)^2/max(res$n) + min(res$s)^2/min(res$n))

ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)

  #Q16: The confidence interval tells us there is relatively strong pollster effect resulting in a difference of
      #about 5%. Random variability does not seem to explain it. Compute a p-value to relay the fact that chance
      #does not explain the observed pollster effect.

res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

p <- 1 - pnorm(estimate, 0, se_hat)   #consider this a t-test where, under the null, pollster 1 and pollster 2
2*p                                   #are not sig. different. Therefore, ?? would be 0, the estimate or observed
                                      #difference is M, or in this case, the x argument in the pnorm function.
                                      #Finally, multiply p*2 for a two-tailed test

  #Q17: Compute the average and standard deviation for each pollster and examine the variability across the
      #averages and how it compares to the variability within the pollsters, summarized by the standard 
      #deviation.

polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

var <- polls %>% 
  group_by(pollster) %>%
  summarize(avg = mean(spread), s = sd(spread))
