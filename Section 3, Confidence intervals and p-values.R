###### Section 3.1: Confidence intervals and p-values ######

#For the following exercises, we will use actual poll data from the 2016 election. The exercises will contain pre-loaded data
#from the dslabs package.  

library(dslabs)
library(tidyverse)
data("polls_us_election_2016")

  #Q1: We will use all the national polls that ended within a few weeks before the election. Assume there are only two candidates
      #and construct a 95% confidence interval for the election night proportion p.

polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.")

nrow(polls)
N <- polls$samplesize[1]
X_hat <- polls$rawpoll_clinton[polls$samplesize == N]/100
se_hat <- sqrt(X_hat*(1-X_hat)/N)
ci <- c(X_hat - qnorm(0.975)* se_hat, X_hat + qnorm(0.975)*se_hat)

  #Q2: Create a new object called pollster_results that contains the pollster's name, the end date of the poll, the proportion of
      #voters who declared a vote for Clinton, the standard error of this estimate, and the lower and upper bounds of the 
      #confidence interval for the estimate.

pollster_results <- polls %>%
  select (pollster, enddate, samplesize, rawpoll_clinton) %>%
  mutate(X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat,
         upper = X_hat + qnorm(0.975)*se_hat) %>%
  select(pollster, enddate, X_hat, se_hat, lower, upper)

  #Q3: The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. Add a column called hit to pollster_results
      #that states if the confidence interval included the true proportion p=0.482 or not. What proportion of confidence
      #intervals included p?


avg_hit <- pollster_results %>%
  mutate(p = 0.482, hit = ifelse(lower< p & upper > p, 1, 0)) %>%
  summarize(mean(hit))
       

  #Q4: If these confidence intervals are constructed correctly, and the theory holds up, what proportion of confidence 
      #intervals should include p?   Answer: 0.95

  #Q5: A much smaller proportion of the polls than expected produce confidence intervals containing p. Notice that most
      #polls that fail to include p are underestimating. The rationale for this is that undecided voters historically divide
      #evenly between the two main candidates on election day. In this case, it is more informative to estimate the spread
      #or the difference between the proportion of two candidates d, or 0.482???0.461=0.021 for this election. Assume that 
      #there are only two parties and that d=2p???1. Construct a 95% confidence interval for difference in proportions on 
      #election night.

polls <- polls %>%
  mutate(d_hat = (rawpoll_clinton - rawpoll_trump)/100)
N <- polls$samplesize[1]

d_hat <- polls$d_hat[1]
d_hat

X_hat <- (d_hat+1)/2

se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat

ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)

  #Q6: Create a new object called pollster_results that contains the pollster's name, the end date of the poll, the 
      #difference in the proportion of voters who declared a vote either, and the lower and upper bounds of the confidence
      #interval for the estimate.

pollster_results <- polls %>%
  mutate(d_hat = (rawpoll_clinton - rawpoll_trump)/100,
         X_hat = (d_hat+1)/2,
         se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = d_hat - qnorm(0.975)*se_hat,
         upper = d_hat + qnorm(0.975)*se_hat) %>%
  select(pollster, enddate, d_hat, lower, upper)
  
  #Q7: What proportion of confidence intervals for the difference between the proportion of voters included d, the actual
      #difference in election day?

avg_hit <- pollster_results %>%
  mutate(d = 0.021, hit = ifelse(lower < d & upper > d, 1, 0)) %>%
  summarize(mean(hit))

  #Q8: Although the proportion of confidence intervals that include the actual difference between the proportion of voters
      #increases substantially, it is still lower that 0.95. In the next chapter, we learn the reason for this. To motivate
      #our next exercises, calculate the difference between each poll's estimate d_bar and the actual d=0.021. Stratify 
      #this difference, or error, by pollster in a plot.

d <- 0.021 

polls %>% 
  mutate(errors = d_hat - d) %>%
  ggplot(aes(pollster, errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  #Q9: Remake the plot you made for the previous exercise, but only for pollsters that took five or more polls.

polls %>% 
  mutate(errors = d_hat - d) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(pollster, errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
