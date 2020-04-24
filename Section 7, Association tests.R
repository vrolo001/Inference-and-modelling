### Section 7: Association Tests ###

  #Q1:In a previous exercise we determine whether each poll preicted the correct winner for their state in the 2016
      #US election. We will determine if polls rated A- made better predictions than polls rated C-. Filter the errors
      #data for just polls with grades A- and C-. Calculate the proportion of times each grade of poll predicted the
      #correct winner
library(dplyr)
library(dslabs)
library(tidyverse)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
cis <- polls %>%
  mutate(X_hat = (spread + 1)/2,
         se = 2*sqrt(X_hat * (1 - X_hat)/samplesize),
         lower = spread - qnorm(0.975)*se,
         upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")
errors <- ci_data %>%
  mutate(error = spread - actual_spread,
         hit = ifelse(sign(spread) == sign(actual_spread), TRUE, FALSE))


totals <- errors %>%
  filter(grade == "A-" | grade == "C-") %>%
  group_by(grade) %>%
  summarise(correct = sum(hit), incorrect = length(hit) - sum(hit))

totals %>%
  summarise(prop_A = sum(correct[2])/(sum(correct[2]) + sum(incorrect[2]))) %>%
  .$prop_A

totals %>%
  summarise(prop_C = sum(correct[1])/(sum(correct[1]) + sum(incorrect[1]))) %>%
  .$prop_C

#answer per data camp

totals <- errors %>%
  filter(grade %in% c("A-", "C-")) %>%
  group_by(grade,hit) %>%
  summarize(num = n()) %>%
  spread(grade, num)
totals[[2,3]]/sum(totals[[3]])
totals[[2,2]]/sum(totals[[2]])

  #Q2: Use a Chi-square test to determine if the proportion of correct prediction between A- and C- polls are different

chi_test <- totals %>%
  select(-hit) %>%
  chisq.test()
chi_test$p.value

  #Q3: Calculate the odds ratio to determine the magnitude of the difference in perofrmance between grades A- and C-
                    
odds_C <- (totals[2,2]/sum(totals[,2]))/(totals[1,2]/sum(totals[,2]))
odds_A <- (totals[2,3]/sum(totals[,3]))/(totals[1,3]/sum(totals[,3]))
odds_A/odds_C                #this gives a data frame structure
odds_A[1,1]/odds_C[1,1]      #this gives a numeric structure, which may be more practical
