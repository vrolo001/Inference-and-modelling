##### Section 6.1: Election Forecasting #####

library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

  #Q1: For each poll in the polling data set, use the CLT to create a 95%CI for the spread. Create a new table 
      #called cis that contains columns for the lower and upper limits of the CIs
  
cis <- polls %>%
  mutate(X_hat = (spread + 1)/2,
         se = 2*sqrt(X_hat * (1 - X_hat)/samplesize),
         lower = spread - qnorm(0.975)*se,
         upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

  #Q2:Add the final election result to the cis table you just created using the left_join function as shown in the 
      #sample code below. Now determine how often the 95%CI includes the actual results

add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")

p_hits <- ci_data %>%
  mutate(hit = ifelse(actual_spread >= lower & actual_spread <= upper, TRUE, FALSE)) %>%
  summarise(mean(hit))

  #Q3: Find the proportion of hits for each pollster. Show only pollsters with at least 5 polls and order them from
      #best to worst. Show the number of polls conducted by each pollster and the FiveThirtyEight grade of each
      #pollster

p_hits <- ci_data %>%
  mutate(hit = ifelse(actual_spread >= lower & actual_spread <= upper, TRUE, FALSE)) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>% 
  arrange(desc(proportion_hits))
  
  #Q4:Repeat the previous exercise with the ci_data data, but stratify by state rather than pollster.Do not show grades

p_hits <- ci_data %>%
  mutate(hit = ifelse(actual_spread >= lower & actual_spread <= upper, TRUE, FALSE)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit), n = n()) %>% 
  arrange(desc(proportion_hits))

  #Q5: Make a barplot based on the result from the previous exercise

library(tidyverse)
p_hits %>%
  ggplot(aes(reorder(state, proportion_hits), proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

  #Q6:Add two columns to ci_data by computing, for each poll, the difference between the predicted spread and the
      #actual spread, and define a column hit that is true if the signs are the same

errors <- ci_data %>%
  mutate(error = spread - actual_spread,
         hit = ifelse(sign(spread) == sign(actual_spread), TRUE, FALSE))

  #Q7: Create an object called p_hits that contains the proportion of instances when the sign of the actual spread
      #matches the predicted spread for states with 5 or more polls. Make a barplot base don the result form the
      #previous exercise that shows the proportion of times the sign of the sptread matched the actual results for 
      #the data in p_hits

p_hits <- errors %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit), n = n())

p_hits %>% 
  ggplot(aes(reorder(state, proportion_hits), proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

  #Q8: Make a histogram of the state errors. What is the median of these errors?

hist(errors$error)
median(errors$error)

  #Q9: Create a boxplot to examine if the bias towards Clinton was general to all states or if it affected some
      #states differently. Filter the data to include only pollsters with grades B+ or higher

errors %>%
  filter(grade == "A+" | grade == "A" | grade == "A-" | grade == "B+") %>%
  ggplot(aes(reorder(state, error), error)) +
  geom_boxplot() +
  geom_point()

  #Q10: Repeat the previous exercise but only include states with five good polls or more
 
errors %>%
  filter(grade == "A+" | grade == "A" | grade == "A-" | grade == "B+") %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  ggplot(aes(reorder(state, error), error)) +
  geom_boxplot() +
  geom_point()





