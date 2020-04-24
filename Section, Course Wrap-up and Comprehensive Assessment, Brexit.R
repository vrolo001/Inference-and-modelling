### Course Wrap-up and COmprehensive Assessment: Brexit ###

##PART 1:

#Import the brexit_polls polling data from the dslabs package and set options for the analysis:
library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

#Define p = .481 as the actual percent voting "Remain" and d = 2p-1 = -.038 as the actual spread, with Remain defined
#as the positive outcome
p <- .481
d <- 2*p-1

  #Q1: The final proportion of voters choosing to Remain was p = 0.481. Consider a poll with sample size 1500 voters...

    #What is the expected total number of voters in the sample choosing Remain?
      N <-1500
      NX_hat <- N*p
    #What is the standard error of the total number of voters in the sample choosing Remain
      Nse_hat <- sqrt(N*p * (1-p))
    #What is the expected value of X, the proportion of Remain voters?
      X_hat <- p
    #What is the standard error of X, the proportion of Remain voters?
      se_hat <- sqrt(p*(1-p)/N)
    #What is the expected value of d, the spread between the proportion of Remain voters and Leave voters?
      d_hat <- 2*p -1
    #What is the standard error of d, the spread between the proportion of Remain voters and Leave voters?
      se_hatd <- 2*sqrt(p*(1-p)/N)
      
  #Q2:Calculate X_hat for each poll in the brexit_polls dataset, the estimate of the proportion of voters choosing
    #Remain (p = 0.481), given the observed spread. Use mutate to add a variable X_hat to the data  
      brexit_polls <- brexit_polls %>%
        mutate(X_hat = (spread + 1)/2)
    #What is the average of the observed spread?
      mean(brexit_polls$spread)
    #What is the standard deviation of the observed spreads?
      sd(brexit_polls$spread)
    #What is the average of X_hat, the estimates of the parameter p ?
      mean(brexit_polls$X_hat)
    #What is the standard deviation of X_hat?
      sd(brexit_polls$X_hat)
  
  #Q3: Confidence interval of a Brexit poll. Consider the first poll in brexit_polls, a YouGov pull run on the same
      #day as the referendum
      brexit_polls[1,]
    #Use qnorm() to compute the 95%CI for X_hat
      CI <- brexit_polls[1,] %>%
        mutate(lower = X_hat - qnorm(.975)*sqrt(X_hat * (1-X_hat)/samplesize),
               upper = X_hat + qnorm(.975)*sqrt(X_hat * (1-X_hat)/samplesize))
    #Does the 95%CI predict a winner? Does it cover the true value of p observed during the referendum?
      #Answer:The interval predicts a winner but does not cover the true value of p
      
##PART 2:
      
  #Q4: Confidence intervals for polls in June: Create the data frame june_polls containing only Brexit polls ending
      #June 2016, aka (enddate of "2016-06-01" and later). Calculate the CIs for all polls and determine how many
      #cover the true value of d
      
      june_polls <- brexit_polls %>%
        filter(enddate >= "2016-06-01") %>%
        mutate(se_x_hat = sqrt(X_hat * (1-X_hat)/samplesize),
               se_spread = 2*se_x_hat,
               lower = spread - qnorm(.975)*se_spread,
               upper = spread + qnorm(.975)*se_spread,
               hit = lower <= d & upper >= d)
    #How many polls are in june_polls?
      nrow(june_polls)
    #What proportion of polls have a CI that covers the value of 0?
      june_polls %>%
        summarise(cover_zero = mean(lower <= 0 & upper >= 0))
    #What porpotion of polls predict Remain (CI above 0)?
      june_polls %>%
        summarise(remain = mean(lower >= 0 & upper >= 0))
    #What poprotion of polls have a CI covering the true value of d?
      mean(june_polls$hit)
  
  #Q5: Hit rate by pollster. Group and sumarise the june_polls object by pollster to find the proporiton of hits 
      #for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate
      june_polls %>%
        group_by(pollster) %>%
        summarise(proportion_hits = mean(hit), n = n()) %>%
        arrange(proportion_hits)
    #Which of the following are TRUE?
      #Unbiased polls and pollsters will theoretically cover the correct value of the spread 50% of the time. (FALSE)
      #Only 1 pollster had a 100% success rate in generating CIs that covered the correct value of the spread. (FALSE)
      #The pollster w/the highest number of polls covered the correct value of the spread in their CI over 
        #60% of the time. (FALSE)
      #All pollsters produced CIs covering the correct spread in at least 1 of their polls. (FALSE)
      #The results are consistent with a large general bias that affects all pollsters. (TRUE)
      
  #Q6:Boxplot of Brexit polls by poll type. Make a boxplot of the spread in june_polls by poll type.      
       june_polls %>%
         ggplot(aes(poll_type, spread)) +
         geom_boxplot()
    #Which of the following are TRUE?
       #Online polls tend to show support for Remain (spread > 0). (FALSE)
       #Telephone polls tend to show support Remain (spread > 0). (TRUE)
       #Telephone polls tend to show higher support for Remain than online polls (higher spread). (TRUE)
       #Online polls have a larger interquartile range for the spread than telephone polls, indicating 
        #that they are more variable. (TRUE)
       #Poll type introduces a bias that affects poll results. (TRUE)
        
  #Q7: Combined spread across poll type. Calculate the confidence intervals of the spread combined across
       #all polls in june_polls, grouping by poll type.Use the code below, which determines the total sample
       #size per poll type; gives each spread estimate a weight based on the poll's sample size; and adds an
       #estimate of p from the combined spread, to begin your analysis
       combined_by_type <- june_polls %>%
         group_by(poll_type) %>%
         summarize(N = sum(samplesize),
                   spread = sum(spread*samplesize)/N,
                   p_hat = (spread + 1)/2)
       
       combined_by_type <- combined_by_type %>%
         mutate (se_spread = 2 * sqrt(p_hat * (1-p_hat)/N),
                 lower = spread - qnorm(.975)*se_spread,
                 upper = spread + qnorm(.975)*se_spread)
       #What is the lower bound of the 95% confidence interval for online voters?
       combined_by_type[1,6]
       #What is the upper bound of the 95% confidence interval for online voters?
       combined_by_type[1,7]
    
  #Q8: Interpreting combined spread estimates across poll type. Interpret the CIs for the combined spreads for each
       #poll type calculated in the previous problem. Which of the following are TRUE about the CIs of the combined
       #spreads for different poll types?
        
       #Neither set of combined polls makes a prediction about the outcome of the Brexit referendum (a prediction is 
        #possible if a confidence interval does not cover 0). (TRUE)
       #The CI for online polls is larger than the confidence interval for telephone polls. (FALSE)
       #The CI for telephone polls covers more positive values than the confidence interval for online polls. (TRUE)
       #The CIs for different poll types do not overlap. (FALSE)
       #Neither CI covers the true value d = -.038. (TRUE)
       
## PART 3:
       
  #Q9: Chi-squared p-value. Define brexit_hit, with the code below, which computes the CI for all Brexit polls in 2016
        #and then calculates whether the CI covers the actual value of the spread d = -.038
       brexit_hit <- brexit_polls %>%
         mutate(p_hat = (spread + 1)/2,
                se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
                spread_lower = spread - qnorm(.975)*se_spread,
                spread_upper = spread + qnorm(.975)*se_spread,
                hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
         select(poll_type, hit)
       #Use brexit_hit to make a 2x2 table of poll type and hit status. Then use the chisq.test() function to perform a 
       #chi-squared test to determine whether the difference in hit rate is significant.
       table <- brexit_hit %>%
         group_by(poll_type, hit) %>%
         summarise(n = n()) %>%
         spread(poll_type, n)
       chi_test <- table %>%
         select(-hit) %>%
         chisq.test()
       #What is the p-value of the chi-squared test comparing the hit rate of online and telephone polls?
        chi_test %>% .$p.value  
      #Determine which poll type has a higher probability of producing a confidence interval that covers the correct
        #value of the spread. Also determine whether this difference is statistically significant at a p-value cutoff
        #of 0.05. Which of the following is true?  
        Hit_online <- (table[2,2]/sum(table[,2]))/(table[1,2]/sum(table[,2]))
        Hit_telephone <- (table[2,3]/sum(table[,3]))/(table[1,3]/sum(table[,3]))
        Hit_online > Hit_telephone
        
        #Answer:Online polls are more likely to cover the correct value of the spread and this difference is 
          #statistically significant.
  
  #Q10: Odds ratio of online and telephone poll hit rate. Use the table from Q9 to calculate the odds ratio
       #between the hit rate of online and telephone polls to determine the magnitude of the difference in 
       #the performance between poll types.
        
        #Calculate the odds that an online poll generates a CI that covers the actual value of the spread
        Hit_online[1,1]
        #Calculate the odds that a telephone poll generates a CI that covers the actual value of the spread
        Hit_telephone[1,1]
        #Determine how many times larger odds are for online polls to hit vs telephone polls
        Hit_online[1,1]/Hit_telephone[1,1]
        
  #Q11:Plotting spread over time. Use brexit_polls to make a plot of the spread over time colored by poll type
        #Use geom_smooth with method = "less" to plot smoot curves with a span of 0.4. Include the individual
        #data points colored by poll type. Add a horizontal line indicating the final value of d = -.038
        spread_time <- brexit_polls %>%
          ggplot(aes(x = enddate, y = spread, col = poll_type)) +
          geom_point() +
          geom_smooth(method = "loess", span = 0.4) +
          geom_hline(yintercept = d)
        
  #Q12: Plotting raw percentages over time. Use the following code to create the object brexit_long, which has a 
        #column vote containing the 3 possible votes on a Brexit poll and a column proportion containing the raw 
        #proportion choosing that vote option on the given poll
        brexit_long <- brexit_polls %>%
          gather(vote, proportion, "remain":"undecided") %>%
          mutate(vote = factor(vote))
        #Make a grap of proportion over time colored by vote. Add a smooth trendline with geom_smooth and
        #method = "loess" with a span of 0.3
        proportion_time <- brexit_long %>%
          ggplot(aes(x = enddate, y = proportion, col = vote)) +
          geom_point() +
          geom_smooth(method = "loess", span = 0.3) +
          geom_hline(yintercept = d)
        #Which of the following are TRUE?
        #The percentage of undecided voters declines over time but is still around 10% throughout June. (TRUE)
        #Over most of the date range, the confidence bands for Leave and Remain overlap. (TRUE)
        #Over most of the date range, the confidence bands for Leave and Remain are below 50%. (TRUE)
        #In the first half of June, Leave was polling higher than Remain, although this difference 
         #was within the CIs. (TRUE)
        #At the time of the election in late June, the percentage voting Leave is trending upwards. (FALSE)