
#'  Takes summary data from 'summariseMetrics' and generates Bayesiam Summary Statistics.  Output is a clean summary table!
#'
#' 
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

summariseBayesianProp <- function(data,probability_threshold) {

  ## define inputs for modeling metric with a beta distribution
  trials1 <- data$n_control
  alpha1 <- round(data$n_control*data$prop_control,digits=0)
  beta1 <- trials1-alpha1

  trials2 <- data$n_variant
  alpha2 <- round(data$n_variant*data$prop_variant,digits=0)
  beta2 <- trials2-alpha2

  ## set the number of random draws to be used for constructing posterior distribution from beta distribution
  trials <- 1000000

  sample1 <- rbeta(trials,alpha1,beta1)
  sample2 <- rbeta(trials,alpha2,beta2)

  ## calculate summary statistics from the above

  rate1 <- alpha1/trials1
  rate2 <- alpha2/trials2
  delta <- (rate1/rate2)-1

  rate1_clean <- paste(round(rate1*100,digits=2),'%',sep='')
  rate2_clean <- paste(round(rate2*100,digits=2),'%',sep='')
  delta_clean <- paste(round(delta*100,digits=2),'%',sep='')

  lower1 <- paste(round(quantile(sample1,0.025)*100,digits=2),'%',sep='')
  upper1 <- paste(round(quantile(sample1,0.975)*100,digits=2),'%',sep='')
  ci1 <- paste(lower1,', ',upper1,sep='')

  lower2 <- paste(round(quantile(sample2,0.025)*100,digits=2),'%',sep='')
  upper2 <- paste(round(quantile(sample2,0.975)*100,digits=2),'%',sep='')
  ci2 <- paste(lower2,', ',upper2,sep='')

  ## how many times within the 1 million draws did we see sample 1 have a value greater than sample 2?
  prob <- sum(sample1 >= sample2)/trials
  prob025 <- sum((sample1/sample2)-1 <= -.0025)/trials ## probability that the delta is worse than 0.25%
  prob05 <- sum((sample1/sample2)-1 <= -.005)/trials ## probability that the delta is worse than 0.5%
  prob1 <- sum((sample1/sample2)-1 <= -.01)/trials  ## probability that the delta is worse than 1%

  ## define if test is significant in a positive or negative direction
  finding <- ifelse(prob <= probability_threshold,'significant - negative','insignificant')
  finding <- ifelse(prob >= 1-probability_threshold,'significant - positive',finding)

  finding025 <- ifelse(prob025 >= 1-probability_threshold,'significant - negative','insignificant')
  finding05 <- ifelse(prob05 >= 1-probability_threshold,'significant - negative','insignificant')
  finding1 <- ifelse(prob1 >= 1-probability_threshold,'significant - negative','insignificant')

  prob_clean <- paste(round(prob*100,digits=2),'%',sep='')
  prob025_clean <- paste(round(prob025*100,digits=2),'%',sep='')
  prob05_clean <- paste(round(prob05*100,digits=2),'%',sep='')
  prob1_clean <- paste(round(prob1*100,digits=2),'%',sep='')

  ## generate estimate for difference in proportions
  sample <- data.frame(prop1 = sample1, prop2 = sample2)
  sample <- sample %>% mutate(diff = (sample1/sample2)-1)

  lower_diff <- paste(round(quantile(sample$diff,0.025)*100,digits=2),'%',sep='')
  upper_diff <- paste(round(quantile(sample$diff,0.975)*100,digits=2),'%',sep='')

  ci_diff <- paste(lower_diff,', ',upper_diff,sep='')

  ## combine stats into summary dataframe
  results <- data.frame(rate1_clean,rate2_clean,delta_clean,prob_clean,prob025_clean,prob05_clean,prob1_clean,ci1,ci2,ci_diff,finding,finding025,finding05,finding1,rate1,rate2,delta,prob,prob025,prob05,prob1,trials1,trials2)

  return(results)
}
