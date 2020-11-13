
#'  Takes summary data from 'summariseMetrics' and generates Bayesiam Summary Statistics.  Output is a clean summary table!
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

summariseBayesianProp <- function(data,probability_threshold) {

  data <- test_metric_agg
  probability_threshold <- .01

  ## define inputs for modeling metric with a beta distribution
  trials_control <- data$n_control
  alpha_control <- round(data$n_control*data$prop_control,digits=0)
  beta_control <- trials_control-alpha_control

  trials_variant <- data$n_variant
  alpha_variant <- round(data$n_variant*data$prop_variant,digits=0)
  beta_variant <- trials_variant-alpha_variant

  ## set the number of random draws to be used for constructing posterior distribution from beta distribution
  trials <- 1000000

  sample_control <- rbeta(trials,alpha_control,beta_control)
  sample_variant <- rbeta(trials,alpha_variant,beta_variant)

  ## calculate summary statistics from the above

  rate_control <- alpha_control/trials_control
  rate_variant <- alpha_variant/trials_variant
  delta <- (rate_control/rate_variant)-1

  rate_control_clean <- paste(round(rate_control*100,digits=2),'%',sep='')
  rate_variant_clean <- paste(round(rate_variant*100,digits=2),'%',sep='')
  delta_clean <- paste(round(delta*100,digits=2),'%',sep='')

  lower_control <- paste(round(quantile(sample_control,0.025)*100,digits=2),'%',sep='')
  upper_control <- paste(round(quantile(sample_control,0.975)*100,digits=2),'%',sep='')
  ci_control <- paste(lower_control,', ',upper_control,sep='')

  lower_variant <- paste(round(quantile(sample_variant,0.025)*100,digits=2),'%',sep='')
  upper_variant <- paste(round(quantile(sample_variant,0.975)*100,digits=2),'%',sep='')
  ci_variant <- paste(lower_variant,', ',upper_variant,sep='')

  ## how many times within the 1 million draws did we see sample 1 have a value greater than sample 2?
  prob <- sum(sample_control >= sample_variant)/trials
  prob025 <- sum((sample_control/sample_variant)-1 <= -.0025)/trials ## probability that the delta is worse than 0.25%
  prob05 <- sum((sample_control/sample_variant)-1 <= -.005)/trials ## probability that the delta is worse than 0.5%
  prob1 <- sum((sample_control/sample_variant)-1 <= -.01)/trials  ## probability that the delta is worse than 1%

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
  sample <- data.frame(prop1 = sample_control, prop2 = sample_variant)
  sample <- sample %>% mutate(diff = (sample_control/sample_variant)-1)

  lower_diff <- paste(round(quantile(sample$diff,0.025)*100,digits=2),'%',sep='')
  upper_diff <- paste(round(quantile(sample$diff,0.975)*100,digits=2),'%',sep='')

  ci_diff <- paste(lower_diff,', ',upper_diff,sep='')

  ## combine stats into summary dataframe
  results <- data.frame(rate_control_clean,rate_variant_clean,delta_clean,prob_clean,prob025_clean,prob05_clean,prob1_clean,ci_control,ci_variant,ci_diff,finding,finding025,finding05,finding1,rate_control,rate_variant,delta,prob,prob025,prob05,prob1,trials_control,trials_variant)

  return(results)
}
