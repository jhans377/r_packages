
#'  takes a few inputs and returns a data.frame with various bayesian summary statistics for hypothetical deltas
#' and experiment run times
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

library("dplyr")

sizeBayesianProp <- function(baseline,min_delta,max_delta,incremental_differences,n_per_day,sample_split,max_days,day_increments,min_effect_desized) {

  ## ## create skeleton frame for visuals
  skeleton <- data.frame(baseline,delta=seq(min_delta,max_delta,.005),binary=1)
  skeleton$variant <- skeleton$baseline*(1+skeleton$delta)

  ## create sample size skeleton to cross join to skeleton
  skeleton2 <- data.frame(days = seq(1,max_days,day_increments),binary=1)
  skeleton2$days <- skeleton2$days-1+day_increments
  skeleton2$n_total <- skeleton2$days*n_per_day
  skeleton2$n_control <- round(skeleton2$n_total*sample_split,digits=0)
  skeleton2$n_variant <- skeleton2$n_total-skeleton2$n_control
  ## cross join!
  base <- skeleton %>% inner_join(skeleton2,by='binary')
  base <- base %>% select(baseline,variant,delta,days,n_total,n_control,n_variant) %>% arrange(days,delta) %>% mutate(row = row_number())

  ## define bayesian function to calculate key statistics for every row
  calc_stats <- function(data) {

   trials1 <- data$n_control
   alpha1 <- round(data$n_control*data$baseline,digits=0)
   beta1 <- trials1-alpha1

   trials2 <- data$n_variant
   alpha2 <- round(data$n_variant*data$variant,digits=0)
   beta2 <- trials2-alpha2

   sim_trials <- 1000000

   sample1 <- rbeta(sim_trials,alpha1,beta1)
   sample2 <- rbeta(sim_trials,alpha2,beta2)

   prob <- sum(sample2 >= sample1)/sim_trials

   ## calculate quantiles for differences from simulations
   sample <- data.frame(control = sample1, variant = sample2)
   sample$diff <- (sample$variant/sample$control)-1

   median_diff <- quantile(sample$diff,.5)
   lower_diff <- quantile(sample$diff,.01)
   upper_diff <- quantile(sample$diff,.99)

   ##
   summary_stats <- data.frame(prob,median_diff,lower_diff,upper_diff)

   return(summary_stats)
  }
  test <- subset(base,row==1)
  calc_stats(test)
  ## run simulations to calculate what we would know given each of these hypothetical deltas and sample sizes
  unique_rows <- unique(base$row)

  results <- NA

  for (i in unique_rows) {

  base_filtered <- subset(base,row==i)
  stats <- calc_stats(base_filtered)

  temp <- data.frame(base_filtered,stats)
  results <- rbind(results,temp)

  }
  results <- subset(results,is.na(prob) == FALSE)

  return(results)
}
