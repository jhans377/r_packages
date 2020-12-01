#' This takes results from summariseMetric and visualizes the metrics within a Bayesian Framework
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' visualizeBayesianProp(test_metric_agg)


visualizeBayesPropDist <- function(data) {

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

  ## generate view of two distributions compared side by side
  sample1 <- data.frame(value = sample1)
  sample1$sample <- 'Control'

  sample2 <- data.frame(value = sample2)
  sample2$sample <- 'Variant'

  sample <- rbind(sample1,sample2)
  dist <- ggplot(sample,aes(x=value,fill=sample)) + geom_density(alpha=0.2) + scale_x_continuous(label=scales::percent) + xlab('Range of Possible Values') + theme(legend.position = 'bottom',legend.title=element_blank())

  return(dist)
}
