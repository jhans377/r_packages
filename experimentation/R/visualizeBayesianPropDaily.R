#' This takes results from summariseMetric and visualizes the metrics within a Bayesian Framework
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()


visualizeBayesianProp <- function(daily_bayes) {

control <- daily_bayes %>% select(date,value=rate_control,lower=lower_control,upper=upper_control) %>% mutate(sample = 'control')
variant <- daily_bayes %>% select(date,value=rate_variant,lower=lower_variant,upper=upper_variant) %>% mutate(sample = 'variant')

sample <- rbind(control,variant)

view1 <- ggplot(sample,aes(x=date(date),y=value,colour=sample,group=sample)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),colour='grey',alpha=0.2) + scale_y_continuous(label=scales::percent) + theme(legend.position='bottom',legend.title=element_blank()) + ylab('Proportion') + xlab('Day of Experiment')

view2 <- ggplot(daily_bayes,aes(x=date(date),y=median_diff,group=1)) + geom_line() + geom_ribbon(aes(ymin=lower_diff,ymax=upper_diff),colour='grey',alpha=0.2) + scale_y_continuous(label=scales::percent) + theme(legend.position='bottom',legend.title=element_blank()) + ylab('Estimated Delta') + xlab('Day of Experiment') + geom_hline(yintercept=0,linetype='dashed',colour='red')

view <- grid.arrange(view1,view2)

return(view)
}
visualizeBayesianProp(bayes)
