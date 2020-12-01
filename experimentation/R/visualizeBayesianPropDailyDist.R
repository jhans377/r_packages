#' This takes results from summariseMetricDaily and generates time-series view with cumulative daily totals and credible intervals.
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' visualizeBayesianPropDaily(bayes_daily)


visualizeBayesianPropDailyDist <- function(daily_bayes) {

control <- daily_bayes %>% select(date,value=rate_control,lower=lower_control,upper=upper_control) %>% mutate(sample = 'Control')
variant <- daily_bayes %>% select(date,value=rate_variant,lower=lower_variant,upper=upper_variant) %>% mutate(sample = 'Variant')

sample <- rbind(control,variant)

view1 <- ggplot(sample,aes(x=date(date),y=value,colour=sample,group=sample)) + geom_line(size=1) + geom_ribbon(aes(ymin=lower,ymax=upper,fill=sample),alpha=0.1) + scale_y_continuous(label=scales::percent) + theme(legend.position='bottom',legend.title=element_blank()) + ylab('Proportion') + xlab('Day of Experiment') + ggtitle('Daily Estimate for Proportion with Credible Intervals')

return(view1)
}
