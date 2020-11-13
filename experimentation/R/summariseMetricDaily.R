#' Takes a metric dataframe and summarizes it on a cumulative, daily level
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summariseMetricDaily(test_metric)

summariseMetricDaily <- function(metric_data) {

  unique_days <- unique(metric_data$date)

  results <- NA

  for (i in unique_days) {

   date_filter <- date(i)
   metric_data$date <- date(metric_data$date)

   temp <- subset(metric_data, date <= date_filter)
   date <- max(temp$date)

   temp <- summariseMetric(temp)
   temp <- data.frame(date,temp)

   results <- rbind(results,temp)
  }
  results <- subset(results,is.na(date) == FALSE)

  results <- results %>% arrange(date)

  return(results)
  }
