#' Takes a metric dataframe and summarizes it on a cumulative, daily level
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

summariseMetricDaily <- function(metric_data) {

  unique_days <- unique(metric_data$date)

  results <- NA

  for (i in unique_days) {

   date_filter <- as_datetime(i)

   temp <- metric_data %>% filter(date(date) <= date(date_filter))
   date <- max(temp$date)

   temp <- summariseMetric(temp)
   temp <- data.frame(date,temp)

   results <- rbind(results,temp)
  }
  results <- subset(results,is.na(date) == FALSE)

  results <- results %>% arrange(date)

  return(results)
  }
