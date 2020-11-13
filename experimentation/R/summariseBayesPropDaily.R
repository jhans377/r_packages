
#'  Takes raw data a constructs a daily time series from the date
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summariseBayesPropDaily()

summariseBayesPropDaily <- function(daily_data,sig_level) {

  unique_days <- date(unique(daily_data$date))

  results <- NA

  for (i in unique_days) {

   daily_data$date <- date(daily_data$date)

   temp <- subset(daily_data, date == i)
   date <- max(temp$date)

   temp <- summariseBayesianProp(temp,sig_level)
   temp <- data.frame(date,temp)

   results <- rbind(temp,results)
  }
  results <- subset(results,is.na(date) == FALSE)

  results <- results %>% arrange(date)

  return(results)
  }
