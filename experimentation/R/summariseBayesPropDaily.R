
#'  Takes raw data a constructs a daily time series from the date
#'
#'
#' @param Yes
#' @keywords ab test, summarise
#' @export
#' @examples
#' summarize_data()

summariseBayesPropDaily <- function(daily_data,sig_level) {

unique_days <- unique(daily_data$date)

results <- NA

for (i in unique_days) {

 date_filter <- as_datetime(i)

 temp <- daily_data %>% filter(date(date) <= date(date_filter))
 date <- max(temp$date)

 temp <- summariseBayesianProp(temp,sig_level)
 temp <- data.frame(date,temp)

 results <- rbind(results,temp)
}
results <- subset(results,is.na(date) == FALSE)

return(results)
}
