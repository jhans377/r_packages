
#'  Takes raw experiment data and transforms it into a format that can easily pass through all of the other functions
#'
#'
#' @param Yes
#' @keywords ab test, transform, metric
#' @export
#' @examples
#' transformData(test_data,user_id,value2,value,sample,'is control',date)


transformData <- function(raw_data,unique_id,numerator,denominator,sample_column,control_value,date_column) {

  numerator = enquo(numerator)
  denominator = enquo(denominator)
  unique_id = enquo(unique_id)
  sample_column = enquo(sample_column)
  date_column = enquo(date_column)

  ## take segmentation variable and set to control and variant values
  raw_data <- raw_data %>% mutate(segmentation = ifelse(UQ(sample_column) == control_value,'control','variant'))

  ## identify denominator and numerator columns for joining together for final 'metric' dataframe
  denominator <- raw_data %>% select(denominator = UQ(denominator), id = UQ(unique_id), date = UQ(date_column), segmentation) %>% filter(denominator == 1)
  numerator <- raw_data %>% select(numerator = UQ(numerator), id = UQ(unique_id)) %>% filter(numerator == 1)

  ## join numerator and denomiantor together
  metric_data <- denominator %>% left_join(numerator, by='id') %>% select(segmentation,date,id,denominator,numerator)

  ## for cases where we didn't have a numerator value, fill in zeros
  metric_data$numerator <- ifelse(is.na(metric_data$numerator) == TRUE,0,metric_data$numerator)

  return(metric_data)

}
